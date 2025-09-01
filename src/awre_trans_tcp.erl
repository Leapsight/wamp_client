%%
%% Copyright (c) 2015 Bas Wegh
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%

-module(awre_trans_tcp).
-behaviour(awre_transport).
-include_lib("kernel/include/logger.hrl").

-define(CONNECT_TIMEOUT, 5000).
-define(CONNECT_OPTIONS(Family), [binary, {packet, 0}, Family]).

%% API
-export([handle_info/2]).
-export([init/1]).
-export([send_to_router/2]).
-export([shutdown/1]).

-record(state, {
    awre_con = unknown,
    socket = none,
    transport = undefined,  %% tcp | tls
    enc = unknown,
    sernum = unknown,
    realm = none,
    version = unknown,
    client_details = unknown,
    buffer = <<"">>,
    out_max = unknown,
    handshake = in_progress,
    %% in case of authentication configuration is provided
    %%  - user: authid
    %%  - method: anonymous | password | wampcra | cryptosign
    %%  - secret: password
    %%  - pubkey: cryptosign public key
    %%  - privkey: cryptosign private key
    auth_details = none
}).



%% =============================================================================
%% PUBLIC API
%% =============================================================================



%% ----------------------------------------------------------------------------
%% @doc Initializes the TCP transport and connects to the router
%% @end
%% ----------------------------------------------------------------------------
-spec init(map()) -> {ok, #state{}} | {error, term()}.

init(
    #{
        realm := Realm,
        awre_con := Con,
        client_details := CDetails,
        version := Version,
        host := Host,
        port := Port,
        enc := Encoding
    } = Args
) ->
    Family = case application:get_env(awre, ip_version, 4) of
        6 -> inet6;
        _ -> inet %% 4 or invalid value
    end,

    %% Create safe args for logging (exclude auth_details)
    SafeArgs = maps:without([auth_details], Args),
    ?LOG_DEBUG(#{
        text => "Starting TCP transport",
        host => Host,
        port => Port,
        family => Family,
        realm => Realm,
        encoding => Encoding,
        use_tls => maps:get(tls, Args, false),
        safe_args => SafeArgs
    }),

    %% Determine transport type and connect
    UseTls = maps:get(tls, Args, false),
    {Transport, ConnOptions} = case UseTls of
        true ->
            %% TLS connection
            SslOpts = ?CONNECT_OPTIONS(Family) ++ [{verify, verify_none}],
            {ssl, SslOpts};
        false ->
            %% TCP connection
            {gen_tcp, ?CONNECT_OPTIONS(Family)}
    end,

    ?LOG_INFO(#{
        text => "Attempting to connect",
        transport => Transport,
        host => Host,
        port => Port,
        options => ConnOptions
    }),
    case Transport:connect(Host, Port, ConnOptions, ?CONNECT_TIMEOUT) of
        {ok, Socket} ->
            ?LOG_INFO(#{
                text => "Connection established",
                transport => Transport,
                socket => Socket
            }),
            %% Only link for gen_tcp sockets, not SSL sockets
            case Transport of
                gen_tcp -> link(Socket);
                ssl -> ok  %% SSL sockets don't need linking
            end,
            % need to send the handshake packet
            {Enc, SerNum} = get_encoding_details(Encoding),
            MaxLen = 15,
            HandshakePacket = build_handshake_packet(MaxLen, SerNum),
            ok = transport_send(Transport, Socket, HandshakePacket),
            State = #state{
                awre_con = Con,
                version = Version,
                client_details = CDetails,
                socket = Socket,
                transport = Transport,
                enc = Enc,
                sernum = SerNum,
                realm = Realm,
                %% in case of authentication configuration is provided
                auth_details = wamp_client_sensitive:wrap(maps:get(auth_details, Args, undefined))
            },
            {ok, State};
        {error, Reason} ->
            ?LOG_ERROR(#{
                text => "Connection failed",
                host => Host,
                port => Port,
                transport => Transport,
                options => ConnOptions,
                reason => Reason
            }),
            {error, Reason}
    end.


%% ----------------------------------------------------------------------------
%% @doc Sends a message to the router
%% @end
%% ----------------------------------------------------------------------------
-spec send_to_router(term(), #state{}) -> {ok, #state{}} | {error, term()}.

send_to_router({ping, Payload}, #state{socket = S, transport = T} = State) ->
    ok = send_ping_pong(ping, Payload, fun(Frame) -> transport_send(T, S, Frame) end),
    {ok, State};

send_to_router({pong, Payload}, #state{socket = S, transport = T} = State) ->
    ok = send_ping_pong(pong, Payload, fun(Frame) -> transport_send(T, S, Frame) end),
    {ok, State};

%% Authenticates using password
send_to_router({challenge, password} = Challenge, State) ->
    Message = send_challenge_response(Challenge, State),
    send_to_router(Message, State);

%% Authenticates using WAMP-CRA
send_to_router({challenge, wampcra, _AuthExtra} = Challenge, State) ->
    Message = send_challenge_response(Challenge, State),
    send_to_router(Message, State);

%% Authenticates using cryptosign
send_to_router({challenge, cryptosign, _AuthExtra} = Challenge, State) ->
    Message = send_challenge_response(Challenge, State),
    send_to_router(Message, State);

send_to_router(Message, #state{socket = S, transport = T, enc = Enc, out_max = MaxLength} = State) ->
    ok = send_wamp_message(Message, Enc, MaxLength, fun(SerMsg) -> transport_send(T, S, SerMsg) end),
    {ok, State}.


%% ----------------------------------------------------------------------------
%% @doc Handles incoming messages from the transport (TCP and TLS)
%% @end
%% ----------------------------------------------------------------------------
handle_info(
    {MsgType, Socket, Data},
    #state{buffer = Buffer, socket = Socket, transport = _Transport, enc = Enc, handshake = done} = State
) when (MsgType =:= tcp orelse MsgType =:= ssl) ->
    {Messages, NewBuffer} = wamper_protocol:deserialize(<<Buffer/binary, Data/binary>>, Enc),
    forward_messages(Messages, State),
    {ok, State#state{buffer = NewBuffer}};

%% Handle handshake failure
handle_info({MsgType, Socket, <<127, 0, 0, 0>>}, #state{socket = Socket, transport = _Transport} = State)
when (MsgType =:= tcp orelse MsgType =:= ssl) ->
    forward_messages([{abort, #{}, handshake_failed}], State),
    {ok, State};

%% Handle handshake response
handle_info(
    {MsgType, Socket, HandshakeData},
    #state{socket = Socket, transport = _Transport} = State
) when (MsgType =:= tcp orelse MsgType =:= ssl) andalso byte_size(HandshakeData) =:= 4 ->
    ?LOG_INFO(#{
        text => "Received handshake response",
        transport => MsgType,
        data => HandshakeData,
        size => byte_size(HandshakeData)
    }),
    case handle_handshake_response(HandshakeData, State) of
        {ok, NewState} ->
            forward_messages([{abort, #{}, handshake_failed}], NewState),
            {ok, NewState};
        {{hello, {Realm, HelloDetails}}, NewState} ->
            send_to_router({hello, Realm, HelloDetails}, NewState)
    end;

%% Handle connection closed (TCP)
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    ?LOG_INFO(#{
        text => "TCP connection closed",
        reason => tcp_closed,
        socket => Socket
    }),
    {stop, tcp_closed, State};

%% Handle connection closed (TLS)
handle_info({ssl_closed, Socket}, #state{socket = Socket} = State) ->
    ?LOG_INFO(#{
        text => "TLS connection closed",
        reason => ssl_closed,
        socket => Socket
    }),
    {stop, ssl_closed, State};

%% Handle connection error (TCP)
handle_info({tcp_error, Socket, Reason}, #state{socket = Socket} = State) ->
    ?LOG_INFO(#{
        text => "TCP connection error",
        socket => Socket,
        reason => Reason
    }),
    {stop, Reason, State};

%% Handle connection error (TLS)
handle_info({ssl_error, Socket, Reason}, #state{socket = Socket} = State) ->
    ?LOG_INFO(#{
        text => "TLS connection error",
        socket => Socket,
        reason => Reason
    }),
    {stop, Reason, State};
handle_info(Info, State) ->
    ?LOG_ERROR(#{
        text => "Received unknown info message",
        message => Info
    }),
    {noreply, State}.


%% ----------------------------------------------------------------------------
%% @doc Shuts down the transport and closes the socket
%% @end
%% ----------------------------------------------------------------------------
-spec shutdown(#state{}) -> ok.

shutdown(#state{socket = S, transport = T}) ->
    ok = transport_close(T, S),
    ok.



%% =============================================================================
%% ENCODING AND MESSAGE HANDLING
%% =============================================================================



%% ----------------------------------------------------------------------------
%% @private
%% @doc Forwards messages to the awre connection process
%% @end
%% ----------------------------------------------------------------------------
-spec forward_messages([term()], #state{}) -> ok.

forward_messages([], _) ->
    ok;

forward_messages([{ping, Payload} | Tail], State0) ->
    {ok, State1} = send_to_router({pong, Payload}, State0),
    forward_messages(Tail, State1);

forward_messages([Msg | Tail], #state{awre_con = Con} = State) ->
    awre_con:send_to_client(Msg, Con),
    forward_messages(Tail, State).


%% ----------------------------------------------------------------------------
%% @private
%% @doc Gets encoding details (enc atom and serialization number) from encoding parameter
%% @end
%% ----------------------------------------------------------------------------
-spec get_encoding_details(atom()) -> {atom(), integer()}.

get_encoding_details(Encoding) ->
    Enc = case Encoding of
        json -> raw_json;
        raw_json -> raw_json;
        msgpack -> raw_msgpack;
        raw_msgpack -> raw_msgpack;
        erlbin -> raw_erlbin;
        raw_erlbin -> raw_erlbin;
        _ -> raw_msgpack
    end,
    SerNum = case Enc of
        raw_json ->
            1;
        raw_msgpack ->
            2;
        raw_erlbin ->
            EBinNumber = application:get_env(wamp_client, erlbin_number, undefined),
            case {is_integer(EBinNumber), EBinNumber > 0} of
                {true, true} -> EBinNumber;
                _ -> error("application parameter erlbin_number not set")
            end;
        _ ->
            0
    end,
    {Enc, SerNum}.


%% ----------------------------------------------------------------------------
%% @private
%% @doc Builds the initial handshake packet
%% @end
%% ----------------------------------------------------------------------------
-spec build_handshake_packet(integer(), integer()) -> binary().

build_handshake_packet(MaxLen, SerNum) ->
    <<127, MaxLen:4, SerNum:4, 0, 0>>.


%% ----------------------------------------------------------------------------
%% @private
%% @doc Sends ping/pong frames
%% @end
%% ----------------------------------------------------------------------------
-spec send_ping_pong(ping | pong, binary(), function()) -> ok.

send_ping_pong(ping, Payload, SendFun) ->
    Frame = <<1:8, (byte_size(Payload)):24, Payload/binary>>,  % RAW_PING_PREFIX = 1
    SendFun(Frame);

send_ping_pong(pong, Payload, SendFun) ->
    Frame = <<2:8, (byte_size(Payload)):24, Payload/binary>>,  % RAW_PONG_PREFIX = 2
    SendFun(Frame).


%% ----------------------------------------------------------------------------
%% @private
%% @doc Handles challenge response messages
%% @end
%% ----------------------------------------------------------------------------
-spec send_challenge_response({challenge, atom()} | {challenge, atom(), map()}, #state{}) ->
    {authenticate, binary(), map()}.

send_challenge_response({challenge, password}, State) ->
    AuthDetails = wamp_client_sensitive:unwrap(State#state.auth_details),
    Password = maps:get(secret, AuthDetails, <<>>),
    Signature = handle_challenge(password, Password),
    {authenticate, Signature, #{}};

send_challenge_response({challenge, wampcra, AuthExtra}, State) ->
    AuthDetails = wamp_client_sensitive:unwrap(State#state.auth_details),
    Password = maps:get(secret, AuthDetails, <<>>),
    Signature = handle_challenge(wampcra, Password, AuthExtra),
    {authenticate, Signature, #{}};

send_challenge_response({challenge, cryptosign, AuthExtra}, State) ->
    AuthDetails = wamp_client_sensitive:unwrap(State#state.auth_details),
    PubKey = maps:get(pubkey, AuthDetails, <<>>),
    PrivKey = maps:get(privkey, AuthDetails, <<>>),
    Signature = handle_challenge(cryptosign, {PubKey, PrivKey}, AuthExtra),
    {authenticate, Signature, #{}}.


%% ----------------------------------------------------------------------------
%% @private
%% @doc Sends WAMP messages with size check
%% @end
%% ----------------------------------------------------------------------------
-spec send_wamp_message(term(), atom(), integer(), function()) -> ok.

send_wamp_message(Message, Enc, MaxLength, SendFun) ->
    SerMessage = wamper_protocol:serialize(Message, Enc),
    case byte_size(SerMessage) > MaxLength of
        true ->
            ok;
        false ->
            SendFun(SerMessage)
    end.


%% ----------------------------------------------------------------------------
%% @private
%% @doc Handles handshake response and builds hello message
%% @end
%% ----------------------------------------------------------------------------
-spec handle_handshake_response(binary(), #state{}) ->
    {ok, #state{}} | {{hello, {binary(), map()}}, #state{}}.

handle_handshake_response(<<127, 0, 0, 0>>, State) ->
    {ok, State};

handle_handshake_response(
    <<127, L:4, S:4, 0, 0>>,
    #state{
        realm = Realm,
        sernum = SerNum,
        version = Version,
        client_details = CDetails,
        auth_details = AuthDetails0
    } = State
) when S =:= SerNum ->
    NewState = State#state{out_max = math:pow(2, 9 + L), handshake = done},
    AuthDetails = wamp_client_sensitive:unwrap(AuthDetails0),
    {Realm2, HelloDetails} = build_hello_message(Realm, Version, CDetails, AuthDetails),
    {{hello, {Realm2, HelloDetails}}, NewState}.


%% ----------------------------------------------------------------------------
%% @private
%% @doc Builds hello message based on authentication details
%% @end
%% ----------------------------------------------------------------------------
-spec build_hello_message(binary(), binary(), map(), undefined | map()) ->
    {binary(), map()}.

build_hello_message(Realm, Version, CDetails, undefined) ->
    %% anonymous authentication
    {Realm, #{agent => Version, roles => CDetails}};

build_hello_message(Realm, Version, CDetails, #{method := anonymous}) ->
    %% anonymous authentication
    {Realm, #{agent => Version, roles => CDetails}};

build_hello_message(Realm, Version, CDetails, #{user := AuthId, method := cryptosign, pubkey := PubKey}) ->
    %% cryptosign authentication
    {Realm, #{
        agent => Version,
        roles => CDetails,
        authid => AuthId,
        authmethods => [cryptosign],
        authextra => #{pubkey => PubKey}
    }};

build_hello_message(Realm, Version, CDetails, #{user := AuthId, method := AuthMethod}) ->
    %% password authentication or wampcra authentication
    {Realm, #{
        agent => Version,
        roles => CDetails,
        authid => AuthId,
        authmethods => [AuthMethod]
    }}.


%% ----------------------------------------------------------------------------
%% @private
%% @doc Handles the challenge received from the router
%% @end
%% ----------------------------------------------------------------------------
-spec handle_challenge(password, binary()) -> binary().

handle_challenge(password, Password) ->
    handle_challenge(password, Password, undefined).


%% ----------------------------------------------------------------------------
%% @private
%% @doc Handles the challenge received from the router
%% @end
%% ----------------------------------------------------------------------------
-spec handle_challenge(password | wampcra | cryptosign, binary() | tuple(), undefined | map()) -> binary().

handle_challenge(password, Password, _) ->
    Password;

%% Authenticates using WAMP-CRA
handle_challenge(wampcra, Password, AuthExtra) ->
    %% Extract challenge parameters
    #{
        salt := Salt,
        iterations := Iterations,
        keylen := KeyLength,
        challenge := Challenge
    } = AuthExtra,

    %% Derive key using PBKDF2 (Password-Based Key Derivation Function 2)
    SaltedPassword = crypto:pbkdf2_hmac(sha256, Password, Salt, Iterations, KeyLength),
    Key = base64:encode(SaltedPassword),

    %% Calculate HMAC-SHA256 of the challenge using the derived key
    Signature = crypto:mac(hmac, sha256, Key, Challenge),
    base64:encode(Signature);

%% Authenticates using cryptosign
handle_challenge(cryptosign, {PubKey, PrivKey}, AuthExtra) ->
    HexMessage = maps:get(challenge, AuthExtra, <<>>),

    Message = hex_utils:hexstr_to_bin(HexMessage),
    Signature = list_to_binary(
        hex_utils:bin_to_hexstr(
            sign(Message, PubKey, PrivKey)
        )
    ),

    Signature.


%% -----------------------------------------------------------------------------
%% @private
%% @doc Signs a challenge using the provided public and private keys. The keys are in hex format.
%% @end
%% -----------------------------------------------------------------------------
-spec sign(Challenge :: binary(), PubKey :: binary(), PrivKey :: binary()) ->
    Signature :: binary().

sign(Challenge, HexPubKey, HexPrivKey) ->
    PubKey = hex_utils:hexstr_to_bin(HexPubKey),
    PrivKey = normalise_privkey(hex_utils:hexstr_to_bin(HexPrivKey)),
    public_key:sign(Challenge, ignored, {ed_pri, ed25519, PubKey, PrivKey}, []).


%% -----------------------------------------------------------------------------
%% @private
%% Normalizes an Ed25519 private key to ensure it is in the 32-byte format required
%% for signing operations. This function accepts either a 32-byte or 64-byte binary key.
%% If a 64-byte key is provided, it assumes the key consists of a 32-byte private key
%% followed by a 32-byte public key, and returns only the first 32 bytes (the private key).
%% If a 32-byte key is provided, or even if the key is neither 32 nor 64 bytes,
%% the private key is returned and then the sign will be fail.
%% @end
%% -----------------------------------------------------------------------------
-spec normalise_privkey(binary()) -> binary().

normalise_privkey(Key) when byte_size(Key) == 64 ->
    binary:part(Key, {0, 32});

normalise_privkey(Key) ->
    Key.



%% =============================================================================
%% TRANSPORT ABSTRACTION
%% =============================================================================



%% ----------------------------------------------------------------------------
%% @private
%% @doc Sends data using the appropriate transport
%% @end
%% ----------------------------------------------------------------------------
-spec transport_send(gen_tcp | ssl, term(), binary()) -> ok.

transport_send(Transport, Socket, Data) ->
    Transport:send(Socket, Data).


%% ----------------------------------------------------------------------------
%% @private
%% @doc Closes socket using the appropriate transport
%% @end
%% ----------------------------------------------------------------------------
-spec transport_close(gen_tcp | ssl, term()) -> ok.

transport_close(Transport, Socket) ->
    Transport:close(Socket).

