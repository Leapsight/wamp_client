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

% <<0:5, 1:3>>
-define(RAW_PING_PREFIX, 1).
% <<0:5, 2:3>>
-define(RAW_PONG_PREFIX, 2).

-export([init/1]).
-export([send_to_router/2]).
-export([handle_info/2]).
-export([shutdown/1]).

-record(state, {
    awre_con = unknown,
    socket = none,
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
    Family =
        case application:get_env(awre, ip_version, 4) of
            6 ->
                inet6;
            _ ->
                %% 4 or invalid value
                inet
        end,
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, Family], 5000),
    link(Socket),
    % need to send the new TCP packet
    Enc =
        case Encoding of
            json -> raw_json;
            raw_json -> raw_json;
            msgpack -> raw_msgpack;
            raw_msgpack -> raw_msgpack;
            erlbin -> raw_erlbin;
            raw_erlbin -> raw_erlbin;
            _ -> raw_msgpack
        end,
    SerNum =
        case Enc of
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
    MaxLen = 15,
    ok = gen_tcp:send(Socket, <<127, MaxLen:4, SerNum:4, 0, 0>>),
    State = #state{
        awre_con = Con,
        version = Version,
        client_details = CDetails,
        socket = Socket,
        enc = Enc,
        sernum = SerNum,
        realm = Realm,
        %% in case of authentication configuration is provided
        auth_details = maps:get(auth_details, Args, undefined)
    },
    {ok, State}.

send_to_router({ping, Payload}, #state{socket = S} = State) ->
    Frame = <<(?RAW_PING_PREFIX):8, (byte_size(Payload)):24, Payload/binary>>,
    ok = gen_tcp:send(S, Frame),
    {ok, State};
send_to_router({pong, Payload}, #state{socket = S} = State) ->
    Frame = <<(?RAW_PONG_PREFIX):8, (byte_size(Payload)):24, Payload/binary>>,
    ok = gen_tcp:send(S, Frame),
    {ok, State};
%% Authenticates using password
send_to_router({challenge, password}, State) ->
    Password = maps:get(secret, State#state.auth_details, <<>>),
    Signature = handle_challenge(password, Password),
    Message = {authenticate, Signature, #{}},
    send_to_router(Message, State);
%% Authenticates using WAMP-CRA
send_to_router({challenge, wampcra, AuthExtra}, State) ->
    Password = maps:get(secret, State#state.auth_details, <<>>),
    Signature = handle_challenge(wampcra, Password, AuthExtra),
    Message = {authenticate, Signature, #{}},
    send_to_router(Message, State);
%% Authenticates using cryptosign
send_to_router({challenge, cryptosign, AuthExtra}, State) ->
    PubKey = maps:get(pubkey, State#state.auth_details, <<>>),
    PrivKey = maps:get(privkey, State#state.auth_details, <<>>),
    Signature = handle_challenge(cryptosign, {PubKey, PrivKey}, AuthExtra),
    Message = {authenticate, Signature, #{}},
    send_to_router(Message, State);
send_to_router(Message, #state{socket = S, enc = Enc, out_max = MaxLength} = State) ->
    SerMessage = wamper_protocol:serialize(Message, Enc),
    case byte_size(SerMessage) > MaxLength of
        true ->
            ok;
        false ->
            ok = gen_tcp:send(S, SerMessage)
    end,
    {ok, State}.

handle_info(
    {tcp, Socket, Data},
    #state{buffer = Buffer, socket = Socket, enc = Enc, handshake = done} = State
) ->
    {Messages, NewBuffer} = wamper_protocol:deserialize(<<Buffer/binary, Data/binary>>, Enc),
    forward_messages(Messages, State),
    {ok, State#state{buffer = NewBuffer}};
handle_info({tcp, Socket, <<127, 0, 0, 0>>}, #state{socket = Socket} = State) ->
    forward_messages([{abort, #{}, tcp_handshake_failed}], State),
    {ok, State};
handle_info(
    {tcp, Socket, <<127, L:4, S:4, 0, 0>>},
    #state{
        socket = Socket,
        realm = Realm,
        sernum = SerNum,
        version = Version,
        client_details = CDetails,
        auth_details = AuthDetails
    } = State
) ->
    S = SerNum,
    case AuthDetails of
        undefined ->
            %% anonymous authentication
            State1 = State#state{out_max = math:pow(2, 9 + L), handshake = done},
            send_to_router({hello, Realm, #{agent => Version, roles => CDetails}}, State1);
        #{method := anonymous} ->
            %% anonymous authentication
            State1 = State#state{out_max = math:pow(2, 9 + L), handshake = done},
            send_to_router({hello, Realm, #{agent => Version, roles => CDetails}}, State1);
        #{user := AuthId, method := cryptosign, pubkey := PubKey} ->
            %% cryptosign authentication
            State1 = State#state{out_max = math:pow(2, 9 + L), handshake = done},
            send_to_router(
                {hello, Realm, #{
                    agent => Version,
                    roles => CDetails,
                    authid => AuthId,
                    authmethods => [cryptosign],
                    authextra => #{pubkey => PubKey}
                }},
                State1
            );
        #{user := AuthId, method := AuthMethod} ->
            %% password authentication
            %% wampcra authentication
            State1 = State#state{out_max = math:pow(2, 9 + L), handshake = done},
            send_to_router(
                {hello, Realm, #{
                    agent => Version,
                    roles => CDetails,
                    authid => AuthId,
                    authmethods => [AuthMethod]
                }},
                State1
            )
    end;
handle_info({tcp_closed, Socket}, State) ->
    ?LOG_INFO(#{
        text => "Connection closed",
        reason => tcp_closed,
        socket => Socket
    }),
    {stop, tcp_closed, State};
handle_info({tcp_error, Socket, Reason}, State) ->
    ?LOG_INFO(#{
        text => "Connection closed",
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

shutdown(#state{socket = S}) ->
    ok = gen_tcp:close(S),
    ok.

forward_messages([], _) ->
    ok;
forward_messages([{ping, Payload} | Tail], State0) ->
    {ok, State1} = send_to_router({pong, Payload}, State0),
    forward_messages(Tail, State1);
forward_messages([Msg | Tail], #state{awre_con = Con} = State) ->
    awre_con:send_to_client(Msg, Con),
    forward_messages(Tail, State).

%% =============================================================================
%% PRIVATE
%% =============================================================================

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
%% AuthExtra = #{
%%  challenge
%%  iterations
%%  keylen
%%  salt
%% }
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
%% AuthExtra = #{
%%  challenge
%%  <<"channel_binding">>
%% }
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