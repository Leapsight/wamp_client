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

%% @private
-module(awre_con).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

% 60 secs
-define(TIMEOUT, 60000).
% 20 secs
-define(PING_TIMEOUT, 30000).

-export([send_to_client/2]).
-export([close_connection/1]).

%% API.
-export([start_link/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(CLIENT_DETAILS, #{
    callee => #{features => #{}},
    caller => #{features => #{}},
    publisher => #{features => #{}},
    subscriber => #{features => #{}}
}).

-record(state, {
    ets = undefined,
    goodbye_sent = false,

    transport = {none, none},

    subscribe_id = 1,
    unsubscribe_id = 1,
    publish_id = 1,
    register_id = 1,
    unregister_id = 1,
    call_id = 1,
    ping_sent = false :: {true, binary(), reference()} | false,
    ping_attempts = 0 :: non_neg_integer(),
    ping_max_attempts = 2 :: non_neg_integer()
}).

-record(ref, {
    key = {none, none},
    req = undefined,
    method = undefined,
    ref = undefined,
    args = []
}).

-record(subscription, {
    id = undefined,
    mfa = undefined,
    pid = undefined
}).

-record(registration, {
    id = undefined,
    mfa = undefined,
    pid = undefined
}).

-spec start_link(Args :: map()) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec init(Args :: map()) -> {ok, #state{}}.
init(_Args) ->
    %% dbg:tracer(), dbg:p(all,c),
    %% dbg:tpl(?MODULE, '_', []),
    %% dbg:tpl(awre_trans_tcp, '_', []),
    Ets = ets:new(undefined, [set, protected, {keypos, 2}]),
    {ok, #state{ets = Ets}, ?TIMEOUT}.

-spec send_to_client(Msg :: term(), Pid :: pid()) -> ok.
send_to_client(Msg, Pid) ->
    gen_server:cast(Pid, {awre_out, Msg}).

close_connection(Pid) ->
    gen_server:cast(Pid, terminate).

-spec handle_call(Msg :: term(), From :: term(), #state{}) -> {reply, Msg :: term(), #state{}}.
handle_call({awre_call, Msg}, From, State) ->
    handle_message_from_client(Msg, From, State);
handle_call(_Msg, _From, State) ->
    {noreply, State, ?TIMEOUT}.

handle_cast({awre_call, From, Msg}, State) ->
    handle_message_from_client(Msg, From, State);
handle_cast({awre_out, Msg}, State) ->
    handle_message_from_router(Msg, State);
handle_cast(
    {shutdown, Details, Reason}, #state{goodbye_sent = GS, transport = {TMod, TState}} = State
) ->
    NewState =
        case GS of
            true ->
                State;
            false ->
                {ok, NewTState} = TMod:send_to_router({goodbye, Details, Reason}, TState),
                State#state{transport = {TMod, NewTState}}
        end,
    {noreply, NewState#state{goodbye_sent = true}, ?TIMEOUT};
handle_cast(terminate, #state{transport = {TMod, TState}} = State) ->
    ok = TMod:shutdown(TState),
    {stop, normal, State};
handle_cast(_Request, State) ->
    {noreply, State, ?TIMEOUT}.

handle_info(
    timeout,
    #state{ping_attempts = N, ping_max_attempts = N} = State
) ->
    stop_timeout(timeout, State);
handle_info(timeout, #state{ping_sent = false} = State0) ->
    {ok, State1} = send_ping(State0),
    {noreply, State1};
handle_info(
    ping_timeout,
    #state{ping_sent = Val, ping_attempts = N, ping_max_attempts = N} = State
) when Val =/= false ->
    stop_timeout(ping_timeout, State);
handle_info(ping_timeout, #state{ping_sent = {_, Bin, _}} = State) ->
    %% We try again until we reach ping_max_attempts
    ?LOG_DEBUG(#{
        text => "Ping timeout. Sending additional ping to router.",
        attempt => io_lib:format(
            "~p/~p", [State#state.ping_attempts, State#state.ping_max_attempts]
        )
    }),
    {ok, State1} = send_ping(Bin, State),
    {noreply, State1, ?TIMEOUT};
handle_info(Data, #state{transport = {T, TState}} = State) ->
    Res = T:handle_info(Data, TState),
    case Res of
        {ok, NewTState} ->
            {noreply, State#state{transport = {T, NewTState}}, ?TIMEOUT};
        {stop, Reason, NewTState} ->
            ?LOG_INFO(#{text => "Connection closed", reason => Reason}),
            {stop, Reason, State#state{transport = {T, NewTState}}}
    end;
handle_info(Info, State) ->
    ?LOG_ERROR(#{
        text => "Received unknown info message",
        message => Info
    }),
    {noreply, State, ?TIMEOUT}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec handle_message_from_client(Msg :: term(), From :: term(), State :: #state{}) ->
    {noreply, #state{}} | {reply, Result :: term(), #state{}}.

handle_message_from_client(
    {connect, Host, Port, Realm, Encoding},
    From,
    #state{transport = {_, _}} = State
) ->
    handle_message_from_client({connect, Host, Port, Realm, Encoding, undefined}, From, State);
handle_message_from_client(
    {connect, Host, Port, Realm, Encoding, AuthDetails} = Msg,
    From,
    #state{transport = {T, _}} = State
) ->
    Args0 = #{
        awre_con => self(),
        host => Host,
        port => Port,
        realm => Realm,
        enc => Encoding,
        version => awre:get_version(),
        client_details => ?CLIENT_DETAILS
    },
    Args =
        case AuthDetails of
            undefined ->
                Args0;
            AuthDetails ->
                Args0#{auth_details => AuthDetails}
        end,
    {Trans, TState} =
        case T of
            none ->
                awre_transport:init(Args);
            T ->
                NewTState = T:init(Args),
                {T, NewTState}
        end,
    {_, NewState} = create_ref_for_message(Msg, From, #{}, State),
    {noreply, NewState#state{transport = {Trans, TState}}, ?TIMEOUT};
handle_message_from_client({subscribe, Options, Topic, Mfa}, From, State) ->
    {ok, NewState} = send_and_ref(
        {subscribe, request_id, Options, Topic}, From, #{mfa => Mfa}, State
    ),
    {noreply, NewState, ?TIMEOUT};
handle_message_from_client({unsubscribe, SubscriptionId}, From, State) ->
    {ok, NewState} = send_and_ref(
        {unsubscribe, request_id, SubscriptionId}, From, #{sub_id => SubscriptionId}, State
    ),
    {noreply, NewState, ?TIMEOUT};
handle_message_from_client({publish, Options, Topic, Arguments, ArgumentsKw}, From, State) ->
    {ok, NewState} = send_and_ref(
        {publish, request_id, Options, Topic, Arguments, ArgumentsKw}, From, #{}, State
    ),
    {reply, ok, NewState};
handle_message_from_client({register, Options, Procedure, Mfa}, From, State) ->
    {ok, NewState} = send_and_ref(
        {register, request_id, Options, Procedure}, From, #{mfa => Mfa}, State
    ),
    {noreply, NewState, ?TIMEOUT};
handle_message_from_client({unregister, RegistrationId}, From, State) ->
    {ok, NewState} = send_and_ref(
        {unregister, request_id, RegistrationId}, From, #{reg_id => RegistrationId}, State
    ),
    {noreply, NewState, ?TIMEOUT};
handle_message_from_client({call, Options, Procedure, Arguments, ArgumentsKw}, From, State) ->
    {ok, NewState} = send_and_ref(
        {call, request_id, Options, Procedure, Arguments, ArgumentsKw}, From, #{}, State
    ),
    {noreply, NewState, ?TIMEOUT};
handle_message_from_client({yield, _, _, _, _} = Msg, _From, State) ->
    {ok, NewState} = send_to_router(Msg, State),
    {reply, ok, NewState, ?TIMEOUT};
handle_message_from_client(
    {error, invocation, RequestId, Details, ErrorUri, Args, ArgsKw}, _From, State
) ->
    {ok, NewState} = send_to_router(
        {error, invocation, RequestId, Details, ErrorUri, Args, ArgsKw}, State
    ),
    {reply, ok, NewState, ?TIMEOUT};
handle_message_from_client(_Msg, _From, State) ->
    {noreply, State, ?TIMEOUT}.

handle_message_from_router({pong, Payload}, St) ->
    %% We received a PONG
    case St#state.ping_sent of
        {true, Payload, TimerRef} ->
            %% We reset the state
            ok = erlang:cancel_timer(TimerRef, [{info, false}]),
            {noreply, St#state{ping_sent = false, ping_attempts = 0}, ?TIMEOUT};
        {true, _, TimerRef} ->
            ok = erlang:cancel_timer(TimerRef, [{info, false}]),
            ?LOG_ERROR(#{
                message => "Invalid pong message from peer",
                reason => "invalid_payload"
            }),
            {stop, invalid_ping_response, St};
        false ->
            ?LOG_ERROR(#{message => "Unrequested pong message from peer"}),
            %% Should we stop instead?
            {noreply, St, ?TIMEOUT}
    end;
%% We received a CHALLENGE message from the router asking for authentication
handle_message_from_router({challenge, <<"password">>, _}, State) ->
    {ok, NewState} = send_to_router({challenge, password}, State),
    {noreply, NewState, ?TIMEOUT};
handle_message_from_router({challenge, wampcra, AuthExtra}, State) ->
    {ok, NewState} = send_to_router({challenge, wampcra, AuthExtra}, State),
    {noreply, NewState, ?TIMEOUT};
handle_message_from_router({challenge, <<"cryptosign">>, AuthExtra}, State) ->
    {ok, NewState} = send_to_router({challenge, cryptosign, AuthExtra}, State),
    {noreply, NewState, ?TIMEOUT};
handle_message_from_router({welcome, SessionId, RouterDetails}, State) ->
    {From, _} = get_ref(hello, hello, State),
    gen_server:reply(From, {ok, SessionId, RouterDetails}),
    {noreply, State, ?TIMEOUT};
handle_message_from_router({abort, Details, Reason}, State) ->
    ?LOG_WARNING(#{
        text => "Aborting connection",
        wamp => #{
            details => Details,
            reason => Reason
        }
    }),
    {From, _} = get_ref(hello, hello, State),
    gen_server:reply(From, {abort, Details, Reason}),
    close_connection(),
    {noreply, State, ?TIMEOUT};
handle_message_from_router({goodbye, _Details, _Reason}, #state{goodbye_sent = GS} = State) ->
    NewState =
        case GS of
            true ->
                State;
            false ->
                {ok, NState} = send_to_router({goodbye, [], goodbye_and_out}, State),
                NState
        end,
    close_connection(),
    {noreply, NewState, ?TIMEOUT};
%handle_message_from_router({error,},#state{ets=Ets}) ->

%handle_message_from_router({published,},#state{ets=Ets}) ->

handle_message_from_router({subscribed, RequestId, SubscriptionId}, #state{ets = Ets} = State) ->
    {From, Args} = get_ref(RequestId, subscribe, State),
    Mfa = maps:get(mfa, Args),
    {Pid, _} = From,
    ets:insert_new(Ets, #subscription{id = SubscriptionId, mfa = Mfa, pid = Pid}),
    gen_server:reply(From, {ok, SubscriptionId}),
    {noreply, State, ?TIMEOUT};
handle_message_from_router({unsubscribed, RequestId}, #state{ets = Ets} = State) ->
    {From, Args} = get_ref(RequestId, unsubscribe, State),
    SubscriptionId = maps:get(sub_id, Args),
    ets:delete(Ets, SubscriptionId),
    gen_server:reply(From, ok),
    {noreply, State, ?TIMEOUT};
handle_message_from_router({event, SubscriptionId, PublicationId, Details}, State) ->
    handle_message_from_router(
        {event, SubscriptionId, PublicationId, Details, undefined, undefined}, State
    );
handle_message_from_router({event, SubscriptionId, PublicationId, Details, Arguments}, State) ->
    handle_message_from_router(
        {event, SubscriptionId, PublicationId, Details, Arguments, undefined}, State
    );
handle_message_from_router(
    {event, SubscriptionId, _PublicationId, Details, Arguments, ArgumentsKw} = Msg,
    #state{ets = Ets} = State
) ->
    [
        #subscription{
            id = SubscriptionId,
            mfa = Mfa,
            pid = Pid
        }
    ] = ets:lookup(Ets, SubscriptionId),
    case Mfa of
        undefined ->
            % send it to user process
            Pid ! {awre, Msg};
        {M, F, S} ->
            try
                erlang:apply(M, F, [Details, Arguments, ArgumentsKw, S])
            catch
                Error:Reason:Stacktrace ->
                    ?LOG_ERROR(#{
                        message => "Error applying event message from router",
                        error => Error,
                        reason => Reason,
                        stacktrace => Stacktrace
                    })
            end
    end,
    {noreply, State, ?TIMEOUT};
handle_message_from_router({result, RequestId, Details}, State) ->
    handle_message_from_router({result, RequestId, Details, undefined, undefined}, State);
handle_message_from_router({result, RequestId, Details, Arguments}, State) ->
    handle_message_from_router({result, RequestId, Details, Arguments, undefined}, State);
handle_message_from_router({result, RequestId, Details, Arguments, ArgumentsKw}, State) ->
    {From, _} = get_ref(RequestId, call, State),
    gen_server:reply(From, {ok, Details, Arguments, ArgumentsKw}),
    {noreply, State, ?TIMEOUT};
handle_message_from_router({registered, RequestId, RegistrationId}, #state{ets = Ets} = State) ->
    {From, Args} = get_ref(RequestId, register, State),
    Mfa = maps:get(mfa, Args),
    {Pid, _} = From,
    ets:insert_new(Ets, #registration{id = RegistrationId, mfa = Mfa, pid = Pid}),
    gen_server:reply(From, {ok, RegistrationId}),
    {noreply, State, ?TIMEOUT};
handle_message_from_router({unregistered, RequestId}, #state{ets = Ets} = State) ->
    {From, Args} = get_ref(RequestId, unregister, State),
    RegistrationId = maps:get(reg_id, Args),
    ets:delete(Ets, RegistrationId),
    gen_server:reply(From, ok),
    {noreply, State, ?TIMEOUT};
handle_message_from_router({invocation, RequestId, RegistrationId, Details}, State) ->
    handle_message_from_router(
        {invocation, RequestId, RegistrationId, Details, undefined, undefined}, State
    );
handle_message_from_router({invocation, RequestId, RegistrationId, Details, Arguments}, State) ->
    handle_message_from_router(
        {invocation, RequestId, RegistrationId, Details, Arguments, undefined}, State
    );
handle_message_from_router(
    {invocation, RequestId, RegistrationId, Details, Arguments, ArgumentsKw} = Msg,
    #state{ets = Ets} = State
) ->
    [
        #registration{
            id = RegistrationId,
            mfa = Mfa,
            pid = Pid
        }
    ] = ets:lookup(Ets, RegistrationId),
    NewState =
        case Mfa of
            undefined ->
                % send it to the user process
                Pid ! {awre, Msg},
                State;
            {M, F, S} ->
                try erlang:apply(M, F, [Details, Arguments, ArgumentsKw, S]) of
                    {ok, Options, ResA, ResAKw} ->
                        {ok, NState} = send_to_router(
                            {yield, RequestId, Options, ResA, ResAKw}, State
                        ),
                        NState;
                    {error, Details, Uri, Arguments, ArgumentsKw} ->
                        {ok, NState} = send_to_router(
                            {error, invocation, RequestId, Details, Uri, Arguments, ArgumentsKw},
                            State
                        ),
                        NState;
                    Other ->
                        {ok, NState} = send_to_router(
                            {error, invocation, RequestId, #{<<"result">> => Other},
                                invalid_argument, undefined, undefined},
                            State
                        ),
                        NState
                catch
                    Error:Reason ->
                        {ok, NState} = send_to_router(
                            {error, invocation, RequestId,
                                #{<<"reason">> => io_lib:format("~p:~p", [Error, Reason])},
                                invalid_argument, undefined, undefined},
                            State
                        ),
                        NState
                end
        end,
    {noreply, NewState, ?TIMEOUT};
handle_message_from_router({error, Action, RequestId, Details, Error}, State) ->
    handle_message_from_router(
        {error, Action, RequestId, Details, Error, undefined, undefined}, State
    );
handle_message_from_router({error, Action, RequestId, Details, Error, Arguments}, State) ->
    handle_message_from_router(
        {error, Action, RequestId, Details, Error, Arguments, undefined}, State
    );
%% example: trying to register without authorization:
%% {
%%      error,
%%      register,
%%      1,
%%      #{},
%%      <<"wamp.error.not_authorized">>,
%%      [<<"Permission denied. User 'john.doe' does not have permission 'wamp.register' on 'com.magenta.thing.alias.erase'">>],
%%      #{<<"message">> => <<"Permission denied. User 'john.doe' does not have permission 'wamp.register' on 'com.magenta.thing.alias.erase'">>}
%% }
handle_message_from_router({error, Action, RequestId, Details, Error, Arguments, ArgumentsKw}, State) ->
    ?LOG_ERROR(#{
        message => "Received error message from router",
        action => Action,
        error => Error,
        args => Arguments,
        kwargs => ArgumentsKw
    }),
    {From, _} = get_ref(RequestId, Action, State),
    gen_server:reply(From, {error, Details, Error, Arguments, ArgumentsKw}),
    {noreply, State, ?TIMEOUT};
handle_message_from_router(Msg, State) ->
    ?LOG_ERROR(#{text => "Received unhandled message from router", message => Msg}),
    {noreply, State}.

%
% Session Scope IDs
%
%     ERROR.Request
%     PUBLISH.Request
%     PUBLISHED.Request
%     SUBSCRIBE.Request
%     SUBSCRIBED.Request
%     UNSUBSCRIBE.Request
%     UNSUBSCRIBED.Request
%     CALL.Request
%     CANCEL.Request
%     RESULT.Request
%     REGISTER.Request
%     REGISTERED.Request
%     UNREGISTER.Request
%     UNREGISTERED.Request
%     INVOCATION.Request
%     INTERRUPT.Request
%     YIELD.Request
%
% IDs in the session scope SHOULD be incremented by 1 beginning with 1
% (for each direction - Client-to-Router and Router-to-Client)
%

send_and_ref(Msg, From, Args, State) ->
    {Message, NewState} = create_ref_for_message(Msg, From, Args, State),
    send_to_router(Message, NewState).

send_to_router(Msg, #state{transport = {TMod, TState}} = State) ->
    {ok, NewTState} = TMod:send_to_router(Msg, TState),
    {ok, State#state{transport = {TMod, NewTState}}}.

create_ref_for_message(Msg, From, Args, #state{ets = Ets} = State) ->
    Method =
        case element(1, Msg) of
            connect -> hello;
            El -> El
        end,
    {RequestId, NewState} =
        case Method of
            hello ->
                {hello, State};
            subscribe ->
                Id = State#state.subscribe_id,
                {Id, State#state{subscribe_id = Id + 1}};
            unsubscribe ->
                Id = State#state.unsubscribe_id,
                {Id, State#state{unsubscribe_id = Id + 1}};
            publish ->
                Id = State#state.publish_id,
                {Id, State#state{publish_id = Id + 1}};
            register ->
                Id = State#state.register_id,
                {Id, State#state{register_id = Id + 1}};
            unregister ->
                Id = State#state.unregister_id,
                {Id, State#state{unregister_id = Id + 1}};
            call ->
                Id = State#state.call_id,
                {Id, State#state{call_id = Id + 1}}
        end,
    true =
        case Method of
            publish ->
                true;
            _ ->
                true = ets:insert_new(Ets, #ref{key = {Method, RequestId}, ref = From, args = Args})
        end,

    case is_integer(RequestId) of
        true ->
            {setelement(2, Msg, RequestId), NewState};
        false ->
            {Msg, NewState}
    end.

get_ref(ReqId, Method, #state{ets = Ets}) ->
    Key = {Method, ReqId},
    [#ref{ref = From, args = Args}] = ets:lookup(Ets, Key),
    ets:delete(Ets, Key),
    {From, Args}.

close_connection() ->
    gen_server:cast(self(), terminate).

%% @private
send_ping(St) ->
    send_ping(term_to_binary(make_ref()), St).

%% -----------------------------------------------------------------------------
%% @private
%% @doc
%% Sends a ping message with a reference() as a payload to the client and sent
%% ourselves a ping_timeout message in the future.
%% @end
%% -----------------------------------------------------------------------------
send_ping(Bin, St0) ->
    {ok, St1} = send_to_router({ping, Bin}, St0),
    Timeout = ?PING_TIMEOUT,
    TimerRef = erlang:send_after(Timeout, self(), ping_timeout),
    St2 = St1#state{
        ping_sent = {true, Bin, TimerRef},
        ping_attempts = St1#state.ping_attempts + 1
    },
    {ok, St2}.

stop_timeout(Reason, State) ->
    ?LOG_ERROR(#{
        text => "Connection closing",
        reason => Reason,
        attempt => io_lib:format(
            "~p/~p", [State#state.ping_attempts, State#state.ping_max_attempts]
        )
    }),
    {stop, timeout, State#state{ping_sent = false}}.
