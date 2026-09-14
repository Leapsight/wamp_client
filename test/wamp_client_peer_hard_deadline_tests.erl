%% =============================================================================
%%  wamp_client_peer_hard_deadline_tests.erl -
%%
%%  Infra-free EUnit tests for the hard-deadline safety net in
%%  wamp_client_peer:call/5. These do NOT require a running Bondy router:
%%  a fake worker process stands in for the peer, and awre:call/5 is mocked
%%  with meck so each scenario is exercised deterministically.
%% =============================================================================
-module(wamp_client_peer_hard_deadline_tests).

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------
%% Fake peer worker: answers `connection` gen_server calls with a stand-in
%% Conn pid, bypassing the need for a real router connection.
%% -----------------------------------------------------------------------------

fake_worker_start(Conn) ->
    spawn(fun() -> fake_worker_loop(Conn) end).

fake_worker_loop(Conn) ->
    receive
        {'$gen_call', From, connection} ->
            gen_server:reply(From, Conn),
            fake_worker_loop(Conn);
        stop ->
            ok
    end.

%% -----------------------------------------------------------------------------
%% Fixture
%% -----------------------------------------------------------------------------

setup() ->
    meck:new(awre, [passthrough, no_link]),
    ConnPid = spawn(fun() -> receive after infinity -> ok end end),
    WorkerPid = fake_worker_start(ConnPid),
    {WorkerPid, ConnPid}.

teardown({WorkerPid, ConnPid}) ->
    catch meck:unload(awre),
    exit(WorkerPid, kill),
    exit(ConnPid, kill),
    ok.

hard_deadline_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun normal_success_case/1,
            fun normal_wamp_timeout_case/1,
            fun hard_deadline_kills_stuck_worker/1,
            fun generic_crash_still_propagates/1
        ]}.

%% -----------------------------------------------------------------------------
%% Compatibility: a normal, fast, successful call is untouched by the wrapper.
%% -----------------------------------------------------------------------------

normal_success_case({WorkerPid, _ConnPid}) ->
    meck:expect(awre, call, fun(_Conn, _Opts, _Uri, _Args, _KWArgs) ->
        {ok, #{}, [<<"pong">>], undefined}
    end),

    Result = wamp_client_peer:call(
        WorkerPid, <<"com.test.echo">>, #{timeout => 1000}, [], #{}
    ),

    ?_assertMatch({ok, [<<"pong">>], undefined, #{}}, Result).

%% -----------------------------------------------------------------------------
%% Compatibility: the existing WAMP-level (awre-internal) timeout path is
%% untouched — same error shape as before this change.
%% -----------------------------------------------------------------------------

normal_wamp_timeout_case({WorkerPid, _ConnPid}) ->
    meck:expect(awre, call, fun(_Conn, _Opts, _Uri, _Args, _KWArgs) ->
        exit({timeout, {gen_server, call, [dummy, msg, 1000]}})
    end),

    Result = wamp_client_peer:call(
        WorkerPid, <<"com.test.echo">>, #{timeout => 1000}, [], #{}
    ),

    ?_assertMatch({error, <<"wamp.error.timeout">>, [], _, #{}}, Result).

%% -----------------------------------------------------------------------------
%% THE FIX: awre:call/5 hangs forever (simulating a stuck connection, no
%% local timeout ever firing) — the caller must still return, bounded, and
%% the worker process spawned to run the call must actually be killed.
%%
%% Run this against the CURRENT (unpatched) call/5 first: it must FAIL by
%% hanging past the test timeout, proving there's no safety net today.
%% After the patch, it must PASS within a couple hundred ms of the hard
%% deadline.
%% -----------------------------------------------------------------------------

hard_deadline_kills_stuck_worker({WorkerPid, _ConnPid}) ->
    Self = self(),

    meck:expect(awre, call, fun(_Conn, _Opts, _Uri, _Args, _KWArgs) ->
        Self ! {stuck_worker, self()},
        receive
        after infinity -> ok
        end
    end),

    Started = erlang:monotonic_time(millisecond),

    Result = wamp_client_peer:call(
        WorkerPid, <<"com.test.hang">>, #{timeout => 500}, [], #{}
    ),

    Elapsed = erlang:monotonic_time(millisecond) - Started,

    StuckPid =
        receive
            {stuck_worker, P} -> P
        after 200 ->
            error(stuck_worker_never_started)
        end,

    %% Give the kill signal a moment to actually land.
    timer:sleep(50),

    [
        ?_assertMatch({error, <<"wamp.error.timeout">>, [], _, #{}}, Result),
        %% Must resolve close to the hard deadline (timeout=500 + the 5000ms
        %% margin defined in wamp_client_peer.erl == ~5500ms), NOT hang
        %% indefinitely. Generous upper bound to absorb scheduler jitter.
        ?_assert(Elapsed < 7000),
        %% The process actually blocked on awre:call must be gone —
        %% otherwise this is just a leak with a lipstick error tuple.
        ?_assertNot(is_process_alive(StuckPid))
    ].

%% -----------------------------------------------------------------------------
%% A genuine crash (not a timeout) inside awre:call/5 must still surface as
%% a crash, not get silently reported as "wamp.error.timeout" — the
%% {'DOWN', ...} branch exists specifically to prevent that mislabeling.
%% -----------------------------------------------------------------------------

generic_crash_still_propagates({WorkerPid, _ConnPid}) ->
    meck:expect(awre, call, fun(_Conn, _Opts, _Uri, _Args, _KWArgs) ->
        error(simulated_crash)
    end),

    ?_assertError(
        _,
        wamp_client_peer:call(WorkerPid, <<"com.test.crash">>, #{timeout => 1000}, [], #{})
    ).
