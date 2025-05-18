-module(wamp_client_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

groups() ->
    [
        {circular, [parallel], lists:map(fun(_) -> circular_test end, lists:seq(1, 100))},
        {parallel_echo, [parallel],
            lists:map(fun(_) -> parallel_echo_test end, lists:seq(1, 100))},
        {unregister_register, [parallel],
            lists:map(fun(_) -> unregister_register_test end, lists:seq(1, 50))}
    ].

all() ->
    [
        echo_test,
        multiple_results_test,
        circular_service_error,
        unknown_error_test,
        notfound_error_test,
        validation_error_test,
        service_error_test,
        authorization_error_test,
        dynamic_register,
        timeout_error_test,
        publish_test,
        % This test must be run with a pool == 1
        % override_registered_procedure,

        {group, parallel_echo},
        {group, circular},
        {group, unregister_register},

        long_call_test
    ].

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(wamp_client),
    timer:sleep(2000),
    Config.

end_per_suite(_Config) ->
    application:stop(wamp_client),
    ok.

echo_test(_) ->
    Msg = <<"Hello, world!">>,
    ?assertMatch(
        {ok, [Msg], _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.echo">>,
            #{},
            [Msg]
        )
    ),
    ?assertMatch(
        {ok, [Msg], _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.echo">>,
            #{},
            [Msg],
            #{}
        )
    ).

multiple_results_test(_) ->
    ?assertMatch(
        {ok, [1, 2, 3], _, _},
        wamp_client_peer:call(default, <<"com.example.multiple">>)
    ),

    ?assertMatch(
        {ok, [1, 2, 3], _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.multiple">>,
            #{},
            []
        )
    ),

    ?assertMatch(
        {ok, [1, 2, 3], _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.multiple">>,
            #{},
            [],
            #{}
        )
    ).

circular_test(_) ->
    Ref = rand:uniform(1 bsl 64),
    ?assertMatch(
        {ok, [Ref], _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.circular">>,
            #{timeout =>30000},
            [Ref]
        )
    ).

circular_service_error(_) ->
    ?assertMatch(
        {error, <<"com.myservice.error.internal">>, _, _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.circular_service_error">>
        )
    ).

unknown_error_test(_) ->
    ?assertMatch(
        {error, <<"com.myservice.error.internal">>, _, _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.unknown_error">>
        )
    ).

notfound_error_test(_) ->
    ?assertMatch(
        {error, <<"com.myservice.error.not_found">>, _, _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.notfound_error">>
        )
    ).

validation_error_test(_) ->
    Expected = <<"wamp.error.invalid_argument">>,
    Result =
        wamp_client_peer:call(
            default,
            <<"com.example.validation_error">>
        ),
    ?assertEqual(Expected, element(2, Result)).

service_error_test(_) ->
    ?assertMatch(
        {error, <<"com.myservice.error.internal">>, _, _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.service_error">>
        )
    ).

authorization_error_test(_) ->
    ?assertMatch(
        {error, <<"wamp.error.not_authorized">>, _, _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.authorization_error">>
        )
    ).

timeout_error_test(_) ->
    ?assertMatch(
        {error, <<"wamp.error.timeout">>, _, _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.timeout">>,
            #{timeout => 1000},
            [3000]
        )
    ).

override_registered_procedure(_) ->
    %% Already Registered
    Uri = <<"com.example.echo">>,
    Fun = fun(_, _, _) -> {ok, [<<"new_echo">>], #{}, #{}} end,
    RegOpts = #{
        invoke => roundrobin
    },

    {ok, _} = wamp_client_peer:register(default, Uri, RegOpts, Fun),
    ?assertMatch(
        {ok, [<<"old_echo">>], _, _},
        wamp_client_peer:call(
            default,
            Uri,
            #{},
            [<<"old_echo">>]
        )
    ),

    {ok, _} = wamp_client_peer:unregister(default, Uri),

    {ok, _} = wamp_client_peer:register(default, Uri, RegOpts, Fun),

    ?assertMatch(
        #{handler := {Fun, _}},
        wamp_client_peer:registration_state(default, Uri)
    ),
    ?assertMatch(
        {ok, [<<"new_echo">>], _, _},
        wamp_client_peer:call(
            default,
            Uri,
            #{},
            [<<"old_echo">>]
        )
    ),

    {ok, _} = wamp_client_peer:unregister(default, Uri),
    {ok, _} =
        wamp_client_peer:register(
            default,
            Uri,
            RegOpts,
            {wamp_client_example, echo}
        ),
    ?assertMatch(
        {ok, [<<"old_echo">>], _, _},
        wamp_client_peer:call(
            default,
            Uri,
            #{},
            [<<"old_echo">>]
        )
    ).

dynamic_register(_) ->
    RegOpts = #{
        invoke => roundrobin
    },
    {ok, _} =
        wamp_client_peer:register(
            default,
            <<"com.example.echo1">>,
            RegOpts,
            fun(X, _, _) -> {ok, [X], #{}, #{}} end
        ),
    %% wait for registration
    timer:sleep(100),
    Msg = <<"Hello, world!">>,
    ?assertMatch(
        {ok, [Msg], _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.echo1">>,
            #{},
            [Msg]
        )
    ).

parallel_echo_test(_) ->
    Msg = <<"Hello, world!">>,
    ?assertMatch(
        {ok, [Msg], _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.echo">>,
            #{},
            [Msg]
        )
    ).

unregister_register_test(_) ->
    N = erlang:unique_integer([positive, monotonic]),
    Uri = <<"com.example.echo.", (integer_to_binary(N))/binary>>,
    RegOpts = #{
        invoke => roundrobin
    },
    {ok, _} =
        wamp_client_peer:register(
            default,
            Uri,
            RegOpts,
            fun(_, _, _) ->
                timer:sleep(500),
                {ok, [<<"pong">>], #{}, #{}}
            end
        ),
    timer:sleep(1000),
    Msg = <<"Hello, world!">>,
    ?assertMatch(
        {ok, [<<"pong">>], _, _},
        wamp_client_peer:call(default, Uri, #{}, [Msg])
    ),
    {ok, _} = wamp_client_peer:unregister(default, Uri).

publish_test(_) ->
    % dbg:tracer(), dbg:p(all, c),
    % dbg:tpl(wamp_client_peer, x),
    % dbg:tpl(gproc_pool, pick, x),
    ok =
        wamp_client_peer:publish(
            default,
            <<"com.example.onhello">>,
            [<<"Hello wamp!">>],
            #{},
            #{}
        ),
    ok =
        wamp_client_peer:publish(
            default,
            <<"com.example.onadd">>,
            [1, 2],
            #{},
            #{}
        ).

long_call_test(_) ->
    ?assertMatch(
        {ok, _, _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.timeout">>,
            #{timeout => 20000},
            [10000]
        )
    ),
    ?assertMatch(
        {error, <<"wamp.error.timeout">>, _, _, _},
        wamp_client_peer:call(
            default,
            <<"com.example.timeout">>,
            #{timeout => 20000},
            [30000]
        )
    ).
