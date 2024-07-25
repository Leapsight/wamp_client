# wamp_client

> This library wraps an existing (incomplete) WAMP client implementation and adds some required features. It is a first approach for a WAMP client implementation in Erlang.


## Handler API
The handler API follows the WAMP APIs.

All callbacks should have at least two arguments (`KWArgs` and `Opts`):

```erlang
-type callback  ::  {module(), FunctionName :: atom()}
    | function(
        (KWArgs :: map(), Opts :: map()) ->
            wamp_return() | wamp_error();
        (Arg1 :: any(), ..., ArgN :: any(), KWArgs :: map(), Opts :: map()) ->
            wamp_return() | wamp_error()
    )
```

Return types:
```erlang
-type wamp_result() ::  {
    ok,
    Args :: list(),
    KWArgs :: map(),
    Details :: map()
}.

-type wamp_error()  ::  {
    error,
    Uri :: binary(),
    Args :: list(),
    KWArgs :: map(),
    Details :: map()
}.
```

## Configuration

### Type Spec and structure

```erlang
{wamp_client, [
    {routers, Routers :: map(Name :: atom() => Router :: router())},
    {defaults, #{
        router => RouterName :: atom(),
        caller => #{
            features => caller_features(),
            options => caller_options()
        },
        callee => #{
            features => caller_features(),
            options => caller_options()
        },
        subscriber => #{
            features => caller_features(),
            options => caller_options()
        },
        publisher => #{
            features => caller_features(),
            options => caller_options()
        }
    }},
    {peers, #{
        PeerName :: atom() => Peer :: peer()
    }}
]}
```

#### Peer
```erlang
#{
    router => RouterName :: atom(),
    pool_size => integer(),
    pool_type => hash | round_robin |direct | random,
    roles => #{
        caller => #{},
        publisher => #{},
        callee => #{
            features => #{},
            registrations => #{URI :: binary() => registration()}
        },
        callee => #{
            features => #{},
            subscriptions => #{URI :: binary() => subscription()}
        }
    }
```

### Complete example
```erlang
[
    %% service conf
    {wamp_client, [
        {routers, #{
            bondy => #{
                hostname => "localhost",
                port => 18082,
                realm => <<"com.leapsight.test">>,
                encoding => json,
                reconnect => true,
                reconnect_max_retries => 10,
                reconnect_backoff_min => 500,
                reconnect_backoff_max => 120000,
                reconnect_backoff_type => jitter
            }
        }},
        {defaults, #{
            router => bondy,
            caller => #{
                features => #{
                    progressive_call_results => false,
                    progressive_calls => false,
                    call_timeout => true,
                    call_canceling => false,
                    caller_identification=> true,
                    call_retries => true
                },
                options => #{
                    timeout => 15000,
                    disclose_me => true
                }
            },
            callee => #{
                features => #{
                    progressive_call_results => false,
                    progressive_calls => false,
                    call_timeout => true,
                    call_canceling => false,
                    caller_identification => true,
                    call_trustlevels => true,
                    registration_revocation => true,
                    session_meta_api => true,
                    pattern_based_registration => true,
                    shared_registration => true,
                    sharded_registration => true
                },
                options => #{
                    disclose_caller => true,
                    invoke => roundrobin
                }
            },
            publisher => #{
                features => #{
                    message_retention => true,
                    publisher_exclusion => true,
                    publisher_identification => true,
                    subscriber_blackwhite_listing => true
                }
            },
            subscriber => #{
                features => #{
                    event_history => false,
                    pattern_based_subscription => true,
                    publication_trustlevels => true,
                    publisher_identification => true,
                    sharded_subscription => true
                }
            }
        }},
        {peers, #{
            default => #{
                router => bondy,
                pool_size => 3,
                pool_type => hash,
                roles => #{
                    caller => #{},
                    publisher => #{},
                    callee => #{
                        features => #{},
                        registrations => #{
                            <<"com.example.add2">> => #{
                                options => #{
                                    disclose_caller => true,
                                    invoke => roundrobin
                                },
                                handler => {wamp_client_example, add}
                            },
                            <<"com.example.echo">> =>#{
                                handler => {wamp_client_example, echo}
                            },
                            <<"com.example.multiple">> =>#{
                                handler => {wamp_client_example, multiple_results}
                            },
                            <<"com.example.circular">> =>#{
                                handler => {wamp_client_example, circular}
                            },
                            <<"com.example.circular_service_error">> =>#{
                                handler => {wamp_client_example, circular_service_error}
                            },
                            <<"com.example.unknown_error">> =>#{
                                handler => {wamp_client_example, unknown_error}
                            },
                            <<"com.example.notfound_error">> =>#{
                                handler => {wamp_client_example, notfound_error}
                            },
                            <<"com.example.validation_error">> =>#{
                                handler => {wamp_client_example, validation_error}
                            },
                            <<"com.example.service_error">> =>#{
                                handler => {wamp_client_example, service_error}
                            },
                            <<"com.example.authorization_error">> =>#{
                                handler => {wamp_client_example, authorization_error}
                            },
                            <<"com.example.timeout">> =>#{
                                handler => {wamp_client_example, timeout}
                            }
                        }
                    },
                    subscriber => #{
                        features => #{},
                        subscriptions => #{
                            <<"com.example.onhello">> =>#{
                                handler => {wamp_client_example, onhello}
                            },
                            <<"com.example.onadd">> => #{
                                handler => {wamp_client_example, onadd}
                            }
                        }
                    }
                }
            }
        }}
    ]},

    {awre, [
        {erlbin_number, 15}
    ]},

    {kernel, [
        {logger, [
          {handler, default, logger_std_h,
            #{formatter => {log_formatter, #{single_line => true, depth => 10 }}}
          }
        ]},
        {logger_level, info}
    ]}
].
```


