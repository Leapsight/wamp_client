-define(SUPERVISOR(Id, Mod, Args, Restart, Timeout), #{
    id => Id,
    start => {Mod, start_link, Args},
    restart => Restart,
    shutdown => Timeout,
    type => supervisor,
    modules => [Mod]
}).

-define(WORKER(Id, Mod, Args, Restart, Timeout), #{
    id => Id,
    start => {Mod, start_link, Args},
    restart => Restart,
    shutdown => Timeout,
    type => worker,
    modules => [Mod]
}).
