%% =============================================================================
%%  wamp_client_peer_sup.erl -
%%
%%  Copyright (c) 2018-2021 Leapsight. All rights reserved.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-module(wamp_client_peer_sup).

-behaviour(supervisor).

-define(SUPERVISOR(Id, Args, Restart, Timeout),
        #{id => Id,
          start => {Id, start_link, Args},
          restart => Restart,
          shutdown => Timeout,
          type => supervisor,
          modules => [Id]}).
-define(WORKER(Id, Args, Restart, Timeout),
        #{id => Id,
          start => {Id, start_link, Args},
          restart => Restart,
          shutdown => Timeout,
          type => worker,
          modules => [Id]}).
-define(EVENT_MANAGER(Id, Restart, Timeout),
        #{id => Id,
          start => {gen_event, start_link, [{local, Id}]},
          restart => Restart,
          shutdown => Timeout,
          type => worker,
          modules => [dynamic]}).

%% API
-export([start_link/3]).
%% SUPERVISOR CALLBACKS
-export([init/1]).

%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
start_link(SupName, PeerName, Peer) ->
    NameString = atom_to_list(PeerName),
    {ok, Sup} = OK = supervisor:start_link({local, SupName}, ?MODULE, [Peer]),

    %% Create the worker pool
    Size = maps:get(pool_size, Peer, 1),
    Type = maps:get(pool_type, Peer, round_robin),
    ok = gproc_pool:new(PeerName, Type, [{size, Size}]),

    %% Add peer instances
    L = lists:seq(1, Size),

    _ = [begin
             WorkerName =
                 list_to_atom(NameString ++ "-" ++ integer_to_list(Id)),
             {ok, _} = supervisor:start_child(Sup, [PeerName, WorkerName]),
             ok
         end
         || Id <- L],

    OK.

%% =============================================================================
%% SUPERVISOR CALLBACKS
%% =============================================================================

init([Peer]) ->
    Children = [?WORKER(wamp_client_peer, [Peer], temporary, 5000)],
    Specs = {{simple_one_for_one, 5, 60}, Children},
    {ok, Specs}.

%% =============================================================================
%% PRIVATE
%% =============================================================================
