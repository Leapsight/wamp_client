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

-module(wamp_client_peer_sup).

-behaviour(supervisor).

-include("supervision_spec.hrl").

%% API
-export([start_link/5]).

%% SUPERVISOR CALLBACKS
-export([init/1]).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc It initialises wamp_client peer configuration
%% @param AwreSupId The unique identifier for the awre supervisor
%% @param PeerSupId The unique identifier for the peer supervisor
%% @param PeerName The name of the peer
%% @param PeerConfig The configuration for the peer
%% @param GlobalConfig The global configuration for the wamp_client
%% @return {ok, pid()} if the supervisor started successfully
%% @return {error, term()} if the supervisor failed to start
%% @end
%% -----------------------------------------------------------------------------
-spec start_link(atom(), atom(), atom(), map(), map()) -> {ok, pid()} | {error, term()}.
start_link(AwreSupId, PeerSupId, PeerName, PeerConfig, GlobalConfig) ->
    NameString = atom_to_list(PeerName),
    {ok, Sup} = OK = supervisor:start_link({local, PeerSupId}, ?MODULE, [AwreSupId, PeerConfig, GlobalConfig]),

    %% Create the worker pool
    Size = maps:get(pool_size, PeerConfig, 1),
    Type = maps:get(pool_type, PeerConfig, round_robin),
    ok = gproc_pool:new(PeerName, Type, [{size, Size}]),

    %% Add peer instances
    L = lists:seq(1, Size),

    %% Create the workers
    _ = [begin
            WorkerName = list_to_atom(NameString ++ "-" ++ integer_to_list(Id)),
            {ok, _} = supervisor:start_child(Sup, [PeerName, WorkerName]),
            ok
        end || Id <- L],

    OK.



%% =============================================================================
%% SUPERVISOR CALLBACKS
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec init([any()]) -> {ok, {supervisor:strategy(), [supervisor:child_spec()]}}.
init([AwreSupId, PeerConfig, GlobalConfig]) ->
    Children = [?WORKER(wamp_client_peer, wamp_client_peer, [AwreSupId, PeerConfig, GlobalConfig], permanent, 5000)],
    Specs = {{simple_one_for_one, 5, 60}, Children},
    {ok, Specs}.



%% =============================================================================
%% PRIVATE
%% =============================================================================
