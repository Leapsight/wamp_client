%% =============================================================================
%%  wamp_client_sup.erl -
%%
%%  Copyright (c) 2016-2021 Leapsight. All rights reserved.
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

-module(wamp_client_sup).

-behaviour(supervisor).

-include("supervision_spec.hrl").

%% API
-export([start_link/1]).
-export([start_link/2]).

%% SUPERVISOR CALLBACKS
-export([init/1]).



%%====================================================================
%% API
%%====================================================================



%% -----------------------------------------------------------------------------
%% @doc It initialises wamp_client configuration
%% @param Config The configuration for the wamp_client
%% @return {ok, pid()} if the supervisor started successfully
%% @return {error, term()} if the supervisor failed to start
%% @end
%% -----------------------------------------------------------------------------
start_link(Config) ->
    _ = application:ensure_all_started(gproc, permanent),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).


%% -----------------------------------------------------------------------------
%% @doc It initialises wamp_client configuration for the given Id
%% @param Id The unique identifier for the wamp_client
%% @param Config The configuration for the wamp_client
%% @return {ok, pid()} if the supervisor started successfully
%% @return {error, term()} if the supervisor failed to start
%% @end
%% -----------------------------------------------------------------------------
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(Id, Config) ->
    _ = application:ensure_all_started(gproc, permanent),
    SupId = wamp_client_utils:build_registration_name([Id, ?MODULE]),
    supervisor:start_link({local, SupId}, ?MODULE, [Id, Config]).



%%====================================================================
%% SUPERVISOR CALLBACKS
%%====================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec init([any()]) -> {ok, {supervisor:strategy(), [supervisor:child_spec()]}}.
init([Id, Config]) ->
    Peers = key_value:get(peers, Config, #{}),

    %% Create the supervisor for the awre_con
    AwreSupId = wamp_client_utils:build_registration_name([Id, awre_sup]),
    AwreSupChild = ?SUPERVISOR(
        AwreSupId,
        awre_sup,
        [AwreSupId],
        permanent,
        5000
    ),

    %% Create the supervisor for the wamp_client_peer
    PeerChildren = maps:fold(
        fun(Name, PeerConfig, Acc) ->
            PeerSupId = wamp_client_utils:build_registration_name([Id, wamp_client_peer_sup, Name]),
            PeerName = wamp_client_utils:build_registration_name([Id, Name]),
            PeerSup = ?SUPERVISOR(
                PeerSupId,
                wamp_client_peer_sup,
                [AwreSupId, PeerSupId, PeerName, PeerConfig, Config],
                permanent,
                5000
            ),
            [PeerSup | Acc]
        end,
        [],
        Peers
    ),

    Children = [AwreSupChild | PeerChildren],

    Specs = {{one_for_one, 5, 60}, Children},
    {ok, Specs};

init([Config]) ->
    init([undefined, Config]).



%% =============================================================================
%% PRIVATE
%% =============================================================================