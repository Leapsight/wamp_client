%% =============================================================================
%%  wamp_client_app.erl -
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
-module(wamp_client_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

% tracer() ->
%     dbg:tracer(),
%     dbg:p(all, c),
%     dbg:tpl(awre, 'connect', x),
%     dbg:tpl(awre_con, '_', x),
%     dbg:tpl(awre_trans_tcp, '_', x).
%     dbg:tpl(wamp_client_peer, '_', x).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc Initialises plum_db configuration
%% @end
%% -----------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    % tracer(),
    ok = wamp_client_config:init(),
    case wamp_client_sup:start_link() of
        {ok, _} = OK ->
            OK;
        {error, _} = Error ->
            Error
    end.


%% -----------------------------------------------------------------------------
%% @doc Initialises plum_db configuration
%% @end
%% -----------------------------------------------------------------------------
stop(_State) ->
    ok.
