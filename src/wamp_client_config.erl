%% =============================================================================
%%  wamp_client_config.erl -
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

%% =============================================================================
%% @doc
%%
%% @end
%% =============================================================================
-module(wamp_client_config).

-behaviour(app_config).

-include_lib("kernel/include/logger.hrl").

-define(APP, wamp_client).
-define(ERROR, '$error_badarg').

-export([get/1]).
-export([get/2]).
-export([set/2]).
-export([init/0]).
-export([on_set/2]).
-export([will_set/2]).

-compile({no_auto_import, [get/1]}).

%% =============================================================================
%% API
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc Initialises plum_db configuration
%% @end
%% -----------------------------------------------------------------------------
init() ->
    ok = setup_dependencies(),
    ok = setup_env(),
    ok = app_config:init(?APP, #{callback_mod => ?MODULE}),
    _ = ?LOG_INFO(#{message => "WAMP Client configuration initialised"}),
    ok.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get(Key :: list() | atom() | tuple()) -> term().
get(Key) ->
    app_config:get(?APP, Key).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get(Key :: list() | atom() | tuple(), Default :: term()) -> term().
get(Key, Default) ->
    app_config:get(?APP, Key, Default).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec set(Key :: key_value:key() | tuple(), Value :: term()) -> ok.
set(Key, Value) ->
    app_config:set(?APP, Key, Value).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec will_set(Key :: key_value:key(), Value :: any()) ->
    ok | {ok, NewValue :: any()} | {error, Reason :: any()}.
will_set(_, _) ->
    ok.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec on_set(Key :: key_value:key(), Value :: any()) -> ok.
on_set(_, _) ->
    ok.

%% =============================================================================
%% PRIVATE
%% =============================================================================

%% @private
setup_env() ->
    ok.

%% @private
setup_dependencies() ->
    _ =
        case application:get_env(kernel, applications) of
            {ok, Apps} when is_list(Apps) ->
                [
                    logger:set_application_level(App, Level)
                 || {App, Level} <- Apps
                ];
            undefined ->
                ok;
            {ok, _} ->
                ok
        end,
    ok.
