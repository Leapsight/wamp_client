%% =============================================================================
%%  wamp_client_sensitive.erl -
%%
%%  Copyright (c) 2025 Leapsight. All rights reserved.
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
%% @doc This module provides utilities for handling sensitive data in WAMP client
%% to prevent accidental exposure of passwords, private keys, and other sensitive
%% information in logs, crash dumps, or debugging output.
%% 
%% Based on bondy_sensitive module pattern.
%% @end
%% -----------------------------------------------------------------------------
-module(wamp_client_sensitive).

-type sensitive()   ::  {sensitive, fun()}.

-export_type([sensitive/0]).

-export([conforms/1]).
-export([format_status/2]).
-export([raise/3]).
-export([unwrap/1]).
-export([wrap/1]).


%% =============================================================================
%% CALLBACKS
%% =============================================================================


-callback format_status(State :: term()) -> NewState :: term().


%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc Returns true if module `Mod' conforms with this behaviour.
%% @end
%% -----------------------------------------------------------------------------
-spec conforms(Mod :: module()) -> boolean().

conforms(Mod) ->
    erlang:function_exported(Mod, format_status, 1).


%% -----------------------------------------------------------------------------
%% @doc Formalises and extends the use of the callback `format_status/1'
%% gen_server callback to modules managing sensitive data.
%% 
%% A callback module managing sensitive data can use this function to sanitize
%% the state before it's exposed in status reports or crash dumps.
%% @end
%% -----------------------------------------------------------------------------
-spec format_status(Mod :: module(), State :: term()) -> NewState :: term().

format_status(Mod, State) ->
     case conforms(Mod) of
        true ->
            case catch Mod:format_status(State) of
                {'EXIT', _} ->
                    State;
                Formatted ->
                    Formatted
            end;

        false ->
            State
    end.


%% -----------------------------------------------------------------------------
%% @doc Wraps sensitive data to prevent accidental exposure.
%% The data is wrapped in a function to defer evaluation and prevent
%% the actual value from appearing in crash dumps or logs.
%% @end
%% -----------------------------------------------------------------------------
-spec wrap(Term :: term() | fun(() -> term())) -> sensitive().

wrap(Fun) when is_function(Fun, 0) ->
    {sensitive, fun() -> Fun() end};

wrap(Term) ->
    {sensitive, fun() -> Term end}.


%% -----------------------------------------------------------------------------
%% @doc Unwraps sensitive data for actual use.
%% @end
%% -----------------------------------------------------------------------------
-spec unwrap(sensitive() | term()) -> term().

unwrap({sensitive, Fun}) when is_function(Fun, 0) ->
    Fun();

unwrap(Term) ->
    %% Handle case where data is not wrapped (e.g., for testing or backward compatibility)
    Term.


%% -----------------------------------------------------------------------------
%% @doc Raises an exception while sanitizing the stacktrace to avoid
%% exposing sensitive data in function arguments.
%% @end
%% -----------------------------------------------------------------------------
-spec raise(
    Class :: error | exit | throw,
    Reason :: term(),
    Stacktrace :: erlang:raise_stacktrace()) -> badarg.

raise(Class, Reason, Stacktrace0) ->
    Stacktrace = prune_stacktrace(Stacktrace0),
    erlang:raise(Class, Reason, Stacktrace).


%% =============================================================================
%% PRIVATE
%% =============================================================================


%% @private
prune_stacktrace([{M, F, [_ | _] = A, Info} | Rest]) ->
    %% We strip the function arguments and replaced them by the arity
    [{M, F, length(A), Info} | Rest];

prune_stacktrace(Stacktrace) ->
    Stacktrace.