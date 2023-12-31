%% =============================================================================
%%  wamp_client_peer_exampdefault, le.erl -
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
-module(wamp_client_example).

-include_lib("kernel/include/logger.hrl").

-export([add/4]).
-export([echo/1]).
-export([echo/2]).
-export([echo/3]).
-export([echo/4]).
-export([multiple_results/1]).
-export([circular/3]).
-export([circular_service_error/1]).
-export([unknown_error/1]).
-export([notfound_error/1]).
-export([validation_error/1]).
-export([service_error/1]).
-export([authorization_error/1]).
-export([timeout/3]).
-export([onhello/3]).
-export([onadd/4]).


-spec add(number(), number(), map(), map()) -> number().
add(A, B, _KWArgs, _Opts) ->
    {ok, [A + B], #{}, #{}}.


echo(_Opts) ->
    {ok, [], #{}, #{}}.

echo(KWArgs, _Opts) ->
    {ok, [], KWArgs, #{}}.

echo(Msg, KWArgs, _Opts) ->
    {ok, [Msg], KWArgs, #{}}.

echo(A, B, KWArgs, _Opts) ->
    {ok, [A, B], KWArgs, #{}}.




-spec multiple_results(map()) -> list().

multiple_results(_Opts) ->
    {ok, [1, 2, 3], #{}, #{}}.

-spec circular(any(), map(), map()) ->
                  {ok, any()} | {error, binary(), map()} | no_return().
circular(Msg, KWArgs, _Opts) ->
    {ok, Args, _, _} =
        wamp_client_peer:call(
            default, <<"com.example.echo">>, #{}, [Msg], KWArgs
        ),
    {ok, Args, #{}, #{}}.

-spec circular_service_error(map()) -> {ok, any()} | no_return().
circular_service_error(_Opts) ->
    wamp_client_peer:call(default, <<"com.example.service_error">>).


-spec unknown_error(map()) -> no_return().

unknown_error(_Opts) ->
    error("no match of right hand side value 2").

-spec notfound_error(map()) -> no_return().

notfound_error(_Opts) ->
    {error, <<"com.myservice.error.not_found">>, [], #{}, #{}}.


-spec validation_error(map()) -> no_return().
validation_error(_Opts) ->
    {error, <<"wamp.error.invalid_argument">>, [], #{code => <<"invalid argument">>}, #{}}.

-spec service_error(map()) -> no_return().
service_error(_Opts) ->
    KWArgs =
        #{code => service_error,
          message => <<"Service error">>,
          description => <<"Service Error">>},
    {error, <<"com.myservice.error.internal">>, [], KWArgs, #{}}.


-spec authorization_error(map()) -> no_return().
authorization_error(_Opts) ->
    KWArgs =
        #{code => authorization_error,
          message => <<"Authorization error">>,
          description => <<"Authorization error">>},
    {error, <<"wamp.error.not_authorized">>, [], KWArgs, #{}}.

-spec timeout(integer(), map(), map()) -> ok.
timeout(Timeout, _KWArgs, _Opts) ->
    timer:sleep(Timeout),
    {ok, [], #{}, #{}}.

-spec onhello(any(), map(), map()) -> ok.
onhello(Msg, _KWArgs, _Opts) ->
    ?LOG_DEBUG("event from com.example.onhello ~p.", [Msg]),
    {ok, [], #{}, #{}}.

onadd(A, B, _KWArgs, _Opts) ->
    ?LOG_DEBUG("event from com.example.onadd ~p.", [A + B]),
    {ok, [], #{}, #{}}.
