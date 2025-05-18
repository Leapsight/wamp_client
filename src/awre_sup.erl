%%
%% Copyright (c) 2015 Bas Wegh
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%

%% @private

-module(awre_sup).

-behaviour(supervisor).

-include("supervision_spec.hrl").

%% API
-export([start_link/0]).
-export([start_link/1]).

%% SUPERVISOR CALLBACKS
-export([init/1]).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [awre_con]).


%% -----------------------------------------------------------------------------
%% @doc
%% @param Id The unique identifier for the awre supervisor
%% @end
%% -----------------------------------------------------------------------------
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(Id) ->
	supervisor:start_link({local, Id}, ?MODULE, [Id, awre_con]).



%% =============================================================================
%% SUPERVISOR CALLBACKS
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec init([atom()]) -> {ok, {supervisor:strategy(), [supervisor:child_spec()]}}.
init(Ids) ->
	AwreConId = wamp_client_utils:build_registration_name(Ids),
	Children = [?WORKER(AwreConId, awre_con, [], temporary, 5000)],
	Specs = {{simple_one_for_one, 10, 10}, Children},
	{ok, Specs}.
