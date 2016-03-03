%% Copyright Eugene Khrustalev 2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @author Eugene Khrustalev <eugene.khrustalev@gmail.com>
%% @doc A simple and easy to use generic pooling library supervisor

-module(erlypool_sup).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% supervisor callbacks
%% ===================================================================

-define(CHILD(Name, Opts), {Name, {erlypool, start_link, [Name, Opts]}, permanent, 5000, worker, [Name]}).

init([]) ->
    Children = case application:get_env(erlypool, pools) of
        undefined -> [];
        {ok, Pools} -> children(Pools, [])
    end,
    {ok, {{one_for_one, 5, 10}, Children}}.

children([], ChildSpecs) ->
    ChildSpecs;

children([{Name, Opts} | Tail], ChildSpecs) ->
    io:format("Name: ~w, Opts: ~w~n", [Name, Opts]),
    children(Tail, ChildSpecs ++ [?CHILD(Name, Opts)]).
