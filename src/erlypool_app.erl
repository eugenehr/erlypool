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
%% @doc A simple and easy to use generic pooling library application

-module(erlypool_app).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").
-behaviour(application).

-export([start/2, stop/1]).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

start(_Type, _Args) ->
    erlypool_sup:start_link().

stop(_Start) ->
    ok.
