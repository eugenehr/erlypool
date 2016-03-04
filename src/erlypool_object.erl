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
%% @doc Public interface of the objects behavior for the erlypool library

-module(erlypool_object).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").


-callback create(CreateOpts) -> {ok, Object} | {error, Reason} when
    CreateOpts :: any(),
    Object     :: any(),
    Reason     :: term().

-callback test(Object) -> ok | {error, Reason} when
    Object   :: any(),
    Reason   :: term().

-callback reset(Object) -> ok | {error, Reason} when
    Object   :: any(),
    Reason   :: term().

-callback close(Object) -> ok | {error, Reason} when
    Object   :: any(),
    Reason   :: term().
