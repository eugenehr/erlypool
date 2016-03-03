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
%% @doc A simple and easy to use generic pooling library

-module(erlypool).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").
-behaviour(gen_server).

-export([start/2, start_link/2, stop/1, borrow/1, borrow/2, release/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start(PoolName, Opts) -> {ok, Pid} | {error, Reason} when
    PoolName :: atom() | {local, Name} | {global, GlobalName} | {via, Module, ViaName},
    Name :: atom(),
    GlobalName :: term(),
    ViaName :: term(),
    Module :: atom(),
    Pid :: pid(),
    Reason :: term(),
    Opts :: proplists:proplist().
%%
%% @doc Start a new standalone pool
%%
start(PoolName, Opts) when is_atom(PoolName) ->
    start({local, PoolName}, Opts);

start(PoolName, Opts) ->
    gen_server:start(PoolName, ?MODULE, Opts, []).


-spec start_link(PoolName, Opts) -> {ok, Pid} | {error, Reason} when
    PoolName :: atom() | {local, Name} | {global, GlobalName} | {via, Module, ViaName},
    Name :: atom(),
    GlobalName :: term(),
    ViaName :: term(),
    Module :: atom(),
    Pid :: pid(),
    Reason :: term(),
    Opts :: proplists:proplist().
%%
%% @doc Start a new pool within a supervision tree
%%
start_link(PoolName, Opts) when is_atom(PoolName) ->
    start_link({local, PoolName}, Opts);

start_link(PoolName, Opts) ->
    gen_server:start_link(PoolName, ?MODULE, Opts, []).


-spec stop(PoolRef) -> ok when
    PoolRef :: Name | {Name, Node} | {global, GlobalName} | {via, Module, ViaName} | pid(),
    Node :: atom(),
    GlobalName :: term(),
    ViaName :: term(),
    Module :: atom().
%%
%% @doc Stop the pool and release it allocated and borrowed objects
%%
stop(PoolRef) ->
    gen_server:stop(PoolRef).


-spec borrow(PoolRef) -> {ok, Object} | {error, Reason} when
    PoolRef :: Name | {Name, Node} | {global, GlobalName} | {via, Module, ViaName} | pid(),
    Node :: atom(),
    GlobalName :: term(),
    ViaName :: term(),
    Module :: atom(),
    Object :: any(),
    Reason :: term().
%%
%% @doc Borrow an object from the pool
%%
borrow(PoolRef) ->
    gen_server:call(PoolRef, borrow).

-spec borrow(PoolRef, Timeout) -> {ok, Object} | {error, Reason} when
    PoolRef :: Name | {Name, Node} | {global, GlobalName} | {via, Module, ViaName} | pid(),
    Node :: atom(),
    GlobalName :: term(),
    ViaName :: term(),
    Module :: atom(),
    Timeout :: integer(),
    Object :: any(),
    Reason :: term().
%%
%% @doc Borrow an object from the pool
%%
borrow(PoolRef, Timeout) ->
    gen_server:call(PoolRef, borrow, Timeout).


-spec release(PoolRef, Object) -> ok | {error, Reason} when
    PoolRef :: Name | {Name, Node} | {global, GlobalName} | {via, Module, ViaName} | pid(),
    Node :: atom(),
    GlobalName :: term(),
    ViaName :: term(),
    Module :: atom(),
    Object :: any(),
    Reason :: term().
%%
%% @doc Return the object to the pool
%%
release(PoolRef, Object) ->
    gen_server:cast(PoolRef, {release, Object}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

-record(state, {min, max, strategy = lifo, module, modstate, free = [], freefun, test_on_borrow, borrowed = []}).

%% @private
init(Opts) ->
    process_flag(trap_exit, true),
    % State params
    Min = proplists:get_value(min_size, Opts, 0),
    Max = proplists:get_value(max_size, Opts, unlimited),
    Strategy = proplists:get_value(strategy, Opts, lifo),
    Module = proplists:get_value(module, Opts),
    {ok, ModState} = erlang:apply(Module, init, []),
    FreeFun = case Strategy of
                  lifo -> fun(Obj, List) -> [Obj | List] end;
                  fifo -> fun(Obj, List) -> List ++ [Obj] end
              end,
    Test = proplists:get_value(test_on_borrow, Opts, true),
    % Initialize minimum number of objects
    State = borrow_free(#state{
        min = Min, max = Max, strategy = Strategy, module = Module, modstate = ModState, freefun = FreeFun, test_on_borrow = Test
    }, Min),
    % io:format("Init: free=~w, borrowed=~w~n", [State#state.free, State#state.borrowed]),
    {ok, State}.

%% @private
handle_call(borrow, {Pid, _}, State) ->
    case borrow_object(State, Pid) of
        {error, Reason} ->
            {reply, {error, Reason}, State};
        {Object, NewState} ->
            % io:format("Borrow: free=~w, borrowed=~w~n", [NewState#state.free, NewState#state.borrowed]),
            {reply, {ok, Object}, NewState}
    end;

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({release, Object}, State) ->
    NewState = release_object(State, Object),
    % io:format("Release: free=~w, borrowed=~w~n", [NewState#state.free, NewState#state.borrowed]),
    {noreply, NewState};

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    NewState = release_object(State, Ref),
    {noreply, NewState};

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
format_status(_Opt, [_PDict, #state{min = Min, max = Max, strategy = Strategy, module = Module, free = Free, test_on_borrow = Test, borrowed = Borrowed}]) ->
    {state, [
        {min, Min},
        {max, Max},
        {strategy, Strategy},
        {module, Module},
        {test_on_borrow, Test},
        {free, Free},
        {borrowed, Borrowed}
    ]}.

%% ===================================================================
%% private functions
%% ===================================================================

borrow_free(State, 0) ->
    State;

borrow_free(#state{module = Module, modstate = ModState, free = Free, freefun = FreeFun} = State, Num) ->
    {ok, Object, NewModState} = erlang:apply(Module, create, [ModState]),
    NewState = State#state{free = FreeFun(Object, Free), modstate = NewModState},
    borrow_free(NewState, Num - 1).


borrow_object(#state{max = Max, module = Module, modstate = ModState, free = [], borrowed = Borrowed} = State, Pid) when Max =:= unlimited; length(Borrowed) < Max ->
    % Create a new object and monitor to the calling process
    {ok, Object, NewModState} = erlang:apply(Module, create, [ModState]),
    borrow_object(State#state{modstate = NewModState}, Pid, [], Object);

borrow_object(#state{free = []}, _Pid) ->
    % Return error if the maximum number of borrowed objects is exceeded
    {error, max_size_exceeded};

borrow_object(#state{free = [Object | Free], test_on_borrow = false} = State, Pid) ->
    % Just take a free object from the pool
    borrow_object(State, Pid, Free, Object);

borrow_object(#state{module = Module, modstate = ModState, free = [Object | Free], test_on_borrow = true} = State, Pid) ->
    case erlang:apply(Module, test, [Object, ModState]) of
        {ok, NewModState} ->
            borrow_object(State#state{modstate = NewModState}, Pid, Free, Object);
        _ ->
            % Remove broken object from the pool and retry
            borrow_object(State#state{free = Free}, Pid)
    end.

borrow_object(State, Pid, Free, Object) ->
    Ref = erlang:monitor(process, Pid),
    NewState = State#state{free = Free, borrowed = [{Object, Ref} | State#state.borrowed]},
    {Object, NewState}.


release_object(#state{min = Min, module = Module, modstate = ModState, free = Free, freefun = FreeFun, borrowed = Borrowed} = State, ObjRef) ->
    case borrow_find([], Borrowed, ObjRef) of
        not_found ->
            State;

        {NewBorrowed, {Object, Ref}} ->
            if
                is_reference(ObjRef) -> ok;
            % Demonitor process if needed
                true -> erlang:demonitor(Ref)
            end,
            % Reset object and put it back to the pool
            {ok, ModState2} = erlang:apply(Module, reset, [Object, ModState]),
            Free2 = FreeFun(Object, Free),
            {NewFree, NewModState} = close_object(Min, Free2, Module, ModState2),
            State#state{modstate = NewModState, free = NewFree, borrowed = NewBorrowed}
    end.


borrow_find(_, [], _) ->
    not_found;

borrow_find(Acc, [{Object, Ref} | Tail], Ref) ->
    {Acc ++ Tail, {Object, Ref}};

borrow_find(Acc, [{Object, Ref} | Tail], Object) ->
    {Acc ++ Tail, {Object, Ref}};

borrow_find(Acc, [H | Tail], Object) ->
    borrow_find(Acc ++ [H], Tail, Object).


close_object(Min, List, _Module, ModState) when length(List) =< Min ->
    {List, ModState};

close_object(Min, [Object | Tail], Module, ModState) ->
    {ok, NewModState} = erlang:apply(Module, close, [Object, ModState]),
    close_object(Min, Tail, Module, NewModState).
