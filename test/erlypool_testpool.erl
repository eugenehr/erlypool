-module(erlypool_testpool).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").
-behavior(erlypool_object).

%% API
-export([init/0, create/1, test/2, reset/2, close/2]).

-record(state, {idx = 0, objects}).

init() ->
    State = #state{idx = 0, objects = [
        object1, object2, object3, object4, object5, broken
    ]},
    {ok, State}.

create(#state{idx = Idx, objects = Objects} = State) when Idx >= length(Objects) ->
    create(State#state{idx = 0});

create(#state{idx = Idx, objects = Objects} = State) ->
    Object = lists:nth(Idx + 1, Objects),
    {ok, Object, State#state{idx = Idx + 1}}.

test(broken, _State) ->
    {error, broken};

test(_Object, State) ->
    {ok, State}.

reset(_Object, State) ->
    {ok, State}.

close(_Object, State) ->
    {ok, State}.


