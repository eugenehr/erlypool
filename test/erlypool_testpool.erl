-module(erlypool_testpool).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").
-behavior(erlypool_object).

%% API
-export([create/1, test/1, reset/1, close/1]).


create(_) ->
    case erlang:get(idx) of
        undefined ->
            Objects = [object1, object2, object3, object4, object5, broken],
            erlang:put(objects, Objects),
            erlang:put(idx, 1),
            {ok, lists:nth(1, Objects)};

        N ->
            Objects = erlang:get(objects),
            N2 = if
                N >= length(Objects) -> 0;
                true -> N
            end,
            erlang:put(idx, N2+1),
            {ok, lists:nth(N2+1, Objects)}
    end.

test(broken) ->
    {error, broken};

test(_Object) ->
    ok.

reset(_Object) ->
    ok.

close(_Object) ->
    ok.


