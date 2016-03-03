-module(erlypool_tests).
-author("Eugene Khrustalev <eugene.khrustalev@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

minmax_test() ->
    % FIFO
    erlypool:start(test1, [{min_size, 3}, {max_size, 5}, {strategy, fifo}, {module, erlypool_testpool}]),
    {ok, object1} = erlypool:borrow(test1),
    {ok, object2} = erlypool:borrow(test1),
    {ok, object3} = erlypool:borrow(test1),
    {ok, object4} = erlypool:borrow(test1),
    {ok, object5} = erlypool:borrow(test1),
    {error, max_size_exceeded} = erlypool:borrow(test1),
    erlypool:stop(test1),
    % LIFO
    erlypool:start(test1, [{min_size, 3}, {max_size, 5}, {module, erlypool_testpool}]),
    {ok, object3} = erlypool:borrow(test1),
    {ok, object2} = erlypool:borrow(test1),
    {ok, object1} = erlypool:borrow(test1),
    erlypool:stop(test1).

borrow_release_fifo_test() ->
    erlypool:start(test2, [{min_size, 3}, {max_size, 5}, {strategy, fifo}, {module, erlypool_testpool}]),
    {ok, object1} = erlypool:borrow(test2),
    {ok, object2} = erlypool:borrow(test2),
    {ok, object3} = erlypool:borrow(test2),
    % Max test
    {ok, object4} = erlypool:borrow(test2),
    {ok, object5} = erlypool:borrow(test2),
    erlypool:release(test2, object4),
    erlypool:release(test2, object5),

    erlypool:release(test2, object3),
    {ok, object4} = erlypool:borrow(test2),

    erlypool:release(test2, object2),
    {ok, object5} = erlypool:borrow(test2),

    erlypool:release(test2, object1),
    {ok, object3} = erlypool:borrow(test2),

    {ok, object2} = erlypool:borrow(test2),
    {ok, object1} = erlypool:borrow(test2),
    erlypool:release(test2, object1),
    erlypool:release(test2, object2),
    erlypool:release(test2, object3),
    erlypool:release(test2, object4),
    erlypool:release(test2, object5),
    erlypool:stop(test2).

borrow_release_lifo_test() ->
    erlypool:start(test3, [{min_size, 5}, {max_size, 5}, {strategy, lifo}, {module, erlypool_testpool}]),
    lists:foreach(fun(_) ->
        {ok, object5} = erlypool:borrow(test3),
        {ok, object4} = erlypool:borrow(test3),
        {ok, object3} = erlypool:borrow(test3),
        {ok, object2} = erlypool:borrow(test3),
        {ok, object1} = erlypool:borrow(test3),

        erlypool:release(test3, object1),
        erlypool:release(test3, object2),
        erlypool:release(test3, object3),
        erlypool:release(test3, object4),
        erlypool:release(test3, object5)

    end, lists:seq(1, 100))
    , erlypool:stop(test3).

borrow_broken_test() ->
    erlypool:start(test4, [{min_size, 3}, {max_size, 6}, {strategy, fifo}, {module, erlypool_testpool}]),
    {ok, object1} = erlypool:borrow(test4),
    {ok, object2} = erlypool:borrow(test4),
    {ok, object3} = erlypool:borrow(test4),
    {ok, object4} = erlypool:borrow(test4),
    {ok, object5} = erlypool:borrow(test4),
    % The last object in pool is broken
    {ok, broken} = erlypool:borrow(test4),
    erlypool:release(test4, broken),
    {ok, object1} = erlypool:borrow(test4),
    % Release unknown object
    erlypool:release(test4, unknown),
    erlypool:stop(test4).


multiprocess_test_() ->
    {timeout, 60,
        fun() ->
            erlypool:start(test5, [{min_size, 3}, {module, erlypool_testpool}, {test_on_borrow, false}]),
            Pids = lists:map(fun(_) ->
                {Pid, _Tag} = spawn_monitor(fun() ->
                    erlypool:borrow(test5, 10)
                end),
                Pid
            end, lists:seq(1, 100)),
            wait_for(Pids),
            erlypool:stop(test5)
        end
    }.

wait_for([]) ->
    ok;

wait_for(List) ->
    List2 = receive
                {'DOWN', _Ref, process, Pid, _Reason} ->
                    lists:delete(Pid, List)
            end,
    wait_for(List2).
