#!/usr/bin/env escript
%%! -pa ebin

%% Meant to be called from the parent directory.

-mode(compile).

-include("bench.hrl").

-define(TRIALS, 1000000).

main(_) ->
    test_increment(),
    test_merge_small(),
    test_compare_small(),
    test_merge_large(),
    test_compare_large().

test_increment() ->
    VC = lists:foldl(fun evc:increment/2, evc:new(), [a, b, c, d, e, f]),
    bench("increment", fun() -> evc:increment(a, VC) end, ?TRIALS).

test_merge_small() ->
    VC1 = lists:foldl(fun evc:increment/2, evc:new(), [a, b, c, d]),
    VC2 = lists:foldl(fun evc:increment/2, VC1, [a, b, c, d]),
    bench("merge-small", fun() -> evc:merge(VC1, VC2) end, ?TRIALS).

test_compare_small() ->
    VC1 = lists:foldl(fun evc:increment/2, evc:new(), [a, b, c, d]),
    VC2 = lists:foldl(fun evc:increment/2, VC1, [a, b, c, d]),
    bench("compare-small", fun() -> evc:compare(VC1, VC2) end, ?TRIALS).

test_merge_large() ->
    VC1 = lists:foldl(fun evc:increment/2, evc:new(), [a, b, c, d, e, f, g, h, i, j]),
    VC2 = lists:foldl(fun evc:increment/2, VC1, [a, b, c, d, e, f, g, h, i, j]),
    bench("merge-large", fun() -> evc:merge(VC1, VC2) end, ?TRIALS).

test_compare_large() ->
    VC1 = lists:foldl(fun evc:increment/2, evc:new(), [a, b, c, d, e, f, g, h, i, j]),
    VC2 = lists:foldl(fun evc:increment/2, VC1, [a, b, c, d, e, f, g, h, i, j]),
    bench("compare-large", fun() -> evc:compare(VC1, VC2) end, ?TRIALS).
