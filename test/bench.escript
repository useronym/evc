#!/usr/bin/env escript
%%! -pa ebin

%% Meant to be called from the parent directory.

-mode(compile).

-include("bench.hrl").

-define(TRIALS, 1000000).

main(_) ->
    test_all(evc_maps),
    test_all(evc_riak),
    test_all(evc_dict).

test_all(Mod) ->
    io:format("~p:~n", [Mod]),
    test_event(Mod),
    test_merge_small(Mod),
    test_compare_small(Mod),
    test_merge_large(Mod),
    test_compare_large(Mod).

test_event(Mod) ->
    VC = lists:foldl(fun Mod:event/2, Mod:new(), [a, b, c, d, e, f]),
    bench("event", fun() -> Mod:event(a, VC) end, ?TRIALS).

test_merge_small(Mod) ->
    VC1 = lists:foldl(fun Mod:event/2, Mod:new(), [a, b, c, d]),
    VC2 = lists:foldl(fun Mod:event/2, VC1, [a, b, c, d]),
    bench("merge-small", fun() -> Mod:merge(VC1, VC2) end, ?TRIALS).

test_compare_small(Mod) ->
    VC1 = lists:foldl(fun Mod:event/2, Mod:new(), [a, b, c, d]),
    VC2 = lists:foldl(fun Mod:event/2, VC1, [a, b, c, d]),
    bench("compare-small", fun() -> Mod:compare(VC1, VC2) end, ?TRIALS).

test_merge_large(Mod) ->
    VC1 = lists:foldl(fun Mod:event/2, Mod:new(), [a, b, c, d, e, f, g, h, i, j]),
    VC2 = lists:foldl(fun Mod:event/2, VC1, [a, b, c, d, e, f, g, h, i, j]),
    bench("merge-large", fun() -> Mod:merge(VC1, VC2) end, ?TRIALS).

test_compare_large(Mod) ->
    VC1 = lists:foldl(fun Mod:event/2, Mod:new(), [a, b, c, d, e, f, g, h, i, j]),
    VC2 = lists:foldl(fun Mod:event/2, VC1, [a, b, c, d, e, f, g, h, i, j]),
    bench("compare-large", fun() -> Mod:compare(VC1, VC2) end, ?TRIALS).
