#!/usr/bin/env escript
%%! -pa ebin

%% Meant to be called from the parent directory.

-mode(compile).

-include("bench.hrl").

-define(TRIALS, 1000000).

main(_) ->
    test_event(),
    test_merge(),
    test_compare().

test_event() ->
    VC = lists:foldl(fun evc:event/2, evc:new(), [a, b, c, d, e, f]),
    bench("event", fun() -> evc:event(a, VC) end, ?TRIALS).

test_merge() ->
    VC1 = lists:foldl(fun evc:event/2, evc:new(), [a, b, c, d, e, f]),
    VC2 = lists:foldl(fun evc:event/2, VC1, [a, b, c, d, e, f]),
    bench("merge", fun() -> evc:merge(VC1, VC2) end, ?TRIALS).

test_compare() ->
    VC1 = lists:foldl(fun evc:event/2, evc:new(), [a, b, c, d, e, f]),
    VC2 = lists:foldl(fun evc:event/2, VC1, [a, b, c, d, e, f]),
    bench("compare", fun() -> evc:compare(VC1, VC2) end, ?TRIALS).
