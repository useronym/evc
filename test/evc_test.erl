%%%-------------------------------------------------------------------
%%% @author entity
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2015 13:44
%%%-------------------------------------------------------------------
-module(evc_test).
-author("entity").

-include_lib("eunit/include/eunit.hrl").

base_test() ->
    ?assertEqual(0, evc:get_counter(aaa, evc:new())),
    VC = evc:increment(aaa, evc:new()),
    ?assertEqual(1, evc:get_counter(aaa, VC)).

merge_test() ->
    A = evc:increment(a, evc:new()),
    B = evc:increment(b, evc:new()),
    ?assertEqual([a, b], evc:get_nodes(evc:merge(A, B))),
    A2 = evc:increment(a, A),
    M = evc:merge(A2, B),
    ?assertEqual(2, evc:get_counter(a, M)),
    C = evc:new(),
    M2 = evc:merge(M, evc:increment(c, C)),
    M2a = evc:increment(a, M2),
    ?assertEqual(3, evc:get_counter(a, M2a)),
    ?assertEqual(1, evc:get_counter(c, M2a)).

older_test() ->
    A = evc:increment(a, evc:new()),
    B = evc:increment(b, evc:new()),
    M = evc:merge(A, B),
    ?assert(evc:descends(M, A)),
    C = evc:increment(c, evc:new()),
    M2 = evc:merge(C, M),
    ?assert(evc:descends(M2, M)).

compare_test() ->
    M = evc:merge(evc:increment(a, evc:new()), evc:increment(b, evc:new())),
    A = evc:increment(a, M),
    timer:sleep(10),
    B = evc:increment(b, M),
    B2 = evc:increment(b, B),
    ?assertNot(evc:descends(A, B)),
    ?assertNot(evc:descends(B, A)),
    ?assert(evc:compare(A, B)),
    timer:sleep(10),
    C = evc:increment(c, evc:new()),
    C2 = evc:increment(c, C),
    ?assertNot(evc:descends(A, C)),
    ?assertNot(evc:descends(C, A)),
    ?assertEqual([A, B, B2, C, C2], lists:sort(fun evc:compare/2, [C2, B, A, C, B2])).