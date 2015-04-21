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


all(Mod) ->
    base(Mod),
    merge(Mod),
    older(Mod),
    compare(Mod).

base(Mod) ->
    ?assertEqual(0, Mod:get_counter(aaa, Mod:new())),
    VC = Mod:event(aaa, Mod:new()),
    ?assertEqual(1, Mod:get_counter(aaa, VC)).

merge(Mod) ->
    A = Mod:event(a, Mod:new()),
    B = Mod:event(b, Mod:new()),
    ?assertEqual([a, b], Mod:get_nodes(Mod:merge(A, B))),
    A2 = Mod:event(a, A),
    M = Mod:merge(A2, B),
    ?assertEqual(2, Mod:get_counter(a, M)),
    C = Mod:new(),
    M2 = Mod:merge(M, Mod:event(c, C)),
    M2a = Mod:event(a, M2),
    ?assertEqual(3, Mod:get_counter(a, M2a)),
    ?assertEqual(1, Mod:get_counter(c, M2a)).

older(Mod) ->
    A = Mod:event(a, Mod:new()),
    B = Mod:event(b, Mod:new()),
    M = Mod:merge(A, B),
    ?assert(Mod:descends(M, A)),
    C = Mod:event(c, Mod:new()),
    M2 = Mod:merge(C, M),
    ?assert(Mod:descends(M2, M)).

compare(Mod) ->
    M = Mod:merge(Mod:event(a, Mod:new()), Mod:event(b, Mod:new())),
    A = Mod:event(a, M),
    timer:sleep(10),
    B = Mod:event(b, M),
    B2 = Mod:event(b, B),
    ?assertNot(Mod:descends(A, B)),
    ?assertNot(Mod:descends(B, A)),
    ?assert(Mod:compare(A, B)),
    timer:sleep(10),
    C = Mod:event(c, Mod:new()),
    C2 = Mod:event(c, C),
    ?assertNot(Mod:descends(A, C)),
    ?assertNot(Mod:descends(C, A)),
    ?assertEqual([A, B, B2, C, C2], lists:sort(fun Mod:compare/2, [C2, B, A, C, B2])).


all_test() ->
    all(evc_maps),
    all(evc_dict),
    all(evc_riak).
