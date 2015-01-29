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

merge_test() ->
    Pa = list_to_pid("<0.100.0>"),
    Pb = list_to_pid("<0.100.1>"),
    A = evc:new(Pa),
    B = evc:new(Pb),
    ?assert(maps:size(evc:merge(A, B)) == 2),
    A2 = evc:event(Pa, A),
    M = evc:merge(A2, B),
    ?assertEqual(1, evc:on_index(Pa, M)),
    Pc = list_to_pid("<0.100.2>"),
    C = evc:new(Pc),
    M2 = evc:merge(M, evc:event(Pc, C)),
    M2a = evc:event(Pa, M2),
    ?assertEqual(2, evc:on_index(Pa, M2a)),
    ?assertEqual(1, evc:on_index(Pc, M2a)).

older_test() ->
    Pa = list_to_pid("<0.100.0>"),
    Pb = list_to_pid("<0.100.1>"),
    A = evc:event(Pa, evc:new(Pa)),
    B = evc:event(Pb, evc:new(Pb)),
    M = evc:merge(A, B),
    ?assert(evc:older(A, M)),
    Pc = list_to_pid("<0.100.2>"),
    C = evc:event(Pc, evc:new(Pc)),
    M2 = evc:merge(C, M),
    ?assert(evc:older(M, M2)).
