%%%-------------------------------------------------------------------
%%% @author Adam 'entity', 'osense' Krupicka
%%% @copyright (C) 2015
%%% @doc Erlang vector clock. Maps Pids to the clocks.
%%%
%%% @end
%%% Created : 29. Jan 2015 11:41
%%%-------------------------------------------------------------------
-module(evc).

%% API
-export([new/0, new/1, on_index/2, event/1, event/2, merge/2, older/2]).

new() ->
    new(node(self())).

new(Pid) ->
    M = maps:new(),
    maps:put(Pid, 0, M).

on_index(Index, M) ->
    maps:get(Index, M).

event(M) ->
    event(node(self()), M).

event(Pid, M) ->
    maps:update(Pid, maps:get(Pid, M) + 1, M).

merge(M1, M2) ->
    merge(size_order(M1, M2)).
merge({SmallM, LargeM}) ->
    NewSmallM = maps:map(
        fun(K, V) ->
            max(V, maps:get(K, LargeM, 0)) end,
        SmallM),
    maps:merge(NewSmallM, maps:without(maps:keys(NewSmallM), LargeM)).

older(M1, M2) ->
    %Intersection = maps:with(maps:keys(LargeM), SmallM),
    older_for_ks(false, M1, M2, maps:keys(maps:merge(M1, M2))).

older_for_ks(true, _, _, []) ->
    true;
older_for_ks(_, _, _, []) ->
    false;
older_for_ks(X, M1, M2, [K | Ks]) ->
    case {maps:get(K, M1, 0), maps:get(K, M2, 0)} of
        {V1, V2} when V1 < V2 ->
            older_for_ks(true, M1, M2, Ks);
        {V1, V2} when V1 == V2 ->
            older_for_ks(X, M1, M2, Ks);
        _ ->
            false
    end.

size_order(M1, M2) ->
    case maps:size(M1) =< maps:size(M2) of
        true ->
            {M1, M2};
        false ->
            {M2, M1}
    end.