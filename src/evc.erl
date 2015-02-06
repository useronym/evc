%%%-------------------------------------------------------------------
%%% @author Adam 'entity', 'osense' Krupicka
%%% @copyright (C) 2015
%%% @doc Erlang vector clock. Hacks total ordering on top of regular vector clocks by adding timestamps.
%%% I have been told this is "reasonableish".
%%%
%%% @end
%%% Created : 29. Jan 2015 11:41
%%%-------------------------------------------------------------------
-module(evc).

%% API
-export([new/0, new/1, on_key/2, get_timestamp/1, event/1, event/2, merge/2, descends/2, compare/2]).


-define(TIMESTAMP, 'TIMESTAMP').

new() ->
    new(node()).

new(Key) ->
    M = maps:put(Key, 0, maps:new()),
    maps:put(?TIMESTAMP, timestamp(), M).

on_key(Key, M) ->
    maps:get(Key, M).

get_timestamp(M) ->
    maps:get(?TIMESTAMP, M).

event(M) ->
    event(node(), M).

event(Pid, M) ->
    Mm = maps:update(Pid, maps:get(Pid, M) + 1, M),
    maps:update(?TIMESTAMP, timestamp(), Mm).

merge(M1, M2) ->
    merge(size_order(M1, M2)).
merge({SmallM, LargeM}) ->
    NewSmallM = maps:map(
        fun(K, V) ->
            max(V, maps:get(K, LargeM, 0)) end,
        SmallM),
    maps:merge(NewSmallM, maps:without(maps:keys(NewSmallM), LargeM)).

% @doc Returns true if M1 is a descendant of M2. Ignores timestamps.
descends(M1, M2) ->
    descends_for_ks(M1, M2, maps:keys(maps:merge(M1, M2))).

descends_for_ks(_, _, []) ->
    true;
descends_for_ks(M1, M2, [?TIMESTAMP | Ks]) ->
    descends_for_ks(M1, M2, Ks);
descends_for_ks(M1, M2, [K | Ks]) ->
    case {maps:get(K, M1, 0), maps:get(K, M2, 0)} of
        {V1, V2} when V1 >= V2 ->
            descends_for_ks(M1, M2, Ks);
        _ ->
            false
    end.

% doc Returns true if M1 is less than or equal to M2. If can't decide, compares the timestamps.
compare(M1, M2) ->
    case descends(M2, M1) of
        true ->
            true;
        _ ->
            case descends(M1, M2) of
                true ->
                    false;
                _ ->
                    get_timestamp(M1) =< get_timestamp(M2)
            end
    end.

size_order(M1, M2) ->
    case maps:size(M1) =< maps:size(M2) of
        true ->
            {M1, M2};
        false ->
            {M2, M1}
    end.

timestamp() ->
    {_Ms, S, Us} = os:timestamp(),
    S * 1000000 + Us.