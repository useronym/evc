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
-export([
    new/0,
    get_counter/2, get_timestamp/1, get_nodes/1,
    increment/1, increment/2,
    merge/2, descends/2,
    compare/2]).


-define(TIMESTAMP, 'TIMESTAMP').

new() ->
    {#{}, timestamp()}.


get_counter(Key, {M, _Ts}) ->
    maps:get(Key, M, 0).


get_timestamp({_M, Ts}) ->
    Ts.


get_nodes({M, _Ts}) ->
    maps:keys(M).


increment(M) ->
    increment(node(), M).


increment(Pid, {M, _Ts}) ->
    Mm = maps:put(Pid, maps:get(Pid, M, 0) + 1, M),
    {Mm, timestamp()}.

merge({M1, Ts1}, {M2, Ts2}) ->
    M = maps:fold(fun(K, M1Val, M2In) ->
            maps:put(K, max(M1Val, maps:get(K, M2, 0)), M2In)
        end,
        M2,
        M1),
    {M, max(Ts1, Ts2)}.


%% @doc Returns true if M1 is a descendant of M2. Ignores timestamps.
descends({M1, _Ts1}, {M2, _Ts2}) ->
    maps:fold(fun(K, V2, Descends) ->
            Descends andalso (V2 =< maps:get(K, M1, 0))
        end,
        true,
        M2).

%% @doc Returns true if M1 is less than or equal to M2. If can't decide, compares the timestamps.
compare(M1, M2) ->
    case descends(M2, M1) of
        true -> true;
        _ ->
            case descends(M1, M2) of
                true -> false;
                _ -> get_timestamp(M1) =< get_timestamp(M2)
            end
    end.


timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    MegaSecs*1000000000000 + Secs*1000000 + MicroSecs.