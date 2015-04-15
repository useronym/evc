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
    new/0, new/1,
    get_counter/2, get_timestamp/1, get_nodes/1,
    event/1, event/2,
    merge/2, descends/2,
    compare/2]).


-define(TIMESTAMP, 'TIMESTAMP').

new() ->
    new(node()).


new(Key) ->
    M = maps:put(Key, 0, maps:new()),
    maps:put(?TIMESTAMP, timestamp(), M).


get_counter(Key, M) ->
    maps:get(Key, M, 0).


get_timestamp(M) ->
    maps:get(?TIMESTAMP, M).


get_nodes(M) ->
    lists:delete(?TIMESTAMP, maps:keys(M)).


event(M) ->
    event(node(), M).


event(Pid, M) ->
    Mm = maps:put(Pid, maps:get(Pid, M, 0) + 1, M),
    maps:update(?TIMESTAMP, timestamp(), Mm).


merge(M1, M2) ->
    maps:fold(fun(K, M1Val, M2In) ->
            maps:put(K, max(M1Val, maps:get(K, M2, 0)), M2In)
        end,
        M2,
        M1).


%% @doc Returns true if M1 is a descendant of M2. Ignores timestamps.
descends(M1, M2) ->
    fun Loop([{K, M2Val} | Rest]) ->
            (maps:get(K, M1, 0) >= M2Val) andalso Loop(Rest);
        Loop([]) ->
            true
    end(maps:to_list(M2)).


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