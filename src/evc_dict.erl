%%%-------------------------------------------------------------------
%%% @author Adam 'entity', 'osense' Krupicka
%%% @copyright (C) 2015
%%% @doc Erlang vector clock. Hacks total ordering on top of regular vector clocks by adding timestamps.
%%% I have been told this is "reasonableish".
%%%
%%% @end
%%% Created : 29. Jan 2015 11:41
%%%-------------------------------------------------------------------
-module(evc_dict).

%% API
-export([
    new/0,
    get_counter/2, get_timestamp/1, get_nodes/1,
    event/1, event/2,
    merge/2, descends/2,
    compare/2]).


-define(TIMESTAMP, 'TIMESTAMP').

new() ->
    dict:store(?TIMESTAMP, timestamp(), dict:new()).


get_counter(Key, M) ->
    case dict:find(Key, M) of
        {ok, Val} -> Val;
        error -> 0
    end.


get_timestamp(M) ->
    dict:fetch(?TIMESTAMP, M).


get_nodes(M) ->
    lists:delete(?TIMESTAMP, dict:fetch_keys(M)).


event(M) ->
    event(node(), M).


event(Pid, M) ->
    Mm = dict:update_counter(Pid, 1, M),
    dict:store(?TIMESTAMP, timestamp(), Mm).


merge(M1, M2) ->
    dict:fold(fun(K, M1Val, M2In) ->
            dict:store(K, max(M1Val, get_counter(K, M2)), M2In)
        end,
        M2,
        M1).


%% @doc Returns true if M1 is a descendant of M2. Ignores timestamps.
descends(M1, M2) ->
    dict:fold(fun(K, V2, Descends) ->
            Descends andalso (V2 =< get_counter(K, M1))
        end,
        true,
        dict:erase(?TIMESTAMP, M2)).

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