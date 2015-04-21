%% -------------------------------------------------------------------
%%
%% riak_core: Core Riak Application
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc A simple Erlang implementation of vector clocks as inspired by Lamport logical clocks.
%%
%% @reference Leslie Lamport (1978). "Time, clocks, and the ordering of events
%% in a distributed system". Communications of the ACM 21 (7): 558-565.
%%
%% @reference Friedemann Mattern (1988). "Virtual Time and Global States of
%% Distributed Systems". Workshop on Parallel and Distributed Algorithms:
%% pp. 215-226

-module(evc_riak).

-export([
    new/0,
    new/1,
    descends/2,
    dominates/2,
    merge/2,
    merge_list/1,
    get_counter/2,
    get_dot/2,
    valid_dot/1,
    event/2,
    event/3,
    get_nodes/1,
    equal/2,
    prune/3,
    timestamp/0,
    event/1,
    compare/2,
    get_mean_timestamp/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([evc/0, timestamp/0, evc_node/0, pure_dot/0]).

-type evc() :: {[pure_dot()], timestamp()}.
-type pure_dot() :: {evc_node(), counter()}.

% Nodes can have any term() as a name, but they must differ from each other.
-type evc_node() :: term().
-type counter() :: integer().
-type timestamp() :: integer().

% @doc Create a brand new evc.
-spec new() -> evc().
new() ->
    {[], timestamp()}.

new(Node) ->
    event(Node, new()).

% @doc Return true if Va is a direct descendant of Vb, else false -- remember, a evc is its own descendant!
-spec descends(Va :: evc(), Vb :: evc()) -> boolean().
descends(_, {[], _}) ->
    % all evcs descend from the empty evc
    true;
descends({Va, Ta}, {Vb, Tb}) ->
    [{NodeB, CtrB}|RestB] = Vb,
    case lists:keyfind(NodeB, 1, Va) of
        false ->
            false;
        {_, CtrA} ->
            (CtrA >= CtrB) andalso descends({Va, Ta}, {RestB, Tb})
    end.

%% @doc true if `A' strictly dominates `B'. Note: ignores
%% timestamps. In Riak it is possible to have evcs that are
%% identical except for timestamps. When two evcs descend each
%% other, but are not equal, they are concurrent. See source comment
%% for more details. (Actually you can have indentical clocks
%% including timestamps, that represent different events, but let's
%% not go there.)
%%
-spec dominates(evc(), evc()) -> boolean().
dominates(A, B) ->
    %% In a sane world if two evcs descend each other they MUST be
    %% equal. In riak they can descend each other and have different
    %% timestamps(!) How? Deleted keys, re-written, then restored is
    %% one example. See riak_kv#679 for others. This is why we must
    %% check descends both ways rather than checking descends(A, B)
    %% and not equal(A, B). Do not "optimise" this to dodge the second
    %% descends call! I know that the laws of causality say that each
    %% actor must act serially, but Riak breaks that.
    descends(A, B) andalso not descends(B, A).

% @doc Merge 2 VClocks, recalculating the mean timestamp.
-spec merge(Va :: evc(), Vb :: evc()) -> evc().
merge({[], Ta}, {[], Tb}) ->
    {[], (Ta + Tb) div 2};
merge({Va, Ta}, {Vb, Tb}) ->
    {merge_list([Va, Vb]), (Ta + Tb) div 2}.

% @doc Combine all VClocks in the input list into their least possible
%      common descendant.
-spec merge_list(VClocks :: [[pure_dot()]]) -> [pure_dot()].
merge_list([])             -> [];
merge_list([SingleVclock]) -> SingleVclock;
merge_list([First|Rest])   -> merge_list(Rest, lists:keysort(1, First)).

merge_list([], NClock) -> NClock;
merge_list([AClock|VClocks],NClock) ->
    merge_list(VClocks, merge_list(lists:keysort(1, AClock), NClock, [])).

merge_list([], [], AccClock) -> lists:reverse(AccClock);
merge_list([], Left, AccClock) -> lists:reverse(AccClock, Left);
merge_list(Left, [], AccClock) -> lists:reverse(AccClock, Left);
merge_list(V=[{Node1, Ctr1} = NCT1 | VClock],
    N=[{Node2, Ctr2} = NCT2 | NClock], AccClock) ->
    if Node1 < Node2 ->
        merge_list(VClock, N, [NCT1|AccClock]);
       Node1 > Node2 ->
            merge_list(V, NClock, [NCT2|AccClock]);
       true ->
           CT = if Ctr1 >= Ctr2 -> Ctr1;
                   Ctr1 < Ctr2 -> Ctr2
                end,
           merge_list(VClock, NClock, [{Node1,CT}|AccClock])
    end.

% @doc Get the counter value in VClock set from Node.
-spec get_counter(Node :: evc_node(), VClock :: evc()) -> counter().
get_counter(Node, {VClock, _}) ->
    case lists:keyfind(Node, 1, VClock) of
        {_, Ctr} -> Ctr;
        false           -> 0
    end.

% @doc Get the entry `dot()' for `evc_node()' from `evc()'.
-spec get_dot(Node :: evc_node(), VClock :: evc()) -> {ok, pure_dot()} | undefined.
get_dot(Node, {VClock, _}) ->
    case lists:keyfind(Node, 1, VClock) of
        false -> undefined;
        Entry -> {ok, Entry}
    end.

%% @doc is the given argument a valid dot, or entry?
-spec valid_dot(pure_dot()) -> boolean().
valid_dot({_, Cnt}) when is_integer(Cnt) ->
    true;
valid_dot(_) ->
    false.

% @doc Increment VClock at Node.
-spec event(Node :: evc_node(), VClock :: evc()) -> evc().
event(Node, VClock) ->
    event(Node, timestamp(), VClock).

% @doc Increment VClock at Node.
-spec event(Node :: evc_node(), IncTs :: timestamp(),
    VClock :: evc()) -> evc().
event(Node, IncTs, {VClock, Ts}) ->
    {C1, NewV} = case lists:keytake(Node, 1, VClock) of
                     false ->
                         {1, VClock};
                     {value, {_N, C}, ModV} ->
                         {C + 1, ModV}
                 end,
    W = length(NewV) + 1,
    NewTs = Ts + ((IncTs - Ts) div W),
    {[{Node,C1}|NewV], NewTs}.


% @doc Return the list of all nodes that have ever evented VClock.
-spec get_nodes(VClock :: evc()) -> [evc_node()].
get_nodes({VClock, _}) ->
    [X || {X, _} <- VClock].

% @doc Return a timestamp for a vector clock
-spec timestamp() -> timestamp().
timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    MegaSecs*1000000000000 + Secs*1000000 + MicroSecs.

% @doc Compares two VClocks for equality.
-spec equal(VClockA :: evc(), VClockB :: evc()) -> boolean().
equal({VA, _}, {VB, _}) ->
    lists:sort(VA) =:= lists:sort(VB).

% @doc Possibly shrink the size of a evc, depending on current age and size.
-spec prune(V::evc(), Now::integer(), BucketProps::term()) -> evc().
prune(V,Now,BucketProps) ->
    %% This sort need to be deterministic, to avoid spurious merge conflicts later.
    %% We achieve this by using the node ID as secondary key.
    SortV = lists:sort(fun({N1,{_,T1}},{N2,{_,T2}}) -> {T1,N1} < {T2,N2} end, V),
    prune_evc1(SortV,Now,BucketProps).
% @private
prune_evc1(V,Now,BProps) ->
    case length(V) =< get_property(small_evc, BProps) of
        true -> V;
        false ->
            {_,{_,HeadTime}} = hd(V),
            case (Now - HeadTime) < get_property(young_evc,BProps) of
                true -> V;
                false -> prune_evc1(V,Now,BProps,HeadTime)
            end
    end.
% @private
prune_evc1(V,Now,BProps,HeadTime) ->
    % has a precondition that V is longer than small and older than young
    case (length(V) > get_property(big_evc,BProps)) orelse
        ((Now - HeadTime) > get_property(old_evc,BProps)) of
        true -> prune_evc1(tl(V),Now,BProps);
        false -> V
    end.

get_property(Key, PairList) ->
    case lists:keyfind(Key, 1, PairList) of
        {_Key, Value} ->
            Value;
        false ->
            undefined
    end.

% @doc Increment VClock at current node.
-spec event(VClock :: evc()) -> evc().
event(VClock) ->
    event(node(), timestamp(), VClock).

% @doc Get the name of the most new value in a VClock.
-spec get_oldest_node(VClock :: evc()) -> {evc_node()}.
get_oldest_node(VClock) ->
    Pairs = ([{Node, get_counter(Node, VClock)} || Node <- get_nodes(VClock)]),
    {Oldest, _} = lists:foldl(
        fun({Node, Count}, {_, MaxCount}) when Count >= MaxCount ->
                {Node, Count};
            ({_, Count}, {MaxNode, MaxCount}) when Count < MaxCount ->
                {MaxNode, MaxCount}
        end,
        {'', 0},
        Pairs),
    Oldest.


%% @doc Get the mean timestamp of a vector clock.
-spec get_mean_timestamp(VClock :: evc()) -> timestamp().
get_mean_timestamp({_VClock, Ts}) ->
    Ts.

% @doc Returns true if Va is less than or equal to Vb, else false
compare(Va, Vb) ->
    case descends(Vb, Va) of
        true -> true;
        false ->
            case descends(Va, Vb) of
                true -> false;
                false ->
                    Ta = get_mean_timestamp(Va),
                    Tb = get_mean_timestamp(Vb),
                    if Ta < Tb -> true;
                       Ta > Tb -> false;
                       Ta == Tb ->
                           get_oldest_node(Va) < get_oldest_node(Vb)
                    end
            end
    end.



%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

mean_timestamp_test() ->
    A = evc_riak:new(),
    A1 = evc_riak:event(a, A),
    timer:sleep(50),
    A2 = evc_riak:event(b, A1),
    T = timestamp(),
    timer:sleep(50),
    A3 = evc_riak:event(c, A2),
    ?assertMatch(X when abs(X) < 1000, get_mean_timestamp(A3) - T).

% To avoid an unnecessary dependency, we paste a function definition from riak_core_until.
riak_core_until_moment() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

% doc Serves as both a trivial test and some example code.
example_test() ->
    A = evc_riak:new(),
    B = evc_riak:new(),
    A1 = evc_riak:event(a, A),
    B1 = evc_riak:event(b, B),
    true = evc_riak:descends(A1,A),
    true = evc_riak:descends(B1,B),
    false = evc_riak:descends(A1,B1),
    A2 = evc_riak:event(a, A1),
    C = evc_riak:merge(A2, B1),
    C1 = evc_riak:event(c, C),
    true = evc_riak:descends(C1, A2),
    true = evc_riak:descends(C1, B1),
    false = evc_riak:descends(B1, C1),
    false = evc_riak:descends(B1, A1),
    ok.

prune_small_test() ->
    % evc with less entries than small_evc will be untouched
    Now = riak_core_until_moment(),
    OldTime = Now - 32000000,
    SmallVC = [{<<"1">>, {1, OldTime}},
        {<<"2">>, {2, OldTime}},
        {<<"3">>, {3, OldTime}}],
    Props = [{small_evc,4}],
    ?assertEqual(lists:sort(SmallVC), lists:sort(prune(SmallVC, Now, Props))).

prune_young_test() ->
    % evc with all entries younger than young_evc will be untouched
    Now = riak_core_until_moment(),
    NewTime = Now - 1,
    VC = [{<<"1">>, {1, NewTime}},
        {<<"2">>, {2, NewTime}},
        {<<"3">>, {3, NewTime}}],
    Props = [{small_evc,1},{young_evc,1000}],
    ?assertEqual(lists:sort(VC), lists:sort(prune(VC, Now, Props))).

prune_big_test() ->
    % evc not preserved by small or young will be pruned down to
    % no larger than big_evc entries
    Now = riak_core_until_moment(),
    NewTime = Now - 1000,
    VC = [{<<"1">>, {1, NewTime}},
        {<<"2">>, {2, NewTime}},
        {<<"3">>, {3, NewTime}}],
    Props = [{small_evc,1},{young_evc,1},
        {big_evc,2},{old_evc,100000}],
    ?assert(length(prune(VC, Now, Props)) =:= 2).

prune_old_test() ->
    % evc not preserved by small or young will be pruned down to
    % no larger than big_evc and no entries more than old_evc ago
    Now = riak_core_until_moment(),
    NewTime = Now - 1000,
    OldTime = Now - 100000,
    VC = [{<<"1">>, {1, NewTime}},
        {<<"2">>, {2, OldTime}},
        {<<"3">>, {3, OldTime}}],
    Props = [{small_evc,1},{young_evc,1},
        {big_evc,2},{old_evc,10000}],
    ?assert(length(prune(VC, Now, Props)) =:= 1).

prune_order_test() ->
    % evc with two nodes of the same timestamp will be pruned down
    % to the same node
    Now = riak_core_until_moment(),
    OldTime = Now - 100000,
    VC1 = [{<<"1">>, {1, OldTime}},
        {<<"2">>, {2, OldTime}}],
    VC2 = lists:reverse(VC1),
    Props = [{small_evc,1},{young_evc,1},
        {big_evc,2},{old_evc,10000}],
    ?assertEqual(prune(VC1, Now, Props), prune(VC2, Now, Props)).

accessor_test() ->
    VC = {[{<<"1">>, 1},
        {<<"2">>, 2}], now()},
    ?assertEqual(1, get_counter(<<"1">>, VC)),
    ?assertEqual(2, get_counter(<<"2">>, VC)),
    ?assertEqual(0, get_counter(<<"3">>, VC)),
    ?assertEqual([<<"1">>, <<"2">>], get_nodes(VC)).

merge2_test() ->
    Time = timestamp(),
    ?assertMatch({[], T} when abs(T - Time) < 100, merge(evc_riak:new(), evc_riak:new())),

    VC1 = event(1, event(1, evc_riak:new())),
    timer:sleep(100),
    VC2 = event(2, evc_riak:new()),
    T1 = get_mean_timestamp(VC1),
    T2 = get_mean_timestamp(VC2),
    ?assertEqual({[{1, 2}, {2, 1}], (T1 + T2) div 2}, merge(VC1, VC2)).

merge_less_left_test() ->
    VC1 = [{<<"5">>, {5, 5}}],
    VC2 = [{<<"6">>, {6, 6}}, {<<"7">>, {7, 7}}],
    ?assertEqual([{<<"5">>, {5, 5}},{<<"6">>, {6, 6}}, {<<"7">>, {7, 7}}],
        evc_riak:merge_list([VC1, VC2])).

merge_less_right_test() ->
    VC1 = [{<<"6">>, {6, 6}}, {<<"7">>, {7, 7}}],
    VC2 = [{<<"5">>, {5, 5}}],
    ?assertEqual([{<<"5">>, {5, 5}},{<<"6">>, {6, 6}}, {<<"7">>, {7, 7}}],
        evc_riak:merge_list([VC1, VC2])).

merge_same_id_test() ->
    VC1 = [{<<"1">>, {1, 2}},{<<"2">>,{1,4}}],
    VC2 = [{<<"1">>, {1, 3}},{<<"3">>,{1,5}}],
    ?assertEqual([{<<"1">>, {1, 3}},{<<"2">>,{1,4}},{<<"3">>,{1,5}}],
        evc_riak:merge_list([VC1, VC2])).

get_entry_test() ->
    VC = evc_riak:new(),
    VC1 = event(a, event(c, event(b, event(a, VC)))),
    ?assertMatch({ok, {a, 2}}, get_dot(a, VC1)),
    ?assertMatch({ok, {b, 1}}, get_dot(b, VC1)),
    ?assertMatch({ok, {c, 1}}, get_dot(c, VC1)),
    ?assertEqual(undefined, get_dot(d, VC1)).

valid_entry_test() ->
    VC = evc_riak:new(),
    VC1 = event(c, event(b, event(a, VC))),
    [begin
         {ok, E} = get_dot(Actor, VC1),
         ?assert(valid_dot(E))
     end || Actor <- [a, b, c]],
    ?assertNot(valid_dot(undefined)),
    ?assertNot(valid_dot("huffle-puff")),
    ?assertNot(valid_dot([])).

-endif.
