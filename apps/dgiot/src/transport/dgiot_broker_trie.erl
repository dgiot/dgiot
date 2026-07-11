%%--------------------------------------------------------------------
%% Copyright (c) 2020-2024 DGIOT Technologies Co., Ltd.
%%--------------------------------------------------------------------

-module(dgiot_broker_trie).
-author("dgiot").

%% Topic trie for fast subscription matching.
%% Supports MQTT wildcards: '+' (single level) and '#' (multi level).
%% O(topic_depth) match instead of O(subscriptions) scan.

-export([new/0, insert/2, delete/2, match/1, match/2, subscribers/1, stats/0]).

-define(TRIE_TAB, dgiot_topic_trie).

-type topic() :: binary().
-type subscriber() :: pid() | binary().
-type match_fun() :: fun((subscriber()) -> term()).

-record(node, {
    id :: non_neg_integer(),
    word :: binary(),
    parent :: non_neg_integer(),
    children = #{} :: map(),
    subs = [] :: [subscriber()]
}).

-define(ROOT_ID, 0).
-define(ROOT_WORD, <<>>).

-spec new() -> ok.
new() ->
    case ets:info(?TRIE_TAB) of
        undefined ->
            ets:new(?TRIE_TAB, [named_table, public, set,
                                {keypos, 2}, {write_concurrency, true}]);
        _ -> ok
    end,
    ets:insert(?TRIE_TAB, #node{id = ?ROOT_ID, word = ?ROOT_WORD, parent = 0}),
    ok.

-spec insert(topic(), subscriber()) -> ok.
insert(Topic, Pid) ->
    Words = split_topic(Topic),
    LastId = insert_path(?ROOT_ID, Words),
    add_subscriber(LastId, Pid),
    ok.

-spec delete(topic(), subscriber()) -> ok.
delete(Topic, Pid) ->
    Words = split_topic(Topic),
    case find_node(?ROOT_ID, Words) of
        {ok, NodeId} ->
            [#node{subs = Subs}] = ets:lookup(?TRIE_TAB, NodeId),
            ets:update_element(?TRIE_TAB, NodeId, {#node.subs, lists:delete(Pid, Subs)});
        _ -> ok
    end.

-spec match(topic()) -> [subscriber()].
match(Topic) ->
    match(Topic, fun(Pid) -> Pid end).

-spec match(topic(), match_fun()) -> [term()].
match(Topic, Fun) ->
    Words = split_topic(Topic),
    Matched = match_path(?ROOT_ID, Words, []),
    [Fun(P) || P <- lists:usort(Matched)].

-spec subscribers(topic()) -> [subscriber()].
subscribers(Topic) ->
    match(Topic, fun(Pid) -> Pid end).

-spec stats() -> #{nodes => non_neg_integer(), total_subscribers => non_neg_integer()}.
stats() ->
    Nodes = ets:info(?TRIE_TAB, size),
    TotalSubs = ets:foldl(fun(#node{subs = S}, Acc) -> length(S) + Acc end, 0, ?TRIE_TAB),
    #{nodes => Nodes, total_subscribers => TotalSubs}.

%% Internal
split_topic(Topic) when is_binary(Topic) ->
    binary:split(Topic, <<"/">>, [global]).

insert_path(NodeId, []) ->
    NodeId;
insert_path(NodeId, [Word | Rest]) ->
    [#node{children = Children}] = ets:lookup(?TRIE_TAB, NodeId),
    NextId = case maps:find(Word, Children) of
        {ok, Id} -> Id;
        error ->
            Id = ets:info(?TRIE_TAB, size) + 1,
            ets:insert(?TRIE_TAB, #node{id = Id, word = Word, parent = NodeId}),
            ets:update_element(?TRIE_TAB, NodeId, {#node.children, Children#{Word => Id}}),
            Id
    end,
    insert_path(NextId, Rest).

find_node(_NodeId, []) ->
    {ok, _NodeId};
find_node(NodeId, [Word | Rest]) ->
    [#node{children = Children}] = ets:lookup(?TRIE_TAB, NodeId),
    case maps:find(Word, Children) of
        {ok, NextId} -> find_node(NextId, Rest);
        error -> error
    end.

add_subscriber(NodeId, Pid) ->
    [#node{subs = Subs}] = ets:lookup(?TRIE_TAB, NodeId),
    ets:update_element(?TRIE_TAB, NodeId, {#node.subs, [Pid | Subs]}).

match_path(_NodeId, [], Acc) ->
    Acc;
match_path(NodeId, [W | Rest], Acc) ->
    [#node{children = Children, subs = Subs}] = ets:lookup(?TRIE_TAB, NodeId),
    Acc1 = Subs ++ Acc,
    Exact = match_child(maps:get(W, Children, undefined), Rest, Acc1),
    Plus = match_child(maps:get(<<"+">>, Children, undefined), Rest, Acc1),
    Hash = match_hash(Children, Acc1),
    lists:usort(Exact ++ Plus ++ Hash).

match_child(undefined, _Rest, Acc) -> Acc;
match_child(ChildId, [], Acc) ->
    collect_node_subs(ChildId, Acc);
match_child(ChildId, Rest, Acc) ->
    match_path(ChildId, Rest, Acc).

match_hash(Children, Acc) ->
    case maps:find(<<"#">>, Children) of
        {ok, HashId} ->
            collect_node_subs(HashId, Acc);
        error -> Acc
    end.

collect_node_subs(NodeId, Acc) ->
    [#node{subs = Subs, children = Children}] = ets:lookup(?TRIE_TAB, NodeId),
    Acc1 = Subs ++ Acc,
    maps:fold(fun(_Word, ChildId, A) ->
        collect_node_subs(ChildId, A)
    end, Acc1, Children).