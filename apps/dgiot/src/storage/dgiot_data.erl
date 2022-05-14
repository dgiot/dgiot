%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
%%ets表的底层是由哈希表实现的,不过ordered_set例外,它是由平衡二叉树实现的。 所以不管是插入还是查找,set的效率要比ordered_set高.采用set还是ordered_set取决于你的需求，
%% 当你需要一个有序的集合时，显然应当采用ordered_set模式。
%%
%%duplicate_bag要比bag的效率要高, 因为bag要和原来的记录比较是否有相同的记录已经插入. 如果数据量很大,相同的记录越多,bag的效率就越差.
%%
%%一张ets表是由创建它的进程所拥有, 当此进程调用ets:delete或者进程终止的时候, ets表就会被删除.
%%
%%一般情况下, 插入一个元组到一张ets表中, 所有代表这个元组的结构都会被从process的堆栈中,复制到ets表中; 当查找一条记录时, 结果tuple从ets表中复制到进程的堆栈中。
%%
%%但是large binaries却不是这样! 它们被存入自已所拥有的off-heap area中。这个区域可以被多个process,ets表,和binaries所共享。它由引用计数的垃圾回收策略管理,
%%这个策略会跟踪到底有多少个process/ets表/binaries引用了这个large binaries. 如果引用数为0的话, 此大型二进制数据就会被垃圾回收掉.
%%
%%看起来很复杂, 实际结论就是: 两进程间发送包含大型binary数据的消息其实费用很低, 往ets表插入binary类型元组也很划算。我们应该尽可能采用binaries来实现字符串或无类型的大数据块

-module(dgiot_data).
-author("johnliu").
-include("dgiot_cron.hrl").

-export([init/0, init/1, init/2, destroy/1, size/1]).
-export([insert/2, save/2, delete/1, delete_all_objects/1, match/1, match_object/2, match_limit/2, match_safe_do/3, match_object/3, match_delete/1, select/2, lookup/1, page/6, update_counter/2]).
-export([insert/3, delete/2, match/2, match/3, match_delete/2, match_limit/3, match_safe_do/4, lookup/2, search/2, search/3, dets_search/2, dets_search/3, loop/2, dets_loop/3, update_counter/3]).
-export([set_consumer/2, set_consumer/3, get_consumer/2, get/1, get/2, clear/1, search_data/0, values/2, keys/1]).
-define(DB, dgiot_data).
-define(ETS, ets).
-define(DETS, dets).
-define(CONSUMER, dgiot_consumer_update_counter).

search_data() ->
    Fun =
        fun({_App, _Vsn, Mod}, Acc) ->
            case code:is_loaded(Mod) of
                false ->
                    Acc;
                _ ->
                    lists:foldl(fun(X, Acc1) ->
                        case X of
                            {dgiot_data, Type} ->
                                InitFun = list_to_atom("init_" ++ Type),
                                Mod:InitFun();
                            _ ->
                                Acc1
                        end
                                end, Acc, Mod:module_info(attributes))
            end
        end,
    dgiot_plugin:check_module(Fun, []).

init() ->
    init(?CONSUMER),
    init(?DB),
    init(?DGIOT_CRON),
    init(?CRON_DB).

init(Name) ->
    init(Name, [public, named_table, ordered_set, {write_concurrency, true}, {read_concurrency, true}]).

init(Name, Options) ->
    case ?ETS:info(Name) of
        undefined ->
            ?ETS:new(Name, Options);
        _ ->
            Name
    end.

destroy(Tab) ->
    case ?ETS:info(Tab, name) of
        undefined -> ok;
        Tab ->
            ?ETS:delete(Tab),
            ok
    end.

size(Name) ->
    case ?ETS:info(Name) of
        undefined -> -1;
        Info ->
            {size, Size} = lists:keyfind(size, 1, Info), Size
    end.


insert(Key, Value) ->
    insert(?DB, Key, Value).
insert(Name, Key, Value) ->
    save(Name, {Key, Value}).

save(Name, Value) ->
    case safe(Name) of
        false ->
            ets_not_find;
        _ ->
            ?ETS:insert(Name, Value)
    end.


delete_all_objects(Name) ->
    case safe(Name) of
        false ->
            pass;
        _ ->
            ?ETS:delete_all_objects(Name)
    end.

delete(Key) ->
    delete(?DB, Key).
delete(Name, Key) ->
    case safe(Name) of
        false ->
            pass;
        _ ->
            ?ETS:delete(Name, Key)
    end.

lookup(Key) ->
    lookup(?DB, Key).
lookup(Name, Key) ->
    case safe(Name) of
        false ->
            {error, not_find};
        _ ->
            case ?ETS:lookup(Name, Key) of
                [] -> {error, not_find};
                [{Key, Value} | _] -> {ok, Value};
                [Value | _] -> {ok, Value}
            end
    end.

values(Name, Key) ->
    case safe(Name) of
        false ->
            {error, not_find};
        _ ->
            case ?ETS:lookup(Name, Key) of
                [] ->
                    {error, not_find};
                [{Key, _Value} | _] = Values ->
                    proplists:get_all_values(Key, Values);
                Values ->
                    Values
            end
    end.

keys(Name) ->
    lists:flatten(ets:match(Name,{'$1','_'})).

get(Key) ->
    get(?DB, Key).
get(Name, Key) ->
    case safe(Name) of
        false ->
            not_find;
        _ ->
            case ?ETS:lookup(Name, Key) of
                [] -> not_find;
                [{Key, Value} | _] -> Value;
                [Value | _] -> Value
            end
    end.

match(Pattern) ->
    match(?DB, Pattern).
match(Name, Pattern) ->
    case safe(Name) of
        false ->
            {error, empty};
        _ ->
            case ?ETS:match(Name, Pattern) of
                [] -> {error, empty};
                Matchs -> {ok, Matchs}
            end
    end.

match(Name, Pattern, Limit) ->
    case safe(Name) of
        false ->
            {error, empty};
        _ ->
            ?ETS:match(Name, Pattern, Limit)
    end.

match_delete(Pattern) ->
    match_delete(?DB, Pattern).
match_delete(Name, Pattern) ->
    case safe(Name) of
        false ->
            ets_not_find;
        _ ->
            ?ETS:match_delete(Name, Pattern)
    end.

match_limit(Pattern, Limit) ->
    match_limit(?DB, Pattern, Limit).

match_limit(Name, Pattern, Limit) ->
    case safe(Name) of
        false ->
            ets_not_find;
        _ ->
            ?ETS:match(Name, Pattern, Limit)
    end.

match_object(Name, Pattern) ->
    case safe(Name) of
        false ->
            ets_not_find;
        _ ->
            ?ETS:match_object(Name, Pattern)
    end.

match_object(Name, Pattern, RowFun) ->
    Rows = match_object(Name, Pattern),
    lists:map(fun(Row) -> RowFun(Row) end, Rows).

match_safe_do(Pattern, Fun, Limit) ->
    match_safe_do(?DB, Pattern, Fun, Limit).

match_safe_do(Name, Pattern, Fun, Limit) ->
    case match_limit(Name, Pattern, Limit) of
        '$end_of_table' ->
            Fun([]);
        {Matchs, _Continuation} ->
            Fun(Matchs)
    end.


page(Name, PageNo, PageSize, Filter, RowFun, Order) ->
    Result = dgiot_pager:page(ets:table(Name), Filter, PageNo, PageSize, RowFun, Order),
    {ok, Result}.


select(Name, MatchSpec) ->
    case safe(Name) of
        false ->
            ets_not_find;
        _ ->
            ?ETS:select(Name, MatchSpec)
    end.


loop(Name, Fun) ->
    case safe(Name) of
        false ->
            ok;
        _ ->
            ?ETS:safe_fixtable(Name, true),
            loop(Name, Fun, ?ETS:first(Name)),
            ?ETS:safe_fixtable(Name, false)
    end.

loop(_, _, '$end_of_table') ->
    ok;
loop(Name, Fun, Key) ->
    case safe(Name) of
        false ->
            ok;
        _ ->
            case ?ETS:lookup(Name, Key) of
                [{Key, Value} | _] ->
                    Fun({Key, Value}),
                    loop(Name, Fun, ?ETS:next(Name, Key));
                '$end_of_table' -> ok
            end
    end.

search(Name, Fun) ->
    search(?ETS, Name, Fun).

search(Mod, Name, Fun) ->
    Mod:safe_fixtable(Name, true),
    DataSet = search(Mod, Name, Fun, Mod:first(Name), []),
    Mod:safe_fixtable(Name, false),
    DataSet.

search(_, _, _, '$end_of_table', Acc) ->
    Acc;
search(Mod, Name, Fun, Key, Acc) ->
    case Mod:lookup(Name, Key) of
        [{Key, Row} | _] ->
            case Fun({Key, Row}, Acc) of
                true ->
                    search(Mod, Name, Fun, Mod:next(Name, Key), [{Key, Row} | Acc]);
                break ->
                    ok;
                {break, NewAcc} ->
                    NewAcc;
                {true, NewAcc} ->
                    search(Mod, Name, Fun, Mod:next(Name, Key), NewAcc);
                _ -> % 返回其它就忽略
                    search(Mod, Name, Fun, Mod:next(Name, Key), Acc)
            end;
        _ ->
            search(Mod, Name, Fun, Mod:next(Name, Key), Acc)
    end.


update_counter(Key, UpdateOp) ->
    update_counter(?DB, Key, UpdateOp).

update_counter(Tab, Key, UpdateOp) ->
    ets:update_counter(Tab, Key, UpdateOp).


clear(Name) ->
    case ?ETS:info(Name) of
        undefined ->
            pass;
        _ ->
            ?ETS:delete_all_objects(Name)
    end.


%%update_counter(Tab, Key, UpdateOp) -> Result
%%update_counter(Tab, Key, UpdateOp, Default) -> Result
%%OTP 18.0
%%update_counter(Tab, Key, X3 :: [UpdateOp]) -> [Result]
%%update_counter(Tab, Key, X3 :: [UpdateOp], Default) -> [Result]
%%OTP 18.0
%%update_counter(Tab, Key, Incr) -> Result
%%update_counter(Tab, Key, Incr, Default) -> Result
%%OTP 18.0
%%Types
%%Tab = tab()
%%Key = term()
%%UpdateOp = {Pos, Incr} | {Pos, Incr, Threshold, SetValue}
%%Pos = Incr = Threshold = SetValue = Result = integer()
%%Default = tuple()

set_consumer(Key, Threshold) ->
    set_consumer(Key, 0, Threshold).

set_consumer(Key, Init, Threshold) ->
    case safe(?CONSUMER) of
        false ->
            pass;
        _ ->
            ?ETS:insert(?CONSUMER, {{Key, threshold}, Threshold}),
            ?ETS:insert(?CONSUMER, {Key, Init})
    end.


get_consumer(Key, Incr) ->
    case safe(?CONSUMER) of
        false ->
            ets_not_find;
        _ ->
            ?ETS:update_counter(?CONSUMER, Key, {2, Incr, ?ETS:lookup_element(?CONSUMER, {Key, threshold}, 2), 1})
    end.

dets_search(Name, Fun) ->
    dets_search(Name, Fun, #{<<"start">> => 0, <<"len">> => 1000000}).

dets_search(Name, Fun, Page) ->
    ?DETS:safe_fixtable(Name, true),
    DataSet = dets_search(Name, Fun, ?DETS:first(Name), [], Page#{<<"current">> => 0}),
    ?DETS:safe_fixtable(Name, false),
    DataSet.

dets_search(_, _, _, Acc, #{<<"len">> := Len}) when length(Acc) =:= Len ->
    lists:reverse(Acc);

dets_search(_, _, '$end_of_table', Acc, _) ->
    lists:reverse(Acc);

dets_search(Name, Fun, Key, Acc, Page = #{<<"start">> := Start, <<"current">> := Current}) ->
    case ?DETS:lookup(Name, Key) of
        [Row | _] ->
            case Fun(Row) of
                true ->
                    case Start =< Current of
                        true ->
                            dets_search(Name, Fun, ?DETS:next(Name, Key), [Row | Acc],
                                Page#{<<"current">> => Current + 1});
                        false ->
                            dets_search(Name, Fun, ?DETS:next(Name, Key), Acc,
                                Page#{<<"current">> => Current + 1})
                    end;
                false ->
                    dets_search(Name, Fun, ?DETS:next(Name, Key), Acc,
                        Page#{<<"current">> => Current})
            end;
        _ ->
            dets_search(Name, Fun, ?DETS:next(Name, Key), Acc,
                Page#{<<"current">> => Current + 1})
    end.

dets_loop(Name, Fun, Acc) ->
    dets:safe_fixtable(Name, true),
    dets_loop(Name, Fun, dets:first(Name), Acc),
    dets:safe_fixtable(Name, false).

dets_loop(_, _, '$end_of_table', _Acc) ->
    ok;
dets_loop(Name, Fun, Key, Acc) ->
    case dets:lookup(Name, Key) of
        [{Key, Value} | _] ->
            Fun(Key, Value, Acc),
            dets_loop(Name, Fun, dets:next(Name, Key), Acc);
        '$end_of_table' -> ok
    end.

safe(Name) ->
    case ?ETS:info(Name) of
        undefined ->
            false;
        _ ->
            true
    end.
