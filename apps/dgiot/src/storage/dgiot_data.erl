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

-module(dgiot_data).
-author("johnliu").
-include("dgiot_cron.hrl").

-export([init/0, init/1, init/2]).
-export([insert/2, save/2, delete/1, match/1, match_object/2, match_limit/2, match_safe_do/3, match_object/3, match_delete/1, select/2, lookup/1, page/6, destroy/1, update_counter/2]).
-export([insert/3, delete/2, match/2, match/3, match_delete/2, match_limit/3, match_safe_do/4, lookup/2, search/2, search/3, dets_search/2, dets_search/3, loop/2, dets_loop/3, update_counter/3]).
-export([set_consumer/2, set_consumer/3, get_consumer/2, get/1, get/2, clear/1,search_data/0]).
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

insert(Key, Value) ->
    insert(?DB, Key, Value).
insert(Name, Key, Value) ->
    save(Name, {Key, Value}).

save(Name, Value) ->
    ?ETS:insert(Name, Value).


delete(Key) ->
    delete(?DB, Key).
delete(Name, Key) ->
    ?ETS:delete(Name, Key).


lookup(Key) ->
    lookup(?DB, Key).
lookup(Name, Key) ->
    case ?ETS:lookup(Name, Key) of
        [] -> {error, not_find};
        [{Key, Value} | _] -> {ok, Value};
        [Value | _] -> {ok, Value}
    end.

get(Key) ->
    get(?DB, Key).
get(Name, Key) ->
    case ?ETS:lookup(Name, Key) of
        [] -> not_find;
        [{Key, Value} | _] -> Value;
        [Value | _] -> Value
    end.

match(Pattern) ->
    match(?DB, Pattern).
match(Name, Pattern) ->
    case ?ETS:match(Name, Pattern) of
        [] -> {error, empty};
        Matchs -> {ok, Matchs}
    end.

match(Name, Pattern, Limit) ->
    ?ETS:match(Name, Pattern, Limit).

match_delete(Pattern) ->
    match_delete(?DB, Pattern).
match_delete(Name, Pattern) ->
    ?ETS:match_delete(Name, Pattern).

match_limit(Pattern, Limit) ->
    match_limit(?DB, Pattern, Limit).

match_limit(Name, Pattern, Limit) ->
    ?ETS:match(Name, Pattern, Limit).

match_object(Name, Pattern) ->
    ?ETS:match_object(Name, Pattern).

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


destroy(Name) ->
    ?ETS:delete(Name).

select(Name, MatchSpec) ->
    ?ETS:select(Name, MatchSpec).


loop(Name, Fun) ->
    ?ETS:safe_fixtable(Name, true),
    loop(Name, Fun, ?ETS:first(Name)),
    ?ETS:safe_fixtable(Name, false).

loop(_, _, '$end_of_table') ->
    ok;
loop(Name, Fun, Key) ->
    case ?ETS:lookup(Name, Key) of
        [{Key, Value} | _] ->
            Fun({Key, Value}),
            loop(Name, Fun, ?ETS:next(Name, Key));
        '$end_of_table' -> ok
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
    ?ETS:insert(?CONSUMER, {{Key, threshold}, Threshold}),
    ?ETS:insert(?CONSUMER, {Key, Init}).


get_consumer(Key, Incr) ->
    ?ETS:update_counter(?CONSUMER, Key, {2, Incr, ?ETS:lookup_element(?CONSUMER, {Key, threshold}, 2), 1}).

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
