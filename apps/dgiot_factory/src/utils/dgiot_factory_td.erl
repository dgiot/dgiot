%%%-------------------------------------------------------------------
%%% @author wolong
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 8月 2022 16:29
%%%-------------------------------------------------------------------
-module(dgiot_factory_td).
-author("wolong").
-include("dgiot_factory.hrl").
-define(PRE, <<"_">>).
-define(Database(Name), <<?PRE/binary, Name/binary>>).
-define(Table(Name), <<?PRE/binary, Name/binary>>).
-define(SHEETID(SHEET), <<SHEET/binary, "_id">>).
-define(TYPE, <<"TD">>).
-export([get_history_data/12, filter_data/1, filter_data/3]).
-export([select/4, get_thing_list/2, limit_skip/2]).
-export([kill_null/1]).

filter_data(undefined, _, HistoryData) ->
    Total = length(HistoryData),
    {Total, HistoryData};

filter_data(Limit, undefined, HistoryData) ->
    filter_data(Limit, 0, HistoryData);

filter_data(Limit, Skip, HistoryData) ->
    Total = length(HistoryData),
    Min = max(Skip + 1, 0),
    Max = min(Total, Limit + Skip),
    Res = case Max > Min of
              true ->
                  lists:sublist(HistoryData, Min, Max);
              _ ->
                  HistoryData
          end,
    {Total, Res}.

filter_data(Data) when is_list(Data) ->
    lists:foldl(
        fun(X, Acc) ->
            Filtered = maps:filter(fun(_, V) -> (V /= <<"null">>) and (V /= null) end, X),
            Acc ++ [Filtered]
        end, [], Data);

filter_data(Data) when is_map(Data) ->
    maps:filter(fun(_, V) -> (V /= <<"null">>) and (V /= null) end, Data).

get_history_data(ProductId, DeviceId, Type, Function, FunctionMap, Group, Having, Where, Order, Channel, Limit, Skip) ->
    DB = dgiot_tdengine:get_database(Channel, ProductId),
    TableName = case DeviceId of
                    undefined ->
                        Lower = list_to_binary(string:to_lower(dgiot_utils:to_list(ProductId))),
                        ?Table(Lower);
                    _ ->
                        Low = list_to_binary(string:to_lower(dgiot_utils:to_list(DeviceId))),
                        ?Table(Low)
                end,
    Select = select(ProductId, Type, Function, FunctionMap),
    From = <<DB/binary, TableName/binary>>,
    GROPU = group(Group),
    Have = have(Having),
    WHERE = where(Where),
    ORDER = order(Order),
    LimitAndSkip = limit_skip(Limit, Skip),
    LimitAndSkip = limit_skip(Limit, Skip),
    Sql = <<"SELECT  ", Select/binary, " FROM ", From/binary, WHERE/binary, GROPU/binary, Have/binary, ORDER/binary, LimitAndSkip/binary, ";">>,
    CountSql = <<"SELECT  count(*) ", " FROM ", From/binary, WHERE/binary, GROPU/binary, Have/binary, ORDER/binary, ";">>,
    case run_data_sql(Channel,Sql) of
        {ok,Data}->
            case run_count_sql(Channel,CountSql) of
                {ok,Total} ->
                    {ok,{Total,Data}};
                _->
                    {ok,{null,Data}}
            end;
        _->
            error
    end .

run_data_sql(Channel,Sql) ->
    case dgiot_tdengine:transaction(Channel,
        fun(Context) ->
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
        end) of
        {ok, #{<<"results">> := HistoryData}} ->
            {ok,HistoryData};
        _->
            error
    end.

run_count_sql(Channel,Sql) ->
    case dgiot_tdengine:transaction(Channel,
        fun(Context) ->
            dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql)
        end) of
        {ok, #{<<"results">> := [#{<<"count(*)">> :=Total}]}} ->
            {ok,Total};
        _->
            error
    end.

group(undefined) ->
    <<" ">>;
group(Group) ->
    <<" group by ", Group/binary, " ">>.
have(undefined) ->
    <<" ">>;
have(Having) ->
    <<" having ", Having/binary>>.

where(undefined) ->
    <<" ">>;

where(Where) when is_binary(Where) ->
    case byte_size(Where) > 0 of
        true ->
            <<" where ", Where/binary>>;
        _ ->
            <<" ">>
    end;
where(_) ->
    <<"">>.

order(undefined) ->
    <<" ">>;
order(Order) ->
    <<" ", Order/binary>>.


limit_skip(undefined, undefined) ->
    <<"  ">>;
limit_skip(Limit, Skip) ->
    BinLimit = dgiot_utils:to_binary(Limit),
    BinSkip = dgiot_utils:to_binary(Skip),
    <<" limit ", BinLimit/binary, " offset ", BinSkip/binary>>.

select(ProductId, Type, Function, undefined) ->
    select(ProductId, Type, Function, #{});

select(ProductId, Type, undefined, _) ->
    ThingList = get_thing_list(ProductId, Type),
    <<" , ", Res/binary>> = lists:foldl(
        fun(X, Acc) ->
            <<Acc/binary, " , ", X/binary>>
        end, <<"">>, ThingList),
    Res;

%%使用FunctiongMap1实现指定字段指定函数的要求格式如下
%%#{<<"sum">> => [ <<"thing1">>,<<"thing2">>],<<"avg">> => <<"thing3">>}
select(ProductId, Type, Function, FunctiongMap1) ->
    ThingList = get_thing_list(ProductId, Type),
    FunctiongMap = dgiot_utils:to_map(FunctiongMap1),
    <<" , ", Res/binary>> =
        lists:foldl(
            fun(Thing, Acc) ->
                Fun = maps:fold(
                    fun
                        (Func, FuncThingList, Acc1) when is_list(FuncThingList) ->
                            case lists:member(Thing, FuncThingList) of
                                true ->
                                    Func;
                                _ ->
                                    Acc1
                            end;
                        (Func, FuncThing, Acc1) ->
                            case Thing == FuncThing of
                                true ->
                                    Func;
                                _ ->
                                    Acc1
                            end
                    end, <<"null">>, FunctiongMap),
                case Fun of
                    <<"null">> ->
                        <<Acc/binary, " , ", Function/binary, "( ", Thing/binary, " ) as ", Thing/binary>>;
                    _ ->
                        <<Acc/binary, " , ", Fun/binary, " ( ", Thing/binary, " ) as ", Thing/binary>>
                end

            end, <<"">>, ThingList),
    Res.


get_thing_list(ProductId, undefined) ->
    case dgiot_product:get_devicetype(ProductId) of
        not_find ->
            [];
        DevTypeList ->
            Res = lists:foldl(
                fun(Type, Acc) ->
                    case dgiot_product:get_device_thing(ProductId, Type) of
                        not_find ->
                            Acc;
                        List ->
                            Acc ++ maps:keys(List)
                    end
                end, [], DevTypeList),
            Res ++ [<<"createdat">>]
    end;

get_thing_list(ProductId, Type) ->
    Quality = case dgiot_product:get_device_thing(ProductId, <<"quality">>) of
                  not_find ->
                      [];
                  Res ->
                      maps:keys(Res)
              end,
    Person = case dgiot_product:get_device_thing(ProductId, <<"person">>) of
                 not_find ->
                     [];
                 Res1 ->
                     maps:keys(Res1)
             end,
    TypeData = case dgiot_product:get_device_thing(ProductId, Type) of
                   not_find ->
                       [];
                   Res2 ->
                       maps:keys(Res2)
               end,
    TypeData ++ Quality ++ Person ++ [<<"createdat">>].


%%select last(manufac_rollnum), sum(manufac_worktime) as  manufac_worktime from _d5e32f7542._d5e32f7542 group by devaddr having manufac_worktime = 139;
kill_null(List) when is_list(List) ->
    lists:foldl(
        fun(X, Acc) when is_map(X) ->
            Res = maps:fold(
                fun(_, null, Acc1) ->
                    Acc1;
                    (K, V, Acc1) ->
                        Acc1#{K => V}
                end, #{}, X),
            Acc ++ [Res]
        end, [], List);
kill_null(List) ->
    List.
