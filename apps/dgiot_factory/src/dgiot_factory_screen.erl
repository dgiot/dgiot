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

-module(dgiot_factory_screen).
-author("kenneth").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_factory.hrl").
%%大屏echart模板文件名
-define(FACTORY_SCREEN, factory_screen).
-export([get_screen/2]).

-export([get_data/3]).

-export([init_screen_data/0, updata_screen_data/3, format_bar/1]).

get_screen(#{<<"ordertype">> := ProductId} = Arg, Channel) ->
    ScreenData = init_screen_data(),
    Res = maps:fold(
        fun(K, V, Acc) ->
            Data = get_data(K, Arg, Channel),
            Res = format_echart(K, V, Data),
            maps:merge(Acc, #{K => Res})
        end, #{}, ScreenData),
    HookData = run_screen_hook(ProductId, Arg, Channel, Res),
    {ok, HookData};

get_screen(_, _) ->
    error.


%%get_history_data(ProductId, DeviceId, Type, Function, FunctionMap, Group, Having, Where, Order, Channel, Limit, Skip)
get_data(<<"total">>, Arg, Channel) ->
    ProductId = maps:get(<<"ordertype">>, Arg, error),
    Function = <<"last">>,
    Group = undefined,
    FunctionMap = #{<<"avg">> => [<<"percent">>],
        <<"sum">> => [<<"statis_produced">>, <<"statis_qualitified">>]},
    Where = format_where(Arg),
    case dgiot_factory_data:get_history_data(ProductId, undefined, undefined, Function, FunctionMap, Group, undefined, Where, undefined, Channel, undefined, undefined) of
        {ok, {_, Data}} ->
            Data;
        _ ->

            []
    end;
get_data(<<"product">>, Arg, Channel) ->
    ProductId = maps:get(<<"ordertype">>, Arg, error),
    Function = <<"last">>,
    Group = <<"order_productid">>,
    FunctionMap = #{<<"avg">> => [<<"percent">>],
        <<"sum">> => [<<"statis_produced">>, <<"statis_qualitified">>]},
    Where = format_where(Arg),
    case dgiot_factory_data:get_history_data(ProductId, undefined, undefined, Function, FunctionMap, Group, undefined, Where, undefined, Channel, undefined, undefined) of
        {ok, {_, Data}} ->
            io:format("~s ~p Data = ~p ~n", [?FILE, ?LINE, length(Data)]),
            Data;
        _R ->
            io:format("~s ~p here  ~p  ~n", [?FILE, ?LINE,_R]),
            []
    end;

get_data(<<"process">>, Arg, Channel) ->
    ProductId = maps:get(<<"ordertype">>, Arg, error),
    Function = <<"last">>,
    Group = <<"order_process">>,
    FunctionMap = #{<<"avg">> => [<<"percent">>],
        <<"sum">> => [<<"statis_produced">>, <<"statis_qualitified">>]},
    Where = format_where(Arg),
    case dgiot_factory_data:get_history_data(ProductId, undefined, undefined, Function, FunctionMap, Group, undefined, Where, undefined, Channel, undefined, undefined) of
        {ok, {_, Data}} ->
            io:format("~s ~p Data = ~p ~n", [?FILE, ?LINE, length(Data)]),
            Data;
        _ ->
            io:format("~s ~p here  ~n", [?FILE, ?LINE]),
            []
    end;
get_data(<<"order">>, Arg, Channel) ->
    ProductId = maps:get(<<"ordertype">>, Arg, error),
    Function = <<"last">>,
    Group = <<"ordername">>,
    FunctionMap = #{<<"avg">> => [<<"percent">>],
        <<"sum">> => [<<"statis_produced">>, <<"statis_qualitified">>]},
    Where = format_where(Arg),
    case dgiot_factory_data:get_history_data(ProductId, undefined, undefined, Function, FunctionMap, Group, undefined, Where, undefined, Channel, undefined, undefined) of
        {ok, {_, Data}} ->
            io:format("~s ~p Data = ~p ~n", [?FILE, ?LINE, length(Data)]),
            Data;
        _ ->
            io:format("~s ~p here  ~n", [?FILE, ?LINE]),
            []
    end;

get_data(_, _, _) ->
    [].


format_echart(<<"total">>, _, [Map]) when is_map(Map) ->
    Produced = maps:get(<<"statis_produced">>, Map, 0),
    Qualitified = maps:get(<<"statis_qualitified">>, Map, 0),
    Schedule = maps:get(<<"statis_schedule">>, Map, 0),
    Percent = maps:get(<<"statis_percent">>, Map, 0),
    #{
        <<"produced">> => Produced,
        <<"qualitified">> => Qualitified,
        <<"percent">> => Percent ,
        <<"schedule">> => Schedule
    };
format_echart(<<"product">>, Model, Data) ->
    {XAxis, SeriesData} = lists:foldl(
        fun(Item, {X, Y}) ->
            case maps:find(<<"order_productid">>, Item) of
                {ok, ProductId} ->
                    Produced = maps:get(<<"statis_produced">>, Item, 0),
                    Qualitified = maps:get(<<"statis_qualitified">>, Item, 0),
                    Schedule = maps:get(<<"statis_schedule">>, Item, 0),
                    ProducedList = maps:get(<<"statis_produced">>, Y, []),
                    QualitifiedList = maps:get(<<"statis_qualitified">>, Y, []),
                    ScheduleList = maps:get(<<"statis_schedule">>, Y, []),
                    {X ++ [ProductId], #{
                        <<"statis_produced">> => ProducedList ++ [Produced],
                        <<"statis_qualitified">> => QualitifiedList ++ [Qualitified],
                        <<"statis_schedule">> => ScheduleList ++ [Schedule]
                    }}
            end
        end, {[], #{}}, Data),
    New = #{
        <<"title">> => #{<<"text">> => <<"产品产量统计"/utf8>>},
        <<"xAxis">> => #{<<"data">> => XAxis},
        <<"series">> => format_process_series(SeriesData)
    },
    dgiot_map:merge(Model, New);
format_echart(<<"process">>, Model, Data) ->
    {XAxis, SeriesData} = lists:foldl(
        fun(Item, {X, Y}) ->
            case maps:find(<<"order_process">>, Item) of
                {ok, Process} ->
                    Produced = maps:get(<<"statis_produced">>, Item, 0),
                    Qualitified = maps:get(<<"statis_qualitified">>, Item, 0),
                    Schedule = maps:get(<<"statis_schedule">>, Item, 0),
                    ProducedList = maps:get(<<"statis_produced">>, Y, []),
                    QualitifiedList = maps:get(<<"statis_qualitified">>, Y, []),
                    ScheduleList = maps:get(<<"statis_schedule">>, Y, []),
                    {X ++ [Process], #{
                        <<"statis_produced">> => ProducedList ++ [Produced],
                        <<"statis_qualitified">> => QualitifiedList ++ [Qualitified],
                        <<"statis_schedule">> => ScheduleList ++ [Schedule]
                    }}
            end
        end, {[], #{}}, Data),
    New = #{
        <<"title">> => #{<<"text">> => <<"工序产量统计"/utf8>>},
        <<"xAxis">> => #{<<"data">> => XAxis},
        <<"series">> => format_process_series(SeriesData)
    },
    dgiot_map:merge(Model, New);
format_echart(<<"order">>, Model, Data) ->
    {XAxis, SeriesData} = lists:foldl(
        fun(Item, {X, Y}) ->
            case maps:find(<<"order_productid">>, Item) of
                {ok, ProductId} ->
                    Produced = maps:get(<<"statis_produced">>, Item, 0),
                    Qualitified = maps:get(<<"statis_qualitified">>, Item, 0),
                    Schedule = maps:get(<<"statis_schedule">>, Item, 0),
                    ProducedList = maps:get(<<"statis_produced">>, Y, []),
                    QualitifiedList = maps:get(<<"statis_qualitified">>, Y, []),
                    ScheduleList = maps:get(<<"statis_schedule">>, Y, []),
                    {X ++ [ProductId], #{
                        <<"statis_produced">> => ProducedList ++ [Produced],
                        <<"statis_qualitified">> => QualitifiedList ++ [Qualitified],
                        <<"statis_schedule">> => ScheduleList ++ [Schedule]
                    }}
            end
        end, {[], #{}}, Data),
    New = #{
        <<"title">> => #{<<"text">> => <<"订单产量统计"/utf8>>},
        <<"xAxis">> => #{<<"data">> => XAxis},
        <<"series">> => format_process_series(SeriesData)
    },
    dgiot_map:merge(Model, New);
format_echart(_, _, _) ->
    #{}.

format_where(Arg) ->
    format_where(Arg, <<"">>).
format_where(#{<<"ordername">> := OrderName} = Arg, Acc) when is_binary(OrderName) and (byte_size(OrderName) > 0) ->
    format_where(maps:without([<<"ordername">>], Arg), <<Acc/binary, "order_ordername like '", OrderName/binary, "' and ">>);
format_where(#{<<"productid">> := ProductId} = Arg, Acc) when is_binary(ProductId) and (byte_size(ProductId) > 0) ->
    format_where(maps:without([<<"productid">>], Arg), <<Acc/binary, "order_productid like '", ProductId/binary, "' and ">>);
format_where(#{<<"process">> := Process} = Arg, Acc) when is_binary(Process) and (byte_size(Process) > 0) ->
    format_where(maps:without([<<"process">>], Arg), <<Acc/binary, "order_process like '", Process/binary, "' and ">>);


format_where(#{<<"startTime">> := StartTime} = Arg, Acc) when is_binary(StartTime) and (byte_size(StartTime) > 0) ->
    format_where(maps:without([<<"startTime">>], Arg), <<Acc/binary, "createdat >", StartTime/binary, " and ">>);


format_where(#{<<"endTime">> := EndTime} = Arg, Acc) when is_binary(EndTime) and (byte_size(EndTime) > 0) ->
    io:format("~s ~p EndTime = ~p ~n", [?FILE, ?LINE, EndTime]),
    format_where(maps:without([<<"endTime">>], Arg), <<Acc/binary, "createdat < ", EndTime/binary, " and ">>);

format_where(_, Acc) ->
    case byte_size(Acc) > 0 of
        true ->
            List = dgiot_utils:to_list(Acc),
            Len = length(List),
            End = Len - 4,
            dgiot_utils:to_binary(lists:sublist(List, 1, End));
        _ ->
            Acc
    end.

init_screen_data() ->
    dgiot_factory_utils:get_json_file(?FACTORY_SCREEN).




updata_screen_data(OldData, BaseData, Content) ->
%%    遍历所有统计图类型，逐个更新
    maps:fold(fun(K, V, Acc) ->
%%        更新单个图
        NewV = updata_one_data(K, V, BaseData, Content),
        Acc#{K => NewV}
              end, #{}, OldData).
run_screen_hook(ProductId, Arg, Channel, Res) ->
    case dgiot_hook:run_hook({factory, screen, ProductId}, [Channel, Arg, Res]) of
        {ok, [NewRes]} ->
            NewRes;
        _R ->
            io:format("~s ~p _R =~p  ~n", [?FILE, ?LINE, _R]),
            Res
    end.

%%        更新单个图
updata_one_data(<<"total">>, #{<<"progress">> := TotalProgress, <<"schedule">> := TotalSchedule, <<"produced">> := TotalProduced},
    BaseData, #{<<"baseInfo">> := #{<<"Material_List">> := #{<<"Number">> := Schedule}}}) ->
    Produced = maps:get(<<"product_num">>, BaseData, 0),
    Progress = maps:get(<<"progress">>, BaseData, 0),
    #{<<"schedule">> => TotalSchedule + dgiot_utils:to_int(Schedule), <<"produced">> => TotalProduced + dgiot_utils:to_int(Produced), <<"progress">> => TotalProgress + dgiot_utils:to_int(Progress)};

%%#{<<"水刺">> => 类别
%% #{<<"Produced">> => 1, 生产总数
%%<<"Qualitied">> => 1, 合格数
%%<<"Schedule">> => 1 计划数
%% }}
updata_one_data(<<"process">>, Data, BaseData,
    #{<<"baseInfo">> := #{<<"Material_List">> := #{<<"Number">> := Schedule, <<"Production_workshop">> := Production_workshop}}}) ->
    Produced = maps:get(<<"product_num">>, BaseData, 0),
    Progress = maps:get(<<"progress">>, BaseData, 0),
    updata_process_echart(Data, Schedule, Produced, Progress, Production_workshop);

updata_one_data(<<"product">>, Data, BaseData,
    #{<<"baseInfo">> := #{<<"Material_List">> := #{<<"Number">> := Schedule, <<"Material_code">> := Material_code}}}) ->
    Produced = maps:get(<<"product_num">>, BaseData, 0),
    Progress = maps:get(<<"progress">>, BaseData, 0),
    updata_process_echart(Data, Schedule, Produced, Progress, Material_code);

updata_one_data(_, V, _, _) ->
    V.



updata_process_echart(Data, Schedule, Produced, Qualitied, Production_workshop) ->
    case maps:find(Production_workshop, Data) of
        {ok, OldData} ->
            OldProduced = maps:get(<<"Produced">>, OldData, 0),
            OldQualitied = maps:get(<<"Qualitied">>, OldData, 0),
            OldSchedule = maps:get(<<"Schedule">>, OldData, 0),
            NewProduced = OldProduced + dgiot_utils:to_int(Produced),
            NewQualitied = OldQualitied + dgiot_utils:to_int(Qualitied),
            NewSchedule = OldSchedule + dgiot_utils:to_int(Schedule),
            dgiot_map:merge(Data, #{Production_workshop => #{<<"Produced">> => NewProduced, <<"Qualitied">> => NewQualitied, <<"Schedule">> => NewSchedule}});
        _ ->
            dgiot_map:merge(Data, #{Production_workshop => #{<<"Produced">> => dgiot_utils:to_int(Produced), <<"Qualitied">> => dgiot_utils:to_int(Qualitied), <<"Schedule">> => dgiot_utils:to_int(Schedule)}})
    end.



format_bar(V) when is_map(V) ->
    Keys = maps:keys(V),
    case length(Keys) == 0 of
        false ->
            Heads = maps:keys(maps:get(lists:nth(1, Keys), V)),
            Series = lists:foldl(
                fun(Head, Acc) ->
                    HeadData = lists:foldl(
                        fun(Categorry, Acc1) ->
                            Acc1 ++ [dgiot_utils:to_int(maps:get(Head, maps:get(Categorry, V, #{}), 0))]
                        end, [], Keys),
                    Acc#{Head => HeadData}
                end, #{}, Heads),
            {Keys, Series};
        _ ->
            {[], V}
    end;
format_bar(V) ->
    V.


%#{<<"水刺">> => 类别
%% #{<<"Produced">> => 1, 总产量
%%<<"Qualitied">> => 1, 合格产量
%%<<"Schedule">> => 1 计划产量
%% }}

format_process_series(Series) when is_map(Series) ->
    maps:fold(
        fun
            (<<"statis_produced">>, V, Acc) ->
                Acc ++ [#{<<"name">> => <<"总产量"/utf8>>, <<"type">> => <<"bar">>, <<"data">> => V}];
            (<<"statis_qualitified">>, V, Acc) ->
                Acc ++ [#{<<"name">> => <<"实际产量"/utf8>>, <<"type">> => <<"bar">>, <<"data">> => V}];
            (<<"statis_schedule">>, V, Acc) ->
                Acc ++ [#{<<"name">> => <<"计划产量"/utf8>>, <<"type">> => <<"bar">>, <<"data">> => V}];
            (K, V, Acc) ->
                Acc ++ [#{<<"name">> => K, <<"type">> => <<"bar">>, <<"data">> => V}]
        end, [], Series);
format_process_series(Series) ->
    Series.
