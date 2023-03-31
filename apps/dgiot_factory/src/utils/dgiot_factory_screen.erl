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
-define(KEY, <<"key">>).
-export([get_screen/2]).

-export([format_data/3, turn_echart/3]).

-export([init_screen_data/0, updata_screen_data/3, format_bar/1]).

get_screen(#{<<"ordertype">> := ProductId} = Arg, Channel) ->
    ScreenData = init_screen_data(),
    Res = maps:fold(
        fun(EchartName, Para, Acc) ->
            Data = get_data(EchartName, Para, Arg, Channel),
%%            case EchartName of
%%                <<"order">> ->
%%                    io:format("~s ~p Data = ~p. ~n", [?FILE, ?LINE, Data]);
%%                _ ->
%%                    pass
%%            end,
            FormatedData = format_data(EchartName, Para, dgiot_factory_data:kill_null(Data)),
%%            case EchartName of
%%                <<"order">> ->
%%                    io:format("~s ~p FormatedData = ~p. ~n", [?FILE, ?LINE, FormatedData]);
%%                _ ->
%%                    pass
%%            end,
            Res = format_echart(EchartName, Para, FormatedData),
            maps:merge(Acc, Res)
        end, #{}, ScreenData),
    HookData = run_screen_hook(ProductId, Arg, Channel, Res),
    {ok, HookData};

get_screen(_, _) ->
    error.
get_func_map(#{<<"data">> := Data}) when is_map(Data) ->
    maps:fold(
        fun(K, #{<<"func">> := Func}, Acc) ->
            OldList = maps:get(Func, Acc, []),
            NewList = OldList ++ [K],
            Acc#{Func => NewList};
            (_, _, Acc) ->
                Acc
        end, #{}, Data);
get_func_map(_) ->
    #{}.

%%get_history_data(ProductId, DeviceId, Type, Function, FunctionMap, Group, Having, Where, Order, Channel, Limit, Skip)
get_data(_, V, Arg, Channel) ->
    ProductId = maps:get(<<"ordertype">>, Arg, error),
    Function = <<"last">>,
    Group = maps:get(<<"group">>, V, undefined),
    FunctionMap = get_func_map(V),
    Where = format_where(Arg),
    case dgiot_factory_data:get_history_data(ProductId, undefined, undefined, Function, FunctionMap, Group, undefined, Where, undefined, Channel, undefined, undefined) of
        {ok, {_, Data}} ->
            Data;
        _ ->

            []
    end.



format_data(_Name, #{<<"group">> := Group} = Para, Data) ->
    io:format("~s ~p _Name = ~p. ~n", [?FILE, ?LINE, _Name]),
    DataKeys = maps:keys(maps:get(<<"data">>, Para, #{})),
    lists:foldl(
        fun(LineData, Acc) ->
            ValueMap = lists:foldl(
                fun(Key, Acc1) ->
                    Acc1#{Key => maps:get(Key, LineData, 0)}
                end, #{}, DataKeys),
            GroupValue = maps:get(Group, LineData, null),
            Acc#{GroupValue => ValueMap}
        end, #{}, Data);
format_data(_, Para, [Data]) when is_map(Data) ->
    DataKeys = maps:keys(maps:get(<<"data">>, Para, #{})),
    Res = lists:foldl(
        fun(Key, Acc) ->
            Acc#{Key => maps:get(Key, Data, 0)}
        end, #{}, DataKeys),
    #{<<"数据"/utf8>> => Res};
format_data(_, _, _) ->
    #{}.


format_echart(EchartName, #{<<"type">> := Type} = Model, FormatedData) ->
    EchartData = turn_echart(Type, Model, FormatedData),
    #{EchartName => EchartData}.

turn_echart(<<"total">>, #{<<"data">> := Data}, FormatedData) ->
    InitModel = lists:foldl(
        fun(Key, Acc) ->
            Acc#{Key => 0}
        end, #{}, maps:keys(Data)),

    Map = maps:fold(
        fun(_, DataMap, OldAcc) ->
            maps:fold(
                fun(Key, OldSum, Acc) ->

                    Num = case maps:find(Key, DataMap) of
                              {ok, null} ->
                                  0;
                              error ->
                                  0;
                              {ok, R} ->
                                  R
                          end,

                    Acc#{Key => OldSum + Num}
                end, #{}, OldAcc)
        end, InitModel, FormatedData),
%%    io:format("~s ~p Map = ~p. ~n", [?FILE, ?LINE, Map]),
    case maps:get(<<"statis_produced">>, Map, 0) of
        0 ->
            Map#{<<"statis_percent">> => 0};
        Ptatis_produced ->
            Qualified = maps:get(<<"statis_qualitified">>, Map, 0),
%%            io:format("~s ~p Qualified = ~p. ~n", [?FILE, ?LINE, Qualified]),
            NewMap = maps:merge(Map, #{<<"statis_percent">> => dgiot_factory_utils:float(Qualified / Ptatis_produced, 2)}),
%%            io:format("~s ~p NewMap = ~p. ~n", [?FILE, ?LINE, NewMap]),
            NewMap

    end;
turn_echart(<<"bar">>, #{<<"model">> := #{<<"series">> := SeriesMod} = Model}, FormatedData) ->
    {XAxis, NewSeries} = maps:fold(
        fun(X, Data, {Xlist, SeriesAcc}) ->
            NewSeriesAcc = lists:foldl(
                fun(#{?KEY := Key} = OneSeries, Acc) ->
                    OldList = maps:get(<<"data">>, OneSeries, []),
                    NewValue = maps:get(Key, Data, 0),
                    NewList = OldList ++ [NewValue],
                    Acc ++ [OneSeries#{<<"data">> => NewList}];
                    (S, Acc) ->
                        Acc ++ [S]
                end, [], SeriesAcc),
            {Xlist ++ [X], NewSeriesAcc}
        end, {[], SeriesMod}, FormatedData),
    io:format("~s ~p XAxis = ~p. ~n", [?FILE, ?LINE, XAxis]),
    dgiot_map:merge(Model, #{<<"xAxis">> => #{<<"data">> => XAxis}, <<"series">> => NewSeries}).


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

%%format_process_series(Series) when is_map(Series) ->
%%    maps:fold(
%%        fun
%%            (<<"statis_produced">>, V, Acc) ->
%%                Acc ++ [#{<<"name">> => <<"总产量"/utf8>>, <<"type">> => <<"bar">>, <<"data">> => V}];
%%            (<<"statis_qualitified">>, V, Acc) ->
%%                Acc ++ [#{<<"name">> => <<"实际产量"/utf8>>, <<"type">> => <<"bar">>, <<"data">> => V}];
%%            (<<"statis_schedule">>, V, Acc) ->
%%                Acc ++ [#{<<"name">> => <<"计划产量"/utf8>>, <<"type">> => <<"bar">>, <<"data">> => V}];
%%            (K, V, Acc) ->
%%                Acc ++ [#{<<"name">> => K, <<"type">> => <<"bar">>, <<"data">> => V}]
%%        end, [], Series);
%%format_process_series(Series) ->
%%    Series.
