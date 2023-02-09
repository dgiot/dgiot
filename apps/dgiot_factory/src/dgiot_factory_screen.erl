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
-export([get_screen/1]).

-export([init_screen_data/0, updata_screen_data/3]).
%%get_screen(_)->
%%    {ok,s}.

get_screen(#{<<"orderType">> := ProductId} = Arg) ->
%%    转换where条件
    Where = format_where(Arg),
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => Where}) of
        {ok, #{<<"results">> := Res}} ->
            get_data(ProductId, Res);
        _ ->
            io:format("~s ~p here ~n", [?FILE, ?LINE]),
            error
    end;
get_screen(_) ->
    error.
format_where(Arg) ->
    Res = maps:fold(
        fun(_, undefined, Acc) ->
            Acc;
            (<<"orderType">>, V, Acc) ->
                Acc#{<<"product">> => dgiot_utils:to_map(V)};
            (<<"process">>, V, Acc) ->
                Acc#{<<"content.baseInfo.Material_List.Production_workshop">> => dgiot_utils:to_map(V)};
            (<<"productId">>, V, Acc) ->
                Acc#{<<"content.baseInfo.Material_List.Material_code">> => dgiot_utils:to_map(V)};
            (<<"orderId">>, V, Acc) ->
                Acc#{<<"name">> => dgiot_utils:to_map(V)};
            (K, V, Acc) ->
                Acc#{K => dgiot_utils:to_map(V)}
        end, #{}, Arg),
    dgiot_bamis:format_multilayer(Res).
get_data(ProductId, Res) ->
%%    初始化数据
    ScreenData = init_screen_data(),
%%    遍历device
    Data = lists:foldl(
        fun(#{<<"basedata">> := BaseData, <<"content">> := Content}, Acc) ->
%%            更新data
            NewData1 = updata_screen_data(Acc, BaseData, Content),
%%            调用hook
            NewData2 = run_screen_hook(ProductId, NewData1, BaseData, Content),
            NewData2
        end, ScreenData, Res),
    io:format("~s ~p here ~n", [?FILE, ?LINE]),
    {ok, format_echart(Data)}.

init_screen_data() ->
%%    初始化字典
    #{
        <<"total">> => #{<<"schedule">> => 0, <<"produced">> => 0, <<"progress">> => 0},
        <<"product">> => init_echart(<<"product">>),
        <<"process">> => init_echart(<<"process">>)
    }.

init_echart(<<"product">>) ->
    #{};
init_echart(<<"process">>) ->
    #{};
init_echart(_) ->
    #{}.


updata_screen_data(OldData, BaseData, Content) ->
%%    遍历所有统计图类型，逐个更新
    maps:fold(fun(K, V, Acc) ->
%%        更新单个图
        NewV = updata_one_data(K, V, BaseData, Content),
        Acc#{K => NewV}
              end, #{}, OldData).
run_screen_hook(ProductId, OldData, BaseData, Content) ->
    case dgiot_hook:run_hook({factory, screen, ProductId}, [BaseData, Content]) of
        {ok, [{ok, Res}]} ->
            Res;
        _ ->
            OldData
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

format_echart(Data) when is_map(Data) ->
    maps:fold(
        fun(K, V, Acc) ->
            FormatedV = format_one_table(K, V),
            Acc#{K => FormatedV}
        end, #{}, Data);
format_echart(_) ->
    io:format("~s ~p here ~n", [?FILE, ?LINE]),
    pass.


format_one_table(<<"process">>, V) when is_map(V) ->
    {XAxis, Series} = format_bar(V),
    Model = dgiot_factory_utils:get_json_file(<<"bar">>),
    New = #{
        <<"title">> => #{<<"text">> => <<"车间产量统计"/utf8>>},
        <<"xAxis">> => #{<<"data">> => XAxis},
        <<"series">> => format_process_series(Series)
    },
    dgiot_map:merge(Model, New);
format_one_table(<<"product">>, V) when is_map(V) ->
    {XAxis, Series} = format_bar(V),
    Model = dgiot_factory_utils:get_json_file(<<"bar">>),
    New = #{
        <<"title">> => #{<<"text">> => <<"产品产量统计"/utf8>>},
        <<"xAxis">> => #{<<"data">> => XAxis},
        <<"series">> => format_process_series(Series)
    },
    dgiot_map:merge(Model, New);
format_one_table(Name, V) ->
    case dgiot_hook:run_hook({factory, screen_foramt, Name}, [V]) of
        {ok, [{ok, Res}]} ->
            Res;
        _ ->
            V
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
            (<<"Produced">>, V, Acc) ->
                Acc ++ [#{<<"name">> => <<"总产量"/utf8>>, <<"type">> => <<"bar">>, <<"data">> => V}];
            (<<"Qualitied">>, V, Acc) ->
                Acc ++ [#{<<"name">> => <<"实际产量"/utf8>>, <<"type">> => <<"bar">>, <<"data">> => V}];
            (<<"Schedule">>, V, Acc) ->
                Acc ++ [#{<<"name">> => <<"计划产量"/utf8>>, <<"type">> => <<"bar">>, <<"data">> => V}];
            (K, V, Acc) ->
                Acc ++ [#{<<"name">> => K, <<"type">> => <<"bar">>, <<"data">> => V}]
        end, [], Series);
format_process_series(Series) ->
    Series.
