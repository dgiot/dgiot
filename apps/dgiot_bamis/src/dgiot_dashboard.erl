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
-module(dgiot_dashboard).
-author("stoneliu").
-include("dgiot_bamis.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([post_dashboard/2, do_task/2]).

post_dashboard(#{<<"dashboardId">> := DashboardId} = Args, #{<<"sessionToken">> := SessionToken} = _Context) ->
    NewArgs = Args#{<<"sessionToken">> => SessionToken},
    dgiot_mqtt:publish(DashboardId, <<"dashboard_task/", DashboardId/binary>>, jsx:encode(NewArgs)),
    #{};

post_dashboard(_Args, _Context) ->
    #{}.

%%  应用总数卡片
%% Query = #{<<"count">> => [<<"objectId">>], <<"keys">> => [<<"name">>, <<"location">>, <<"status">>]}.
do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"app_count">>, <<"table">> := <<"App">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"App">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := _Count} = Value} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"app_count">>, <<"table">> => <<"App">>, <<"value">> => Value})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

%%  产品总数卡片
do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"product_count">>, <<"table">> := <<"Product">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"Product">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := _Count} = Value} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"product_count">>, <<"table">> => <<"Product">>,  <<"value">> => Value})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

%%  在线设备总数卡片
do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"dev_online_count">>, <<"table">> := <<"Device">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := _Count} = Value} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"dev_online_count">>, <<"table">> => <<"Device">>,  <<"value">> => Value})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

%%  离线设备总数卡片
do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"dev_off_count">>, <<"table">> := <<"Device">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := _Count} = Value} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"dev_off_count">>, <<"table">> => <<"Device">>, <<"value">> => Value})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

%%  设备总数卡片
do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"device_count">>, <<"table">> := <<"Device">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := _Count} = Value} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"device_count">>, <<"table">> => <<"Device">>, <<"value">> => Value})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

%%  设备在线/离线总数卡片
do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"ChartStatus">>, <<"table">> := <<"Device">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    Where = maps:get(<<"where">>, Query, #{}),
    OnlineCount =
        case dgiot_parse:query_object(<<"Device">>, Query#{<<"count">> => <<"objectId">>, <<"where">> => Where#{<<"status">> => <<"ONLINE">>}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
            {ok, #{<<"count">> := OnlineCount1}} ->
                OnlineCount1;
            _ ->
                0
        end,
    OfflineCount =
        case dgiot_parse:query_object(<<"Device">>, Query#{<<"count">> => <<"objectId">>, <<"where">> => Where#{<<"status">> => <<"OFFLINE">>}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
            {ok, #{<<"count">> := OfflineCount2}} ->
                OfflineCount2;
            _ ->
                0
        end,
    Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"ChartStatus">>, <<"table">> => <<"Device">>,
        <<"value">> => #{<<"chartData">> => #{
            <<"columns">> => [<<"状态"/utf8>>, <<"数量"/utf8>>],
            <<"rows">> => [
                #{<<"状态"/utf8>> => <<"在线"/utf8>>, <<"数量"/utf8>> => OnlineCount},
                #{<<"状态"/utf8>> => <<"离线"/utf8>>, <<"数量"/utf8>> => OfflineCount}]}}})),
    send(DashboardId, Base64);

%%  告警总数卡片
do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"warn_count">>, <<"table">> := <<"Notification">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"Notification">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := _Count} = Value} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"warn_count">>, <<"table">> => <<"Notification">>, <<"value">> => Value})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

%%  当前只实现了百度地图，后续可以支持腾讯地图
%%Query5 = #{<<"count">> => [<<"objectId">>], <<"keys">> => [<<"name">>, <<"location">>, <<"status">>],<<"limit">> =>1}.
do_task(#{<<"dataType">> := <<"map">>, <<"vuekey">> := <<"baiduMap">>, <<"table">> := <<"Device">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
%%    io:format("~s ~p Client = ~p.~n", [?FILE, ?LINE, Query]),
    case dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} ->
            NewResult =
                lists:foldl(fun(X, Acc) ->
                    case X of
                        #{<<"objectId">> := ObjectId, <<"name">> := Name, <<"status">> := <<"ONLINE">>, <<"location">> := #{<<"latitude">> := Latitude, <<"longitude">> := Longitude}} ->
                            [BaiduLongitude, BaiduLatitude] = dgiot_gps:get_baidu_gps(Longitude, Latitude, 0, -0.0013),
                            Acc ++ [#{<<"objectId">> => ObjectId, <<"name">> => Name, <<"icon">> => <<"1">>, <<"location">> => #{<<"latitude">> => BaiduLatitude, <<"longitude">> => BaiduLongitude}}];
                        #{<<"objectId">> := ObjectId, <<"name">> := Name, <<"status">> := <<"OFFLINE">>, <<"location">> := #{<<"latitude">> := Latitude, <<"longitude">> := Longitude}} ->
                            [BaiduLongitude, BaiduLatitude] = dgiot_gps:get_baidu_gps(Longitude, Latitude, 0, -0.0013),
                            Acc ++ [#{<<"objectId">> => ObjectId, <<"name">> => Name, <<"icon">> => <<"2">>, <<"location">> => #{<<"latitude">> => BaiduLatitude, <<"longitude">> => BaiduLongitude}}];
                        _ ->
                            Acc
                    end
                            end, [], Results),
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"map">>, <<"vuekey">> => <<"baiduMap">>, <<"table">> => <<"Device">>, <<"value">> => NewResult})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

do_task(_Task, _State) ->
    ok.

send(DashboardId, Base64) ->
    Topic = <<"$dg/dashboard/", DashboardId/binary, "/report">>,
    dgiot_mqtt:publish(self(), Topic, Base64).
