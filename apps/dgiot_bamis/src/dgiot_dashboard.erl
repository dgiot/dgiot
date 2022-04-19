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
-export([post_dashboard/2, do_task/2, dashboard/1]).

post_dashboard(#{<<"dashboardId">> := DashboardId} = Args, #{<<"sessionToken">> := SessionToken} = _Context) ->
    NewArgs = Args#{<<"sessionToken">> => SessionToken},
    dgiot_mqtt:publish(DashboardId, <<"dashboard_task/", DashboardId/binary>>, jsx:encode(NewArgs)),
    #{};

post_dashboard(_Args, _Context) ->
    #{}.

%%
%%<<"location">> =>
%%#{<<"__type">> => <<"GeoPoint">>,<<"latitude">> => 30.262441,
%%<<"longitude">> => 120.161324},
%%<<"name">> =>
%%<<229,164,170,233,152,179,232,131,189,230,142,167,229,136,
%%182,229,153,168,49,50,51,52,...>>,
%%<<"objectId">> => <<"5c413c7040">>,

%%  当前只实现了百度地图，后续可以支持腾讯地图
do_task(#{<<"dataType">> := <<"map">>, <<"vuekey">> := <<"baiduMap">>, <<"table">> := <<"Device">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
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

do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"app_count">>, <<"table">> := <<"App">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"App">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count}} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"app_count">>, <<"table">> => <<"App">>, <<"value">> => Count})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"product_count">>, <<"table">> := <<"Product">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"Product">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count}} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"product_count">>, <<"table">> => <<"Product">>, <<"value">> => Count})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"dev_online_count">>, <<"table">> := <<"Device">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count}} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"dev_online_count">>, <<"table">> => <<"Device">>, <<"value">> => Count})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"dev_off_count">>, <<"table">> := <<"Device">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count}} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"dev_off_count">>, <<"table">> => <<"Device">>, <<"value">> => Count})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"device_count">>, <<"table">> := <<"Device">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count}} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"device_count">>, <<"table">> => <<"Device">>, <<"value">> => Count})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"ChartStatus">>, <<"table">> := <<"Device">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    Where = maps:get(<<"where">>, Query, #{}),
    OnlineCount =
        case dgiot_parse:query_object(<<"Device">>, Query#{<<"keys">> => [<<"count(*)">>], <<"where">> => Where#{<<"status">> => <<"ONLINE">>}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
            {ok, #{<<"count">> := OnlineCount1}} ->
                OnlineCount1;
            _ ->
                0
        end,
    OfflineCount =
        case dgiot_parse:query_object(<<"Device">>, Query#{<<"keys">> => [<<"count(*)">>], <<"where">> => Where#{<<"status">> => <<"OFFLINE">>}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
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

do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := <<"warn_count">>, <<"table">> := <<"Notification">>, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(<<"Device">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count}} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => <<"warn_count">>, <<"table">> => <<"Notification">>, <<"value">> => Count})),
            send(DashboardId, Base64);
        _ ->
            pass
    end;

do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := Vuekey, <<"table">> := Table, <<"query">> := Query}, #task{dashboardId = DashboardId, sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(Table, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Results} = NewResults} ->
            Base64 =
                case Table of
                    <<"Product">> ->
                        Products =
                            lists:foldl(fun(X, Acc) ->
                                case X of
                                    #{<<"objectId">> := ObjectId} ->
                                        DeviceChild = getDevice(ObjectId, SessionToken),
                                        Acc ++ [X#{<<"deviceChild">> => DeviceChild}];
                                    _ ->
                                        Acc
                                end
                                        end, [], Results),
                        base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => Vuekey, <<"table">> => Table, <<"value">> => #{<<"count">> => Count, <<"results">> => Products}}));
                    _ ->
                        base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => Vuekey, <<"table">> => Table, <<"value">> => NewResults}))
                end,
            send(DashboardId, Base64);
        _ ->
            pass
    end;

do_task(#{<<"dataType">> := <<"list">>}, #task{sessiontoken = SessionToken}) ->
    Topic = <<"$dg/dashboard/getDashboard">>,
    case dashboard(SessionToken) of
        {ok, Info} ->
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"list">>, <<"value">> => Info})),
            dgiot_data:insert({dashboard, Topic}, Base64),
            dgiot_mqtt:publish(self(), Topic, Base64);
        _ ->
            pass
    end;

do_task(Task, State) ->
    ?LOG(info, "Task ~p", [Task]),
    ?LOG(info, "State ~p", [State]),
    ok.

send(DashboardId, Base64) ->
    Topic = <<"$dg/dashboard/", DashboardId/binary, "/report">>,
    dgiot_mqtt:publish(self(), Topic, Base64).

getDevice(<<"all">>, SessionToken) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"limit">> => 10}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"objectId">> := ObjectId, <<"name">> := Name} ->
                        Acc ++ [#{<<"objectid">> => ObjectId, <<"name">> => Name}];
                    _ ->
                        Acc
                end
                        end, [], Results);
        _ ->
            []
    end;

getDevice(ProductId, SessionToken) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"where">> => #{<<"product">> => ProductId}, <<"limit">> => 10}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"objectId">> := ObjectId, <<"name">> := Name} ->
                        Acc ++ [#{<<"objectid">> => ObjectId, <<"name">> => Name}];
                    _ ->
                        Acc
                end
                        end, [], Results);
        _ ->
            []
    end.

dashboard(SessionToken) ->
    ProductAndDeviceCount = case get_product_and_device_count(SessionToken) of
                                {ok, Info1} ->
                                    Info1;
                                _ ->
                                    {}
                            end,
    DeviceStatus = case get_device_status(SessionToken) of
                       {ok, Info2} ->
                           Info2;
                       _ ->
                           {}
                   end,
    DeviceCountAndProductCount = case get_device_count_and_product_count(SessionToken) of
                                     {ok, Info3} ->
                                         Info3;
                                     _ ->
                                         {}
                                 end,
    DeviceStatusList = case get_device_status_list(SessionToken) of
                           {ok, Info4} ->
                               Info4;
                           _ ->
                               {}
                       end,
    ErrorList = case get_error_list(SessionToken) of
                    {ok, Info5} ->
                        Info5;
                    _ ->
                        {}
                end,
    DeviceLocationList = case get_location_list(SessionToken) of
                             {ok, Info6} ->
                                 Info6;
                             _ ->
                                 {}
                         end,
    {ok, #{<<"deviceStatus">> => DeviceStatus,
        <<"deviceCountAndProductCount">> => DeviceCountAndProductCount,
        <<"productDevice">> => ProductAndDeviceCount,
        <<"deviceStatusRecords">> => DeviceStatusList,
        <<"errorList">> => ErrorList,
        <<"deviceLocationRecords">> => DeviceLocationList
    }}.

get_product_and_device_count(SessionToken) ->
    case dgiot_parse:query_object(<<"Product">>, #{<<"keys">> => [<<"count(*)">>]}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Results}} ->
            List = lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"objectId">> := ObjectId, <<"name">> := Name} ->
                        DeviceCount =
                            case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"where">> => #{<<"product">> => ObjectId}}) of
                                {ok, #{<<"count">> := DeviceCount1}} ->
                                    DeviceCount1
                            end,
                        Acc ++ [#{<<"objectid">> => ObjectId, <<"name">> => Name, <<"devicecount">> => DeviceCount}];
                    _ ->
                        Acc
                end
                               end, [], Results),
            {ok, #{<<"Records">> => List, <<"productCount">> => Count}};
        Other ->
            io:format("~p ~n", [Other])
    end.

get_device_status(SessionToken) ->
    OnLineCount = case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"where">> => #{<<"status">> => <<"ONLINE">>}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                      {ok, #{<<"count">> := Count1}} ->
                          Count1;
                      _ ->
                          0
                  end,
    OffLineCount = case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"where">> => #{<<"status">> => <<"OFFLINE">>}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                       {ok, #{<<"count">> := Count2}} ->
                           Count2;
                       _ ->
                           0
                   end,
    {ok, [#{<<"name">> => <<"在线设备"/utf8>>, <<"value">> => OnLineCount}, #{<<"name">> => <<"离线设备"/utf8>>, <<"value">> => OffLineCount}]}.

get_device_count_and_product_count(SessionToken) ->
    DeviceCount = case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>]}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                      {ok, #{<<"count">> := Count1}} ->
                          Count1;
                      _ ->
                          0
                  end,
    ProductCount = case dgiot_parse:query_object(<<"Product">>, #{<<"keys">> => [<<"count(*)">>]}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                       {ok, #{<<"count">> := Count2}} ->
                           Count2;
                       _ ->
                           0
                   end,
    {ok, #{<<"deviceCount"/utf8>> => DeviceCount, <<"productCount"/utf8>> => ProductCount}}.

get_device_status_list(SessionToken) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>]}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} ->
            List = lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"objectId">> := ObjectId, <<"name">> := Name, <<"status">> := Status} ->
                        Acc ++ [#{<<"objectid">> => ObjectId, <<"name">> => Name, <<"deviceStatus">> => Status}];
                    _ ->
                        Acc
                end
                               end, [], Results),
            {ok, #{<<"Records">> => List}};
        Other ->
            io:format("~p ~n", [Other])
    end.

get_error_list(SessionToken) ->
    case dgiot_parse:query_object(<<"Notification">>, #{<<"keys">> => [<<"count(*)">>], <<"order">> => <<"-createdAt">>, <<"limit">> => 20, <<"where">> => #{<<"status">> => <<"0">>}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} ->
            NewList = lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"content">> := Content, <<"createdAt">> := Createdat} ->
                        DeviceId = maps:get(<<"_deviceid">>, Content, <<"">>),
                        case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                            {ok, #{<<"name">> := DeviceName, <<"detail">> := Detail}} ->
                                Address = maps:get(<<"address">>, Detail, <<"无位置"/utf8>>),
                                NewDate = dgiot_datetime:format(dgiot_datetime:to_localtime(Createdat), <<"YY-MM-DD HH:NN:SS">>),
                                Acc ++ [[DeviceName, Address, <<"设备"/utf8, DeviceName/binary, "离线"/utf8>>, NewDate]];
                            _ ->
                                Acc
                        end;
                    _Other2 ->
                        Acc
                end
                                  end, [], Results),
            Header = [<<"设备名称"/utf8>>, <<"设备地址"/utf8>>, <<"报警内容"/utf8>>, <<"离线时间"/utf8>>],
            {ok, #{<<"header">> => Header, <<"data">> => NewList}};
        _ ->
            {error, <<"system error">>}
    end.

get_location_list(SessionToken) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"order">> => <<"-createdAt">>}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} ->
            NewList = lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"objectId">> := ObjectId, <<"name">> := Name, <<"location">> := #{<<"latitude">> := Latitude, <<"longitude">> := Longitude}} ->
                        Acc ++ [#{<<"objectId">> => ObjectId, <<"deviceName">> => Name, <<"location">> => #{<<"lat">> => Latitude, <<"lng">> => Longitude}}];
                    _ ->
                        Acc
                end
                                  end, [], Results),
            {ok, #{<<"records">> => NewList}}
    end.
