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
-include("dgiot_topo.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([post_dashboard/2, do_task/2]).


post_dashboard(Args, #{<<"sessionToken">> := SessionToken} = _Context) ->
    supervisor:start_child(dashboard_task, [Args#{<<"sessionToken">> => SessionToken}]),
    timer:sleep(1000).
%%
%%<<"location">> =>
%%#{<<"__type">> => <<"GeoPoint">>,<<"latitude">> => 30.262441,
%%<<"longitude">> => 120.161324},
%%<<"name">> =>
%%<<229,164,170,233,152,179,232,131,189,230,142,167,229,136,
%%182,229,153,168,49,50,51,52,...>>,
%%<<"objectId">> => <<"5c413c7040">>,

do_task(#{<<"dataType">> := <<"map">>, <<"vuekey">> := Vuekey, <<"table">> := Table, <<"query">> := Query}, #task{sessiontoken = SessionToken}) ->
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
            Topic = <<"dashboard/", SessionToken/binary, "/post">>,
            Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"map">>, <<"vuekey">> => Vuekey, <<"table">> => Table, <<"value">> => NewResult})),
            dgiot_mqtt:publish(self(), Topic, Base64);
        _ ->
            pass
    end;

%% dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>],<<"order">> => <<"-updatedAt">>, <<"limit">> => 10, <<"skip">> => 0}, [{"X-Parse-Session-Token", <<"r:3bcb41395765d5affefd549a0bcc6f0f">>}], [{from, rest}])
do_task(#{<<"dataType">> := <<"card">>, <<"vuekey">> := Vuekey, <<"table">> := Table, <<"query">> := Query}, #task{sessiontoken = SessionToken}) ->
    case dgiot_parse:query_object(Table, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Results} = NewResults} ->
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
                    Topic = <<"dashboard/", SessionToken/binary, "/post">>,
                    Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => Vuekey, <<"table">> => Table, <<"value">> => #{<<"count">> => Count, <<"results">> => Products}})),
                    dgiot_mqtt:publish(self(), Topic, Base64);
                <<"ChartStatus">> ->
                    OnlineCount =
                        case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"where">> => #{<<"status">> => <<"ONLINE">>}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                            {ok, #{<<"count">> := OnlineCount1}} ->
                                OnlineCount1;
                            _ ->
                                0
                        end,
                    OfflineCount =
                        case dgiot_parse:query_object(<<"Device">>, #{<<"keys">> => [<<"count(*)">>], <<"where">> => #{<<"status">> => <<"OFFLINE">>}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
                            {ok, #{<<"count">> := OfflineCount2}} ->
                                OfflineCount2;
                            _ ->
                                0
                        end,
                    Topic = <<"dashboard/", SessionToken/binary, "/post">>,
                    Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => Vuekey, <<"table">> => Table, <<"value">> => #{<<"chartData">> => #{<<"columns">> => [<<"状态"/utf8>>, <<"数量"/utf8>>], <<"rows">> => [#{<<"状态"/utf8>> => <<"在线"/utf8>>, <<"数量"/utf8>> => OnlineCount}, #{<<"状态"/utf8>> => <<"离线"/utf8>>, <<"数量"/utf8>> => OfflineCount}]}}})),
                    dgiot_mqtt:publish(self(), Topic, Base64);
                _ ->
                    Topic = <<"dashboard/", SessionToken/binary, "/post">>,
                    Base64 = base64:encode(jsx:encode(#{<<"dataType">> => <<"card">>, <<"vuekey">> => Vuekey, <<"table">> => Table, <<"value">> => NewResults})),
                    dgiot_mqtt:publish(self(), Topic, Base64)
            end;
        _ ->
            pass
    end;

do_task(Task, State) ->
    ?LOG(info, "Task ~p", [Task]),
    ?LOG(info, "State ~p", [State]),
    ok.

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
