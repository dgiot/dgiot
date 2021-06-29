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

-module(dgiot_sinmahe).
-include_lib("dgiot/include/logger.hrl").
-author("jonhl").

-export([
    get_sinmahe_json/0,
    create_subdev/3,
    test_fault/0,
    send_fault/4,
    send/4,
    get_fault/1,
    post_fault/0,
    update_runstate/3,
    update_powerstate/3,
    get_device_count/1,
    post_control_device/2
]).

send(DeviceId, Name, FaultRule, #{
    <<"FaultType">> := FaultType,
    <<"FaultLevel">> := FaultLevel,
    <<"FaultCode">> := FaultCode} = FaultData) ->
    maps:fold(fun(UserId, Rule, Acc) ->
        IsFaultType = lists:member(dgiot_utils:to_binary(FaultType), maps:get(<<"FaultType">>, Rule)),
        IsFaultLevel = lists:member(dgiot_utils:to_binary(FaultLevel), maps:get(<<"FaultLevel">>, Rule)),
        IsFaultCode = lists:member(dgiot_utils:to_int(FaultCode), maps:get(<<"FaultCode">>, Rule)),
        case (IsFaultType and IsFaultLevel and IsFaultCode) of
            true ->
                send_fault(DeviceId, Name, UserId, #{
                    <<"FaultType">> => maps:get(<<"FaultType">>, FaultData, 1),
                    <<"FaultLevel">> => maps:get(<<"FaultLevel">>, FaultData, 1),
                    <<"FaultCode">> => maps:get(<<"FaultCode">>, FaultData, 1),
                    <<"FaultSrc">> => maps:get(<<"FaultSrc">>, FaultData, 0)
                }),
                Acc + 1;
            false -> Acc
        end
              end, 0, FaultRule).

test_fault() ->
    UserId = <<"QOGSAQMoX4">>,
    DeviceId = <<"1e82f1c59b">>,
    Name = <<"数蛙002"/utf8>>,
    FaultData = #{
        <<"FaultType">> => 1,
        <<"FaultLevel">> => 1,
        <<"FaultCode">> => 5,
        <<"FaultSrc">> => 0
    },
    send_fault(DeviceId, Name, UserId, FaultData).

get_fault(FaultData) ->
    FaultData#{
        <<"FaultType">> => get_faulttype(maps:get(<<"FaultType">>, FaultData)),
        <<"FaultLevel">> => get_faultlevel(maps:get(<<"FaultLevel">>, FaultData)),
        <<"FaultCode">> => get_faultcode(maps:get(<<"FaultCode">>, FaultData), maps:get(<<"FaultSrc">>, FaultData))
    }.

post_fault() ->
    Rules = dgiot_data:get({sinmahe_json, fault}),
    FaultType =
        maps:fold(fun(K, V, Acc) ->
            Acc ++ [#{<<"id">> => K, <<"text">> => V}]
                  end, [], maps:get(<<"FaultType">>, Rules)),
    FaultLevel =
        maps:fold(fun(K, V, Acc) ->
            Acc ++ [#{<<"id">> => K, <<"text">> => V}]
                  end, [], maps:get(<<"FaultLevel">>, Rules)),
    FaultCode =
        maps:fold(fun(_K, V, Acc) ->
            Acc ++ V
                  end, [], maps:get(<<"FaultCode">>, Rules)),
    #{
        <<"FaultType">> => FaultType,
        <<"FaultLevel">> => FaultLevel,
        <<"FaultCode">> => FaultCode
    }.

%%
%%#{
%%<<"FaultType">> => 1,
%%<<"FaultLevel">> => 1,
%%<<"FaultCode">> => 5,
%%<<"FaultSrc">> => 0
%%}
send_fault(DeviceId, Name, UserId, FaultData) ->
    ?LOG(info, "DeviceId ~p ,Name ~p", [DeviceId, Name]),
    FaultType = get_faulttype(maps:get(<<"FaultType">>, FaultData)),
    FaultLevel = get_faultlevel(maps:get(<<"FaultLevel">>, FaultData)),
    Data = get_faultcode(maps:get(<<"FaultCode">>, FaultData), maps:get(<<"FaultSrc">>, FaultData)),
    Payload = #{
        <<"description">> => <<"设备:"/utf8, Name/binary, " ", FaultType/binary>>,
        <<"title">> => FaultLevel,
        <<"ticker">> => DeviceId,
        <<"text">> => Data
    },
    dgiot_umeng:send(UserId, Payload).

get_faulttype(FaultType) ->
    case dgiot_data:get({sinmahe_json, fault}) of
        #{<<"FaultType">> := Type} ->
            maps:get(dgiot_utils:to_binary(FaultType), Type);
        _ -> <<"正常"/utf8>>
    end.

get_faultlevel(FaultLevel) ->
    case dgiot_data:get({sinmahe_json, fault}) of
        #{<<"FaultLevel">> := Level} ->
            maps:get(dgiot_utils:to_binary(FaultLevel), Level);
        _ -> <<"正常"/utf8>>
    end.

get_faultcode(FaultCode, FaultSrc) ->
    case dgiot_data:get({sinmahe_json, fault}) of
        #{<<"FaultCode">> := Codes} ->
            BinFaultCode = dgiot_utils:to_binary(FaultCode),
            Error = <<"Err", BinFaultCode/binary>>,
            lists:filter(fun(#{<<"code">> := Code}) ->
                case Code of
                    FaultSrc -> true;
                    _ -> false
                end
                         end, maps:get(Error, Codes));
        _ -> <<"正常"/utf8>>
    end.

get_sinmahe_json() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    TplPath = Root ++ "sinmahe.json",
    case file:read_file(TplPath) of
        {ok, Bin} ->
            case catch jsx:decode(Bin, [{labels, binary}, return_maps]) of
                {'EXIT', Reason} ->
                    ?LOG(info, "~p", [Reason]);
                Datas ->
                    dgiot_data:insert({sinmahe_json, fault}, Datas)
            end;
        {error, Reason2} ->
            ?LOG(info, "~p", [Reason2])
    end,
    case dgiot_parse:query_object(<<"Product">>, #{<<"where">> => #{<<"devType">> => <<"sinmahe_PeriodicInformation">>}}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := PeriodProductId} | _]}} ->
            dgiot_data:insert({simahe, <<"PeriodicInformation">>}, PeriodProductId);
        _ ->
            ?LOG(info, "Product not exist")
    end,
    case dgiot_parse:query_object(<<"Product">>, #{<<"where">> => #{<<"devType">> => <<"sinmahe_FaultInformation">>}}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := FaultProductId} | _]}} ->
            dgiot_data:insert({simahe, <<"FaultInformation">>}, FaultProductId);
        _ ->
            ?LOG(info, "Product not exist")
    end,
    case dgiot_parse:query_object(<<"Product">>, #{<<"where">> => #{<<"devType">> => <<"sinmahe_BasicInformation">>}}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := BasicProductId} | _]}} ->
            dgiot_data:insert({simahe, <<"BasicInformation">>}, BasicProductId);
        _ ->
            ?LOG(info, "Product not exist"),
            <<"">>
    end.

create_subdev(ProductId, DevAddr, Type) ->
    SubProductId = dgiot_data:get({simahe, Type}),
    SubDevaddr = <<Type/binary, "_", DevAddr/binary>>,
    case dgiot_parse:get_object(<<"Device">>, dgiot_parse:get_deviceid(SubProductId, SubDevaddr)) of
        {ok, #{<<"objectId">> := _ObjectId}} -> pass;
        _ ->
            Device = #{
                <<"ACL">> => #{
                    <<"*">> => #{
                        <<"read">> => true
                    }
                },
                <<"devaddr">> => SubDevaddr,
                <<"product">> => #{<<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Product">>,
                    <<"objectId">> => SubProductId},
                <<"parentId">> => #{<<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Device">>,
                    <<"objectId">> => dgiot_parse:get_deviceid(ProductId, DevAddr)},
                <<"route">> => #{DevAddr => SubDevaddr}
            },
            dgiot_parse:create_object(<<"Device">>, Device)
    end.

update_runstate(ProductId, DevAddr, Data) ->
    case maps:get(<<"RunState">>, Data, undefine) of
        undefine -> pass;
        RunState ->
            case dgiot_data:get({sinmahe_runstate, DevAddr}) of
                RunState -> pass;
                _ -> case dgiot_parse:get_object(<<"Device">>, dgiot_parse:get_deviceid(ProductId, DevAddr)) of
                         {ok, #{<<"objectId">> := ObjectId, <<"basedata">> := BaseData}} ->
                             dgiot_parse:update_object(<<"Device">>, ObjectId, #{<<"isEnable">> => false,
                                 <<"basedata">> => BaseData#{<<"RunState">> => RunState}});
                         _ ->
                             pass
                     end
            end
    end.

update_powerstate(ProductId, DevAddr, Data) ->
    case maps:get(<<"PowerState">>, Data, undefine) of
        undefine -> pass;
        RunState ->
            case dgiot_data:get({sinmahe_runstate, DevAddr}) of
                RunState -> pass;
                _ -> case dgiot_parse:get_object(<<"Device">>, dgiot_parse:get_deviceid(ProductId, DevAddr)) of
                         {ok, #{<<"objectId">> := ObjectId, <<"basedata">> := BaseData}} ->
                             dgiot_parse:update_object(<<"Device">>, ObjectId, #{<<"isEnable">> => false,
                                 <<"basedata">> => BaseData#{<<"PowerState">> => RunState}});
                         _ ->
                             pass
                     end
            end
    end.


% dgiot_sinmahe_oper:get_device_count().
get_device_count(SessionToken) ->
    ProductId = dgiot_data:get({simahe, <<"PeriodicInformation">>}),
    Filter = #{
        <<"keys">> => [<<"count(*)">>, <<"devaddr">>, <<"basedata">>, <<"status">>],
        <<"where">> => #{<<"product">> => ProductId}
    },
    case dgiot_parse:query_object(<<"Device">>, Filter, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"count">> := Count, <<"results">> := Results}} ->
            {Loc2, Warn2} =
                lists:foldl(fun(#{<<"objectId">> := DeviceId, <<"devaddr">> := Addr, <<"basedata">> := Basedata}, {Loc, Warn}) ->
                    Factory =
                        case maps:find(<<"factory">>, Basedata) of
                            {ok, Factory1} -> Factory1;
                            error -> <<"新马赫"/utf8>>
                        end,
                    Loc1 =
                        case maps:find(<<"basicdata">>, Basedata) of
                            {ok, BasicData} ->
                                case maps:is_key(<<"GPS">>, BasicData) of
                                    true ->
                                        #{<<"GPS">> := #{<<"Lon">> := Lon, <<"Lat">> := Lat}} = BasicData,
                                        [#{<<"Lon">> => Lon, <<"Lat">> => Lat,
                                            <<"factory">> => Factory, <<"addr">> => Addr, <<"did">> => DeviceId} | Loc];
                                    false ->
                                        [#{<<"factory">> => Factory, <<"addr">> => Addr, <<"did">> => DeviceId} | Loc]
                                end;
                            error ->
                                [#{<<"factory">> => Factory, <<"addr">> => Addr, <<"did">> => DeviceId} | Loc]
                        end,
                    Warn1 =
                        case maps:find(<<"faultdata">>, Basedata) of
                            {ok, #{<<"FaultLevel">> := Level}} ->
                                [
                                    #{
                                        <<"FaultLevel">> => dgiot_utils:to_int(Level),
                                        <<"factory">> => Factory,
                                        <<"addr">> => Addr, <<"did">> => DeviceId} | Warn];
                            _ ->
                                [
                                    #{
                                        <<"FaultLevel">> => 0,
                                        <<"factory">> => Factory,
                                        <<"addr">> => Addr, <<"did">> => DeviceId} | Warn]
                        end,
                    {Loc1, Warn1}
                            end, {[], []}, Results),
            Query = #{
                <<"keys">> => [<<"last_row(RunState,createdat)">>],
                <<"limit">> => 1,
                <<"group">> => <<"devaddr">>,
                <<"where">> => #{
                    <<"createdat">> => #{
                        <<"$gt">> => <<"now - 30s">>
                    }
                }
            },
            {NewOnline, NewRunning, NewSuspend} =
                case dgiot_tdengine:get_product(ProductId, Query) of
                    {ok, #{<<"results">> := Result}} when length(Result) > 0 ->
                        lists:foldl(fun(#{<<"runstate">> := State, <<"devaddr">> := DevAddr, <<"createdat">> := Ts}, {Online, Running, Suspend}) ->
                            case DevAddr of
                                <<"_PeriodicInformation">> ->
                                    {Online, Running, Suspend};
                                _ ->
                                    case (dgiot_datetime:now_secs() - dgiot_tdengine:to_unixtime(Ts) < 500) of
                                        true ->
                                            case State of
                                                0 ->
                                                    {Online + 1, Running, Suspend + 1};
                                                1 ->
                                                    {Online + 1, Running + 1, Suspend};
                                                2 ->
                                                    {Online + 1, Running + 1, Suspend};
                                                _ ->
                                                    {Online, Running, Suspend}
                                            end;
                                        false ->
                                            {Online, Running, Suspend}
                                    end
                            end
                                    end, {0, 0, 0}, Result);
                    _Result ->
                        {0, 0, 0}
                end,
            {200, #{
                <<"deviceInfo">> =>
                #{
                    <<"productid">> => ProductId,
                    <<"total">> => Count,
                    <<"online">> => NewOnline,
                    <<"running">> => NewRunning,
                    <<"suspend">> => NewSuspend},
                <<"devicedata">> => Loc2,
                <<"warnings">> => Warn2}
            };
        _ ->
            {error, <<"devaddr not exist">>}
    end.


% dgiot_sinmahe_oper:post_control_device(Args).
post_control_device(#{<<"type">> := Type, <<"devaddr">> := DevAddr, <<"payload">> := Payload}, SessionToken) ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"devaddr">> => DevAddr}},
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Result}} when length(Result) > 0 ->
            Topic = <<Type/binary, "/", DevAddr/binary>>,
            dgiot_mqtt:publish(DevAddr, Topic, jsx:encode(Payload)),
            {ok, #{<<"results">> => Result}};
        Error ->
            Error
    end.

