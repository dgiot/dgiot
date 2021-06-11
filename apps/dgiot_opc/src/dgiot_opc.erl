%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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
-module(dgiot_opc).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").

-export([
    create_konva/2,
    scan_opc/1,
    read_opc/4,
    process_opc/2
]).


%% 下发扫描OPC命令
%% topic: dgiot_opc_da
%% payload：
%%{
%%"cmdtype":"scan",
%%"opcserver":"Kepware.KEPServerEX.V6"
%%}
scan_opc(#{<<"OPCSEVER">> := OpcServer}) ->
    Payload = #{
        <<"cmdtype">> => <<"scan">>,
        <<"opcserver">> => OpcServer
    },
    dgiot_mqtt:publish(<<"opcserver">>, <<"dgiot_opc_da">>, jsx:encode(Payload)).

read_opc(ChannelId, OpcServer, DevAddr, Instruct) ->
%%    [DevAddr | _] = maps:keys(Instruct),
%%    Values = maps:get(DevAddr, Instruct, #{}),
%%    Items =
%%        maps:fold(fun(K, _V, Acc) ->
%%            case Acc of
%%                <<"">> -> K;
%%                _ -> <<Acc/binary, ",", K/binary>>
%%            end
%%                  end, <<"">>, Values),
    Payload = #{
        <<"cmdtype">> => <<"read">>,
        <<"opcserver">> => OpcServer,
        <<"group">> => DevAddr,
        <<"items">> => Instruct
    },
    dgiot_bridge:send_log(ChannelId, "to_opc: ~p: ~p  ~ts ", [OpcServer, DevAddr, unicode:characters_to_list(Instruct)]),
    dgiot_mqtt:publish(<<"opcserver">>, <<"dgiot_opc_da">>, jsx:encode(Payload)).

process_opc(ChannelId, Payload) ->
    [DevAddr | _] = maps:keys(Payload),
    Items = maps:get(DevAddr, Payload, #{}),
    case dgiot_data:get({dgiot_opc, DevAddr}) of
        not_find ->
            pass;
        ProductId ->
            NewTopic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/post">>,
            dgiot_bridge:send_log(ChannelId, "to_task: ~ts", [unicode:characters_to_list(jsx:encode(Items))]),
            dgiot_mqtt:publish(DevAddr, NewTopic, jsx:encode(Items))
%%        _ -> pass
    end.

create_konva(ProductId, Map) ->
    {Count, Shape} =
        maps:fold(fun(Type, Items, {Index, Acc}) ->
            IsSystem = lists:any(fun(E) ->
                lists:member(E, [<<"_System">>, <<"_Statistics">>, <<"_ThingWorx">>, <<"_DataLogger">>])
                                 end, binary:split(Type, <<$.>>, [global, trim])),
            case IsSystem of
                true -> {Index, Acc};
                _ ->
                    lists:foldl(fun(Item, {Index1, Acc1}) ->
                        case Item of
                            #{<<"ItemId">> := ItemId, <<"Name">> := Name} ->
                                {Index1 + 1, Acc1 ++ [ #{
                                    <<"type">> => <<"text">>,
                                    <<"x">> => 100,
                                    <<"y">> => 130 + Index1 * 30,
                                    <<"id">> => Name,
                                    <<"fill">> => <<"#e579f2">>,
                                    <<"text">> => ItemId,
                                    <<"fontSize">> => 20,
                                    <<"fontFamily">> => <<"Calibri">>}]
                                };
                            _ ->
                                {Index1, Acc1}
                        end
                                end, {Index, Acc}, Items)
            end
                  end, {0, []}, Map),
    case Count of
        0 ->
            pass;
        _ ->
            Konva = #{
                <<"Stage">> => #{
                    <<"id">> => <<"stage_", ProductId/binary>>
                },
                <<"Layer">> => #{
                    <<"id">> => <<"layer_", ProductId/binary>>,
                    <<"fill">> => <<"#e579f2">>,
                    <<"x">> => 200,
                    <<"y">> => 30,
                    <<"text">> => <<"OPCDA点位信息">>,
                    <<"fontSize">> => 40,
                    <<"fontFamily">> => <<"Calibri">>
                },
                <<"Group">> => #{
                    <<"id">> => <<"group_", ProductId/binary>>
                }
            },
            case dgiot_parse:get_object(<<"Product">>, ProductId) of
                {ok, #{<<"config">> := _Config}} ->
                    pass;
                _ ->
                    dgiot_parse:update_object(<<"Product">>, ProductId,
                        #{<<"config">> => #{<<"konva">> => Konva#{<<"Shape">> => Shape}}})
            end
    end.
