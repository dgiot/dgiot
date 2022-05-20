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
    scan_opc/1,
    read_opc/4,
    process_opc/2,
    read_opc_ack/3,
    scan_opc_ack/5,
    create_changelist/1,
    create_final_Properties/1,
    create_x_y/1,
    change_config/1,
    create_config/1,
    send_properties/3
]).


%% 下发扫描OPC命令
%% topic: dgiot_opc_da
%% payload：
%%{
%%"cmdtype":"scan",
%%"opcserver":"Kepware.KEPServerEX.V6"
%%}
scan_opc(#{<<"OPCSEVER">> := OpcServer, <<"Topic">> := Topic}) ->
    Payload = #{
        <<"cmdtype">> => <<"scan">>,
        <<"opcserver">> => OpcServer
    },
    dgiot_mqtt:publish(<<"opcserver">>, Topic, jsx:encode(Payload)).

read_opc(ChannelId, OpcServer, Topic, Instruct) ->
    Payload = #{
        <<"cmdtype">> => <<"read">>,
        <<"opcserver">> => OpcServer,
        <<"items">> => Instruct
    },
    dgiot_bridge:send_log(ChannelId, "to_opc: ~p: ~p  ~ts ", [OpcServer, unicode:characters_to_list(Instruct)]),
    dgiot_mqtt:publish(<<"opcserver">>, Topic, jsx:encode(Payload)).

scan_opc_ack(Payload, OpcServer, Topic, Group, ProductId) ->            %%---------- 用以创建组态、物模型。
    Map = jsx:decode(Payload, [return_maps]),
    {ok, #{<<"name">> := ProductName}} = dgiot_parse:get_object(<<"Product">>, ProductId),
    Instruct = maps:fold(fun(K, V, Acc) ->
        IsSystem = lists:any(fun(E) ->
            lists:member(E, [<<"_System">>, <<"_Statistics">>, <<"_ThingWorx">>, <<"_DataLogger">>])
                             end, binary:split(K, <<$.>>, [global, trim])),
        case IsSystem of
            true ->
                Acc;
            false ->
                lists:foldl(fun(X, Acc1) ->
                    case X of
                        #{<<"ItemId">> := ItemId} ->
                            case binary:split(ItemId, <<$.>>, [global, trim]) of
                                [Project, _Device, Id] ->
                                    case Project == ProductName of
                                        true ->
                                            case binary:split(Id, <<$_>>, [global, trim]) of
                                                [Id] ->
                                                    get_instruct(Acc1, ItemId);
                                                _ ->
                                                    Acc1
                                            end;
                                        _ ->
                                            Acc1
                                    end;

                                [Project, _Device, _Id, Type] ->
                                    case Project == ProductName of
                                        true ->
                                            case lists:member(Type, [<<"_Description">>, <<"_RawDataType">>]) of
                                                true ->
                                                    get_instruct(Acc1, ItemId);
                                                false ->
                                                    Acc1
                                            end;
                                        _ ->
                                            Acc1
                                    end;
                                _ -> Acc1
                            end;
                        _ ->
                            Acc1
                    end
                            end, Acc, V)
        end
                         end, <<"">>, maps:without([<<"status">>], Map)),
    Payload1 = #{
        <<"cmdtype">> => <<"read">>,
        <<"opcserver">> => OpcServer,
        <<"group">> => Group,
        <<"items">> => Instruct
    },

    dgiot_mqtt:publish(<<"opcserver">>, Topic, jsx:encode(Payload1)).


get_instruct(Acc1, ItemId) ->
    case Acc1 of
        <<"">> ->
            ItemId;
        _ ->
            <<Acc1/binary, ",", ItemId/binary>>
    end.

read_opc_ack(Payload, ProductId, {DeviceId, Devaddr}) ->
    case jsx:decode(Payload, [return_maps]) of
        #{<<"status">> := 0} = Map0 -> %% opc read的情况
            [Map1 | _] = maps:values(maps:without([<<"status">>], Map0)),
            Map2 = case maps:find(<<"status">>, Map1) of
                       {ok, _} ->
                           [Map3 | _] = maps:values(maps:without([<<"status">>], Map1)),
                           Map3;
                       error ->
                           Map1

                   end,

            %%  -------------------------------- 组态数据传递
            Data = maps:fold(fun(K, V, Acc) ->
                case binary:split(K, <<$.>>, [global, trim]) of
                    [_, DeviceName1, K1] ->
                        case Devaddr == DeviceName1 of
                            true ->
                                Name =
                                    case dgiot_product:lookup_prod(ProductId) of
                                        {ok, #{<<"thing">> := #{<<"properties">> := Properties}}}
                                            ->
                                            ALL_list = [{maps:get(<<"identifier">>, H), maps:get(<<"name">>, H)} || H <- Properties],
                                            proplists:get_value(K1, ALL_list);
                                        _ ->
                                            <<" ">>
                                    end,
                                Unit =
                                    case dgiot_product:lookup_prod(ProductId) of
                                        {ok, #{<<"thing">> := #{<<"properties">> := Properties1}}}
                                            ->
                                            ALL_list1 = [{maps:get(<<"identifier">>, H), maps:get(<<"dataType">>, H)} || H <- Properties1],
                                            Map_datatype = proplists:get_value(K1, ALL_list1),
                                            Specs = maps:get(<<"specs">>, Map_datatype),
                                            maps:get(<<"unit">>, Specs);
                                        _ ->
                                            <<" ">>
                                    end,
                                V1 = binary:bin_to_list(Name),
                                V2 = dgiot_utils:to_list(V),
                                V1_unit = dgiot_utils:to_list(Unit),
                                V3 = V1 ++ ": " ++ V2 ++ " " ++ V1_unit,
                                Acc#{K => V3};
                            _ ->
                                Acc
                        end;
                    _ -> Acc
                end
                             end, #{}, Map2),
            Data2 = maps:fold(fun(K, V, Acc) ->
                case binary:split(K, <<$.>>, [global, trim]) of
                    [_, DeviceName2, K1] ->
                        case Devaddr == DeviceName2 of
                            true ->
                                Acc#{K1 => V};
                            _ ->
                                Acc
                        end;
                    _ -> Acc
                end
                              end, #{}, Map2),
            try dgiot_topo:push(ProductId, Devaddr, DeviceId, Data)
            catch _:_ ->
                ?LOG(info, "{ TOPO PUSH ERROR},dgiot_topo:push(~p, ~p, ~p, ~p)", [ProductId, Devaddr, DeviceId, Data])
            after
                pass
            end,
            %% --------------------------------  数据存TD库
            try dgiot_tdengine_adapter:save(ProductId, Devaddr, Data2)
            catch _:_ ->
                ?LOG(info, "{ TD SAVE ERROR },dgiot_tdengine_adapter:save(~p, ~p, ~p)", [ProductId, Devaddr, Data2])
            after
                pass
            end,
            %%  -------------------------------- 设备上线状态修改
            case dgiot_data:get({dev, status, DeviceId}) of
                not_find ->
                    dgiot_data:insert({dev, status, DeviceId}, self()),
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"status">> => <<"ONLINE">>});
                _ -> pass

            end;

        _ ->
            pass
    end.

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


%%scan后创建物模型
create_Properties({Item, RawDataType, Description}) ->
    DataType =
        case RawDataType of
            <<"Boolean">> ->
                <<"bool">>;
            <<"Char">> ->
                <<"string">>;
            <<"Byte">> ->
                <<"string">>;
            <<"Short">> ->
                <<"int">>;
            <<"Word">> ->
                <<"string">>;
            <<"Long">> ->
                <<"int">>;
            <<"DWord">> ->
                <<"string">>;
            <<"LLong">> ->
                <<"string">>;
            <<"QWord">> ->
                <<"string">>;
            <<"Float">> ->
                <<"float">>;
            <<"Double">> ->
                <<"double">>;
            <<"String">> ->
                <<"string">>;
            <<"Date">> ->
                <<"date">>;
            _ ->
                <<"string">>
        end,
    #{<<"accessMode">> => <<"r">>,
        <<"dataForm">> =>
        #{<<"address">> => <<"null">>,
            <<"byteorder">> => <<"big">>,
            <<"collection">> => <<"%s">>,
            <<"control">> => <<"%d">>, <<"data">> => <<"null">>,
            <<"offset">> => 0, <<"protocol">> => <<"normal">>,
            <<"quantity">> => <<"null">>, <<"rate">> => 1,
            <<"strategy">> => <<"20">>},
        <<"dataType">> =>
        #{<<"specs">> =>
        #{<<"max">> => 1000, <<"min">> => -1000,
            <<"step">> => 0.01, <<"unit">> => <<" ">>},
            <<"type">> => DataType},
        <<"identifier">> => Item,
        <<"name">> => Description,
        <<"required">> => true}.



create_final_Properties(List) -> [create_Properties(X) || X <- List].




%%%创建组态config
create_config(List) ->
    #{<<"konva">> =>
    #{<<"Stage">> =>
    #{<<"attrs">> =>
    #{<<"draggable">> => true, <<"height">> => 469,
        <<"id">> => <<"container">>, <<"width">> => 1868,
        <<"x">> => 14, <<"y">> => 29},
        <<"children">> =>
        [#{<<"attrs">> =>
        #{<<"id">> => <<"Layer_sBE2t0">>},
            <<"children">> =>
            [#{<<"attrs">> =>
            #{<<"height">> => 2000,
                <<"id">> => <<"Group_9H6kPPA">>,
                <<"width">> => 2000},
                <<"children">> => List,              %%%组态按钮标签
                <<"className">> => <<"Group">>}],
            <<"className">> => <<"Layer">>}],
        <<"className">> => <<"Stage">>}}}.


%%创建组态按钮标签List->{text}





create_lable({{Item, _, Description}, {X, Y}}) ->
    #{<<"attrs">> =>
    #{
        <<"draggable">> => true,
        <<"fill">> => <<"#000000">>,
        <<"fontFamily">> => <<"Calibri">>,
        <<"fontSize">> => 20,
        <<"id">> => Item,
        <<"text">> => Description, %% 太阳能板电压
        <<"type">> => <<"text">>,
        <<"x">> => X,
        <<"y">> => Y},
        <<"className">> => <<"Text">>}.



change_config(List) ->
    [create_lable(X) || X <- List].

create_x_y(Num) when Num > 0 -> %% 根据属性个数生成合理的（x,y)坐标
    [{((Num - 1) div 5) * 300 + 100, 50 + 150 * ((Num - 1) rem 5)} | create_x_y(Num - 1)];
create_x_y(0) -> [].



create_changelist(List_Data) ->
    Item = [{K, V} || {K, V} <- List_Data, jud1(K)],
    RawDataType = [{K, V} || {K, V} <- List_Data, jud2(K)],
    Description = [{K, V} || {K, V} <- List_Data, jud3(K)],
%%    Scan_instruct = [{K,V}||{K,V} <- List_Data,jud4(K)],
%%    ?LOG(info,"Scan_instruct:~p",[RawDataType]),
    List = [{Item1, RawDataType1, Description1} || {K1, Item1} <- Item, {K2, RawDataType1} <- RawDataType, {K3, Description1} <- Description, jud(K1, K2, K3)],
    List1 = lists:usort(List),
    List1.

jud1(X) ->
    case binary:split(X, <<$_>>, [global, trim]) of
        [X] ->
            true;
        _ ->
            false
    end.

jud2(X) ->
    case binary:split(X, <<$_>>, [global, trim]) of
        [_, <<"RawDataType">>] ->
            true;
        _ ->
            false
    end.

jud3(X) ->
    case binary:split(X, <<$_>>, [global, trim]) of
        [_, <<"Description">>] ->
            true;
        _ ->
            false
    end.

%%jud4(X) ->
%%    case binary:split(X, <<$.>>, [global, trim])  of
%%        [_,_,_] ->
%%            true;
%%        _ ->
%%            false
%%    end.

jud(K1, K2, K3) ->
    [Key2, _] = binary:split(K2, <<$_>>, [global, trim]),
    [Key3, _] = binary:split(K3, <<$_>>, [global, trim]),
%%    [_,_,Key4]= binary:split(K4, <<$.>>, [global, trim]),
%%    ?LOG(info,"------------Key:~p",[Key4]),
    case Key2 == Key3 of
        true ->
            case K1 == Key2 of
                true ->
                    true;
                false ->
                    false
            end;
        false ->
            false
    end.


send_properties(ProductId, DtuAddr, Properties) ->
    NewProperties = get_properties(ProductId, Properties),
    NewTopic = <<"thing/", ProductId/binary, "/", DtuAddr/binary, "/post">>,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
%%    io:format("~s ~p NewProperties = ~p.~n", [?FILE, ?LINE, NewProperties]),
    dgiot_mqtt:publish(DeviceId, NewTopic, jsx:encode(NewProperties)).

get_properties(ProductId, Properties) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case X of
                    #{<<"identifier">> := Identifier,
                        <<"dataType">> := DataType} ->
                        Das = maps:get(<<"das">>, DataType, []),
                        maps:fold(fun(PK, PV, Acc1) ->
                            case lists:member(PK, Das) of
                                true ->
                                    Acc1#{Identifier => PV};
                                _ ->
                                    Acc1#{PK => PV}
                            end
                                  end, Acc, Properties);
                    _ ->
                        Acc
                end
                        end, #{}, Props);
        _Error ->
            Properties
    end.
