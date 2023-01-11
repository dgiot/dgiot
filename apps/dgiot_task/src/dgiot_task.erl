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

-module(dgiot_task).
-include("dgiot_task.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").

-export([start/1, start/2, send/3, get_pnque_len/1, save_pnque/4, get_pnque/1, del_pnque/1, save_td/4, merge_cache_data/3, save_cache_data/2]).
-export([get_control/3, get_collection/3, get_calculated/2, get_instruct/2, get_storage/2, string2value/2, string2value/3]).
-export([save_td_no_match/4]).
start(ChannelId) ->
    lists:map(fun(Y) ->
        case Y of
            {ClientId, _} ->
                dgiot_client:start(ChannelId, ClientId);
            _ ->
                pass
        end
              end, ets:tab2list(?DGIOT_PNQUE)).

start(ChannelId, Products) when is_list(Products) ->
    lists:map(fun({ProductId, _}) ->
        Success = fun(Page) ->
            lists:map(fun(#{<<"objectId">> := DeviceId}) ->
                dgiot_client:start(ChannelId, DeviceId)
                      end, Page)
                  end,
        Query = #{
            <<"order">> => <<"updatedAt">>,
            <<"keys">> => [<<"objectId">>],
            <<"where">> => #{<<"product">> => ProductId}
        },
        dgiot_parse_loader:start(<<"Device">>, Query, 0, 100, 1000000, Success)
              end, Products);

start(ChannelId, ClientId) ->
    dgiot_client:start(ChannelId, ClientId).

send(ProductId, DevAddr, Payload) ->
    case dgiot_data:get({?TYPE, ProductId}) of
        not_find ->
            pass;
        ChannelId ->
            Topic = <<"$dg/thing/", ProductId/binary, "/", DevAddr/binary, "/properties/report">>,
            dgiot_client:send(ChannelId, DevAddr, Topic, Payload)
    end.

%%获取计算值，必须返回物模型里面的数据表示，不能用寄存器地址
get_calculated(ProductId, Calculated) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case Acc of
                    error ->
                        Acc;
                    _ ->
                        case X of
                            #{<<"isstorage">> := true,
                                <<"identifier">> := Identifier, <<"dataForm">> := #{
                                <<"strategy">> := <<"计算值"/utf8>>, <<"collection">> := Collection},
                                <<"dataType">> := #{<<"type">> := Type, <<"specs">> := Specs}} ->
                                Str1 = maps:fold(fun(K, V, Acc2) ->
                                    Str = re:replace(Acc2, dgiot_utils:to_list(<<"%%{", K/binary, "}">>), dgiot_utils:to_list(V), [global, {return, list}]),
                                    re:replace(Str, "%{s}", dgiot_utils:to_list(V), [global, {return, list}])
                                                 end, dgiot_utils:to_list(Collection), Calculated),
                                case string2value(Str1, Type, Specs) of
                                    error ->
                                        maps:without([Identifier], Acc);
                                    Value1 ->
                                        Acc#{Identifier => Value1}
                                end;
                            _ ->
                                Acc
                        end
                end
                        end, Calculated, Props);
        _Error ->
            Calculated
    end.

%% 主动上报 dis为[]
get_collection(ProductId, [], Payload) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc2) ->
                case Acc2 of
                    error ->
                        Acc2;
                    _ ->
                        case X of
                            #{<<"dataForm">> := #{<<"strategy">> := Strategy} = DataForm,
                                <<"dataType">> := DataType,
                                <<"identifier">> := Identifier} when Strategy =/= <<"计算值"/utf8>> ->
                                dgiot_task_data:get_userdata(ProductId, Identifier, DataForm, DataType, Payload, Acc2);
                            _ ->
                                Acc2
                        end
                end
                        end, Payload, Props);
        _Error ->
            Payload
    end;

%%转换设备上报值，必须返回物模型里面的数据表示，不能用寄存器地址
get_collection(ProductId, Dis, Payload) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(Identifier, Acc1) ->
                lists:foldl(fun(X, Acc2) ->
                    case Acc2 of
                        error ->
                            Acc2;
                        _ ->
                            case X of
                                #{<<"dataForm">> := #{<<"strategy">> := Strategy} = DataForm,
                                    <<"dataType">> := DataType,
                                    <<"identifier">> := Identifier} when Strategy =/= <<"计算值"/utf8>> ->
                                    dgiot_task_data:get_userdata(ProductId, Identifier, DataForm, DataType, Payload, Acc2);
                                _ ->
                                    Acc2
                            end
                    end
                            end, Acc1, Props)
                        end, Payload, Dis);
        _Error ->
            Payload
    end.

%% 获取控制值
get_control(Round, Data, Control) ->
    case Data of
        <<"null">> ->
            <<"null">>;
        Data ->
            Str = re:replace(dgiot_utils:to_list(Control), "%{d}", dgiot_utils:to_list(Data), [global, {return, list}]),
            Str1 = re:replace(Str, "%{r}", dgiot_utils:to_list(Round), [global, {return, list}]),
            dgiot_task:string2value(Str1, <<"type">>)
    end.

%%获取存储值
get_storage(ProductId, Calculated) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(X, Acc) ->
                case Acc of
                    error ->
                        Acc;
                    _ ->
                        case X of
                            #{<<"isstorage">> := true, <<"identifier">> := Identifier} ->
                                case maps:find(Identifier, Calculated) of
                                    {ok, Value} ->
                                        Acc#{Identifier => Value};
                                    _ ->
                                        Acc
                                end;
                            _ ->
                                Acc
                        end
                end
                        end, #{}, Props);
        _Error ->
            Calculated
    end.

get_instruct(ProductId, Round) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} when length(Props) > 0 ->
            {_, NewList} = lists:foldl(fun(X, Acc) ->
                {Seq, List} = Acc,
                case X of
                    #{<<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>}} ->    %% 计算值加入采集指令队列
                        Acc;
                    #{<<"dataForm">> := #{<<"strategy">> := <<"主动上报"/utf8>>}} ->  %% 主动上报值加入采集指令队列
                        Acc;
                    #{<<"accessMode">> := AccessMode,
                        <<"identifier">> := Identifier,
                        <<"dataType">> := #{<<"specs">> := Specs},
                        <<"dataForm">> := DataForm,
                        <<"dataSource">> := DataSource} ->
                        Min = maps:get(<<"min">>, Specs, 0),
                        Protocol = maps:get(<<"protocol">>, DataForm, <<"Dlink">>),
                        Control = maps:get(<<"control">>, DataForm, "%{d}"),                       %% 控制参数
                        Data = dgiot_task:get_control(Round, Min, Control),                      %% 控制参数的初始值，可以根据轮次进行计算
                        NewDataSource = dgiot_task_data:get_datasource(Protocol, AccessMode, Data, DataSource),    %% 根据协议类型生成采集数据格式
                        Order = maps:get(<<"order">>, DataForm, Seq),                            %% 指令顺序
                        Interval = dgiot_utils:to_int(maps:get(<<"strategy">>, DataForm, 20)),   %% 下一个指令的采集间隔
                        ThingRound = maps:get(<<"round">>, DataForm, <<"all">>),                 %% 物模型中的指令轮次规则
                        BinRound = dgiot_utils:to_binary(Round),                                 %% 判断本轮是否需要加入采集指令队列
                        case ThingRound of
                            <<"all">> ->  %% 所有轮次
                                {Seq + 1, List ++ [{Order, Interval, Identifier, NewDataSource}]};
                            BinRound ->
                                {Seq + 1, List ++ [{Order, Interval, Identifier, NewDataSource}]};
                            Rounds ->
                                RoundList = binary:split(Rounds, <<",">>, [global]),
                                case lists:member(BinRound, RoundList) of
                                    true ->
                                        {Seq + 1, List ++ [{Order, Interval, Identifier, NewDataSource}]};
                                    false ->
                                        Acc
                                end
                        end;
                    _ ->
                        Acc
                end
                                       end, {1, []}, Props),
            lists:keysort(1, NewList);
        _ ->
            []
    end.

string2value(Str, <<"TEXT">>) when is_list(Str) ->
    %% eralng语法中. 表示事务结束
    case string:find(Str, "%%") of
        nomatch ->
            Str;
        _ -> error
    end;

string2value(Str, _) ->
    %% eralng语法中. 表示事务结束
    case string:find(Str, "%%") of
        nomatch ->
            {ok, Tokens, _} = erl_scan:string(Str ++ "."),
            case erl_parse:parse_exprs(Tokens) of
                {error, _} ->
                    error;
                {ok, Exprs} ->
                    Bindings = erl_eval:new_bindings(),
                    case catch erl_eval:exprs(Exprs, Bindings) of
                        {value, Value, _} ->
                            Value;
                        _ ->
                            0
                    end
            end;
        _ -> error
    end.

string2value(Str, Type, Specs) ->
    Type1 = list_to_binary(string:to_upper(binary_to_list(Type))),
    case string2value(Str, Type1) of
        error ->
            error;
        Value ->
            case Type1 of
                <<"INT">> ->
                    round(Value);
                Type2 when Type2 == <<"FLOAT">>; Type2 == <<"DOUBLE">> ->
                    Precision = maps:get(<<"precision">>, Specs, 3),
                    dgiot_utils:to_float(Value, Precision);
                _ ->
                    Value
            end
    end.

save_pnque(DtuProductId, DtuAddr, ProductId, DevAddr) ->
    DtuId = dgiot_parse_id:get_deviceid(DtuProductId, DtuAddr),
    Topic = <<"$dg/device/", ProductId/binary, "/", DevAddr/binary, "/properties">>,
    dgiot_mqtt:subscribe(Topic),
    case dgiot_data:get(?DGIOT_PNQUE, DtuId) of
        not_find ->
            dgiot_data:insert(?DGIOT_PNQUE, DtuId, [{ProductId, DevAddr}]);
        Pn_que ->
            New_Pn_que = dgiot_utils:unique_2(Pn_que ++ [{ProductId, DevAddr}]),
            dgiot_data:insert(?DGIOT_PNQUE, DtuId, New_Pn_que)
    end,
    case dgiot_data:get({task_args, DtuProductId}) of
        not_find ->
            pass;
        #{<<"channel">> := Channel} = Args ->
%%            io:format("Args ~p.~n", [Args]),
            supervisor:start_child(?TASK_SUP(Channel), [Args#{<<"dtuid">> => DtuId}])
    end.

get_pnque_len(DtuId) ->
    case dgiot_data:get(?DGIOT_PNQUE, DtuId) of
        not_find ->
            0;
        PnQue ->
            length(PnQue)
    end.

get_pnque(DtuId) ->
    case dgiot_data:get(?DGIOT_PNQUE, DtuId) of
        not_find ->
            not_find;
        PnQue when length(PnQue) > 0 ->
            Head = lists:nth(1, PnQue),
            dgiot_data:insert(?DGIOT_PNQUE, DtuId, lists:nthtail(1, PnQue) ++ [Head]),
            Head;
        _ ->
            not_find
    end.

%% INSERT INTO _b8b630322d._4ad9ab0830 using _b8b630322d._b8b630322d TAGS ('_862607057395777') VALUES  (now,638,67,2.1,0.11,0,27,38,0.3,0.0,0.0,11.4,0);
del_pnque(DtuId) ->
    case dgiot_data:get(?DGIOT_PNQUE, DtuId) of
        not_find ->
            pass;
        PnQue when length(PnQue) > 0 ->
            dgiot_data:delete(?DGIOT_PNQUE, DtuId);
        _ ->
            pass
    end.

save_td(ProductId, DevAddr, Ack, _AppData) ->
    case length(maps:to_list(Ack)) of
        0 ->
            #{};
        _ ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            Interval = dgiot_product:get_interval(ProductId),
            %%            是否有缓存
            CacheData = merge_cache_data(DeviceId, Ack, Interval),
            %%            计算上报值
            Collection = dgiot_task:get_collection(ProductId, [], CacheData),
            %%            计算计算值
            AllData = dgiot_task:get_calculated(ProductId, Collection),
            %%            过滤存储值
            Storage = dgiot_task:get_storage(ProductId, AllData),
            case Interval of
                0 ->
                    save_cache_data(DeviceId, CacheData),
                    dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage);
                Interval ->
                    Keys = dgiot_product:get_keys(ProductId),
                    AllStorageKey = maps:keys(Storage),
                    case Keys -- AllStorageKey of
                        List when length(List) == 0 andalso length(AllStorageKey) =/= 0 ->
                            dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage);
                        _ ->
                            save_cache_data(DeviceId, CacheData),
                            AllData
                    end
            end
    end.

%% 处理数据
dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage) ->
    %%                    告警
    NotificationTopic = <<"$dg/user/alarm/", ProductId/binary, "/", DeviceId/binary, "/properties/report">>,
    dgiot_mqtt:publish(DeviceId, NotificationTopic, jsx:encode(AllData)),
    %%                    实时数据
    ChannelId = dgiot_parse_id:get_channelid(dgiot_utils:to_binary(?BRIDGE_CHL), <<"DGIOTTOPO">>, <<"TOPO组态通道"/utf8>>),
    dgiot_channelx:do_message(ChannelId, {topo_thing, ProductId, DeviceId, AllData}),
    %%                    save td
    dgiot_tdengine_adapter:save(ProductId, DevAddr, Storage),
    Channel = dgiot_product_channel:get_taskchannel(ProductId),
    dgiot_bridge:send_log(Channel, ProductId, DevAddr, "~s ~p save td => ProductId ~p DevAddr ~p ~ts ", [?FILE, ?LINE, ProductId, DevAddr, unicode:characters_to_list(jsx:encode(Storage))]),
    dgiot_metrics:inc(dgiot_task, <<"task_save">>, 1),
    AllData.

save_cache_data(DeviceId, Data) ->
    NewData = maps:fold(fun(K, V, Acc) ->
        AtomKey = dgiot_utils:to_atom(K),
        Acc#{AtomKey => V}
                        end, #{}, Data),
    dgiot_data:insert(?DGIOT_DATA_CACHE, DeviceId, {NewData, dgiot_datetime:now_secs()}).

merge_cache_data(DeviceId, NewData, 0) ->
    case dgiot_data:get(?DGIOT_DATA_CACHE, DeviceId) of
        not_find ->
            NewData;
        {OldData, _Ts} ->
            NewOldData =
                maps:fold(fun(K, V, Acc) ->
                    Key = dgiot_utils:to_binary(K),
                    Acc#{Key => V}
                          end, #{}, OldData),
            dgiot_map:merge(NewOldData, NewData)
    end;

merge_cache_data(DeviceId, NewData, Interval) ->
    case dgiot_data:get(?DGIOT_DATA_CACHE, DeviceId) of
        not_find ->
            NewData;
        {OldData, Ts} ->
            case dgiot_datetime:now_secs() - Ts < Interval of
                true ->
                    NewOldData =
                        maps:fold(fun(K, V, Acc) ->
                            Key = dgiot_utils:to_binary(K),
                            Acc#{Key => V}
                                  end, #{}, OldData),
                    dgiot_map:merge(NewOldData, NewData);
                false ->
                    NewData
            end
    end.

save_td_no_match(ProductId, DevAddr, Ack, AppData) ->
    case length(maps:to_list(Ack)) of
        0 ->
            #{};
        _ ->
%%            计算上报值
            Collection = dgiot_task:get_collection(ProductId, [], Ack),
%%            计算计算值
            Calculated = dgiot_task:get_calculated(ProductId, Collection),
%%            过滤存储值
            Storage = dgiot_task:get_storage(ProductId, Calculated),
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            Interval = maps:get(<<"interval">>, AppData, 3),
            AllData = merge_cache_data(DeviceId, Storage, Interval),
            dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage),
            AllData
    end.
