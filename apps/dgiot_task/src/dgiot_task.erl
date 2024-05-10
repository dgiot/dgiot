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

-export([start/2, send/3, get_pnque_len/1, save_pnque/4, get_pnque/1, del_pnque/1, save_td/4, merge_cache_data/3, save_cache_data/2]).
-export([get_props/1, get_control/3, get_collection/4, get_calculated/4, get_instruct/2, get_storage/2, string2value/2, string2value/3, get_statistic/7]).
-export([save_td_no_match/4, get_last_value/4]).
-export([save_client/2, del_client/1]).

%% 注册协议类型
-protocol_type(#{
    cType => <<"TASKSTATISTICS">>,
    type => <<"TASKSTATISTICS">>,
    colum => 10,
    title => #{
        zh => <<"任务统计"/utf8>>
    },
    description => #{
        zh => <<"任务统计"/utf8>>
    }
}).

-params(#{
    <<"type">> => #{
        order => 1,
        type => string,
        required => true,
        default => #{<<"value">> => <<"duration">>, <<"label">> => <<"时长累加"/utf8>>},
        enum => [
            #{<<"value">> => <<"duration">>, <<"label">> => <<"时长累加"/utf8>>},
            #{<<"value">> => <<"frequency">>, <<"label">> => <<"次数累加"/utf8>>}
        ],
        title => #{
            zh => <<"条件"/utf8>>
        },
        description => #{
            zh => <<"条件"/utf8>>
        }
    },
    <<"key">> => #{
        order => 2,
        type => string,
        required => true,
        default => <<"key"/utf8>>,
        title => #{
            zh => <<"物模型标识符"/utf8>>
        },
        description => #{
            zh => <<"统计的物模型标识符"/utf8>>
        }
    },
    <<"comparetype">> => #{
        order => 3,
        type => string,
        required => true,
        default => #{<<"value">> => <<"EQ">>, <<"label">> => <<"等于"/utf8>>},
        enum => [
            #{<<"value">> => <<"LT">>, <<"label">> => <<"小于"/utf8>>},
            #{<<"value">> => <<"LE">>, <<"label">> => <<"小于等于"/utf8>>},
            #{<<"value">> => <<"GT">>, <<"label">> => <<"大于"/utf8>>},
            #{<<"value">> => <<"GE">>, <<"label">> => <<"大于等于"/utf8>>},
            #{<<"value">> => <<"EQ">>, <<"label">> => <<"等于"/utf8>>},
            #{<<"value">> => <<"NE">>, <<"label">> => <<"不等于"/utf8>>}
        ],
        title => #{
            zh => <<"条件"/utf8>>
        },
        description => #{
            zh => <<"条件"/utf8>>
        }
    },
    <<"value">> => #{
        order => 4,
        type => string,
        required => true,
        default => <<"1">>,
        title => #{
            zh => <<"值"/utf8>>
        },
        description => #{
            zh => <<"物模型比较值"/utf8>>
        }
    }
}).

start(ChannelId, ProductIds) ->
    lists:map(fun(Y) ->
        case Y of
            {ClientId, [{ProductId, _} | _]} ->
                case lists:member(ProductId, ProductIds) of
                    true ->
                        timer:sleep(1),
                        dgiot_data:insert({taskchannel_product, binary_to_atom(ProductId)}, ChannelId),
                        save_client(ChannelId, ClientId),
                        dgiot_client:start(ChannelId, ClientId);
                    _ ->
                        pass

                end;
            _ ->
                pass
        end
              end, ets:tab2list(?DGIOT_PNQUE)).

save_client(ChannelId, ClientId) ->
    case dgiot_data:get(?DGIOT_TASK, ChannelId) of
        not_find ->
            dgiot_data:insert(?DGIOT_TASK, ChannelId, [ClientId]);
        ClientIds ->
            New_ClientIds = dgiot_utils:unique_2(ClientIds ++ [ClientId]),
            dgiot_data:insert(?DGIOT_TASK, ChannelId, New_ClientIds)
    end.

del_client(ChannelId) ->
    case dgiot_data:get(?DGIOT_TASK, ChannelId) of
        not_find ->
            pass;
        ClientIds when length(ClientIds) > 0 ->
            lists:map(fun(ClientId) ->
                dgiot_client:stop(ChannelId, ClientId)
                      end, ClientIds),
            dgiot_data:delete(?DGIOT_TASK, ChannelId);
        _ ->
            pass
    end.

send(ProductId, DevAddr, Payload) ->
    case dgiot_data:get({?TYPE, ProductId}) of
        not_find ->
            pass;
        ChannelId ->
            Topic = <<"$dg/thing/", ProductId/binary, "/", DevAddr/binary, "/properties/report">>,
            dgiot_client:send(ChannelId, DevAddr, Topic, Payload)
    end.

%% 比较统计值
compare(KeyValue, <<"LT">>, Value) ->
    KeyValue < Value;
compare(KeyValue, <<"LE">>, Value) ->
    KeyValue =< Value;
compare(KeyValue, <<"GT">>, Value) ->
    KeyValue > Value;
compare(KeyValue, <<"GE">>, Value) ->
    KeyValue >= Value;
compare(KeyValue, <<"EQ">>, Value) ->
    KeyValue == Value;
compare(KeyValue, <<"NE">>, Value) ->
    KeyValue =/= Value;
compare(_, _, _) ->
    false.

%% 查询上次值
%% select last(devaddr) as devaddr FROM  _24b9b4bc50._1c9966755d;
%% dgiot_data:get({last_value, <<"857ed41119">>, <<"PDJ">>, <<"m583_10">>, <<"bc11c_failnum">>})
get_last_value(ProductId, DevAddr, Key, Identifier) ->
    case dgiot_data:get({last_value, ProductId, DevAddr, Key, Identifier}) of
        not_find ->
            case dgiot_tdengine:get_channel(ProductId) of
                {ok, Channel} ->
                    dgiot_tdengine:transaction(Channel,
                        fun(Context) ->
                            DB = dgiot_tdengine:get_database(Channel, ProductId),
                            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
                            Sql = <<"select last(", Identifier/binary, ") as ", Identifier/binary, " FROM ", DB/binary, "_", DeviceId/binary, ";">>,
                            case dgiot_tdengine_pool:run_sql(Context#{<<"channel">> => Channel}, execute_query, Sql) of
                                {ok, #{<<"results">> := [#{Identifier := Value} | _]}} when Value =/= null ->
                                    dgiot_utils:to_int(Value);
                                _ ->
                                    0
                            end
                        end);
                _ ->
                    0
            end;
        Value ->
            dgiot_utils:to_int(Value)
    end.

%% 统计时长
get_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, #{<<"type">> := <<"duration">>, <<"comparetype">> := Comparetype, <<"value">> := Value}, Acc) ->
    Last_Value = get_last_value(ProductId, DevAddr, Key, Identifier),
    case compare(KeyValue, Comparetype, dgiot_utils:to_int(Value)) of
        true ->
            Time =
                case dgiot_data:get({last_time, ProductId, DevAddr, Key, Identifier}) of
                    {true, OldTime} ->
                        dgiot_datetime:now_secs() - OldTime;
                    _ ->
                        0
                end,
            dgiot_data:insert({last_time, ProductId, DevAddr, Key, Identifier}, {true, dgiot_datetime:now_secs()}),
            dgiot_data:insert({last_value, ProductId, DevAddr, Key, Identifier}, Last_Value + Time),
            Acc#{Identifier => Last_Value + Time};
        _ ->
            dgiot_data:insert({last_time, ProductId, DevAddr, Key, Identifier}, {false, dgiot_datetime:now_secs()}),
            dgiot_data:insert({last_value, ProductId, DevAddr, Key, Identifier}, Last_Value),
            Acc#{Identifier => Last_Value}
    end;

%% 次数累加
%% dgiot_data:get({last_flag, <<"857ed41119">>, <<"PDJ">>, <<"m583_10">>, <<"bc11c_failnum">>}).
get_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, #{<<"type">> := <<"frequency">>, <<"comparetype">> := Comparetype, <<"value">> := Value}, Acc) ->
    Num = get_last_value(ProductId, DevAddr, Key, Identifier),
    case compare(KeyValue, Comparetype, dgiot_utils:to_int(Value)) of
        true ->
            case dgiot_data:get({last_flag, ProductId, DevAddr, Key, Identifier}) of
                not_find when Num =:= 0 ->
                    dgiot_data:insert({last_value, ProductId, DevAddr, Key, Identifier}, Num + 1),
                    dgiot_data:insert({last_flag, ProductId, DevAddr, Key, Identifier}, true),
                    Acc#{Identifier => Num + 1};
                false ->
                    dgiot_data:insert({last_value, ProductId, DevAddr, Key, Identifier}, Num + 1),
                    dgiot_data:insert({last_flag, ProductId, DevAddr, Key, Identifier}, true),
                    Acc#{Identifier => Num + 1};
                _ ->
                    Acc#{Identifier => Num}
            end;
        _ ->
            dgiot_data:insert({last_value, ProductId, DevAddr, Key, Identifier}, Num),
            dgiot_data:insert({last_flag, ProductId, DevAddr, Key, Identifier}, false),
            Acc#{Identifier => Num}
    end;

get_statistic(_, _, _, _, _, _, Acc) ->
    Acc.

%%获取计算值，必须返回物模型里面的数据表示，不能用寄存器地址
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    lists:foldl(fun(X, Acc) ->
        case Acc of
            error ->
                Acc;
            _ ->
                case X of
                    #{<<"isaccumulate">> := true,
                        <<"isstorage">> := true,
                        <<"identifier">> := Identifier,
                        <<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>},
                        <<"dataSource">> := #{<<"key">> := Key} = DataSource
                    } ->
                        case maps:get(Key, Calculated, not_find) of
                            not_find ->
                                Acc;
                            KeyValue ->
                                get_statistic(ProductId, DevAddr, Key, Identifier, dgiot_utils:to_int(KeyValue), DataSource, Acc)
                        end;
                    #{<<"isstorage">> := true,
                        <<"identifier">> := Identifier,
                        <<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>, <<"collection">> := Collection},
                        <<"dataType">> := #{<<"type">> := Type, <<"specs">> := Specs}
                    } ->
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
                end, Calculated, Props).

get_props(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            Props;
        _Error ->
            []
    end.

%% 主动上报 dis为[]
get_collection(ProductId, [], Payload, Props) ->
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

%%转换设备上报值，必须返回物模型里面的数据表示，不能用寄存器地址
get_collection(ProductId, Dis, Payload, Props) ->
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
                end, Payload, Dis).

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
get_storage(Calculated, Props) ->
    lists:foldl(fun
                    (#{<<"isstorage">> := true, <<"identifier">> := Identifier}, Acc) ->
                        case maps:find(Identifier, Calculated) of
                            {ok, Value} ->
                                Acc#{Identifier => Value};
                            _ ->
                                Acc
                        end;
                    (_, Acc) ->
                        Acc
                end, #{}, Props).

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
    Topic = <<"$dg/thing/", ProductId/binary, "/", DevAddr/binary, "/properties/report">>,
    dgiot_mqttc_channel:send(ProductId, DevAddr, Topic, Ack),
    case length(maps:to_list(Ack)) of
        0 ->
            #{};
        _ ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            Interval = dgiot_product:get_interval(ProductId),
            %%            是否有缓存
            CacheData = dgiot_task:merge_cache_data(DeviceId, Ack, Interval),
            %%            获取物模型
            Props = dgiot_task:get_props(ProductId),
            %%            计算上报值
            Collection = dgiot_task:get_collection(ProductId, [], CacheData, Props),
            %%            计算计算值
            AllData = dgiot_task:get_calculated(ProductId, DevAddr, Collection, Props),
            %%            过滤存储值
            Storage = dgiot_task:get_storage(AllData, Props),
            save_cache_data(DeviceId, CacheData),
            dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage, Interval)
    end.

%% 处理数据
dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage, _Interval) ->
    %%                    告警
    NotificationTopic = <<"$dg/user/alarm/", ProductId/binary, "/", DeviceId/binary, "/properties/report">>,
    dgiot_mqtt:publish(DeviceId, NotificationTopic, dgiot_json:encode(AllData)),
    %% 实时数据
    ChannelId = dgiot_parse_id:get_channelid(dgiot_utils:to_binary(?BRIDGE_CHL), <<"DGIOTTOPO">>, <<"TOPO组态通道"/utf8>>),
    dgiot_channelx:do_message(ChannelId, {topo_thing, ProductId, DeviceId, AllData}),
    %%  save td
    dgiot_tdengine_adapter:save(ProductId, DevAddr, Storage),
    dgiot_metrics:inc(dgiot_task, <<"task_save">>, 1),
    Channel = dgiot_product_channel:get_taskchannel(ProductId),
    dgiot_bridge:send_log(Channel, ProductId, DevAddr, "~s ~p save td => ProductId ~p DevAddr ~p ~ts ", [?FILE, ?LINE, ProductId, DevAddr, unicode:characters_to_list(dgiot_json:encode(Storage))]),
    Storage.

save_cache_data(DeviceId, Data) ->
    NewData = maps:fold(fun(K, V, Acc) ->
        AtomKey = dgiot_utils:to_atom(K),
        Acc#{AtomKey => V}
                        end, #{}, Data),
    dgiot_data:insert(?DGIOT_DATA_CACHE, DeviceId, {NewData, dgiot_datetime:now_ms()}).

merge_cache_data(_DeviceId, NewData, 0) ->
    NewData;

merge_cache_data(DeviceId, NewData, _) ->
    case dgiot_data:get(?DGIOT_DATA_CACHE, DeviceId) of
        not_find ->
            NewData;
        {OldData, _} ->
            NewOldData =
                maps:fold(fun(K, V, Acc) ->
                    Key = dgiot_utils:to_binary(K),
                    Acc#{Key => V}
                          end, #{}, OldData),
            dgiot_map:merge(NewOldData, NewData)
    end.

save_td_no_match(ProductId, DevAddr, Ack, AppData) ->
    case length(maps:to_list(Ack)) of
        0 ->
            #{};
        _ ->
            Props = dgiot_task:get_props(ProductId),
%%            计算上报值
            Collection = dgiot_task:get_collection(ProductId, [], Ack, Props),
%%            计算计算值
            Calculated = dgiot_task:get_calculated(ProductId, DevAddr, Collection, Props),
%%            过滤存储值
            Storage = dgiot_task:get_storage(Calculated, Props),
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            Interval = maps:get(<<"interval">>, AppData, 3),
            AllData = merge_cache_data(DeviceId, Storage, Interval),
            dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage, Interval),
            AllData
    end.
