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

%% @doc 任务统计主模块
%% 负责任务统计的核心业务逻辑，包括数据采集、计算、存储和指令生成
-module(dgiot_task).
-include("dgiot_task.hrl").

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").

%% 导出函数
-export([start/2, send/3, get_pnque_len/1, save_pnque/4, get_pnque/1, del_pnque/1, save_td/4, merge_cache_data/3, save_cache_data/2]).
-export([get_props/1, get_control/3, get_collection/4, get_calculated/4, get_instruct/2, get_storage/2, string2value/2, string2value/3, get_statistic/7]).
-export([save_td_no_match/4, get_last_value/4]).
-export([save_client/2, del_client/1]).
-export([compare/3]).

%% @doc 注册任务统计协议类型
%% 定义任务统计协议的基本信息，包括类型、列数、标题和描述
-protocol_type(#{
                 cType => <<"TASKSTATISTICS">>,  %% 协议类型代码
                 type => <<"TASKSTATISTICS">>,  %% 协议类型名称
                 colum => 10,  %% 显示列数
                 title => #{
                            zh => <<"任务统计"/utf8>>  %% 中文标题
                           },
                 description => #{
                                  zh => <<"任务统计"/utf8>>  %% 中文描述
                                 }
                }).

%% @doc 定义协议参数
%% 配置任务统计协议的可配置参数，包括统计类型、键、比较类型和值
-params(#{
          <<"type">> => #{
                          order => 1,  %% 参数顺序
                          type => string,
                          required => true,
                          default => #{<<"value">> => <<"duration">>, <<"label">> => <<"时长累加"/utf8>>},  %% 默认值
                          enum => [  %% 枚举选项
                                   #{<<"value">> => <<"duration">>, <<"label">> => <<"时长累加"/utf8>>},
                                   #{<<"value">> => <<"frequency">>, <<"label">> => <<"次数累加"/utf8>>}],
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
                                 enum => [  %% 比较类型枚举
                                          #{<<"value">> => <<"LT">>, <<"label">> => <<"小于"/utf8>>},
                                          #{<<"value">> => <<"LE">>, <<"label">> => <<"小于等于"/utf8>>},
                                          #{<<"value">> => <<"GT">>, <<"label">> => <<"大于"/utf8>>},
                                          #{<<"value">> => <<"GE">>, <<"label">> => <<"大于等于"/utf8>>},
                                          #{<<"value">> => <<"EQ">>, <<"label">> => <<"等于"/utf8>>},
                                          #{<<"value">> => <<"NE">>, <<"label">> => <<"不等于"/utf8>>}],
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


%% @doc 启动任务客户端
%% 根据通道ID和产品ID列表启动对应的任务客户端
%% @param ChannelId 通道ID
%% @param ProductIds 产品ID列表
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
              end,
              ets:tab2list(?DGIOT_PNQUE)).


%% @doc 保存客户端到任务列表
%% 将客户端ID保存到指定通道的任务客户端列表中
%% @param ChannelId 通道ID
%% @param ClientId 客户端ID
save_client(ChannelId, ClientId) ->
    case dgiot_data:get(?DGIOT_TASK, ChannelId) of
        not_find ->
            dgiot_data:insert(?DGIOT_TASK, ChannelId, [ClientId]);
        ClientIds ->
            New_ClientIds = dgiot_utils:unique_2(ClientIds ++ [ClientId]),
            dgiot_data:insert(?DGIOT_TASK, ChannelId, New_ClientIds)
    end.


%% @doc 删除通道的所有客户端
%% 停止并删除指定通道的所有任务客户端
%% @param ChannelId 通道ID
del_client(ChannelId) ->
    case dgiot_data:get(?DGIOT_TASK, ChannelId) of
        not_find ->
            pass;
        ClientIds when length(ClientIds) > 0 ->
            lists:map(fun(ClientId) ->
                              dgiot_client:stop(ChannelId, ClientId)
                      end,
                      ClientIds),
            dgiot_data:delete(?DGIOT_TASK, ChannelId);
        _ ->
            pass
    end.


%% @doc 发送数据到任务通道
%% 通过任务通道发送设备上报数据
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Payload 数据负载
send(ProductId, DevAddr, Payload) ->
    case dgiot_data:get({?TYPE, ProductId}) of
        not_find ->
            pass;
        ChannelId ->
            Topic = <<"$dg/thing/", ProductId/binary, "/", DevAddr/binary, "/properties/report">>,
            dgiot_client:send(ChannelId, DevAddr, Topic, Payload)
    end.


%% @doc 获取上次统计值
%% 从缓存或TDengine数据库获取上次统计的值
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Key 统计键
%% @param Identifier 物模型标识符
%% @return 上次统计值
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


%% @doc 获取统计值
%% 根据统计类型（时长/次数）获取相应的统计值
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Key 统计键
%% @param Identifier 物模型标识符
%% @param KeyValue 当前键值
%% @param DataSource 数据源配置
%% @param Acc 累计结果
%% @return 更新后的统计结果
get_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, #{<<"type">> := <<"duration">>} = DataSource, Acc) ->
    dgiot_task_utils:handle_duration_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, DataSource, Acc);

%% @doc 处理次数统计
get_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, #{<<"type">> := <<"frequency">>} = DataSource, Acc) ->
    dgiot_task_utils:handle_frequency_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, DataSource, Acc);

%% @doc 默认统计处理
get_statistic(_, _, _, _, _, _, Acc) ->
    Acc.


%% @doc 获取计算值
%% 根据物模型配置计算统计值，必须返回物模型里面的数据表示
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Calculated 已计算的数据
%% @param Props 物模型属性列表
%% @return 包含计算值的映射
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    lists:foldl(fun(X, Acc) ->
                        case Acc of
                            error ->
                                Acc;
                            _ ->
                                case X of
                                    #{
                                      <<"isaccumulate">> := true,
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
                                    #{
                                      <<"isstorage">> := true,
                                      <<"identifier">> := Identifier,
                                      <<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>, <<"collection">> := Collection},
                                      <<"dataType">> := #{<<"type">> := Type, <<"specs">> := Specs}
                                     } ->
                                        Str1 = maps:fold(fun(K, V, Acc2) ->
                                                                 Str = re:replace(Acc2, dgiot_utils:to_list(<<"%%{", K/binary, "}">>), dgiot_utils:to_list(V), [global, {return, list}]),
                                                                 re:replace(Str, "%{s}", dgiot_utils:to_list(V), [global, {return, list}])
                                                         end,
                                                         dgiot_utils:to_list(Collection),
                                                         Calculated),
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
                end,
                Calculated,
                Props).


%% @doc 获取物模型属性列表
%% 查询产品的物模型属性配置
%% @param ProductId 产品ID
%% @return 物模型属性列表
get_props(ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            Props;
        _Error ->
            []
    end.


%% @doc 获取采集数据（主动上报模式）
%% 当设备主动上报数据时，根据物模型配置获取用户数据
%% @param ProductId 产品ID
%% @param Dis 设备标识符列表（为空表示主动上报）
%% @param Payload 原始数据负载
%% @param Props 物模型属性列表
%% @return 处理后的数据映射
get_collection(ProductId, [], Payload, Props) ->
    lists:foldl(fun(X, Acc2) ->
                        case Acc2 of
                            error ->
                                Acc2;
                            _ ->
                                case X of
                                    #{
                                      <<"dataForm">> := #{<<"strategy">> := Strategy} = DataForm,
                                      <<"dataType">> := DataType,
                                      <<"identifier">> := Identifier
                                     } when Strategy =/= <<"计算值"/utf8>> ->
                                        dgiot_task_data:get_userdata(ProductId, Identifier, DataForm, DataType, Payload, Acc2);
                                    _ ->
                                        Acc2
                                end
                        end
                end,
                Payload,
                Props);

%% @doc 获取采集数据（指定标识符模式）
%% 根据指定的设备标识符列表获取用户数据
%% @param ProductId 产品ID
%% @param Dis 设备标识符列表
%% @param Payload 原始数据负载
%% @param Props 物模型属性列表
%% @return 处理后的数据映射
get_collection(ProductId, Dis, Payload, Props) ->
    lists:foldl(fun(Identifier, Acc1) ->
                        lists:foldl(fun(X, Acc2) ->
                                            case Acc2 of
                                                error ->
                                                    Acc2;
                                                _ ->
                                                    case X of
                                                        #{
                                                          <<"dataForm">> := #{<<"strategy">> := Strategy} = DataForm,
                                                          <<"dataType">> := DataType,
                                                          <<"identifier">> := Identifier
                                                         } when Strategy =/= <<"计算值"/utf8>> ->
                                                            dgiot_task_data:get_userdata(ProductId, Identifier, DataForm, DataType, Payload, Acc2);
                                                        _ ->
                                                            Acc2
                                                    end
                                            end
                                    end,
                                    Acc1,
                                    Props)
                end,
                Payload,
                Dis).


%% @doc 获取控制值
%% 根据轮次、数据和控件模板生成控制值
%% @param Round 轮次
%% @param Data 原始数据
%% @param Control 控件模板字符串
%% @return 处理后的控制值
get_control(Round, Data, Control) ->
    case Data of
        <<"null">> ->
            <<"null">>;
        Data ->
            Str = re:replace(dgiot_utils:to_list(Control), "%{d}", dgiot_utils:to_list(Data), [global, {return, list}]),
            Str1 = re:replace(Str, "%{r}", dgiot_utils:to_list(Round), [global, {return, list}]),
            dgiot_task:string2value(Str1, <<"type">>)
    end.


%% @doc 获取存储值
%% 从计算数据中筛选需要存储的物模型属性
%% @param Calculated 计算后的数据
%% @param Props 物模型属性列表
%% @return 需要存储的数据映射
get_storage(Calculated, Props) ->
    lists:foldl(fun(#{<<"isstorage">> := true, <<"identifier">> := Identifier}, Acc) ->
                        case maps:find(Identifier, Calculated) of
                            {ok, Value} ->
                                Acc#{Identifier => Value};
                            _ ->
                                Acc
                        end;
                   (_, Acc) ->
                        Acc
                end,
                #{},
                Props).


get_instruct(ProductId, Round) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} when length(Props) > 0 ->
            {_, NewList} = lists:foldl(fun(X, Acc) ->
                                               {Seq, List} = Acc,
                                               case X of
                                                   #{<<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>}} ->  %% 计算值加入采集指令队列
                                                       Acc;
                                                   #{<<"dataForm">> := #{<<"strategy">> := <<"主动上报"/utf8>>}} ->  %% 主动上报值加入采集指令队列
                                                       Acc;
                                                   #{
                                                     <<"accessMode">> := AccessMode,
                                                     <<"identifier">> := Identifier,
                                                     <<"dataType">> := #{<<"specs">> := Specs},
                                                     <<"dataForm">> := DataForm,
                                                     <<"dataSource">> := DataSource
                                                    } ->
                                                       Min = maps:get(<<"min">>, Specs, 0),
                                                       Protocol = maps:get(<<"protocol">>, DataForm, <<"Dlink">>),
                                                       Control = maps:get(<<"control">>, DataForm, "%{d}"),  %% 控制参数
                                                       Data = dgiot_task:get_control(Round, Min, Control),  %% 控制参数的初始值，可以根据轮次进行计算
                                                       NewDataSource = dgiot_task_data:get_datasource(Protocol, AccessMode, Data, DataSource),  %% 根据协议类型生成采集数据格式
                                                       Order = maps:get(<<"order">>, DataForm, Seq),  %% 指令顺序
                                                       Interval = dgiot_utils:to_int(maps:get(<<"strategy">>, DataForm, 20)),  %% 下一个指令的采集间隔
                                                       ThingRound = maps:get(<<"round">>, DataForm, <<"all">>),  %% 物模型中的指令轮次规则
                                                       BinRound = dgiot_utils:to_binary(Round),  %% 判断本轮是否需要加入采集指令队列
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
                                       end,
                                       {1, []},
                                       Props),
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


%% @doc 保存PN队列
%% 将产品设备对保存到DTU的PN队列中，并订阅相关MQTT主题
%% @param DtuProductId DTU产品ID
%% @param DtuAddr DTU地址
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
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


%% @doc 获取PN队列长度
%% 获取指定DTU的PN队列长度
%% @param DtuId DTU设备ID
%% @return 队列长度
get_pnque_len(DtuId) ->
    case dgiot_data:get(?DGIOT_PNQUE, DtuId) of
        not_find ->
            0;
        PnQue ->
            length(PnQue)
    end.


%% @doc 获取PN队列
%% 轮询获取PN队列中的下一个产品设备对（循环队列）
%% @param DtuId DTU设备ID
%% @return {ProductId, DevAddr} | not_find
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
    case maps:size(Ack) of
        0 ->
            #{};
        _ ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            Interval = dgiot_product:get_interval(ProductId),
            %% 是否有缓存
            CacheData = dgiot_task:merge_cache_data(DeviceId, Ack, Interval),
            %% 获取物模型
            Props = dgiot_task:get_props(ProductId),
            %% 计算上报值
            Collection = dgiot_task:get_collection(ProductId, [], CacheData, Props),
            %% 计算计算值
            % io:format("Calculated Collection ~p ~n", [Collection]),
            AllData = dgiot_task:get_calculated(ProductId, DevAddr, Collection, Props),
            %% 过滤存储值
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
    try
        dgiot_channelx:do_message(ChannelId, {topo_thing, ProductId, DeviceId, AllData})
    catch
        _ExceptionType1:_ExPattern1 ->
            % io:format("~s ~p ~p ~p ~n", [?FILE, ?LINE, _ExceptionType1, _ExPattern1]),
            pass
    end,

    %%  save td
    dgiot_tdengine_adapter:save(ProductId, DevAddr, Storage),
    dgiot_metrics:inc(dgiot_task, <<"task_save">>, 1),
    Channel = dgiot_product_channel:get_taskchannel(ProductId),
    dgiot_bridge:send_log(Channel, ProductId, DevAddr, "~s ~p save td => ProductId ~p DevAddr ~p ~ts ", [?FILE, ?LINE, ProductId, DevAddr, unicode:characters_to_list(dgiot_json:encode(Storage))]),
    
    %% 更新两个缓存：确保实时卡片API能获取到数据
    %% 1. 更新标准缓存（?DGIOT_DATA_CACHE）
    %% 2. 更新last_data缓存（供实时卡片API使用）
    dgiot_data:put({last_data, DeviceId}, AllData),
    ?LOG(info, "Updated last_data cache for device ~p", [DeviceId]),
    
    Storage.


save_cache_data(DeviceId, Data) ->
    NewData = maps:fold(fun(K, V, Acc) ->
                                AtomKey = dgiot_utils:to_atom(K),
                                Acc#{AtomKey => V}
                        end,
                        #{},
                        Data),
    dgiot_data:insert(?DGIOT_DATA_CACHE, DeviceId, {NewData, dgiot_datetime:now_ms()}).


%% @doc 合并缓存数据
%% 根据时间间隔合并新旧缓存数据，避免频繁的数据库写入
%% @param DeviceId 设备ID
%% @param NewData 新数据
%% @param Interval 缓存间隔（0表示不使用缓存）
%% @return 合并后的数据
merge_cache_data(DeviceId, NewData, Interval) ->
    case Interval of
        0 ->
            NewData;
        _ ->
            case dgiot_data:get(?DGIOT_DATA_CACHE, DeviceId) of
                not_find ->
                    NewData;
                {OldData, _} ->
                    NewOldData = binary_key_map(OldData),
                    dgiot_map:merge(NewOldData, NewData)
            end
    end.


%% @doc 将原子键转换为二进制键
%% 用于统一缓存数据的键格式
%% @param OldData 包含原子键的旧数据
%% @return 包含二进制键的新数据
binary_key_map(OldData) ->
    maps:fold(fun(K, V, Acc) ->
                      Key = dgiot_utils:to_binary(K),
                      Acc#{Key => V}
              end,
              #{},
              OldData).


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


%% @doc 比较两个值
%% 根据比较类型比较两个值，支持 LT、LE、GT、GE、EQ、NE 六种比较类型
%% @param Value1 第一个值
%% @param CompareType 比较类型（<<"LT">>, <<"LE">>, <<"GT">>, <<"GE">>, <<"EQ">>, <<"NE">>）
%% @param Value2 第二个值
%% @return 布尔值，表示比较结果
-spec compare(Value1 :: term(), CompareType :: binary(), Value2 :: term()) -> boolean().
compare(Value1, <<"LT">>, Value2) ->
    Value1 < Value2;
compare(Value1, <<"LE">>, Value2) ->
    Value1 =< Value2;
compare(Value1, <<"GT">>, Value2) ->
    Value1 > Value2;
compare(Value1, <<"GE">>, Value2) ->
    Value1 >= Value2;
compare(Value1, <<"EQ">>, Value2) ->
    Value1 == Value2;
compare(Value1, <<"NE">>, Value2) ->
    Value1 /= Value2;
compare(_Value1, _CompareType, _Value2) ->
    false.
