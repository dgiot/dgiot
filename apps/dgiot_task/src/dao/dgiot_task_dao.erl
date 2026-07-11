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

%% @doc 任务统计数据访问层
%% 负责任务统计的数据访问操作，包括缓存管理、PN队列管理等
-module(dgiot_task_dao).
-include("dgiot_task.hrl").

-include_lib("dgiot/include/logger.hrl").

%% API导出
-export([start/2, save_client/2, del_client/1]).
-export([save_pnque/4, get_pnque_len/1, get_pnque/1, del_pnque/1]).
-export([merge_cache_data/3, save_cache_data/2, binary_key_map/1]).
-export([send/3]).

%%%===================================================================
%%% 客户端管理函数
%%%===================================================================

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

%%%===================================================================
%%% PN队列管理函数
%%%===================================================================

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

%% @doc 删除PN队列
%% 删除指定DTU的PN队列
%% @param DtuId DTU设备ID
del_pnque(DtuId) ->
    case dgiot_data:get(?DGIOT_PNQUE, DtuId) of
        not_find ->
            pass;
        PnQue when length(PnQue) > 0 ->
            dgiot_data:delete(?DGIOT_PNQUE, DtuId);
        _ ->
            pass
    end.

%%%===================================================================
%%% 数据发送函数
%%%===================================================================

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

%%%===================================================================
%%% 缓存管理函数
%%%===================================================================

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

%% @doc 保存缓存数据
%% 将数据保存到缓存中
%% @param DeviceId 设备ID
%% @param Data 数据
save_cache_data(DeviceId, Data) ->
    NewData = maps:fold(fun(K, V, Acc) ->
                                AtomKey = dgiot_utils:to_atom(K),
                                Acc#{AtomKey => V}
                        end,
                        #{},
                        Data),
    dgiot_data:insert(?DGIOT_DATA_CACHE, DeviceId, {NewData, dgiot_datetime:now_ms()}).

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
