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

%% 内部工具模块，仅供 dgiot_task 模块内部使用
-module(dgiot_task_utils).

-include("dgiot_task.hrl").
%% Removed unused include file: logger.hrl

%% 导出统计处理函数
-export([handle_duration_statistic/7, handle_frequency_statistic/7]).

%% @doc 处理时长统计
%% 根据条件统计满足条件的时长，用于累加统计
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Key 统计键
%% @param Identifier 物模型标识符
%% @param KeyValue 当前键值
%% @param DataSource 数据源配置（包含比较类型和比较值）
%% @param Acc 累计结果
%% @return 更新后的累计结果
handle_duration_statistic(ProductId, DevAddr, Key, Identifier, KeyValue, 
                         #{<<"comparetype">> := Comparetype, <<"value">> := Value}, Acc) ->
    Last_Value = dgiot_task:get_last_value(ProductId, DevAddr, Key, Identifier),
    case dgiot_task:compare(KeyValue, Comparetype, dgiot_utils:to_int(Value)) of
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
    end.

%% @doc 处理次数统计
%% 根据条件统计满足条件的次数，用于频率统计
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param Key 统计键
%% @param Identifier 物模型标识符
%% @param KeyValue 当前键值
%% @param DataSource 数据源配置（包含比较类型和比较值）
%% @param Acc 累计结果
%% @return 更新后的累计结果
handle_frequency_statistic(ProductId, DevAddr, Key, Identifier, KeyValue,
                          #{<<"comparetype">> := Comparetype, <<"value">> := Value}, Acc) ->
    Num = dgiot_task:get_last_value(ProductId, DevAddr, Key, Identifier),
    case dgiot_task:compare(KeyValue, Comparetype, dgiot_utils:to_int(Value)) of
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
    end.
