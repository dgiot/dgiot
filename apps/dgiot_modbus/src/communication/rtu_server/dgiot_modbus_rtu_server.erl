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
-module(dgiot_modbus_rtu_server).
-author("stoneliu").
-include("dgiot_modbus.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_device/include/dgiot_device.hrl").

%% 日志辅助宏 - 使用标准?LOG宏，支持动态日志级别调整
%% 通过logger:set_primary_config(level, Level)或环境变量设置日志级别
%% 支持的级别: debug, info, notice, warning, error, critical, alert, emergency
%% 示例: ?LOG(debug, "调试信息: ~p", [Data])
%% 动态调整: logger:set_module_level(dgiot_modbus_rtu_server, debug)

-export([start/2, init/1, handle_info/2, handle_cast/2, handle_call/3, 
         terminate/2, code_change/3, send_aggregated_device_report/5,
         handle_port_registration/7, handle_regular_registration/6,
         get_actual_dtu_addr/2, get_actual_product_id/3,
         build_data_things/5, send_to_task_channel/5]).

%% @doc 启动TCP服务器
%% Port参数说明：这是服务器端口（通道配置中的端口），如20000
%% 注意：这是服务器监听的端口，不是客户端连接端口
%% 服务器端口是固定的配置端口，用于接收设备连接
start(Port, State) ->
    dgiot_tcp_server:child_spec(?MODULE, dgiot_utils:to_int(Port), State).

%% =======================
%% 初始化函数
%% =======================

init(#tcp{socket = Socket, state = #state{id = ChannelId, dtutype = Dtutype, regtype = <<"RegisterByIp">>} = State} = TCPState) ->
    % 安全获取IP地址，处理可能的socket错误
    DtuAddr = case dgiot_utils:get_ip(Socket) of
        <<"">> -> 
            ?LOG(warning, "Failed to get IP from socket, using default address"),
            <<"unknown_ip">>;
        IP -> 
            IP
    end,
    ?LOG(info, "Device Connected by IP, ChannelId: ~p, DtuAddr: ~p", [ChannelId, DtuAddr]),
    ?LOG(debug, "Socket info: ~p", [Socket]),
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _TYPE, [ProductId | _]} ->
            ?LOG(debug, "IP注册处理: ChannelId=~p, ProductId=~p, DtuAddr=~p", [ChannelId, ProductId, DtuAddr]),
            handle_ip_registration(ChannelId, ProductId, DtuAddr, Dtutype, TCPState, State);
        {error, not_find} ->
            ?LOG(error, "Channel not found: ~p", [ChannelId]),
            {stop, not_find_channel}
    end;

init(#tcp{state = #state{id = ChannelId}} = TCPState) ->
    ?LOG(info, "Device Connected, ChannelId: ~p", [ChannelId]),
    case dgiot_bridge:get_products(ChannelId) of
        {ok, _TYPE, _} -> 
            ?LOG(debug, "Products found for channel: ~p", [ChannelId]),
            {ok, TCPState};
        {error, not_find} ->
            ?LOG(error, "Channel not found: ~p", [ChannelId]),
            {stop, not_find_channel}
    end.


%% =======================
%% 上行报文处理（简化版）
%% =======================

%% 处理设备注册（RegisterByPort和RegisterByRegular）
handle_info({tcp, Buff}, #tcp{register = false, socket = _Socket,
                state = #state{id = ChannelId, regtype = RegType, head = Head, dtutype = Dtutype, env = Env} = State} = TCPState) ->
    case RegType of
        <<"RegisterByPort">> ->
            % RegisterByPort使用服务器端口（通道端口）
            case Env of
                #{port := ServerPort} ->
                    ?LOG(debug, "RegisterByPort: Processing registration with server port ~p", [ServerPort]),
                    handle_port_registration(ChannelId, Buff, Head, Dtutype, ServerPort, TCPState, State);
                _ ->
                    ?LOG(error, "RegisterByPort: Server port not found in env, falling back to regular registration", []),
                    handle_regular_registration(ChannelId, Buff, Head, Dtutype, TCPState, State)
            end;
        <<"RegisterByRegular">> ->
            ?LOG(debug, "RegisterByRegular: Processing registration", []),
            handle_regular_registration(ChannelId, Buff, Head, Dtutype, TCPState, State);
        _ ->
            ?LOG(error, "Unknown registration type: ~p, ChannelId: ~p", [RegType, ChannelId]),
            {noreply, TCPState#tcp{buff = <<>>}}
    end;

%% 处理已注册设备的数据包（统一处理函数）
%% 通讯层接收原始数据，构建Things格式，通过任务通道发送给业务层处理
%% 业务层会通过钩子机制调用协议层（modbus_rtu）进行数据解码
handle_info({tcp, Buff}, #tcp{register = true, state = #state{id = ChannelId, devaddr = DtuAddr, product = ProductId, env = Env} = State} = TCPState) ->
    % 获取实际设备地址（处理DtuAddr为空的情况）
    ActualDtuAddr = get_actual_dtu_addr(DtuAddr, Env),
    
    % 获取产品ID（处理从Products列表查找的情况）
    ActualProductId = get_actual_product_id(ProductId, DtuAddr, Env),
    
    % 记录接收到的数据
    HexBuff = dgiot_utils:binary_to_hex(Buff),
    ?LOG(debug, "Received data (hex): ~p", [HexBuff]),
    ?LOG(debug, "ActualProductId: ~p, ActualDtuAddr: ~p", [ActualProductId, ActualDtuAddr]),
    dgiot_bridge:send_log(ChannelId, ActualProductId, ActualDtuAddr, "DTU ~p received data: ~p", [ActualDtuAddr, HexBuff]),
    dgiot_device:save_log(ActualProductId, ActualDtuAddr, HexBuff, <<"tcp_receive">>),
    ?LOG(debug, "数据接收: product_id=~p, dtu_addr=~p, data_length=~p", [ActualProductId, ActualDtuAddr, byte_size(Buff)]),
    
    % 构建Things格式（根据不同的env内容）
    Things = build_data_things(Buff, ActualProductId, ActualDtuAddr, ChannelId, Env),
    ?LOG(debug, "Built Things keys: ~p", [maps:keys(Things)]),
    ?LOG(debug, "Built Things details: ~p", [Things]),
    
    % 发送数据到任务通道
    send_to_task_channel(ChannelId, ActualProductId, ActualDtuAddr, Things, Env),
    
    % 更新状态（清理env）
    NewState = case Env of
        #{product := _, pn := _, di := _} -> 
            ?LOG(debug, "Clearing env after processing", []),
            State#state{env = <<>>};
        _ -> 
            ?LOG(debug, "Keeping env unchanged", []),
            State
    end,
    
    {noreply, TCPState#tcp{buff = <<>>, state = NewState}};

%% 处理MQTT消息
handle_info({deliver, _, Msg}, #tcp{state = #state{id = ChannelId}} = TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    Topic = dgiot_mqtt:get_topic(Msg),
    
    case binary:split(Topic, <<$/>>, [global, trim]) of
        [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"profile">>] ->
            handle_profile_message(ProductId, DevAddr, Payload, TCPState, ChannelId);
        [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"properties">>] ->
            handle_properties_message(ProductId, DevAddr, Payload, TCPState, ChannelId);
        [<<"$dg">>, <<"device">>, ProductId, DevAddr, <<"debug">>] ->
            handle_debug_message(ProductId, DevAddr, Payload, TCPState, ChannelId);
        _ ->
            {noreply, TCPState}
    end;

%% 处理其他消息
handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, #tcp{clientid = DeviceId, state = #state{devaddr = DtuAddr}} = _TCPState) ->
    ?LOG(info, "Terminating connection, DeviceId: ~p, DtuAddr: ~p", [DeviceId, DtuAddr]),
    ?LOG(debug, "连接终止: device_id=~p, dtu_addr=~p, reason=~p", [DeviceId, DtuAddr, _Reason]),
    
    case dgiot_device:get_productid(DeviceId) of
        not_find ->
            dgiot_task:del_pnque(DeviceId),
            ok;
        ProductId ->
            Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
            dgiot_task:del_pnque(DeviceId),
            dgiot_device:save_log(ProductId, DtuAddr, DtuAddr, <<"offline">>),
            dgiot_client:stop(Taskchannel, DeviceId)
    end,
    ok;

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.

%% =======================
%% 辅助函数
%% =======================

%% @doc 获取实际设备地址（处理DtuAddr为空的情况）
%% @spec get_actual_dtu_addr(DtuAddr, Env) -> binary()
get_actual_dtu_addr(DtuAddr, Env) ->
    case DtuAddr of
        <<>> ->
            case Env of
                #{port := Port} ->
                    % 从端口信息构建设备地址
                    <<"port_", (integer_to_binary(Port))/binary>>;
                _ ->
                    % 如果也没有端口信息，使用默认地址
                    <<"unknown_device">>
            end;
        _ ->
            DtuAddr
    end.

%% @doc 获取实际产品ID（处理从Products列表查找的情况）
%% @spec get_actual_product_id(ProductId, DtuAddr, Env) -> binary()
get_actual_product_id(ProductId, DtuAddr, Env) ->
    case ProductId of
        undefined ->
            % 从Products列表中查找产品
            case Env of
                #{products := Products} when is_list(Products) ->
                    DtuHeader = DtuAddr,
                    ProductItem = modbus_util:find_product(DtuHeader, Products),
            case ProductItem of
                not_found ->
                    ?LOG(warning, "Product not found for DtuHeader: ~p", [DtuHeader]),
                    <<"unknown_product">>;
                #{<<"productId">> := FoundProductId} ->
                    ?LOG(debug, "Found product: ~p for DtuHeader: ~p", [FoundProductId, DtuHeader]),
                    FoundProductId
            end;
                _ ->
                    <<"unknown_product">>
            end;
        _ ->
            ProductId
    end.

%% @doc 构建数据Things结构（统一封装函数）
%% @spec build_data_things(Buff, ProductId, DtuAddr, ChannelId, Env) -> map()
build_data_things(Buff, ProductId, DtuAddr, ChannelId, Env) ->
    case Env of
        #{product := EnvProductId, pn := Pn, di := Di} ->
            % 环境中有product, pn, di信息
            build_raw_data_things(Buff, EnvProductId, DtuAddr, ChannelId, #{<<"pn">> => Pn, <<"di">> => Di});
        #{port := _Port} ->
            % 环境中有端口信息
            build_raw_data_things(Buff, ProductId, DtuAddr, ChannelId, #{<<"env">> => Env});
        #{} ->
            % env是空map，主动上报数据
            build_raw_data_things(Buff, ProductId, DtuAddr, ChannelId, #{<<"report_type">> => <<"active_report">>});
        <<>> ->
            % 兼容旧代码：env是空二进制，主动上报数据
            build_raw_data_things(Buff, ProductId, DtuAddr, ChannelId, #{<<"report_type">> => <<"active_report">>});
        _ ->
            % 默认情况
            build_raw_data_things(Buff, ProductId, DtuAddr, ChannelId, #{})
    end.

%% @doc 发送数据到任务通道（统一封装函数）
%% @spec send_to_task_channel(ChannelId, ProductId, DtuAddr, Things, Env) -> ok
send_to_task_channel(ChannelId, ProductId, DtuAddr, Things, Env) ->
    % 发送聚合设备报告
    send_aggregated_device_report(ChannelId, ProductId, DtuAddr, Things, Env),
    
    % 如果Env为空（非注册、非响应数据），则认为是设备主动上报数据
    % 需要额外发送到任务通道进行实时处理
    case Env of
        <<>> ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
            NewTopic = <<"$dg/thing/", ProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
            
            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, 
                                 "Sending to task: ~p", [NewTopic]),
            
            Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
            dgiot_client:send(Taskchannel, DeviceId, NewTopic, Things);
        #{} ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
            NewTopic = <<"$dg/thing/", ProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
            
            dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, 
                                 "Sending to task: ~p", [NewTopic]),
            
            Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
            dgiot_client:send(Taskchannel, DeviceId, NewTopic, Things);
        _ ->
            ok
    end.

%% @doc 构建原始数据Things结构
%% 通讯层只负责构建原始数据包，不进行数据解码
%% @spec build_raw_data_things(Buff, ProductId, DtuAddr, ChannelId, Extra) -> Things
build_raw_data_things(Buff, ProductId, DtuAddr, ChannelId, Extra) ->
    BaseThings = #{
        <<"raw_data">> => Buff,
        <<"data_type">> => <<"modbus_rtu">>,
        <<"product_id">> => ProductId,
        <<"dtu_addr">> => DtuAddr,
        <<"channel_id">> => ChannelId
    },
    case Extra of
        #{<<"pn">> := Pn, <<"di">> := Di} ->
            BaseThings#{<<"pn">> => Pn, <<"di">> => Di};
        #{<<"env">> := Env} ->
            BaseThings#{<<"env">> => Env};
        #{<<"report_type">> := ReportType} ->
            BaseThings#{<<"report_type">> => ReportType};
        _ ->
            BaseThings
    end.

%% 发送聚合设备报告消息，支持父设备消息汇聚
%% 通讯层只负责消息路由，不进行数据解码或属性计算
send_aggregated_device_report(ChannelId, ProductId, DtuAddr, Things, _) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
    
    % 获取父设备信息
    ParentInfo = dgiot_device_cache:get_parent_info(DeviceId),
    ParentId = maps:get(deviceid, ParentInfo, <<"">>),
    ParentProductId = maps:get(productid, ParentInfo, <<"">>),
    ParentDevAddr = maps:get(devaddr, ParentInfo, <<"">>),
    
    ?LOG(debug, "Sending to Task Channel, ProductId: ~p, DtuAddr: ~p", [ProductId, DtuAddr]),
    ?LOG(debug, "Data to send keys: ~p", [maps:keys(Things)]),
    ?LOG(debug, "Data to send details: ~p", [Things]),
    
    % 发送子设备消息（直接转发Things，不进行属性计算）
    ChildTopic = <<"$dg/thing/", ProductId/binary, "/", DtuAddr/binary, "/properties/report">>,
    dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "Sending to task: ~p", [ChildTopic]),
    dgiot_device:save_log(ProductId, DtuAddr, Things, <<"reportProperty">>),
    Taskchannel = dgiot_product_channel:get_taskchannel(ProductId),
    ?LOG(debug, "Taskchannel: ~p, DeviceId: ~p", [Taskchannel, DeviceId]),
    case dgiot_client:send(Taskchannel, DeviceId, ChildTopic, Things) of
        ok -> ?LOG(debug, "Successfully sent to task channel", []);
        false -> ?LOG(error, "Failed to send to task channel: client not found or dead", []);
        {error, Reason} -> ?LOG(error, "Failed to send to task channel: ~p", [Reason])
    end,
    
    % 保存数据到TD库（通过dgiot_task模块）
    dgiot_task:save_td(ProductId, DtuAddr, Things, #{}),
    
    % 如果父设备存在，发送父设备消息
    case ParentId of
        <<"">> -> ok;
        _ ->
            ParentTopic = <<"$dg/thing/", ParentProductId/binary, "/", ParentDevAddr/binary, "/properties/report">>,
            dgiot_bridge:send_log(ChannelId, ParentProductId, ParentDevAddr, "Sending to parent task: ~p", [ParentTopic]),
            dgiot_device:save_log(ParentProductId, ParentDevAddr, Things, <<"reportProperty">>),
            ParentTaskchannel = dgiot_product_channel:get_taskchannel(ParentProductId),
            dgiot_client:send(ParentTaskchannel, ParentId, ParentTopic, Things)
    end,
    
    ok.


%% =======================
%% 消息处理辅助函数
%% =======================

handle_profile_message(ProductId, DevAddr, Payload, TCPState, _ChannelId) ->
    ProfilePayload = dgiot_device_profile:encode_profile(ProductId, dgiot_json:decode(Payload)),
    Payloads = modbus_rtu:set_params(ProfilePayload, ProductId, DevAddr),
    
    lists:foreach(fun(X) ->
        timer:sleep(100),
        dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(X), <<"device_operationlog">>),
        dgiot_tcp_server:send(TCPState, X)
    end, Payloads),
    {noreply, TCPState}.

handle_properties_message(ProductId, DevAddr, Payload, TCPState, ChannelId) ->
    handle_frame_message(ProductId, DevAddr, Payload, TCPState, ChannelId, <<"readProperty">>, "Channel sends data to DTU: ~p").

handle_debug_message(ProductId, DevAddr, Payload, TCPState, ChannelId) ->
    handle_frame_message(ProductId, DevAddr, Payload, TCPState, ChannelId, <<"debug">>, "Channel sends debug data to DTU: ~p").

%% @doc 处理帧消息通用函数
handle_frame_message(ProductId, DevAddr, Payload, TCPState, ChannelId, LogType, LogMsg) ->
    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
        #{<<"_dgiotTaskFreq">> := _Freq, <<"slaveid">> := _SlaveId, <<"address">> := _Address} = DataSource ->
            Data = modbus_rtu:to_frame(DataSource),
            dgiot_device:save_log(ProductId, DevAddr, dgiot_utils:binary_to_hex(Data), LogType),
            dgiot_bridge:send_log(ChannelId, ProductId, DevAddr, LogMsg, [DevAddr]),
            dgiot_tcp_server:send(TCPState, Data),
            {noreply, TCPState};
        _ ->
            {noreply, TCPState}
    end.


%% @doc 处理IP注册
handle_ip_registration(ChannelId, ProductId, DtuAddr, Dtutype, TCPState, State) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
    case dgiot_device:lookup(DeviceId) of
        {ok, _} ->
            dgiot_modbus:register_client(ChannelId, ProductId, DtuAddr, DtuAddr, Dtutype),
            {ok, TCPState#tcp{buff = <<>>, register = true, clientid = DeviceId, 
                             state = State#state{devaddr = DtuAddr, deviceId = DeviceId}}};
        _ ->
            case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"objectId">> := DeviceId, <<"product">> := #{<<"objectId">> := ProductId}}} ->
                    dgiot_modbus:register_client(ChannelId, ProductId, DtuAddr, DtuAddr, Dtutype),
                    {ok, TCPState#tcp{buff = <<>>, register = true, clientid = DeviceId, 
                                     state = State#state{devaddr = DtuAddr, deviceId = DeviceId}}};
                _ ->
                    {ok, TCPState}
            end
    end.

%% @doc 处理端口注册（使用服务器端口）
%% @spec handle_port_registration(ChannelId, Buff, Head, Dtutype, Port, TCPState, State) -> 
%%        {noreply, TCPState}
%% Port参数说明：这是服务器端口（通道配置中的端口）
%% 在RegisterByPort注册方式中，服务器端口作为设备地址的一部分
%% 注意：服务器端口是固定的配置端口（如20000），不是客户端连接端口
%% 设备地址 = 注册报文 + "-" + 服务器端口
%% 设计目的：使用固定端口作为设备标识的一部分，便于设备管理
handle_port_registration(ChannelId, Buff, Head, Dtutype, Port, TCPState, State) ->
    ?LOG(error, "RegisterByPort: Processing registration packet with server port ~p, ChannelId: ~p, Buff: ~p", 
         [Port, ChannelId, dgiot_utils:binary_to_hex(Buff)]),
    
    % 处理注册报文（使用服务器端口）
    case process_registration_packet(Buff, Head, Dtutype, Port) of
        {ok, ProductId, DeviceAddr, DeviceId} ->
            % 注册设备
            dgiot_modbus:register_client(ChannelId, ProductId, DeviceAddr, DeviceAddr, Dtutype),
            
            % 更新状态，标记为已注册
            {noreply, TCPState#tcp{
                buff = <<>>, 
                register = true, 
                clientid = DeviceId, 
                state = State#state{
                    devaddr = DeviceAddr, 
                    deviceId = DeviceId, 
                    product = ProductId
                }
            }};
        {error, Reason} ->
            ?LOG(error, "RegisterByPort: Registration failed: ~p, Head: ~p", 
                 [Reason, Head]),
            % 不匹配，保持未注册状态
            {noreply, TCPState#tcp{buff = <<>>}}
    end.

%% @doc 处理正则注册
%% @spec handle_regular_registration(ChannelId, Buff, Head, Dtutype, TCPState, State) -> 
%%        {noreply, TCPState}
handle_regular_registration(ChannelId, Buff, Head, Dtutype, TCPState, State) ->
    ?LOG(error, "RegisterByRegular: Processing registration packet, ChannelId: ~p, Buff: ~p", 
         [ChannelId, dgiot_utils:binary_to_hex(Buff)]),
    
    % 处理注册报文（正则注册不使用端口）
    case process_regular_registration_packet(Buff, Head, Dtutype) of
        {ok, ProductId, DeviceAddr, DeviceId} ->
            % 注册设备
            dgiot_modbus:register_client(ChannelId, ProductId, DeviceAddr, DeviceAddr, Dtutype),
            
            % 更新状态，标记为已注册
            {noreply, TCPState#tcp{
                buff = <<>>, 
                register = true, 
                clientid = DeviceId, 
                state = State#state{
                    devaddr = DeviceAddr, 
                    deviceId = DeviceId, 
                    product = ProductId
                }
            }};
        {error, Reason} ->
            ?LOG(error, "RegisterByRegular: Registration failed: ~p, Head: ~p", 
                 [Reason, Head]),
            % 不匹配，保持未注册状态
            {noreply, TCPState#tcp{buff = <<>>}}
    end.

%% @doc 处理注册报文（带服务器端口）
%% @spec process_registration_packet(Buff, Head, Dtutype, Port) -> 
%%        {ok, ProductId, DeviceAddr, DeviceId} | {error, Reason}
%% Port参数说明：这是服务器端口（通道配置中的端口）
%% 在RegisterByPort注册方式中，设备地址 = 注册报文 + "-" + 服务器端口
%% 设计目的：使用固定端口作为设备标识的一部分，便于设备管理
%% 示例：服务器端口20000 -> 设备地址 = "wrj_dm-zqy-20000"
process_registration_packet(Buff, Head, Dtutype, Port) ->
    % 首先检查是否是十六进制字符串，如果是则解码为ASCII字符串
    AsciiBuff = case dgiot_utils:is_hex_string(Buff) of
        true -> 
            case dgiot_utils:hex_to_binary(Buff) of
                {error, _} -> Buff;  % 解码失败，使用原值
                Decoded -> Decoded
            end;
        false -> Buff  % 不是十六进制字符串，直接使用
    end,
    
    % 将通配符模式转换为正则表达式
    RegexPattern = modbus_util:convert_pattern(Head),
    Regex = binary_to_list(RegexPattern),
    
    % 检查报文是否匹配正则表达式
    case re:run(binary_to_list(AsciiBuff), Regex) of
        {match, _} ->
            % 解析注册报文：按-分割，取第一部分进行产品匹配
            Productname = 
                case binary:split(AsciiBuff, <<"-">>, [global]) of
                    [Part | _] -> Part;
                    _ -> AsciiBuff
                end,
            
            % 使用第一部分进行产品匹配
            ProductId = modbus_util:get_product_id(Productname, Dtutype),

            % 生成设备地址：注册报文 + "-" + 服务器端口
            % 注意：这里的Port是服务器端口（通道配置中的固定端口）
            DeviceAddr = <<AsciiBuff/binary, "-", (integer_to_binary(Port))/binary>>,
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
            
            ?LOG(warning, "RegisterByPort: Generated device address: ~p, ProductId: ~p, ServerPort: ~p", 
                 [DeviceAddr, ProductId, Port]),
            {ok, ProductId, DeviceAddr, DeviceId};
        nomatch ->
            {error, <<"packet_not_match">>}
    end.

%% @doc 处理正则注册报文（不带端口）
%% @spec process_regular_registration_packet(Buff, Head, Dtutype) -> 
%%        {ok, ProductId, DeviceAddr, DeviceId} | {error, Reason}
process_regular_registration_packet(Buff, Head, Dtutype) ->
    % 首先检查是否是十六进制字符串，如果是则解码为ASCII字符串
    AsciiBuff = case dgiot_utils:is_hex_string(Buff) of
        true -> 
            case dgiot_utils:hex_to_binary(Buff) of
                {error, _} -> Buff;  % 解码失败，使用原值
                Decoded -> Decoded
            end;
        false -> Buff  % 不是十六进制字符串，直接使用
    end,
    
    % 将通配符模式转换为正则表达式
    RegexPattern = modbus_util:convert_pattern(Head),
    Regex = binary_to_list(RegexPattern),
    
    % 检查报文是否匹配正则表达式
    case re:run(binary_to_list(AsciiBuff), Regex) of
        {match, _} ->
            % 解析注册报文：按-分割，取第一部分进行产品匹配
            Productname = 
                case binary:split(AsciiBuff, <<"-">>, [global]) of
                    [Part | _] -> Part;
                    _ -> AsciiBuff
                end,
            
            % 使用第一部分进行产品匹配
            ProductId = modbus_util:get_product_id(Productname, Dtutype),

            % 生成设备地址：直接使用注册报文
            DeviceAddr = AsciiBuff,
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
            
            ?LOG(warning, "RegisterByRegular: Generated device address: ~p, ProductId: ~p", 
                 [DeviceAddr, ProductId]),
            {ok, ProductId, DeviceAddr, DeviceId};
        nomatch ->
            {error, <<"packet_not_match">>}
    end.
