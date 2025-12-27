# Modbus RTU 业务层处理方案

## 问题分析

根据七层架构设计，各层职责如下：
1. **通讯层**: 只转发原始数据，不进行解码
2. **协议层**: 负责Modbus RTU协议解析
3. **业务层**: 负责数据解码、属性计算、业务逻辑

当前问题：
- 通讯层正确地将原始数据转发给业务层
- 业务层目前只能处理已经解析后的数据，无法处理原始Modbus数据
- 缺少从原始数据到业务数据的转换机制

## 解决方案

### 方案一：在业务层添加Modbus数据解析钩子

#### 1. 修改业务层数据接收逻辑
在`dgiot_task.erl`的`save_td/4`函数中，添加对原始Modbus数据的处理逻辑：

```erlang
save_td(ProductId, DevAddr, Ack, _AppData) ->
    Topic = <<"$dg/thing/", ProductId/binary, "/", DevAddr/binary, "/properties/report">>,
    dgiot_mqttc_channel:send(ProductId, DevAddr, Topic, Ack),
    case maps:size(Ack) of
        0 ->
            #{};
        _ ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            Interval = dgiot_product:get_interval(ProductId),
            
            %% 检查是否为原始Modbus数据
            ParsedData = case maps:get(<<"raw_data">>, Ack, undefined) of
                undefined ->
                    %% 不是原始数据，直接使用
                    Ack;
                RawData ->
                    %% 是原始Modbus数据，调用协议层解析
                    parse_modbus_raw_data(ProductId, DevAddr, RawData, Ack)
            end,
            
            %% 是否有缓存
            CacheData = dgiot_task:merge_cache_data(DeviceId, ParsedData, Interval),
            %% 获取物模型
            Props = dgiot_task:get_props(ProductId),
            %% 计算上报值
            Collection = dgiot_task:get_collection(ProductId, [], CacheData, Props),
            %% 计算计算值
            AllData = dgiot_task:get_calculated(ProductId, DevAddr, Collection, Props),
            %% 过滤存储值
            Storage = dgiot_task:get_storage(AllData, Props),
            save_cache_data(DeviceId, CacheData),
            dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage, Interval)
    end.
```

#### 2. 添加原始Modbus数据解析函数

```erlang
%% @doc 解析原始Modbus数据
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param RawData 原始二进制数据
%% @param Metadata 元数据（包含data_type等信息）
%% @return 解析后的数据映射
parse_modbus_raw_data(ProductId, DevAddr, RawData, Metadata) ->
    DataType = maps:get(<<"data_type">>, Metadata, <<"modbus_rtu">>),
    
    case DataType of
        <<"modbus_rtu">> ->
            %% 调用Modbus RTU协议解析
            parse_modbus_rtu_data(ProductId, DevAddr, RawData, Metadata);
        _ ->
            %% 其他协议类型，直接返回原始数据
            #{<<"raw_data">> => RawData}
    end.

%% @doc 解析Modbus RTU数据
parse_modbus_rtu_data(ProductId, DevAddr, RawData, Metadata) ->
    %% 获取产品配置
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            %% 调用协议层解析数据
            case modbus_rtu:parse_frame(RawData, #{}, #{
                <<"dtuproduct">> => ProductId,
                <<"dtuaddr">> => DevAddr,
                <<"slaveId">> => get_slave_id_from_metadata(Metadata),
                <<"address">> => get_address_from_metadata(Metadata)
            }) of
                {Rest, ParsedData} when is_map(ParsedData) ->
                    %% 成功解析，返回解析后的数据
                    ParsedData;
                _ ->
                    %% 解析失败，返回原始数据
                    #{<<"raw_data">> => RawData, <<"parse_error">> => true}
            end;
        _ ->
            #{<<"raw_data">> => RawData, <<"product_not_found">> => true}
    end.
```

#### 3. 在Modbus模块中添加数据解析钩子

修改`dgiot_modbus_app.erl`，添加更多钩子：

```erlang
start_hook() ->
    %% 注册数据源钩子
    dgiot_hook:add(one_for_one, {?DGIOT_DATASOURCE, <<"MODBUSRTU">>}, fun modbus_rtu:get_datasource/1),
    
    %% 注册原始数据解析钩子
    dgiot_hook:add(one_for_one, {?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, fun modbus_rtu:parse_raw_data/3),
    
    ok.

stop_hook() ->
    dgiot_hook:remove({?DGIOT_DATASOURCE, <<"MODBUSRTU">>}),
    dgiot_hook:remove({?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}),
    ok.
```

#### 4. 在`modbus_rtu.erl`中添加原始数据解析函数

```erlang
%% @doc 解析原始Modbus RTU数据
%% @param ProductId 产品ID
%% @param DevAddr 设备地址
%% @param RawData 原始二进制数据
%% @return 解析后的数据映射
parse_raw_data(ProductId, DevAddr, RawData) ->
    %% 解析数据帧
    case parse_frame(RawData, #{}, #{
        <<"dtuproduct">> => ProductId,
        <<"dtuaddr">> => DevAddr
    }) of
        {_Rest, ParsedData} when is_map(ParsedData) ->
            %% 成功解析，转换为业务层需要的格式
            convert_to_business_format(ProductId, DevAddr, ParsedData);
        Error ->
            ?LOG(error, "Failed to parse Modbus RTU data: ~p", [Error]),
            #{<<"raw_data">> => RawData, <<"parse_error">> => true}
    end.

%% @doc 将协议层数据转换为业务层格式
convert_to_business_format(ProductId, DevAddr, ParsedData) ->
    %% 获取产品属性配置
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            %% 根据属性配置转换数据
            lists:foldl(fun(Prop, Acc) ->
                case Prop of
                    #{<<"identifier">> := Identifier, <<"dataSource">> := DataSource} ->
                        case extract_value_from_parsed_data(Identifier, DataSource, ParsedData) of
                            {ok, Value} ->
                                Acc#{Identifier => Value};
                            error ->
                                Acc
                        end;
                    _ ->
                        Acc
                end
            end, #{}, Props);
        _ ->
            ParsedData
    end.
```

### 方案二：修改通讯层，在转发前调用协议层解析

#### 1. 修改通讯层数据转发逻辑

在`dgiot_modbusrtu_tcp.erl`中，修改数据转发逻辑：

```erlang
%% 处理已注册设备的数据
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = DtuAddr, 
                                            env = #{product := ProductId, pn := Pn, di := Di}} = State} = TCPState) ->
    HexBuff = dgiot_utils:binary_to_hex(Buff),
    dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr,  "DTU ~p received data: ~p", [DtuAddr, HexBuff]),
    dgiot_device:save_log(ProductId, DtuAddr, HexBuff, <<"tcp_receive">>),
    
    %% 方案二：在通讯层调用协议层解析
    ParsedData = case parse_modbus_data_in_communication_layer(ProductId, DtuAddr, Buff) of
        {ok, Data} ->
            %% 解析成功，发送解析后的数据
            Data;
        {error, Reason} ->
            ?LOG(warning, "Failed to parse Modbus data: ~p", [Reason]),
            %% 解析失败，发送原始数据
            #{
                <<"raw_data">> => Buff,
                <<"data_type">> => <<"modbus_rtu">>,
                <<"product_id">> => ProductId,
                <<"dtu_addr">> => DtuAddr,
                <<"channel_id">> => ChannelId,
                <<"parse_error">> => true
            }
    end,
    
    %% 发送聚合设备报告
    send_aggregated_device_report(ChannelId, ProductId, DtuAddr, ParsedData, ProductId),
    
    {noreply, TCPState#tcp{buff = <<>>, state = State#state{env = <<>>}}}.

%% @doc 在通讯层解析Modbus数据
parse_modbus_data_in_communication_layer(ProductId, DtuAddr, Buff) ->
    %% 注意：这违反了七层架构原则，通讯层不应该进行数据解析
    %% 这里仅作为备选方案
    try
        %% 调用协议层解析
        case modbus_rtu:parse_frame(Buff, #{}, #{
            <<"dtuproduct">> => ProductId,
            <<"dtuaddr">> => DtuAddr
        }) of
            {_Rest, ParsedData} when is_map(ParsedData) ->
                {ok, ParsedData};
            Error ->
                {error, Error}
        end
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.
```

### 推荐方案：方案一（业务层钩子机制）

**优点**：
1. 符合七层架构设计原则
2. 各层职责清晰
3. 可扩展性强，支持其他协议
4. 代码结构清晰，易于维护

**缺点**：
1. 需要修改业务层代码
2. 增加了一层数据转换

## 实施步骤

### 阶段一：协议层增强（1天）
1. 在`modbus_rtu.erl`中添加`parse_raw_data/3`函数
2. 添加`convert_to_business_format/3`函数
3. 完善错误处理和日志记录

### 阶段二：业务层修改（1天）
1. 在`dgiot_task.erl`中添加原始数据解析逻辑
2. 修改`save_td/4`函数支持原始数据
3. 添加`parse_modbus_raw_data/4`函数

### 阶段三：钩子注册（0.5天）
1. 修改`dgiot_modbus_app.erl`注册新钩子
2. 测试钩子调用机制

### 阶段四：测试验证（1.5天）
1. 单元测试：测试各个函数
2. 集成测试：测试完整数据流
3. 端到端测试：从设备到API完整流程

## 代码示例

### 完整的业务层修改

```erlang
%% 在dgiot_task.erl中添加以下函数

%% @doc 处理原始Modbus数据
handle_raw_modbus_data(ProductId, DevAddr, RawData, Metadata) ->
    %% 调用钩子解析原始数据
    case dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, [ProductId, DevAddr, RawData]) of
        {ok, [ParsedData | _]} when is_map(ParsedData) ->
            ParsedData;
        _ ->
            %% 钩子解析失败，尝试直接解析
            try_direct_modbus_parse(ProductId, DevAddr, RawData, Metadata)
    end.

%% @doc 尝试直接解析Modbus数据
try_direct_modbus_parse(ProductId, DevAddr, RawData, Metadata) ->
    %% 这里可以添加直接解析逻辑
    %% 或者返回原始数据让上层处理
    #{
        <<"raw_data">> => RawData,
        <<"product_id">> => ProductId,
        <<"dev_addr">> => DevAddr,
        <<"metadata">> => Metadata
    }.
```

### 完整的协议层修改

```erlang
%% 在modbus_rtu.erl中添加以下函数

%% @doc 解析原始数据（钩子函数）
parse_raw_data(ProductId, DevAddr, RawData) ->
    ?LOG(debug, "Parsing raw Modbus RTU data, ProductId: ~p, DevAddr: ~p", [ProductId, DevAddr]),
    
    %% 解析数据帧
    case parse_frame(RawData, #{}, #{
        <<"dtuproduct">> => ProductId,
        <<"dtuaddr">> => DevAddr,
        <<"slaveId">> => extract_slave_id(RawData),
        <<"address">> => 0  %% 默认地址，可以从元数据中获取
    }) of
        {Rest, ParsedData} when is_map(ParsedData), map_size(ParsedData) > 0 ->
            ?LOG(debug, "Successfully parsed Modbus data: ~p", [ParsedData]),
            
            %% 转换为业务层格式
            BusinessData = convert_to_business_format(ProductId, DevAddr, ParsedData),
            
            %% 合并原始数据（用于调试）
            BusinessData#{
                <<"_raw_data">> => dgiot_utils:binary_to_hex(RawData),
                <<"_rest_data">> => dgiot_utils:binary_to_hex(Rest)
            };
            
        {Rest, _} when byte_size(Rest) > 0 ->
            ?LOG(warning, "Partial parse, rest data: ~p", [dgiot_utils:binary_to_hex(Rest)]),
            #{<<"raw_data">> => dgiot_utils:binary_to_hex(RawData), <<"partial_parse">> => true};
            
        Error ->
            ?LOG(error, "Failed to parse Modbus data: ~p", [Error]),
            #{<<"raw_data">> => dgiot_utils:binary_to_hex(RawData), <<"parse_error">> => true}
    end.

%% @doc 从原始数据中提取从机ID
extract_slave_id(<<SlaveId:8, _/binary>>) ->
    SlaveId;
extract_slave_id(_) ->
    1.  %% 默认从机ID
```

## 总结

采用**方案一（业务层钩子机制）**是最佳选择，因为它：

1. **符合架构原则**：各层职责清晰，通讯层
