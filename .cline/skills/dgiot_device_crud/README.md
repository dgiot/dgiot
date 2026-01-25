# DGIOT设备增删查改技巧总结

## 概述

本技能总结了DGIOT系统中设备增删查改（CRUD）的最佳实践和技巧，基于对`dgiot_parse`模块和`dgiot_device`模块的深入分析。

## 核心模块分析

### 1. dgiot_parse模块 - 基础数据操作

#### create_object函数分析

```erlang
%% 创建对象
create_object(Class, Map) ->
    create_object(?DEFAULT, Class, Map).

create_object(Name, Class, Map) ->
    create_object(Name, Class, Map, [], [{from, master}]).

create_object(Class, Map, Header, Options) ->
    create_object(?DEFAULT, Class, Map, Header, Options).

create_object(Name, Class, #{<<"objectId">> := _ObjectId} = Map, Header, Options) ->
    Path = <<"/classes/", Class/binary>>,
    request_rest(Name, 'POST', Header, Path, Map, Options);
create_object(Name, Class, Map, Header, Options) ->
    Path = <<"/classes/", Class/binary>>,
    request_rest(Name, 'POST', Header, Path, dgiot_parse_id:get_objectid(Class, Map), Options).
```

**关键技巧：**
1. **多层函数重载**：提供不同参数组合的接口，方便调用
2. **自动生成objectId**：使用`dgiot_parse_id:get_objectid/2`自动生成对象ID
3. **默认参数处理**：使用`?DEFAULT`作为默认连接名称
4. **请求路由**：自动构建REST API路径 `/classes/{ClassName}`

#### 其他核心函数

```erlang
%% 获取对象
get_object(Name, Class, ObjectId, Header, Options) ->
    Path = <<"/classes/", Class/binary, "/", ObjectId/binary>>,
    request_rest(Name, 'GET', Header, Path, #{}, Options).

%% 更新对象
update_object(Name, Class, ObjectId, Map, Header, Options) ->
    Path = <<"/classes/", Class/binary, "/", ObjectId/binary>>,
    request_rest(Name, 'PUT', Header, Path, Map, Options).

%% 删除对象
del_object(Name, Class, ObjectId, Header, Options) ->
    Path = <<"/classes/", Class/binary, "/", ObjectId/binary>>,
    request_rest(Name, 'DELETE', Header, Path, #{}, Options).

%% 查询对象
query_object(Name, Class, Args, Header, Options) ->
    Path = <<"/classes/", Class/binary>>,
    request_rest(Name, 'GET', Header, Path, Args, Options).
```

### 2. dgiot_device模块 - 设备管理封装

#### 创建设备的两种方式

**方式1：完整设备数据**
```erlang
create_device(Device) ->
    case dgiot_device_manager:create_device(Device) of
        {ok, CreatedDevice} ->
            {ok, CreatedDevice};
        {error, Reason} ->
            {error, Reason}
    end.
```

**方式2：简化参数**
```erlang
create_device(ProductId, DeviceAddr, Ip) ->
    case dgiot_device_manager:create_device(ProductId, DeviceAddr, Ip) of
        ok -> ok;
        pass -> pass;
        Result -> Result
    end.
```

#### 设备查询
```erlang
%% 通过设备ID查询
lookup(DeviceId) ->
    dgiot_device_cache:lookup(DeviceId).

%% 通过产品和设备地址查询
lookup(ProductId, DevAddr) ->
    dgiot_device_cache:lookup(ProductId, DevAddr).

%% 获取设备信息
get(ProductId, DevAddr) ->
    dgiot_device_manager:get_device(ProductId, DevAddr).
```

#### 设备更新
```erlang
%% 更新设备数据
put(Device) ->
    dgiot_device_put:put(Device).

%% 保存设备
save(Device) ->
    dgiot_device_cache:save(Device).
```

#### 设备删除
```erlang
%% 通过设备ID删除
delete(DeviceId) ->
    case dgiot_device_manager:delete_device(DeviceId) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% 通过产品和设备地址删除
delete(ProductId, DevAddr) ->
    case dgiot_device_manager:delete_device(ProductId, DevAddr) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.
```

## 最佳实践技巧

### 1. 设备创建技巧

#### 技巧1：设备ID生成
```erlang
%% 使用标准方式生成设备ID
DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr)
```

#### 技巧2：完整设备数据结构
```erlang
Device = #{
    <<"devaddr">> => DeviceAddr,
    <<"productId">> => ProductId,
    <<"deviceType">> => DeviceType,
    <<"name">> => DeviceName,
    <<"status">> => <<"offline">>,
    <<"ip">> => IP,
    <<"port">> => Port,
    <<"description">> => Description,
    <<"is_virtual">> => true,
    <<"createdAt">> => dgiot_datetime:now_secs(),
    <<"updatedAt">> => dgiot_datetime:now_secs(),
    <<"location">> => #{
        <<"workstation">> => DeviceAddr,
        <<"production_line">> => <<"生产线">>,
        <<"factory">> => <<"工厂">>
    },
    <<"attributes">> => #{
        <<"workstation_type">> => DeviceType,
        <<"workstation_addr">> => DeviceAddr,
        <<"plc_ip">> => IP,
        <<"has_plc">> => HasPLC,
        <<"test_capabilities">> => TestCapabilities
    }
}
```

#### 技巧3：设备存在性检查
```erlang
%% 先检查设备是否已存在
case dgiot_device:lookup(DeviceId) of
    {ok, _Device} ->
        %% 设备已存在，更新状态
        update_device_status(DeviceId, <<"online">>);
    _ ->
        %% 设备不存在，创建新设备
        create_new_device(DeviceProps)
end.
```

### 2. 设备查询技巧

#### 技巧1：使用缓存提高性能
```erlang
%% dgiot_device_cache模块提供缓存功能
dgiot_device_cache:lookup(DeviceId)  %% 优先从缓存读取
dgiot_device_cache:save(Device)      %% 保存到缓存
```

#### 技巧2：批量查询优化
```erlang
%% 使用query_object进行复杂查询
Query = #{
    <<"where">> => #{
        <<"status">> => <<"online">>,
        <<"product">> => ProductId
    },
    <<"limit">> => 100,
    <<"skip">> => 0,
    <<"order">> => <<"-createdAt">>
},
dgiot_parse:query_object(<<"Device">>, Query)
```

### 3. 设备更新技巧

#### 技巧1：部分更新
```erlang
%% 只更新需要的字段
Updates = #{
    <<"status">> => NewStatus,
    <<"updatedAt">> => dgiot_datetime:now_secs()
},
dgiot_device_manager:update_device(DeviceId, Updates)
```

#### 技巧2：状态管理
```erlang
%% 使用专门的函数管理设备状态
dgiot_device:online(DeviceId)     %% 设置在线
dgiot_device:offline(DeviceId)    %% 设置离线
dgiot_device:enable(DeviceId)     %% 启用设备
dgiot_device:disable(DeviceId)    %% 禁用设备
```

### 4. 设备删除技巧

#### 技巧1：软删除与硬删除
```erlang
%% 软删除：更新状态为删除
SoftDelete = #{
    <<"status">> => <<"deleted">>,
    <<"deletedAt">> => dgiot_datetime:now_secs()
},
dgiot_device_manager:update_device(DeviceId, SoftDelete)

%% 硬删除：从数据库移除
dgiot_device_manager:delete_device(DeviceId)
```

#### 技巧2：级联删除
```erlang
%% 删除设备时同时删除相关数据
delete_device_with_relations(DeviceId) ->
    %% 1. 删除设备影子数据
    delete_device_shadow(DeviceId),
    
    %% 2. 删除设备日志
    delete_device_logs(DeviceId),
    
    %% 3. 删除设备配置
    delete_device_profile(DeviceId),
    
    %% 4. 删除主设备记录
    dgiot_device_manager:delete_device(DeviceId).
```

### 5. 错误处理技巧

#### 技巧1：统一错误处理
```erlang
handle_device_operation(Operation, Args) ->
    try
        case Operation of
            create -> dgiot_device:create_device(Args);
            read -> dgiot_device:lookup(Args);
            update -> dgiot_device:put(Args);
            delete -> dgiot_device:delete(Args)
        end
    catch
        _:Error ->
            ?LOG(error, "Device operation failed: ~p", [Error]),
            {error, Error}
    end.
```

#### 技巧2：验证设备数据
```erlang
validate_device_data(Device) ->
    RequiredFields = [<<"product">>, <<"devaddr">>, <<"name">>],
    validate_required_fields(Device, RequiredFields).

validate_required_fields(Device, [Field | Rest]) ->
    case maps:is_key(Field, Device) of
        true -> validate_required_fields(Device, Rest);
        false -> {error, {missing_field, Field}}
    end;
validate_required_fields(_Device, []) ->
    ok.
```

## 实际应用示例

### 示例1：创建设备工作流
```erlang
create_workstation_device(ProductId, DeviceType, DeviceAddr, DeviceName, IP, Description) ->
    %% 1. 生成设备ID
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    
    %% 2. 检查设备是否已存在
    case dgiot_device:lookup(DeviceId) of
        {ok, _Device} ->
            ?LOG(info, "设备已存在: ~s (~s)", [DeviceName, DeviceAddr]),
            {ok, DeviceId};
        _ ->
            %% 3. 创建设备属性
            DeviceProps = #{
                <<"devaddr">> => DeviceAddr,
                <<"productId">> => ProductId,
                <<"deviceType">> => DeviceType,
                <<"name">> => DeviceName,
                <<"status">> => <<"offline">>,
                <<"ip">> => IP,
                <<"description">> => Description,
                <<"createdAt">> => dgiot_datetime:now_secs(),
                <<"updatedAt">> => dgiot_datetime:now_secs()
            },
            
            %% 4. 创建设备
            case dgiot_device:create_device(DeviceProps) of
                {ok, _} ->
                    ?LOG(info, "创建成功: ~s (~s)", [DeviceName, DeviceAddr]),
                    
                    %% 5. 创建设备影子
                    create_device_shadow(DeviceId, DeviceType),
                    
                    {ok, DeviceId};
                {error, Reason} ->
                    ?LOG(error, "创建失败: ~s (~s) - 原因: ~p", [DeviceName, DeviceAddr, Reason]),
                    {error, Reason}
            end
    end.
```

### 示例2：批量创建设备
```erlang
batch_create_devices(ProductId, DeviceList) ->
    Results = lists:map(
        fun({DeviceType, DeviceAddr, DeviceName, IP, Desc}) ->
            create_workstation_device(ProductId, DeviceType, DeviceAddr, DeviceName, IP, Desc)
        end,
        DeviceList
    ),
    
    %% 统计结果
    {SuccessCount, ErrorCount} = lists:foldl(
        fun
            ({ok, _}, {S, E}) -> {S + 1, E};
            ({error, _}, {S, E}) -> {S, E + 1}
        end,
        {0, 0},
        Results
    ),
    
    ?LOG(info, "批量创建设备完成: 成功 ~p 个, 失败 ~p 个", [SuccessCount, ErrorCount]),
    {ok, #{success => SuccessCount, error => ErrorCount, results => Results}}.
```

### 示例3：设备状态监控
```erlang
monitor_device_status(DeviceId) ->
    %% 1. 获取设备当前状态
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"status">> := Status, <<"updatedAt">> := UpdatedAt}} ->
            %% 2. 检查是否超时
            Now = dgiot_datetime:now_secs(),
            TimeDiff = Now - UpdatedAt,
            
            if
                TimeDiff > 300 ->  %% 5分钟无更新
                    dgiot_device:offline(DeviceId),
                    {offline, timeout};
                Status == <<"online">> ->
                    {online, normal};
                true ->
                    {Status, normal}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

## 性能优化建议

### 1. 缓存策略
- 使用`dgiot_device_cache`模块缓存热点设备数据
- 设置合理的缓存过期时间
- 批量操作时使用缓存预热

### 2. 批量操作
- 使用`dgiot_parse:batch/1`进行批量数据库操作
- 批量创建设备时使用事务保证一致性
- 合理设置批量操作的大小（建议100-500条）

### 3. 异步处理
- 非关键操作使用异步处理
- 设备日志保存使用异步队列
- 状态更新使用消息队列解耦

### 4. 索引优化
- 为常用查询字段创建索引
- 复合索引优化多条件查询
- 定期分析查询性能

## 调试技巧

### 1. 日志记录
```erlang
%% 在关键操作处添加详细日志
?LOG(debug, "开始创建设备: ProductId=~p, DeviceAddr=~p", [ProductId, DeviceAddr]),
?LOG(info, "设备创建成功: DeviceId=~p", [DeviceId]),
?LOG(error, "设备创建失败: Reason=~p", [Reason])
```

### 2. 错误追踪
```erlang
handle_device_error(Error) ->
    case Error of
        {error, {missing_field, Field}} ->
            ?LOG(error, "缺少必需字段: ~p", [Field]);
        {error, device_not_found} ->
            ?LOG(warning, "设备不存在");
        {error, Reason} ->
            ?LOG(error, "未知错误: ~p", [Reason])
    end.
```

### 3. 性能监控
```erlang
monitor_device_operations() ->
    %% 监控设备操作性能
    StartTime = erlang:monotonic_time(),
    
    %% 执行设备操作
    Result = perform_device_operation(),
    
    EndTime = erlang:monotonic_time(),
    Duration = erlang:convert_time_unit(EndTime - StartTime, native, millisecond),
    
    ?LOG(info, "设备操作耗时: ~p ms", [Duration]),
    Result.
```

## 总结

DGIOT设备增删查改的核心技巧包括：

1. **标准化接口**：使用统一的函数接口进行设备操作
2. **缓存优化**：合理使用缓存提高查询性能
3. **错误处理**：完善的错误处理和日志记录
4. **批量操作**：支持批量操作提高效率
5. **状态管理**：专门的状态管理函数
6. **数据验证**：严格的数据验证机制

通过掌握这些技巧，可以高效、稳定地进行DGIOT设备管理，提高系统性能和可靠性。
