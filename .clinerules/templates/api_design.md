# API设计模板

## 概述

本模板用于指导API的设计和实现，确保API符合DG-IoT平台的规范和最佳实践。

## 1. API设计原则

### 1.1 RESTful API设计
- **资源导向**：使用名词表示资源，动词表示操作
- **HTTP方法**：GET（查询）、POST（创建）、PUT（更新）、DELETE（删除）
- **状态码**：使用标准的HTTP状态码
- **版本控制**：在URL中包含API版本（如 `/api/v1/`）

### 1.2 数据格式
- **请求/响应格式**：JSON
- **日期时间**：ISO 8601格式（如 `2023-01-01T00:00:00Z`）
- **分页**：使用 `limit` 和 `offset` 参数
- **排序**：使用 `sort` 参数（如 `sort=created_at:desc`）

## 2. API端点设计

### 2.1 设备管理API
```erlang
%% 设备相关API端点
-define(API_DEVICES, "/api/v1/devices").
-define(API_DEVICE_BY_ID, "/api/v1/devices/:id").
-define(API_DEVICE_STATUS, "/api/v1/devices/:id/status").
-define(API_DEVICE_COMMAND, "/api/v1/devices/:id/command").
```

### 2.2 数据查询API
```erlang
%% 数据查询API端点
-define(API_DATA_QUERY, "/api/v1/data/query").
-define(API_DATA_HISTORY, "/api/v1/data/history").
-define(API_DATA_REALTIME, "/api/v1/data/realtime").
-define(API_DATA_AGGREGATE, "/api/v1/data/aggregate").
```

## 3. API实现模板

### 3.1 控制器模块模板
```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_<module_name>_controller 模块 - API控制器
%%%
%%% 处理HTTP请求，调用相应的服务模块
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_<module_name>_controller).

%% API
-export([handle_request/3]).

%% 内部函数
-export([]).

-include("dgiot_<module_name>.hrl").
-include_lib("dgiot/include/dgiot.hrl").

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 处理HTTP请求
%% @spec handle_request(Method, Path, Req) -> {ok, Response} | {error, Reason}
handle_request('GET', <<"/api/v1/devices">>, Req) ->
    % 查询设备列表
    QueryParams = cowboy_req:parse_qs(Req),
    Limit = proplists:get_value(<<"limit">>, QueryParams, <<"100">>),
    Offset = proplists:get_value(<<"offset">>, QueryParams, <<"0">>),
    
    case dgiot_device:get_devices(binary_to_integer(Limit), binary_to_integer(Offset)) of
        {ok, Devices} ->
            Response = #{
                code => 200,
                message => <<"success">>,
                data => Devices,
                total => length(Devices)
            },
            {ok, Response};
        {error, Reason} ->
            {error, Reason}
    end;

handle_request('GET', <<"/api/v1/devices/", DeviceId/binary>>, Req) ->
    % 查询单个设备
    case dgiot_device:get_device(DeviceId) of
        {ok, Device} ->
            Response = #{
                code => 200,
                message => <<"success">>,
                data => Device
            },
            {ok, Response};
        {error, not_found} ->
            Response = #{
                code => 404,
                message => <<"Device not found">>
            },
            {ok, Response};
        {error, Reason} ->
            {error, Reason}
    end;

handle_request('POST', <<"/api/v1/devices">>, Req) ->
    % 创建设备
    {ok, Body, _} = cowboy_req:read_body(Req),
    DeviceData = jsx:decode(Body, [return_maps]),
    
    case dgiot_device:create_device(DeviceData) of
        {ok, Device} ->
            Response = #{
                code => 201,
                message => <<"Device created successfully">>,
                data => Device
            },
            {ok, Response};
        {error, Reason} ->
            Response = #{
                code => 400,
                message => <<"Failed to create device">>,
                error => Reason
            },
            {ok, Response}
    end;

handle_request(_, _, _) ->
    Response = #{
        code => 404,
        message => <<"API endpoint not found">>
    },
    {ok, Response}.
```

### 3.2 服务模块模板
```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_<module_name>_service 模块 - 业务逻辑服务
%%%
%%% 处理业务逻辑，调用数据访问层
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_<module_name>_service).

%% API
-export([get_devices/2, get_device/1, create_device/1, update_device/2, delete_device/1]).

%% 内部函数
-export([]).

-include("dgiot_<module_name>.hrl").
-include_lib("dgiot/include/dgiot.hrl").

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 获取设备列表
%% @spec get_devices(Limit, Offset) -> {ok, [map()]} | {error, term()}
get_devices(Limit, Offset) when is_integer(Limit), is_integer(Offset) ->
    case dgiot_device_db:find_devices(Limit, Offset) of
        {ok, Devices} ->
            % 转换数据格式
            FormattedDevices = lists:map(fun format_device/1, Devices),
            {ok, FormattedDevices};
        Error ->
            Error
    end.

%% @doc 获取单个设备
%% @spec get_device(DeviceId) -> {ok, map()} | {error, term()}
get_device(DeviceId) when is_binary(DeviceId) ->
    case dgiot_device_db:find_device(DeviceId) of
        {ok, Device} ->
            {ok, format_device(Device)};
        {error, not_found} ->
            {error, not_found};
        Error ->
            Error
    end.

%% @doc 创建设备
%% @spec create_device(DeviceData) -> {ok, map()} | {error, term()}
create_device(DeviceData) when is_map(DeviceData) ->
    % 验证设备数据
    case validate_device_data(DeviceData) of
        true ->
            % 生成设备ID
            DeviceId = generate_device_id(),
            NewDevice = DeviceData#{id => DeviceId, created_at => erlang:system_time()},
            
            case dgiot_device_db:insert_device(NewDevice) of
                {ok, _} ->
                    {ok, NewDevice};
                Error ->
                    Error
            end;
        false ->
            {error, invalid_device_data}
    end.

%% @doc 更新设备
%% @spec update_device(DeviceId, Updates) -> {ok, map()} | {error, term()}
update_device(DeviceId, Updates) when is_binary(DeviceId), is_map(Updates) ->
    case dgiot_device_db:update_device(DeviceId, Updates) of
        {ok, UpdatedDevice} ->
            {ok, format_device(UpdatedDevice)};
        Error ->
            Error
    end.

%% @doc 删除设备
%% @spec delete_device(DeviceId) -> ok | {error, term()}
delete_device(DeviceId) when is_binary(DeviceId) ->
    dgiot_device_db:delete_device(DeviceId).

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% @doc 格式化设备数据
format_device(Device) ->
    #{
        id => maps:get(id, Device),
        name => maps:get(name, Device, <<"">>),
        type => maps:get(type, Device, <<"unknown">>),
        status => maps:get(status, Device, <<"offline">>),
        created_at => maps:get(created_at, Device),
        updated_at => maps:get(updated_at, Device, erlang:system_time())
    }.

%% @private
%% @doc 验证设备数据
validate_device_data(DeviceData) ->
    RequiredFields = [<<"name">>, <<"type">>],
    lists:all(fun(Field) -> maps:is_key(Field, DeviceData) end, RequiredFields).

%% @private
%% @doc 生成设备ID
generate_device_id() ->
    Timestamp = integer_to_binary(erlang:system_time()),
    Random = integer_to_binary(rand:uniform(1000000)),
    <<"device_", Timestamp/binary, "_", Random/binary>>.
```

## 4. API文档模板

### 4.1 OpenAPI/Swagger文档
```yaml
openapi: 3.0.0
info:
  title: DG-IoT <Module Name> API
  description: <模块功能描述>
  version: 1.0.0
servers:
  - url: http://localhost:8080/api/v1
    description: 开发服务器
paths:
  /devices:
    get:
      summary: 获取设备列表
      description: 分页查询设备列表
      parameters:
        - name: limit
          in: query
          description: 每页数量
          required: false
          schema:
            type: integer
            default: 100
        - name: offset
          in: query
          description: 偏移量
          required: false
          schema:
            type: integer
            default: 0
      responses:
        '200':
          description: 成功
          content:
            application/json:
              schema:
                type: object
                properties:
                  code:
                    type: integer
                    example: 200
                  message:
                    type: string
                    example: "success"
                  data:
                    type: array
                    items:
                      $ref: '#/components/schemas/Device'
                  total:
                    type: integer
                    example: 50
    post:
      summary: 创建设备
      description: 创建新的设备
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/DeviceCreate'
      responses:
        '201':
          description: 创建成功
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/Device'
        '400':
          description: 请求参数错误

components:
  schemas:
    Device:
      type: object
      properties:
        id:
          type: string
          example: "device_1234567890"
        name:
          type: string
          example: "温度传感器"
        type:
          type: string
          example: "temperature_sensor"
        status:
          type: string
          example: "online"
        created_at:
          type: integer
          example: 1672531200000
        updated_at:
          type: integer
          example: 1672531200000
    DeviceCreate:
      type: object
      required:
        - name
        - type
      properties:
        name:
          type: string
          example: "温度传感器"
        type:
          type: string
          example: "temperature_sensor"
        description:
          type: string
          example: "用于测量环境温度"
```

### 4.2 API使用示例
```bash
# 查询设备列表
curl -X GET "http://localhost:8080/api/v1/devices?limit=10&offset=0"

# 查询单个设备
curl -X GET "http://localhost:8080/api/v1/devices/device_1234567890"

# 创建设备
curl -X POST "http://localhost:8080/api/v1/devices" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "温度传感器",
    "type": "temperature_sensor",
    "description": "用于测量环境温度"
  }'

# 更新设备
curl -X PUT "http://localhost:8080/api/v1/devices/device_1234567890" \
  -H "Content-Type: application/json" \
  -d '{
    "status": "online"
  }'

# 删除设备
curl -X DELETE "http://localhost:8080/api/v1/devices/device_1234567890"
```

## 5. API测试模板

### 5.1 API单元测试
```erlang
-module(dgiot_<module_name>_controller_test).

-include_lib("eunit/include/eunit.hrl").

handle_request_get_devices_test() ->
    % 模拟GET请求
    Req = #{qs => <<"limit=10&offset=0">>},
    ?assertMatch({ok, #{code := 200}}, dgiot_<module_name>_controller:handle_request('GET', <<"/api/v1/devices">>, Req)).

handle_request_get_device_test() ->
    % 模拟GET请求（单个设备）
    Req = #{},
    ?assertMatch({ok, #{code := 200}}, dgiot_<module_name>_controller:handle_request('GET', <<"/api/v1/devices/test_device">>, Req)).

handle_request_create_device_test() ->
    % 模拟POST请求
    DeviceData = #{<<"name">> => <<"测试设备">>, <<"type">> => <<"test">>},
    Body = jsx:encode(DeviceData),
    Req = #{body => Body},
    ?assertMatch({ok, #{code := 201}}, dgiot_<module_name>_controller:handle_request('POST', <<"/api/v1/devices">>, Req)).
```

### 5.2 API集成测试
```bash
#!/bin/bash
# test_api_integration.sh

echo "开始API集成测试..."
echo "========================================"

# 1. 测试设备列表API
echo "1. 测试设备列表API..."
curl -s -X GET "http://localhost:8080/api/v1/devices?limit=5" | jq .

# 2. 测试创建设备API
echo "2. 测试创建设备API..."
DEVICE_ID=$(curl -s -X POST "http://localhost:8080/api/v1/devices" \
  -H "Content-Type: application/json" \
  -d '{"name": "集成测试设备", "type": "integration_test"}' | jq -r '.data.id')

echo "创建的设备ID: $DEVICE_ID"

# 3. 测试查询设备API
echo "3. 测试查询设备API..."
curl -s -X GET "http://localhost:8080/api/v1/devices/$DEVICE_ID" | jq .

# 4. 测试更新设备API
echo "4. 测试更新设备API..."
curl -s -X PUT "http://localhost:8080/api/v1/devices/$DEVICE_ID" \
  -H "Content-Type: application/json" \
  -d '{"status": "online"}' | jq .

# 5. 测试删除设备API
echo "5. 测试删除设备API..."
curl -s -X DELETE "http://localhost:8080/api/v1/devices/$DEVICE_ID" | jq .

echo "========================================"
echo "API集成测试完成！"
```

## 6. API最佳实践

### 6.1 安全性
- [ ] 使用HTTPS加密通信
- [ ] 实现身份验证和授权
- [ ] 验证输入参数
- [ ] 防止SQL注入和XSS攻击

### 6.2 性能
- [ ] 实现API缓存
- [ ] 使用分页查询大数据集
- [ ] 优化数据库查询
- [ ] 监控API响应时间

### 6.3 可维护性
- [ ] 保持API向后兼容
- [ ] 提供详细的错误信息
- [ ] 记录API访问日志
- [ ] 版本控制API

### 6.4 文档
- [ ] 提供完整的API文档
- [ ] 包含请求/响应示例
- [ ] 说明错误码和含义
- [ ] 提供SDK和客户端示例

## 7. API调试和监控

### 7.1 调试命令
```bash
# 查看API日志
tail -f logs/api.log

# 测试API端点
curl -v "http://localhost:8080/api/v1/devices"

# 监控API性能
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_api_monitor:get_stats().'
```

### 7.2 监控指标
- API响应时间
- 请求成功率
- 错误率
- 并发连接数
- 数据吞吐量

---

**提示：** 将 `<module_name>` 替换为实际的模块名称，根据具体业务需求调整API设计。
