# API接口管理规则（精简版）

## 概述

DG-IoT平台API接口的增加和删除管理规则，确保API生命周期管理的规范性和一致性。

## 核心原则

### 1. 三层架构（必须遵守）

```
API Gateway (handler) → Function Gateway (dgiot_*.erl) → Implementation (service/dao)
```

- **禁止**在Handler中实现业务逻辑
- **禁止**在Function Gateway中实现具体逻辑

### 2. API设计规范

- **命名**：`动词+名词`（`get_device_status/1`, `create_device/1`）
- **版本**：URL中包含版本号（`/api/v1/`）
- **响应格式**：统一使用`{ok, #{<<"data">> => Data, <<"status">> => 0}}`或`{error, Reason}`

## 新增API流程

### 1. Handler层（仅处理HTTP）

```erlang
do_request(get_device_status, Args, _Context, _Req) ->
    case dgiot_plugin:get_device_status(Args) of
        {ok, Data} -> {ok, #{<<"data">> => Data, <<"status">> => 0}};
        {error, Reason} -> {ok, #{<<"status">> => 1, <<"msg">> => Reason}}
    end.
```

### 2. 函数网关层（仅转发）

```erlang
-export([get_device_status/1]).

get_device_status(Args) ->
    dgiot_plugin_service:get_device_status(Args).
```

### 3. 服务层（业务逻辑）

```erlang
-export([get_device_status/1]).

get_device_status(#{<<"device_id">> := DeviceId}) ->
    % 具体业务逻辑
    {ok, #{<<"status">> => <<"online">>}}.
```

## 删除API流程

### 1. 四步清理法

1. **删除Handler中的API处理**：移除对应的`do_request`子句
2. **删除函数网关中的函数**：从`-export`列表中移除，删除函数实现
3. **删除Swagger文档**：移除对应的API定义
4. **删除测试代码**：移除相关的单元测试和集成测试

### 2. 验证步骤

```bash
# 编译验证
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_<plugin>).'

# 功能验证
# 确保其他API正常工作
```

## 检查清单

### 新增API检查清单

- [ ] Handler实现（仅HTTP处理）
- [ ] 函数网关实现（仅转发）
- [ ] 服务层实现（业务逻辑）
- [ ] 参数验证和错误处理
- [ ] 单元测试覆盖

### 删除API检查清单

- [ ] Handler清理完成
- [ ] 函数网关清理完成
- [ ] Swagger文档清理完成
- [ ] 测试代码清理完成
- [ ] 编译验证通过

## 最佳实践

### 1. 错误处理

```erlang
% 统一错误格式
{error, <<"Invalid parameters">>}
```

### 2. 日志记录

```erlang
io:format("~s ~p [API] ~p = ~p.~n", [?FILE, ?LINE, Action, Data])
```

### 3. 性能优化

- 使用ETS缓存频繁访问的数据
- 避免N+1查询问题
- 监控API响应时间

## 更新记录

- 2025-12-19：创建精简版API管理规则
