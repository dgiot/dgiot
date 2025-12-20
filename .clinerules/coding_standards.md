# DG-IoT编码规范

## 概述

Erlang/OTP编码规范和最佳实践，确保代码质量和一致性。

## 核心规范

### 1. 模块结构

```erlang
%%%-------------------------------------------------------------------
%%% @doc 模块说明 @end
%%%-------------------------------------------------------------------
-module(dgiot_example).
-export([start/0, stop/0, parse_packet/1]).
-include("dgiot.hrl").

%% @doc 启动函数
start() -> ok.

%% @doc 解析数据包
parse_packet(Packet) when is_binary(Packet) ->
    case Packet of
        <<16#EB, 16#90, Rest/binary>> -> {ok, #{magic => 16#EB90, data => Rest}};
        _ -> {error, invalid_magic}
    end.
```

### 2. 函数规范

#### 2.1 函数注释

```erlang
%% @doc 函数说明
%% @spec function_name(Type1, Type2) -> ReturnType
%% @param Param1 参数1说明
%% @param Param2 参数2说明
%% @returns 返回值说明
function_name(Param1, Param2) ->
    % 函数体
    ok.
```

#### 2.2 函数命名

- **动词+名词**：`parse_packet/1`, `send_message/2`
- **查询函数**：`get_device_status/1`, `find_by_id/1`
- **设置函数**：`set_config/2`, `update_status/2`
- **检查函数**：`check_connection/0`, `is_valid/1`
- **转换函数**：`to_binary/1`, `from_json/1`

### 3. 三层架构（必须遵守）

```
API Gateway (handler) → Function Gateway (dgiot_*.erl) → Implementation (service/dao)
```

- **禁止**在Handler中实现业务逻辑
- **禁止**在Function Gateway中实现具体逻辑

### 4. ETS表规范

```erlang
-dgiot_data("ets").
-export([init_ets/0]).
-define(USER_ETS, user_ets).

init_ets() ->
    dgiot_data:init(?USER_ETS),
    dgiot_data:init(?ORDER_ETS, [ordered_set]).
```

### 5. 错误处理规范

#### 5.1 返回值约定

```erlang
% 成功返回 {ok, Result}
% 失败返回 {error, Reason}
% 可选返回 {ok, Result, Extra}
```

#### 5.2 异常处理

```erlang
% 使用try-catch处理异常
safe_operation(Data) ->
    try
        do_risky_operation(Data)
    catch
        error:badarg ->
            {error, invalid_argument};
        error:Reason ->
            {error, Reason}
    end.
```

### 6. 日志格式

```erlang
% 标准日志格式
io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event])

% 带上下文的日志
io:format("~s ~p [~p] ~p = ~p.~n", [?FILE, ?LINE, Module, Action, Data])
```

## 目录结构

```
apps/dgiot_plugin/
├── src/
│   ├── dgiot_plugin.erl          # 函数网关
│   ├── dgiot_plugin_handler.erl   # API网关
│   ├── dgiot_plugin_service.erl   # 业务服务
│   └── dgiot_parser.erl          # 协议解析
├── include/dgiot_plugin.hrl
└── test/dgiot_plugin_test.erl
```

## 子模块命名规范

- `*_service.erl` - 业务逻辑服务
- `*_dao.erl` - 数据访问对象
- `*_utils.erl` - 工具函数
- `*_parser.erl` - 协议解析
- `*_channel.erl` - 通道管理
- `*_statistics.erl` - 统计功能
- `*_handler.erl` - HTTP请求处理（API网关）

## 快速检查清单

- [ ] 模块结构符合模板
- [ ] 函数有`@doc`注释
- [ ] 遵循三层架构
- [ ] 错误处理完整
- [ ] 日志格式正确
- [ ] 有单元测试
- [ ] ETS表规范正确

## 更新记录

- 2025-12-19：融合全局规则，创建统一编码规范
