# 工位逻辑扩展Hooks

## 概述

本文件定义了工位逻辑扩展的Hooks系统，支持在工位生命周期的关键点插入自定义逻辑。

## Hook点定义

### 1. 工位创建Hook
- **Hook名称**: `station_create`
- **触发时机**: 工位Worker创建之前
- **参数**: `{StationId, StationType, Config}`
- **返回值**: `{ok, ModifiedConfig} | {error, Reason}`

### 2. 工位初始化Hook
- **Hook名称**: `station_init`
- **触发时机**: 工位Worker初始化之后
- **参数**: `{StationId, StationType, State}`
- **返回值**: `{ok, ModifiedState} | {error, Reason}`

### 3. 数据处理Hook
- **Hook名称**: `station_data_process`
- **触发时机**: 工位处理数据之前
- **参数**: `{StationId, RawData, State}`
- **返回值**: `{ok, ModifiedRawData, ModifiedState} | {error, Reason}`

### 4. 命令执行Hook
- **Hook名称**: `station_command_execute`
- **触发时机**: 工位执行命令之前
- **参数**: `{StationId, CommandCode, Params, State}`
- **返回值**: `{ok, ModifiedParams, ModifiedState} | {error, Reason}`

### 5. 工位销毁Hook
- **Hook名称**: `station_destroy`
- **触发时机**: 工位Worker销毁之前
- **参数**: `{StationId, StationType, State}`
- **返回值**: `ok | {error, Reason}`

## Hook注册规范

### 1. Hook注册函数
```erlang
%% 注册工位Hook
register_station_hooks() ->
    dgiot_hook:add(one_for_one, {station_create, <<"my_plugin">>}, 
                   fun my_plugin:on_station_create/1),
    dgiot_hook:add(one_for_one, {station_init, <<"my_plugin">>}, 
                   fun my_plugin:on_station_init/1),
    dgiot_hook:add(one_for_one, {station_data_process, <<"my_plugin">>}, 
                   fun my_plugin:on_station_data_process/1),
    dgiot_hook:add(one_for_one, {station_command_execute, <<"my_plugin">>}, 
                   fun my_plugin:on_station_command_execute/1),
    dgiot_hook:add(one_for_one, {station_destroy, <<"my_plugin">>}, 
                   fun my_plugin:on_station_destroy/1),
    ok.
```

### 2. Hook实现示例
```erlang
%% Hook实现模块
-module(my_station_hooks).

%% Hook函数
-export([
    on_station_create/1,
    on_station_init/1,
    on_station_data_process/1,
    on_station_command_execute/1,
    on_station_destroy/1
]).

%% 工位创建Hook
on_station_create({StationId, StationType, Config}) ->
    ?LOG(info, "工位创建Hook: ~p, ~p", [StationId, StationType]),
    
    % 添加自定义配置
    ModifiedConfig = Config#{
        custom_field => <<"custom_value">>,
        hook_processed => true
    },
    
    {ok, ModifiedConfig}.

%% 工位初始化Hook
on_station_init({StationId, StationType, State}) ->
    ?LOG(info, "工位初始化Hook: ~p, ~p", [StationId, StationType]),
    
    % 修改工位状态
    ModifiedState = State#{
        hook_initialized => true,
        initialization_time => ?NOW_MS
    },
    
    {ok, ModifiedState}.

%% 数据处理Hook
on_station_data_process({StationId, RawData, State}) ->
    ?LOG(debug, "数据处理Hook: ~p, 数据大小: ~p", [StationId, byte_size(RawData)]),
    
    % 预处理数据
    ModifiedRawData = preprocess_data(RawData),
    
    {ok, ModifiedRawData, State}.

%% 命令执行Hook
on_station_command_execute({StationId, CommandCode, Params, State}) ->
    ?LOG(info, "命令执行Hook: ~p, 命令: ~p", [StationId, CommandCode]),
    
    % 验证命令参数
    case validate_command_params(CommandCode, Params) of
        true ->
            {ok, Params, State};
        false ->
            {error, invalid_parameters}
    end.

%% 工位销毁Hook
on_station_destroy({StationId, StationType, State}) ->
    ?LOG(info, "工位销毁Hook: ~p, ~p", [StationId, StationType]),
    
    % 清理资源
    cleanup_resources(StationId),
    ok.
```

## Hook执行流程

### 1. 工位创建流程
```
1. 接收工位创建请求
2. 执行station_create Hook
3. 创建工位Worker
4. 执行station_init Hook
5. 返回工位实例
```

### 2. 数据处理流程
```
1. 接收原始数据
2. 执行station_data_process Hook
3. 调用子类数据处理方法
4. 返回处理结果
```

### 3. 命令执行流程
```
1. 接收命令请求
2. 执行station_command_execute Hook
3. 调用子类命令执行方法
4. 返回执行结果
```

### 4. 工位销毁流程
```
1. 接收工位销毁请求
2. 执行station_destroy Hook
3. 停止工位Worker
4. 清理资源
```

## Hook配置管理

### 1. Hook配置文件
```yaml
# station_hooks.yaml
hooks:
  station_create:
    enabled: true
    plugins:
      - my_plugin
      - logging_plugin
      - validation_plugin
  
  station_init:
    enabled: true
    plugins:
      - my_plugin
      - monitoring_plugin
  
  station_data_process:
    enabled: true
    plugins:
      - data_filter_plugin
      - encryption_plugin
  
  station_command_execute:
    enabled: true
    plugins:
      - command_validator_plugin
      - audit_plugin
  
  station_destroy:
    enabled: true
    plugins:
      - cleanup_plugin
      - logging_plugin
```

### 2. Hook启用/禁用
```erlang
%% 启用Hook
enable_station_hook(HookName) ->
    dgiot_hook:enable(HookName).

%% 禁用Hook
disable_station_hook(HookName) ->
    dgiot_hook:disable(HookName).

%% 检查Hook状态
check_hook_status(HookName) ->
    dgiot_hook:status(HookName).
```

## Hook性能考虑

### 1. 异步Hook执行
```erlang
%% 异步执行Hook
execute_hook_async(HookName, Args) ->
    spawn(fun() ->
        case dgiot_hook:run_hook(HookName, Args) of
            {ok, Result} ->
                ?LOG(debug, "Hook ~p 执行成功", [HookName]);
            {error, Reason} ->
                ?LOG(error, "Hook ~p 执行失败: ~p", [HookName, Reason])
        end
    end).
```

### 2. Hook超时控制
```erlang
%% 带超时的Hook执行
execute_hook_with_timeout(HookName, Args, Timeout) ->
    case dgiot_hook:run_hook(HookName, Args, Timeout) of
        {ok, Result} ->
            {ok, Result};
        {error, timeout} ->
            ?LOG(warning, "Hook ~p 执行超时", [HookName]),
            {error, timeout};
        {error, Reason} ->
            {error, Reason}
    end.
```

## Hook测试

### 1. Hook单元测试
```erlang
-module(station_hooks_test).

-include_lib("eunit/include/eunit.hrl").

station_create_hook_test() ->
    Args = {1, station1, #{station_id => 1}},
    ?assertMatch({ok, _}, my_station_hooks:on_station_create(Args)).

station_init_hook_test() ->
    Args = {1, station1, #{}},
    ?assertMatch({ok, _}, my_station_hooks:on_station_init(Args)).
```

### 2. Hook集成测试
```erlang
test_hook_integration() ->
    % 注册Hook
    register_station_hooks(),
    
    % 创建工位测试Hook
    Config = #{station_id => 1, station_type => station1},
    {ok, ModifiedConfig} = dgiot_hook:run_hook(station_create, [1, station1, Config]),
    
    ?assert(maps:is_key(hook_processed, ModifiedConfig)),
    ok.
```

## 安全考虑

### 1. Hook权限控制
```erlang
%% 检查Hook执行权限
check_hook_permission(HookName, PluginName) ->
    case get_hook_permissions(HookName) of
        {ok, AllowedPlugins} ->
            lists:member(PluginName, AllowedPlugins);
        _ ->
            false
    end.
```

### 2. Hook输入验证
```erlang
%% 验证Hook输入
validate_hook_input(HookName, Args) ->
    case HookName of
        station_create ->
            validate_station_create_args(Args);
        station_init ->
            validate_station_init_args(Args);
        _ ->
            true
    end.
```

## 监控和日志

### 1. Hook执行监控
```erlang
%% 监控Hook执行
monitor_hook_execution(HookName, StartTime, EndTime, Result) ->
    ExecutionTime = EndTime - StartTime,
    
    Metrics = #{
        hook_name => HookName,
        execution_time => ExecutionTime,
        result => Result,
        timestamp => ?NOW_MS
    },
    
    dgiot_metrics:record(hook_execution, Metrics).
```

### 2. Hook执行日志
```erlang
%% Hook执行日志
log_hook_execution(HookName, Args, Result) ->
    ?LOG(info, "Hook执行: ~p, 参数: ~p, 结果: ~p", [HookName, Args, Result]).
```

## 更新记录

- 2026-01-26：创建工位逻辑扩展Hooks文档