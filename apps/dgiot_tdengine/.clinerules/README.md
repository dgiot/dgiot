# TDengine插件开发规则

## 概述

本文件定义了DG-IoT TDengine插件的开发规则和最佳实践，专门针对时序数据存储和查询。

## 开发命令

### 1. 热编译和热加载
```bash
# TDengine插件热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_tdengine).'

# TDengine插件热加载
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_tdengine).'

# 在线测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_tdengine:test().'

# 全量编译调试
make run
```

## 工程建议

### 1. 文件生成策略
- **先生成插件所需要的所有文件**：在全量编译之前，确保所有必要的文件都已创建
- **全量编译之后就不再新增文件**：一旦完成全量编译，后续修改只做热编译
- **全部做热编译**：日常开发中使用热编译提高效率

### 2. 文档设计流程
- **建议先做项目概要设计**：明确项目目标、范围和架构
- **完成工程文件布局**：创建标准的目录结构和文件组织
- **详细设计通过小步迭代方式完成**：采用敏捷开发，小步快跑

## 编码规范

### 1. 日志打印格式
```erlang
% 标准日志格式
io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event])

% 带上下文的日志
io:format("~s ~p [TDENGINE] ~p = ~p.~n", [?FILE, ?LINE, Action, Data])
```

### 2. 错误处理
```erlang
% 使用try-catch处理异常
try
    do_tdengine_operation()
catch
    error:Reason ->
        io:format("~s ~p TDengine Error: ~p~n", [?FILE, ?LINE, Reason]),
        {error, Reason}
end.
```

## API调试

### 1. Schema更新
```bash
# 新增API后更新Schema
dgiot_parse_utils:update_schemas_json().
```

## 本地笔记管理

### 1. 笔记存储
- **插件工程相关笔记都存到工程下面的ReadMe文件**
- 保持README.md文件更新，记录开发过程中的重要发现
- 添加代码注释，解释复杂的逻辑和算法
- 创建API文档，方便其他开发者使用

## TDengine规范

### 1. 数据库设计
```sql
-- 创建数据库
CREATE DATABASE IF NOT EXISTS dgiot KEEP 365 DAYS 10 BLOCKS 6;

-- 创建超级表
CREATE STABLE IF NOT EXISTS devices (
    ts TIMESTAMP,
    value DOUBLE,
    status INT
) TAGS (
    device_id BINARY(64),
    device_type BINARY(32),
    location BINARY(128)
);

-- 创建子表
CREATE TABLE IF NOT EXISTS device_001 USING devices TAGS ('device_001', 'sensor', 'room_101');
```

### 2. 数据操作
```erlang
% 插入时序数据
insert_timeseries_data(Database, Table, Data) ->
    % 数据格式：[{ts, value, status}]
    % 转换为SQL插入语句
    SQL = build_insert_sql(Database, Table, Data),
    execute_sql(SQL).

% 查询时序数据
query_timeseries_data(Database, Table, StartTime, EndTime, Fields) ->
    SQL = build_query_sql(Database, Table, StartTime, EndTime, Fields),
    execute_sql(SQL).
```

## 测试规范

### 1. 单元测试
```erlang
-module(dgiot_tdengine_test).

-include_lib("eunit/include/eunit.hrl").

build_insert_sql_test() ->
    Data = [{<<"2023-01-01 00:00:00">>, 25.5, 1}],
    ?assertMatch(<<"INSERT INTO", _/binary>>, dgiot_tdengine:build_insert_sql(<<"dgiot">>, <<"device_001">>, Data)).

build_query_sql_test() ->
    Fields = [<<"value">>, <<"status">>],
    ?assertMatch(<<"SELECT", _/binary>>, dgiot_tdengine:build_query_sql(<<"dgiot">>, <<"device_001">>, <<"2023-01-01">>, <<"2023-01-02">>, Fields)).
```

### 2. 集成测试
```bash
# 端到端测试脚本
#!/bin/bash
# test_tdengine_integration.sh

echo "启动TDengine插件集成测试..."
echo "1. 编译插件..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_tdengine).'

echo "2. 加载插件..."
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_tdengine).'

echo "3. 运行测试..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_tdengine:test_integration().'

echo "4. 验证结果..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_tdengine:verify_test_results().'
```

## 代码规范

### 1. 模块结构
```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_tdengine 模块 - TDengine时序数据库插件
%%%
%%% 支持功能：
%%% 1. 数据库创建和管理
%%% 2. 超级表和子表管理
%%% 3. 时序数据插入和查询
%%% 4. 数据聚合和分析
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_tdengine).

%% API
-export([create_database/1, create_stable/2, insert_data/3, query_data/5]).

%% 内部函数
-export([]).

-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/dgiot.hrl").
```

### 2. 数据库操作函数规范
```erlang
%% @doc 创建数据库
%% @spec create_database(Database) -> ok | {error, Reason}
%% @param Database 数据库名称
create_database(Database) when is_binary(Database) ->
    SQL = <<"CREATE DATABASE IF NOT EXISTS ", Database/binary, " KEEP 365 DAYS 10 BLOCKS 6">>,
    case execute_sql(SQL) of
        {ok, _} -> ok;
        Error -> Error
    end.

%% @doc 插入时序数据
%% @spec insert_data(Database, Table, Data) -> ok | {error, Reason}
%% @param Database 数据库名称
%% @param Table 表名
%% @param Data 数据列表，格式为[{Timestamp, Value, Status}]
insert_data(Database, Table, Data) when is_binary(Database), is_binary(Table), is_list(Data) ->
    SQL = build_insert_sql(Database, Table, Data),
    execute_sql(SQL).
```

## 故障排除

### 1. 常见问题
```bash
# 插件加载失败
# 检查依赖：确保所有依赖模块已编译
# 检查导出函数：确保API函数正确导出

# 数据库连接失败
# 检查TDengine服务状态：systemctl status taosd
# 检查连接配置：确保host、port、user、password正确

# SQL执行失败
# 检查SQL语法：使用TDengine客户端验证
# 检查权限：确保有足够的数据库权限
```

### 2. 调试命令
```erlang
% 启用详细日志
dgiot_tdengine:set_log_level(debug).

% 手动测试数据库连接
dgiot_tdengine:test_connection().

% 查看插件状态
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:status(dgiot_tdengine).'
```

## 最佳实践

### 1. 开发流程
1. **分析数据需求**：明确时序数据的存储和查询需求
2. **设计数据库结构**：设计超级表、子表和标签结构
3. **编写数据操作模块**：实现数据插入、查询、聚合功能
4. **搭建测试框架**：创建自动化测试脚本
5. **性能优化**：优化查询性能和数据存储效率

### 2. 测试策略
- **单元测试**：覆盖所有SQL构建函数
- **集成测试**：验证端到端数据流程
- **性能测试**：测试高并发数据插入和查询
- **压力测试**：测试大数据量下的性能表现

### 3. 文档要求
- 每个函数必须有完整的@doc注释
- 数据库设计必须有详细说明
- 测试用例必须有预期结果
- 故障排除必须有具体步骤

## 更新记录

- 2025-12-03：创建TDengine插件规则文档
- 基于现有规则和TDengine最佳实践

## 相关资源

- [TDengine官方文档]：https://docs.taosdata.com/
- [测试框架]：`apps/dgiot_tdengine/test/`
- [API文档]：`apps/dgiot_tdengine/README.md`
