---
name: dgiot_data_storage
description: DGIOT数据存储专家，详细解释DGIOT的多级数据存储体系，包括ETS/DETS/Mnesia内存存储、Parse Server业务存储、TDengine时序存储和文件存储
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-23
category: development
tags: [dgiot, data_storage, ets, dets, mnesia, parse_server, tdengine, redis, fastdfs, architecture]
trigger_phrases:
  - DGIOT数据存储
  - 多级存储体系
  - ETS内存存储
  - Parse Server
  - TDengine时序存储
  - 数据存储选择
  - 缓存管理
  - 文件存储
  - 数据持久化
---

# DGIOT数据存储专家

详细解释DGIOT的多级数据存储体系，包括ETS/DETS/Mnesia内存存储、Parse Server业务存储、TDengine时序存储和文件存储。

## 快速开始

当用户需要了解DGIOT的数据存储架构、存储选择策略或具体存储模块使用时，激活本技能。

## DGIOT多级数据存储架构

### 1. 存储层次结构

```
DGIOT数据存储层次:
├── 内存层 (Memory Layer) - 高性能访问
│   ├── ETS: 会话缓存、进程状态、热点数据
│   ├── DETS: 配置持久化、临时数据
│   └── Mnesia: 集群数据同步、分布式锁
├── 业务层 (Business Layer) - 持久化存储
│   ├── Parse Server: 用户/设备/产品数据 (MongoDB/PostgreSQL)
│   ├── TDengine: 时序传感器数据 (时序数据库)
│   └── Redis: 缓存和消息队列
├── 文件层 (File Layer) - 文件存储
│   ├── 本地文件系统: 配置文件、日志文件
│   ├── FastDFS: 分布式文件存储
│   └── 对象存储: 图片/视频文件
└── 备份层 (Backup Layer) - 数据保护
    ├── 数据库备份
    ├── 配置文件备份
    └── 日志归档
```

### 2. 存储选择策略

```erlang
%% 根据数据类型选择存储
存储选择矩阵:
1. 会话数据: ETS内存存储 (高性能访问)
2. 配置数据: DETS磁盘存储 (持久化)
3. 业务数据: Parse Server (复杂查询)
4. 时序数据: TDengine (时间序列优化)
5. 文件数据: FastDFS (分布式存储)
6. 缓存数据: Redis (高速缓存)
```

## 核心存储模块详解

### 1. dgiot_data.erl - 统一数据访问接口

#### A. 模块功能概述
```erlang
%% 统一数据操作API特性
- 支持ETS/DETS/Mnesia多种后端
- 线程安全的数据操作
- 分页查询和条件查询
- 自动缓存管理
- 数据遍历和搜索
```

#### B. 核心API使用示例
```erlang
%% 数据插入操作
dgiot_data:insert(<<"cache">>, <<"key">>, Value).      % 插入数据
dgiot_data:save(<<"cache">>, {<<"key">>, Value}).      % 保存数据

%% 数据查询操作
dgiot_data:lookup(<<"cache">>, <<"key">>).            % 精确查询
dgiot_data:get(<<"cache">>, <<"key">>).               % 获取数据
dgiot_data:match(<<"cache">>, Pattern).               % 模式匹配
dgiot_data:select(<<"cache">>, MatchSpec).            % 选择查询

%% 数据删除操作
dgiot_data:delete(<<"cache">>, <<"key">>).            % 删除指定键
dgiot_data:delete_all_objects(<<"cache">>).           % 清空表
dgiot_data:match_delete(<<"cache">>, Pattern).        % 模式删除

%% 数据遍历操作
dgiot_data:loop(<<"cache">>, Fun).                    % 遍历数据
dgiot_data:search(<<"cache">>, Fun).                  % 搜索数据
```

#### C. 高级功能
```erlang
%% 分页查询
dgiot_data:page(Name, PageNo, PageSize, Filter, RowFun, Order) ->
    dgiot_pager:page(ets:table(Name), Filter, PageNo, PageSize, RowFun, Order).

%% 计数器操作
dgiot_data:update_counter(<<"counter">>, <<"key">>, 1).  % 增加计数器
dgiot_data:update_counter(<<"counter">>, <<"key">>, -1). % 减少计数器

%% 消费者模式
dgiot_data:set_consumer(<<"consumer_key">>, 1000).      % 设置消费者阈值
dgiot_data:get_consumer(<<"consumer_key">>, 1).         % 获取并增加消费者计数
```

### 2. dgiot_parse.erl - Parse Server客户端

#### A. Parse Server集成
```erlang
%% Parse Server操作API特性
- RESTful API封装
- 数据同步和缓存
- 权限集成 (ACL/RBAC)
- 批量操作支持
- 文件上传下载
- 用户认证管理
```

#### B. 核心API使用示例
```erlang
%% 对象操作
dgiot_parse:create_object(<<"Device">>, DeviceData).  % 创建设备
dgiot_parse:query_object(<<"Device">>, Query).        % 查询设备
dgiot_parse:update_object(<<"Device">>, Id, Updates). % 更新设备
dgiot_parse:del_object(<<"Device">>, Id).             % 删除设备

%% 用户操作
dgiot_parse:signup(UserData).                         % 用户注册
dgiot_parse:login(Username, Password).                % 用户登录
dgiot_parse:get_user(UserId).                         % 获取用户信息
dgiot_parse:update_user(UserId, Updates).             % 更新用户

%% 文件操作
dgiot_parse:upload_file(Filename, Content).           % 上传文件
dgiot_parse:download_file(FileId).                    % 下载文件

%% 批量操作
dgiot_parse:batch_create(Objects).                    % 批量创建
dgiot_parse:batch_update(Updates).                    % 批量更新
```

#### C. 查询语法
```erlang
%% Parse Server查询条件
Query = #{
    <<"where">> => #{<<"status">> => <<"online">>},
    <<"order">> => <<"-createdAt">>,
    <<"limit">> => 100,
    <<"skip">> => 0,
    <<"count">> => 1,
    <<"include">> => <<"product">>
}.

%% 复杂查询示例
ComplexQuery = #{
    <<"where">> => #{
        <<"$and">> => [
            #{<<"status">> => <<"online">>},
            #{<<"productId">> => ProductId},
            #{<<"lastSeen">> => #{<<"$gte">> => StartTime}}
        ]
    },
    <<"order">> => <<"-lastSeen">>,
    <<"limit">> => 50
}.
```

### 3. dgiot_tdengine.erl - 时序数据存储

#### A. TDengine集成
```erlang
%% TDengine时序数据操作特性
- 时序数据写入优化
- 时间窗口查询
- 数据聚合计算
- 性能监控指标
- 多表联合查询
- 数据压缩存储
```

#### B. 核心API使用示例
```erlang
%% 数据写入
dgiot_tdengine:save(DeviceId, Metrics, Timestamp).    % 保存时序数据
dgiot_tdengine:batch_save(DeviceId, MetricsList).     % 批量保存

%% 数据查询
dgiot_tdengine:query(DeviceId, StartTime, EndTime).   % 时间范围查询
dgiot_tdengine:latest(DeviceId).                      % 最新数据查询
dgiot_tdengine:aggregate(DeviceId, StartTime, EndTime, <<"avg">>). % 聚合查询

%% 表管理
dgiot_tdengine:create_table(DeviceId, Schema).        % 创建时序表
dgiot_tdengine:alter_table(DeviceId, Alterations).    % 修改表结构
dgiot_tdengine:drop_table(DeviceId).                  % 删除表
```

#### C. 时序数据模型
```erlang
%% 时序数据结构
时序数据记录:
{
    "deviceId": "device_001",
    "timestamp": 1634567890000,
    "metrics": {
        "temperature": 25.5,
        "humidity": 60.2,
        "voltage": 220.0,
        "current": 5.5
    },
    "tags": {
        "location": "room_101",
        "product": "smart_meter"
    }
}

%% 聚合查询示例
AggregationQuery = #{
    <<"start">> => <<"2024-01-01 00:00:00">>,
    <<"end">> => <<"2024-01-31 23:59:59">>,
    <<"interval">> => <<"1h">>,
    <<"aggregation">> => <<"avg">>,
    <<"metrics">> => [<<"temperature">>, <<"humidity">>]
}.
```

### 4. 其他存储模块

#### A. Redis缓存
```erlang
%% Redis操作API
dgiot_redis:set(<<"key">>, Value).                    % 设置键值
dgiot_redis:get(<<"key">>).                           % 获取值
dgiot_redis:del(<<"key">>).                           % 删除键
dgiot_redis:expire(<<"key">>, TTL).                   % 设置过期时间
dgiot_redis:incr(<<"counter">>).                      % 增加计数器
dgiot_redis:lpush(<<"list">>, Value).                 % 列表操作
```

#### B. FastDFS文件存储
```erlang
%% FastDFS文件操作
dgiot_fastdfs:upload(FileData, Filename).             % 上传文件
dgiot_fastdfs:download(FileId).                       % 下载文件
dgiot_fastdfs:delete(FileId).                         % 删除文件
dgiot_fastdfs:get_url(FileId).                        % 获取文件URL
```

#### C. Mnesia分布式存储
```erlang
%% Mnesia集群操作
dgiot_mnesia:create_table(TableDef).                  % 创建表
dgiot_mnesia:write(Record).                           % 写入记录
dgiot_mnesia:read(Table, Key).                        % 读取记录
dgiot_mnesia:delete(Table, Key).                      % 删除记录
dgiot_mnesia:sync().                                  % 同步集群
```

## 存储性能优化

### 1. 内存存储优化

```erlang
%% ETS表优化配置
ETS优化参数:
- {write_concurrency, true}    % 写并发优化
- {read_concurrency, true}     % 读并发优化
- {decentralized_counters, true} % 分散计数器
- {compressed, true}           % 数据压缩
- {memory, MaxMemory}          % 内存限制

%% 表类型选择
表类型比较:
1. set: 哈希表，快速查找，无序
2. ordered_set: 平衡二叉树，有序，稍慢
3. bag: 允许重复值，较慢
4. duplicate_bag: 允许重复值，较快
```

### 2. 数据库优化

```erlang
%% Parse Server优化
1. 索引优化: 为查询字段创建索引
2. 查询优化: 使用投影减少返回字段
3. 缓存优化: 热点数据缓存到Redis
4. 批量操作: 使用批量API减少请求

%% TDengine优化
1. 分区策略: 按时间分区提高查询性能
2. 预聚合: 创建预聚合表减少实时计算
3. 压缩设置: 配置合适的压缩算法
4. 缓存配置: 调整内存缓存大小
```

### 3. 缓存策略

```erlang
%% 多级缓存策略
缓存层级:
1. L1缓存: ETS内存缓存 (纳秒级)
2. L2缓存: Redis分布式缓存 (毫秒级)
3. L3缓存: 数据库查询缓存 (秒级)

%% 缓存更新策略
缓存更新方式:
1. 写穿透: 先写数据库，再更新缓存
2. 写回: 先写缓存，异步写数据库
3. 失效: 数据变更时使缓存失效
4. 刷新: 定时刷新缓存数据
```

## 数据一致性保障

### 1. 事务管理

```erlang
%% Mnesia分布式事务
mnesia:transaction(fun() ->
    mnesia:write(#user{id = 1, name = "test"}),
    mnesia:write(#device{id = 1, userId = 1})
end).

%% 补偿事务
handle_compensating_transaction(Operation, Args) ->
    try
        Result = Operation(Args),
        {ok, Result}
    catch
        error:Reason ->
            % 执行补偿操作
            compensate(Operation, Args, Reason),
            {error, Reason}
    end.
```

### 2. 数据同步

```erlang
%% 数据同步机制
同步策略:
1. 实时同步: 重要数据立即同步
2. 异步同步: 非关键数据异步同步
3. 批量同步: 定时批量同步
4. 增量同步: 只同步变更数据

%% 同步实现
sync_data(Source, Target) ->
    % 获取增量数据
    Changes = get_changes_since(LastSyncTime),
    
    % 应用变更
    apply_changes(Target, Changes),
    
    % 更新同步时间
    update_sync_time(CurrentTime).
```

## 实际应用案例

### 1. 设备数据存储方案

```erlang
%% 设备数据完整存储流程
handle_device_data(DeviceId, Data) ->
    % 1. 实时数据存入ETS (快速访问)
    dgiot_data:insert(<<"device_realtime">>, DeviceId, Data),
    
    % 2. 时序数据存入TDengine (长期存储)
    Metrics = extract_metrics(Data),
    dgiot_tdengine:save(DeviceId, Metrics, dgiot_datetime:nowstamp()),
    
    % 3. 设备状态更新到Parse Server
    Status = calculate_device_status(Data),
    dgiot_parse:update_object(<<"Device">>, DeviceId, #{
        <<"status">> => Status,
        <<"lastSeen">> => dgiot_datetime:nowstamp()
    }),
    
    % 4. 缓存最新数据到Redis
    dgiot_redis:setex(<<"device_latest:", DeviceId/binary>>, 300, Data),
    
    ok.
```

### 2. 用户会话管理

```erlang
%% 用户会话存储方案
handle_user_session(UserId, SessionData) ->
    % 1. 会话Token存入ETS (快速验证)
    Token = generate_token(),
    dgiot_data:insert(<<"session_tokens">>, Token, #{
        userId => UserId,
        createdAt => dgiot_datetime:nowstamp(),
        expiresAt => dgiot_datetime:nowstamp() + 86400
    }),
    
    % 2. 用户会话信息存入Parse Server (持久化)
    dgiot_parse:create_object(<<"Session">>, #{
        <<"userId">> => UserId,
        <<"sessionToken">> => Token,
        <<"createdAt">> => dgiot_datetime:nowstamp(),
        <<"expiresAt">> => dgiot_datetime:nowstamp() + 86400,
        <<"ipAddress">> => get_client_ip(),
        <<"userAgent">> => get_user_agent()
    }),
    
    % 3. 用户活跃状态缓存到Redis
    dgiot_redis:setex(<<"user_active:", UserId/binary>>, 300, <<"online">>),
    
    {ok, Token}.
```

## 技能集成

### 1. 与其他技能联动

```yaml
# 完整数据存储工作流
workflow:
  - 架构理解: dgiot_architecture_learning
  - 核心概念: dgiot_core_concepts
  - 数据存储: dgiot_data_storage (本技能)
  - API设计: dgiot_api_design
  - 权限系统: dgiot_auth_system
  - 开发流程: development_workflow_cycle
```

### 2. Hook系统集成

```erlang
%% 数据存储Hook集成
handle_data_storage_hook(Operation, Data) ->
    % PreStorage Hook: 数据验证
    case validate_data(Data) of
        {ok, ValidatedData} ->
            % Storage Hook: 实际存储
            store_data(Operation, ValidatedData),
            
            % PostStorage Hook: 后处理
            post_process_data(Operation, ValidatedData),
            
            {ok, stored};
        {error, Reason} ->
            {error, Reason}
    end.
```

## 维护信息

- **版本历史**:
  - v1.0.0 (2026-01-23): 初始版本，详细解释DGIOT数据存储体系
- **更新计划**:
  - 添加更多存储模块案例
  - 集成性能监控工具
  - 优化存储策略指南
- **依赖技能**: dgiot_architecture_learning, dgiot_core_concepts

---

*本技能详细解释了DGIOT的多级数据存储体系，帮助开发者理解如何根据数据类型选择合适的存储
