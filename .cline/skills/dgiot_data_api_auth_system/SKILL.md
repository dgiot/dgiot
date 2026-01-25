---
name: dgiot_data_api_auth_system
description: DGIOT数据存储、API设计与权限系统专家，详细解释DGIOT的多级数据存储体系、RESTful API架构和RBAC权限控制系统，并与Hook系统形成联动
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-23
category: development
tags: [dgiot, data_storage, api_design, auth_system, rbac, hook_integration, architecture]
trigger_phrases:
  - DGIOT数据存储
  - API设计
  - 权限系统
  - 数据存储类型
  - API架构
  - RBAC权限控制
  - Hook联动
  - 多级存储体系
  - RESTful API
  - 会话管理
---

# DGIOT数据存储、API设计与权限系统专家

详细解释DGIOT的多级数据存储体系、RESTful API架构和RBAC权限控制系统，并与Hook系统形成联动。

## 快速开始

当用户需要了解DGIOT的数据存储、API设计或权限系统时，激活本技能。本技能提供完整的架构解析、最佳实践和Hook集成方案。

## DGIOT数据存储体系

### 1. 多级数据存储架构

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

### 2. 核心存储模块

#### A. dgiot_data.erl - 统一数据访问接口
```erlang
%% 统一数据操作API
- 支持ETS/DETS/Mnesia多种后端
- 线程安全的数据操作
- 分页查询和条件查询
- 自动缓存管理

%% 使用示例
dgiot_data:insert(<<"cache">>, <<"key">>, Value).      % 插入数据
dgiot_data:lookup(<<"cache">>, <<"key">>).            % 查询数据
dgiot_data:delete(<<"cache">>, <<"key">>).            % 删除数据
dgiot_data:loop(<<"cache">>, Fun).                    % 遍历数据
```

#### B. dgiot_parse.erl - Parse Server客户端
```erlang
%% Parse Server操作API
- RESTful API封装
- 数据同步和缓存
- 权限集成
- 批量操作支持

%% 使用示例
dgiot_parse:create_object(<<"Device">>, DeviceData).  % 创建设备
dgiot_parse:query_object(<<"Device">>, Query).        % 查询设备
dgiot_parse:update_object(<<"Device">>, Id, Updates). % 更新设备
dgiot_parse:del_object(<<"Device">>, Id).             % 删除设备
```

#### C. dgiot_tdengine.erl - 时序数据存储
```erlang
%% TDengine时序数据操作
- 时序数据写入
- 时间窗口查询
- 数据聚合计算
- 性能监控

%% 使用示例
dgiot_tdengine:save(DeviceId, Metrics, Timestamp).    % 保存时序数据
dgiot_tdengine:query(DeviceId, StartTime, EndTime).   % 查询时序数据
```

### 3. 存储选择策略

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

## DGIOT API设计架构

### 1. RESTful API设计原则

#### A. API分类体系
```erlang
%% DGIOT API分类
1. 系统API (System APIs)
   - /iotapi/upload: 文件上传
   - /iotapi/health: 健康检查
   - /iotapi/config: 配置管理

2. 用户API (User APIs)
   - /iotapi/login: 用户登录
   - /iotapi/users: 用户管理
   - /iotapi/roles: 角色管理

3. 设备API (Device APIs)
   - /iotapi/devices: 设备查询
   - /iotapi/device/{id}: 设备详情
   - /iotapi/device/debug: 设备调试

4. 数据API (Data APIs)
   - /iotapi/data: 数据查询
   - /iotapi/export: 数据导出
   - /iotapi/import: 数据导入

5. 产品API (Product APIs)
   - /iotapi/products: 产品管理
   - /iotapi/thing: 物模型管理
   - /iotapi/properties: 属性管理
```

#### B. API处理器设计
```erlang
%% API处理器模板
-module(dgiot_data_handler).
-behavior(dgiot_rest).
-dgiot_rest(all).

%% API描述
swagger_data() ->
    [dgiot_http_server:bind(<<"/swagger_data.json">>, ?MODULE, [], priv)].

%% 请求处理
handle(OperationID, Args, Context, Req) ->
    case catch do_request(OperationID, Args, Context, Req) of
        {ok, Res} -> {200, Headers, Res, Req};
        {error, Reason} -> {500, Headers, #{<<"error">> => Reason}}
    end.

%% 具体API实现
do_request(post_upload, #{<<"file">> := FileInfo}, Context, _Req) ->
    % 文件上传处理逻辑
    {ok, FileInfo#{<<"objectId">> => ObjectId}};
```

### 2. 认证与授权机制

#### A. 认证方式支持
```erlang
%% 支持的认证方式
1. Basic认证: Authorization: Basic base64(username:password)
2. Bearer Token认证: Authorization: Bearer {sessionToken}
3. API Key认证: X-API-Key: {apiKey}
4. Session Token认证: X-Parse-Session-Token: {sessionToken}
```

#### B. 认证检查流程
```erlang
%% 认证检查实现
pre_check(OperationID, LogicHandler, AuthList, Req) ->
    % 检查Authorization头
    case dgiot_req:get_value(<<"header">>, <<"authorization">>, Req) of
        {undefined, Req1} ->
            pre_check_impl(OperationID, LogicHandler, AuthList, Req1);
        {<<"Bearer ", Token/binary>>, Req1} ->
            {ok, #{<<"type">> => <<"apiKey">>, <<"apiKey">> => Token}, Req1}
    end.
```

### 3. 错误处理规范

```erlang
%% 统一错误响应格式
错误响应结构:
{
    "code": 200,          # 状态码
    "message": "成功",     # 消息描述
    "data": {...},        # 业务数据
    "timestamp": 1634567890  # 时间戳
}

%% 标准状态码
- 200: 成功
- 400: 请求参数错误
- 401: 未授权
- 403: 禁止访问
- 404: 资源不存在
- 500: 服务器内部错误
```

## DGIOT权限控制系统

### 1. RBAC权限模型

#### A. 四级权限模型
```erlang
%% 权限数据结构
1. 用户 (User): 系统使用者
2. 角色 (Role): 权限集合
3. 权限 (Permission): 具体操作权限
4. 资源 (Resource): 被保护的对象

%% 权限关系
User --(属于)--> Role --(拥有)--> Permission --(控制)--> Resource
```

#### B. 角色管理
```erlang
%% 角色数据结构
-record(role, {
    objectId,      % 角色ID
    name,          % 角色名称
    alias,         % 角色别名
    level,         % 角色级别
    parent,        % 父角色ID
    org_type,      % 组织类型
    tag,           % 标签信息
    users = [],    % 用户列表
    rules = [],    % 权限规则
    menus = []     % 菜单权限
}).

%% 角色树结构
角色层级: 超级管理员 → 管理员 → 操作员 → 查看员
权限继承: 子角色继承父角色的所有权限
```

### 2. 权限检查实现

#### A. 权限检查流程
```erlang
%% 权限检查核心逻辑
check_auth(OperationID, Args, Req) ->
    % 1. 提取认证信息
    case pre_check(OperationID, LogicHandler, AuthList, Req) of
        {ok, #{<<"type">> := Type, <<"apiKey">> := Token}} ->
            % 2. 验证Token
            case get_session(Token) of
                undefined -> {false, #{<<"code">> => 209}, Req};
                #{<<"rules">> := Rules} = UserInfo ->
                    % 3. 检查操作权限
                    Action = list_to_binary(string:to_upper(atom_to_list(OperationID))),
                    case lists:member(Action, Rules) of
                        true -> {true, #{<<"user">> => UserInfo}, Req};
                        false -> {forbidden, #{<<"code">> => 119}, Req}
                    end
            end
    end.
```

#### B. 会话管理
```erlang
%% 会话管理机制
1. 会话创建: login_by_account/2 → create_session/3
2. 会话存储: dgiot_cache:set({Token, parse}, UserInfo, TTL)
3. 会话验证: get_session/1 → jsx:decode/2
4. 会话刷新: refresh_session/1 → update expiresAt
5. 会话销毁: delete_session/1 → dgiot_cache:delete/1
```

### 3. ACL权限控制

```erlang
%% Parse Server ACL机制
ACL数据结构:
#{
    <<"role:admin">> => #{<<"read">> => true, <<"write">> => true},
    <<"userId">> => #{<<"read">> => true, <<"write">> => false},
    <<"*">> => #{<<"read">> => true}  % 公共读权限
}

%% ACL应用场景
1. 数据行级权限: 每条数据独立的ACL
2. 角色级权限: role:{roleName}格式
3. 用户级权限: 具体用户ID
4. 公共权限: *表示所有用户
```

## Hook系统联动

### 1. Hook系统概述

#### A. Hook类型体系
```erlang
%% DGIOT Hook类型
1. TaskStart Hook: 任务开始时的检查
2. PreToolUse Hook: 工具使用前的验证
3. PostToolUse Hook: 工具使用后的处理
4. TaskComplete Hook: 任务完成时的清理
5. UserPromptSubmit Hook: 用户提交时的分析
```

#### B. Hook与数据存储联动
```erlang
%% Hook数据存储示例
PreToolUse Hook → 检查数据权限 → 记录操作日志 → 存储到Parse Server

%% 实现代码
handle_pre_tool_use(ToolName, Args) ->
    % 1. 检查用户权限
    case check_data_permission(ToolName, Args) of
        allowed ->
            % 2. 记录操作日志
            LogData = #{
                <<"tool">> => ToolName,
                <<"args">> => Args,
                <<"timestamp">> => dgiot_datetime:nowstamp(),
                <<"user">> => get_current_user()
            },
            dgiot_parse:create_object(<<"OperationLog">>, LogData),
            {ok, allowed};
        denied ->
            {error, <<"Permission denied">>}
    end.
```

### 2. Hook与API联动

#### A. API请求Hook
```erlang
%% API请求前的Hook处理
handle_api_pre_request(OperationID, Args, Context) ->
    % 1. 数据验证
    case validate_api_data(OperationID, Args) of
        {ok, ValidatedArgs} ->
            % 2. 权限检查
            case check_api_permission(OperationID, Context) of
                {ok, NewContext} ->
                    % 3. 记录审计日志
                    log_api_request(OperationID, ValidatedArgs, NewContext),
                    {ok, ValidatedArgs, NewContext};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

#### B. API响应Hook
```erlang
%% API响应后的Hook处理
handle_api_post_response(OperationID, Args, Context, Response) ->
    % 1. 记录响应日志
    LogData = #{
        <<"operation">> => OperationID,
        <<"args">> => Args,
        <<"response">> => Response,
        <<"timestamp">> => dgiot_datetime:nowstamp(),
        <<"duration">> => calculate_duration()
    },
    
    % 2. 存储到Parse Server
    dgiot_parse:create_object(<<"ApiResponseLog">>, LogData),
    
    % 3. 更新统计数据
    update_api_statistics(OperationID, Response),
    
    Response.
```

### 3. Hook与权限系统联动

#### A. 权限验证Hook
```erlang
%% 权限验证Hook
handle_permission_check(UserId, Resource, Action) ->
    % 1. 获取用户角色
    case dgiot_parse_auth:get_role(UserId) of
        {ok, #{<<"roles">> := Roles}} ->
            % 2. 检查角色权限
            case check_role_permission(Roles, Resource, Action) of
                allowed ->
                    % 3. 记录权限检查
                    log_permission_check(UserId, Resource, Action, allowed),
                    {ok, allowed};
                denied ->
                    % 4. 记录拒绝访问
                    log_permission_check(UserId, Resource, Action, denied),
                    {error, <<"Access denied">>}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

#### B. 会话管理Hook
```erlang
%% 会话创建Hook
handle_session_create(UserId, SessionToken) ->
    % 1. 验证用户状态
    case check_user_status(UserId) of
        active ->
            % 2. 创建会话记录
            SessionData = #{
                <<"userId">> => UserId,
                <<"sessionToken">> => SessionToken,
                <<"createdAt">> => dgiot_datetime:nowstamp(),
                <<"expiresAt">> => dgiot_datetime:nowstamp() + 86400,
                <<"ipAddress">> => get_client_ip(),
                <<"userAgent">> => get_user_agent()
            },
            
            % 3. 存储到Parse Server
            dgiot_parse:create_object(<<"Session">>, SessionData),
            
            % 4. 更新用户最后登录时间
            dgiot_parse:update_object(<<"_User">>, UserId, #{
                <<"lastLogin">> => dgiot_datetime:nowstamp()
            }),
            
            {ok, SessionToken};
        suspended ->
            {error, <<"User account suspended">>};
        deleted ->
            {error, <<"User account deleted">>}
    end.
```

## 实际应用示例

### 1. 完整数据流示例

```erlang
%% 设备数据上报完整流程
1. 设备通过MQTT上报数据
   Topic: $dg/device/{productId}/{devaddr}/post
   Payload: {"temperature": 25.5, "humidity": 60}

2. Hook处理 (PreDataProcess)
   - 验证设备权限
   - 检查数据格式
   - 记录接收时间

3. 数据存储
   - 实时数据: 存储到ETS缓存
   - 时序数据: 存储到TDengine
   - 业务数据: 存储到Parse Server

4. API查询
   GET /iotapi/device/{deviceId}/data
   - 权限检查: check_auth/3
   - 数据查询: dgiot_parse:query_object/2
   - 响应处理: 统一格式返回

5. Hook处理 (PostDataProcess)
   - 更新设备状态
   - 触发告警规则
   - 记录操作日志
```

### 2. 权限控制示例

```erlang
%% 设备管理权限控制
1. 用户登录获取Token
   POST /iotapi/login
   → dgiot_auth:login_by_account/2
   → create_session/3
   → 返回sessionToken

2. 查询设备列表
   GET /iotapi/devices
   Headers: {"
