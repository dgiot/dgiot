---
name: dgiot_core_concepts
description: DGIOT核心概念专家，详细解释DGIOT的产品、设备、通道、用户、视图、菜单等核心概念设计，并与Hook系统形成联动
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-23
category: development
tags: [dgiot, core_concepts, product, device, channel, user, view, menu, architecture, hook_integration]
trigger_phrases:
  - DGIOT产品设计
  - 设备管理
  - 通道配置
  - 用户体系
  - 视图设计
  - 菜单管理
  - 核心概念
  - 物模型
  - 产品设备通道
  - Hook联动
---

# DGIOT核心概念专家

详细解释DGIOT的产品、设备、通道、用户、视图、菜单等核心概念设计，并与Hook系统形成联动。

## 快速开始

当用户需要了解DGIOT的核心概念体系时，激活本技能。本技能提供完整的概念解析、数据模型设计和Hook集成方案。

## DGIOT核心概念体系

### 1. 产品 (Product) - 物模型定义

#### A. 产品数据结构
```erlang
%% 产品核心数据结构
-record(product, {
    objectId,          % 产品ID
    name,              % 产品名称
    category,          % 产品分类
    manufacturer,      % 制造商
    model,             % 型号
    version,           % 版本
    status = <<"active">>,  % 状态: active/inactive
    config = #{},      % 配置信息
    thing = #{},       % 物模型定义
    createdBy,         % 创建者
    createdAt,         % 创建时间
    updatedAt          % 更新时间
}).

%% 物模型定义结构
物模型包含:
1. 属性 (Properties): 设备状态数据
2. 服务 (Services): 设备可执行的操作
3. 事件 (Events): 设备触发的事件
4. 标签 (Tags): 设备分类标签
```

#### B. 产品管理API
```erlang
%% 产品管理API接口
1. 创建产品: POST /iotapi/products
2. 查询产品: GET /iotapi/products
3. 产品详情: GET /iotapi/products/{productId}
4. 更新产品: PUT /iotapi/products/{productId}
5. 删除产品: DELETE /iotapi/products/{productId}
6. 物模型管理: GET/POST/PUT/DELETE /iotapi/thing
```

#### C. 产品与Hook联动
```erlang
%% 产品创建Hook
handle_product_create(ProductData) ->
    % 1. 验证产品数据
    case validate_product_data(ProductData) of
        {ok, ValidatedData} ->
            % 2. 创建物模型
            ThingModel = create_thing_model(ValidatedData),
            
            % 3. 存储到Parse Server
            Product = ValidatedData#{<<"thing">> => ThingModel},
            dgiot_parse:create_object(<<"Product">>, Product),
            
            % 4. 记录操作日志
            log_product_operation(create, Product),
            
            {ok, Product};
        {error, Reason} ->
            {error, Reason}
    end.
```

### 2. 设备 (Device) - 物理实体

#### A. 设备数据结构
```erlang
%% 设备核心数据结构
-record(device, {
    objectId,          % 设备ID
    productId,         % 所属产品ID
    name,              % 设备名称
    devaddr,           % 设备地址
    status = <<"offline">>,  % 状态: online/offline/error
    location = #{},    % 位置信息
    config = #{},      % 设备配置
    lastSeen,          % 最后上线时间
    metadata = #{},    % 元数据
    createdBy,         % 创建者
    createdAt,         % 创建时间
    updatedAt          % 更新时间
}).

%% 设备状态管理
设备状态流转:
offline → online (设备上线)
online → offline (设备下线)
online → error (设备故障)
error → online (故障恢复)
```

#### B. 设备管理API
```erlang
%% 设备管理API接口
1. 创建设备: POST /iotapi/devices
2. 查询设备: GET /iotapi/devices
3. 设备详情: GET /iotapi/devices/{deviceId}
4. 更新设备: PUT /iotapi/devices/{deviceId}
5. 删除设备: DELETE /iotapi/devices/{deviceId}
6. 设备调试: GET /iotapi/device/debug
7. 设备数据: GET /iotapi/device/{deviceId}/data
```

#### C. 设备与Hook联动
```erlang
%% 设备上线Hook
handle_device_online(DeviceId, Session) ->
    % 1. 更新设备状态
    dgiot_parse:update_object(<<"Device">>, DeviceId, #{
        <<"status">> => <<"online">>,
        <<"lastSeen">> => dgiot_datetime:nowstamp(),
        <<"session">> => Session
    }),
    
    % 2. 记录设备日志
    LogData = #{
        <<"deviceId">> => DeviceId,
        <<"action">> => <<"online">>,
        <<"timestamp">> => dgiot_datetime:nowstamp(),
        <<"session">> => Session
    },
    dgiot_parse:create_object(<<"DeviceLog">>, LogData),
    
    % 3. 触发设备上线事件
    dgiot_bridge:send_event(<<"device_online">>, #{
        <<"deviceId">> => DeviceId,
        <<"timestamp">> => dgiot_datetime:nowstamp()
    }),
    
    ok.
```

### 3. 通道 (Channel) - 通信管道

#### A. 通道数据结构
```erlang
%% 通道核心数据结构
-record(channel, {
    objectId,          % 通道ID
    name,              % 通道名称
    type,              % 通道类型: tcp/udp/mqtt/http/modbus
    cType,             % 客户端类型
    productId,         % 关联产品ID
    config = #{},      % 通道配置
    status = <<"disabled">>,  % 状态: enabled/disabled
    clientNum = 0,     % 客户端数量
    maxClient = 1000,  % 最大客户端数
    createdBy,         % 创建者
    createdAt,         % 创建时间
    updatedAt          % 更新时间
}).

%% 通道类型说明
1. TCP通道: 面向连接的字节流传输
2. UDP通道: 无连接的数据报传输
3. MQTT通道: 发布订阅消息协议
4. HTTP通道: RESTful API通信
5. Modbus通道: 工业协议通信
```

#### B. 通道管理API
```erlang
%% 通道管理API接口
1. 创建通道: POST /iotapi/channels
2. 查询通道: GET /iotapi/channels
3. 通道详情: GET /iotapi/channels/{channelId}
4. 更新通道: PUT /iotapi/channels/{channelId}
5. 删除通道: DELETE /iotapi/channels/{channelId}
6. 通道状态: GET /iotapi/channels/{channelId}/status
```

#### C. 通道与Hook联动
```erlang
%% 通道消息处理Hook
handle_channel_message(ChannelId, ClientId, Message) ->
    % 1. 解析消息协议
    case parse_protocol(ChannelId, Message) of
        {ok, ParsedData} ->
            % 2. 业务逻辑处理
            ProcessedData = process_business_logic(ChannelId, ParsedData),
            
            % 3. 数据存储
            save_device_data(ClientId, ProcessedData),
            
            % 4. 记录消息日志
            log_channel_message(ChannelId, ClientId, Message, ProcessedData),
            
            {ok, ProcessedData};
        {error, Reason} ->
            {error, Reason}
    end.
```

### 4. 用户 (User) - 系统使用者

#### A. 用户数据结构
```erlang
%% 用户核心数据结构
-record(user, {
    objectId,          % 用户ID
    username,          % 用户名
    email,             % 邮箱
    mobile,            % 手机号
    fullName,          % 全名
    avatar,            % 头像
    status = <<"active">>,  % 状态: active/inactive
    roles = [],        % 角色列表
    org = #{},         % 组织信息
    config = #{},      % 用户配置
    lastLogin,         % 最后登录时间
    createdAt,         % 创建时间
    updatedAt          % 更新时间
}).

%% 用户权限体系
用户 → 角色 → 权限 → 资源
基于RBAC的权限控制模型
```

#### B. 用户管理API
```erlang
%% 用户管理API接口
1. 用户注册: POST /iotapi/register
2. 用户登录: POST /iotapi/login
3. 用户查询: GET /iotapi/users
4. 用户详情: GET /iotapi/users/{userId}
5. 用户更新: PUT /iotapi/users/{userId}
6. 用户删除: DELETE /iotapi/users/{userId}
7. 角色管理: GET/POST/PUT/DELETE /iotapi/roles
```

#### C. 用户与Hook联动
```erlang
%% 用户登录Hook
handle_user_login(Username, Password, Context) ->
    % 1. 验证用户凭证
    case dgiot_auth:login_by_account(Username, Password) of
        {ok, #{<<"sessionToken">> := Token} = UserInfo} ->
            % 2. 创建会话
            SessionData = create_session(UserInfo, Context),
            
            % 3. 记录登录日志
            log_login_activity(Username, <<"success">>, Context),
            
            % 4. 更新用户最后登录时间
            dgiot_parse:update_object(<<"_User">>, UserInfo#{
                <<"lastLogin">> => dgiot_datetime:nowstamp()
            }),
            
            {ok, Token, UserInfo};
        {error, Reason} ->
            % 记录失败日志
            log_login_activity(Username, <<"failed">>, Context#{<<"reason">> => Reason}),
            {error, Reason}
    end.
```

### 5. 部门 (Department) - 组织架构

#### A. 部门数据结构
```erlang
%% 部门核心数据结构
-record(department, {
    objectId,          % 部门ID
    name,              % 部门名称
    code,              % 部门编码
    parentId,          % 上级部门ID
    managerId,         % 部门负责人ID
    orgType,           % 组织类型: company/dept/team
    level = 1,         % 部门层级
    order = 0,         % 排序序号
    status = <<"active">>,  % 状态: active/inactive
    description,       % 部门描述
    users = [],        % 部门成员列表
    roles = [],        % 部门角色列表
    config = #{},      % 部门配置
    createdBy,         % 创建者
    createdAt,         % 创建时间
    updatedAt          % 更新时间
}).

%% 部门层级结构
公司 → 一级部门 → 二级部门 → 团队
树形组织结构，支持无限层级
```

#### B. 部门管理API
```erlang
%% 部门管理API接口
1. 创建部门: POST /iotapi/departments
2. 查询部门: GET /iotapi/departments
3. 部门详情: GET /iotapi/departments/{deptId}
4. 更新部门: PUT /iotapi/departments/{deptId}
5. 删除部门: DELETE /iotapi/departments/{deptId}
6. 部门树: GET /iotapi/departments/tree
7. 部门成员: GET /iotapi/departments/{deptId}/users
8. 部门角色: GET /iotapi/departments/{deptId}/roles
```

#### C. 部门与Hook联动
```erlang
%% 部门创建Hook
handle_department_create(DeptData, Context) ->
    % 1. 验证部门数据
    case validate_department_data(DeptData) of
        {ok, ValidatedData} ->
            % 2. 生成部门编码
            DeptCode = generate_dept_code(ValidatedData),
            
            % 3. 存储到Parse Server
            Department = ValidatedData#{<<"code">> => DeptCode},
            dgiot_parse:create_object(<<"Department">>, Department),
            
            % 4. 记录部门操作日志
            log_department_operation(create, Department, Context),
            
            % 5. 初始化部门角色
            init_department_roles(Department),
            
            {ok, Department};
        {error, Reason} ->
            {error, Reason}
    end.
```

### 6. 会话 (Session) - 用户会话管理

#### A. 会话数据结构
```erlang
%% 会话核心数据结构
-record(session, {
    objectId,          % 会话ID
    userId,            % 用户ID
    sessionToken,      % 会话Token
    deviceId,          % 设备ID (可选)
    ipAddress,         % IP地址
    userAgent,         % 用户代理
    loginTime,         % 登录时间
    lastActivity,      % 最后活动时间
    expiresAt,         % 过期时间
    status = <<"active">>,  % 状态: active/expired/revoked
    metadata = #{},    % 会话元数据
    permissions = [],  % 会话权限
    createdAt,         % 创建时间
    updatedAt          % 更新时间
}).

%% 会话生命周期
创建 → 活跃 → 过期/撤销
支持多设备同时登录
自动会话续期
```

#### B. 会话管理API
```erlang
%% 会话管理API接口
1. 创建会话: POST /iotapi/sessions (登录时自动创建)
2. 查询会话: GET /iotapi/sessions
3. 会话详情: GET /iotapi/sessions/{sessionId}
4. 更新会话: PUT /iotapi/sessions/{sessionId} (续期)
5. 删除会话: DELETE /iotapi/sessions/{sessionId} (登出)
6. 用户会话: GET /iotapi/user/{userId}/sessions
7. 会话验证: POST /iotapi/sessions/verify
8. 批量登出: POST /iotapi/sessions/batch_logout
```

#### C. 会话与Hook联动
```erlang
%% 会话创建Hook
handle_session_create(UserId, Context) ->
    % 1. 生成会话Token
    SessionToken = generate_session_token(),
    
    % 2. 创建会话记录
    SessionData = #{
        <<"userId">> => UserId,
        <<"sessionToken">> => SessionToken,
        <<"ipAddress">> => maps:get(<<"ip">>, Context, <<"">>),
        <<"userAgent">> => maps:get(<<"userAgent">>, Context, <<"">>),
        <<"loginTime">> => dgiot_datetime:nowstamp(),
        <<"lastActivity">> => dgiot_datetime:nowstamp(),
        <<"expiresAt">> => dgiot_datetime:nowstamp() + 86400, % 24小时
        <<"status">> => <<"active">>,
        <<"metadata">> => #{}
    },
    
    % 3. 存储到Parse Server
    dgiot_parse:create_object(<<"Session">>, SessionData),
    
    % 4. 缓存会话到ETS (快速验证)
    dgiot_data:insert(<<"sessions">>, SessionToken, SessionData),
    
    % 5. 记录登录日志
    log_session_activity(create, UserId, Context),
    
    {ok, SessionToken, SessionData}.
```

### 7. 视图 (View) - 数据展示

#### A. 视图数据结构
```erlang
%% 视图核心数据结构
-record(view, {
    objectId,          % 视图ID
    name,              % 视图名称
    type,              % 视图类型: dashboard/chart/table/form
    config = #{},      % 视图配置
    dataSource,        % 数据源
    filters = [],      % 过滤器
    layout = #{},      % 布局配置
    permissions = [],  % 权限控制
    createdBy,         % 创建者
    createdAt,         % 创建时间
    updatedAt          % 更新时间
}).

%% 视图类型说明
1. 仪表盘 (Dashboard): 综合数据展示
2. 图表 (Chart): 数据可视化
3. 表格 (Table): 数据列表展示
4. 表单 (Form): 数据录入界面
```

#### B. 视图管理API
```erlang
%% 视图管理API接口
1. 创建视图: POST /iotapi/views
2. 查询视图: GET /iotapi/views
3. 视图详情: GET /iotapi/views/{viewId}
4. 更新视图: PUT /iotapi/views/{viewId}
5. 删除视图: DELETE /iotapi/views/{viewId}
6. 视图数据: GET /iotapi/views/{viewId}/data
```

#### C.
