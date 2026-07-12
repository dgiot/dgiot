# dgaiot 函数目录 — 150+ 函数, 10 apps, 调用关系

## Data 层函数

### dgiot_parse_id (30 functions) — 确定性寻址
```
get_deviceid(Pid, DevAddr)          → MD5("Device"+Pid+DevAddr) 前10 hex
get_productid(Cat, DevType, Name)   → MD5("Product"+Cat+Type+Name)
get_channelid(Type, CType, Name)    → MD5("Channel"+Type+CType+Name)
get_roleid(Name)                    → MD5("_Role"+Name)
get_userid(Name)                    → MD5("_User"+Name)
get_menuid(Name)                    → MD5("Menu"+Name)
get_ruleid(Name)                    → MD5("Permission"+Name)
get_sessionId(Token)                → MD5("_Session"+Token)
get_objectid(Class, Map)            → 通用: 20个pattern match clauses
...
```

### dgiot_parse (20 functions) — Parse REST 客户端
```
create_object(Class, Map)           → POST /classes/{Class}
get_object(Class, ObjectId)         → GET /classes/{Class}/{id}
update_object(Class, Id, Map)       → PUT /classes/{Class}/{id}
query_object(Class, Where)          → GET /classes/{Class}?where=...
batch(Requests)                     → POST /batch
get_schemas()                       → GET /schemas
create_schemas(Fields)              → POST /schemas
health()                            → GET /health
```
被调用: dgiot_ontology, dgiot_device, dgiot_role, dgiot_tdengine...

### dgiot_tdengine (15 functions) — 时序存储
```
get_database(ChId, Pid)             → _{ChannelId} 或 ETS缓存
create_database(DB, Keep)           → CREATE DATABASE IF NOT EXISTS
create_schemas(Channel, Schema)     → CREATE TABLE (Super/sub)
create_object(Channel, Table, Obj)  → INSERT INTO ... VALUES
query_object(Channel, Table, Query) → SELECT ... FROM
save_fields(Pid, Fields)            → ETS缓存列定义
```
被调用: dgiot_ontology.push_point, dgiot_device

## Logic 层函数

### dgiot_ontology (10 functions) — 本体引擎
```
init()                              → 创建3 ETS表 (model/instance/rules)
load_model(#{class,props,rules})    → ETS插入模型 + 编译规则
spawn_instance(Class, InstanceId)   → gen_statem:start_link
get_model(Class)                    → ETS lookup
list_instances(Class)               → ETS select
get_path(PointId)                   → 构建MQTT topic路径 (3次REST→缓存)
push_point(PointId, Value)          → get_path + MQTT publish
register(layer, Map)                → Parse create_object
```
调用: dgiot_parse, dgiot_mqtt, gen_statem

### dgiot_thing (4 functions) — 物模型解码
```
decoder(Data, Props)                → 二进制字节→物模型字段映射
check_value(Id, Value, Thing)       → 阈值校验 (alarm_hi/lo, range)
check_value(Data, Thing)            → 批量校验
format_string(Value, Format)        → "%{d}MPa" 格式化显示
```
调用链: dgiot_product:parse_frame → dgiot_thing:decoder

### dgiot_product (25 functions) — 产品管理
```
lookup_prod(Pid)                    → ETS查产品 (高频!)
get_productSecret(Pid)              → 设备连接密码 (dlink auth)
get_device_thing(Pid, DevId)        → 获取设备对应物模型
parse_frame(Pid, Frame)             → 帧解析→物模型字段
to_frame(Pid, Props)                → 物模型字段→帧编码
create_product(Map)                 → Parse + ETS
get_props(Pid)                      → 产品属性列表
update_properties(Pid, Props)       → 属性变更→TDengine alter_table
save_prod(Pid, Data)               → 产品数据保存
```
调用: dgiot_thing, dgiot_parse, dgiot_tdengine

### dgiot_device (30 functions) — 设备管理
```
create_device(Map)                  → 注册设备到Parse
lookup(DeviceId)                    → ETS查设备
post/put/save/delete                → CRUD
online(DeviceId) / offline(DeviceId)→ 状态变更 (存量接管)
get_sub_device(ParentId)            → 子设备树查询
get_acl(DeviceId)                   → 设备ACL
get_roleids(DeviceId)               → 设备角色白名单
save_log(DeviceId, Level, Msg)      → 设备日志
enable(DeviceId) / disable(DeviceId)→ 启停设备
```
调用: dgiot_parse, dgiot_product, dgiot_role

### dgiot_role (15 functions) — 角色引擎
```
childrole([RoleId], [])             → 递归展开角色层级树
get_childacl(AclName)               → 查询角色子ACL
get_acls(Device)                    → 设备ACL key列表
get_acl(Device)                     → 设备完整ACL
get_rolenames(UserId)               → 用户角色名列表
load_roles()                        → 启动时加载全部角色到ETS
```
调用: dgiot_mqtt_acl.check_device_acl, dgiot_auth

## Action 层函数

### dgiot_shadow (6 functions) — 设备影子
```
start_link(DeviceId, Opts)          → gen_statem:start_link
get_state(Pid)                      → 查询影子状态
get_device(Pid)                     → 查询影子设备信息
inject(Pid, Msg)                    → MQTT消息注入影子
sync_to_parse(Pid)                  → 同步状态到Parse
```
状态机: authenticate → online → {normal,alarm,offline}

### dgiot_bridge (15 functions) — 桥接框架
```
start_channel(ChId, Config)         → 启动通道
register_channel(ChId, Type)        → 注册通道类型
get_product_info(Pid)               → 获取产品桥接信息
parse_frame(Pid, Frame)             → 帧解析→MQTT消息
to_frame(Pid, Props)                → MQTT消息→帧编码
send_log(ChId, Fmt, Args)           → 通道日志
apply_channel(ChId, Type, Args)     → 通道配置
```
通道类型: tcp/udp/http/mqtt/log/httpc/tcpc/udpc

### dgiot_task_worker (6 functions) — 任务调度
```
start_link(Args)                    → gen_server:start_link
handle_call/3                       → 同步任务处理
handle_cast/2                       → 异步任务处理
handle_info/2                       → 定时任务触发
```

## Security 层函数

### dgiot_mqtt_acl (3 functions) — MQTT ACL
```
check_acl(ClientInfo, PubSub, Topic, Action, Params)
                                    → 25个pattern match子句
                                      设备: PID+DevAddr匹配
                                      用户: Token→Session→Role
                                      超级: dgiot@127.0.0.1
description()                       → "Acl with Dlink"
check_device_acl(Token, DevId, Uid) → Token→Role→intersect→allow/deny
```

### dgiot_auth (7 functions) — 认证
```
pre_check(Method, Path, Headers, Body) → 请求预检
check_auth(Method, Path, Headers)     → 认证检查
put_session(Token, User)              → 创建会话
get_session(Token)                    → 查询会话→User+Roles
delete_session(Token)                 → 删除会话
```

## Erlang 消息架构 — 所有关系=消息路由

```
Erlang 核心原则:
  函数调用     → 同步, 同进程, call(Pid, Msg) → handle_call
  消息传递     → 异步, 跨进程, cast(Pid, Msg) / Pid ! Msg → handle_cast/handle_info
  状态迁移     → gen_statem: cast → handle_event → {next_state, NewState, Data}
  监督树       → start_link → init → {ok, Pid, State}
```

### 进程间消息路由 (关系=消息)

```
EMQX hook:
  Client PUBLISH $dg/thing/{PID}/{DevAddr}/properties/report
    → dgiot_mqtt_acl:check_acl (函数调用, 同步)
    → dgiot_bridge:apply_channel → ChannelPid ! {publish, Payload}
    → ChannelPid handle_info({publish, ...})
      → dgiot_bridge:parse_frame → dgiot_product:parse_frame
      → dgiot_thing:decoder → map
      → ShadowPid ! {data, Props}          ← 关键消息!

Shadow gen_statem:
  handle_event(cast, {data, Props}, online, Device) →
    evaluate(Rules, Props)
    → state: online → alarm (if triggered)
    → BridgePid ! {sync_parse, Device}     ← 消息!
    → TdEnginePid ! {insert, Table, Values} ← 消息!

Role 引擎:
  check_device_acl(Token, DevId, Uid) →
    SessionPid ! {get_session, Token}      ← 消息!
    RoleTreePid ! {childrole, [RoleId]}    ← 消息!
    DevicePid ! {get_roleids, DevId}       ← 消息!
    → gather responses → allow/deny

物模型编译:
  load_model(Class, Model) →
    Rules = compile_rules(Model.rules)  ← 函数调用, 同步
    EtsPid ! {insert, model_table, Class, Model} ← 消息!
    RulePid ! {insert, rule_table, RuleId, Rule} ← 消息!
```

### 消息流全景

```
物理设备 (Modbus RTU)
  │ RS-485 frame
  ▼
TCP Worker (dgiot_tcp_worker.erl / gen_server)
  │ handle_info({tcp, Socket, Data})
  ▼
dgiot_decoder:decode(Data)
  ▼
dgiot_product:parse_frame(ProductId, Decoded)
  ▼
dgiot_thing:decoder(Fields, ThingModel)  → #{oil_pressure => 2.35}
  ▼
EMQX publish: $dg/thing/{PID}/{DevAddr}/properties/report
  │ MQTT broker routes to subscribers
  ▼
Shadow gen_statem: handle_event(cast, {data, Props})
  │ evaluate(Rules)
  │ state transition?
  ├─→ BridgePid ! {sync_parse, State}
  │   dgiot_parse:update_object(Device, Id, State)
  │
  ├─→ TdPid ! {insert, Table, Values}
  │   dgiot_tdengine:create_object(Channel, Table, Values)
  │
  └─→ AlarmPid ! {alarm, Severity, Action} (if triggered)
      dgiot_notification:send(...)
```

### gen_statem 消息驱动状态迁移

```erlang
%% Shadow 进程接收的消息类型:
handle_event(cast, {data, Props}, State, Device)       %% MQTT数据
handle_event(cast, heartbeat, State, Device)           %% 心跳
handle_event(cast, {event, Event}, State, Device)      %% 事件/告警
handle_event(state_timeout, heartbeat_missed, State)   %% 超时
handle_event({call, From}, get_state, State, Device)   %% 查询
handle_event({call, From}, sync, State, Device)        %% 同步

%% 消息驱动状态迁移:
online  + {data, T=82.3}   → R_HIGH_TEMP匹配 → alarm
alarm   + heartbeat         → online (恢复)
online  + heartbeat_missed  → offline
offline + heartbeat         → online
```

## 函数调用关系图

```
MQTT消息到达
  → dgiot_mqtt_acl:check_acl       (授权)
  → dgiot_bridge:parse_frame       (帧解析)
  → dgiot_product:parse_frame      (协议→字段)
  → dgiot_thing:decoder            (二进制→map)
  → dgiot_thing:check_value        (阈值校验)
  → dgiot_ontology:push_point      (推送)
       ├→ dgiot_ontology:get_path  (MQTT topic)
       │    └→ dgiot_parse:get_object ×3 (REST, 后续ETS缓存)
       └→ dgiot_mqtt:publish
  → dgiot_shadow:evaluate          (状态机)
       ├→ dgiot_ontology:eval_rules (规则匹配)
       ├→ dgiot_device:save_log     (日志)
       ├→ dgiot_parse:update_object (状态持久)
       └→ dgiot_tdengine:create_object (时序写入)

用户登录
  → dgiot_auth:put_session
  → dgiot_role:load_user
  → dgiot_parse_id:get_sessionId  (MD5编码)

设备注册
  → dgiot_device:create_device
  → dgiot_parse_id:get_deviceid   (MD5编码)
  → dgiot_product:get_productSecret
  → dgiot_tdengine:create_schemas (建表)
  → dgiot_ontology:register       (本体注册)
  → dgiot_ontology:spawn_instance (启动影子)

ACL检查
  → dgiot_mqtt_acl:check_device_acl
  → dgiot_auth:get_session
  → dgiot_role:childrole          (角色递归)
  → dgiot_device:get_roleids      (设备角色)
  → intersect → allow/deny
```
