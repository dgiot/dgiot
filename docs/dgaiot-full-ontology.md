# dgaiot 完整本体 — 10 apps 全量盘点

## 源码扫描范围

```
apps/dgiot/             核心引擎 (未逐行, 已有架构理解)
apps/dgiot_ontology/    本体引擎 (已逐行 v2.0)
apps/dgiot_parse/       Parse客户端 (已逐行 dgiot_parse_id.erl 429行)
apps/dgiot_task/        任务引擎 (已逐行 dgiot_shadow.erl + worker API)
apps/dgiot_device/      设备管理 (已逐行 36个源文件)
apps/dgiot_bridge/      桥接框架 (已逐行 31个源文件)
apps/dgiot_dlink/       数据链路 (已逐行 dgiot_mqtt_acl.erl 228行 + mock)
apps/dgiot_api/         管理API (已逐行 dgiot_auth.erl)
apps/dgiot_http/        HTTP服务 (已逐行 11个源文件)
apps/dgiot_tdengine/    TDengine (已逐行 schema 173行 + channel)
```

## 完整实体清单

### Data 层

```
Parse 23类 (JSONB):
  Site, Gateway, Device, Point, Product, ProductTemplet,
  Channel, Dict, Category, Timescale, Log, Devicelog, Userlog,
  Notification, Evidence, Instruct, Files, Git,
  _User, _Role, _Session, _SCHEMA, Menu, View,
  Permission, App, Article

TDengine:
  Database = _{ChannelId}
  SuperTable = _{ProductId}  (thing.properties → columns)
  SubTable = _{ProductId}_{DeviceId}  (USING SuperTable, TAGS devaddr)
  ETS keys: {tdengine_db, ChId, PId} → DB, {td, PId, DevId} → SubTable

ETS (dgiot_data):
  {ProductId, "TD"} → ChannelId
  {ProductId, describe_table} → [Columns]
  {ProductId, fields_table} → [Fields]
  {last_data, DeviceId} → #{实时值}
  {tdengine_db, ChannelId} → config
  user_role_ets → UserId → [RoleId]
  ?PARENT_ROLE_ETS → RoleId → ParentRoleId
  ?NAME_ROLE_ETS → RoleName → RoleId
```

### Logic 层

```
dgiot_thing:       decoder(binary→map) + check_value(阈值校验)
dgiot_product:     create/update + parse_frame + get_productSecret
dgiot_device:      create/update + online/offline + subdevice tree
dgiot_ontology:    load_model + spawn_instance + ETS + push_point
dgiot_parse_id:    30+ MD5编码函数 (确定性寻址)
dgiot_role:        childrole递归 + ACL/CLP
dgiot_formula_calculator: 表达式计算 (油井功图等)
```

### Action 层

```
gen_statem Shadow:  1:1 device process (dgiot_shadow.erl)
Bridge channels:    7种 (tcp/udp/http/mqtt/log/httpc/tcpc/udpc)
  dgiot_*_channel.erl + dgiot_*_worker.erl (共14个文件)
Task worker:        dgiot_task_worker.erl (gen_server)
Decoder:            dgiot_decoder.erl (帧解码→物模型字段)
Bridge server:      dgiot_bridge_server.erl (消息路由)
```

### Security 层

```
dgiot_auth:         pre_check + check_auth + put_session/get_session
dgiot_mqtt_acl:     3层授权 (Device/User/Superuser)
dgiot_device_permission: 设备级权限
dgiot_device:       get_acl/get_roleids (设备角色白名单)
```

## 关系矩阵 (N×N)

```
Device → Product      _Join:product:Channel (1332)
Product → Channel     dgiot_product_channel
Device → Gateway      Point→Device→Gateway (ontology path)
_User → _Role         _Join:users:_Role (399)
_Role → _Role         _Join:roles:_Role (298)
Rule → _Role          _Join:rules:_Role (85497)
Menu → _Role          _Join:menus:_Role (8492)
Device → _User        via _Join:rules:_Role + device ACL
Session → _User       _Join:users:_Session
Notification → _User  _Join:deletedBy + readBy
```

## 数据管线 (完整)

```
Modbus帧 (raw bytes)
  → dgiot_decoder: frame→fields
  → dgiot_thing:decoder: binary→map {oil_pressure:2.35}
  → dgiot_thing:check_value: 阈值校验
  → dgiot_product:parse_frame: 协议→MQTT topic
  → MQTT publish: $dg/thing/{PID}/{DevAddr}/properties/report
  → EMQX broker
  → gen_statem Shadow evaluate
  → bridge: sync_parse + dgiot_tdengine:create_object
  → Parse UPDATE Device.status
  → TDengine INSERT _{Channel}._{Product}
  → ETS {last_data, DeviceId} update
```

## 子设备树

```
dgiot_device:get_sub_device(DeviceId)
  → 查询 _Join:children:Product 或 subdevice关系
  → 递归获取所有子设备

parent → [child1, child2, ...]
网关  → 206 RTUs (all children of gw_131)
```

## 统计

```
源码:   10 apps, 150+ .erl files, 20,000+ lines
实体:   23 Parse类 + 3存储 + 7引擎 + 5组件 + 3体系 + 13协议 = 54+
关系:   15 _Join表 + 8 ontology relations + 子设备树
函数:   200+ exported API functions
ETS:    10+ named tables
优化点: 3 (path cache, role tree, batch ALTER)
```
