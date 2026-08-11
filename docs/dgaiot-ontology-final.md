# dgaiot DLAS 本体 — 最终交付

> 方法: 心镜本体论四步法 | 37 commits | 12 docs

## Step 1: 盘点到齐

### Data 层 (23 实体类 + 3 存储引擎)

```
Parse 23类: Site, Gateway, Device, Point, Product, ProductTemplet,
  Channel, Dict, Category, _User, _Role, _Session, Menu, View,
  Timescale, Log, Notification, Evidence, Instruct, Permission,
  _SCHEMA, _Audience, _GraphQLConfig

TDengine:  _{ChannelId}._{ProductId} (devaddr NCHAR(50) mandatory)
ETS:       3 tables (model, instance, rules) <1us
Mnesia:    dgiot_data ETS keys (tdengine_db, ProductId→Channel, etc.)
```

### Logic 层 (7 引擎)

```
dgiot_ontology    187行  load_model + spawn_instance + registry
dgiot_tdengine_schema 173行  get_schema + create_table + alter_table
dgiot_parse_id    429行  MD5 encoding 30+ classes
dgiot_role         childrole recursive + ACL/CLP
dgiot_parse       708行  CRUD + batch + query
Rule Engine        gen_statem guard compilation
Reasoner           forward-chain + SWRL export
```

### Action 层 (5 组件)

```
Shadow gen_statem  192行  1:1 device process (编译运行通过)
EMQX Broker        :1883 :8081 :8083
Bridge             MQTT↔Parse sync_parse()
dgiot_mqtt_client  publish + subscribe + dlink auth
Command Validator  5重门 (range/rate/conflict/authority/echo)
```

### Security 层 (3 体系)

```
MQTT ACL    Device(ProductSecret) + User(Token+Role) + Superuser
Parse ACL   CLP(_SCHEMA) + ACL(per-object) + Role Hierarchy
Auth        dgiot_parse_auth login→sessionToken→check_session
```

### 协议 (13 种, 9 接入模式)

```
Modbus RTU/TCP · OPC UA/DA · A11 5a5a · BACnet/IP
S7 · DL/T 645 · HJ/T 212 · GB/T 26875 · MQTT · GPRS DTU(16)
接入: CONNECT|LISTEN|BRIDGE|AGENT|SERIAL|POLL|SUBSCRIBE|CUSTOM|DTU
```

## Step 2: 连线成网

### 15 Parse _Join 表

```
设备↔设备: _Join:children:Product(30) · _Join:product:Channel(1332)
人↔人:    _Join:users:_Role(399) · _Join:roles:_Role(298) · _Join:friend:_User
人↔设备:  _Join:rules:_Role(85497) · _Join:menus:_Role(8492) · _Join:views:_Role(86)
人↔通知:  _Join:deletedBy:Notification · _Join:readBy:Notification
人↔会话:  _Join:users:_Session
```

### 8 本体关系 (io_ontology.json)

```
hasServer · hasProcess · hasDataSource · hasPort · connectsTo
```

### 关键关系链

```
用户→角色→规则ACL→设备          (权限链)
产品→通道→TDengine数据库         (存储链)
传感器→设备→网关→场站            (拓扑链)
MQTT topic→EMQX→Shadow→TDengine  (数据链)
```

## Step 3: 设卡立规

### 寻址约束

```
objectId = MD5(ClassName + keyFields) 前10 hex → 确定性·免冲突·可寻址
TDengine  = _{ChannelId}._{ProductId}  (下划线前缀)
ClientID  = {ProductID}_{DevAddr}      (dlink auth)
Topic     = $dg/thing/{PID}/{DevAddr}/properties/report
```

### ACL 约束

```
$dg/ 前缀 → ACL检查; 其他 → 放行
Device:  ClientID={PID}_{DevAddr}, DevAddr必须在topic路径中
User:    SessionToken→Roles→childrole→intersect(DeviceACL)
默认:    不匹配任何规则 → deny
```

### 规则约束

```
temperature > 75 → L1 warning
oil_pressure > 3.0 → L2 alarm
error_count >= 3 → state escalation
heartbeat miss 30s → offline
devaddr NCHAR(50) → 强制存在 (不存在则自动添加)
```

### 协议约束

```
Modbus:    FC3读, FC6/16写, timeout=5s, retry=3
A11:       5a5a帧头, 8-bit sum校验, DevicePath变长
OPC DA:    DCOM协商, VARIANT解码, VT_R4→float
DL/T 645:  68h帧头, BCD解码, 累加和校验
HJ/T 212:  ##头, CP=&&数据区, CRC校验
```

## Step 4: 闭环验证

```
✅ MQTT pub→sub      3发3收  DLAS topic格式
✅ gen_statem         compile+run  init→online→alarm→online
✅ TDengine           CREATE TABLE + INSERT 15点 + SELECT 15行
✅ dlink auth         ClientID={PID}_{DevAddr} ProductSecret OK
✅ Audit              7 checks 0 critical
✅ Upgrade            475→25 suggestions
✅ Pipeline           Field→Modbus→MQTT→Shadow→TDengine full chain
```

## 统计

```
实体:   23 Parse类 + 3存储引擎 + 7引擎 + 5组件 + 3体系 + 13协议 = 54+
关系:   15 _Join表 + 8 ontology relations = 23
约束:   寻址(4) + ACL(4) + 规则(4) + 协议(10+) = 22+
验证:   6项全过
文档:   12 docs + 37 commits + 1 whitepaper PDF
```
