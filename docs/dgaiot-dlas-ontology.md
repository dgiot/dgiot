# dgaiot DLAS 本体 — 四层完整盘点

> 方法: 心镜本体论四步法 (盘点到齐 → 连线成网 → 设卡立规 → 闭环验证)

## Step 1: 盘点到齐

### Data 层 (23+ 实体)

```
存储引擎:
  Parse/PG   23类 JSONB (:7432)
  TDengine   时序 _{ChannelId}._{ProductId} (:6041)
  ETS        3表 model/instance/rules (<1us)

物模型:
  thing_model.json      125 properties (Modbus float32_AB/uint16)
  ProductTemplet        66 templates (属性+事件+服务)
  Product               5 definitions
  Device                211 registrations

本体拓扑:
  Site                  1 (oil_field_01)
  Gateway               5 (IO-131 + 4 DCS endpoints)
  Device                45 (206 RTU + 5 DCS + 31 wireless + ...)
  Point                 125 per product

关系:
  _Join:rules:_Role     85,497 rows (ACL)
  _Join:menus:_Role      8,492 rows (菜单)
  _Join:product:Channel  1,332 rows (产品通道)
  _Join:users:_Role        399 rows (用户角色)
  _Join:roles:_Role        298 rows (角色层级)
```

### Logic 层 (7 引擎)

```
dgiot_ontology:  load_model + spawn_instance + registry (187行)
dgiot_tdengine_schema: get_schema + create_table + alter_table (173行)
dgiot_parse_id:  MD5编码 30+类 (429行)
dgiot_role:      childrole递归 + ACL/CLP
dgiot_parse:     CRUD + batch + query (708行)
Rule Engine:     guard clauses compiled from thing_model.rules[]
Reasoner:        forward-chain + SWRL export
```

### Action 层 (5 组件)

```
gen_statem Shadow:  1:1 device process (192行, 编译运行通过)
EMQX Broker:        :1883 :8081 :8083 (million-device)
Bridge:             MQTT<->Parse sync_parse()
dgiot_mqtt_client:  publish + subscribe + dlink auth
Command Validator:  range/rate/conflict/authority/echo 5重门
```

### Security 层 (3 体系)

```
MQTT ACL:    Device(User/ProductSecret) + User(Token+Role) + Superuser
Parse ACL:   CLP(类级) + ACL(对象级) + Role Hierarchy
Auth:        dgiot_parse_auth login->sessionToken->check_session
```

## Step 2: 连线成网 (核心关系)

```
传感器 → [监测] → 设备 → [安装] → 网关 → [归属] → 场站
设备   → [注册] → Product → [映射] → Channel → [存储] → TDengine
测点   → [定义] → thing_model.property → [编译] → gen_statem guard
数据   → [MQTT] → EMQX → [评估] → Shadow → [状态迁移] → Parse.update
Shadow → [推送] → dgiot_ontology:push_point → [写入] → TDengine INSERT
用户   → [登录] → SessionToken → [角色] → check_device_acl → [允许/拒绝]
```

## Step 3: 设卡立规 (关键约束)

```
寻址约束:
  objectId = MD5(ClassName + keyFields) 10 hex chars (确定性)
  TDengine DB = _{ChannelId}, Table = _{ProductId}
  devaddr NCHAR(50) 强制存在

ACL约束:
  $dg/ 前缀检查, 其他直接放行
  ClientID = {PID}_{DevAddr}, Username = {PID}
  DeviceAddr 必须在 topic 路径中

规则约束:
  temperature > 75 → alarm L1
  oil_pressure > 3.0 → alarm L2
  error_count >= 3 → state escalation
  heartbeat miss 30s → offline

存储约束:
  _{ChannelId}._{ProductId} 唯一
  alter_table 自动同步列定义
  ETS {last_data, DeviceId} 实时缓存
```

## Step 4: 闭环验证

```
MQTT 管道:     pub → sub 3/3 verified ✅
gen_statem:    init→online→alarm→online verified ✅
TDengine:      INSERT 15点 → SELECT 15行 verified ✅
dlink auth:    ClientID={PID}_{DevAddr} verified ✅
审计:          7 checks, 0 critical ✅
场景升级:      475→25 suggestions ✅
白皮书:        7 pages PDF ✅
```

```
统计:
  Data:    23+实体, 15 JOIN表, 3存储引擎
  Logic:   7引擎, 125属性, 66模板
  Action:  5组件, 1:1影子进程
  Security: 3体系, 85K ACL规则-角色关联
  关系:    15 _Join表 + 8 ontology relations
  约束:    寻址+ACL+规则+存储 4类
  验证:    6项闭环全部通过
```
