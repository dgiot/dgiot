# 关系本体 — Parse 15 _Join 表完整模型

## 三层关系

```
设备↔设备    _Join:children:Product · _Join:product:Channel
人↔人       _Join:users:_Role · _Join:roles:_Role · _Join:friend:_User
人↔设备     _Join:rules:_Role · _Join:menus:_Role · _Join:views:_Role
```

## 设备↔设备

```
_Join:children:Product (30 rows)
  Product(parent) → Product(child)
  例: 油井产品 → 井口RTU子产品
  用途: 产品模板继承

_Join:product:Channel (1,332 rows)
  Product → Channel
  例: oil_well_rtu → Modbus_TCP:53001
  用途: 物模型→TDengine数据库映射
  → dgiot_tdengine_channel:check_database(ChannelId, ProductId)
```

## 人↔人

```
_Join:users:_Role (399 rows)
  _User → _Role
  例: 张三 → operator
  用途: 用户角色分配

_Join:roles:_Role (298 rows)
  _Role(child) → _Role(parent)
  例: operator → engineer → admin → root
  用途: 角色层级继承
  → dgiot_role:childrole([RoleId], []) 递归展开所有子角色

_Join:roles:_User (67 rows)
  _Role → _User
  例: operator → {张三, 李四, 王五}
  用途: 反向查询: 角色下所有用户

_Join:friend:_User
  _User → _User
  用途: 用户好友/协作关系
```

## 人↔设备 (通过权限链)

```
_Join:rules:_Role (85,497 rows) ← 最大!
  Permission(ACL规则) → _Role
  例: "油井读取权限" → operator
  用途: 角色拥有哪些ACL规则
  → dgiot_mqtt_acl:check_device_acl(Token, DeviceId, UserId)
     = User → Roles → childrole(recursive) → intersect(Device.ACL) → allow/deny

_Join:menus:_Role (8,492 rows)
  Menu → _Role
  例: 设备管理菜单 → operator
  用途: 角色可见哪些菜单

_Join:menuviews:_Role (337 rows)
  MenuView → _Role
  用途: 角色可见哪些菜单视图

_Join:views:_Role (86 rows)
  View → _Role
  用途: 角色可见哪些视图
```

## 人↔通知·会话·应用

```
_Join:deletedBy:Notification
  Notification → _User (删除者)
  用途: 审计追踪

_Join:readBy:Notification
  Notification → _User (已读者)
  用途: 已读状态追踪

_Join:users:_Session
  _User → _Session
  用途: 用户会话管理
  → dgiot_auth:get_session(Token)

_Join:fda:App
  FDA → App
  用途: 应用注册

_Join:role:_User
  _Role → _User
  用途: (同 _Join:roles:_User)
```

## 查询路径

```
"张三能不能读 rtu_001 的数据?"
  User(张三) → _Join:users:_Role → Role(operator)
  → dgiot_role:childrole(operator, []) → [operator, engineer, admin]
  → dgiot_device:get_roleids(rtu_001) → [operator]
  → intersect → allow ✅

"operator 角色能看到哪些菜单?"
  _Join:menus:_Role → Menu(设备管理, 数据分析, 告警管理...)

"rtu_001 用哪个 TDengine 数据库?"
  Device(rtu_001) → Product(oil_well_rtu)
  → _Join:product:Channel → Channel(TD_ch_85ef6b7459)
  → dgiot_tdengine:get_database(ChannelId, ProductId) → _85ef6b7459
```

## 本体关系 vs Parse 关系

```
本体关系 (io_ontology.json)    Parse _Join 表 (PG)
─────────────────────────     ──────────────────
物理连接: hasPort, connectsTo  产品-通道: product:Channel
数据流:   produces, forwards   权限: rules:_Role, menus:_Role
控制:     controls, triggers   角色: users:_Role, roles:_Role
语义:     JSON-LD @context      关系: SQL JOIN (无外键)
```
