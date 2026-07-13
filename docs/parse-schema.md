# Parse 库完整结构 — 23 类

## 本体核心 (4)

### Site
```
objectId, name, type, location
用途: 场站定义 (oil_field_01)
```

### Gateway  
```
objectId, ip, site->Site, hostname, protocols[], devices[]
用途: IO服务器 / 边缘网关
注: Gateway = Device(device_type="gateway"), 复用 get_deviceid
```

### Channel
```
objectId, cType, name, product->Product, isEnable, status, config
编码: get_channelid(Type, CType, Name) -> MD5 10hex
实例: 85ef6b7459 (Modbus TCP通道)
```

### Device
```
objectId, devaddr*, name, product->Product*, ip, status, isEnable, basedata, profile
编码: get_deviceid(ProductId, DevAddr) -> MD5 10hex
实例: 02110120089 (井口RTU)
```

## 物模型 (3)

### Product
```
objectId, devType*, name*, category, producttemplet->ProductTemplet, thing, icon, nodeType
编码: get_productid(CategoryId, DevType, Name)
实例: 油井RTU, 注水泵
```

### ProductTemplet
```
objectId, name, icon, thing({properties,events,services}), decoder, config
编码: get_producttempletid(CategoryId, Name)
实例: oil_well_rtu 模板 (42 properties)
```

### Point
```
无独立Parse类 — 通过 thing_model.properties[] 定义
identifier, dataType, dataForm(address,protocol,originaltype), alarm, range
```

## 系统管理 (6)

### _User
```
objectId, username, password_hash, email, role, sessionToken
编码: get_userid(UserName) -> MD5
```

### _Role
```
objectId, name, alias, parent_id
编码: get_roleid(Name) -> MD5
关联: _Join:users:_Role(399) + _Join:roles:_Role(298) 层级树
```

### _Session
```
objectId, sessionToken, user->_User, expiresAt
编码: get_sessionId(Token) -> MD5
```

### Menu
```
objectId, name, path, icon, group, order, parent->Menu
编码: get_menuid(Name) -> MD5
关联: _Join:menus:_Role(8492)
71 菜单项
```

### View
```
objectId, name, path, config
关联: _Join:views:_Role(86) + _Join:menuviews:_Role(337)
```

### Permission
```
objectId, name
编码: get_ruleid(Name) -> MD5
关联: _Join:rules:_Role(85497) — 最大关联表
321 权限条目
```

## 业务配置 (4)

### Dict
```
objectId, class, key, title, type, dict->Dict (树形)
编码: get_dictid(Key,Type,Class,Title)
```

### Category
```
objectId, name, level, order, parent->Category
编码: get_categoryid(Level, Name)
```

### Timescale
```
objectId, device_id, point_id, storage
时序存储配置
```

### Channel (config)
```
cType=TD | MQTT | Modbus | OPC | HTTP | ...
config: {host, port, database, keep, ...}
```

## 运营数据 (6)

### Instruct
```
objectId, device->Device, pn, di, command, params, status
编码: get_instructid(DeviceId, Pn, Di)
```

### Notification
```
objectId, user->_User, title, body, status, type
关联: _Join:deletedBy + _Join:readBy
```

### Log
```
objectId, device_id, level, message
设备日志
```

### Devicelog
```
objectId, device->Device, devaddr, operator, timestamp, action
编码: get_devicelogid(DeviceId, DevAddr)
```

### Evidence
```
objectId, device_id, ukey, file_url, type, timestamp
编码: get_evidenceId(Ukey, Timestamp)
```

### Files / Git / Article / Maintenance
```
Files:       path, name -> get_filesId(Path,Name)
Git:         id, ts -> get_gitid(Id,Ts)
Article:     projectId, timestamp -> get_articleid(ProjectId,Timestamp)
Maintenance: device, number -> get_maintenanceid(Deviceid, Number)
```

## 关系 (_Join 表, 15张)

```
设备↔设备:
  _Join:children:Product      (30)   产品父子
  _Join:product:Channel       (1332) 产品→通道

人↔人:
  _Join:users:_Role           (399)  用户→角色
  _Join:roles:_Role           (298)  角色层级
  _Join:roles:_User            (67)  角色→用户
  _Join:friend:_User                 用户好友

人↔权限:
  _Join:rules:_Role          (85497) 规则→角色
  _Join:menus:_Role           (8492) 菜单→角色
  _Join:menuviews:_Role        (337) 菜单视图→角色
  _Join:views:_Role             (86) 视图→角色

人↔设备(通过权限间接):
  _Join:users:_Session               用户→会话
  _Join:role:_User                   角色→用户

通知:
  _Join:deletedBy:Notification       删除者
  _Join:readBy:Notification          已读者

应用:
  _Join:fda:App                     FDA应用
```

## MD5 编码规则 (dgiot_parse_id.erl)

```
全部 class -> get_xxxid(keys) -> MD5("ClassName"+keys) 前10 hex

确定性·幂等·免冲突·可寻址
```
