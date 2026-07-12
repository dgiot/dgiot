# Parse Relation System — 完整关系模型

## _Join 表 (15 张)

```
Parse 不使用外键——用 _Join:{A}:{B} 中间表表达关系

权限关系:
  _Join:users:_Role         (399)    用户 → 角色
  _Join:roles:_Role          (298)    角色层级继承 (树形)
  _Join:roles:_User          (67)     角色 → 用户 (反向)
  _Join:rules:_Role         (85,497)  ACL规则 → 角色 ← 最大!

菜单关系:
  _Join:menus:_Role        (8,492)   菜单 → 角色
  _Join:menuviews:_Role     (337)    菜单视图 → 角色
  _Join:views:_Role          (86)    视图 → 角色

产品关系:
  _Join:product:Channel    (1,332)   产品 → 通道
  _Join:children:Product     (30)    产品层级 (父子)

用户关系:
  _Join:users:_Session        用户 → 会话
  _Join:friend:_User          用户好友

通知关系:
  _Join:deletedBy:Notification  通知 → 删除者
  _Join:readBy:Notification     通知 → 已读者

应用关系:
  _Join:fda:App                 FDA应用 → App
  _Join:role:_User              (同 roles:_User)
```

## 本体关系 (io_ontology.json)

```
JSON-LD @context 定义的关系类型:
  hasServer       "拥有服务器"
  hasProcess      "运行进程"
  hasDataSource   "接入数据源"
  hasPort         "开放端口"
  connectsTo      "网络连接"

实例:
  IOMan --connectsTo(OPC DA/DCOM)--> DCS 1-5
  IO-131 --hasPort(Modbus TCP)--> :53001
  IO-131 --connectsTo(A11 5a5a)--> IO-130
  IoCommit --connectsTo(Oracle TNS)--> Oracle 129
```

## dgiot_ontology.erl 中的关系

```
ETS model: {Class -> #{properties, relations, rules}}

load_model 时:
  Relations = maps:get(<<"relations">>, Model, [])
  存入 ETS model 表

关系用于:
  forward-chain 推理 (reasoner)
  SWRL 规则导出
  因果图构建
```

## 三层关系对比

| 层 | 存储 | 示例 | 大小 |
|-----|------|------|------|
| Parse Join | PG JSONB | _Join:rules:_Role | 85K rows |
| Ontology | ETS/JSON-LD | connectsTo, hasPort | 8 relations |
| Thing Model | ETS model | product→channel, device→point | per class |
