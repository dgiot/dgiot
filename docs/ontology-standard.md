# dgaiot 标准本体定义

## 定义

**本体 = 工业系统的完整语义模型。描述物理世界有什么、在哪、怎么连、谁说了算。**

## 四个部分

```
Ontology
├── 1. 物模型 (Thing Model)        "是什么"
├── 2. 拓扑   (Topology)           "在哪、怎么连"
├── 3. 通道   (Channel)            "怎么存"
└── 4. 授权   (ACL)                "谁能访问"
```

## 1. 物模型 (Thing Model)

```
ProductTemplet {
  properties[]  {
    identifier   "oil_pressure"
    name         "油压"
    dataForm     {address, protocol, operatetype, originaltype, strategy...}
    dataType     {type: float|int|bool|string|enum, specs}
    accessMode   r | w | rw
    isstorage    true | false
  }
  events[]      {identifier, name, type, params[]}     %% 告警·状态变更
  services[]    {identifier, name, input[], output[]}   %% 指令·配置
  rules[]       {when: {property, op, value}, then: {state, severity, action}}
}
```

## 2. 拓扑 (Topology)

```
4 层 record (Erlang):

-record(site,    {id, name, type, location}).
-record(gateway, {id, ip, site, protocols[], devices[]}).
-record(device,  {id, gateway, name, type, protocol, slaveid, points[]}).
-record(point,   {id, device, name, unit, range, alarm}).

MQTT Path: dgiot/{site}/{gateway}/{device}/{point}/data
```

## 3. 通道 (Channel)

```
Channel {
  cType        "TD" | "MQTT" | "Modbus" | ...
  product      -> Product
  config       {host, port, database, keep...}
}

TDengine 映射:
  Database   = _{ChannelId}
  SuperTable = _{ProductId}     (properties -> columns, tags -> TAGS)
  SubTable   = ... USING _{ProductId} TAGS(devaddr, ...)
```

## 4. 授权 (ACL)

```
三层:
  Device:  ClientID = {ProductID}_{DevAddr} · ProductSecret
  User:    ClientID = {Token}{Type} · SessionToken -> Role -> check_device_acl
  Super:   Username = "dgiot" (127.0.0.1 免检)

Topic ACL: $dg/thing/{PID}/... (设备) · $dg/user/... (用户)
```

## DLAS 四层架构

```
Security    auth · role · ACL/CLP · Hooks
Action      Shadow(gen_statem) · Bridge · MQTT · Rule
Logic       Ontology Engine · Model Registry · Reasoner
Data        Parse/PG(23类) · TDengine(_{ChannelId}) · ETS(3表) · EMQX
```

## 存储三层

```
ETS        内存  {model, instance, rules}  <1us
Parse/PG   关系  JSONB 23类                ~10ms
TDengine   时序  SuperTable/SubTable       ~5ms
```

## FDE 管道

```
Model → Ontology → Device Access → TimeSeries → Rules → Dashboard
  1        2           3              4           5          6
```

## 唯一性约束

```
物模型: ProductTemplet.objectId 全局唯一
拓扑:   Device.devaddr 在 product 内唯一
通道:   {ChannelId, ProductId} -> {tdengine_db} ETS 唯一
授权:   {ProductID}_{DevAddr} ClientID 格式唯一
TDengine: devaddr NCHAR(50) Tags 强制存在
```
