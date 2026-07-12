# 点位→物模型→实时→历史→时序 全链路

## 数据流

```
物理点位 (Modbus 40300)
  ↓
物模型 (thing_model.json)
  identifier: oil_pressure, dataForm: {addr:40300, protocol:modbus, originaltype:float32_AB}
  ↓
实时数据 (ETS)
  {last_data, DeviceId} -> #{oil_pressure => 2.35, temperature => 45.6, ts => 1751884800}
  查找: <1us, 内存
  ↓
MQTT 上报
  Topic: $dg/thing/2de1b3e1b8/2de1b3e1b8_02110120089/properties/report
  Payload: {oil_pressure: 2.35, temperature: 45.6}
  ↓
Shadow gen_statem evaluate
  online(cast, {data, Props}) -> evaluate(Rules) -> state transition -> bridge
  ↓
TDengine INSERT (时序数据)
  dgiot_tdengine:create_object(Channel, TableName, Payload)
  -> INSERT INTO _85ef6b7459._2de1b3e1b8 USING _2de1b3e1b8
     TAGS('02110120089') VALUES (NOW, 2.35, 192)
  ↓
Parse/PG UPDATE (历史数据)
  dgiot_parse:update_object(Device, rtu_001, #{status => online, oil_pressure => 2.35})
```

## 三层存储

```
实时 (ETS)                 历史/关系 (Parse/PG)          时序 (TDengine)
─────────                  ──────────────────          ────────────────
dgiot_data:get              dgiot_parse:get_object      dgiot_tdengine:query_object
{last_data, DeviceId}       Device/rtu_001              _85ef6b7459._2de1b3e1b8
<1 us                       ~10 ms                      ~5 ms
内存, 进程内                 JSONB, 23类                 SuperTable/SubTable
当前值 only                 配置+关系+审计              全量历史+聚合
进程死亡=数据丢失           持久化                       持久化+压缩(10:1)
```

## TDengine 表结构 (从物模型生成)

```
物模型 -> TDengine Schema:

thing_model.properties[]:
  oil_pressure    float    40300  Modbus  ->  column: oil_pressure FLOAT
  temperature     float    40304  Modbus  ->  column: temperature FLOAT
  pump_status     int      40308  Modbus  ->  column: pump_status INT

thing_model.tags[] (or mandatory devaddr):
  devaddr         NCHAR(50)         ->  TAG: devaddr NCHAR(50)

生成:
  CREATE STABLE _{ProductId} (
    createdat TIMESTAMP,
    oil_pressure FLOAT,
    temperature FLOAT,
    pump_status INT
  ) TAGS (devaddr NCHAR(50))
```

## 查询路径

```
"rtu_001 现在的油压是多少?"
  → ETS: {last_data, rtu_001} -> oil_pressure=2.35  (<1us)

"rtu_001 过去1小时的温度趋势?"
  → TDengine: SELECT ts, value FROM _85ef6b7459.t_rtu001_temp
    WHERE ts > NOW-1h  (~5ms)

"rtu_001 的设备信息?"
  → Parse: GET /classes/Device/rtu_001  (~10ms)

"rtu_001 的告警规则?"
  → ETS: ets:lookup(dgiot_ontology_rules, R_HIGH_TEMP)  (<1us)
```
