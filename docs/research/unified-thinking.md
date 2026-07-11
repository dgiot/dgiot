# DLAS 统一架构：从物理世界到数字孪生的完整链路

## 一句话

**物模型定义"是什么"，本体定义"怎么关联"，gen_statem 定义"什么状态"，时序数据记录"发生了什么"。四者通过 MQTT topic（本体路径）串联。**

---

## 核心逻辑链

```
物模型                    本体                      影子 gen_statem           时序数据
(Thing Model)            (Ontology)                (Shadow Device)          (TDengine)
────────                 ────────                  ──────────────           ─────────

定义"是什么"             定义"在哪、怎么连"         定义"什么状态"           记录"发生了什么"

class: oil_well_rtu       Site: oil_field_01        State: online            iot_telemetry
properties:              └─ Gateway: gw_131         PID: <0.123.0>          ├─ device_id
  oil_pressure: float       └─ Device: rtu_001      Props: oil_pressure=2.35 ├─ point_id
  temperature:  float          └─ Point:            Rules[]:                 ├─ ts
  status:       enum               oil_pressure       if T>85 → alarm        ├─ value
                                     ↓                                        └─ quality
                                    MQTT Topic:
  dgiot/oil_field_01/gw_131/rtu_001/oil_pressure/data
```

**四个组件不是独立的——它们通过本体路径耦合在一起，形成一个完整的设备生命周期。**

---

## 分层解析

### 1. 物模型 (Thing Model) — 定义"是什么"

```json
{
  "class": "oil_well_rtu",
  "properties": [
    {"id": "oil_pressure",    "type": "float", "register": {"addr": 40300, "op": "readHoldingRegisters", "format": "float32_AB"}},
    {"id": "temperature",     "type": "float", "register": {"addr": 40304}},
    {"id": "pump_status",     "type": "int",   "register": {"addr": 40308}}
  ],
  "rules": [
    {"id": "R1", "when": {"property": "temperature", "op": ">", "value": 85},
     "then": {"state": "alarm", "action": "notify"}}
  ]
}
```

**物模型是静态的 Class 定义——一个产品模板。所有 oil_well_rtu 设备共享同一份物模型。**

### 2. 本体 (Ontology) — 定义"在哪、怎么连"

```
Site:   oil_field_01          ← 采油一厂
 └─ Gateway: gw_131           ← IO服务器 11.66.12.131
      ├─ protocols: [modbus_tcp:53001, a11:8889]
      ├─ processes:  [IOMan x36, CommBridge]
      └─ Device: rtu_001      ← 井口RTU (物理)
           ├─ class: oil_well_rtu  ← 关联物模型
           ├─ slaveid: 1
           └─ Point:
                ├─ oil_pressure    ← Modbus 40300
                ├─ temperature     ← Modbus 40304
                └─ pump_status     ← Modbus 40308
```

**本体定义了 Site→Gateway→Device→Point 四层拓扑。每一层都有关联关系：Gateway 属于 Site，Device 属于 Gateway，Point 属于 Device。**

本体加载过程：

```
dgiot_ontology:init()
  → ETS 三表: model / instance / rules

dgiot_ontology:load_model(oil_well_rtu)
  → 编译: properties[] → ETS model
  → 编译: rules[] → ETS rules (guard 子句)

dgiot_ontology:spawn_instance(oil_well_rtu, rtu_001)
  → {ok, ShadowPid}  ← gen_statem 进程启动
```

### 3. 影子 gen_statem (Shadow) — 定义"什么状态"

```erlang
init() → {ok, authenticate, Device}.

authenticate(cast, {mqtt, heartbeat}, Device) →
    {next_state, online, Device}.

online(cast, {data, Props}, Device) →
    evaluate(Rules, Props),        %% 物模型编译的规则
    NewState = transition(Props),   %% normal → alarm → offline
    bridge → Parse + TDengine,
    {next_state, NewState, Device}.

alarm(state_timeout, 60s, Device) →
    {next_state, critical, Device}.

critical/offline(cast, {mqtt, heartbeat}, Device) →
    {next_state, online, Device}.
```

**每个物理设备 = 1 个 gen_statem 进程。状态机由物模型的 rules[] 编译驱动。物理世界通过 MQTT 注入数据，影子评估规则后做状态迁移。**

关键：**影子进程是 Erlang OTP 原生进程——不是线程、不是协程、不是 goroutine。Erlang VM 的设计目标就是百万级轻量进程，每个进程独立 GC、独立崩溃域。**

### 4. 时序数据 (TDengine) — 记录"发生了什么"

```sql
-- 源码: dgiot_tdengine_schema.erl, dgiot_tdengine.hrl
-- 宏: ?Database(Name) = "_" ++ Name

CREATE DATABASE IF NOT EXISTS _5392ccb3d7 KEEP 10;

CREATE TABLE IF NOT EXISTS _2de1b3e1b8 (
    createdat TIMESTAMP,
    oil_pressure FLOAT,
    temperature FLOAT
) TAGS (
    devaddr NCHAR(50)     -- 强制标签
);

-- 影子进程每次 push_point 写入
INSERT INTO _5392ccb3d7._2de1b3e1b8
  USING _2de1b3e1b8
  TAGS ('02110120089')
  VALUES (NOW, 2.35, 192);
```

**所有对象名以下划线开头。devaddr 标签强制存在（不存在则自动添加 NCHAR(50)）。**

---

## 端到端数据流

```
物理世界:  RTU-001 温度传感器 → 25.6°C
  │
  │ Modbus TCP (FC3, addr 40304)
  ▼
边缘采集:  DeviceAccessManager → UnifiedPipeline → EdgeStreamEngine
  │          9种适配器之一        15种算法之一       range_check(25.6, 0, 85)
  │                                                    ↓ OK
  │ MQTT publish
  ▼
EMQX:     Topic: dgiot/oil_field_01/gw_131/rtu_001/temperature/data
           Payload: {ts:1751884800, v:25.6, q:192}
  │
  │ MQTT subscribe
  ▼
Shadow:   gen_statem:online ! {data, #{temperature => 25.6}}
           evaluate(Rules, Props)
           R1: temperature > 85? → false → 保持 online
  │
  ├─→ Parse:  update Device.status = online
  ├─→ TDengine: INSERT INTO ... VALUES (NOW, 25.6, 192)
  └─→ MQTT:   publish 状态变更 (如有)
```

---

## 为什么 Erlang/OTP

| 需求 | Erlang/OTP 原生能力 |
|------|-------------------|
| 百万设备 = 百万进程 | 轻量进程 (309 words/进程)，独立 GC |
| 设备不崩溃平台 | Supervisor 树，let it crash |
| 状态机 | gen_statem behaviour (OTP 标准) |
| 热升级 | release upgrade (appup/relup) |
| 分布式 | 原生分布式 (EPMD, global, pg2) |
| ETS 内存表 | 常量时间查找，存模型/规则/实例 |

**不是"用 Erlang 实现 IoT"——而是 Erlang 的设计目标恰好就是电信级容错 + 百万并发，与工业物联网天然对齐。**

---

## 与传统方案对比

| | DLAS (本方案) | AWS IoT | ThingsBoard |
|---|-------------|---------|-------------|
| 设备建模 | 本体 4 层 + 物模型 | Thing Shadow (扁平) | Device Profile |
| 状态机 | gen_statem 编译 | 无 (规则引擎) | 有限状态机 |
| 规则引擎 | 本体编译 + 边缘流式 (双层) | AWS IoT Rules | 规则链 |
| 时序 | TDengine supertable | Timestream | Cassandra/Timescale |
| 寻址 | MQTT topic = 本体路径 | MQTT topic = thingName | MQTT topic = deviceId |
| 设备孪生 | OTP 进程 (1:1) | JSON 文档 | JSON 文档 |
