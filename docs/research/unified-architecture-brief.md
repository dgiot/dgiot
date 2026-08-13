# DLAS 统一 IIoT 架构：研究简报

## 文本架构图

```
 ┌──────────────────────────────────────────────────────────────────────────────┐
 │                          SECURITY LAYER                                      │
 │          auth · role · ACL/CLP · beforeSave/afterSave · audit · JWT          │
 └──────────────────────────────────────────────────────────────────────────────┘

 ┌──────────────────────────────────────────────────────────────────────────────┐
 │                          ACTION LAYER                                        │
 │  ┌──────────────────────┐   ┌──────────────┐   ┌──────────────────────┐      │
 │  │  Shadow gen_statem   │   │  Bridge      │   │  dgiot_mqtt          │      │
 │  │  (device lifecycle)  │◄─►│  (MQTT↔Parse)│◄─►│  (pub/sub +          │      │
 │  │  init→auth→online→   │   │  + sync_parse│   │   ont_push_point)    │      │
 │  │  alarm→offline       │   └──────────────┘   └──────────────────────┘      │
 │  └──────────┬───────────┘                                                     │
 │             │                                                                 │
 │  ┌──────────▼───────────┐   ┌──────────────────┐   ┌──────────────────┐      │
 │  │  tqics_action        │   │  物模型/指令下发  │   │  dgiot_task      │      │
 │  │  (L1/L2/L3 dispatch) │   │  (写寄存器/OTA)  │   │  (workflow)      │      │
 │  └──────────────────────┘   └──────────────────┘   └──────────────────┘      │
 └──────────────────────────────────────────────────────────────────────────────┘

 ┌──────────────────────────────────────────────────────────────────────────────┐
 │                           LOGIC LAYER                                        │
 │  ┌────────────────────┐  ┌───────────────┐  ┌────────────────────────────┐   │
 │  │ Ontology Engine    │  │ Model         │  │ dgiot_ontology_rule        │   │
 │  │ (load_model →      │──│ Registry      │──│ (compile → evaluate        │   │
 │  │  compile → spawn)  │  │ (3 ets tables)│  │  → match → trigger)        │   │
 │  └──────────┬─────────┘  └───────────────┘  └────────────────────────────┘   │
 │             │                                                                │
 │  ┌──────────▼──────────┐  ┌────────────┐  ┌───────────────────────┐         │
 │  │ Thing Model [JSON]  │  │ Relations  │  │ Reasoner              │         │
 │  │ class/properties/   │──│ (connect/3 │──│ (forward-chain/       │         │
 │  │ rules/relations     │  │  inverse)  │  │  OWL export / SWRL)   │         │
 │  └─────────────────────┘  └────────────┘  └───────────────────────┘         │
 └──────────────────────────────────────────────────────────────────────────────┘

 ┌──────────────────────────────────────────────────────────────────────────────┐
 │                           DATA LAYER                                         │
 │  ┌───────────┐  ┌──────────────┐  ┌─────────────────────┐  ┌─────────────┐  │
 │  │  Parse    │  │  PostgreSQL  │  │  TDengine            │  │  EMQX       │  │
 │  │  23 类    │  │  (relational)│  │  Database=_+ChannelId │  │  (MQTT     │  │
 │  │  Device/  │  │  Site/Gate- │  │  SuperTable=_+ProdId  │  │   broker)  │  │
 │  │  Product  │  │  way/Device │  │  SubTable=_+MD5(DevId)│  │  1883/8081 │  │
 │  │  Channel/ │  │  /Point     │  │  iot_telemetry(ts,    │  │            │  │
 │  │  Menu/Log │  │              │  │  value, quality)      │  │            │  │
 │  └───────────┘  └──────────────┘  └───────────────────────┘  └─────────────┘  │
 └──────────────────────────────────────────────────────────────────────────────┘

                    ▲  MQTT/SQL 直连
                    ▼
 ┌──────────────────────────────────────────────────────────────────────────────┐
 │                    EDGE COLLECTOR LAYER (dgiot_collector)                     │
 │                                                                              │
 │  ┌─────────────────────────────────────────────────────────────────────┐     │
 │  │                  DeviceAccessManager (9 种适配器)                    │     │
 │  │  ┌─────────┐  ┌──────────┐  ┌──────────┐  ┌─────────┐  ┌────────┐  │     │
 │  │  │ModbusTCP│  │ModbusRTU │  │OPC UA    │  │OPC DA   │  │IEC 104 │  │     │
 │  │  │Connect  │  │Over DTU  │  │Connect   │  │Agent→MQ │  │Bridge  │  │     │
 │  │  │静态IP   │  │LISTEN 4G │  │opc.tcp://│  │TT订阅    │  │双向透传 │  │     │
 │  │  └────┬────┘  └────┬─────┘  └────┬─────┘  └────┬────┘  └────┬───┘  │     │
 │  │       │            │             │             │            │       │     │
 │  │  ┌────▼────┐ ┌─────▼──────┐ ┌────▼─────┐ ┌────▼────┐ ┌────▼───┐ │     │
 │  │  │ModbusRTU│ │HTTP Poll  │ │MQTT      │ │Custom   │ │Bridge  │ │     │
 │  │  │SERIAL   │ │(HTTP API) │ │Subscribe  │ │Protocol │ │Bypass→ │ │     │
 │  │  │RS-485   │ │           │ │无线变送器 │ │A11/bin  │ │Takeover │ │     │
 │  │  └─────────┘ └───────────┘ └───────────┘ └──────────┘ └─────────┘ │     │
 │  └─────────────────────────────────────────────────────────────────────┘     │
 │                                                                              │
 │  ┌─────────────────────────────────────────────────────────────────────┐     │
 │  │                  UnifiedPipeline (主循环编排器)                      │     │
 │  │                                                                      │     │
 │  │  ┌──────────────────────────────────────────────────────────────┐   │     │
 │  │  │  EdgeStreamEngine (15 种流式算法, 滑动窗口)                  │   │     │
 │  │  │  ┌──────┐ ┌──────┐ ┌──────┐ ┌────────┐ ┌─────────┐          │   │     │
 │  │  │  │range │ │slid- │ │sudden│ │trend   │ │volati-  │ ...15种  │   │     │
 │  │  │  │check │ │ingAvg│ │change│ │detect  │ │lity     │           │   │     │
 │  │  │  └──────┘ └──────┘ └──────┘ └────────┘ └─────────┘          │   │     │
 │  │  └──────────────────────────────────────────────────────────────┘   │     │
 │  │                           │ alarm_id > 0                          │     │
 │  │  ┌──────────────────────────────────────────────────────────────┐   │     │
 │  │  │  AlertPipeline (6 阶段告警闭环)                              │   │     │
 │  │  │  Dedup → WorkOrder → Notification → Knowledge → Analytics→Trace│   │     │
 │  │  └──────────────────────────────────────────────────────────────┘   │     │
 │  └─────────────────────────────────────────────────────────────────────┘     │
 │                                                                              │
 │  ┌──────────────────────────────┐  ┌──────────────────────┐                  │
 │  │  DgiotAdapter (平台同步)     │  │  TDengineManager     │                  │
 │  │  REST API + MQTT 双通道      │  │  批量写入 (5s 缓冲)  │                  │
 │  └──────────────────────────────┘  └──────────────────────┘                  │
 └──────────────────────────────────────────────────────────────────────────────┘
```

## 组件关系

### 1. 物模型 -> 本体引擎 -> gen_statem：编译时绑定

系统的核心创新在于**物模型通过编译加载为本体**。这一过程在 `dgiot_ontology.erl` 中实现：

```
thing_model.json ──► load_model/1 ──► compile_model/2 ──► gen_statem .erl 源码
                      (ETS 存储)        (代码生成)

gen_statem 模块：
  - 状态: init → authenticate → online → alarm → offline
  - 规则嵌入为模式匹配 guard 子句
  - 运行时规则评估兜底 (dgiot_ontology_rule:evaluate/3)
```

`dgiot_ontology:load_model/1` 接收物模型 JSON，包含以下字段：
- `class` —— 设备类别（如 compressor, pumping_unit）
- `sub_class` —— OWL 继承（如 equipment）
- `properties[]` —— 含 id、type、unit 的属性数组
- `relations[]` —— 与其他本体的对象属性连接
- `rules[]` —— SWRL 风格的条件-动作规则，含严重度（L1/L2/L3）

再将此模型编译为成熟的 OTP gen_statem 模块。规则以双重路径嵌入：路径 A 将其编译为 Erlang guard 子句，实现纳秒级匹配；路径 B 保留运行时 `dgiot_ontology_rule:evaluate/3` 评估器，支持动态规则追加，无需代码重编译。

### 2. Shadow -> 本体实例：运行时 1:1 映射

`dgiot_ontology:spawn_instance(Class, InstanceId)` 为每个物理设备创建一个 gen_statem 进程：

```
Shadow 进程 (gen_statem)
  Record: #device{id, product_id, name, type, ontology_path,
                  points=[], status, last_online, error_count}
  消息处理:
    {data, Values}      ──► update_points() ──► push_point() ──► MQTT
    {event, Event}      ──► 状态迁移 (online→alarm)
    heartbeat           ──► 保活计时器重置
    state_timeout       ──► 120s 无心跳 → offline
```

此处的注册表 `tqics_registry` 使用三个映射来维护 OWL 风格的本体注册表：
- `by_id` —— 每个实体的 #{class, pid, meta, registered_at}
- `by_class` —— 类别 -> {Id -> Pid} 索引
- `relations` —— 关系图（FromId -> Relation -> [ToId]）

`tqics_registry:connect(FromId, Relation, ToId)` 实现为 OWL 对象属性——通过消息传递实现进程间的语义关系。例如，`Equipment.executes → Process` 会变为发送给双方进程的 `{relation_added, executes, TargetId, TargetPid}` 消息。

### 3. 测点映射 -> 本体路径 -> MQTT 主题：结构化寻址

每个测点（点位）均携带明确的**本体路径**，从物理世界的 Modbus 寄存器地址开始：

```
物理世界:  Modbus 寄存器地址 40300 (oil_pressure)
  采集层 (dgiot_collector):
    ThingModel 测点定义:
      field: oil_pressure
      address: 40300
      data_type: float32
      byte_order: little_endian  (中科网关默认)
      scale: 0.01               (压力类除以100)
      unit: MPa
      group: G1                 (基础工况组)

  本体层 (dgiot_ontology):
    Site:      oil_field_01      ← 采油厂
    ├── Gateway: gw_131          ← IO 服务器 (Modbus 主站)
    │   ├── Device: rtu_001      ← 井口 RTU
    │   │   ├── Point: oil_pressure    ← Modbus 40300
    │   │   ├── Point: casing_pressure ← Modbus 40302
    │   │   └── Point: temperature     ← Modbus 40304

MQTT 主题: dgiot/{site}/{gateway}/{device}/{point}/data
    原始结构 = 分层语义标识符

TDengine 存储:
    Database:     _24b9b4bc50          (= _ + ChannelId)
    SuperTable:   _82e47fe45f          (= _ + ProductId)
    SubTable:     _f2c14dd153          (= _ + MD5(Device+ProdId+DevAddr)[:10])
    Columns:      ts, value, quality   Tags: device_id, point_id, unit
```

测点映射链中有一个关键细节：**厂商映射层**。`dgiot_collector` 中的 `VendorMapper` 在物理值（Modbus 原始十六进制 -> 工程值）与本体属性（工程值 -> 物模型字段）之间建立桥梁。不同的 DTU 厂商可能使用不同的字节序（大端/小端）或缩放因子；`vendor_mapping.py` 标准化了这些差异，使得本体层能够以开放格式接收统一的值，而不受特定厂商的 Modbus 实现约束。

系统预定义的 G1-G8 标准寄存器组如下：

| 分组 | 寄存器基础 | 寄存器数 | 覆盖范围 | 示例测点 |
|------|----------|---------|----------|---------|
| G1 | 40300 | 26 | 基础工况 | 油压、套压、回压、载荷、位移、动液面 |
| G2 | 40351 | 15 | 电参 | 电流ABC、电压ABC、有功/无功/视在功率 |
| G3 | 40400 | 5 | 变频参数 | 频率、变频输出、变频器温度、故障码 |
| G4 | 40420 | 5 | 抽油机特有 | 冲次、冲程、泵径 |
| G5 | 40430 | 4 | 螺杆泵特有 | 转速、扭矩、扬程、容积效率 |
| G6 | 40440 | 10 | 报警诊断 | 终端状态、仪表故障码、AI报警码 |
| G7 | 40450 | 30 | 仪表扩展 | 无线压力、无线温度、示功仪数据、传感器电量 |
| G8 | 40550 | 15 | 变频扩展 | 功率平衡、间抽控制、PID |

`dgiot_ontology:get_path(PointId)` 通过遍历 Parse 中 Device、Gateway、Site 对象的外键关系来重构本体路径。`dgiot_ontology:push_point(PointId, Value)` 将值发布到该主题，携带时间戳、值和质量（QoS 192 = 良好）。

### 4. 规则 -> 动作 -> L1/L2/L3 分级响应

规则引擎 `dgiot_ontology_rule` 采用基于严重度的分级响应架构：

| 级别 | 延迟 | 模式 | 示例 |
|------|----------|------|---------|
| L1（严重） | <2s | 自动执行 | 健康值<60 -> 紧急停机 |
| L2（重要） | <30s | 人机协同 | 置信度<85% -> 升级人工审核 |
| L3（一般） | 通知 | 人工主导 | 健康值下降 -> 趋势预警 |

`tqics_action.erl` 将此映射到具体的规则处理器：`?RULE_P2_HEALTH_LOW` 触发维护请求，`?RULE_D1_SEAL_OFFSET` 触发供料器自动调整，`?RULE_A2_ESCALATE` 升级告警。

在边缘采集层（dgiot_collector），还有第二套 15 种实时流式算法作为补充，用于油气行业特有的工况诊断：

| 算法 | 触发条件 | 告警示例 |
|----------|------|-------|
| range_check | 超限检测 | 油压超出 [0, 40] MPa |
| sliding_avg | 滑动平均 | 电流平均值 > 80A |
| sudden_change | 突变检测 | 载荷突变 > 20kN |
| trend_detect | 趋势检测 | 压力持续上升 |
| volatility | 波动检测 | 电流标准差 > 10A |
| threshold_count | 超限计数 | 窗口内10点中超过5个 > 35 |
| rate_of_change | 变化率检测 | 相邻点平均变化 > 2MPa |
| cumulative_sum | 累积和检测 | 日耗电量 > 1000kWh |
| peak_detect | 峰值检测 | 峰值 > 40MPa |
| valley_detect | 谷值检测 | 谷值 < 8MPa |
| continuous_abnormal | 连续异常 | 连续3个点 > 75A |
| deviation_from_baseline | 基线偏离 | 偏离基线 > 50% |
| ratio_check | 比值检测 | 三相电流不平衡 < 0.8 |
| difference_check | 差值检测 | 油压-套压差值异常 |
| periodic_pattern | 周期模式 | 冲次偏离 > 50% |

两层规则之间的关系是正交的：**本体层规则**（通过 gen_statem guard 子句）管理设备状态转换（normal -> warning -> fault）；**边缘层规则**（通过 EdgeStreamEngine 滑动窗口）管理油气行业特有的工况诊断。前者由物模型编译而来，后者由 `stream_tasks.yaml` 配置驱动。

### 5. 边缘采集架构：9 种访问模式

`dgiot_collector` 中的 `DeviceAccessManager` 通过工厂方法 `_create_adapter()` 支持 9 种不同的物理接入模式，每种模式对应不同的物联网设备连接场景：

| 模式 | 适配器 | 适用场景 | 协议 |
|------|---------|-------------|--------|
| CONNECT | ModbusTCPConnectAdapter | 静态 IP 网关 | Modbus TCP |
| LISTEN | ModbusRTUOverDTUAdapter | 4G DTU 动态 IP | Modbus RTU over TCP |
| BRIDGE | BridgeModeAdapter | 存量网关透传，只读旁路 | 帧级双向转发 |
| AGENT | AgentSubscribeAdapter | OPC DA Agent if→MQTT 订阅 | MQTT 接收 |
| SERIAL | ModbusRTUSerialAdapter | 本地 RS-485 总线 | Modbus RTU |
| POLL | HTTPPollAdapter | HTTP API 设备 | REST API |
| SUBSCRIBE | AgentSubscribeAdapter | 无线变送器 | MQTT |
| CUSTOM | CustomProtocolAdapter | 二进制私有协议 | A11 等 |
| DTU | DtuListener | 5 厂商 DTU 注册帧 | RTU 透传 |

所有适配器通过 `_emit_data()` 方法输出标准化的 `CollectedDataPoint` 格式，然后通过 `UnifiedPipeline.process_data_points()` 传递至后续处理。这种适配器工厂模式意味着添加新协议只需实现统一接口，而无需更改管道本身。

## 数据流：端到端

```
物理设备 ──► 9 种适配器 ──► UnifiedPipeline ──► EdgeStreamEngine ──► TDengine + AlertPipeline
                                          └──► DgiotAdapter ──► Shadow gen_statem ──► 规则评估 ──► 状态转换 ──► Parse

步骤 1：物理采集（9 种模式之一）
  ┌─ CONNECT 模式 ───────────────────────────────────┐
  │ Modbus TCP 读取中科网关寄存器 40300               │
  │ → 原生 Socket（非 pymodbus），自定义 MBAP 帧      │
  │ → 原始值 = 0x4016B852                            │
  │ → VendorMapper: scale=0.01, byte_order=little    │
  │ → 工程值: 2.35 MPa                               │
  │ → 输出: CollectedDataPoint(field=oil_pressure,   │
  │        value=2.35, ts=1751884800, quality=192)   │
  └──────────────────────────────────────────────────┘

  网关迁移支持三种渐进阶段:
    阶段 1 — 旁路 (Bypass):  只读监听，不影响原 IO Server
    阶段 2 — 接管 (Takeover): 处理数据并透传至原 IO Server
    阶段 3 — 稳定 (Stable):  边缘中枢独立运行，不依赖原 IO Server

步骤 2：统一管道
  UnifiedPipeline.process_data_points(points):
    └► EdgeStreamEngine.process(point)     ──► 缓冲至 TDengine (批量 5s)
    └► alarm_id > 0 → AlertPipeline        ──► 6 阶段闭环
    └► DgiotAdapter.push(message)          ──► MQTT 至平台

步骤 3：边缘流式计算
  per device_id + field_name 滑动窗口（配置化窗口大小）:
    特征向量: {oil_pressure_avg, oil_pressure_min, oil_pressure_max,
               oil_pressure_rate, oil_pressure_std, oil_pressure_trend}
    15 种规则算法 → alarm_id 0-15

步骤 4：告警闭环（6 阶段）
  ┌────────────────────────────────────────────────┐
  │ 1. DedupEngine  (就地去重，防重复告警)         │
  │ 2. WorkOrder    (工单生命周期状态机)            │
  │ 3. Notification (分级推送: 电话/短信/APP)      │
  │ 4. Knowledge    (知识沉淀，可检索)              │
  │ 5. Analytics    (多维分析聚合)                  │
  │ 6. Trace        (全链路追溯，devid→alarm→工单)  │
  └────────────────────────────────────────────────┘

步骤 5：平台同步（双通道）
  DgiotAdapter:
    通道 1 — REST API:  批量上报设备档案 + 告警
    通道 2 — MQTT:      实时告警推送 + 控制指令下发

步骤 6：Shadow 注入（平台侧）
  dgiot_shadow:inject(Pid, {data, #{<<"oil_pressure">> => 2.35}})
    └► gen_statem:cast(Pid, {data, Values})
    └► online(cast, {data, Values}, Device) ->
        NewDevice = update_points(Device, Values),
        sync_parse(NewDevice),
        {keep_state, NewDevice, [{state_timeout, 120000, heartbeat_missed}]};

步骤 7：规则评估（双重路径）
  路径 A — 编译时 guard（纳秒级）:
      online(cast, {oil_pressure, V}, Data) when V > 15.0 -> {next_state, alarm, ...}
      └► 警戒阈值编码在 gen_statem 函数子句中

  路径 B — 运行时规则引擎（毫秒级）:
      online(cast, {eval_rules, Props}, Data) ->
          case dgiot_ontology_rule:evaluate(Rules, Props, Data) of
              [] -> keep_state_and_data;
              Matched -> dgiot_ontology_rule:trigger(Matched, Props, Data)
          end
      └► 复杂条件（AND/OR 组合，可动态追加）

步骤 8：状态转换
  normal → warning (80>健康值>=60)
  warning → fault (健康值<60)
  fault → online (健康值>=80 恢复)
  online → offline (120s 心跳超时)

  每次状态转换：
    └► sync_parse(Device)  更新 Parse 中 Device.status
    └► 触发 OWL 关系通知
    └► tqics_audit:log(...)  安全审计轨迹

步骤 9：持久化（两条路径）
  路径 1 — Parse（设备状态、元数据、当前值）:
      dgiot_parse:update_object(<<"Device">>, DeviceId,
          #{<<"status">> => online, <<"last_online">> => 1751884800})

  路径 2 — TDengine（时序遥测）:
      SuperTable: iot_telemetry TAGS(device_id, point_id, unit)
      Columns:    ts TIMESTAMP, value FLOAT, quality INT
      INSERT INTO _f2c14dd153 USING _82e47fe45f TAGS(...) VALUES(now, 2.35, 192)
      命名: Database=_+ChannelId, SuperTable=_+ProductId, SubTable=_+MD5(Id)[:10]
```

## 与传统 IoT 平台的关键创新与差异

### 1. 本体->OTP 编译，而非解释型规则引擎

传统 IoT 平台（AWS IoT、Azure IoT Hub、ThingsBoard）均使用解释型规则引擎——在运行时评估规则会产生线性或更差的性能，且缺乏类型安全性。本系统在**编译时**将物模型规则编译为 Erlang gen_statem guard 子句。这意味着规则匹配开销为单个模式匹配操作，而非规则引擎解释周期。其副作用同样重要：物模型中的错误（未定义属性、无效操作符）在编译时被捕获，而非在设备已上线后的运行时被捕获。

### 2. 进程即数字孪生，而非文档即孪生

云原生 IoT 平台通常将数字孪生实现为版本化 JSON 文档（AWS IoT Device Shadow、Azure Digital Twins）。本系统将每个数字孪生实现为一个**OTP 进程**——一个具有自有状态、邮箱和生命周期管理的轻量级 Actor。这不仅实现了真正的隔离（设备崩溃不会波及邻居设备），还实现了 OTP 的好处：监督树（自动重启）、系统监控（进程间监控）和热代码升级。

### 3. MQTT 主题为本体地址，而非扁平命名空间

大多数 IoT 平台使用扁平或两层 MQTT 主题（`device_id/data`）。本系统的主题遵循分层本体结构：`dgiot/{site}/{gateway}/{device}/{point}/data`。主题本身就表达了一个**可在语义上遍历的路径**——平台可通过解析主题段从 Site 导航至 Point。这使得 MQTT 主题同时充当传输通道和语义标识符。

### 4. 双重路径规则评估

该架构不是选择编译时或运行时规则评估，而是同时采用两者：将关键阈值（如温度>85C）编译为 gen_statem guard 子句（纳秒级评估），同时将复杂条件（多属性 AND/OR）保留在运行时引擎（`dgiot_ontology_rule:evaluate/3`）中，支持动态规则追加。这种双层方法避免了纯 AOT 系统的僵化和纯 JIT 系统的延迟。

在边缘采集层还有一个额外的规则层——15 种基于滑动窗口的流式算法，与本体层规则正交。前者管理设备状态转换（由物模型编译而来），后者管理行业工况诊断（由 stream_tasks.yaml 配置驱动）。

### 5. 四层本体架构（DLAS），而非三层

典型企业 IoT 参考架构（工业 4.0 RAMI、Azure IoT）定义了三层：边缘、平台和企业。本系统的 DLAS 框架按关注点而非部署拓扑定义分层：

- **Data**：万物皆数据（Parse CRUD、TDengine 时序、Mnesia/ETS 状态）
- **Logic**：万物皆关系（类层次、属性继承、规则链）
- **Action**：万物皆状态（gen_statem 生命周期、MQTT 桥接、指令下发）
- **Security**：万物皆经过鉴权（JWT、ACL、CLP、审计追踪）

这种正交分层意味着安全约束可应用于 Data 的 CRUD 操作和 Action 的状态转换——与传统架构的周边安全模型相比，这是一种更统一的安全模型。

### 6. 网关迁移：渐进式存量替换

`GatewayMigrationService` 支持三阶段迁移：**旁路**（只读，不干扰）、**接管**（处理并透传至原 IO Server）、**稳定**（不依赖原 IO Server独立运行）。这种设计解决了工业 IoT 中最棘手的实际问题之一——如何在不停产的情况下替换运行中的现场网关——而这在传统的全有或全无网关替换方案中是无法解决的。

## 行业对等参考

| 模式/标准 | 与本系统的映射 |
|---------------|-------------------|
| **数字孪生** (Grieves, 2002) | Shadow gen_statem 进程作为实时数字孪生，通过 MQTT 实现物理实体的 1:1 映射 |
| **OPC UA 地址空间** (IEC 62541) | 本体路径（site->gateway->device->point）相当于 OPC UA 分层节点结构，MQTT 主题作为 BrowseName |
| **资产管理壳** (工业 4.0, 2015) | Thing Model JSON 作为 AAS 子模型，gen_statem 状态作为运行时 AAS，MQTT 协议作为通信通道 |
| **W3C 物联万维网 (WoT) 事物描述** | 物模型等价于 WoT TD，properties/relations/rules 与 WoT 交互模式（property/action/event）对齐 |
| **SWRL** (W3C, 2004) | `{when: {property, op, value}, then: {state, action}}` 格式本质上即 Horn 子句 SWRL 规则，可导出为 OWL |
| **Actor 模型** (Hewitt, 1973) | 每个设备进程即一个 Actor，通过异步消息传递，监督树提供容错 |
| **CQRS** (Fowler, 2011) | Parse（状态）与 TDengine（时序）分离——命令路径和查询路径使用不同的存储引擎 |
| **边缘计算参考架构** (IEC, 2019) | 9 种访问模式覆盖 IEC 边缘计算分类，桥接模式对应边缘网关级联 |

## 结论

本系统的架构统一性强，因为它将六种看似独立的关注点——设备生命周期（gen_statem）、语义分类（本体）、数字孪生（影子）、设备模板（物模型）、时序数据（TDengine）、工业协议寻址（点位）——通过五个关键设计决策统一起来：(1) 将物模型编译为 OTP 行为，而非运行时解释，(2) 将数字孪生实现为 Erlang 进程，而非 JSON 文档，(3) 将 MQTT 主题编码为本体路径，而非扁平命名空间，(4) 将规则分解为编译时 guard 与运行时评估器，(5) 将边缘采集器作为一个统一管道，9 种接入模式均输出标准化数据点。其结果是一个在设备数量上线性扩展的系统，具有 OTP 提供的实时/容错属性，同时保持语义互操作性所需的 OWL 级可导出性，以及面向工业场景的渐进式网关迁移能力。

## 关键源文件

### 平台层 (Erlang/OTP)

| 文件 | 作用 |
|------|----------|
| `D:\ai\kylin\scripts\dgiot_shadow.erl` | Shadow 设备 gen_statem：生命周期与状态管理 |
| `D:\ai\kylin\scripts\dgiot_ontology.erl` | DLAS 4 层本体引擎：init/load_model/spawn_instance/push_point |
| `D:\ai\dgiot_smartleaf\otp\dgiot_ontology\src\dgiot_ontology.erl` | 物模型 -> gen_statem 自动代码生成，含 OWL 导出 |
| `D:\ai\dgiot_smartleaf\otp\dgiot_ontology\src\dgiot_ontology_rule.erl` | SWRL 规则引擎：编译、评估、匹配、触发 |
| `D:\ai\dgiot_smartleaf\otp\include\tqics_ontology.hrl` | OWL->Record 映射：状态、规则、实体定义 |
| `D:\ai\dgiot_smartleaf\otp\src\tqics_registry.erl` | 本体注册表：进程间类注册与关系连接 |
| `D:\ai\dgiot_smartleaf\otp\src\tqics_action.erl` | 动作执行器：L1/L2/L3 分级响应 |
| `D:\ai\dgiot_smartleaf\otp\src\tqics_device.erl` | TQICS 设备 gen_statem：OWL Equipment 类实现 |

### 边缘采集层 (Python)

| 文件 | 作用 |
|------|----------|
| `D:\ai\dgiot_collector\src\core\device_access.py` | 9 种适配器工厂：DeviceAccessManager + _create_adapter() |
| `D:\ai\dgiot_collector\src\core\unified_pipeline.py` | 统一采集管线：UnifiedPipeline 主循环编排器 |
| `D:\ai\dgiot_collector\src\core\edge_stream_engine.py` | 15 种流式算法 + 滑动窗口特征提取 |
| `D:\ai\dgiot_collector\src\core\oilfield_alert_pipeline.py` | 6 阶段告警闭环编排器 |
| `D:\ai\dgiot_collector\src\core\vendor_mapping.py` | 多厂商差异映射（字节序、缩放） |
| `D:\ai\dgiot_collector\src\core\gateway_migration.py` | 3 阶段网关迁移服务 |
| `D:\ai\dgiot_collector\src\core\thing_model_manager.py` | 物模型 CRUD + TDengine schema 自动生成 |
| `D:\ai\dgiot_collector\src\core\dtu_listener.py` | 5 厂商 DTU 注册帧识别（宏电/映翰通/亿帆/有人/四信） |
| `D:\ai\dgiot_collector\src\core\modbus_bridge_server.py` | 双向桥接服务器（旁路+转发） |
| `D:\ai\dgiot_collector\src\core\dgiot_adapter.py` | dgiot 平台双通道适配器（REST + MQTT） |
| `D:\ai\dgiot_collector\src\storage\tdengine_manager.py` | TDengine 批量写入器（5s 缓冲） |
| `D:\ai\dgiot_collector\src\config\endpoints.yaml` | 9 种接入场景端点配置 |
| `D:\ai\dgiot_collector\src\config\thing_models\G1-G8_standard.yaml` | 8 组标准行业物模型（80+ 测点） |

### 架构参考

| 文件 | 作用 |
|------|----------|
| `D:\ai\dgiot_lite\DGAIOT_ONTOLOGY.md` | 架构本体参考：Erlang DG-IoT 与 Python dgiot_lite 的设计对齐 |
