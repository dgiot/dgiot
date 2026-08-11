# 场站本体模拟：PLANT_A 131 号 IO 服务器

> 基于真实数据: `io_ontology.json`, `油水井寄存器.xlsx`, `thing_model.json`

---

## 场站本体拓扑

```
Site:  PLANT_A (oil_field_01)
│
├─ Gateway: IO-131
│   ip: 192.168.10.131
│   hostname: IO-SERVER-01
│   role: IO服务器
│   os: Win10.0.14393
│   processes:
│     ├─ IoProject.exe (306MB, 主引擎)
│     ├─ IOMan.exe ×36 (16MB/each, Modbus/OPC客户端)
│     ├─ CommBridge.exe (32MB, GPRS网桥)
│     └─ IoCommit.exe ×7 (15MB/each, Oracle写入)
│   protocols:
│     ├─ Modbus TCP :53001 → 206台 RTU (有线)
│     ├─ A11 5a5a :8889 → 200台井口 (TCP, 已逆向)
│     └─ DCE/RPC :135 → 5个DCS (DCOM拒绝, 待配)
│   data_sources: 9个
│   ports: 12个开放服务
│
├─ Device: rtu_001 → DEVICE_ID_PLACEHOLDER (B1V25VE33) 油井
│   protocol: Modbus TCP, slaveid: 1
│   product: oil_well_rtu
│   │
│   ├─ Point: oil_pressure     (40300, float32_AB, 0-10MPa, alarm:>3.0)
│   ├─ Point: casing_pressure  (40302, float32_AB, 0-10MPa)
│   ├─ Point: temperature      (40304, float32_AB, -20~85°C, alarm:>75)
│   ├─ Point: flow_rate        (40306, float32_AB, m³/h)
│   ├─ Point: pump_status      (40308, uint16, 0=stop 1=run)
│   └─ Point: alarm_code       (40309, uint16, 位掩码)
│
├─ Device: rtu_002 → DEVICE_ID_PLACEHOLDER2 (B1V51VSFK01) 油井
│   protocol: Modbus TCP, slaveid: 2
│   │
│   ├─ Point: max_load         (40300, float32_AB, kN, alarm:>80)
│   ├─ Point: min_load         (40302, float32_AB, kN)
│   ├─ Point: up_current       (40304, float32_AB, A, alarm:>20)
│   └─ Point: down_current     (40306, float32_AB, A)
│
├─ Device: dcs_01 → DCS1 (RSLinx Classic)
│   subnet: 172.23.9.0/24, 30设备
│   protocol: OPC DA/DCOM :58648
│   status: DCOM拒绝 (需配dcomcnfg)
│
├─ Device: dcs_02 → DCS2 (RSLinx Classic)
│   subnet: 172.23.18.0/24, 30设备
│   status: DCOM拒绝
│
├─ Device: dcs_03 → DCS3
│   subnet: 172.26.6.0/24, 60设备
│   status: DCOM拒绝
│
├─ Device: dcs_04 → DCS4
│   subnet: 172.21.14.0/24, 40设备
│   status: DCOM拒绝
│
└─ Device: dcs_05 → DCS5 (RSLinx+WinCC)
    subnet: 172.28.5.0/24, 40设备
    protocol: OPC DA/DCOM :59655
    status: DCOM拒绝
```

---

## 模拟：从物理寄存器到状态迁移

### T=0s 初始化

```erlang
%% 1. 加载物模型
dgiot_ontology:load_model(#{
    <<"class">> => <<"oil_well_rtu">>,
    <<"properties">> => [
        #{<<"id">> => <<"oil_pressure">>,    <<"type">> => <<"float">>, <<"unit">> => <<"MPa">>},
        #{<<"id">> => <<"casing_pressure">>, <<"type">> => <<"float">>, <<"unit">> => <<"MPa">>},
        #{<<"id">> => <<"temperature">>,     <<"type">> => <<"float">>, <<"unit">> => <<"celsius">>},
        #{<<"id">> => <<"flow_rate">>,       <<"type">> => <<"float">>, <<"unit">> => <<"m3h">>},
        #{<<"id">> => <<"pump_status">>,     <<"type">> => <<"int">>},
        #{<<"id">> => <<"alarm_code">>,      <<"type">> => <<"int">>}
    ],
    <<"rules">> => [
        #{<<"id">> => <<"R_HIGH_TEMP">>,  <<"when">> => #{<<"property">> => <<"temperature">>, <<"op">> => <<">">>, <<"value">> => 75},
         <<"then">> => #{<<"state">> => <<"warning">>, <<"severity">> => <<"L1">>, <<"action">> => <<"notify">>}},
        #{<<"id">> => <<"R_HIGH_PRESS">>, <<"when">> => #{<<"property">> => <<"oil_pressure">>, <<"op">> => <<">">>, <<"value">> => 3.0},
         <<"then">> => #{<<"state">> => <<"alarm">>, <<"severity">> => <<"L2">>, <<"action">> => <<"shutdown">>}},
        #{<<"id">> => <<"R_PUMP_FAIL">>,  <<"when">> => #{<<"property">> => <<"pump_status">>, <<"op">> => <<"==">>, <<"value">> => 0},
         <<"then">> => #{<<"state">> => <<"warning">>, <<"severity">> => <<"L1">>, <<"action">> => <<"notify">>}}
    ]
}).
%% → {ok, <<"oil_well_rtu">>}
%% → ETS rules: 3 rules compiled

%% 2. 注册本体节点
dgiot_ontology:register(site,    #{id => <<"oil_field_01">>, name => <<"PLANT_A">>, type => <<"oil_field">>}).
dgiot_ontology:register(gateway, #{id => <<"gw_131">>, ip => <<"192.168.10.131">>, site => <<"oil_field_01">>, protocols => [<<"modbus_tcp:53001">>, <<"a11:8889">>] }).
dgiot_ontology:register(device,  #{id => <<"rtu_001">>, gateway => <<"gw_131">>, name => <<"DEVICE_ID_PLACEHOLDER">>, type => <<"oil_well_rtu">>, protocol => <<"modbus">>, slaveid => 1}).
dgiot_ontology:register(point,   #{id => <<"oil_pressure">>,    device => <<"rtu_001">>, name => <<"油压">>, unit => <<"MPa">>}).
dgiot_ontology:register(point,   #{id => <<"temperature">>,     device => <<"rtu_001">>, name => <<"温度">>, unit => <<"celsius">>}).
dgiot_ontology:register(point,   #{id => <<"pump_status">>,     device => <<"rtu_001">>, name => <<"泵状态">>}).

%% → Parse: 4 个对象创建
%% → 本体链: oil_field_01 → gw_131 → rtu_001 → oil_pressure
```

### T=5s 设备上线

```erlang
%% 3. 启动影子进程
{ok, ShadowPid} = dgiot_ontology:spawn_instance(<<"oil_well_rtu">>, <<"rtu_001">>).
%% → gen_statem:start_link(dgiot_shadow, [<<"rtu_001">>, #{class => <<"oil_well_rtu">>}])
%% → PID: <0.456.0>
%% → State: authenticate

%% 4. 设备认证心跳
ShadowPid ! {cast, heartbeat}.
%% → State: authenticate → online
%% → ETS instance: {rtu_001, #{class=>oil_well_rtu, pid=>0.456.0, status=>online}}
```

### T=10s 正常数据注入 (Modbus 采集 → MQTT → Shadow)

```bash
# 边缘采集器从 192.168.10.131:53001 readHoldingRegisters(40300, 6)
# → {oil_pressure:2.35, casing_pressure:1.82, temperature:45.6, flow_rate:12.3, pump_status:1, alarm_code:0}
# → MQTT publish

mosquitto_pub -h 127.0.0.1 -p 1883 \
  -t "dgiot/oil_field_01/gw_131/rtu_001/oil_pressure/data" \
  -m '{"ts":1751884810000,"v":2.35,"q":192}'

mosquitto_pub -h 127.0.0.1 -p 1883 \
  -t "dgiot/oil_field_01/gw_131/rtu_001/temperature/data" \
  -m '{"ts":1751884810000,"v":45.6,"q":192}'
```

```erlang
%% Shadow 进程内部:
online(cast, {data, #{oil_pressure := 2.35}}, Device) ->
    evaluate(Rules, Props):
      R_HIGH_PRESS: 2.35 > 3.0? → false
      → 保持 online

%% → bridge:
%%   dgiot_ontology:push_point(oil_pressure, 2.35)
%%   → Topic: dgiot/oil_field_01/gw_131/rtu_001/oil_pressure/data
%%   dgiot_parse:update_object(Device, rtu_001, #{status => online, oil_pressure => 2.35})
%%   TDengine: INSERT INTO dgiot_oil_field_01_gw_131_rtu_001 VALUES (NOW, 2.35, 192)
```

### T=35s 温度告警 (温度升高)

```bash
# 第 5 轮采集: 温度突然升高
mosquitto_pub -h 127.0.0.1 -p 1883 \
  -t "dgiot/oil_field_01/gw_131/rtu_001/temperature/data" \
  -m '{"ts":1751884835000,"v":82.3,"q":192}'  # ← 超过 75°C 阈值!
```

```erlang
%% Shadow 进程:
online(cast, {data, #{temperature := 82.3}}, Device) ->
    evaluate(Rules, Props):
      R_HIGH_TEMP: 82.3 > 75? → TRUE! → severity: L1, action: notify
      → state: online → warning

%% → Log: [rtu_001] WARNING: temperature 82.3°C exceeds 75°C (rule R_HIGH_TEMP)
%% → Alarm: L1 通知运维
%% → bridge: Parse/Device.status = warning, TDengine 打标
```

### T=60s 持续高温 → 升级告警

```bash
# 温度持续不降
mosquitto_pub -h 127.0.0.1 -p 1883 \
  -t "dgiot/oil_field_01/gw_131/rtu_001/temperature/data" \
  -m '{"ts":1751884860000,"v":88.1,"q":192}'
```

```erlang
%% 连续 3 次 >75°C, error_count = 3
online(cast, {data, #{temperature := 88.1}}, Device) ->
    error_count = 3 >= ?MAX_ERRORS →
    state: warning → alarm

%% → Alarm: L2 紧急, 建议停机
%% → TDengine 记录告警事件
%% → MQTT 发布告警:
%%   Topic: dgiot/oil_field_01/gw_131/rtu_001/event
%%   Payload: {level: L2, code: HIGH_TEMP, msg: "温度 88.1°C 超阈值 75°C, 持续60s"}
```

### T=90s 修复 → 恢复

```bash
# 运维处理, 温度回落
mosquitto_pub -h 127.0.0.1 -p 1883 \
  -t "dgiot/oil_field_01/gw_131/rtu_001/temperature/data" \
  -m '{"ts":1751884890000,"v":48.2,"q":192}'
```

```erlang
%% Shadow 收到心跳 (温度正常)
alarm(cast, heartbeat, Device) →
    evaluate(Rules): T=48.2 < 75 ✓, P=2.4 < 3.0 ✓
    state: alarm → online
    error_count = 0

%% → Parse: Device.status = online
%% → 告警归档
```

---

## 整个场站同时在线的影子进程

```
ETS instance table (场站快照):

oil_field_01
 ├─ gw_131
 │   ├─ rtu_001  <0.456.0>  online     oil_pressure:2.35  T:45.6  pump:1
 │   ├─ rtu_002  <0.457.0>  online     max_load:41.3  up_current:10.7
 │   ├─ rtu_003  <0.458.0>  offline    (心跳超时 30s)
 │   └─ ...     (203 more RTUs)
 │
 ├─ dcs_01  <0.500.0>  unavailable  (DCOM拒绝)
 ├─ dcs_02  <0.501.0>  unavailable  (DCOM拒绝)
 └─ dcs_03  <0.502.0>  unavailable  (DCOM拒绝)

Total: 206 RTU shadows + 5 DCS shadows = 211 gen_statem processes
Memory: 211 × ~3KB ≈ 0.6 MB
```

**211 个 Erlang 进程 = 211 台物理设备的数字孪生。总共不到 1MB 内存。**

---

## MQTT Topic 全景

```
dgiot/oil_field_01/gw_131/rtu_001/oil_pressure/data     {ts, v:2.35, q:192}
dgiot/oil_field_01/gw_131/rtu_001/temperature/data      {ts, v:45.6, q:192}
dgiot/oil_field_01/gw_131/rtu_001/pump_status/data      {ts, v:1, q:192}
dgiot/oil_field_01/gw_131/rtu_001/event                 {level:L2, code:HIGH_TEMP}
dgiot/oil_field_01/gw_131/rtu_002/max_load/data         {ts, v:41.3, q:192}
dgiot/oil_field_01/gw_131/rtu_002/up_current/data        {ts, v:10.7, q:192}
...

每轮采集: 206 RTU × 4 测点 = 824 MQTT messages / 20s
峰值: ~40 msg/s
EMQX 承载: 40 msg/s << 100万 msg/s (EMQX 基准)
```
