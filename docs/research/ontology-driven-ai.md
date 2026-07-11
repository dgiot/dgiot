# 本体驱动 AI 编程 — dgaiot 核心思路

## 原则

**本体是唯一真相源。AI 读本体，生成代码。本体变，代码自动变。**

## 流程

```
本体 (Single Source of Truth)
  │
  ├── thing_model.json (125 properties, Modbus registers)
  ├── io_ontology.json (5 servers, 9 sources, 31 terminals)
  ├── Product/Device/Point (Parse 23 classes)
  ├── Channel config (TDengine mapping)
  └── ACL rules (dgiot_mqtt_acl.erl)
         │
         ▼
  AI 编程 (Claude / Copilot)
         │
         ├── dgiot_ontology.erl    (load_model → compile → spawn)
         ├── dgiot_shadow.erl      (gen_statem guard clauses)
         ├── dgiot_tdengine_*      (schema from thing_model)
         ├── bridge_to_hub.py      (device mapping from io_ontology)
         └── MQTT topics           (ontology path: Site/Gateway/Device/Point)
                │
                ▼
  运行时 (确定性)
         │
         ├── gen_statem: evaluate(Rules)
         ├── dgiot_tdengine: INSERT
         └── EMQX: publish/subscribe
```

## 不做什么

```
❌ 手写协议适配器       → 本体有 register 定义，AI 生成
❌ 手配 MQTT topic      → 本体有 path 映射，AI 生成
❌ 手写 TDengine schema → 本体有 properties，AI 生成
❌ 手写规则引擎          → 本体有 rules[]，AI 编译
❌ AI 放运行时推理       → 不需要，稳定性优先
```

## 本体即代码

```
thing_model.json 的一个 property:
{
  "name": "油压",
  "dataForm": {"address": "40300", "protocol": "modbus", "originaltype": "float32_AB"},
  "dataType": {"type": "float"},
  "identifier": "oil_pressure"
}

AI 自动生成:
  → Modbus:    readHoldingRegisters(40300, 2, float32_AB)
  → MQTT:      dgiot/oil_field_01/gw_131/rtu_001/oil_pressure/data
  → TDengine:  INSERT INTO _85ef6b7459._2de1b3e1b8 VALUES (NOW, {v}, 192)
  → gen_statem: case Props of #{oil_pressure := V} when V > 3.0 → alarm
```

## 验证

```
本体 → AI 生成的代码 → 在 Kylin-DMZ 上跑 → 全链路验证通过 ✅

已生成:
  dgiot_ontology.erl (187行)   ← thing_model.json
  dgiot_shadow.erl   (192行)   ← gen_statem rules
  bridge_to_hub.py   (105行)   ← io_ontology.json
  mqtt_to_td.py      (70行)    ← TDengine schema
  simulate_131.py    (198行)   ← 131 IO Server analysis
  dlink-auth.md      (117行)   ← dgiot_mqtt_acl.erl
```
