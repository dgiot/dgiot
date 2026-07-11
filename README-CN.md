<p align="center">
  <img src="https://img.shields.io/badge/Erlang-24.3-%23A90533">
  <img src="https://img.shields.io/badge/OTP-24.3-green">
  <img src="https://img.shields.io/badge/EMQX-4.9-orange">
  <img src="https://img.shields.io/badge/License-Apache%202.0-blue">
</p>

<h1 align="center">dgaiot</h1>
<h3 align="center">Industrial IoT Aggregation Engine</h3>
<p align="center">Erlang/OTP · Million-Device · Shadow · Ontology · gen_statem</p>

---

## DLAS Architecture

```
  ┌──────────────────────────────────────────────────────────────────┐
  │                    EDGE: iotStudio (Python+Vue)                   │
  │  DeviceAccess(9) → UnifiedPipeline → StreamEngine(15) → Alert(6) │
  └──────────────────────────┬───────────────────────────────────────┘
                             │ MQTT / HTTP
  ┌──────────────────────────▼───────────────────────────────────────┐
  │  SECURITY    auth · role · ACL/CLP · Hooks · JWT                 │
  ├──────────────────────────────────────────────────────────────────┤
  │  ACTION      Shadow(gen_statem) · Bridge · MQTT · Rule Engine   │
  │              init → auth → online → {normal, alarm, offline}    │
  ├──────────────────────────────────────────────────────────────────┤
  │  LOGIC       Ontology Engine · Model Registry · 3 ETS Tables     │
  │              load_model → compile → spawn → evaluate → reason    │
  ├──────────────────────────────────────────────────────────────────┤
  │  DATA        Parse(23 Classes) · PG(:7432)                       │
  │              TDengine(_{ProductId}, devaddr NCHAR(50))           │
  │              EMQX(:1883) · Mnesia/ETS                             │
  └──────────────────────────────────────────────────────────────────┘
```

[Full HTML Diagram](docs/architecture-diagram.html)

## FDE Pipeline

```
Model → Ontology → Device Access → TimeSeries → Rules → Dashboard
  1        2           3              4           5          6
```

## Data Flow

```
Modbus 40300 → MQTT(dgiot/{site}/{gw}/{dev}/{pt}/data) → Shadow PID
  → evaluate(Rules) → state transition → Parse + TDengine INSERT
```

## Ontology: DLAS

| Layer | Function | Key Technology |
|:------|:---------|:---------------|
| **Data** | 23 Parse classes, TDengine, Mnesia/ETS | JSONB, SuperTable/SubTable, _{ProductId} |
| **Logic** | load_model, spawn_instance, registry, rules, reasoner | 3 ETS tables, gen_statem compile |
| **Action** | Shadow 1:1 device process, Bridge, MQTT | gen_statem OTP, ont_push_point |
| **Security** | auth, role tree, ACL(object), CLP(class), Hooks | JWT, Parse ACL |

[Full Ontology Doc](docs/DGAIOT_ONTOLOGY.md) · [Storage Architecture](docs/ontology-storage.md) · [Station Simulation](docs/research/station-simulation.md)

## Modules

| App | Layer | Role |
|:----|:------|:-----|
| `dgiot` | Data | Core — EMQX, rule engine, alarms |
| `dgiot_ontology` | Logic | Model registry, reasoner, instance spawner |
| `dgiot_parse` | Data | Parse Server REST client |
| `dgiot_task` | Action | Shadow gen_statem worker |
| `dgiot_device` | Logic | Device management + thing model |
| `dgiot_bridge` | Action | Protocol bridge framework |
| `dgiot_dlink` | Action | Data link layer |
| `dgiot_api` | Security | REST API + auth gateway |
| `dgiot_http` | — | HTTP service layer |
| `dgiot_tdengine` | Data | TDengine timeseries connector |

## Quick Start

```bash
git clone git@gitee.com:dgaiot/dgaiot.git
cd dgaiot
export PATH=/usr/local/erlang_24.3/bin:$PATH
make
```

## Related

- [iotStudio](https://gitee.com/dgiiot/iotStudio) — Edge Agent & Application (Python + Vue)
- [dgiot](https://gitee.com/dgiiot/dgiot) — Full Erlang IoT Platform

## License

Apache 2.0
