<p align="center">
  <img src="https://img.shields.io/badge/Erlang-24.3-%23A90533">
  <img src="https://img.shields.io/badge/OTP-24.3-green">
  <img src="https://img.shields.io/badge/EMQX-4.9-orange">
  <img src="https://img.shields.io/badge/License-Apache%202.0-blue">
</p>

<h1 align="center">dgaiot</h1>
<h3 align="center">Industrial IoT Aggregation Engine</h3>
<p align="center">Erlang/OTP · Million-Device · Shadow · Ontology · gen_statem</p>

<br>

---

## Ontology: DLAS

```
Security  ┌── auth · role · ACL/CLP · Hooks ───────────┐
Action    ├── Shadow(gen_statem) · Bridge · MQTT · Rule  │
Logic     ├── Ontology Engine · Model Registry · Reasoner│
Data      ├── Parse/PG · TDengine · Mnesia/ETS · EMQX    │
          └──────────────────────────────────────────────┘
```

| Layer | Function |
|:------|:---------|
| **Data** | Parse 23 classes · TDengine · Mnesia/ETS · EMQX |
| **Logic** | Ontology Engine · load_model · spawn_instance · Rule · Reasoner |
| **Action** | Shadow gen_statem · Bridge → Parse/TDengine · MQTT Publish |
| **Security** | auth · Role Tree · ACL (object) · CLP (class) · Hooks |

[Full Ontology Doc](docs/DGAIOT_ONTOLOGY.md)

## FDE Pipeline

```
Model → Ontology → Device Access → TimeSeries → Rules → Dashboard
  1        2           3              4           5          6
```

## Shadow Device

```
  sensor_update(Props)
    → evaluate(Rules, Props)
    → state transition (normal → warning → critical → offline)
    → bridge → Parse + TDengine
```

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
