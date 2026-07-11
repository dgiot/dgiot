# dgaiot

**Industrial IoT Aggregation Engine**

Erlang/OTP · Million Devices · Shadow · Ontology · State Machine

---

## Architecture

```
                   PROTOCOL ADAPTERS (iotStudio)
                   Modbus · OPC · A11 · S7 · MQTT
                          │
                          ▼
   ┌──────────────────────────────────────────────────┐
   │                dgaiot PLATFORM                    │
   │                                                   │
   │  ┌─────────┐  ┌──────────┐  ┌─────────────────┐  │
   │  │ Shadow  │  │ Ontology │  │  State Machine  │  │
   │  │ Device  │  │ 4-Layer  │  │ gen_statem OTP  │  │
   │  │ 1:1 OTP │  │ Site>Gw  │  │ init→auth→online│  │
   │  │ Process │  │ >Dev>Pt  │  │ →alarm→offline  │  │
   │  └────┬────┘  └────┬─────┘  └───────┬─────────┘  │
   │       └────────────┼────────────────┘             │
   │                    ▼                              │
   │  ┌─────────────────────────────────────────────┐  │
   │  │              EMQX MQTT Broker                │  │
   │  │      Million-Device Concurrent Access        │  │
   │  │      :1883 (MQTT) :8083 (WS) :8081 (API)    │  │
   │  └─────────────────────────────────────────────┘  │
   │                    │                              │
   │       ┌────────────┼────────────┐                 │
   │       ▼            ▼            ▼                 │
   │  ┌─────────┐ ┌──────────┐ ┌──────────┐           │
   │  │ Parse   │ │ TDengine │ │   PG     │           │
   │  │ :1337   │ │  :6041   │ │  :7432   │           │
   │  │ REST API│ │ Timeseries│ │Business DB│          │
   │  └─────────┘ └──────────┘ └──────────┘           │
   └──────────────────────────────────────────────────┘
                          │
                          ▼
              APPLICATION LAYER (iotStudio)
              Vue3 · amis · 2D/3D Dashboard
```

## Apps

```
dgiot/           Core engine (EMQX + rules + alarms)
dgiot_ontology/  4-layer ontology
dgiot_parse/     Parse Server client
dgiot_task/      Shadow device / task worker
dgiot_device/    Device management
dgiot_bridge/    Bridge framework
dgiot_dlink/     Data link
dgiot_api/       Management API
dgiot_http/      HTTP service
dgiot_tdengine/  TDengine timeseries
```

## Ontology (4-Layer)

```
Site ──→ Gateway ──→ Device ──→ Point

  oil_field     gw_131      rtu_001     oil_pressure
                   │
          protocols: modbus_tcp, a11, opc_da
          processes: IoProject, CommBridge
          devices:  206 RTUs

MQTT Topic: dgiot/{site}/{gateway}/{device}/{point}/data
Payload:    {ts, v, q}
```

## State Machine

```
       ┌──────┐   heartbeat    ┌────────┐
  ────→│ auth │──────────────→│ online │←──────┐
       └──┬───┘               └───┬────┘       │
          │ fail                  │ error×3    │ heartbeat
          ▼                       ▼            │
       ┌────────┐              ┌───────┐       │
       │ offline│              │ alarm │───────┘
       └────────┘              └───────┘
```

## Quick Start

```bash
export PATH=/usr/local/erlang_24.3/bin:$PATH
make
```

## License

Apache 2.0
