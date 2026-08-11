# DGIOT — Open Source Industrial IoT Platform

<p align="center">
  <strong>300+ Protocols · 6-Minute Deploy · 30M Concurrent Connections · 99.9999% Uptime</strong>
</p>

<p align="center">
  <a href="https://github.com/dgiot/dgiot/stargazers"><img src="https://img.shields.io/github/stars/dgiot/dgiot?style=flat&color=f5c542" alt="Stars"></a>
  <a href="https://github.com/dgiot/dgiot/blob/master/LICENSE"><img src="https://img.shields.io/github/license/dgiot/dgiot?style=flat&color=2ea043" alt="License"></a>
  <a href="https://github.com/dgiot/dgiot/discussions"><img src="https://img.shields.io/badge/Discussions-join-blue?style=flat" alt="Discussions"></a>
  <a href="https://github.com/dgiot/dgiot/releases"><img src="https://img.shields.io/github/v/release/dgiot/dgiot?style=flat" alt="Release"></a>
</p>

<p align="center">
  <a href="./README.md">English</a> | <a href="./README-CN.md">简体中文</a> | <a href="./README-JP.md">日本語</a> | <a href="./README-RU.md">Русский</a>
</p>

---

## What is DGIOT?

DGIOT is an open-source, production-grade Industrial IoT platform. It connects industrial devices — PLCs, sensors, meters, drones, robots — to real-time dashboards through 300+ protocol adapters.

Unlike general-purpose IoT platforms, DGIOT is built for **industrial environments**: edge autonomy, protocol diversity, carrier-grade reliability, and zero-code device modeling.

**Deployed at:** Daqing Oil Field (928 gateways, 114K sensor points) · Asian Games 2022 (zero-failure venue operations) · Southern Power Grid (120K smart meters, 7 cities) · Saudi SEC · Toppan Japan.

---

## Architecture: DLAS

```
┌──────────────────────────────────────────────────────┐
│  EDGE        iotStudio (Python + Vue 3)              │
│              Device Access → Pipeline → Stream → Alert│
├──────────────────────────────────────────────────────┤
│  SECURITY    JWT · RBAC · ACL/CLP · Audit Log        │
├──────────────────────────────────────────────────────┤
│  ACTION      Shadow (gen_statem) · MQTT · Rules      │
│              States: init → auth → online → alert     │
├──────────────────────────────────────────────────────┤
│  LOGIC       Ontology Engine · Model Registry         │
├──────────────────────────────────────────────────────┤
│  DATA        Parse (23 Classes) · PostgreSQL · TDengine
│              EMQX · Mnesia/ETS                        │
└──────────────────────────────────────────────────────┘
```

### FDE Pipeline

```
Model → Ontology → Device Access → TimeSeries → Rules → Dashboard
```

---

## Quick Start

```bash
git clone https://github.com/dgiot/dgiot.git
cd dgiot
docker-compose up -d

# Open http://localhost:5080
# Default credentials: admin / dgiot_admin
```

**One-line install (Linux):**
```bash
wget -q https://raw.githubusercontent.com/dgiot/dgiot/master/dgiot_install.sh && bash dgiot_install.sh
```

**[Full Documentation →](https://docs.dgiotcloud.cn)**

---

## Key Capabilities

| Capability | Description |
|------------|-------------|
| **300+ Protocols** | Modbus RTU/TCP, OPC UA, MQTT, IEC 104, BACnet, A11 CNPC, Siemens S7, and 290+ more |
| **Edge Computing** | Runs on ARM/x86 gateways with 7-day offline buffer. Stream computation engine with 15 algorithms |
| **Digital Twin** | Drag-and-drop SCADA, 3D Konva.js canvas, ECharts dashboards |
| **AI Integration** | DeepSeek LLM → natural language to SWRL rules. Ontology engine detects failures before SCADA alarms |
| **Carrier Grade** | 30M concurrent connections verified. QoS 0/1/2. Hot code reloading (zero-downtime upgrades) |
| **6-Minute Deploy** | Docker Compose → ready. No Kubernetes PhD required |

---

## Who Uses DGIOT

| Organization | Industry | Deployment Scale |
|-------------|----------|------------------|
| **Daqing Oil Field** | Oil & Gas | 928 gateways, 114,809 sensor points, 16 extraction plants |
| **Asian Games 2022** | Smart Venues | 30+ subsystems, zero-failure across 15 competition venues |
| **Southern Power Grid** | Energy | 120,000 smart meters, 7 cities, 30M concurrency verified |
| **Aerospace Research Institute** | Defense | UAV automated testing, 60 units/day, 23 test procedures |
| **Saudi SEC** | Power Utility | First Chinese IoT platform in Middle East power grid |
| **Toppan (Japan)** | Manufacturing | 15M ZETA tag stress test, 1 billion data points |

---

## Repository Structure

```
dgiot/
├── apps/
│   ├── dgiot/              # Core (EMQX bridge, rules engine, alerts)
│   ├── dgiot_ontology/     # Model registry, OWL reasoning
│   ├── dgiot_parse/        # Parse Server REST client
│   ├── dgiot_task/         # Shadow gen_statem workers
│   ├── dgiot_device/       # Device management + thing models
│   ├── dgiot_bridge/       # Protocol bridges (300+ adapters)
│   ├── dgiot_api/          # REST API + JWT auth gateway
│   └── dgiot_tdengine/     # TDengine time-series connector
├── lib-extra/              # Community plugin directory
├── scripts/                # Deployment scripts
├── docker/                 # Docker & Docker Compose
├── docs/                   # Documentation
├── docker-compose.yml      # Quick start
└── Makefile                # Build system
```

---

## Contributing

We welcome contributions — especially new protocol adapters, documentation improvements, and bug fixes.

See [CONTRIBUTING.md](./CONTRIBUTING.md) for development setup, PR process, and the protocol adapter guide.

**Good First Issues:** [View tasks labeled "good first issue" →](https://github.com/dgiot/dgiot/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22)

---

## Community & Support

| Channel | Link |
|---------|------|
| **GitHub Discussions** | [github.com/dgiot/dgiot/discussions](https://github.com/dgiot/dgiot/discussions) |
| **Documentation** | [docs.dgiotcloud.cn](https://docs.dgiotcloud.cn) |
| **Website** | [dgiotcloud.cn](https://www.dgiotcloud.cn) |
| **Discord** | *Coming soon* |
| **Gitee (中文)** | [gitee.com/dgiiot/dgiot](https://gitee.com/dgiiot/dgiot) |

---

## License

DGIOT is open source under the [Apache License 2.0](./LICENSE).

---

<p align="center">
  <sub>Built with Erlang/OTP · Maintained by the DGIOT community · Hangzhou, China & Dallas, TX</sub>
</p>
