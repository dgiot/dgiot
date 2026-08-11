# CLAUDE.md — DGIOT

## Project Identity

**DGIOT** is an open-source Industrial IoT platform built on Erlang/OTP. It connects 300+ industrial protocols to real-time dashboards in 6 minutes. Battle-tested at Daqing Oil Field (928 gateways, 114K sensor points), Asian Games (zero failure), and Southern Power Grid (120K smart meters).

- **Org**: github.com/dgiot
- **License**: Apache 2.0
- **Stars**: 12K+ (GitHub + Gitee)
- **Community**: 70K+ developers worldwide

## Architecture — DLAS Four-Layer Model

```
EDGE       iotStudio (Python + Vue 3)
           DeviceAccess -> UnifiedPipeline -> StreamEngine -> Alert

SECURITY   auth, role, ACL/CLP, Hooks, JWT

ACTION     Shadow (gen_statem), Bridge, MQTT, Rule Engine
           States: init -> auth -> online -> {normal, alarm, offline}

LOGIC      Ontology Engine, Model Registry, 3 ETS Tables

DATA       Parse (23 Classes), PostgreSQL, TDengine, EMQX, Mnesia/ETS
```

## FDE Pipeline (Six-Step Methodology)

```
Model -> Ontology -> Device Access -> TimeSeries -> Rules -> Dashboard
```

## Project Structure

```
dgiot/
├── apps/
│   ├── dgiot/              # Core: EMQX bridge, rules engine, alerts
│   ├── dgiot_ontology/     # Model registry, reasoning, instance generation
│   ├── dgiot_parse/        # Parse Server REST client
│   ├── dgiot_task/         # Shadow gen_statem workers
│   ├── dgiot_device/       # Device management + thing models
│   ├── dgiot_bridge/       # Protocol bridges
│   ├── dgiot_dlink/        # Data links
│   ├── dgiot_api/          # REST API + auth gateway
│   └── dgiot_tdengine/     # TDengine time-series connector
├── docs/                   # Architecture diagrams, ontology docs
├── scripts/                # Deployment scripts
├── lib-extra/              # Community plugins
└── docker-compose.yml      # Quick start
```

## Build & Run

### Quick Start
```bash
git clone https://github.com/dgiot/dgiot.git
cd dgiot
make run
# Open http://localhost:5080
```

### Docker
```bash
docker-compose up -d
```

## Key Repositories

| Repo | Stack | Purpose |
|------|-------|---------|
| dgiot | Erlang/OTP | Core IoT platform |
| iotStudio | Python + Vue 3 | Edge agent + low-code apps |
| iotView | Vue 3 | Dashboard framework |
| iotApp | Java | Mobile low-code app |
| dgiot_dtu | C# | Windows edge gateway |

## Development Conventions

- **Language**: Erlang for core, Python for edge, Vue 3 for frontend
- **Build**: `make` for full build, `make DIAGNOSTIC=1` for debug
- **Protocols**: New adapters in `apps/dgiot_bridge/`, implement `dgiot_protocol` behaviour
- **Docs**: English for code comments, Chinese OK for commit messages
- **PR**: Fork -> feature branch -> conventional commits -> PR to `master`

## Community

- [GitHub Discussions](https://github.com/dgiot/dgiot/discussions)
- [Gitee (中文)](https://gitee.com/dgiiot/dgiot)
- [Official Site](https://www.dgiotcloud.cn)

## Enterprise Deployments

| Customer | Industry | Scale |
|----------|----------|-------|
| Daqing Oil Field | Oil & Gas | 928 gateways, 114K points |
| Asian Games 2022 | Smart Venues | 30+ subsystems, zero failure |
| Southern Power Grid | Energy | 120K meters, 7 cities |
| Aerospace Institute | Defense | UAV testing, 60 units/day |
| Saudi SEC | Power | First Chinese IoT in Middle East |
| Toppan Japan | Mfg | 15M ZETA tags, 1B data points |
