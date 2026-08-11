<<<<<<< HEAD
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
=======
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

DGAIOT is an open-source IoT platform for edge intelligence — a fork/customization of EMQX (Erlang MQTT broker v4.3.x). It transforms raw device connectivity into business-semantic operations via an OWL ontology engine. **Erlang 24.3**, **Rebar3** build system, OTP umbrella project with 26 applications.

**Core philosophy**: Model → Ontology → Device Access → Time-Series → Rules → Dashboard (6-step FDE workflow).

## Build & Run

```bash
# Full build + release (dgiot profile, the default)
make

# Full build + start in console mode (for dev/debug)
make run

# Build with the original EMQX profile
DGIOT_WITH_EMQX=true make run

# Verify both native and EMQX modes compile cleanly
make verify
```

**Do NOT use `make compile`** — this command is unsupported and will hang.

### Hot Compile (Primary Dev Workflow)

After `make run` is running, hot-reload changed modules without restart:

```bash
# Hot compile a plugin (general)
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot).'

# Hot compile a specific plugin (e.g., modbus)
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'

# Hot reload
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_modbus).'
```

Hot compile must produce **zero warnings**. Unused variables must use `_` prefix; unused functions must be removed or exported.

## Testing

```bash
# All EUnit tests (with coverage)
make eunit

# All Common Test suites (with coverage)
make ct

# PropEr property-based tests
make proper

# Tests for a specific app (e.g., dgiot_modbus, dgiot_parse, dgiot_ontology)
make dgiot_modbus-ct
make dgiot_parse-ct

# Plugin test framework (Modbus examples)
make test-modbus                # All Modbus tests
make test-modbus-simple         # Basic connectivity
make test-plugin PLUGIN=dgiot_xxx TESTCASE=simple

# List available plugins and test cases
make list-plugins
make list-testcases PLUGIN=dgiot_modbus
```

## Static Analysis

```bash
make xref       # Cross-reference checks
make dialyzer   # Type analysis (slow, use sparingly)
make cover      # Coverage report
```

## Architecture: 7-Layer Design

All code follows strict layer separation. Each layer communicates only with adjacent layers via defined interfaces. Never skip layers.

| Layer | Responsibility | Examples |
|-------|---------------|----------|
| 1. Transport | TCP/UDP connections, device registration, raw data forwarding | `dgiot_modbusrtu_tcp.erl` |
| 2. Protocol | Frame parse/encode, CRC check, format conversion | `modbus_rtu.erl` |
| 3. Message Routing | MQTT routing, task queues, parent-device aggregation | Message routing functions |
| 4. Business | Data decoding, attribute calculation, alarms, state mgmt | `dgiot_task.erl` |
| 5. Data | Time-series storage, queries, aggregation (TDengine) | `dgiot_tdengine_adapter.erl` |
| 6. Cache | Real-time cache, device state cache, session mgmt | Cache logic in `dgiot_task.erl` |
| 7. API | REST/gRPC queries, control commands, dashboard data | API handler modules |

**Key rules**:
- Transport layer: NEVER decode data or call business logic
- Protocol layer: NEVER store data or call business functions — use hooks
- Business layer: NEVER directly access the database — use data layer APIs
- Cross-layer calls use the hook system: `dgiot_hook:run_hook/2`

## Umbrella App Structure

```
apps/
  dgiot/             Core platform (supervisor, transport, crypto, rules, channels)
  dgiot_api/         REST API handlers and routers
  dgiot_parse/       Parse Server integration (PostgreSQL ORM substitute)
  dgiot_ontology/    OWL ontology engine (new, active development)
  dgiot_bridge/      Device bridge/connector framework
  dgiot_device/      Device management (thing models)
  dgiot_task/        Task and data processing pipeline
  dgiot_tdengine/    TDengine time-series DB adapter
  dgiot_http/        HTTP protocol adapter
  dgiot_modbus/      Modbus RTU/TCP protocol adapter
  dgiot_meter/       Meter reading protocols
  dgiot_topo/        Topology visualization
  dgiot_dlink/       Data link/channel management
  dgiot_bamis/       BI/analytics views
  emqx_*             EMQX plugins (auth, management, rule engine, retainer, etc.)
src/                  EMQX broker core (connection, channel, session, MQTT protocol)
lib-ce/               Community Edition: dashboard, modules, telemetry
```

## Extension Mechanisms

Three ways to extend the platform:

1. **Hook system** (`dgiot_hook`): Pub-sub callbacks. `one_for_one` (single handler) or `one_for_more` (chained). Register: `dgiot_hook:add/2,3`. Fire: `dgiot_hook:run_hook/2`. Hooks are the ONLY way for protocol and business layers to communicate.

2. **Protocol decoders** (`dgiot_protocol`): Modules declare `-protocol([MsgType1, MsgType2]).` in source. On startup `dgiot_protocol:start/0` auto-registers them as `{MsgType, parse_frame}` and `{MsgType, to_frame}` hooks. New protocol modules discovered via `dgiot_plugin:check_module/1`.

3. **Channel behaviour** (`dgiot_channelx`): A gen_server behaviour for device communication. Callbacks: `init/3`, `handle_message/2`, `handle_event/3`. Managed by `dgiot_channelx_mgr`, supervised by `dgiot_channelx_sup`.

## Supervision Tree

```
dgiot_sup (one_for_all)
  ├── dgiot_kernel_sup
  ├── dgiot_mnesia_sup
  ├── dgiot_cm_sup        (connection management)
  ├── dgiot_rule_engine_sup
  ├── dgiot_dcache         (disk cache worker)
  └── dgiot_channelx_mgr   (channel manager)
```

## Data Flow

```
Device → Transport (TCP/UDP/MQTT) → Protocol Decoder → Rule Engine → Storage/Action
                                                      ↓
                                              Hook callbacks (user plugins)
```

Key storage: **Mnesia** (config/routing tables), **TDengine** (device telemetry time-series), **Disk cache** (`dgiot_dcache`: persistent ETS).

## Known Issues

- **OTP 26 warning**: `OTP release 26 or later is required. Version in use: 24.3.2` — from newer rebar3 tooling. Project builds and runs fine on OTP 24.
- **rebar3_lint fails**: Fails to load on OTP 24. Harmless.
- **Module name / filename mismatch**: Erlang requires `-module(Name)` to match `.erl` filename exactly.
- **Dependency version warnings**: Multiple deps pull different versions of gun/jsx/ranch. Generally harmless unless runtime undefined function errors appear.

## Ontology Engine (`dgiot_ontology`)

The most actively developed subsystem. Core concepts:
- **5-dimension, 4-layer** semantic model: Thing → Collect → Compute → Manage → Act
- **OWL-based** with SWRL rules for closed-loop automation
- **PostgreSQL persistence** via `dgiot_parse` (zero epgsql dependency)
- **Hooks** integrate the ontology into the device data pipeline
- Startup: `./scripts/start_ontology.sh [m|e|b]` (m=manufacturing, e=energy, b=building)

Three semantic pillars: **Identity** (device ≠ order), **Relationship** (execution ≠ ownership), **Action** (fault → reschedule).

## Erlang Coding Standards

- **Module naming**: `*_handler.erl` (API gateway), `*_service.erl` (business logic), `*_dao.erl` (data access), `*_parser.erl` (protocol), `*_channel.erl` (channel), `*_utils.erl` (utilities)
- **Return values**: `{ok, Result}` / `{error, Reason}` / `{ok, Result, Extra}`. Use try-catch for unsafe ops.
- **ETS tables**: Declare with `-dgiot_data("ets")`, init via `dgiot_data:init/1`, access via `dgiot_data:get/1` / `dgiot_data:put/2`
- **Non-ASCII strings**: Always use `<<"中文"/utf8>>` binary format, or `dgiot_utils:safe_format("中文~n", [])`. Never raw `io:format("中文~n")`.
- **Code reuse**: Search existing implementations before writing — `grep -r "func_name" apps/ --include="*.erl"`. Prefer `dgiot_utils`, `dgiot_data` platform functions.
- **Three-tier API**: `Handler (API Gateway)` → `Function Gateway (dgiot_*.erl)` → `Implementation (service/dao)`. Never put business logic in handlers.
- **Commented-out debug logs**: Remove them entirely. Don't leave `% io:format(...)` in code.

## Log Management

Always use `?LOG(Level, Fmt, Args)` from `logger.hrl` — never create custom log systems. OTP logger only.

```bash
# Dynamic per-module log level (no restart needed):
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, debug).'
_build/emqx/rel/emqx/bin/emqx eval 'logger:get_module_level(dgiot_modbusrtu_tcp).'
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, error).'

# Monitor logs:
tail -f _build/emqx/rel/emqx/log/emqx.log.1 | grep -E "(ERROR|DEBUG.*modbus)"
```

## Channel Management

Channels handle device communication lifecycle. Key API:

```bash
# List all channels
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_channelx:get_all_channels().'
# Get channel detail
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_channelx:get_channel_info(<<"channel_id">>).'
# Check health
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_channelx:check_channel_health(<<"channel_id">>).'
# Restart a stuck channel
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_channelx:restart_channel(<<"channel_id">>).'
```

## Key Conventions

- **Commit messages**: Conventional Commits (`feat:`, `fix:`, etc.) enforced by commitlint
- **Config**: Cuttlefish schema in `priv/emqx.schema`, runtime config in `etc/emqx.conf`
- **API schema**: After adding REST endpoints, run `dgiot_parse_utils:update_schemas_json()`
- **WSL development**: Requires openEuler 24.03 WSL; port forwarding via `11_wsl_debug_rules.md`

## Deep Reference

For detailed debugging workflows, Modbus RTU frame analysis, sensor data pipeline troubleshooting, and integration test cycles, invoke: `/dgiot-dev`
>>>>>>> origin/dgaiot-plugins
