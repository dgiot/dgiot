# DGAIoT Pragmatic Enhancement Architecture — Mature Components, No Clones

> Companion documents: [comparison.md](comparison.md) (DGIOT vs. Palantir positioning)
> and [open-source-palantir-landscape.md](open-source-palantir-landscape.md)
> (reality-checked OSS survey).
>
> Principle: build data-fusion and analytics on production-proven open-source
> components — **no dependency on unproven "open-source Palantir" clones**. The
> landscape survey shows only two such projects have real communities (WorldMonitor,
> Semantica), and neither targets the industrial edge. We do not need them to ship.
>
> 中文版（Chinese version）: [zh/pragmatic-enhancement-architecture.md](zh/pragmatic-enhancement-architecture.md)

## Core Principles

1. **DGIOT stays as is** — device connection, protocol conversion and data
   acquisition are the foundation; no rework.
2. **Layered decoupling** — acquisition / storage / analysis / visualization are
   separated; each layer picks a mature component.
3. **Incremental enhancement** — get the basic pipeline working first, then layer
   in rules and AI.

## Recommended Stack

| Layer | Component | Purpose | Notes |
|---|---|---|---|
| **Data bus** | **Apache Kafka** | Async decoupling, peak shaving | DGIOT pushes device data into Kafka; downstream consumers scale freely. |
| **Time-series store** | **TimescaleDB** (PostgreSQL extension) | Device telemetry | Full SQL; joins naturally with relational metadata for analytical queries. |
| **Relational metadata** | **PostgreSQL** | Object definitions, device registry, work orders, personnel — the pragmatic "ontology" | Foreign keys give a lightweight ontology without a graph database. |
| **Rules engine** | **Node-RED** or **Drools** | Real-time thresholds → alerts → actions | Covers ~90% of industrial threshold scenarios with low latency and high reliability. Node-RED subscribes to Kafka via its Kafka extension nodes. |
| **Scheduling** | **Apache Airflow** | ETL orchestration, periodic reports | Offline aggregation and model-training triggers. |
| **Visualization** | **Grafana** + **Apache Superset** | Dashboards + self-service BI | Grafana excels at real-time monitoring; Superset at drag-and-drop BI; they coexist. |
| **Optional AI/ML** | **MLflow** + PyTorch / XGBoost | Predictive models | When rules are not enough (failure prediction, quality optimization); model output can call back into the rules engine. |

## Fitting the Existing DGIOT Stack

DGIOT already ships several pieces this plan would introduce — reuse them
consciously instead of creating parallel systems:

- **MQTT/EMQX bridge**: Phase 1's "forward device data to Kafka" is one new sink on
  the existing `dgiot_bridge` — no new protocol work.
- **Time-series store**: DGIOT ships **TDengine**. Choose deliberately: keep
  TDengine for high-ingest telemetry and add TimescaleDB only where relational
  JOINs with metadata matter — or standardize on one. Do not run both blindly.
- **Metadata / "ontology"**: DGIOT's Parse Server (23 classes) + PostgreSQL already
  hold device registry data. Phase 2's business tables should **extend** those
  classes rather than create a parallel schema.

## Roadmap (4 phases, 2–4 weeks each)

### Phase 1 — Data pipeline (weeks 1–3)

- Deploy Kafka (single node with KRaft is fine to start).
- Extend the DGIOT MQTT bridge to forward device data into Kafka.
- Start TimescaleDB; create the `device_telemetry` hypertable
  (timestamp, device_id, metric key/value).
- Write a simple Kafka consumer (Python/Java) that writes into TimescaleDB.

### Phase 2 — Lightweight "ontology" modeling (weeks 4–6)

- Create business tables in PostgreSQL: `devices`, `product_lines`,
  `maintenance_orders`, `personnel` (extending existing Parse classes where they
  already cover the entity).
- Establish foreign-key relations (e.g. `maintenance_orders.device_id → devices.id`).
- Use hypertable JOINs with `devices` to form a unified per-device view.

### Phase 3 — Real-time rules & alerting (weeks 7–8)

- Deploy Node-RED (with Kafka extension nodes) or a Drools service.
- Subscribe to the telemetry topic; threshold rules (e.g. temperature > 85 °C
  sustained for 5 minutes) trigger alerts.
- Alerts auto-create work orders (written back to PostgreSQL) or notify via
  email / enterprise IM.

### Phase 4 — Visualization & self-service analytics (weeks 9–12)

- Grafana on TimescaleDB: real-time dashboards (per-device metric curves, alert status).
- Superset on PostgreSQL (metadata) + TimescaleDB (telemetry): interactive
  dashboards for business users to explore without engineering help.

### Optional — AI prediction (separate initiative, on demand)

- Collect historical telemetry; train lightweight LSTM or XGBoost models, managed
  with MLflow.
- Deploy models as REST APIs; the rules engine calls them periodically; prediction
  results land in the database and trigger early warnings.

## Palantir Comparison (the honest version)

| Capability | Palantir approach | This approach | Trade-off |
|---|---|---|---|
| Data integration | Proprietary connectors + ontology mapping | DGIOT + Kafka + own consumers | Lighter, but self-maintained |
| Ontology | Proprietary semantic layer, dynamic object relations | PG foreign keys + views | Limited but stable; fits small/mid scale |
| Decisioning | AIP large models + simulation | Rules engine + optional ML | Cannot handle extreme complexity, but covers 90% of industrial scenarios |
| App development | Workshop low-code | Grafana / Superset | Less flexible, far lower learning cost |

## Bottom Line

- Every component is **production-grade OSS** — no toy dependencies (see the
  reality-checked landscape survey for why we avoid the clones).
- Low cost: the minimum footprint (Kafka + PostgreSQL + Grafana) runs on a single
  4C16G server.
- Extensible: Spark/Flink can be introduced later for real-time stream compute.
- **No need to wait for any "open-source Palantir" to mature — this lands now.**
