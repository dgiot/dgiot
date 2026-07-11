# Ontology Storage Architecture

## Three-Layer Storage

| Layer | Engine | Data | Speed |
|-------|--------|------|-------|
| Real-time | ETS | Shadow state, compiled rules | <1us |
| Relational | Parse/PG (JSONB) | 23 classes, ontology, ACL | ~10ms |
| Time-series | TDengine | telemetry (ts, value, quality) | ~5ms |

## Layer 1: Parse/PG — 23 Classes as JSONB

```sql
CREATE TABLE "Device" (
    "objectId"  TEXT PRIMARY KEY,
    "data"      JSONB,
    "createdAt" TIMESTAMP,
    "updatedAt" TIMESTAMP,
    "ACL"       JSONB
);
```

```json
// Device/rtu_001
{
  "objectId": "rtu_001",
  "data": {
    "name": "DEVICE_ID_PLACEHOLDER",
    "gateway": {"__type":"Pointer", "className":"Gateway", "objectId":"gw_131"},
    "product":  {"__type":"Pointer", "className":"Product",  "objectId":"oil_well_rtu"},
    "type": "oil_well_rtu",
    "protocol": "modbus",
    "slaveid": 1,
    "status": "online",
    "basedata": {"points":["oil_pressure","temperature"], "registers":{"oil_pressure":40300}}
  }
}
```

```
23 Classes:
  Ontology (4):  Site, Gateway, Device, Point
  Thing Model (2): Product, ProductTemplet
  Config (4): Channel, Dict, Category, Timescale
  System (5): _User, _Role, _Session, Menu, View
  Operations (5): Instruct, Notification, Log, Evidence, _SCHEMA
```

## Layer 2: ETS — In-Memory Cache

```
ETS 1: dgiot_ontology_model   {Class -> properties[], relations[], rules[]}
ETS 2: dgiot_ontology_instance {DeviceId -> class, model, pid, status, created}
ETS 3: dgiot_ontology_rules    {RuleId -> when, then, severity}

Lookup: O(1), ~1 microsecond
Memory: 211 devices x 3KB = 0.6MB
```

## Layer 3: TDengine — Time-Series

```sql
-- SuperTable: 每个产品一张 (productId 全局唯一)
CREATE STABLE dgiot_2de1b3e1b8 (
    ts      TIMESTAMP,
    value   FLOAT,
    quality INT
) TAGS (
    device_id  BINARY(64),    -- 设备地址 (如 DEVICE_ID_PLACEHOLDER)
    point_id   BINARY(64),    -- 点名 (如 oil_pressure)
    unit       BINARY(16)     -- 单位 (如 MPa)
);

-- SubTable: MD5(productId + devaddr) 保证全局唯一
CREATE TABLE dgiot_e8f3a9c2 
USING dgiot_2de1b3e1b8 
TAGS ('DEVICE_ID_PLACEHOLDER', 'oil_pressure', 'MPa');

-- 数据行: (ts, value, quality) + TAGS (device_id, point_id, unit)
INSERT INTO dgiot_e8f3a9c2 VALUES (NOW, 2.35, 192);
```

```
唯一性保证:
  Database   = dgiot_{ChannelId}           channel级隔离
  SuperTable = dgiot_{ProductId}           product级隔离
  SubTable   = dgiot_{MD5(ProductId+devaddr)}  设备级隔离
  Row        = ts + TAGS(device, point)    时间+点位唯一
```

## Query Paths

| Path | Engine | Use Case | Latency |
|------|--------|----------|---------|
| ETS instance | gen_statem PID | Real-time state | <1us |
| MQTT topic | EMQX subscribe | Streaming data | <10ms |
| Parse REST | PG JSONB | Configuration | ~10ms |
| TDengine SQL | Columnar TSDB | Historical trends | ~5ms |

## Data Flow

```
Physical Register (Modbus 40300)
  -> Edge Collector (readHoldingRegisters)
  -> MQTT (dgiot/oil_field_01/gw_131/rtu_001/oil_pressure/data)
  -> Shadow gen_statem (evaluate Rules -> state transition)
  -> Parse (update Device.status)
  -> TDengine (INSERT telemetry)
  -> ETS (update instance state)
```
