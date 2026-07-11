# Ontology Storage Architecture (based on dgiot source)

> Source: `dgiot_tdengine_schema.erl`, `dgiot_tdengine.hrl`, `dgiot_tdengine_channel.erl`

## Macros

```erlang
-define(PRE, <<"_">>).
-define(Database(Name), <<"_", Name/binary>>).
-define(Table(Name),    <<"_", Name/binary>>).
```

## Three-Layer Storage

| Layer | Engine | Object | Key | Speed |
|-------|--------|--------|-----|-------|
| Memory | ETS/dgiot_data | Instance state, compiled rules | {td, ProductId, DeviceId} | <1us |
| Relational | Parse/PG JSONB | 23 classes, ontology, ACL | objectId | ~10ms |
| Time-series | TDengine | Telemetry | _{ChannelId}._{ProductId} | ~5ms |

## TDengine Storage

```
Database   = _{ChannelId}          (or _{ProductId} if configured, cached in ETS)
SuperTable = _{ProductId}           (cols = thing.properties, tags = thing.tags + devaddr)
SubTable   = INSERT INTO _{DB}._{ProductId} USING _{ProductId} TAGS(devaddr=..., ...)
```

## ETS Mapping Keys

| Key | Value | Description |
|-----|-------|-------------|
| {tdengine_db, ChannelId, ProductId} | DatabaseName | DB name cache |
| {ProductId, "TD"} | ChannelId | Product to Channel mapping |
| {td, ProductId, DeviceId} | SubTableName | Device to SubTable |
| {ProductId, describe_table} | [Columns] | Column defs for alter_table |
| {ProductId, fields_table} | [Fields] | Field cache |
| {last_data, DeviceId} | Data | Last received data |

## Create Flow

1. Channel -> Database: `CREATE DATABASE IF NOT EXISTS _{ChannelId} KEEP 10`
2. Product -> SuperTable: `CREATE TABLE IF NOT EXISTS _{ProductId} (cols) TAGS (devaddr NCHAR(50), ...)`
3. Device -> INSERT: `INSERT INTO _{DB}._{ProductId} USING _{ProductId} TAGS(...) VALUES (NOW, v, q)`

## Mandatory devaddr Tag

```erlang
%% dgiot_tdengine_schema.erl:62-67
proplists:get_value(<<"devaddr">>, NewTags) == undefined
  -> NewTags ++ [{<<"devaddr">>, #{<<"type">> => <<"NCHAR(50)">>}}]
```

Every subtable MUST have a `devaddr` tag. Auto-added as NCHAR(50) if missing.

## Uniqueness Chain

```
Database:   {tdengine_db, ChannelId, ProductId}  -> ETS key
SuperTable: ProductId (Parse ObjectId)            -> globally unique
SubTable:   {td, ProductId, DeviceId}             -> ETS key
devaddr:    mandatory TAG (NCHAR 50)              -> per-device unique
```
