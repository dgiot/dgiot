# TDengine 本体存储 — 基于 dgiot 源码

> 源码: `dgiot_tdengine_schema.erl`, `dgiot_tdengine.hrl`, `dgiot_tdengine_channel.erl`

## 宏定义

```erlang
-define(PRE, <<"_">>).
-define(Database(Name), <<"_", Name/binary>>).        %% _5392ccb3d7
-define(Table(Name),    <<"_", Name/binary>>).        %% _2de1b3e1b8
```

**所有 TDengine 对象名以下划线开头。**

## 三层命名

```
Channel  ──→ Database:  _{ChannelId}          (或 _{ProductId}，最多 ProductId 优先)
Product  ──→ SuperTable: _{ProductId}         columns = thing.properties, tags = thing.tags
Device   ──→ SubTable:   ...USING _{ProductId}  TAGS(devaddr='DEV-001', ...)
```

## 创建流程

### Step 1: Channel 注册 → Database

```erlang
dgiot_tdengine_channel:check_database(ChannelId, ProductId, Config) ->
    Id = case dgiot_data:get({tdengine_db, ChannelId}) of
        <<"ProductId">> -> ProductId;    %% 优先用 ProductId
        _               -> ChannelId     %% 否则用 ChannelId
    end,
    DataBase = dgiot_tdengine_select:format_db(?Database(Id)),
    %% → CREATE DATABASE IF NOT EXISTS _5392ccb3d7 KEEP 10
```

### Step 2: Product → SuperTable

```erlang
dgiot_tdengine_channel:create_table(ChannelId, ProductId, Config) ->
    Schema = dgiot_tdengine_schema:get_schema(ChannelId, Product),
    TableName = ?Table(ProductId),   %% _2de1b3e1b8
    %% → CREATE TABLE IF NOT EXISTS _5392ccb3d7._2de1b3e1b8
    %%   (createdat TIMESTAMP, oil_pressure FLOAT, temperature FLOAT, ...)
    %%   TAGS (devaddr NCHAR(50), point_id NCHAR(64), unit NCHAR(16))
```

### Step 3: Schema 提取 — 物模型→列定义

```erlang
%% dgiot_tdengine_schema:get_schema/2
get_schema(ChannelId, #{<<"thing">> := Thing}) ->
    {Columns, Tags} = get_field_tag(Thing),
    %% Columns = 测点属性 (oil_pressure, temperature, flow_rate...)
    %% Tags    = 标签属性 (devaddr NCHAR(50) 强制保留)
    #{<<"fields">> => Columns, <<"tags">> => Tags}
```

### Step 4: Device → SubTable

```erlang
%% dgiot_tdengine:create_object (INSERT)
create_object(Channel, TableName, #{<<"values">> := Values}) ->
    DB = get_database(Channel, ProductId),
    %% → INSERT INTO _5392ccb3d7._a1b2c3d4
    %%   USING _2de1b3e1b8
    %%   TAGS ('DEV-001', 'oil_pressure', 'MPa')
    %%   VALUES (NOW, 2.35, 192)
```

## 本体→TDengine 映射

```
Ontology Layer        TDengine Object        Example
══════════════════════════════════════════════════════
Channel (通道)        Database               _5392ccb3d7
Product  (产品)       SuperTable             _2de1b3e1b8
  ├─ properties[]      Columns               oil_pressure FLOAT, temperature FLOAT
  └─ tags[]            TAGS                  devaddr NCHAR(50), unit NCHAR(16)
Device   (设备)       SubTable 或 INSERT      USING _2de1b3e1b8 TAGS(...)
Point    (测点)       单行 INSERT             VALUES (NOW, 2.35, 192)
```

## 唯一性保证（源码实现）

```
1. Database 唯一:
   dgiot_data:insert({tdengine_db, ChannelId, ProductId}, DataBase)
   → ChannelId + ProductId 联合索引

2. SuperTable 唯一:
   TableName = ?Table(ProductId) = "_" ++ ProductId
   → ProductId 全局唯一

3. SubTable 唯一:
   使用 TDengine USING 子句，数据归属 SuperTable
   → 设备通过 devaddr TAG 区分

4. 列唯一:
   alter_table() 对比 ETS 缓存的字段与 TDengine 实际列
   → 自动 ADD COLUMN / DROP COLUMN 同步

5. devaddr TAG 强制:
   proplists:get_value(<<"devaddr">>, NewTags) == undefined
   → NewTags ++ [{<<"devaddr">>, #{<<"type">> => <<"NCHAR(50)">>}}]
   → 每个子表必须有 devaddr 标签
```
