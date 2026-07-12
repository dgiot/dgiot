# 新设备接入指南 — 以"注水泵 PUMP-003"为例

## 接入步骤

### Step 1: 定义物模型 (thing_model.json)

```json
{
  "name": "泵出口压力",
  "identifier": "pump_outlet_pressure",
  "moduleType": "properties",
  "dataForm": {
    "address": "40500",
    "slaveid": "40500",
    "protocol": "modbus",
    "operatetype": "readHoldingRegisters",
    "originaltype": "float32_AB",
    "strategy": 20
  },
  "dataType": {"type": "float"},
  "isstorage": true,
  "accessMode": "r",
  "devicetype": "D1",
  "alarm": {"high": 10.0, "low": 0.5},
  "range": [0, 16]
}
```

### Step 2: 计算 objectId (确定性)

```python
import hashlib

# Product: md5("Product" + CategoryId + DevType + Name)
product_id = hashlib.md5(
    "Product" + "category_oilfield" + "injection_pump" + "注水泵"
).hexdigest()[:10]
# → "a3f8c2e1b4"

# Device: md5("Device" + ProductId + DevAddr)
device_id = hashlib.md5(
    "Device" + product_id + "PUMP-003"
).hexdigest()[:10]
# → "7d2e1f9a5c"

# Permission: md5("Permission" + Name)
rule_id = hashlib.md5(
    "Permission" + "pump_overpressure_alarm"
).hexdigest()[:10]
# → "b1c4e8f2a6"
```

### Step 3: 注册本体 (Parse)

```bash
# 产品
curl -X POST http://localhost:1337/parse/classes/Product \
  -H "X-Parse-Application-Id: ddc9ac052450367e4a03c4056c21bff8" \
  -d '{"objectId":"a3f8c2e1b4","devType":"injection_pump","name":"注水泵"}'

# 设备
curl -X POST http://localhost:1337/parse/classes/Device \
  -H "X-Parse-Application-Id: ddc9ac052450367e4a03c4056c21bff8" \
  -d '{
    "objectId":"7d2e1f9a5c",
    "devaddr":"PUMP-003",
    "name":"3号注水泵",
    "product":{"__type":"Pointer","className":"Product","objectId":"a3f8c2e1b4"},
    "gateway":{"__type":"Pointer","className":"Gateway","objectId":"gw_131"},
    "type":"injection_pump",
    "protocol":"modbus",
    "slaveid":3,
    "status":"init",
    "isEnable":true,
    "ACL":{"*":{"read":true},"role:operator":{"write":true}}
  }'
```

### Step 4: MQTT 设备授权

```
ClientID:   a3f8c2e1b4_PUMP-003     ← {ProductID}_{DevAddr}
Username:   a3f8c2e1b4               ← ProductID
Password:   <ProductSecret>          ← dgiot_product:get_productSecret(PID)
Topic pub:  $dg/thing/a3f8c2e1b4/a3f8c2e1b4_PUMP-003/properties/report
```

```python
import paho.mqtt.client as mqtt
import json, hashlib

PID = "a3f8c2e1b4"
DEVADDR = "PUMP-003"
CID = f"{PID}_{DEVADDR}"
PWD = hashlib.md5(f"dgiot_{PID}".encode()).hexdigest()

client = mqtt.Client(client_id=CID, protocol=mqtt.MQTTv311)
client.username_pw_set(username=PID, password=PWD)
client.connect("127.0.0.1", 1883, 60)

# 上报数据
topic = f"$dg/thing/{PID}/{CID}/properties/report"
data = {"pump_outlet_pressure": 4.2, "pump_status": 1}
client.publish(topic, json.dumps(data))
```

### Step 5: TDengine 自动建表

```sql
-- dgiot_tdengine 自动执行 (create_schemas)
CREATE DATABASE IF NOT EXISTS _85ef6b7459 KEEP 365;
CREATE STABLE IF NOT EXISTS _85ef6b7459._a3f8c2e1b4
  (ts TIMESTAMP, value FLOAT, quality INT)
  TAGS (devaddr NCHAR(50));

-- 设备首次上报时自动建子表
CREATE TABLE IF NOT EXISTS _85ef6b7459.sub_a3f8c2e1b4_PUMP003
  USING _85ef6b7459._a3f8c2e1b4
  TAGS('PUMP-003');

-- 每条数据自动 INSERT
INSERT INTO _85ef6b7459.sub_a3f8c2e1b4_PUMP003 VALUES (NOW, 4.2, 192);
```

### Step 6: gen_statem 影子进程

```erlang
%% dgiot_ontology:spawn_instance(a3f8c2e1b4, "7d2e1f9a5c")
%% → {ok, ShadowPid}

%% Shadow 收到 MQTT 数据:
online(cast, {data, #{pump_outlet_pressure := 4.2}}, Device) ->
    evaluate(Rules, Props),   %% R1: pressure>10? → false → keep online
    dgiot_ontology:push_point(pump_outlet_pressure, 4.2),
    {keep_state, Device}.
```

## 接入总结

```
新设备 PUMP-003 接入全流程:

1. 物模型    thing_model.json 加 1 条 property
2. objectId   MD5 确定性计算 (不查DB)
3. 注册      Parse POST Product + Device
4. MQTT认证   ClientID={PID}_{DevAddr} ProductSecret
5. 建表      TDengine 自动 CREATE TABLE
6. 影子      gen_statem spawn → online → 接收数据

总耗时: <1分钟 (AI自动生成前三步, 人工确认后部署)
```
