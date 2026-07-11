# DGAIOT 架构本体

> 从 Erlang 源码 (`wsl.localhost/openEuler/root/gitee/dgaiot`) 阅读中提取的核心架构本体论。
> 作为 dgiot_lite (Python 轻量版) 的设计对齐参考。

## 四层本体架构 (Data · Logic · Action · Security)

```
Security  ┌─────────────────────────────────────────────┐
          │  auth · role · ACL/CLP · beforeSave/afterSave│
Action    ├─────────────────────────────────────────────┤
          │  Shadow (gen_statem) · Bridge · MQTT · Rule  │
Logic     ├─────────────────────────────────────────────┤
          │  Ontology Engine · Model Registry · Reasoner │
Data      ├─────────────────────────────────────────────┤
          │  Parse/PG · TDengine · Mnesia/ETS · EMQX     │
          └─────────────────────────────────────────────┘
```

---

## 一、Data 层 — 23 个 Parse 类

| 类 | 用途 | 关键字段 |
|----|------|---------|
| Device | 设备注册 | devaddr*, name, product→Product*, ip, status, isEnable, basedata, profile |
| Product | 产品类型 | devType*, name*, category, producttemplet, thing, icon, nodeType |
| Channel | 通道配置 | cType*, name*, product→Product, isEnable, status, config, desc |
| ProductTemplet | 物模型模板 | name, icon, thing ({属性/服务/事件}), decoder, config |
| Dict | 字典表 | class, key, title, type, dict→Dict (树形) |
| Category | 分类 | name, level, order, parent→Category |
| _Role | 租户/岗位 | name, alias, parent_id, users (Relation), roles (Relation) |
| _User | 用户 | username, password_hash, role, sessionToken |
| _Session | 会话 | sessionToken, user→_User, expiresAt |
| Menu | 菜单 | name, path, icon, group, order, parent→Menu |
| View | 视图 | name, path, config |
| Timescale | 时序配置 | device_id, point_id, storage |
| Log | 日志 | device_id, level, message |
| Notification | 通知 | user→_User, title, body, status |
| Evidence | 附件 | device_id, file_url, type |
| Instruct | 指令 | device_id, command, params, status |

### TDengine 时序

```
Supertable: iot_telemetry
  Tags:     device_id, point_id, unit
  Columns:  ts, value, quality
```

### dgiot_lite 对齐

| DG-IoT | dgiot_lite |
|--------|-----------|
| Parse :1337 + PG :7432 | `parse_lite.py` + SQLite `parse.db` |
| TDengine :6041 | config.yaml → 172.22.193.167:6041 (远端) 或 SQLite 降级 |

---

## 二、Logic 层 — 本体引擎

```
dgiot_ontology:init()           → ETS 内存表
dgiot_ontology:load_model(M)    → 加载物模型 {class, props[], relations[], rules[]}
dgiot_ontology:spawn_instance() → 根据模型创建 Shadow 进程
dgiot_ontology_registry         → Class → Instances 注册表
dgiot_ontology_rule             → 规则编译 + evaluate + match
dgiot_ontology_reasoner         → 前向推理 (继承 + 规则链)
```

**物模型格式**:
```json
{
  "class": "compressor",
  "sub_class": "equipment",
  "properties": [
    {"id": "temperature", "type": "float", "unit": "celsius"},
    {"id": "pressure",    "type": "float", "unit": "mpa"}
  ],
  "rules": [
    {"id": "R1", "severity": "L1",
     "when": {"property": "temperature", "op": ">", "value": 85},
     "then": {"state": "warning", "action": "alarm"}}
  ]
}
```

### dgiot_lite 对齐

| DG-IoT | dgiot_lite |
|--------|-----------|
| ETS + load_model | parse_lite 动态 Schema (ensure_table) |
| ProductTemplet | ProductsView TSL 分区 (属性/事件/服务) |
| 规则引擎 | safety_rules.py + phm_engine.py |

---

## 三、Action 层 — 设备影子

**每个物理设备 = 一个 Erlang gen_statem 进程**:

```
状态: normal → warning(告警) → critical(60s未消) → offline(心跳30s超时)
Shadow = {device_id, class, properties{}, model{}, rules[], last_update}

sensor_update(Props)
  → evaluate(Rules, Props)        %% 第二层: 规则匹配
  → state transition              %% 第三层: 状态迁移
  → bridge → Parse/TDengine       %% 持久化
```

### dgiot_lite 对齐

| DG-IoT | dgiot_lite |
|--------|-----------|
| gen_statem Shadow | collector.py (状态机 待增强) |
| Bridge → Parse | parse_lite API |
| Bridge → TDengine | tdengine.py / sqlite fallback |

---

## 四、Security 层

- **dgiot_parse_auth**: login → sessionToken, check_session
- **dgiot_role**: 层级角色树, users/roles Relation
- **ACL**: 对象级 `{"*":{"read":true},"role:X":{"write":true}}`
- **CLP**: 类级 `{find:{"*":true},create:{"role:root":true}}`

### dgiot_lite 对齐

| DG-IoT | dgiot_lite |
|--------|-----------|
| parse_auth + role | auth.py JWT + tenant_api.py |
| ACL | parse_lite.check_acl() |
| CLP | parse_lite.check_clp() |

---

## 五、插件架构

```erlang
-dgiot_plugin(Order).  %% 声明插件 + 启动顺序

dgiot_app:start()
  → dgiot:init_plugins()           %% 扫描所有 -dgiot_plugin 模块
  → 按 Order 排序加载
  → 调用 Mod:start_link() 启动
```

**协议插件**: dgiot_modbus, dgiot_meter(DLT645/376), dgiot_dlink
**功能插件**: dgiot_topo(拓扑), dgiot_task(任务), dgiot_bridge(桥接)

### dgiot_lite 对齐

| DG-IoT | dgiot_lite |
|--------|-----------|
| -dgiot_plugin 声明 | `VENDOR_CHANNELS` 注册表 + `src/protocols/` |
| dgiot_modbus | modbus_tcp.py, modbus_scanner.py |
| dgiot_meter | a11.py, youyeyun.py |
| dgiot_task | collector.py |
| dgiot_bridge | push/ MQTT + HTTP |

---

## 六、FDE 六步工作流

```
Model → Ontology → Device Access → TimeSeries → Rules → Dashboard
  1        2           3              4           5          6
```

| 步骤 | DG-IoT | dgiot_lite |
|------|--------|-----------|
| 1. Model | ProductTemplet → Parse | ProductsView (TSL zones) |
| 2. Ontology | dgiot_ontology:load_model | parse_lite 动态表 |
| 3. Device | Shadow gen_statem | collector + 设备注册 |
| 4. TimeSeries | TDengine supertable | TDengine / SQLite |
| 5. Rules | rule engine + reasoner | safety_rules + phm_engine |
| 6. Dashboard | 2D组态 + 报表 | 12页 Vue3 后台 |

---

## 七、dgiot_lite 实施状态

| 模块 | 状态 |
|------|------|
| parse_lite.py (CRUD/ACL/CLP/Hooks/Batch) | ✅ |
| dgiot_schema.py (Device/Product/Channel/Templet) | ✅ |
| 多租户 (tenants + user_roles + X-Tenant-ID) | ✅ |
| TDengine 连接 (172.22.193.167:6041) | ✅ |
| 厂商通道插件 (6个 VENDOR_CHANNELS) | ✅ |
| youyeyun.py 协议适配器 | ✅ |
| Vue3 前端 (12页7组) | ✅ |
| 设备影子 (state machine) | ⚠️ 待实现 |
| 规则引擎增强 | ⚠️ 待实现 |
