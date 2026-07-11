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

### TDengine 时序 (基于 dgiot_tdengine 源码)

```
宏:
  Database   = _{ChannelId}                    (或 _{ProductId}, ETS缓存)
  SuperTable = _{ProductId}                     (列=properties, 标签=tags+devaddr)

ETS 映射:
  {tdengine_db, ChannelId, ProductId} -> DB
  {ProductId, "TD"} -> ChannelId
  {td, ProductId, DeviceId} -> SubTable
  {ProductId, describe_table} -> [Columns]

devaddr 强制标签:
  proplists:get_value(<<"devaddr">>, Tags) == undefined
    -> Tags ++ [{<<"devaddr">>, NCHAR(50)}]

创建流程:
  1. create_database:  CREATE DATABASE IF NOT EXISTS _{Id} KEEP 10
  2. create_table(ST):  CREATE TABLE IF NOT EXISTS _{ProductId} (cols) TAGS(devaddr NCHAR(50),...)
  3. create_table(SUB): CREATE TABLE IF NOT EXISTS ... USING _{ProductId} TAGS(...)
  4. alter_table:       对比 ETS 缓存列定义, 自动 ADD/DROP COLUMN
```

### dgiot_lite 对齐

| DG-IoT | dgiot_lite |
|--------|-----------|
| Parse :1337 + PG :7432 | `parse_lite.py` + SQLite `parse.db` |
| TDengine :6041 | config.yaml → 192.168.10.167:6041 (远端) 或 SQLite 降级 |

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

### 本体 ↔ 物理世界映射

**四层联动 — 静态本体模型 + 动态影子进程 + MQTT 桥梁**

```
物理世界                           dgaiot 影子世界
────────                          ────────────────

┌──────────┐    Modbus/A11       ┌──────────────────────┐
│ RTU-001  │ ──────────────────→ │ gen_statem Shadow    │
│ 油井井口 │   MQTT Topic:       │ PID: <0.123.0>       │
│ 11.66.12 │ dgiot/oil_field_01/ │ State: online        │
│ .130:8889│ gw_131/rtu_001/     │ Props: #{            │
└──────────┘ oil_pressure/data   │   oil_pressure→2.35  │
                                 │   temperature→45.6   │
  物理设备                        │ }                    │
  1:1 Shadow                     └──────────┬───────────┘
                                            │
                                       ┌────┴──────┐
                                       ▼           ▼
                                   ┌──────┐   ┌────────┐
                                   │Parse │   │TDengine │
                                   │:1337 │   │ :6041   │
                                   └──────┘   └────────┘
```

**本体映射链:**

```
Site:  oil_field_01    ← 采油厂
  └─ Gateway: gw_131   ← IO服务器 192.168.10.131:53001 (Modbus主站)
       └─ Device: rtu_001  ← 井口RTU (物理设备)
            ├─ Point: oil_pressure     ← Modbus 40300 float32_AB
            ├─ Point: casing_pressure  ← Modbus 40302
            └─ Point: temperature      ← Modbus 40304

MQTT Topic:
dgiot/{site}/{gateway}/{device}/{point}/data
dgiot/oil_field_01/gw_131/rtu_001/oil_pressure/data
```

**生命周期:**

```
1. 注册 (Data层):
   dgiot_ontology:register(device, #{id=>rtu_001,gateway=>gw_131,...})
   → Parse: CREATE Device

2. 启动 (Logic层):
   dgiot_ontology:spawn_instance(rtu_class, rtu_001)
   → {ok, ShadowPid}  %% gen_statem 进程启动

3. 数据注入 (Action层):
   ShadowPid ! {data, #{oil_pressure => 2.35}}
   → evaluate(Rules, Props)  %% 温度>85 → warning
   → state: normal → alarm
   → bridge → Parse + TDengine

4. MQTT 路径 (Data→Action):
   dgiot_ontology:push_point(oil_pressure, 2.35)
   → Topic: dgiot/oil_field_01/gw_131/rtu_001/oil_pressure/data
   → Payload: {ts:1751884800, v:2.35, q:192}
```

**核心原则**: 本体是静态模型 (Class/Property/Relation)，影子是动态实例 (PID/State/Value)，MQTT 是物理世界与数字世界的桥梁。

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
| dgiot_meter | a11.py, vendor_oilmon.py |
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
| TDengine 连接 (192.168.10.167:6041) | ✅ |
| 厂商通道插件 (6个 VENDOR_CHANNELS) | ✅ |
| vendor_oilmon.py 协议适配器 | ✅ |
| Vue3 前端 (12页7组) | ✅ |
| 设备影子 (state machine) | ⚠️ 待实现 |
| 规则引擎增强 | ⚠️ 待实现 |
| 131 IO Server 本体建模 | ✅ 2026-07-11 |

---

## 八、131 IO Server 本体实例 (2026-07-11 WinRM 深度扫描)

> 通过本体论方法论，将 131 GENERIC_VENDOR IoMonitor 服务器的完整架构建模为可查询、可推理、可执行的五层本体。

### Step 1: 盘点到齐 — 25 个实体

#### Data 层 (14 实体)

| 实体 | 类型 | 标识 | 状态 |
|------|------|------|------|
| IoMonitor.exe | 进程 | PID 18400, 63MB | ✅ 运行中 |
| IoCommit.exe | 进程 | 数据提交引擎 | ✅ 活跃 |
| CommBridge.exe | 进程 | 通信网桥 | ⚠️ 频繁崩溃 |
| IOMan.exe | 进程 | IO 管理器 | ⛔ 大量崩溃(2026-03) |
| IM_A11_RTU | 协议驱动 | ioapi.dll | ✅ A11 采油厂协议 |
| OPC_FC_Client | 协议驱动 | ioapi.dll | ✅ OPC DA 客户端 |
| Standard_Umodbus | 协议驱动 | PORTCONF.DAT | ✅ Modbus |
| A11SQLSERVICE.exe | 转储服务 | RTUSql 目录 | ✅ A11→Oracle |
| eForceCon DB | 数据库 | GENERIC_VENDOR自研 | ⛔ 已停用 |
| RTDB | 实时库 | GENERIC_VENDOR实时数据库 | ⛔ 2022年停 |
| Oracle 11.2.0.1 | 数据库 | 192.168.10.129:1521/orcl | ✅ 活跃 |
| OPCDAAuto.dll | COM组件 | SysWOW64 | ✅ 已注册(32位) |
| OPC Core Components 2.00 | SDK | D:\OPC Core Components...msi | ✅ 已安装 |
| TagID_IOCommitDB*.dat | 映射文件 | E:\IO ServerOnLine\run\ | ✅ 实时更新 |

#### Logic 层 (5 实体)

| 实体 | 来源 | 关键参数 |
|------|------|---------|
| IoMonitor.ini | 采集规则 | CommitRealSpan=300ms, CommitHisSpan=500ms |
| IoChannelCfg.ini | 通道配置 | SYNCH×1, CommBridge×3, TCP×1 |
| Device.ini | 设备定义 | 12类保护继电器 + 计算公式 |
| SqlFilSet.ini | Oracle连接 | Provider=OraOLEDB.Oracle.1; INDUSTRYPROD@orcl |
| OPCClientCfg.ini | OPC客户端 | 4个OPC Server配置 |

#### Action 层 (4 实体)

| 实体 | 参数 | 语义 |
|------|------|------|
| 实时提交管线 | 300ms + 15,000点/批 | 测点值→IoCommit→Oracle |
| 历史提交管线 | 500ms + 15,000点/批 | 历史曲线写入 |
| A11 SQL 管线 | F:\TRANgo\...\RTUSql | A11 RTU→SQL→Oracle |
| OPC 采集管线 | DCOM :135 | OPC Server→GENERIC_VENDORClient→IoMonitor |

#### Security 层 (2 实体)

| 实体 | 值 |
|------|-----|
| Oracle 凭据 | INDUSTRYPROD / industrya11_PASS |
| WinRM 入口 | Administrator / 5985 |

### Step 2: 连线成网 — 关系矩阵

```
                      Field Layer (物理世界)
                      ─────────────────────
  井口RTU (20+)           OPC Server ×4           Modbus RTU
  11.248.x.x             .9.23 .18.194            11.248.x.x
  A11 TCP :8889          .26.6.3 .21.14.192      TCP :53001
       │                       │                       │
       │ A11协议               │ DCOM                  │ Modbus TCP
       ▼                       ▼                       ▼
  ┌────────────────────────────────────────────────────────┐
  │                  IO Layer (131 服务器)                   │
  │  IM_A11_RTU      OPC_FC_Client      Standard_Umodbus   │
  │  ioapi.dll       ioapi.dll          PORTCONF.DAT       │
  │       │               │                  │              │
  │       └───────────────┼──────────────────┘              │
  │                       ▼                                │
  │               IoMonitor.exe (PID 18400)                 │
  │               实时采集 · 状态管理 · 事件检测              │
  │                       │                                │
  │                       ▼                                │
  │               IoCommit.exe                              │
  │               TagID 映射 · 批量提交                      │
  └───────────────────────┬────────────────────────────────┘
                          │
              ┌───────────┼───────────┐
              ▼           ▼           ▼
         Oracle :1521  RTDB      eForceCon DB
         (活跃)       (停用)       (停用)
```

**关系类型统计:**
| 关系 | 数量 | 示例 |
|------|------|------|
| protocol_driver → gateway | 6 | ch_opc_da, ch_a11_rtu... |
| channel → device | 14 | 12 relay + 1 OPC + 1 Modbus |
| device → point | 6+ | Ia,Ib,Ic,Ua,F,P (sample) |
| gateway → datasource | 3 | Oracle, RTDB, eForceCon |
| constraint → channel | 6 | 实时提交/批量/超时/连接池 |
| gateway → opc_server | 4 | 172.23.9.23/.3/.18.194/.26.6.3 |

### Step 3: 设卡立规 — 6 条约束

| ID | 约束 | 严重度 | 来源 |
|----|------|--------|------|
| c_commit_real | 实时数据提交延迟 ≤ 300ms | info | IoMonitor.ini |
| c_commit_batch | 单次提交上限 15000点 | warning | IoMonitor.ini |
| c_io_timeout | IO设备 30s 无响应 → 离线 | danger | IoChannelCfg.ini |
| c_ado_pool | Oracle 连接池上限 4 | warning | SqlFilSet.ini |
| c_current_overload | Ia/Ib/Ic > 5A + 持续 >1s → 过流告警 | danger | Device.ini |
| c_voltage_abnormal | U < 198V or U > 260V + 持续 >10s → 电压异常 | danger | Device.ini |

### Step 4: 闭环验证 — 活性证据

```
TagID_IOCommitDB3_DEVICE_D.dat → 2026-07-11 21:45 (385KB) ✅ 活跃
TagID_IOCommitDB5_DEVICE_D.dat → 2026-07-11 22:52 (319KB) ✅ 活跃
IoMonitor.exe → 192.168.10.129:1521 ESTABLISHED          ✅ Oracle连通
131 → 192.168.10.130:8889 ×7 ESTABLISHED                  ✅ A11连通
131 → 20+ 11.248.x.x:53001 ESTABLISHED                  ✅ Modbus连通
131 → 172.23.9.23:135 ESTABLISHED                       ✅ OPC DA连通
```

### 五层本体结构总览

```
Site:    industry_c1 (大庆采油厂)
  └─ Gateway: gw_131 (IO-SERVER-01, 192.168.10.131)
       ├─ Channel: ch_opc_da       → OPC DA Client → 4 OPC Servers
       ├─ Channel: ch_a11_rtu      → A11 采油厂协议 → 130:8889
       ├─ Channel: ch_modbus_tcp   → Modbus → 20+ RTU
       ├─ Channel: ch_oracle       → Oracle 数据出口 → 129:1521
       ├─ Channel: ch_rtdb       → RTDB (已停)
       └─ Channel: ch_eforcecon    → eForceCon DB (已停)
            │
            ├─ Device: dev_relay_00~110 (12类保护继电器)
            ├─ Device: dev_opc_device_1
            └─ Device: dev_rtu_wellhead
                 │
                 └─ Point: pt_ia, pt_ib, pt_ic, pt_ua, pt_freq, pt_power
```

### 本体引擎 API 使用

```python
from src.ontology import build_131_ontology

engine = build_131_ontology()

# 完整性校验
print(engine.validate())
# {"valid": true, "counts": {"sites":1,"gateways":1,"channels":6,"devices":14,"points":6,"constraints":6,"datasources":3}}

# 树形导出
tree = engine.tree("industry_c1")

# MQTT 路径
engine.get_path("pt_ia")
# → dgiot/industry_c1/gw_131/ch_a11_rtu/dev_relay_00/pt_ia

# 推送到 MQTT
engine.push_point("pt_ia", 2.35)
# → dgiot/industry_c1/gw_131/ch_a11_rtu/dev_relay_00/pt_ia/data {"ts":...,"v":2.35,"q":192}

# 同步到 SQLite
engine.sync_to_parse("default")
```
