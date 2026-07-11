# 自动创建 + 审核本体

## 当前链路

```
物理设备 → AI 读配置 → 自动生成本体 → 人工审核 → AI 验证 → 确定性生产
   │            │            │            │          │           │
  Modbus     thing_model   Product/      human      range      gen_statem
  registers  .json         Device/       reviews    check      MQTT
  A11 frames               Point rules   approve    conflict   TDengine
```

## Step 1: AI 读配置 → 自动生成本体

```
输入:
  油水井寄存器.xlsx (Modbus 地址表)
  io_ontology.json (网络拓扑)
  Device.ini (保护继电器定义)
  runBack1.zio (运行时设备列表)

AI 输出:
  thing_model.json      125 properties + alarms
  ProductTemplet        oil_well_rtu, modbus_rtu, opc_device ...
  Device registration   rtu_001..rtu_206 (206 Modbus RTU)
  Point registration    oil_pressure, temperature, pump_status ...
  Channel config        Modbus TCP :53001, A11 :8889
  TDengine schema       _{ChannelId}._{ProductId}
```

```
验证: 自动生成的 ontology → deploy → pipeline test → logs confirm
  不是"生成完就完"，是"生成→部署→日志验证→通过后才算真正的本体"
```

## Step 2: AI 审核本体

```python
def audit_ontology(thing_model, devices, io_topology):
    """AI 审核本体 — 7 项检查"""
    issues = []

    # 1. 寄存器地址冲突
    addrs = {}
    for p in thing_model['properties']:
        addr = p['dataForm']['address']
        if addr in addrs:
            issues.append(f"ADDR_CONFLICT: {p['name']} vs {addrs[addr]} both use {addr}")
        addrs[addr] = p['name']

    # 2. 数据类型不匹配
    for p in thing_model['properties']:
        fmt = p['dataForm'].get('originaltype','')
        dt = p['dataType']['type']
        if fmt == 'float32_AB' and dt != 'float':
            issues.append(f"TYPE_MISMATCH: {p['name']} register={fmt} but dataType={dt}")

    # 3. 告警阈值不合理
    for p in thing_model['properties']:
        alarm = p.get('alarm', {})
        rng = p.get('range', [])
        if alarm.get('high') and rng and alarm['high'] > rng[1]:
            issues.append(f"ALARM_OOB: {p['name']} alarm_hi={alarm['high']} > range_max={rng[1]}")

    # 4. devaddr 重复
    devaddrs = [d.get('devaddr') for d in devices]
    if len(devaddrs) != len(set(devaddrs)):
        issues.append("DEVADDR_DUPLICATE: duplicate device addresses")

    # 5. 协议端口冲突
    ports = {}
    for gw in io_topology.get('ports', []):
        key = (gw.get('server_id'), gw['port'])
        if key in ports:
            issues.append(f"PORT_CONFLICT: server {key[0]} port {key[1]} used by {ports[key]} and {gw['service']}")
        ports[key] = gw['service']

    # 6. TDengine 表名长度 (TD限制 192)
    for d in devices:
        name = f"sub_{d['product']}_{d['devaddr']}"
        if len(name) > 192:
            issues.append(f"TABLE_TOO_LONG: {name} ({len(name)} chars)")

    # 7. ACL 规则覆盖检查
    for d in devices:
        pid = d.get('productid', 'unknown')
        if pid == 'unknown':
            issues.append(f"ACL_GAP: device {d.get('devaddr','?')} has no productid — ACL will deny")

    return issues
```

## Step 3: 对接 AI 完成场景智能化升级

AI 审核通过后，基于本体自动生成智能规则：

```
本体 (已审核) → AI 场景分析 → 智能升级建议 → 人工确认 → 部署

不是"AI 替代确定性系统"
是"AI 分析本体，发现可优化的场景，生成升级方案"
```

### 智能升级类型

| 场景 | 本体提供 | AI 分析 | 升级方案 |
|------|---------|---------|---------|
| 告警阈值优化 | point.alarm | 历史数据分布 | alarm_hi 从 3.0 → 2.8 (减少误报) |
| 关联告警 | device.points[] | 时序相关性 | 油压↑+温度↑ → 泵故障预警 |
| 预测维护 | point.register | 振动+温度趋势 | 新增 vibration 测点 + RUL 模型 |
| 能效优化 | pump_status | 泵态+流量关联 | 多泵联动策略 (减少空转) |
| DTU 信号优化 | gateway.protocols | GPRS 丢包率 | 切换主备通道或降频上报 |
| 协议升级建议 | device.protocol | Modbus→OPC UA | 迁移路径 + 兼容性评估 |

### 流程

```
Auto-audit PASS
  ↓
AI scene analysis (read ontology + history)
  ↓
Generate upgrade plan:
  ├── 告警规则优化 (本体 rules[] 更新)
  ├── 预测维护模型 (新增 point + model)
  ├── 联动控制策略 (跨 device 规则)
  └── DTU/协议优化 (channel config)
  ↓
Human review + approve
  ↓
Deploy: 本体更新 → AI 重新生成 → pipeline test → 生产
```

### 实例：油井智能升级

```
本体: rtu_001 {oil_pressure, temperature, pump_status}
       alarm: T>75, P>3.0

AI 分析 TDengine 历史:
  - temperature 在 70-74°C 区间波动 23% 时间
  - oil_pressure 与 temperature 正相关 (r=0.82)
  - pump_status=0 后 temperature 5分钟内不降

升级方案:
  1. alarm: T>70 → L1 warning (早期预警)
  2. 新增关联规则: T>70 AND P>2.8 → pump_pre_failure
  3. 建议新增 vibration 测点 (Modbus 40310)
  
人工确认 → 本体更新 → AI 重新生成 gen_statem guard → 部署
```

```
AI generate ontology
  ↓
AI audit (7 checks)
  ↓
├── PASS → deploy to test
│           ↓
│         pipeline test (MQTT pub/sub → TDengine SELECT)
│           ↓
│         logs confirm → deploy to production
│
└── FAIL → issues reported
            ↓
          human reviews issues
            ↓
          fix ontology (not generated code!)
            ↓
          AI re-audit → PASS
```

## 127 IO Server 真实案例

```
输入: 2047 files from IO ServerOnLine/
AI 分析: 
  - 40+ DLL protocol drivers
  - 8 executables + 6 daemons
  - 16 DTU protocols
  - 42 Modbus registers detected
  - 5 DCS endpoints on DCOM

AI 生成:
  - thing_model (42 properties, with register mapping)
  - io_ontology.json (5 servers, 9 data_sources, 12 ports)
  - ProductTemplet: oil_well_rtu (42 points)
  - Device registration: 5 DCS + 206 RTU

AI 审核:
  - PORT_CONFLICT: 4 IOMan ports on same server → OK (expected, multi-instance)
  - DCOM_DENY: 5 DCS unreachable → mark as unavailable, suggest dcomcnfg
  - ORACLE_AUTH: 129:1521 password unknown → mark as blocked
  - 206 wireless terminals identified → auto-register as DTU devices

结果: 本体草案已生成，DCOM/密码问题需人工处理
```
