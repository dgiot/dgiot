# 工业协议本体 — DLAS 分类

> RQ: 工业物联网协议的 DLAS 本体结构是什么？

## 协议分类

```
按物理层:
  有线串行:  Modbus RTU, DL/T 645, CJ/T 188
  有线TCP:   Modbus TCP, OPC UA, Siemens S7, BACnet/IP
  无线:      GPRS/CDMA DTU, MQTT

按行业:
  通用:      Modbus, OPC, MQTT
  油田:      A11(中石油私有)
  电力:      DL/T 645-2007
  水务:      CJ/T 188
  环保:      HJ/T 212-2017
  消防:      GB/T 26875
  楼宇:      BACnet/IP
  制造:      S7, FANUC, Mitsubishi
```

## DLAS 四层协议本体

### Data 层 — 协议数据单元

| 协议 | 帧格式 | 寻址方式 | 数据类型 | 典型负载 |
|------|--------|---------|---------|---------|
| Modbus RTU | 8N1, CRC16 | SlaveID(1B)+Addr(2B) | uint16, float32_AB/CD | 125 registers |
| Modbus TCP | MBAP(7B)+PDU | UnitID(1B)+Addr(2B) | uint16, float32 | 同上 |
| A11 | 5a5a + Len + Cmd | DeviceID | custom struct | 功图·电参 |
| OPC UA | TPKT + SecureChannel | NodeId | Variant | 订阅/读写 |
| S7 | TPKT + COTP + Header | DB.Offset | S7-Any | 240 bytes |
| DL/T 645 | 68h + A0-A5 + 68h | 表号 (BCD 6B) | BCD, XXX.X | DI0-DI3 |
| HJ/T 212 | ## + Len + ST | MN(14B) + CN(4B) | CP=&& || 字符串 | 实时/分钟/小时 |
| MQTT | Fixed(2B) + Variable | Topic String | Raw bytes | JSON/Binary |

### Logic 层 — 协议感知规则

```
Modbus:
  readHoldingRegisters(addr, count) → float32_AB 解码 → 阈值比较
  规则: polling_interval=20s, timeout=5s, retry=3

A11:
  5a5a 帧解析 → 功图数据 → 功图诊断模型
  规则: 功图面积<阈值 → 泵效低告警

OPC UA:
  Subscription(interval=100ms) → DataChange → 规则评估
  规则: Deadband filtering, 变化>1%才触发

HJ/T 212:
  CP=&& → 解析key=value → 超标判断
  规则: 排放浓度>标准 → L1告警→L2上报环保局
```

### Action 层 — 协议触发的动作

```
Modbus:
  read → Shadow gen_statem evaluate → state transition
  write → Modbus FC6/FC16 → 设备控制

A11:
  parse frame → push_point → TDengine INSERT
  firmware → $dg/thing/{PID}/{DevAddr}/firmware/report

OPC UA:
  DataChange → gen_statem cast → bridge
  Call Method → device command

MQTT (dlink):
  publish → $dg/thing/{PID}/{DevAddr}/properties/report
  subscribe → $dg/device/{PID}/{DevAddr}/profile ← 配置下发
```

### Security 层 — 协议安全特性

| 协议 | 认证 | 加密 | 完整性 | 审计 |
|------|------|------|--------|------|
| Modbus RTU | 无 | 无 | CRC16 | 无 |
| Modbus TCP | 无 | 无 | TCP checksum | 无 |
| OPC UA | X.509, JWT | AES-256 | Sign & Encrypt | Audit Event |
| S7 | 无 | 无 | TPKT | 无 |
| A11 | DeviceID | 无 | 无 | 无 |
| MQTT | User/Pwd, X.509 | TLS | TLS | EMQX Audit |
| DL/T 645 | 密码 (4级) | 无 | 累加和 | 操作记录 |
| HJ/T 212 | PW(6B) | 无 | CRC | 日志要求 |

## 协议接入模式 (from dgiot_dlink)

```
CONNECT    主动连接设备     Modbus TCP, OPC UA
LISTEN     监听端口        Modbus RTU server
BRIDGE     协议转发        MQTT↔Modbus
AGENT      安装代理        边缘采集器
SERIAL     串口直连        DL/T 645, CJ/T 188
POLL       轮询采集        Modbus RTU
SUBSCRIBE  订阅模式        OPC UA, MQTT
CUSTOM     自定义协议      A11 (5a5a)
DTU        无线数传        GPRS/CDMA DTU
```

## 本体注册模板 (per protocol)

```json
{
  "protocol": "modbus",
  "layer": {
    "data": {
      "frame": "MBAP(7B)+PDU",
      "addressing": "UnitID(1B)+RegAddr(2B)",
      "encoding": "uint16, float32_AB, float32_CD",
      "max_payload": "125 registers"
    },
    "logic": {
      "poll_interval": 20,
      "timeout": 5,
      "retry": 3,
      "decoders": ["uint16", "float32_AB", "float32_CD"]
    },
    "action": {
      "read": "FC3/FC4",
      "write": "FC6/FC16",
      "topic": "$dg/thing/{PID}/{DevAddr}/properties/report"
    },
    "security": {
      "auth": "none (gateway-level: ProductSecret)",
      "integrity": "CRC16 + TCP checksum"
    }
  }
}
```
