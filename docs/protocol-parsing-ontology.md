# 协议解析本体 — 帧→字段→值→物模型映射

## 解析管线

```
原始字节 → 帧切分 → 字段提取 → 类型解码 → 物模型映射 → gen_statem
```

## Modbus RTU

```
帧: [SlaveID:1B][FC:1B][Data:N][CRC16:2B]

解析规则 (readHoldingRegisters):
  请求: [01][03][40300_hex:2B][0002:count:2B][CRC16]
  响应: [01][03][04:bytecount][float32_AB×2:4B][CRC16]

解码器:
  uint16:      (H<<8)|L
  float32_AB:  struct.unpack('>f', bytes([A,B,C,D]))   # ABCD顺序
  float32_CD:  struct.unpack('>f', bytes([C,D,A,B]))   # CDAB顺序

字段映射:
  registers[0:2]  float32_AB  →  oil_pressure  (MPa)
  registers[2:4]  float32_AB  →  temperature  (°C)
  registers[4:5]  uint16      →  pump_status  (0/1)
```

## A11 (5a5a 行业私有协议)

```
帧头:  5a 5a
长度:  Len (2B, 不含帧头)
命令:  Cmd (2B)
数据:  Data (Len-2 B)
和校验: 8-bit sum (不含帧头)

设备路径解析:
  cmd=0x0101 (功图数据):
    偏移0:  DevicePath (变长, 以00结尾)  →  "DEV_A/M5/y9065..."
    偏移N:  Timestamp (4B, Unix)
    偏移N+4: PointCount (1B)
    偏移N+5: Points[] (每点8B: x[float32]+y[float32])

解码器:
  device_path:  bytes.decode('gbk').rstrip('\x00')
  point:        struct.unpack('<ff', bytes[0:8])  →  (位移, 载荷)
  electric:     struct.unpack('<HHHH', bytes[0:8])  →  (Ia,Ib,Ic,U)

字段映射:
  path → Device.devaddr  (设备寻址)
  points → 功图数据 (TDengine BLOB)
  Ia,Ib,Ic → 电机三相电流 (thing_model)
```

## OPC DA (DCOM)

```
DCE/RPC:
  EPM (端口映射器):  TCP :135 → OXID resolve → Port (动态)
  IOxfer:  IOPCServer::Read
  Automation Wrapper:  OPCDAAuto.dll

解析流程:
  1. DCOM协商:  bind + alter_context + auth
  2. OPC Enum:   IOPCServer::GetStatus → state
                 IOPCBrowse::Browse → item list
  3. OPC Read:   IOPCSyncIO::Read(items[]) → VARIANT[]
  4. VARIANT解码:  VT_R4→float, VT_BSTR→string, VT_BOOL→int

字段映射:
  RSLinx.Tag1  VT_R4    →  process_value  (float)
  WinCC.Level  VT_BSTR  →  tank_level     (string→float)
  PLC.Status   VT_BOOL  →  run_status     (0/1)
```

## DL/T 645-2007 (电表)

```
帧:  [68h][A0-A5:6B][68h][C:1B][L:1B][DATA:N][CS:1B][16h]

地址:  A0-A5 = BCD编码的表号 (12位十进制)
控制码: C = 11h(读数据), 14h(读后续)
长度:   L = DATA字节数
和校验: CS = 累加和 mod 256

数据标识 (DI0-DI3):
  DI0=00h, DI1=01h, DI2=00h, DI3=00h → 正向有功总电能

解码器:
  BCD:       (X>>4)*10 + (X&0x0F)
  XXX.X:     NNN.X 格式, 减 33h
  XXX.XX:    NNN.XX 格式, 减 33h再/100

字段映射:
  DI[00010000] → 正向有功总电能 (kWh, XXX.XX)
  DI[00020000] → 反向有功总电能 (kWh, XXX.XX)
  DI[02010100] → A相电压 (V, XXX.X)
```

## HJ/T 212-2017 (环保数采仪)

```
帧:  [##][LEN:4][ST:2][DATA:N][CRC:4]

ST=31: 实时数据, ST=32: 分钟数据, ST=33: 小时数据
DATA:  CP=&& + key=value;key=value;...&&

解析:
  1. strip '##' + CRC → raw data
  2. CP=&& → data section
  3. split by ';' → key=value pairs
  4. key like: 'MN'=设备编码, 'ST'=时间, '011-Rtd'=烟温, '011-Flag'=标记

解码器:
  字符串分割:  split(';') → dict
  设备编码:    14位字符串
  数据标记:    011-Rtd=实测值, 011-Flag=N=正常

字段映射:
  MN(14B)       → 设备编码 → Device.devaddr
  ST(14B)       → 采集时间
  011-Rtd       → 烟气温度 (°C)
  011-Flag      → 数据标记 (N=正常)
```

## 解析器生成规则

```
从协议本体自动生成解析器:

输入: protocol-ontology.json (协议描述)
输出: parser.py (解析器代码)

生成规则:
  1. 帧头匹配 → re.search(pattern, data)
  2. 字段提取 → struct.unpack(fmt, data[offset:offset+size])
  3. 类型转换 → decoder_map[encoding](raw_bytes)
  4. 映射 → thing_model[field_id] = value

示例 (Modbus):
  protocol-ontology:
    {"frame": {"header": null, "trailer": "CRC16"},
     "fields": [{"name": "oil_pressure", "offset": 3, "size": 4, "encoding": "float32_AB", "map_to": "oil_pressure"}]}

  生成:
    def parse_modbus(data):
        values = {}
        values['oil_pressure'] = struct.unpack('>f', data[3:7])[0]
        return values
```
