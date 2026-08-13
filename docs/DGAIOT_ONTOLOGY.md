# DGAIOT 架构本体

> 从 Erlang 源码 (`wsl.localhost/openEuler/root/gitee/dgaiot`) 阅读中提取的核心架构本体论。
> 作为 dgiot_lite (Python 轻量版) 的设计对齐参考。
>
> **最后更新: 2026-07-12 — 131 IO Server 2047 文件逐字精读完成**

---

## 九、131 IO Server 本体实例 — 2047 文件完整分析

> 基于 `D:\ai\dgiot_lite\io服务器分析\IO ServerOnLine\` 本地镜像的逐文件精读。
> 三级 Agent 并发分析：顶层配置 + IO Servers 子目录 + Data Servers/DTU/run 子目录。

### 9.1 全景架构图

```
┌─────────────────────────────────────────────────────────────────────┐
│                     IO ServerOnLine 完整架构                         │
│                      GENERIC_VENDOR ForceControl 7.x                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌─ 进程管理 ───────────────────────────────────────────────────┐  │
│  │ psNTService.exe (守护进程) → 6个服务自动重启/心跳监控         │  │
│  │ IOMan.exe (IO管理器) → 多线程消息驱动 (32088+ worker threads) │  │
│  │ IoMonitor.exe (采集核心) → CommitRealSpan=300ms, max 15K点/批 │  │
│  │ LegacyComm.exe (通信桥) → SYNCH×1 + LegacyComm×3 + TCP×1     │  │
│  │ IoCommit.exe (提交引擎) → 12个提交组 (DB0~DB11)              │  │
│  │ IOFileServer.exe (文件服务) → Port 7001                      │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  ┌─ 协议驱动层 (IO Servers) ────────────────────────────────────┐  │
│  │ IM_A11_RTU    → A11采油厂RTU采集 (功图模块)                  │  │
│  │ OPC_FC_Client → OPC DA Client (KEPware.KEPServerEx.V4)       │  │
│  │ FORCE_HLS_SIM → 仿真模拟器 (测试用)                           │  │
│  │ Standard_Umodbus → Modbus RTU/TCP (空/未启用)                 │  │
│  │ DTU/ (16种)   → 博海粤能/四信/宏电/映翰通...                │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  ┌─ 设备定义 (Device.ini) ──────────────────────────────────────┐  │
│  │ 12类保护继电器: DSL-31A/DST-31A/DBPA-31A/DSB-31A/DSL-24D...│  │
│  │ 每类含通道映射 + 事件/告警定义 + 计算公式                     │  │
│  │ 通用公式: I=Y*170/8192(A) U=Y*170/8192(V) P=Y*170*8.5√3/8192│  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  ┌─ 数据出口 (Data Servers) ────────────────────────────────────┐  │
│  │ Oracle @ 192.168.10.129:1521/orcl (INDUSTRYPROD/INDUSTRYA11_pass) ←主 │  │
│  │ RTDB @ 192.168.10.141:8889 (GENERIC_VENDOR实时库) ←辅助               │  │
│  │ eForceCon DB (空配置/未启用)                                  │  │
│  │ OPC Server (空配置/未启用)                                    │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  ┌─ 现场设备 (从 Event.txt + runBack1.zio 发现) ────────────────┐  │
│  │ ~20+ 井口RTU (Modbus TCP :502, IPv6 240C:8042:... )          │  │
│  │ 16 口油井 (WELL-03,WELL-07,WELL-06,WELL-04~WELL-05,WELL-08...)              │  │
│  │ 18 台仿真泵 (SJ0001~SJ0018, FORCE_HLS_SIM协议)              │  │
│  │ 966 口井 (Oracle SYS_SINGLE_WELL_BASE_INFO)                  │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  ┌─ 网络拓扑 ──────────────────────────────────────────────────┐  │
│  │ 192.168.10.131 (本机 IO Server)                                │  │
│  │   ├─ :502     Modbus TCP 接入 (20+ RTU)                      │  │
│  │   ├─ :135     DCOM (OPC DA)                                  │  │
│  │   ├─ :7001    IOFileServer                                   │  │
│  │   ├─ :6582    IOConfig Client                                │  │
│  │   └─ :6000/1  冗余心跳 (→ 192.168.10.102)                    │  │
│  │ 192.168.10.129 :1521 Oracle 数据库 (INDUSTRYPROD)                  │  │
│  │ 192.168.10.141 :8889 RTDB 实时库                             │  │
│  │ 192.168.10.102      冗余备机                                  │  │
│  └──────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
```

### 9.2 实体清单 (Step 1: 盘点到齐)

#### 9.2.1 可执行程序 (8 个)

| 文件 | 大小 | 版本 | 角色 |
|------|------|------|------|
| `IoMonitor.exe` | - | 6.0.0.1 | 主采集进程 (PID 18400, Session 2) |
| `IOMan.exe` | 299KB | 6.x | IO 管理器 (多线程消息驱动) |
| `IoCommit.exe` | 249KB | - | 数据提交引擎 (12组并发) |
| `LegacyComm.exe` | 155KB | - | 通信网桥 (TCP/串口) |
| `IoProject.exe` | - | - | 工程管理器 |
| `IoTest.exe` | - | - | 测试工具 |
| `IOFileServer.exe` | - | 6.0.0.1 | 文件服务 (Port 7001) |
| `A11SQLSERVICE.exe` | - | - | A11→Oracle SQL桥 |

#### 9.2.2 守护服务 (6 个, from psNTService.csv)

| 服务 | 版本 | 说明 | 心跳(s) | 自动启 |
|------|------|------|---------|--------|
| `RTDBServer64.exe` | 6.0.1.9 | RTDB 实时库 | 60 | ✅ |
| `IOFileServer.exe` | 6.0.0.1 | IO 文件服务 | 60 | ✅ |
| `IOMonitor.exe` | 6.0.0.1 | IO 采集 | 60 | ✅ |
| `CalcFileServer.exe` | 6.0.0.1 | 计算引擎文件服务 | 60 | ✅ |
| `CalcEngine.exe` | 6.0.0.1 | 计算引擎 | 60 | ✅ |
| `SyncTaskManager.exe` | 0.0.0.1 | 同步平台 | 30 | ✅ |

#### 9.2.3 协议驱动 DLL (按协议族分类, 40+ 个)

**OPC 协议 (6 DLL):**
- `OPCDAAuto.dll` (114KB) — OPC DA Automation 封装
- `OPCProxy.dll` (102KB) — DCOM proxy/stub
- `opccomn_ps.dll` (61KB) — OPC Common proxy/stub
- `OpcRcw.Da.dll` (20KB) — .NET DA 封装 (D:\Bin)
- `OpcRcw.Comn.dll` (6KB) — .NET Common (D:\Bin)
- `RTDBAPI.dll` (313KB) — RTDB 实时库 API

**Siemens PLC (2 DLL):**
- `s7onlinx.dll` (159KB) — Siemens S7 协议
- `W95_s7.dll` (95KB) — 旧版 S7

**Mitsubishi PLC (4 DLL):**
- `MruComDll.dll` (518KB) — 三菱协议
- `MyS3.dll` (24KB)
- `Komfort.dll` (59KB)
- `Setss3dll.dll` (40KB)

**Beckhoff (1 DLL):**
- `TcAdsDll.dll` (221KB) — TwinCAT ADS

**Omron (3 DLL):**
- `HCTPXYIF.DLL` (25KB)
- `HKCANDLL.dll` (36KB)
- `IMPDRVR.dll` (33KB)

**GE Fanuc (5 DLL):**
- `GEFSNP32.DLL` (94KB) — GE SNP
- `GEFSRX32.DLL` (41KB) — GE SRX
- `GEFTCP32.DLL` (38KB) — GE TCP
- `GEFEGD32.DLL` (91KB) — GE EGD
- `GEFCCL32.DLL` (64KB) — GE CCL

**DTU/GPRS (5 DLL):**
- `GPRSDLL.dll` (1.3MB) — GPRS 主库
- `gprs_dll.dll` (28KB)
- `AL_GPRS_ DLL.dll` (135KB)
- `CB_NetClient.dll` (320KB)
- `psNetClient.dll` (96KB)

**串行通信 (3 DLL):**
- `SCommDll.dll` (229KB)
- `SCommCom.dll` (28KB)
- `wcomm_dll.dll` (131KB)

**PCI/DAQ 采集卡 (17 DLL):**
`PCI8KA.dll` / `PCI8336A.dll` / `KPСI811.DLL` / `kpci800.dll` / `pci8402.dll` / `pci8327.dll` / `PCI-Dask.dll` / `PC6000.dll` / `PIODA.DLL` / `PIODIO.DLL` / `PISO725.DLL` / `PISO813.DLL` / `PISODIO.DLL` / `P16r16.dll` / `TMC12.DLL` / `USBID.DLL` / `Usb7kC.dll`

**GENERIC_VENDOR核心 IO (11 DLL):**
`ioapi.dll` (106KB) / `iodevman.dll` (299KB) / `IoDevCfg.dll` (41KB) / `IoDevUi.dll` (28KB) / `Ioitemui.dll` (28KB) / `iomem.dll` (14KB) / `IOconfigProject.dll` (176KB) / `IODbCtrl.dll` (36KB) / `ServiceAPI.dll` (38KB) / `FileAPI.dll` (143KB) / `ProjectCMP.dll` (104KB)

**基础库 (19 DLL):**
`libcrypto.dll` (2.3MB) / `libssl.dll` (502KB) / `libxml2.dll` (1MB) / `liblz4.dll` (82KB) / `HPSocket.dll` (1.7MB) / `mosquitto.dll` (38KB) / `uastack.dll` (919KB) / `ACE5.6.9_VC9.dll` (1MB) / `AnyComm.dll` (413KB) / `pthreadVC2.dll` (117KB) ...

#### 9.2.4 DTU 协议插件 (16 种, from DTU/)

| 目录 | 品牌 | 中文名 |
|------|------|--------|
| `DTU_BHYN` | 博海粤能 | Bohai YueNeng |
| `DTU_CAIMAO` | 莱司凯茂 | Laisecaimao |
| `DTU_DATA6211` | 唐山平升 Data6211 | Tangshan Pingsheng |
| `DTU_DATA86` | 唐山平升 Data6100 | Tangshan Pingsheng |
| `DTU_DLHB_HJT212` | 动力环保 HJT212 | HJ/T212 国标 |
| `DTU_DQQY` | GENERIC_VENDOR | (redacted) |
| `DTU_ETUNG` | 亿通 | Etung |
| `DTU_FENGSHI` | 山东锋士 | Shandong Fengshi |
| `DTU_FOUR_FAITH` | 四信 | Four Faith |
| `DTU_HONGDIAN` | 宏电 | Hongdian |
| `DTU_InHand` | 映翰通 | InHand Networks |
| `DTU_LANDI` | 唐山蓝迪 | Tangshan Landi |
| `DTU_SUNWAY` | 三维GENERIC_VENDOR (动态IP) | Sunway Dynamic IP |
| `DTU_SUNWAY_COMMSERVER` | 通用TCP Server | Common Server |
| `DTU_SUNWAY_MULTIPORT` | TCP多端口 | Multiport TCP |
| `DTU_SUNWAY_UDP` | 通用UDP | UDP Protocol |

#### 9.2.5 保护继电器类型 (12 类, from Device.ini)

| 代码 | 型号 | 中文名 | 通道数 |
|------|------|--------|--------|
| 00 | DSL-31A | 线路保护 | 20 |
| 10 | DST-31A | 变压器差动保护 | 15 |
| 20 | DBPA-31A | 电源备投 | 13 |
| 30 | DSB-31A | 母联保护 | 20 |
| 40 | - | 电动机保护 | 19 |
| 50 | DST-22D | 变压器差动保护 | 15 |
| 60 | DSB-22D | 变压器后备保护 | 20 |
| 70 | DSL-24D | 线路保护 | 20 |
| 80 | DGP-11 | 电容器差动保护 | 21 |
| 90 | DGP-12 | 电容器后备保护 | 24 |
| 100 | DGP-13 | 电容器接地保护 | 22 |
| 110 | DMP-31A/DST-31A | 电动机保护 | 19 |

**通用遥测公式 (Device.ini):**
| 遥测量 | 公式 | 单位 |
|--------|------|------|
| Ia/Ib/Ic (电流) | Y × 170 / 8192 | A |
| Iac/Ibc/Icc/3I0 | Y × 8.5 / 8192 | A |
| Ua/Ub/Uc/Uab/Ubc/Uca/UX/3U0/U2 | Y × 170 / 8192 | V |
| P/Q (功率) | Y × 170 × 8.5 × √3 / 8192 | W/VAR |
| cosφ | Y / 8192 | - |
| F (频率) | 50 + Y × 2 / 8192 | Hz |

#### 9.2.6 数据库表 (from Oracle INDUSTRYPROD)

| 表名 | 行数 | 说明 |
|------|------|------|
| `PC_FD_PUMPJACK_FDYNA_DIA_T` | 4,814,742 | 抽油机功图诊断 (最大表) |
| `SYS_DEVICE_RUN_DETAILS_HIST` | 233,269 | 设备运行详情历史 (INSERT_TIME + TODAY_RUN_RATE) |
| `SYS_SINGLE_WELL_BASE_INFO` | 966 | 单井基础信息 (9 字段) |
| `SYS_POINTRELATION_WELL` | 4,567 | 测点-井关系 |
| `TOURWELL_RECORD_DETAIL` | 2,134 | 巡井记录 |
| `ALARM_HISTORY_DUMP` | 0 | 告警历史 (空) |
| `SYS_USER` | 4 | 系统用户 |

#### 9.2.7 数据提交组 (12 组, from run/TagID_IOCommitDB*.dat)

| 组 | DEVICE_C 大小 | DEVICE_D 大小 | pPluse 大小 | RTDB |
|----|------------|------------|------------|--------|
| DB0 | 102KB | 132KB | 123KB | 542B |
| DB1 | 103KB | 150KB | 159KB | - |
| DB2 | 153KB | 96KB | 156KB | - |
| DB3 | 134KB | 385KB | 162KB | - |
| DB4 | 148KB | 242KB | 145KB | - |
| DB5 | 123KB | 319KB | 188KB | - |
| DB6 | 115KB | 231KB | 143KB | - |
| DB7 | 119KB | - | 94KB | - |
| DB8 | 185KB | - | 108KB | - |
| DB9 | 137KB | - | 69KB | - |
| DB10 | 116KB | - | - | - |
| DB11 | 67KB | - | - | - |

**测点路径格式:** `/DEVICE_D/{WellID}/{DeviceCode}{WellID}{MeasurementType}`
- 示例: `/DEVICE_D/B1V354V611/JD010804B1V354V611GYS`
- 后缀含义: TGP(套压)/ZWG(总无功)/ZYGX(总有功)/ZHL(总回流)/DWL(低回流)/RCV(热采阀)...

### 9.3 关系矩阵 (Step 2: 连线成网)

#### 9.3.1 数据流关系

```
现场设备 (RTU/PLC/继电器)
   │
   ├──[Modbus TCP :502]──→ LegacyComm.exe ──→ IoMonitor.exe
   ├──[OPC DA DCOM :135]──→ OPC_FC_Client/ioapi.dll ──→ IoMonitor.exe
   ├──[S7 Protocol]──→ s7onlinx.dll ──→ IOMan.exe
   ├──[DTU GPRS]──→ DTU_*/DTUAPI.dll ──→ IoMonitor.exe
   │
   ▼
IoMonitor.exe (主采集引擎, CommitRealSpan=300ms)
   │
   ├──→ IoCommit.exe (12 提交组, 每批 15K 点)
   │      │
   │      ├──[ADO/OLEDB]──→ Oracle 192.168.10.129:1521/orcl (INDUSTRYPROD)
   │      │     └── A11SQLSERVICE.exe ──→ F:\TRANgo\...\RTUSql
   │      │
   │      ├──[RTDB API]──→ RTDB 192.168.10.141:8889
   │      │
   │      └──[Redundancy]──→ 192.168.10.102:6000/6001
   │
   ├──[FileServer :7001]──→ IOFileServer.exe ──→ IOConfig clients (:6582)
   │
   └──[CalcEngine]──→ CalcEngine.exe ──→ CalcFileServer.exe
```

#### 9.3.2 IO Servers 关系 (IODESC 标准协议)

每个 IO Server 子目录包含标准的 `IODESC.TXT`:
```
{协议};{描述};{可执行文件}
{版本};{flag};{flag};{flag}
```

| Server | 协议 | 描述 | EXE |
|--------|------|------|-----|
| OPC_FC_Client | OPC | Microsoft OPC Client | OPC_FC_Client.exe |
| IM_A11_RTU | RTU采集 | 功图模块;A11项目 | IM_A11_RTU |
| FORCE_HLS_SIM | Simulator(仿真) | 仿真器;力仿真 | FORCE_HLS_SIM.exe |

#### 9.3.3 二进制数据结构 (OPC_FC_Client)

**DeviceStruct.txt — OPC 设备记录 (256 字节, 22 字段):**
```
0:   32B string → 服务节点 IP
32:  80B string → 服务名称
112: 16B string → OPC 组名称
128: 32B string → 服务冗余节点 IP
160: 4B  int   → 服务版本
164: 4B  uint  → 刷新时间
172: 4B  int   → 读写方式
176: 4B  int   → 重连时间
180: 16B struct → CLSID
196: 16B struct → CATID
212: 4B  int   → 冗余方式
216: 4B  long  → 是否单点注册
220: 4B  long  → Ping 检测
228: 4B  long  → 异步刷新
232: 4B  long  → 字符编码
```

**DefinedStruct.txt — OPC 点记录 (96 字节, 17 字段):**
```
0:   80B string → 数据内容 (OPC Item ID)
80:  1B  byte   → 数据类型
81:  1B  byte   → 转换系数
82:  1B  byte   → 转换偏移
83:  1B  byte   → 读写属性
86:  1B  byte   → 字符串长度选择
88:  1B  byte   → 数组大小
90:  1B  byte   → 公式运算选择
```

#### 9.3.4 冗余关系

```
IO-SERVER-01 (主) ←→ 192.168.10.102 (备)
  Port 6000 (收)          Port 6000
  Port 6001 (发)          Port 6001
  心跳 1500ms × 3次超时 = 4.5s 判定离线
```

### 9.4 约束规则 (Step 3: 设卡立规)

#### 9.4.1 采集约束 (IoMonitor.ini)

| 约束 | 值 | 说明 |
|------|-----|------|
| CommitRealSpan | 300ms | 实时数据提交间隔 |
| CommitHisSpan | 500ms | 历史数据提交间隔 |
| CommitTagOnce | 15000 点 | 单批最大提交量 |
| CommitTagCount | 1000000 | 总标签容量 |
| DataDelay | 5ms | IO 提交超时 |
| MaxTagValueCount | 100000 | 缓存刷新阈值 |
| MaxIOCount | 10 | 最大 IOMan 连接数 |

#### 9.4.2 通道约束 (IoChannelCfg.ini)

| 约束 | 值 | 说明 |
|------|-----|------|
| SYNCH 模式 | 1 设备/进程 | 同步模式 |
| LegacyComm 模式 | 3 设备/进程 | 网桥模式 |
| TCP 模式 | 1 通道 | TCP 客户端 |
| IO 超时 | 30s | 设备无响应判定 |
| 通道打开间隔 | 10s | 防止冲击 |

#### 9.4.3 数据库约束 (SqlFilSet.ini)

| 约束 | 值 | 说明 |
|------|-----|------|
| ADOCOUNT | 4 | Oracle 并发连接数 |
| EXECUTECYC | 1000ms | SQL 执行周期 |
| TASKALLOCATIONCYC | 5 | 任务分配周期 |

#### 9.4.4 设备告警约束 (Device.ini 示例)

**DSL-31A 线路保护 (25 告警项):**
CPU故障 / 采样故障 / RAM故障 / EEPROM故障 / 电源故障 / A/D故障 / I/O接口故障 / 定值校验 / 配置参数 / 模拟量系数 / CAN总线故障 / 变频异常 / 控制回路断线 / 跳闸不成功 / 合闸不成功 / 小电流接地告警 / 过负荷告警 / 智能重合闸告警 / PT断线...

**DMP-31A 电动机保护 (32 告警项):**
额外包含: 过流告警 / 堵转告警 / 过载告警 / 轴承告警 / 低电压 / 过电压 / CT断线 / 温度故障...

#### 9.4.5 A11 RTU 约束 (Time.ini / Timer.ini)

| 约束 | 值 | 说明 |
|------|-----|------|
| TimeSyn | 1 | 时间同步开启 |
| ReadHisDiag | 0 天 | 历史功图读取窗口 |
| StatusTime | 30 分钟 | 设备在线判定周期 |
| EXPECTTIME | 0 | 期望功图时间 |
| JUDGETIME | 30 分钟 | 在线判定间隔 |

#### 9.4.6 冗余约束 (RedunndancyCfg.ini)

| 约束 | 值 | 说明 |
|------|-----|------|
| TimeOutCyc | 1500ms | 心跳超时周期 |
| TimeOutTimes | 3 | 连续超时次数 |
| 故障转移 | 4.5s | 总故障判定时间 |

### 9.5 闭环验证 (Step 4)

#### 9.5.1 活性证据

```
✅ IoMonitor.exe PID 18400, Session 2, 运行中 (2026-07-10 以来)
✅ IoMonitor → Oracle 192.168.10.129:1521 ESTABLISHED
✅ 131 → 192.168.10.130:8889 ×7 ESTABLISHED (A11 RTU)
✅ 131 → 20+ 192.168.20.x:53001 ESTABLISHED (Modbus RTU)
✅ 运行率 99.31% @ 2026-07-11 23:50
✅ Oracle PC_FD_PUMPJACK_FDYNA_DIA_T: 4,814,742 行
✅ RTDB 服务心跳正常
```

#### 9.5.2 已知故障

```
⚠️  IoCommit.exe 崩溃 10 次 (2022-01 ~ 2023-01)
    - 6 次: 内存损坏 (8B 82 F8 19 00 00...)
    - 4 次: NULL 指针解引用
    - 1 次: 写越界

⚠️  LegacyComm.exe 频繁崩溃 (2023-03 ~ 2026-07)
    - 11+ 次异常退出 (dmp 文件 0 字节)

⚠️  IOMan.exe 大量崩溃 (2026-03-27)
    - 20+ 次崩溃 (IoMan20260327*.dmp)

⚠️  数据存储失败 (IOSaveErr/)
    - 大量 CommitErr: "存储失败" (epoch=1970 未初始化数据)
    - "时间太过" (时间戳超前)
    - DEVICE_C + pPluse 都有影响

⚠️  Modbus TCP 连接不稳定 (Event.txt)
    - 2021-12-25: 多次 "Connecting TCP/IP node...failed"
```

### 9.6 本体 API 使用

```python
from src.ontology import build_131_ontology, OntologyEngine

engine = build_131_ontology()

# 完整 5 层本体树
tree = engine.tree("industry_c1")

# 实时运行率 (via Oracle Bridge)
from src.storage.oracle_bridge import get_bridge
b = get_bridge()
rate = b.get_run_rate()   # → 99.31% @ 23:50

# 测点路径解析
b.parse_point_path("/DEVICE_D/B1V354V611/JD010804B1V354V611GYS")
# → {site: DEVICE_D, well: B1V354V611, station: JD010804, point_code: GYS}

# 本体完整性校验
engine.validate()
# → {valid: true, counts: {sites:1, gateways:1, channels:6, devices:14, points:6, constraints:6, datasources:3}}
```

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
│ 192.168. │ dgiot/oil_field_01/ │ State: online        │
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
| TDengine 连接 (192.168.10.167:6041) | ✅ |
| 厂商通道插件 (6个 VENDOR_CHANNELS) | ✅ |
| youyeyun.py 协议适配器 | ✅ |
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
| LegacyComm.exe | 进程 | 通信网桥 | ⚠️ 频繁崩溃 |
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
| IoChannelCfg.ini | 通道配置 | SYNCH×1, LegacyComm×3, TCP×1 |
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
  192.168.20.x             .9.23 .18.194            192.168.20.x
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
| gateway → opc_server | 4 | 192.168.10.23/.3/.18.194/.26.6.3 |

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
131 → 20+ 192.168.20.x:53001 ESTABLISHED                  ✅ Modbus连通
131 → 192.168.10.23:135 ESTABLISHED                       ✅ OPC DA连通
```

### 五层本体结构总览

```
Site:    industry_c1 (示例场站)
  └─ Gateway: gw_131 (IO-SERVER-01, 192.168.10.131)
       ├─ Channel: ch_opc_da       → OPC DA Client → 4 OPC Servers
       ├─ Channel: ch_a11_rtu      → A11 采油厂协议 → 130:8889
       ├─ Channel: ch_modbus_tcp   → Modbus → 20+ RTU
       ├─ Channel: ch_oracle       → Oracle 数据出口 → 129:1521
       ├─ Channel: ch_pspace       → RTDB (已停)
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
