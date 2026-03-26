# 磁航向工位一体化调测系统

## 📋 项目概述

磁航向工位一体化调测系统是基于磁航向工位调测智能体和测试步骤文档开发的完整调测工具，实现了从环境准备到结果验证的全流程自动化。

## 🎯 核心特性

✅ **环境自动化检查**
- IP绑定状态检查
- DG-IoT服务器状态检查
- 端口监听状态检查

✅ **一键IP绑定**
- 自动绑定磁航向工位IP
- 支持手动指定设备ID
- 自动检测网络接口

✅ **完整测试流程**
- 扫码绑定设备
- PLC七步校验流程
- EB90遥控指令下发
- 遥测数据持续发送
- 测试结果汇聚

✅ **报文日志系统**
- 完整的报文记录
- TX/RX方向标识
- 时间戳和描述

✅ **结果验证**
- 测试结果统计
- 通过率计算
- DG-IoT状态验证
- MES数据上报

## 📁 文件结构

```
apps/dgiot_uav/priv/scripts/simulators/stations/
├── station_1700_magnetic.py                # 原始测试脚本（完整版）
├── station_1700_magnetic_enhanced.py        # 增强版测试脚本（新增）⭐
├── run_magnetic_station_test.sh            # 一键调测脚本（新增）⭐
├── MAGNETIC_STATION_TEST_GUIDE.md          # 调测指南（新增）⭐
└── README_MAGNETIC_STATION.md             # 本文档（新增）⭐

.codebuddy/agents/
├── 磁航向工位调测智能体.md                   # 智能体文档
├── 磁航向工位智能体.md
└── 磁航向工位快速参考.md

根目录/
├── MAGNETIC_STATION_TEST_STEPS.md          # 测试步骤详解
└── MAGNETIC_STATION_AGENT_SUMMARY.md       # 智能体总结
```

## 🚀 快速开始

### 1. 一键调测（推荐）

```bash
# 基本用法
./apps/dgiot_uav/priv/scripts/simulators/stations/run_magnetic_station_test.sh

# 自动绑定IP并测试
./run_magnetic_station_test.sh --auto-bind

# 指定设备ID
./run_magnetic_station_test.sh --device-id UAV-002

# 详细日志
./run_magnetic_station_test.sh --verbose
```

### 2. Python脚本方式

```bash
# 基本用法
python3 station_1700_magnetic_enhanced.py

# 自动绑定IP
python3 station_1700_magnetic_enhanced.py --auto-bind

# 详细日志
python3 station_1700_magnetic_enhanced.py --verbose
```

## 📊 测试流程

### 完整测试步骤

```
┌─────────────────────────────────────────────────────────────┐
│                     磁航向工位测试流程                        │
└─────────────────────────────────────────────────────────────┘
                              ↓
        ┌───────────────────────────────────────┐
        │        阶段一：环境准备                │
        │  1. 启动DG-IoT服务器                   │
        │  2. 检查IP绑定状态                     │
        │  3. 自动绑定IP（如需要）               │
        │  4. 检查端口监听状态                   │
        └───────────────────────────────────────┘
                              ↓
        ┌───────────────────────────────────────┐
        │        阶段二：测试执行                │
        │  场景1: 扫码绑定设备                   │
        │  场景2: PLC七步校验流程                │
        │  场景3: 无人机指令下发                 │
        │  场景4: 持续发送遥测数据               │
        │  场景5: 测试结果汇聚                   │
        └───────────────────────────────────────┘
                              ↓
        ┌───────────────────────────────────────┐
        │        阶段三：结果验证                │
        │  1. 查看测试日志                       │
        │  2. 查看报文日志                       │
        │  3. 验证DG-IoT设备状态                 │
        │  4. 生成测试报告                       │
        └───────────────────────────────────────┘
```

### PLC七步校验流程

```
┌─────────────────────────────────────────────────────────────┐
│                   PLC七步校验流程                            │
└─────────────────────────────────────────────────────────────┘

  Step 1/7: READ  D1700    → 读取工位就绪状态
     ↓
  Step 2/7: WRITE D1751    → 写入测试命令码 (100)
     ↓
  Step 3/7: READ  D1710    → 读取测试确认状态
     ↓
  Step 4/7: WRITE D1700    → 复位工位状态 (0)
     ↓
  Step 5/7: WRITE D1710    → 清除测试确认 (0)
     ↓
  Step 6/7: WRITE D1760    → 写入完成确认码 (100)
     ↓
  Step 7/7: WRITE D1761    → 触发完成信号 (1)
```

## 🔧 配置说明

### 工位配置

| 配置项 | 值 | 说明 |
|--------|-----|------|
| 工位ID | 1700 | 磁航向工位标识 |
| 工位名称 | 磁航向校准工位 | 工位名称 |
| 业务类型 | 扫码绑定 | 主要业务 |

##***REMOVED***配置

| 设备 | IP地址 | 端口 | 说明 |
|------|--------|------|------|
| PLC | 192.168.100.20 | 502 | Modbus TCP服务器 |
| 地测口 | 192.168.100.21 | 10007 | DG-IoT客户端 |
| 扫码枪 | 192.168.100.23 | 1234 | DG-IoT客户端 |
| DG-IoT | 192.168.100.100 | 20000 | DG-IoT服务器 |

## 📝 测试结果

### 成功标准

- ✅ 扫码绑定成功
- ✅ PLC七步校验成功
- ✅ 所有遥控指令发送成功
- ✅ 遥测数据发送成功（3次）
- ✅ 测试通过率达到100%

### 测试报告

```json
{
  "device_id": "UAV-001",
  "station_id": 1700,
  "total_tests": 6,
  "passed": 6,
  "failed": 0,
  "skipped": 0,
  "pass_rate": "100.0%",
  "timestamp": "2026-03-26T02:33:45.123456"
}
```

## 📚 文档导航

### 核心文档

1. **调测指南** - `MAGNETIC_STATION_TEST_GUIDE.md`
   - 完整的使用说明
   - 配置参数详解
   - 问题排查指南

2. **智能体文档** - `.codebuddy/agents/磁航向工位调测智能体.md`
   - 智能体概述
   - API接口说明
   - 故障排除方法

3. **测试步骤详解** - `MAGNETIC_STATION_TEST_STEPS.md`
   - 详细测试步骤
   - 每个步骤的验证标准
   - 报文格式说明

### 辅助文档

4. **智能体总结** - `MAGNETIC_STATION_AGENT_SUMMARY.md`
   - 文档体系结构
   - 核心功能模块
   - 使用建议

5. **快速参考** - `.codebuddy/agents/磁航向工位快速参考.md`
   - 核心信息速查表
   - 常用命令
   - 故障排除速查

## 🔍 问题排查

### 常见问题

1. **DG-IoT服务器未运行**
   ```bash
   cd /root/gitee/dgiot
   make run
   ```

2. **IP地址未绑定**
   ```bash
   # 自动绑定
   ./run_magnetic_station_test.sh --auto-bind
   
   # 手动绑定
   sudo ip addr add 192.168.100.20/24 dev eth0
   ```

3. **端口被占用**
   ```bash
   sudo lsof -i :1801
   sudo kill -9 <PID>
   ```

更多问题排查请参考 `MAGNETIC_STATION_TEST_GUIDE.md`。

## 🛠️ 开发说明

### 技术栈

- **Python 3.x** - 测试脚本开发
- **Bash Shell** - 自动化脚本
- **Socket** - TCP通信
- **Modbus TCP** - PLC通信
- **EB90协议** - 无人机协议

### 代码结构

```python
# 核心类
EnvironmentChecker    # 环境检查器
PacketLogger          # 报文日志记录器
GroundStationClient    # 地测口客户端
ScannerClient         # 扫码枪客户端
EB90CommandSender     # EB90指令发送器
PLCSevenStepValidator # PLC七步校验器
TestResultAggregator  # 测试结果聚合器
MagneticStationTest   # 主测试类
```

### 扩展开发

如需添加新功能，请参考以下步骤：

1. 在 `station_1700_magnetic_enhanced.py` 中添加新类
2. 实现相应的接口方法
3. 在主测试类中集成新功能
4. 更新文档和测试用例

## 📞 技术支持

如有问题或建议，请联系：

- **文档**: 参考相关文档
- **日志**: 查看 `test_records/station_1700/` 目录
- **DG-IoT日志**: 查看 `_build/emqx/rel/emqx/log/console.log`
- **技术支持**: DG-IoT开发团队

## 📄 许可证

本项目遵循DG-IoT开源许可证。

## 🔄 更新日志

### v1.0 (2026-03-26)

- ✅ 初始版本发布
- ✅ 实现完整测试流程
- ✅ 添加IP绑定自动化
- ✅ 集成报文日志系统
- ✅ 创建完整文档体系

## 🙏 致谢

感谢磁航向工位调测智能体和测试步骤文档的贡献者！

---

**项目**: DG-IoT  
**版本**: v1.0  
**日期**: 2026-03-26  
**维护者**: DG-IoT Team
