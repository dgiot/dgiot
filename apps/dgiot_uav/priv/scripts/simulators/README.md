# 无人机测试产线模拟器

## 目录结构

```
simulators/
├── stations/        工位模拟器（一个工位一个文件）
├── devices/         设备模拟器（PLC、治具、无人机、MES）
├── core/            核心模块（多播、设备基类）
├── tools/           工具脚本（报告生成、环境检查）
├── docs/            文档（核心文档 + 历史归档）
└── logs/            日志输出
```

## 快速开始

### 磁航向工位闭环测试（推荐）

```bash
# 最完整的单工位测试脚本
python3 stations/station_1700_magnetic.py
```

**功能包含**:
- ✅ PLC Server (Modbus TCP)
- ✅ 地测口 Client (EB90协议)
- ✅ 扫码枪 Client
- ✅ MES Server (HTTP API)
- ✅ 自动化测试流程

### 全产线测试

```bash
# 多工位协同测试
python3 stations/production_line.py
```

### 单设备模拟

```bash
# PLC模拟器
python3 devices/plc_simulator.py

# 治具模拟器
python3 devices/fixture_simulator.py

# 无人机模拟器
python3 devices/uav_simulator.py

# MES模拟器
python3 devices/mes_simulator.py
```

## 工位脚本说明

| 工位 | 脚本 | 功能 |
|------|------|------|
| **磁航向 (1700)** | station_1700_magnetic.py | PLC+地测口+扫码枪+MES<br>完整闭环测试 |
| **产线整合** | production_line.py | 多工位协同测试 |
| **一键测试** | one_click_production_test.py | 快速验证测试 |

## 设备脚本说明

| 设备 | 脚本 | 端口 | 说明 |
|------|------|------|------|
| PLC | plc_simulator.py | 502 | Modbus TCP Server |
| 治具 | fixture_simulator.py | 20000 | 舵面+单片机 |
| 无人机 | uav_simulator.py | 226.0.0.80:8001/8002 | 多播EB90协议 |
| MES | mes_simulator.py | 801 | HTTP API Server |

## 文档

- **[工作流程指南](docs/WORKFLOW_GUIDE.md)** - 完整工作流程和磁航向工位详解
- **[快速开始](docs/QUICK_START.md)** - 快速上手指南
- **[快速参考](docs/QUICK_REFERENCE.md)** - 常用命令速查
- **[整理说明](docs/README_REORGANIZED.md)** - 目录整理方案

## 历史文档

所有历史文档已归档到 `docs/archive/` 目录，包括：
- 编译修复记录
- 测试报告
- 配置文档
- 分析文档

## 开发指南

### 新增工位脚本

```python
# 1. 在stations/目录创建脚本
touch stations/station_XXXX.py

# 2. 参考station_1700_magnetic.py编写

# 3. 实现必要接口
- init()          # 初始化设备
- start()         # 启动服务
- stop()          # 停止服务
- test()          # 测试流程
```

### 新增设备脚本

```python
# 1. 在devices/目录创建脚本
touch devices/new_device.py

# 2. 参考现有设备脚本

# 3. 实现设备协议
```

## 故障排除

```bash
# 查看日志
tail -f logs/test.log

# 归档位置
ls docs/archive/
```

---

**整理原则**: 一个工位一个文件，设备独立，文档归档

**最后更新**: 2026-03-26
