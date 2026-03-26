# 工位场景测试目录结构

## 概述

本目录按照工位场景组织测试脚本和日志，实现按工位分类的独立测试环境。

## 目录结构

每个工位应包含以下标准目录结构：

```
工位ID_工位名称/
├── scripts/              # 测试脚本
│   ├── station_XXXX_scenario.py   # Python测试场景
│   └── run_XXXX_test.sh           # Shell监控脚本
├── logs/                 # 日志文件
│   ├── erlang/          # Erlang过滤日志（按工位关键词过滤）
│   ├── python/          # Python脚本日志
│   └── monitor/         # 综合监控日志
├── records/             # 测试记录（JSON格式）
└── config/              # 工位配置文件
```

## 磁航向工位示例

已创建的磁航向工位（1700）作为示例：

- **目录**: `1700_magnetic/`
- **脚本**: 
  - `scripts/station_1700_magnetic_scenario.py` - 完整的3步测试场景
  - `scripts/run_magnetic_test.sh` - 监控脚本，同时收集Erlang和Python日志
- **日志分类**:
  - Erlang日志: `logs/erlang/erlang_station_1700_YYYYMMDD_HHMMSS.log` (按关键词过滤)
  - Python日志: `logs/python/station_1700_YYYYMMDD_HHMMSS.log`
  - 监控日志: `logs/monitor/magnetic_test_monitor_YYYYMMDD_HHMMSS.log`
- **测试记录**: `records/station_1700_test_record_YYYYMMDD_HHMMSS.json`

## 使用指南

### 1. 创建新工位目录

```bash
# 创建工位目录结构
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/stations
mkdir -p 1100_heartbeat/{scripts,logs/{erlang,python,monitor},records,config}
```

### 2. 复制和修改脚本

以磁航向工位脚本为模板，修改以下内容：

1. **Python脚本**:
   - 更新工位配置（IP、端口、设备信息）
   - 修改测试步骤
   - 更新日志目录路径

2. **Shell脚本**:
   - 更新工位ID和名称
   - 修改grep过滤关键词
   - 更新路径引用

### 3. 日志分类原理

- **Erlang日志**: 通过`grep`过滤系统Erlang日志，只保留与工位相关的消息
  - 过滤关键词: 工位ID、工位名称、相关模块名
  - 输出到工位专属的`logs/erlang/`目录

- **Python日志**: Python脚本直接输出到`logs/python/`目录
- **监控日志**: 综合Erlang和Python输出的完整日志，用于调试

### 4. 运行测试

```bash
# 进入工位目录
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/stations/1700_magnetic/scripts

# 方法1: 直接运行Python测试场景
python3 station_1700_magnetic_scenario.py

# 方法2: 使用监控脚本（推荐）
bash run_magnetic_test.sh
```

## 最佳实践

1. **工位命名**: 使用`工位ID_工位描述`格式，如`1100_heartbeat`、`1500_total_test`
2. **日志关键词**: 在Shell脚本的grep模式中包含工位ID、工位名称和相关业务关键词
3. **路径引用**: 使用相对路径`$SCRIPT_DIR/../`引用工位目录的各个子目录
4. **配置文件**: 将工位特定配置放在`config/`目录下，使用JSON或YAML格式

## 扩展建议

- 可以创建通用模板脚本`template_scenario.py`和`template_test.sh`
- 可以开发自动化工具自动创建工位目录结构
- 考虑将工位配置集中管理，支持动态加载

## 相关文件

- `base_station_scenario.py` - 工位测试场景基类
- `station_1500_total_test.py` - 总测工位测试脚本
- `../magnetic_station_test_scenario.py` - 旧的磁航向测试脚本（已迁移）

## 更新记录

- 2026-03-25: 创建磁航向工位（1700）标准目录结构，实现按工位分类的日志管理

## 一键式测试脚本

为了简化测试流程，磁航向工位提供了一键式测试启动脚本：

### 1. 高级一键式脚本 (`start_magnetic_test.sh`)

位于工位根目录 (`stations/1700_magnetic/`)，提供完整的环境检查和自动化功能：

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/stations/1700_magnetic

# 基本用法（仅检查环境并执行测试）
./start_magnetic_test.sh

# 绑定IP后执行测试（需要root权限）
sudo ./start_magnetic_test.sh --bind-ips

# 启动模拟器后执行测试
./start_magnetic_test.sh --start-simulators

# 显示帮助信息
./start_magnetic_test.sh --help
```

**功能特点**：
- ✅ 自动检查emqx运行状态
- ✅ 检查磁航向工位IP绑定状态（192.168.100.20/21/23）
- ✅ 检查PLC和治具模拟器运行状态
- ✅ 支持自动绑定IP和启动模拟器
- ✅ 交互式确认，支持非自动化环境
- ✅ 自动生成测试报告摘要

### 2. 快速启动脚本 (`run_station_1700.sh`)

位于模拟器目录 (`simulators/`)，提供快速访问：

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators

# 快速启动磁航向工位测试
./run_station_1700.sh

# 绑定IP后执行测试
sudo ./run_station_1700.sh --bind-ips

# 启动模拟器后执行测试
./run_station_1700.sh --start-simulators
```

### 3. 原监控脚本 (`run_magnetic_test.sh`)

位于工位脚本目录 (`scripts/`)，保持向后兼容：

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/stations/1700_magnetic/scripts

# 直接执行监控和测试
./run_magnetic_test.sh
```

### 环境要求检查表

在运行一键式脚本前，建议确保以下环境：

| 项目 | 状态 | 检查命令 |
|------|------|----------|
| emqx运行 | ✅ | `pgrep -f "emqx.*console"` |
| 磁航向PLC IP绑定 | ✅ | `ip addr show eth0 \| grep 192.168.100.20` |
| 磁航向DTU IP绑定 | ✅ | `ip addr show eth0 \| grep 192.168.100.21` |
| 扫码枪IP绑定 | ✅ | `ip addr show eth0 \| grep 192.168.100.23` |
| PLC模拟器运行 | ✅ | `pgrep -f "plc_simulator.py"` |
| 治具模拟器运行 | ✅ | `pgrep -f "fixture_simulator.py"` |

### 快速开始命令

```bash
# 完整的一键式测试（推荐）
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
sudo ./run_station_1700.sh --bind-ips --start-simulators
```