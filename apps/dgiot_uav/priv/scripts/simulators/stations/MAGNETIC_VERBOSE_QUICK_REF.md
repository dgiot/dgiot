# 磁航向工位详细日志系统 - 快速参考

## 🚀 快速开始

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/stations

# 一键运行（推荐）
./run_magnetic_verbose.sh

# 自动绑定IP并测试
./run_magnetic_verbose.sh --auto-bind

# 仅检查环境
./run_magnetic_verbose.sh --check-only

# 查看日志
./run_magnetic_verbose.sh --view-logs
```

## 📋 常用命令

### Shell脚本命令

```bash
./run_magnetic_verbose.sh              # 运行完整测试
./run_magnetic_verbose.sh --auto-bind  # 自动绑定IP
./run_magnetic_verbose.sh --check-only # 仅检查环境
./run_magnetic_verbose.sh --skip-check # 跳过检查
./run_magnetic_verbose.sh --view-logs  # 查看日志
./run_magnetic_verbose.sh --cleanup    # 清理旧日志
./run_magnetic_verbose.sh --help       # 查看帮助
```

### Python脚本命令

```bash
python3 station_1700_magnetic_verbose.py --verbose     # 详细日志
python3 station_1700_magnetic_verbose.py --auto-bind   # 自动绑定
python3 station_1700_magnetic_verbose.py --help        # 查看帮助
```

## 📊 日志查询

### 查看特定类型日志

```bash
# 查看所有错误
grep -i error logs/magnetic_verbose_*.log

# 查看PLC通信
grep "【PLC" logs/magnetic_verbose_*.log

# 查看EB90指令
grep "【EB90" logs/magnetic_verbose_*.log

# 查看绑定事件
grep "【绑定事件】" logs/magnetic_verbose_*.log

# 查看MES通信
grep "【MES" logs/magnetic_verbose_*.log
```

### 实时监控

```bash
# 实时查看日志
tail -f logs/magnetic_verbose_*.log

# 实时查看错误
tail -f logs/magnetic_verbose_*.log | grep --line-buffered ERROR

# 实时查看PLC通信
tail -f logs/magnetic_verbose_*.log | grep --line-buffered "【PLC"
```

### 统计信息

```bash
# 统计通过的步骤
grep "步骤完成.*状态: PASS" logs/magnetic_verbose_*.log | wc -l

# 统计失败的步骤
grep "步骤完成.*状态: FAIL" logs/magnetic_verbose_*.log | wc -l

# 统计PLC请求数量
grep "【PLC请求】" logs/magnetic_verbose_*.log | wc -l

# 统计EB90指令数量
grep "【EB90指令下发】" logs/magnetic_verbose_*.log | wc -l
```

## 📁 文件结构

```
stations/
├── dgiot_magnetic_station_logger.erl  # Erlang日志模块 ⭐
├── station_1700_magnetic_verbose.py   # Python详细日志脚本 ⭐
├── run_magnetic_verbose.sh            # Shell一键调测脚本 ⭐
├── VERBOSE_TEST_GUIDE.md              # 完整使用指南
├── VERBOSE_SYSTEM_SUMMARY.md          # 系统总结文档
├── MAGNETIC_VERBOSE_QUICK_REF.md       # 快速参考（本文件）
└── logs/                              # 日志目录
    ├── magnetic_test_*.log             # 主日志
    └── magnetic_verbose_*.log         # 详细日志
```

## 🎯 测试流程

```
┌─────────────────────────────────────┐
│     阶段一: 环境准备                │
├─────────────────────────────────────┤
│ 1. 检查Python环境                   │
│ 2. 检查IP绑定状态                  │
│ 3. 检查DG-IoT服务状态              │
│ 4. 检查端口监听状态                │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│     阶段二: 测试执行                │
├─────────────────────────────────────┤
│ 1. 扫码绑定设备                    │
│ 2. PLC七步校验流程                 │
│ 3. EB90指令下发                    │
│ 4. 遥测数据发送                    │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│     阶段三: 结果验证                │
├─────────────────────────────────────┤
│ 1. 查看测试日志                    │
│ 2. 查看报文日志                    │
│ 3. 验证设备状态                    │
│ 4. 生成测试报告                    │
└─────────────────────────────────────┘
```

## 🔧 配置参数

### 磁航向工位配置

```python
MAGNETIC_STATION_CONFIG = {
    "station_id": 1700,
    "station_name": "磁航向校准工位",
    "plc_ip": "192.168.100.20",
    "plc_port": 502,
    "plc_base_addr": 1700,  # D1700
    "ground_station_ip": "192.168.100.21",
    "ground_station_port": 10007,
    "scanner_ip": "192.168.100.23",
    "scanner_port": 1234,
    "business_type": "扫码绑定"
}
```

##***REMOVED***绑定列表

```bash
192.168.100.20  # 磁航向PLC
192.168.100.21  # 磁航向DTU
192.168.100.23  # 扫描枪
```

### 端口列表

```bash
502    # Modbus端口
1234   # 扫描枪端口
10007  # 地测口端口
1801   # MES端口
20000  # DG-IoT端口
```

## 📝 日志格式

### 阶段日志

```
======================================================================
【阶段开始】
  阶段名称: XXX
  阶段描述: XXX
  开始时间: YYYY-MM-DD HH:MM:SS
======================================================================
```

### 步骤日志

```
======================================================================
【步骤开始】
  步骤序号: N
  步骤名称: XXX
  步骤描述: XXX
  开始时间: HH:MM:SS
======================================================================
```

### PLC通信日志

```
======================================================================
【PLC请求/响应】
  工位地址: DXXXX
  功能码: XXX
  请求/响应数据: {...}
  请求/响应时间: HH:MM:SS.ffffff
  响应延迟: XXX.XXXms
======================================================================
```

### EB90指令日志

```
======================================================================
【EB90指令下发】
  指令名称: XXX
  指令类型: XXX
  指令长度: XX 字节
  指令数据(hex): XXXXXXXX...
  发送时间: HH:MM:SS.ffffff
======================================================================
```

## 🛠️ 故障排查

### 问题: 日志文件不存在

**解决方案:**
```bash
# 检查日志目录
ls -la logs/

# 查看最新日志
ls -lt logs/ | head -5
```

### 问题: 日志输出过多

**解决方案:**
```bash
# 使用grep过滤
grep "ERROR" logs/magnetic_verbose_*.log

# 查看特定时间段的日志
grep "11:30:" logs/magnetic_verbose_*.log
```

### 问题: 找不到关键日志

**解决方案:**
```bash
# 搜索关键词
grep "关键词" logs/magnetic_verbose_*.log

# 搜索多个关键词
grep -E "关键词1|关键词2" logs/magnetic_verbose_*.log
```

## 📚 相关文档

- [详细日志调测指南](VERBOSE_TEST_GUIDE.md) - 完整使用指南
- [系统总结文档](VERBOSE_SYSTEM_SUMMARY.md) - 系统功能总结
- [磁航向工位调测智能体](../../../../../.codebuddy/agents/磁航向工位调测智能体.md)
- [磁航向测试步骤详解](../../../../../MAGNETIC_STATION_TEST_STEPS.md)

## 🆘 获取帮助

```bash
# Shell脚本帮助
./run_magnetic_verbose.sh --help

# Python脚本帮助
python3 station_1700_magnetic_verbose.py --help

# 查看详细指南
cat VERBOSE_TEST_GUIDE.md
```

---

*最后更新: 2026-03-26*
