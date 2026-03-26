# 磁航向工位详细日志调测指南

## 📋 概述

本指南介绍如何使用磁航向工位详细日志调测系统,该系统提供了完整的日志记录功能,便于调试和问题排查。

### 核心特性

✅ **详细的阶段日志** - 环境准备、测试执行、结果验证
✅ **详细的步骤日志** - 每个测试步骤的开始和完成
✅ **详细的PLC通信日志** - 请求和响应数据
✅ **详细的EB90指令日志** - 指令名称、类型、数据
✅ **详细的遥测数据日志** - 数据类型、序列号、内容
✅ **详细的绑定事件日志** - 扫码、绑定、上线事件
✅ **详细的错误日志** - 错误类型、上下文、原因
✅ **详细的测试总结** - 统计数据和结果

## 📁 文件结构

```
stations/
├── station_1700_magnetic_enhanced.py      # 增强版测试脚本
├── station_1700_magnetic_verbose.py       # 详细日志测试脚本 ⭐
├── run_magnetic_verbose.sh                # 一键调测脚本 ⭐
├── dgiot_magnetic_station_logger.erl      # Erlang日志模块 ⭐
└── logs/                                  # 日志目录
    ├── magnetic_test_YYYYMMDD_HHMMSS.log  # 主日志
    └── magnetic_verbose_YYYYMMDD_HHMMSS.log # 详细日志
```

## 🚀 快速开始

### 方式1: 一键调测（推荐）

```bash
# 进入脚本目录
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/stations

# 运行完整测试
./run_magnetic_verbose.sh

# 自动绑定IP并测试
./run_magnetic_verbose.sh --auto-bind

# 仅检查环境
./run_magnetic_verbose.sh --check-only

# 查看日志
./run_magnetic_verbose.sh --view-logs
```

### 方式2: Python脚本

```bash
# 运行详细日志测试
python3 station_1700_magnetic_verbose.py --verbose

# 自动绑定IP并测试
python3 station_1700_magnetic_verbose.py --auto-bind --verbose
```

## 📊 日志输出示例

### 阶段日志

```
======================================================================
【阶段开始】
  阶段名称: 环境准备
  阶段描述: 检查IP绑定、DG-IoT状态、端口监听
  开始时间: 2026-03-26 11:30:00
======================================================================
```

### 步骤日志

```
======================================================================
【步骤开始】
  步骤序号: 1
  步骤名称: 检查IP绑定
  步骤描述: 检查磁航向工位IP地址绑定状态
  开始时间: 11:30:01
======================================================================
```

### PLC通信日志

```
======================================================================
【PLC请求】
  工位地址: D1700
  功能码: READ
  请求数量: 1
  请求数据: {'address': 1700, 'count': 1}
  请求时间: 11:30:02.123456
======================================================================

======================================================================
【PLC响应】
  工位地址: D1700
  功能码: READ
  响应数据: {'value': 0}
  响应时间: 11:30:02.234567
  响应延迟: 111.111ms
======================================================================
```

### EB90指令日志

```
======================================================================
【EB90指令下发】
  指令名称: 舵面中位
  指令类型: 遥控指令
  指令长度: 66 字节
  指令数据(hex): EB90FFFF00006100000000A55AF0FB000000000000000000...
  发送时间: 11:30:05.123456
======================================================================
```

### 遥测数据日志

```
======================================================================
【遥测数据发送】
  数据类型: D1遥测帧
  序列号: 1
  数据长度: 128
  数据内容: {...}
  发送时间: 11:30:06.123456
======================================================================
```

### 绑定事件日志

```
======================================================================
【绑定事件】
  事件类型: 扫码绑定
  无人机ID: UAV-001
  事件时间: 2026-03-26 11:30:10
  绑定数据:
    serial_no: Test01|1|5000000020004|10|2026032502|||
    material_code: 5000000020004
    project_no: 1
    purchase_order_no: Test01
    drone_no: 2026032502
======================================================================
```

### MES通信日志

```
======================================================================
【MES请求】
  MES URL: http://172.1.2.222:801/lezao/jymes/api/equip/proExec
  请求数据:
    {
        "data_record": {
            "material_code": "5000000020004",
            "project_no": "1",
            "purchase_order_no": "Test01",
            "scanner_time": 1774494217,
            "supplier": ""
        },
        "date_time": "2026-03-26 11:03:37",
        "drone_no": "2026032502",
        "func_id": "SCANNER_MES",
        "line_no": "A:1",
        "line_sta": 1
    }
  请求时间: 11:30:15.123456
======================================================================

======================================================================
【MES响应】
  HTTP状态码: 200
  响应数据:
    {
        "code": 200,
        "msg": "成功"
    }
  响应时间: 11:30:15.234567
  响应延迟: 111.111ms
======================================================================
```

### 测试总结

```
======================================================================
【测试总结】
  测试统计:
    total_steps: 10
    passed_steps: 10
    failed_steps: 0
    skipped_steps: 0
    start_time: 2026-03-26 11:30:00
    end_time: 2026-03-26 11:30:30
    duration: 30.0
  结果统计:
    device_bound: true
    plc_tested: true
    eb90_sent: true
    telemetry_sent: true
    mes_reported: true
  总结时间: 2026-03-26 11:30:31
======================================================================
```

## 🔧 日志级别

### Erlang日志级别

- **error** - 错误信息（默认显示）
- **warning** - 警告信息
- **info** - 一般信息（默认显示）
- **debug** - 调试信息（需要verbose模式）

### Python日志级别

- **ERROR** - 错误信息
- **WARNING** - 警告信息
- **INFO** - 一般信息
- **DEBUG** - 调试信息（需要--verbose参数）

## 📝 日志文件说明

### 主日志 (magnetic_test_YYYYMMDD_HHMMSS.log)

包含Shell脚本的执行日志:
- 环境检查结果
- 测试执行状态
- 错误和警告信息
- 测试总结

### 详细日志 (magnetic_verbose_YYYYMMDD_HHMMSS.log)

包含Python脚本的详细日志:
- 阶级开始和完成
- 步骤开始和完成
- PLC通信详情
- EB90指令详情
- 遥测数据详情
- 绑定事件详情
- MES通信详情

## 🛠️ 故障排查

### 问题1: 日志文件过大

**解决方案:**
```bash
# 清理7天前的旧日志
./run_magnetic_verbose.sh --cleanup

# 手动清理
find logs/ -name "*.log" -mtime +7 -delete
```

### 问题2: 日志输出太多

**解决方案:**
```bash
# 使用INFO级别（不显示DEBUG日志）
python3 station_1700_magnetic_verbose.py

# 只显示ERROR级别
tail -f logs/magnetic_verbose_*.log | grep ERROR
```

### 问题3: 找不到关键日志

**解决方案:**
```bash
# 搜索特定关键词
grep "EB90指令下发" logs/magnetic_verbose_*.log

# 搜索错误日志
grep "ERROR" logs/magnetic_verbose_*.log

# 搜索特定时间段
grep "11:30:" logs/magnetic_verbose_*.log
```

## 📊 日志分析工具

### 统计测试结果

```bash
# 统计通过的步骤
grep "步骤完成.*状态: PASS" logs/magnetic_verbose_*.log | wc -l

# 统计失败的步骤
grep "步骤完成.*状态: FAIL" logs/magnetic_verbose_*.log | wc -l

# 统计PLC请求数量
grep "【PLC请求】" logs/magnetic_verbose_*.log | wc -l
```

### 提取关键信息

```bash
# 提取所有EB90指令
grep "【EB90指令下发】" logs/magnetic_verbose_*.log > eb90_commands.log

# 提取所有绑定事件
grep "【绑定事件】" logs/magnetic_verbose_*.log > binding_events.log

# 提取所有MES请求
grep "【MES请求】" logs/magnetic_verbose_*.log > mes_requests.log
```

### 生成测试报告

```bash
# 生成HTML格式的测试报告
python3 -c "
import re
import json

log_file = 'logs/magnetic_verbose_*.log'
# 实现日志解析和报告生成逻辑
"
```

## 🔍 日志调试技巧

### 1. 使用grep过滤日志

```bash
# 查看所有错误
grep -i error logs/magnetic_verbose_*.log

# 查看PLC通信
grep "【PLC" logs/magnetic_verbose_*.log

# 查看EB90指令
grep "【EB90" logs/magnetic_verbose_*.log
```

### 2. 使用tail实时查看

```bash
# 实时查看日志
tail -f logs/magnetic_verbose_*.log

# 实时查看错误日志
tail -f logs/magnetic_verbose_*.log | grep --line-buffered ERROR

# 实时查看特定步骤
tail -f logs/magnetic_verbose_*.log | grep --line-buffered "步骤名称: 扫码绑定"
```

### 3. 使用awk分析日志

```bash
# 统计各步骤的执行时间
awk '/步骤名称:/ {name=\$0} /结束时间:/ {print name, \$0}' logs/magnetic_verbose_*.log

# 提取PLC响应时间
awk '/响应延迟:/ {print \$0}' logs/magnetic_verbose_*.log
```

## 📚 相关文档

- [磁航向工位调测智能体](../../../../../.codebuddy/agents/磁航向工位调测智能体.md)
- [磁航向测试步骤详解](../../../../../MAGNETIC_STATION_TEST_STEPS.md)
- [EB90协议文档](../../../../../docs/协议/)
- [MES接口文档](../../../../../docs/MES/)

## 🆘 获取帮助

```bash
# 查看Shell脚本帮助
./run_magnetic_verbose.sh --help

# 查看Python脚本帮助
python3 station_1700_magnetic_verbose.py --help
```

---

*最后更新: 2026-03-26*
