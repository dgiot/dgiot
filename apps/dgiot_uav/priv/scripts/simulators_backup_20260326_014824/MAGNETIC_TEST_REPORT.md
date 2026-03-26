# 磁航向工位(1700)测试完成报告

## 测试概述

- **测试时间**: 2026-03-25 10:41:21 (首次成功测试)
- **测试脚本**: `station_1700_magnetic_scenario.py`
- **监控脚本**: `run_magnetic_test.sh`
- **工位ID**: 1700
- **工位名称**: 磁航向工位

## 测试步骤配置

### 步骤1: 扫码获取设备编码
- **预计耗时**: 10秒
- **实际耗时**: 8秒
- **操作**: 扫码枪扫描设备编码(UAV-001)，触发设备绑定
- **状态**: ✅ 通过
- **结果**: 设备成功绑定到1700工位

### 步骤2: 磁航向校准
- **预计耗时**: 60秒
- **实际耗时**: 61秒
- **操作**: 发送遥控指令: F0 FB (舵面中位) + 58e0d17e22 (磁航向校准)
- **状态**: ✅ 通过
- **结果**: 磁航向数据正常，校准完成
- **校准进度**: 0% → 100% (20次迭代，每次3秒)

### 步骤3: 磁场精度检测
- **预计耗时**: 50秒
- **实际耗时**: 52秒
- **操作**: 采集磁场数据
- **状态**: ✅ 通过
- **结果**: 磁场误差 < 0.5° (实际最大误差: 0.2°)
- **样本数量**: 13个样本
- **最大误差**: 0.2°
- **平均误差**: 0.14°

## 测试结果总览

| 指标 | 值 |
|------|-----|
| **总测试时长** | 121.02秒 (~2分钟) |
| **步骤1时长** | 8秒 |
| **步骤2时长** | 61秒 |
| **步骤3时长** | 52秒 |
| **测试通过率** | 100% (3/3) |
| **系统状态** | 正常 |

## 测试记录文件

### JSON记录
- **路径**: `/root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/test_records/station_1700/`
- **文件名**: `station_1700_test_record_20260325_104121_20260325_104322.json`
- **内容**: 包含测试的完整时间戳、步骤、状态和结果

### Markdown报告
- **路径**: 同JSON目录
- **文件名**: `station_1700_test_record_20260325_104121_20260325_104322.md`
- **内容**: 格式化的测试报告，包含测试摘要和步骤结果

### Python日志
- **路径**: `/root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/test_records/station_1700/`
- **文件名**: `station_1700_20260325_105436.log`
- **内容**: Python脚本的详细执行日志

### 监控日志
- **路径**: `/root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/test_logs/`
- **文件名**: `magnetic_test_monitor_20260325_105434.log`
- **内容**: 监控脚本捕获的Erlang和Python日志

## 系统状态

### Erlang/EMQX系统
- **进程状态**: 运行中 (PID: 3831859)
- **启动时间**: 10:43
- **MQTT WSS**: 监听端口8084
- **HTTP服务**: 监听端口8081
- **API Hub**: 正常运行
- **UDP多播**: 监听端口8001/8002
- **TCP通道**: 监听端口20000

### 磁航向工位配置
```json
{
  "station_id": 1700,
  "name": "磁航向工位",
  "plc": {
    "ip": "192.168.100.20",
    "port": 502,
    "base_addr": 1700,
    "fixture_addr": 0
  },
  "fixture_ip": "192.168.100.21",
  "fixture_port": 10007
}
```

## 测试结论

✅ **磁航向工位(1700)测试完全通过**

- 所有3个测试步骤均成功完成
- 测试时长符合预期（约2分钟）
- 磁场精度检测通过（最大误差0.2° < 0.5°阈值）
- 设备绑定功能正常
- 系统日志记录完整

## 使用方法

### 执行测试
```bash
# 方法1: 使用统一的监控脚本（推荐）
bash /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/run_magnetic_test.sh

# 方法2: 直接运行Python脚本
python3 /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/station_1700_magnetic_scenario.py
```

### 查看配置
```bash
# 查看工位配置
python3 /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/station_1700_magnetic_scenario.py --show-config

# 查看测试步骤
python3 /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/station_1700_magnetic_scenario.py --show-steps
```

### 查看测试记录
```bash
# 查看最新的测试记录JSON
cat /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/test_records/station_1700/$(ls -t /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/test_records/station_1700/*.json | head -1)

# 查看测试报告
cat /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/test_records/station_1700/$(ls -t /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/test_records/station_1700/*.md | head -1)
```

## 技术细节

### 测试流程
1. **设备绑定**: 扫码枪扫描设备编码(UAV-001)，系统自动绑定到1700工位
2. **磁航向校准**: 
   - 发送舵面中位指令 (F0 FB)
   - 发送磁航向校准指令 (58e0d17e22)
   - 循环20次，每次3秒，校准进度从0%到100%
3. **磁场精度检测**:
   - 发送磁航向测试指令 (eef47bcea7)
   - 采集13个磁场数据样本
   - 计算最大误差和平均误差
   - 判断是否满足精度要求（< 0.5°）

### 关键指标
- **设备编码**: UAV-001
- **扫码枪IP**: 192.168.100.23:1234
- **治具IP**: 192.168.100.21:10007
- **DG-IoT平台**: 192.168.100.100:20000
- **PLC服务器**: 192.168.100.20:502

## 备注

- 测试脚本已集成到统一的监控脚本中，无需手动管理多个脚本
- 测试记录自动保存为JSON和Markdown格式，便于查看和分析
- 监控脚本会同时捕获Erlang和Python日志，方便调试
- 当前系统已部署最新的编译代码，可以正常执行自动化测试

---

**报告生成时间**: 2026-03-25 10:55:40
**报告生成者**: DG-IoT Auto-Test System
