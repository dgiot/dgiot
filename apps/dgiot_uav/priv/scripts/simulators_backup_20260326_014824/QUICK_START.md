# 快速开始指南

## 状态

✅ **编译错误已修复**
✅ **磁航向工位测试脚本已创建**
✅ **测试记录管理已实现**

## 立即开始

### 第1步：编译验证

```bash
cd /root/gitee/dgiot
make emqx
```

**预期结果**: 编译成功，无错误

### 第2步：启动系统

```bash
cd /root/gitee/dgiot
make run
```

**预期结果**: 系统成功启动，端口20000监听

### 第3步：测试磁航向工位

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators

# 查看配置
python3 station_1700_magnetic_scenario.py --show-config

# 查看测试步骤
python3 station_1700_magnetic_scenario.py --show-steps

# 模拟运行
python3 station_1700_magnetic_scenario.py --dry-run

# 执行完整测试
python3 station_1700_magnetic_scenario.py
```

## 修复的错误

### dgiot_uav_auto_tester.erl (7个错误)

1. ✅ 第185行: `R` → `_R`
2. ✅ 第186行: `R` → `_R`
3. ✅ 第142行: `StationId` → `_StationId`
4. ✅ 第204行: `ItemId` → `_ItemId`
5. ✅ 第224行: `DeviceId` → `_DeviceId`
6. ✅ 第230行: `DeviceId` → `_DeviceId`
7. ✅ 第230行: `StationId` → `_StationId`

### dgiot_uav_ground_station_mapper.erl (2个错误)

1. ✅ 第236行: 删除重复的 `update_test_step` 调用
2. ✅ 第270行: 取消注释，使代码执行

## 磁航向工位配置

```
工位ID: 1700
工位名称: 磁航向工位
DG-IoT平台: 192.168.100.100:20000
PLC: 192.168.100.20:502
治具IP: 192.168.100.21

设备:
  - 地测口（无人机）: 192.168.100.21:10007 (动态IP)
  - 扫码枪: 192.168.100.23:1234 (静态IP)
```

## 测试步骤

### 1. 扫码获取设备编码 (5秒)
- 扫码枪扫描: UAV-001
- 自动绑定到1700工位

### 2. 磁航向校准 (30秒)
- 发送指令: 58e0d17e22
- 校准进度: 10% ~ 100%

### 3. 磁场精度检测 (20秒)
- 发送指令: eef47bcea7
- 验证误差 < 0.5°

**总耗时**: 约55秒

## 测试记录

测试完成后，生成以下文件：

```
test_records/station_1700/
├── station_1700_test_record_YYYYMMDD_HHMMSS.json  # JSON格式记录
├── station_1700_test_record_YYYYMMDD_HHMMSS.md   # Markdown格式报告
└── station_1700_YYYYMMDD_HHMMSS.log              # 日志文件
```

## 常用命令

### 查看编译错误

```bash
make emqx 2>&1 | grep -E "(error|Error|failed)"
```

### 验证修复

```bash
./verify_fix.sh
```

### 快速编译

```bash
./quick_compile.sh
```

### 查看系统状态

```bash
# 检查进程
ps aux | grep emqx

# 检查端口
netstat -tlnp | grep 20000

# 查看日志
tail -f _build/emqx/rel/emqx/log/console.log
```

### 热编译（系统运行时）

```bash
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_uav).'
```

## 文档索引

| 文档 | 说明 |
|------|------|
| `COMPILE_FIX_SUMMARY.md` | 编译错误修复总结 |
| `STATION_1700_USAGE_GUIDE.md` | 磁航向工位使用说明 |
| `STATION_1700_DEVICES_TEST_ITEMS.md` | 设备和测试项梳理 |
| `OTHER_STATIONS_TEST_PLAN.md` | 其他工位测试计划 |
| `STATION_TEST_SUMMARY.md` | 测试完成总结 |
| `QUICK_START.md` | 快速开始指南（本文件） |

## 故障排查

### 编译失败

**问题**: make emqx 报错

**解决**:
```bash
# 1. 清理编译产物
make clean

# 2. 重新编译
make emqx

# 3. 查看详细错误
make emqx 2>&1 | tee /tmp/compile.log
cat /tmp/compile.log | grep -A10 "error"
```

### 系统无法启动

**问题**: make run 启动失败

**解决**:
```bash
# 1. 检查端口占用
netstat -tlnp | grep 20000

# 2. 检查日志
tail -100 _build/emqx/rel/emqx/log/console.log

# 3. 检查配置
cat etc/emqx.conf | grep -E "(log.level|listener)"
```

### 测试脚本失败

**问题**: python3 station_1700_magnetic_scenario.py 报错

**解决**:
```bash
# 1. 检查Python版本
python3 --version  # 需要 Python 3.6+

# 2. 检查模块
python3 -c "import sys, json, logging"

# 3. 查看详细错误
python3 station_1700_magnetic_scenario.py --debug
```

### 连接失败

**问题**: 无法连接到DG-IoT平台

**解决**:
```bash
# 1. 检查系统运行
ps aux | grep emqx

# 2. 检查网络配置
ip addr show eth0 | grep 192.168.100.100

# 3. 测试连接
telnet 192.168.100.100 20000

# 4. 检查防火墙
iptables -L -n | grep 20000
```

## 下一步

完成磁航向工位测试后，可以：

1. **查看测试结果**
   ```bash
   cat test_records/station_1700/station_1700_test_record_*.md
   ```

2. **开发其他工位**
   参考 `OTHER_STATIONS_TEST_PLAN.md`

3. **产线集成测试**
   创建完整的产线测试流程

---

**文档版本**: v1.0.0
**创建日期**: 2026-03-25
**作者**: CodeBuddy AI Assistant
**状态**: ✅ 准备开始测试
