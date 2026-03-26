# 磁航向工位测试脚本使用指南

## 📍 脚本位置

```
/root/gitee/dgiot/apps/dgiot_uav/test/test_magnetic_full_flow.erl
```

**文件信息**：
- 代码行数：114行
- 文件大小：3.9KB
- 语言：Erlang
- 用途：磁航向工位完整业务流程测试

---

## 🎯 功能说明

### 测试流程（3步）

```
步骤1: 检查PLC客户端
  └─ 检查磁航向工位（1700）的PLC客户端是否运行

步骤2: 加载测试项
  └─ 从Parse库加载磁航向工位的测试项

步骤3: 执行测试项和PLC指令
  └─ 解析测试步骤
  └─ 下发PLC指令
  └─ 打印执行结果
```

### 支持的操作

| 操作类型 | 说明 | 执行内容 |
|---------|------|---------|
| **send** | 发送指令 | 下发PLC指令码到D1751地址 |
| **judge** | 判据等待 | 等待传感器数据判据 |
| **其他** | 其他操作 | 打印操作信息 |

---

## 🚀 使用方法

### 方式1：直接运行

```bash
cd /root/gitee/dgiot
_build/emqx/rel/emqx/bin/emqx eval 'test_magnetic_full_flow:test().'
```

### 方式2：编译后运行

```bash
cd /root/gitee/dgiot
_build/emqx/rel/emqx/bin/emqx eval 'c:c("apps/dgiot_uav/test/test_magnetic_full_flow").'
_build/emqx/rel/emqx/bin/emqx eval 'test_magnetic_full_flow:test().'
```

---

## 📊 输出示例

```
========================================
Magnetic Station Full Business Flow Test
========================================

Step 1: Check PLC Client for Magnetic Station (1700)
  PLC Client PID: <0.1234.0>

Step 2: Load Test Items from Parse
  Loaded 2 test items

Step 3: Test Items and PLC Commands

  Test Item: 磁航向校准测试项
    ID: 58e0d17e22
    Steps: 13
    Step: action=send, target=工位PLC, send=2
      -> Send Action: target=工位PLC, content=2
      -> Sending PLC Command: Code=2
      -> Result: {send,<<...>>}
    ...

========================================
Full Business Flow Test Complete
========================================
```

---

## 🔧 前置条件

### 1. 系统运行

```bash
# 启动DGIOT系统
make run
```

### 2. PLC模拟器运行

```bash
# 启动PLC模拟器（可选，用于测试）
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
python3 plc_simulator.py
```

### 3. PLC客户端启动

```bash
# 检查PLC客户端状态
_build/emqx/rel/emqx/bin/emqx eval 'global:whereis_name({plc, 1700}).'
```

---

## 📝 测试项配置

测试项从Parse库加载，包含以下字段：

```json
{
  "id": "58e0d17e22",
  "name": "磁航向校准测试项",
  "station_id": 1700,
  "station_name": "磁航向",
  "steps": [
    {
      "action_type": "send",
      "target": "工位PLC",
      "send": {
        "content": "2"
      }
    },
    ...
  ]
}
```

---

## 🎯 PLC指令映射

### 磁航向工位指令码

| 指令码 | 说明 | 地址 |
|--------|------|------|
| 1 | 顺时针旋转360度 | D1751 |
| 2 | 逆时针旋转360度 | D1751 |
| 3 | 反转90度垂直 | D1751 |
| 4 | 垂直位置零 | D1751 |

### 地址映射

```
工位ID: 1700
基地址: D1700
指令地址: D1751 (相对地址51)
告警地址: D1730 (相对地址30)
心跳地址: D1749 (相对地址49)
```

---

## 🧪 完整测试流程

### 1. 启动系统

```bash
# 终端1：启动DGIOT系统
cd /root/gitee/dgiot
make run
```

### 2. 启动PLC模拟器（可选）

```bash
# 终端2：启动PLC模拟器
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
python3 plc_simulator.py
```

### 3. 运行测试

```bash
# 终端3：运行磁航向测试
cd /root/gitee/dgiot
_build/emqx/rel/emqx/bin/emqx eval 'test_magnetic_full_flow:test().'
```

### 4. 查看日志

```bash
# 查看系统日志
tail -f /root/gitee/dgiot/_build/emqx/rel/emqx/log/console.log

# 查看PLC模拟器日志
tail -f /tmp/plc_simulator.log
```

---

## 📋 测试检查清单

- [ ] DGIOT系统已启动
- [ ] PLC客户端已注册（工位1700）
- [ ] 测试项已配置在Parse库
- [ ] PLC模拟器已启动（可选）
- [ ] 网络连接正常

---

## 🐛 故障排除

### 问题1：PLC客户端未找到

**症状**：
```
ERROR: PLC Client not found
```

**解决**：
```bash
# 检查PLC客户端状态
_build/emqx/rel/emqx/bin/emqx eval 'global:whereis_name({plc, 1700}).'

# 重启PLC客户端
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_plc_tcp_client:start_link(...).'
```

### 问题2：测试项加载失败

**症状**：
```
ERROR loading test items: not_found
```

**解决**：
```bash
# 检查Parse库连接
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_parse:ping().'

# 手动加载测试项
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_test_loader:load_by_station(1700).'
```

### 问题3：PLC指令下发失败

**症状**：
```
Result: {error, connection_refused}
```

**解决**：
```bash
# 检查PLC模拟器状态
ps aux | grep plc_simulator.py

# 重启PLC模拟器
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
python3 plc_simulator.py
```

---

## 📚 相关文档

- [PLC模拟器使用指南](/tmp/PLC_SIMULATOR_GUIDE.md)
- [PLC客户端文档](../apps/dgiot_uav/src/channel/dgiot_uav_plc_tcp_client.erl)
- [测试项加载器](../apps/dgiot_uav/src/business/test/dgiot_uav_test_loader.erl)

---

## 🎉 总结

**test_magnetic_full_flow.erl** 是磁航向工位的唯一测试脚本：

- ✅ **简洁高效**：114行代码
- ✅ **系统集成**：与DGIOT紧密集成
- ✅ **完整流程**：覆盖PLC检查、测试项加载、指令下发
- ✅ **易于维护**：代码清晰，注释完整
- ✅ **独立测试**：无需额外依赖

**快速测试命令**：
```bash
_build/emqx/rel/emqx/bin/emqx eval 'test_magnetic_full_flow:test().'
```
