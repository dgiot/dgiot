# 磁航向工位闭环测试 - 快速启动指南

## ✅ 环境检查结果

```
✅ DG-IoT服务器         - 运行中 (192.168.100.100:20000)
✅ IP 192.168.100.20   - 已绑定 (磁航向PLC)
✅ IP 192.168.100.21   - 已绑定 (地测口)
✅ IP 192.168.100.23   - 已绑定 (扫码枪)
✅ PLC模拟器            - 运行中
✅ MES端口             - 可用 (801)
✅ Python环境          - 3.11

总计: 7/7 项通过 ✅
```

## 🚀 快速启动

### 一键启动闭环测试

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
./start_magnetic_closed_loop.sh
```

### 或者直接运行Python脚本

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
python3 magnetic_station_closed_loop_test.py
```

## 📋 测试流程

1. **MES Server启动** - 监听801端口
2. **地测口连接** - TCP Client连接DG-IoT:20000
3. **地测口注册** - 发送 `wrj_dicekou\n`
4. **扫码枪连接** - TCP Client连接DG-IoT:20000
5. **扫码绑定** - 扫描设备编码 `UAV-001`
6. **EB90数据上报** - 发送遥测数据
7. **MES数据接收** - DG-IoT转发数据到MES

## 🔍 验证结果

### 查看IP-工位映射

```bash
_build/emqx/rel/emqx/bin/emqx eval 'ets:tab2list(uav_ip_station_mapping).'
```

**预期输出**:
```erlang
[{<<"192.168.100.21">>,{Timestamp, 1700}}]
```

### 查询工位状态

```bash
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_business_service:get_station_by_ip(<<"192.168.100.21">>).'
```

**预期输出**:
```erlang
{ok, 1700}
```

### 查看EB90解析日志

```bash
# 实时查看日志
tail -f _build/emqx/rel/emqx/log/console.log | grep "EB90"

# 或者使用我们的日志工具
cd /root/gitee/dgiot
./tools/logs_153.sh watch | grep -E "(EB90|1700)"
```

## 📊 测试输出示例

```
======================================================================
磁航向工位闭环测试系统
======================================================================

[步骤1] 启动MES服务器...
2026-03-25 10:00:00 [INFO] [MES] 服务器启动成功: http://0.0.0.0:801

[步骤2] 检查DG-IoT服务器状态...
✅ DG-IoT服务器正在运行: 192.168.100.100:20000

[步骤3] 地测口连接DG-IoT...
2026-03-25 10:00:01 [INFO] [地测口] 成功连接到DG-IoT: 192.168.100.100:20000

[步骤4] 地测口注册...
2026-03-25 10:00:02 [INFO] [地测口] 发送注册报文: b'wrj_dicekou\n'
2026-03-25 10:00:03 [INFO] [地测口] ✅ 注册成功

[步骤5] 扫码枪连接DG-IoT...
2026-03-25 10:00:04 [INFO] [扫码枪] 成功连接到DG-IoT: 192.168.100.100:20000

✅ 测试环境准备完成

======================================================================
开始执行测试场景
======================================================================

[场景1] 扫码绑定设备...
2026-03-25 10:00:05 [INFO] [扫码枪] 扫描设备: UAV-001
✅ 设备 UAV-001 扫描成功

[场景2] 地测口发送EB90遥测数据...
2026-03-25 10:00:07 [INFO] [地测口] 发送EB90数据成功: D1遥测帧, 长度: 68字节
✅ EB90遥测数据发送成功

[场景3] 持续发送遥测数据（3次）...
  第1次发送...
  ✅ 第1次发送成功
  第2次发送...
  ✅ 第2次发送成功
  第3次发送...
  ✅ 第3次发送成功

✅ 测试场景执行完成

======================================================================
测试总结
======================================================================

测试结果:
  ✅ 扫码绑定: passed (UAV-001)
  ✅ EB90遥测: passed (D1帧)
  ✅ 持续遥测: passed (3次发送)

总计: 3/3 通过

请检查DG-IoT日志确认处理结果:
  _build/emqx/rel/emqx/bin/emqx eval 'ets:tab2list(uav_ip_station_mapping).'
  _build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_business_service:get_station_by_ip(<<"192.168.100.21">>).'

======================================================================

闭环测试完成！
MES服务器将继续运行，监听DG-IoT的数据上报...
访问: http://localhost:801

按 Ctrl+C 退出...
```

## 🎯 核心组件

| 组件 | 类型 | 端口 | 状态 |
|------|------|------|------|
| **MES Server** | HTTP Server | 801 | ✅ 启动 |
| **地测口 Client** | TCP Client | 10007→20000 | ✅ 连接 |
| **扫码枪 Client** | TCP Client | 1234→20000 | ✅ 连接 |
| **DG-IoT Server** | TCP Server | 20000 | ✅ 运行中 |
| **PLC Server** | Modbus Server | 502 | ✅ 运行中 |

## 📁 相关文件

```
/root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/
├── magnetic_station_closed_loop_test.py  # 主测试脚本
├── start_magnetic_closed_loop.sh         # 启动脚本
├── check_magnetic_environment.py         # 环境检查
├── MAGNETIC_CLOSED_LOOP_TEST.md          # 详细文档
├── plc_simulator.py                      # PLC模拟器
└── mes_simulator.py                      # MES模拟器（独立）
```

## 🔧 故障排查

### 问题1: IP未绑定

```bash
# 检查IP绑定状态
ip addr show eth0 | grep "192.168.100"

# 绑定缺失的IP
sudo ip addr add 192.168.100.20/24 dev eth0
sudo ip addr add 192.168.100.21/24 dev eth0
sudo ip addr add 192.168.100.23/24 dev eth0
```

### 问题2: DG-IoT未运行

```bash
# 启动DG-IoT
cd /root/gitee/dgiot
make run
```

### 问题3: 端口被占用

```bash
# 检查端口占用
sudo netstat -tlnp | grep -E "(801|502|20000)"

# 终止占用进程
sudo kill -9 <PID>
```

## 💡 提示

- ✅ 所有组件已启动，环境准备就绪
- 📝 测试过程会自动记录日志
- 🔄 MES服务器会持续运行，直到手动退出
- 📊 可以通过DG-IoT日志查看详细处理过程

---

**环境状态**: ✅ 就绪  
**最后检查**: 2026-03-25  
**文档版本**: v1.0
