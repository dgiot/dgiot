# 磁航向工位闭环测试系统

## 概述

完整的磁航向工位自闭环测试系统，包含所有必要的客户端和服务端组件，实现端到端的自动化测试。

## 架构

```
┌─────────────────────────────────────────────────────────────┐
│                    磁航向工位闭环测试架构                      │
└─────────────────────────────────────────────────────────────┘

                          ┌──────────────┐
                          │  MES Server  │
                          │  (HTTP:801)  │
                          └──────┬───────┘
                                 │
                                 │ 接收测试结果
                                 │
┌──────────────┐         ┌──────▼───────┐         ┌──────────────┐
│ 地测口 Client│◄───────►│  DG-IoT      │◄───────►│ 扫码枪 Client│
│ (TCP:10007)  │  注册    │  Server      │  扫描    │ (TCP:1234)  │
│              │  遥测    │  (TCP:20000) │          │             │
└──────────────┘         └──────┬───────┘         └──────────────┘
                                 │
                                 │ Modbus TCP
                                 │
                          ┌──────▼───────┐
                          │  PLC Server  │
                          │  (Modbus:502)│
                          │              │
                          │ 192.168.100.20 │
                          └──────────────┘
```

## 组件说明

### 1. MES Server (HTTP Server)
- **端口**: 801
- **协议**: HTTP REST API
- **功能**: 接收产线测试结果和状态更新

### 2. 地测口 Client (TCP Client)
- **源端口**: 10007
- **目标**: DG-IoT服务器 (192.168.100.100:20000)
- **功能**: 
  - 发送注册报文 `wrj_dicekou\n`
  - 发送EB90遥测数据

### 3. 扫码枪 Client (TCP Client)
- **源端口**: 1234
- **目标**: DG-IoT服务器 (192.168.100.100:20000)
- **功能**: 
  - 扫描设备编码
  - 触发设备绑定

### 4. DG-IoT Server (已运行)
- **端口**: 20000
- **功能**: 
  - 接收设备连接
  - 处理EB90协议
  - 工位映射
  - 数据转发到MES

### 5. PLC Server (可选)
- **端口**: 502
- **协议**: Modbus TCP
- **功能**: 模拟PLC设备

## 快速开始

### 1. 启动DG-IoT服务器

```bash
cd /root/gitee/dgiot
make run
```

### 2. 绑定IP地址（需要root权限）

```bash
# 绑定磁航向工位相关IP
sudo ip addr add 192.168.100.20/24 dev eth0  # 磁航向PLC
sudo ip addr add 192.168.100.21/24 dev eth0  # 地测口
sudo ip addr add 192.168.100.23/24 dev eth0  # 扫码枪

# 验证绑定
ip addr show eth0 | grep "192.168.100"
```

### 3. 启动PLC模拟器（可选）

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
python3 plc_simulator.py &
```

### 4. 运行闭环测试

```bash
# 方法1: 使用启动脚本（推荐）
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
chmod +x start_magnetic_closed_loop.sh
./start_magnetic_closed_loop.sh

# 方法2: 直接运行Python脚本
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
python3 magnetic_station_closed_loop_test.py
```

## 测试场景

### 场景1: 扫码绑定设备
- 扫码枪扫描设备编码 `UAV-001`
- DG-IoT接收扫描消息
- 设备绑定到1700工位

### 场景2: 地测口注册
- 地测口连接DG-IoT
- 发送注册报文 `wrj_dicekou\n`
- 创建工位映射: `192.168.100.21 → 1700`

### 场景3: EB90遥测数据上报
- 地测口发送EB90 D1遥测帧
- DG-IoT解析EB90协议
- 数据存储到TDengine

### 场景4: MES数据上报
- DG-IoT转发测试结果到MES
- MES Server接收并确认

## 检查测试结果

### 查看IP-工位映射

```bash
_build/emqx/rel/emqx/bin/emqx eval 'ets:tab2list(uav_ip_station_mapping).'
```

预期输出:
```erlang
[{<<"192.168.100.21">>,{Timestamp, 1700}}]
```

### 查询特定IP的工位

```bash
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_business_service:get_station_by_ip(<<"192.168.100.21">>).'
```

预期输出:
```erlang
{ok, 1700}
```

### 查看EB90解析日志

```bash
# 查看地测口注册日志
tail -f _build/emqx/rel/emqx/log/console.log | grep "EB90"

# 查看工位绑定日志
tail -f _build/emqx/rel/emqx/log/console.log | grep "1700"
```

### 测试MES接口

```bash
# 发送测试数据到MES
curl -X POST http://localhost:801/lezao/jymes/api/equip/proExec \
  -H "Content-Type: application/json" \
  -d '{
    "device_id": "UAV-001",
    "station_id": "1700",
    "test_result": "passed",
    "test_data": {"magnetic_heading": 0.5}
  }'
```

## 日志说明

### Python脚本日志
- **MES Server**: `[MES]` 前缀
- **地测口 Client**: `[地测口]` 前缀
- **扫码枪 Client**: `[扫码枪]` 前缀

### DG-IoT日志
- **地测口注册**: `[EB90] 地测口无人机注册成功`
- **工位映射**: `[EB90] 动态创建工位映射`
- **EB90解析**: `[EB90_RECEIVE]` 和 `[EB90_HANDLE]`

## 常见问题

### 1. IP未绑定导致测试失败

**问题**: 连接失败，提示IP不存在

**解决**: 绑定IP地址
```bash
sudo ip addr add 192.168.100.21/24 dev eth0
```

### 2. DG-IoT服务器未运行

**问题**: 无法连接到端口20000

**解决**: 启动DG-IoT服务器
```bash
cd /root/gitee/dgiot
make run
```

### 3. 端口被占用

**问题**: MES Server启动失败，端口801被占用

**解决**: 
```bash
# 查看端口占用
sudo netstat -tlnp | grep 801

# 修改MES端口（在脚本中修改MES_SERVER_PORT常量）
```

### 4. EB90解析失败

**问题**: 日志显示EB90解析错误

**解决**: 检查EB90帧格式
```bash
# 验证帧数据
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_eb90_protocol:test_ground_station().'
```

## 扩展功能

### 1. 添加更多测试场景

编辑 `magnetic_station_closed_loop_test.py`:

```python
def run_test_scenario(self):
    # 添加自定义测试场景
    logger.info("\n[场景4] 自定义测试...")
    # ... 自定义逻辑
```

### 2. 集成PLC交互

启动PLC模拟器后，可以模拟PLC读写:

```python
# 在测试脚本中添加PLC交互
import pymodbus.client as ModbusClient

client = ModbusClient.ModbusTcpClient('192.168.100.20', port=502)
client.connect()

# 读取寄存器
result = client.read_holding_registers(1700, 10)
print(result.registers)

client.close()
```

### 3. 性能测试

添加循环发送测试:

```python
# 压力测试：发送1000次EB90数据
for i in range(1000):
    self.ground_station.send_eb90_data(EB90_D1_FRAME, f"压力测试-{i}")
    time.sleep(0.1)
```

## 相关文件

| 文件 | 说明 |
|------|------|
| `magnetic_station_closed_loop_test.py` | 闭环测试主脚本 |
| `start_magnetic_closed_loop.sh` | 启动脚本 |
| `plc_simulator.py` | PLC模拟器 |
| `mes_simulator.py` | MES模拟器（独立） |
| `test_ground_station_register.py` | 地测口注册测试（简单版） |

## 技术栈

- **Python 3.8+**
- **socket**: TCP客户端
- **http.server**: HTTP服务端
- **threading**: 多线程
- **dataclasses**: 数据模型

## 更新记录

- 2026-03-25: 创建完整闭环测试系统
  - 集成MES Server
  - 集成地测口Client
  - 集成扫码枪Client
  - 实现自动化测试流程

## 参考资料

- [DG-IoT 开发指南](/root/gitee/dgiot/CODEBUDDY.md)
- [EB90协议文档](/root/gitee/dgiot/apps/dgiot_uav/priv/capture/协议文档/)
- [磁航向工位配置](/root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/stations/1700_magnetic/)

---

**维护者**: DGIoT Team  
**最后更新**: 2026-03-25
