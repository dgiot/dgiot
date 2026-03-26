# 磁航向工位一体化调测指南

## 概述

本指南提供磁航向工位（工位1700）的完整调测流程，包括环境准备、IP绑定、测试执行、结果验证等所有步骤。

## 文件清单

```
apps/dgiot_uav/priv/scripts/simulators/stations/
├── station_1700_magnetic_enhanced.py      # 增强版测试脚本（Python）
├── run_magnetic_station_test.sh          # 一键调测脚本（Shell）
└── MAGNETIC_STATION_TEST_GUIDE.md         # 本文档
```

## 工位配置

| 配置项 | 值 |
|--------|-----|
| 工位ID | 1700 |
| 工位名称 | 磁航向校准工位 |
| 业务类型 | 扫码绑定 |
| PLC IP | 192.168.100.20 |
| PLC端口 | 502 |
| PLC基地址 | D1700 |
| 地测口IP | 192.168.100.21 |
| 地测口端口 | 10007 |
| 扫码枪IP | 192.168.100.23 |
| 扫码枪端口 | 1234 |
| DG-IoT主机 | 192.168.100.100 |
| DG-IoT端口 | 20000 |

## 快速开始

### 方法1: 使用一键调测脚本（推荐）

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

### 方法2: 使用Python测试脚本

```bash
# 基本用法
python3 station_1700_magnetic_enhanced.py

# 自动绑定IP
python3 station_1700_magnetic_enhanced.py --auto-bind

# 指定设备ID
python3 station_1700_magnetic_enhanced.py --device-id UAV-002

# 跳过环境检查
python3 station_1700_magnetic_enhanced.py --skip-check

# 详细日志
python3 station_1700_magnetic_enhanced.py --verbose
```

## 测试流程

### 阶段一: 环境准备

1. **启动DG-IoT服务器**
   ```bash
   cd /root/gitee/dgiot
   make run
   ```

2. **检查IP绑定状态**
   ```bash
   ip addr show eth0 | grep "192.168.100"
   ```

3. **绑定IP地址（如需要）**
   ```bash
   sudo ip addr add 192.168.100.20/24 dev eth0
   sudo ip addr add 192.168.100.21/24 dev eth0
   sudo ip addr add 192.168.100.23/24 dev eth0
   ```

### 阶段二: 测试执行

测试脚本自动执行以下5个场景：

#### 场景1: 扫码绑定设备
- 模拟扫码枪扫描无人机二维码
- 绑定设备到工位1700
- 记录测试结果

#### 场景2: PLC七步校验流程
1. 读取工位就绪状态 (D1700)
2. 写入测试命令码 (D1751)
3. 读取测试确认状态 (D1710)
4. 复位工位状态 (D1700)
5. 清除测试确认 (D1710)
6. 写入完成确认码 (D1760)
7. 触发完成信号 (D1761)

#### 场景3: 无人机指令下发
- 舵面中位 (F0FB)
- 舵面使能 (F0F3)
- 复飞 (F0B9)

#### 场景4: 持续发送遥测数据
- 发送EB90 D1遥测帧（3次）
- 记录报文日志

#### 场景5: 测试结果汇聚
- 统计测试结果
- 聚合到无人机大物模型
- 上报MES服务器

### 阶段三: 结果验证

1. **查看测试日志**
   ```bash
   # 查看最新测试日志
   ls -lt test_records/station_1700/test_*.log | head -1
   
   # 查看日志内容
   cat test_records/station_1700/test_*.log | tail -100
   ```

2. **查看报文日志**
   ```bash
   # 查看报文日志
   ls -lt test_records/station_1700/packets/packets_*.log | head -1
   cat test_records/station_1700/packets/packets_*.log
   ```

3. **验证DG-IoT设备状态**
   ```bash
   # 查询设备绑定状态
   _build/emqx/rel/emqx/bin/emqx eval 'ets:tab2list(uav_ip_station_mapping).'
   
   # 查询工位信息
   _build/emqx/rel/emqx/bin/emqx eval '
     dgiot_uav_business_service:get_station_by_ip(<<"192.168.100.21">>).'
   
   # 查看测试项
   _build/emqx/rel/emqx/bin/emqx eval '
     dgiot_uav_test_loader:load_by_station(1700).'
   ```

4. **使用脚本快速验证**
   ```bash
   # 查看测试结果
   ./run_magnetic_station_test.sh --show-results
   
   # 验证设备状态
   ./run_magnetic_station_test.sh --verify
   ```

## 命令行参数

### Shell脚本参数

```
--device-id <ID>      指定设备ID (默认: UAV-001)
--auto-bind           自动绑定IP地址
--verbose             详细日志输出
--skip-check          跳过环境检查
--show-results        仅显示测试结果
--verify              验证DG-IoT中的设备状态
--help                显示帮助信息
```

### Python脚本参数

```
--device-id <ID>      指定设备ID (默认: UAV-001)
--auto-bind           自动绑定IP地址
--skip-check          跳过环境检查
-v, --verbose         详细日志输出
-h, --help            显示帮助信息
```

## 测试结果示例

### 成功的测试输出

```
======================================================================
磁航向工位一体化测试系统 - 增强版
======================================================================
工位ID: 1700
工位名称: 磁航向校准工位
业务类型: 扫码绑定
设备ID: UAV-001
自动绑定IP: 是
======================================================================

[场景1] 扫码绑定设备...
✅ 扫码枪扫描成功: UAV-001
✅ 设备绑定成功: UAV-001

[场景2] PLC七步校验流程...
  Step 1/7: 读取工位就绪状态
  Step 2/7: 写入测试命令码
  Step 3/7: 读取测试确认状态
  Step 4/7: 复位工位状态
  Step 5/7: 清除测试确认
  Step 6/7: 写入完成确认码
  Step 7/7: 触发完成信号
✅ PLC七步校验成功

[场景3] 无人机指令下发...
✅ 指令发送成功: 舵面中位
✅ 指令发送成功: 舵面使能
✅ 指令发送成功: 复飞

[场景4] 持续发送EB90遥测数据...
✅ 第1次遥测发送成功
✅ 第2次遥测发送成功
✅ 第3次遥测发送成功

[场景5] 测试结果汇聚...
测试结果汇聚: {'device_id': 'UAV-001', 'station_id': 1700, 'total_tests': 6, 'passed': 6, 'failed': 0, 'skipped': 0, 'pass_rate': '100.0%', 'timestamp': '2026-03-26T02:33:45.123456'}

======================================================================
测试总结
======================================================================

测试结果:
  ✅ 扫码绑定: passed
      UAV-001
  ✅ PLC七步校验: passed
      全部完成
  ✅ 遥控指令-舵面中位: passed
  ✅ 遥控指令-舵面使能: passed
  ✅ 遥控指令-复飞: passed
  ✅ 持续遥测: passed
      3次发送

总计: 6/6 通过
通过率: 100.0%

报文日志:
  记录报文数: 10
  日志文件: test_records/station_1700/packets/packets_20260326_023345.log

======================================================================

🎉 磁航向工位测试成功完成！
```

## 常见问题

### 1. DG-IoT服务器未运行

**问题**: 测试脚本提示DG-IoT服务器未运行

**解决**:
```bash
cd /root/gitee/dgiot
make run
```

### 2. IP地址未绑定

**问题**: 测试脚本提示IP地址未绑定

**解决**:
```bash
# 方法1: 使用脚本自动绑定
./run_magnetic_station_test.sh --auto-bind

# 方法2: 手动绑定
sudo ip addr add 192.168.100.20/24 dev eth0
sudo ip addr add 192.168.100.21/24 dev eth0
sudo ip addr add 192.168.100.23/24 dev eth0

# 方法3: 批量绑定
for ip in 192.168.100.{20,21,23}; do
    sudo ip addr add $ip/24 dev eth0
done
```

### 3. 端口被占用

**问题**: 测试脚本提示端口被占用

**解决**:
```bash
# 查找占用端口的进程
sudo lsof -i :1801
sudo lsof -i :502

# 终止进程
sudo kill -9 <PID>
```

### 4. Python模块缺失

**问题**: 测试脚本提示Python模块缺失

**解决**:
```bash
# 检查Python版本
python3 --version

# 安装必要的包（通常不需要，脚本使用标准库）
# pip3 install <module_name>
```

### 5. 权限不足

**问题**: IP绑定失败，提示权限不足

**解决**:
```bash
# 使用sudo执行脚本
sudo ./run_magnetic_station_test.sh --auto-bind

# 或者在脚本执行时输入密码
```

## 日志位置

所有测试日志都保存在 `test_records/station_1700/` 目录下：

```
test_records/station_1700/
├── test_YYYYMMDD_HHMMSS.log       # 测试主日志
└── packets/
    └── packets_YYYYMMDD_HHMMSS.log  # 报文日志
```

## 相关文档

- **磁航向工位智能体**: `.codebuddy/agents/磁航向工位调测智能体.md`
- **测试步骤详解**: `MAGNETIC_STATION_TEST_STEPS.md`
- **智能体总结**: `MAGNETIC_STATION_AGENT_SUMMARY.md`
- **原始脚本**: `station_1700_magnetic.py`

## 技术支持

如有问题，请参考：
1. 查看测试日志文件
2. 检查DG-IoT系统日志: `_build/emqx/rel/emqx/log/console.log`
3. 参考相关文档
4. 联系DG-IoT技术支持团队

---

**文档版本**: v1.0  
**创建日期**: 2026-03-26  
**维护者**: DG-IoT Team
