# 无人机测试产线模拟器

## 核心脚本

| 文件 | 说明 |
|------|------|
| `integrated_production_line.py` | **主脚本** - 产线集成调度器，统一调度所有模拟器 |
| `fixture_simulator.py` | 治具模拟器（舵面传感器、单片机、地测口） |
| `plc_simulator.py` | PLC模拟器（Modbus TCP服务器） |
| `uav_simulator.py` | 无人机模拟器（多播通信） |
| `multicast_core.py` | 多播通信核心模块 |
| `mes_simulator.py` | MES模拟器（制造执行系统） |

## 快速开始

```bash
# 执行产线测试（带闭环验证）
python3 integrated_production_line.py --verify --save-report

# 查看帮助
python3 integrated_production_line.py --help
```

## 产线配置

##***REMOVED***地址规划
- **工控机**: 192.168.100.100
- **PLC服务器**: 192.168.100.40 (端口502)
- **磁航向PLC**: 192.168.100.20 (端口502)
- **扫描枪**: 192.168.100.23 (端口1234)
- **噪音传感器**: 192.168.100.35/36 (端口21000)

### 工位配置
| IP | 工位 | PLC基地址 | 设备 |
|----|------|----------|------|
| 192.168.100.21 | 磁航向 | 1700 | 地测口+扫描枪 |
| 192.168.100.45 | 总测工位2 | 1600 | 舵面×5+单片机+地测口+无人机 |
| 192.168.100.46 | 动力检测 | 1600 | 总测+噪音传感器 |
| 192.168.100.47 | 总测工位1 | 1500 | 舵面×5+单片机+地测口+无人机 |
| 192.168.100.48 | 动力检测 | 1500 | 总测+噪音传感器 |
| 192.168.100.49 | 拷机工位2 | 1300 | 舵面×5+单片机+地测口+无人机+导引头 |
| 192.168.100.50 | 拷机工位1 | 1200 | 舵面×5+单片机+地测口+无人机+导引头 |
| 192.168.100.51 | 桁架 | 1100 | 舵面×5+单片机+地测口+无人机+导引头 |
| 192.168.100.52 | 上料台 | - | - |

### 端口映射
| 端口 | 设备类型 |
|------|---------|
| 10001-10005 | 舵面传感器 |
| 10006 | 单片机（治具） |
| 10007 | 地测口（无人机） |
| 1234 | 扫描枪 |
| 21000 | 噪音传感器 |
| 502 | PLC服务器 |
| 20000 | DGIOT服务器 |

## 测试用例

### 基础测试
```bash
# 简化测试流程
python3 integrated_production_line.py --test-case normal_flow --verify

# 拷机测试
python3 integrated_production_line.py --test-case 拷机测试 --verify

# 全工位循环
python3 integrated_production_line.py --test-case 全工位循环 --verify
```

### 完整产线测试
```bash
# 产线A路径1
python3 integrated_production_line.py --test-case complete_production_line_a1 --verify

# 产线A路径2
python3 integrated_production_line.py --test-case complete_production_line_a2 --verify

# 产线B路径1
python3 integrated_production_line.py --test-case complete_production_line_b1 --verify

# 产线B路径2
python3 integrated_production_line.py --test-case complete_production_line_b2 --verify

# 所有产线（4条路径）
python3 integrated_production_line.py --test-case all_production_lines --verify
```

### 特定工位测试
```bash
# 磁航向工位
python3 integrated_production_line.py --test-case magnetic_station_only --verify

# 动力检测+噪音
python3 integrated_production_line.py --test-case power_test_with_noise --verify

# 拷机+导引头
python3 integrated_production_line.py --test-case bake_with_guidance_head --verify
```

### 循环测试
```bash
# 执行3次循环
python3 integrated_production_line.py --test-case all_production_lines --cycles 3 --verify
```

## 参数说明

| 参数 | 说明 | 默认值 |
|------|------|--------|
| `--test-case` | 测试用例名称 | normal_flow |
| `--cycles` | 循环次数 | 1 |
| `--verify` | 执行闭环验证 | False |
| `--save-report` | 保存测试报告 | False |
| `--enable-mes` | 启用MES模拟器 | False |
| `--dgiot-host` | DGIOT服务器地址 | 0.0.0.0 |
| `--dgiot-port` | DGIOT服务器端口 | 20000 |

## 闭环验证

测试完成后会自动验证：

1. **工位状态检查** - 验证工位管理器
2. **设备监控器检查** - 验证监控功能

验证结果在日志中以 `[CLOSED_LOOP]` 标记。

## MES集成

启用MES模拟器后会自动：

1. **启动MES服务器** - 监听端口801（本地）
2. **接收测试结果** - 自动上报测试数据
3. **更新产线状态** - 实时同步产线状态
4. **统计数据查询** - 支持测试结果统计

### MES架构

```
Erlang后端 → Nginx代理(80) → MES模拟器(801)
```

### MES API端点

**通过Nginx代理访问（推荐）**：
```
POST http://<nginx-ip>/lezao/jymes/api/equip/proExec - 上报测试结果
GET  http://<nginx-ip>/mes/health                       - 健康检查
```

**直接访问（本地）**：
```
POST http://127.0.0.1:801/api/test/result     - 上报测试结果
POST http://127.0.0.1:801/api/line/status     - 更新产线状态
GET  http://127.0.0.1:801/api/test/results    - 查询测试结果
GET  http://127.0.0.1:801/api/line/status     - 查询产线状态
GET  http://127.0.0.1:801/api/statistics      - 查询统计数据
GET  http://127.0.0.1:801/health              - 健康检查
```

### 使用MES

#### 方式1：独立启动MES

```bash
bash start_mes.sh
```

#### 方式2：集成启动（推荐）

```bash
python3 integrated_production_line.py --enable-mes --verify
```

### Erlang后端配置

Erlang代码中的MES配置（`dgiot_uav_mes_api.erl:53`）：
```erlang
api_url => application:get_env(dgiot_uav, mes_api_url,
    "http://172.1.2.222/lezao/jymes/api/equip/proExec"),
```

**建议修改为通过nginx代理访问**：
```erlang
"http://127.0.0.1:80/lezao/jymes/api/equip/proExec"
```

详见：`MES_PROXY_CONFIG.md`

## 测试报告

使用 `--save-report` 保存JSON格式测试报告，包含：
- 测试ID和时间
- 测试步骤
- 验证结果（通过/失败）
- 错误和警告列表

## 产线流程

```
磁航向 → 总测 → 动力检测 → 拷机 → 桁架
```

**4条完整路径**：
- A线路径1：磁航向 → 总测2 → 动力检测2 → 拷机2 → 桁架
- A线路径2：磁航向 → 总测1 → 动力检测1 → 拷机1 → 桁架
- B线路径1：磁航向 → 总测2 → 动力检测2 → 拷机2 → 桁架
- B线路径2：磁航向 → 总测1 → 动力检测1 → 拷机1 → 桁架

## 注意事项

1. 确保DGIOT服务器已启动（端口20000）
2. 脚本会自动绑定必要的IP地址
3. PLC模拟器会自动启动在127.0.0.1:502
4. 测试完成后会自动清理模拟器进程

---

**最后更新**: 2026-03-25
**版本**: 2.0
