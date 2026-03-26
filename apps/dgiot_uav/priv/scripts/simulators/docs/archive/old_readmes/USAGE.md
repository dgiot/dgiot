# 产线模拟器使用说明

## 目录结构

```
simulators/
├── integrated_production_line.py   # 主脚本（产线调度器）
├── fixture_simulator.py             # 治具模拟器
├── plc_simulator.py                 # PLC模拟器
├── uav_simulator.py                 # 无人机模拟器
├── mes_simulator.py                 # MES模拟器
├── multicast_core.py                # 多播通信核心
├── quick_test.sh                    # 快速启动脚本
├── README.md                        # 完整说明文档
└── __pycache__/                     # Python缓存（自动生成）
```

## 使用方法

### 方式1：快速启动（推荐）

```bash
./quick_test.sh
```

交互式菜单选择测试场景，自动执行并显示结果。

### 方式2：命令行启动

```bash
# 基础测试
python3 integrated_production_line.py --verify --save-report

# 指定测试用例
python3 integrated_production_line.py --test-case all_production_lines --verify

# 循环测试
python3 integrated_production_line.py --test-case normal_flow --cycles 3 --verify
```

### 方式3：后台运行

```bash
nohup python3 integrated_production_line.py --test-case all_production_lines --verify --save-report > test.log 2>&1 &
```

## 测试场景

### 1. 简化测试流程 (normal_flow)
```
上料台 → 磁航向 → 总测工位2 → 总测工位1
```
用时：约30秒

### 2. 拷机测试
```
上料台 → 拷机工位1 → 拷机工位2
```
用时：约20秒

### 3. 全工位循环
```
所有工位按顺序执行
```
用时：约2分钟

### 4. 完整产线测试 (complete_production_line_a1/a2/b1/b2)
```
磁航向 → 总测 → 动力检测 → 拷机 → 桁架
```
用时：约1分钟

### 5. 所有产线 (all_production_lines)
```
4条完整路径依次执行
```
用时：约4分钟

### 6. 特定工位测试
- `magnetic_station_only` - 磁航向工位
- `power_test_with_noise` - 动力检测+噪音
- `bake_with_guidance_head` - 拷机+导引头

## 输出说明

### 日志标记

| 标记 | 说明 |
|------|------|
| `[TEST_RESULT]` | 测试结果（JSON格式） |
| `[CLOSED_LOOP]` | 闭环验证结果 |
| `[PLC]` | PLC通信 |
| `[报文]` | 报文发送/接收 |

### 测试报告

测试完成后生成JSON格式报告：

```json
{
  "test_id": "test_20260325_082100",
  "verification": {
    "total": 2,
    "passed": 2,
    "failed": 0
  }
}
```

报告文件：`test_report_YYYYMMDD_HHMMSS.json`

## 常见问题

### 1. DGIOT服务器未运行
```
错误: 连接拒绝
解决: 启动DGIOT: make run
```

### 2. IP绑定失败
```
错误: IP绑定失败
解决: 检查网络接口: ip addr show eth0
```

### 3. 端口被占用
```
错误: Address already in use
解决: 检查端口占用: netstat -tunlp | grep 502
```

### 4. 验证失败
```
[CLOSED_LOOP] ✗ 验证失败
解决: 检查Erlang后端是否正常运行
```

## 技术细节

##***REMOVED***地址（固化配置）
- 工控机：192.168.100.100
- PLC服务器：192.168.100.40:502
- 磁航向PLC：192.168.100.20:502

### 端口（固化配置）
- DGIOT：20000
- PLC：502
- 舵面传感器：10001-10005
- 单片机：10006
- 地测口：10007
- 扫描枪：1234
- 噪音传感器：21000

### 工位地址（固化配置）
| IP | 地址 | 名称 |
|----|------|------|
| 192.168.100.21 | 1700 | 磁航向 |
| 192.168.100.45 | 1600 | 总测2 |
| 192.168.100.47 | 1500 | 总测1 |
| 192.168.100.49 | 1300 | 拷机2 |
| 192.168.100.50 | 1200 | 拷机1 |
| 192.168.100.51 | 1100 | 桁架 |
| 192.168.100.52 | 255 | 上料台 |

---

**版本**: 2.0
**最后更新**: 2026-03-25
