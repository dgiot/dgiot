# 磁航向工位快速参考卡片

## 🚀 一键命令

```bash
# 最简单的方式（推荐）
./run_magnetic_station_test.sh

# 自动绑定IP并测试
./run_magnetic_station_test.sh --auto-bind

# 指定设备ID
./run_magnetic_station_test.sh --device-id UAV-002

# 查看测试结果
./run_magnetic_station_test.sh --show-results

# 验证设备状态
./run_magnetic_station_test.sh --verify

# 详细日志
./run_magnetic_station_test.sh --verbose
```

## 📋 工位配置速查

| 项目 | 值 |
|------|-----|
| 工位ID | 1700 |
| 工位名称 | 磁航向校准工位 |
| 业务类型 | 扫码绑定 |

## 🌐 IP配置速查

| 设备 | IP地址 | 端口 |
|------|--------|------|
| PLC | 192.168.100.20 | 502 |
| 地测口 | 192.168.100.21 | 10007 |
| 扫码枪 | 192.168.100.23 | 1234 |
| DG-IoT | 192.168.100.100 | 20000 |

## 📝 测试步骤速查

```
阶段一: 环境准备
  1. 启动DG-IoT服务器
  2. 检查IP绑定状态
  3. 绑定IP（如需要）

阶段二: 测试执行
  场景1: 扫码绑定设备
  场景2: PLC七步校验
  场景3: 无人机指令下发
  场景4: 持续发送遥测数据
  场景5: 测试结果汇聚

阶段三: 结果验证
  1. 查看测试日志
  2. 查看报文日志
  3. 验证设备状态
```

## 🔧 PLC七步校验速查

| 步骤 | 操作 | 地址 | 值 |
|------|------|------|-----|
| 1/7 | READ | D1700 | - |
| 2/7 | WRITE | D1751 | 100 |
| 3/7 | READ | D1710 | - |
| 4/7 | WRITE | D1700 | 0 |
| 5/7 | WRITE | D1710 | 0 |
| 6/7 | WRITE | D1760 | 100 |
| 7/7 | WRITE | D1761 | 1 |

## 📊 EB90指令速查

| 指令名称 | 命令码 | 载荷 |
|----------|--------|------|
| 舵面中位 | F0FB | A55AF0FB |
| 舵面使能 | F0F3 | A55AF0F3 |
| 复飞 | F0B9 | A55AF0B9 |

## 🔍 快速诊断命令

```bash
# 检查IP绑定状态
ip addr show eth0 | grep "192.168.100"

# 检查DG-IoT服务器
nc -zv 192.168.100.100 20000

# 检查DG-IoT设备状态
_build/emqx/rel/emqx/bin/emqx eval 'ets:tab2list(uav_ip_station_mapping).'

# 查询工位信息
_build/emqx/rel/emqx/bin/emqx eval '
  dgiot_uav_business_service:get_station_by_ip(<<"192.168.100.21">>).'

# 查看DG-IoT日志
tail -f _build/emqx/rel/emqx/log/console.log | grep -E "(1700|磁航向|magnetic)"

# 查看最新测试日志
ls -lt test_records/station_1700/test_*.log | head -1
cat $(ls -t test_records/station_1700/test_*.log | head -1)
```

## 📁 日志位置

```
test_records/station_1700/
├── test_YYYYMMDD_HHMMSS.log       # 测试日志
└── packets/
    └── packets_YYYYMMDD_HHMMSS.log  # 报文日志
```

## ⚠️ 常见问题速查

| 问题 | 解决方案 |
|------|----------|
| DG-IoT未运行 | `cd /root/gitee/dgiot && make run` |
| IP未绑定 | `./run_magnetic_station_test.sh --auto-bind` |
| 端口被占用 | `sudo lsof -i :1801` 然后 `sudo kill -9 <PID>` |
| 权限不足 | 使用 `sudo` 执行脚本 |

## 📚 文档导航

| 文档 | 用途 |
|------|------|
| README_MAGNETIC_STATION.md | 项目概述和快速开始 |
| MAGNETIC_STATION_TEST_GUIDE.md | 完整调测指南 |
| 磁航向工位调测智能体.md | 智能体详细文档 |
| MAGNETIC_STATION_TEST_STEPS.md | 测试步骤详解 |
| 磁航向工位快速参考.md | 本文档 |

## 🎯 成功标准

- ✅ 所有6个测试项通过
- ✅ 通过率100%
- ✅ 无错误日志
- ✅ 设备成功绑定到工位
- ✅ PLC七步校验成功
- ✅ 所有EB90指令发送成功

## 💡 提示

1. **优先使用一键脚本** - `run_magnetic_station_test.sh` 集成了所有功能
2. **查看详细日志** - 使用 `--verbose` 参数查看详细信息
3. **自动绑定IP** - 使用 `--auto-bind` 参数自动绑定IP
4. **指定设备ID** - 使用 `--device-id` 参数指定测试设备
5. **查看结果** - 使用 `--show-results` 快速查看测试结果

---

**版本**: v1.0  
**日期**: 2026-03-26
