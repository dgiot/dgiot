# Modbus集成测试框架完整说明

## 概述

本框架为DG-IoT Modbus插件提供了完整的集成测试解决方案，支持两种协议模式：
1. **Modbus RTU over TCP Server**：作为服务器监听，支持三种注册方式
2. **Modbus TCP Client**：作为客户端访问其他Modbus TCP服务器

## 架构设计

### 1. 测试框架结构
```
test/integration/
├── modules/                          # 功能模块
│   ├── test_environment_manager.sh   # 测试环境管理器
│   ├── log_analysis.sh              # 日志分析模块
│   ├── device_registration.sh       # 设备注册模块
│   ├── data_report.sh               # 数据上报模块
│   ├── api_verification.sh          # API验证模块
│   └── temporal_analysis.sh         # 时空对应分析模块
├── simulators/                       # 模拟器
│   ├── modbus_rtu_client_simulator.py   # RTU Client模拟器
│   └── modbus_tcp_server_simulator.py   # TCP Server模拟器
├── test_cases/                       # 测试用例
│   └── test_device_registration.sh  # 设备注册测试用例
├── run_integration_test.sh          # 主测试脚本
├── run_all_test_cases.sh            # TDD测试运行器
├── TEST_CASE_SPECIFICATION.md       # 测试用例规范
└── MODBUS_INTEGRATION_TEST_FRAMEWORK.md  # 本文档
```

### 2. 支持的协议模式

#### 2.1 Modbus RTU over TCP Server
- **工作模式**：TCP Server监听模式
- **注册方式**：
  - `RegisterByIp`：IP地址注册
  - `RegisterByPort`：端口注册
  - `RegisterByRegular`：正则匹配注册
- **模拟器**：Modbus RTU Client模拟器

#### 2.2 Modbus TCP Client
- **工作模式**：TCP Client主动连接模式
- **功能**：访问外部Modbus TCP服务器
- **模拟器**：Modbus TCP Server模拟器

## 核心功能

### 1. 测试环境管理
- **自动环境设置**：一键创建通道、产品、设备
- **模拟器管理**：自动启动/停止RTU Client和TCP Server模拟器
- **环境清理**：自动清理测试数据

### 2. 测试驱动开发（TDD）
- **测试用例规范**：定义6大类测试用例
- **具体测试用例**：设备注册、数据上报、API验证等
- **TDD运行器**：统一运行所有测试用例

### 3. 精准时空对应分析
- **时间精准**：纳秒级时间戳分析
- **空间精准**：设备标识、代码位置追踪
- **时空对应**：设备日志时间线分析

### 4. 完整测试循环
```
发包 → 读日志 → 检查 → 修改 → 热加载 → 再发包
```

## 使用指南

### 1. 快速开始

```bash
# 1. 设置完整的测试环境
cd /root/gitee/dgiot/apps/dgiot_modbus
make test-env-setup

# 2. 运行完整的Modbus协议测试
make test-modbus-full

# 3. 清理测试环境
make test-env-cleanup
```

### 2. 常用命令

#### 2.1 测试环境管理
```bash
# 设置测试环境
make test-env-setup

# 检查环境状态
make test-env-check

# 清理测试环境
make test-env-cleanup

# 启动模拟器
make test-simulator-start

# 停止模拟器
make test-simulator-stop
```

#### 2.2 测试驱动开发
```bash
# 运行所有测试用例
make test-tdd-all

# 显示测试用例列表
make test-tdd-list

# 运行设备注册测试用例
make test-tdd-device-reg

# 查看测试用例规范
make test-tdd-spec
```

#### 2.3 集成测试
```bash
# 运行完整集成测试循环
make test-integration-full

# 运行设备注册测试
make test-integration-device-reg

# 运行数据上报测试
make test-integration-data-report

# 运行日志分析测试
make test-integration-log-analysis

# 运行热编译测试
make test-integration-hot-reload
```

### 3. 模拟器使用

#### 3.1 Modbus RTU Client模拟器
```bash
# 手动启动模拟器
cd /root/gitee/dgiot/apps/dgiot_modbus/test/integration/simulators
python3 modbus_rtu_client_simulator.py \
  --host 127.0.0.1 \
  --port 20000 \
  --device test_device_01 \
  --regtype RegisterByPort \
  --interval 10
```

#### 3.2 Modbus TCP Server模拟器
```bash
# 手动启动模拟器
cd /root/gitee/dgiot/apps/dgiot_modbus/test/integration/simulators
python3 modbus_tcp_server_simulator.py \
  --host 0.0.0.0 \
  --port 502
```

## 测试用例规范

### 1. 测试用例分类
1. **环境检查测试用例**：检查系统服务、端口监听等
2. **设备注册测试用例**：测试三种注册方式
3. **Modbus数据上报测试用例**：测试数据解析和存储
4. **数据存储测试用例**：验证TDengine数据存储
5. **API查询测试用例**：验证API接口功能
6. **时空对应分析测试用例**：验证日志和数据的时空对应关系

### 2. 测试用例结构
每个测试用例包含：
- **前置条件**：测试环境要求
- **测试步骤**：具体的操作步骤
- **预期结果**：期望的输出结果
- **日志规范**：要求的日志格式
- **时空对应**：时间和空间的对应关系

## 开发流程

### 1. 新功能开发（TDD流程）
```bash
# 1. 设计测试用例
# 2. 编写测试脚本
# 3. 运行测试（应该失败）
# 4. 实现功能代码
# 5. 运行测试（应该通过）
# 6. 重构优化
```

### 2. 问题修复流程
```bash
# 1. 复现问题的测试用例
# 2. 编写重现脚本
# 3. 分析失败原因
# 4. 修复代码
# 5. 验证修复
```

### 3. 回归测试流程
```bash
# 运行所有测试用例
make test-tdd-all
```

## 技术特点

### 1. 模块化设计
- 6个独立的功能模块
- 可单独测试每个模块
- 支持模块组合测试

### 2. 自动化程度高
- 自动环境设置
- 自动模拟器管理
- 自动测试报告生成

### 3. 精准问题定位
- 纳秒级时间戳分析
- 设备标识追踪
- 代码位置定位

### 4. 团队协作友好
- 统一的测试用例规范
- 清晰的日志格式要求
- 可重复的测试流程

## 配置说明

### 1. 端口配置
```bash
# Modbus RTU over TCP Server端口
MODBUS_RTU_PORT=20000

# Modbus TCP Server端口（模拟器）
MODBUS_TCP_PORT=502

# Modbus TCP Client连接端口
MODBUS_TCP_CLIENT_PORT=503
```

### 2. 测试产品配置
```bash
# 测试产品ID
TEST_PRODUCT_ID="feeb43bffb"

# 测试产品名称
TEST_PRODUCT_NAME="Modbus测试产品"

# 测试通道ID
TEST_CHANNEL_ID="modbus_test_channel"

# 测试设备前缀
TEST_DEVICE_PREFIX="test_modbus_device"
```

## 故障排除

### 1. 常见问题

#### 问题1：模拟器无法启动
**原因**：Python未安装或端口被占用
**解决方案**：
```bash
# 检查Python安装
python3 --version

# 检查端口占用
netstat -tlnp | grep :20000
netstat -tlnp | grep :502
```

#### 问题2：测试环境设置失败
**原因**：DG-IoT平台未运行或API不可用
**解决方案**：
```bash
# 检查平台运行状态
make run

# 检查API可用性
curl -X GET "http://127.0.0.1/iotapi/health"
```

#### 问题3：测试用例执行失败
**原因**：测试数据不匹配或环境不一致
**解决方案**：
```bash
# 清理测试环境
make test-env-cleanup

# 重新设置测试环境
make test-env-setup

# 重新运行测试
make test-tdd-all
```

### 2. 调试技巧

#### 查看详细日志
```bash
# 查看实时日志
tail -f _build/emqx/rel/emqx/log/emqx.log.1

# 查看特定模块日志
tail -f _build/emqx/rel/emqx/log/emqx.log.1 | grep -E "(dgiot_modbus|modbus_rtu)"
```

#### 手动测试API
```bash
# 测试设备查询API
curl -X GET "http://127.0.0.1/iotapi/devicecard/test_device_01-20000" \
  -H "Authorization: Bearer r:db1f3d43d05c782c8ceebb87724a2ac0"
```

## 更新记录

### v1.0 (2025-12-26)
- 初始版本发布
- 支持两种Modbus协议模式
- 完整的测试环境管理
- 测试驱动开发框架
- 精准时空对应分析

### 未来计划
1. **性能测试模块**：添加性能基准测试
2. **压力测试模块**：支持高并发测试
3. **可视化报告**：生成HTML测试报告
4. **CI/CD集成**：集成到持续集成流程

## 总结

本框架为DG-IoT Modbus插件提供了：
1. **完整的测试覆盖**：支持两种协议模式，三种注册方式
2. **高效的开发流程**：测试驱动开发，快速迭代
3. **精准的问题定位**：时空对应分析，快速定位问题
4. **团队协作支持**：统一的测试规范，清晰的日志要求

通过使用本框架，团队可以实现：
- 更快的功能开发和验证
- 更准确的问题定位和修复
- 更可靠的代码质量和稳定性
- 更高效的团队协作和知识共享
