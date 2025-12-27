# Modbus插件集成测试工具

## 概述

本目录包含 `dgiot_modbus` 插件的集成测试工具和脚本，用于测试完整的Modbus RTU数据处理流程。**已融合模拟器和集成测试，提供统一的测试框架**。

## 目录结构（清理后）

```
integration/
├── README.md                    # 本文档
├── modbus_integration_test_framework.sh  # 主脚本：统一的集成测试框架
├── simulators/                  # 模拟器脚本
│   └── simulate_rtu_device_complete.py  # 完整的RTU模拟器（保留）
├── test_runners/                # 测试运行脚本（核心功能）
│   ├── integration_test_registerbyport_enhanced.sh  # 增强的注册测试
│   ├── test_modbus_env_check.sh   # 环境检查测试
│   ├── test_modbus_hex_data.sh    # 数据测试
│   └── test_modbus_rtu_database_report.sh # RTU客户端数据库上报测试
├── analysis/                    # 分析工具
│   ├── analyze_modbus_flow.sh     # 数据流分析
│   └── debug_self_closed.sh       # 自闭环调试
└── INTEGRATION_TEST_WORKFLOW.md   # 集成测试工作流程规范
```

## 核心脚本说明

### 主脚本：`modbus_integration_test_framework.sh`
**功能**：统一的集成测试框架，融合模拟器和集成测试
**设计理念**：
- **主脚本**：提供统一的测试入口
- **二级功能**：各个业务功能模块
- **模块化设计**：每个模块独立可测试

**二级功能模块**：
1. `--env-check`：环境检查
2. `--device-reg`：设备注册测试
3. `--data-report`：Modbus数据上报测试
4. `--api-query`：API查询测试
5. `--log-analysis`：日志分析
6. `--data-storage`：数据存储验证
7. `--hot-reload`：热编译和热加载
8. `--all`：执行完整测试流程

**使用方法**：
```bash
# 执行完整测试流程
bash modbus_integration_test_framework.sh --all

# 只检查环境
bash modbus_integration_test_framework.sh --env-check

# 只测试设备注册
bash modbus_integration_test_framework.sh --device-reg

# 只测试数据上报
bash modbus_integration_test_framework.sh --data-report

# 显示帮助
bash modbus_integration_test_framework.sh --help
```

### 模拟器脚本 (simulators/)

#### `simulate_rtu_device_complete.py`
**功能**：完整的RTU模拟器，模拟Modbus RTU设备
**用途**：
- 发送注册报文（ASCII字符串）
- 发送数据报文（HEX格式）
- 查询API实时值
- 查询设备属性

**使用方法**：
```bash
cd /root/gitee/dgiot
python3 apps/dgiot_modbus/test/tools/integration/simulators/simulate_rtu_device_complete.py
```

### 测试运行脚本 (test_runners/)

#### `integration_test_registerbyport_enhanced.sh`
**功能**：增强的RegisterByPort注册测试
**用途**：完整的设备注册和数据块主动上报集成测试

#### `test_modbus_env_check.sh`
**功能**：环境检查测试
**用途**：检查系统状态、端口监听、插件加载

#### `test_modbus_hex_data.sh`
**功能**：Modbus数据测试
**用途**：发送和验证Modbus HEX数据

#### `test_modbus_rtu_database_report.sh`
**功能**：Modbus RTU客户端模拟数据库上报测试
**用途**：
- 模拟RTU客户端发送注册报文和业务报文
- 验证数据解析和上报流程
- 生成详细的测试报告
- 作为系统防护底线的关键测试用例

### 分析工具 (analysis/)

#### `analyze_modbus_flow.sh`
**功能**：Modbus数据流分析
**用途**：分析Modbus数据处理流程中的各个环节

#### `debug_self_closed.sh`
**功能**：自闭环调试报告生成
**用途**：生成详细的调试报告

## 测试流程

### 使用统一框架的测试流程
1. **环境检查**：`bash modbus_integration_test_framework.sh --env-check`
2. **设备注册**：`bash modbus_integration_test_framework.sh --device-reg`
3. **数据上报**：`bash modbus_integration_test_framework.sh --data-report`
4. **API查询**：`bash modbus_integration_test_framework.sh --api-query`
5. **日志分析**：`bash modbus_integration_test_framework.sh --log-analysis`
6. **数据存储验证**：`bash modbus_integration_test_framework.sh --data-storage`
7. **热编译验证**：`bash modbus_integration_test_framework.sh --hot-reload`

### 完整测试流程（一键执行）
```bash
# 执行完整测试流程
bash modbus_integration_test_framework.sh --all

# 或使用传统脚本
bash integration_test_registerbyport_enhanced.sh
```

### 快速测试（仅核心功能）
```bash
# 1. 启动EMQ X服务
make run

# 2. 运行核心测试
bash test_modbus_env_check.sh
bash test_modbus_rtu_database_report.sh

# 3. 使用Python模拟器
python3 simulators/simulate_rtu_device_complete.py
```

## 融合设计理念

### 为什么需要融合？
1. **模拟器和集成测试是同一件事情**：模拟一条DTU与dgiot后台建立连接，发送注册报文，然后发送各种业务报文
2. **避免重复**：减少脚本数量，提高维护性
3. **统一接口**：提供一致的测试体验
4. **模块化设计**：每个业务功能模块独立可测试

### 融合后的优势
1. **主脚本统一入口**：`modbus_integration_test_framework.sh`
2. **二级功能模块化**：7个业务功能模块
3. **灵活组合**：可以单独测试某个模块，也可以执行完整流程
4. **易于扩展**：新增业务功能只需添加新模块

## 依赖关系

### Python依赖
```bash
pip3 install requests
```

### 系统依赖
- Python 3.x
- curl
- grep, tail, awk 等标准Linux工具

## 配置说明

### 产品配置
- 产品ID：`feeb43bffb`
- 通道端口：`20000`

### 测试数据
- 注册报文：ASCII字符串 `"wrj_dm-zqy"`
- 数据报文：95字节的HEX数据

## 常见问题

### 1. API返回401错误
**原因**：认证Cookie过期或无效
**解决方案**：
- 更新脚本中的Cookie信息
- 使用有效的认证令牌

### 2. 设备地址为空
**原因**：RegisterByPort注册方式未正确解析端口信息
**解决方案**：
- 检查 `dgiot_modbusrtu_tcp.erl` 中的注册逻辑
- 确保Env中包含端口信息

### 3. 数据未解析
**原因**：协议解析器未正确加载
**解决方案**：
```bash
# 重新编译加载modbus插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_modbus).'
```

## 维护说明

### 添加新脚本
1. 根据功能将脚本放入对应的子目录
2. 更新本README文档
3. 确保脚本有适当的注释和文档

### 更新脚本
1. 测试脚本功能是否正常
2. 更新相关文档
3. 验证与其他脚本的兼容性

## 联系信息

如有问题，请联系插件维护团队。

---
*最后更新：2025年12月26日*
