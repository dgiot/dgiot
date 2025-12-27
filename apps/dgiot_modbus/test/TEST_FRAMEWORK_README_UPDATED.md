# 插件测试框架使用说明（更新版）

## 概述

本测试框架提供了一个统一的方式来管理和执行DG-IoT平台中各个插件的测试用例。框架支持通过Makefile命令或直接脚本执行测试。

## 文件结构

```
/root/gitee/dgiot/
├── .testcases                    # 测试用例注册文件
├── scripts/                      # 脚本目录
│   ├── test_framework.sh         # 测试框架主脚本
│   ├── refresh_cline_rules.sh    # Cline规则刷新脚本
│   ├── switch_cline_rules.sh     # Cline规则切换脚本
│   └── test_cline_config.sh      # Cline配置测试脚本
├── Makefile                      # 包含测试命令
└── TEST_FRAMEWORK_README.md      # 使用说明文档
```

## 使用方法

### 1. 通过Makefile使用（推荐）

```bash
# 查看帮助
make test-help

# 列出所有插件
make list-plugins

# 列出指定插件的测试用例
make list-testcases PLUGIN=dgiot_modbus

# 执行单个测试用例
make test-plugin PLUGIN=dgiot_modbus TESTCASE=simple

# 执行插件的所有测试用例
make test-plugin-all PLUGIN=dgiot_modbus

# 快速测试命令
make test-modbus              # 测试所有Modbus用例
make test-modbus-simple       # 测试Modbus简化用例
make test-modbus-register     # 测试Modbus注册用例
make test-modbus-simulator    # 测试Modbus模拟器
```

### 2. 直接使用脚本

```bash
# 查看帮助
./scripts/test_framework.sh --help

# 列出所有插件
./scripts/test_framework.sh --list-plugins

# 列出指定插件的测试用例
./scripts/test_framework.sh --list dgiot_modbus

# 执行单个测试用例
./scripts/test_framework.sh --run dgiot_modbus simple

# 执行所有测试用例
./scripts/test_framework.sh --all dgiot_modbus

# 快速命令
./scripts/test_framework.sh modbus              # 测试所有Modbus用例
./scripts/test_framework.sh modbus simple       # 测试Modbus简化用例
./scripts/test_framework.sh modbus register     # 测试Modbus注册用例
```

## 测试用例注册

测试用例在`.testcases`文件中注册，格式如下：

```
插件名:测试用例名:测试脚本路径:描述
```

### 当前注册的测试用例（dgiot_modbus插件） - 共16个

#### 核心功能测试：
1. **simple** - Modbus RTU简化测试
2. **registerbyport** - RegisterByPort注册测试
3. **hex_data** - 十六进制数据测试
4. **error_handling** - Modbus错误处理测试

#### 环境与配置测试：
5. **env_check** - 环境检查测试
6. **config_management** - 配置管理测试
7. **hot_reload** - 插件热重载测试

#### 数据验证测试：
8. **database_report** - 数据库报告测试
9. **data_report** - 数据上报测试

#### 模拟器测试：
10. **simulator_python** - Python模拟器测试
11. **simulator_complete** - 完整模拟器测试

#### API测试：
12. **api_auth** - API认证测试
13. **api_query** - API查询测试

#### 分析与监控测试：
14. **log_analysis** - 日志分析测试

#### 集成测试：
15. **integration** - 集成测试
16. **all** - 所有测试用例

## 测试用例覆盖分析

### ✅ 已全面覆盖Modbus插件的主要测试需求：

1. **设备连接管理** - registerbyport, env_check
2. **协议解析功能** - simple, hex_data, error_handling
3. **数据持久化** - database_report, data_report
4. **配置管理** - config_management
5. **热更新能力** - hot_reload
6. **API接口** - api_auth, api_query
7. **日志与监控** - log_analysis
8. **端到端流程** - integration, all
9. **测试工具** - simulator_python, simulator_complete

### 🎯 测试重点：
- **设备注册和连接**：确保设备能正确连接到平台
- **数据解析和存储**：确保Modbus数据正确解析并存储到数据库
- **API接口**：确保前后端数据交互正常
- **错误处理**：确保系统能正确处理各种异常情况
- **热更新**：确保插件能在线更新而不中断服务

## 添加新测试用例

### 步骤1：创建测试脚本

在适当的目录中创建测试脚本（Shell或Python）。

### 步骤2：注册测试用例

在`.testcases`文件中添加一行：

```
dgiot_插件名:测试用例名:脚本路径:描述
```

### 步骤3：验证注册

```bash
make list-testcases PLUGIN=dgiot_插件名
```

## 框架特点

1. **统一管理**：所有测试用例集中注册，便于维护
2. **易于扩展**：添加新插件只需在`.testcases`文件中添加一行
3. **灵活执行**：支持单个用例执行和批量执行
4. **错误处理**：完善的错误检查和用户提示
5. **兼容性**：支持Shell脚本和Python脚本
6. **用户友好**：彩色输出、详细帮助、快速命令

## 注意事项

1. 测试脚本必须具有可执行权限
2. 脚本路径可以是相对路径或绝对路径
3. 如果测试脚本不存在，框架会显示错误信息
4. 测试执行失败不会中断其他测试的执行

## 更新记录

- 2025-12-26：创建插件测试框架
  - 移动相关脚本到scripts目录
  - 更新Makefile引用
  - 创建完整的使用文档
- 2025-12-26：完善Modbus测试用例
  - 添加6个新的测试用例
  - 全面覆盖Modbus插件的主要测试需求
  - 更新使用说明文档
