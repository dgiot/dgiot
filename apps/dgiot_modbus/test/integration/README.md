# Modbus集成测试框架 v2.0

## 概述

Modbus集成测试框架是一个完整的测试解决方案，支持从设备注册、数据上报到前端展示的完整工作流测试。框架采用模块化设计，支持完整的测试循环：发包 → 读日志 → 检查 → 修改 → 热加载 → 再发包。

## 架构设计

### 目录结构
```
test/integration/
├── run_integration_test.sh          # 主测试脚本
├── README.md                        # 本文档
├── modules/                         # 模块目录
│   ├── environment_check.sh         # 环境检查模块
│   ├── device_registration.sh       # 设备注册模块
│   ├── data_report.sh               # 数据上报模块
│   ├── log_analysis.sh              # 日志分析模块
│   ├── api_query.sh                 # API查询模块
│   ├── hot_reload.sh                # 热编译模块
│   └── common.sh                    # 公共函数模块
└── Makefile.test                    # Makefile集成
```

### 模块功能

1. **环境检查模块** (`environment_check.sh`)
   - 检查系统服务状态
   - 验证端口监听
   - 检查插件加载状态

2. **设备注册模块** (`device_registration.sh`)
   - 发送设备注册报文
   - 验证设备注册成功
   - 检查设备数据库记录

3. **数据上报模块** (`data_report.sh`)
   - 发送Modbus RTU数据包
   - 验证数据解析正确性
   - 检查缓存数据格式

4. **日志分析模块** (`log_analysis.sh`)
   - 实时监控系统日志
   - 分析关键日志信息
   - 诊断问题原因

5. **API查询模块** (`api_query.sh`)
   - 查询设备实时数据API
   - 验证API响应格式
   - 检查数据存储状态

6. **热编译模块** (`hot_reload.sh`)
   - 编译modbus插件
   - 热加载插件更新
   - 系统状态诊断

## 使用方式

### 快速开始

```bash
# 进入modbus插件目录
cd apps/dgiot_modbus

# 运行完整集成测试
make -f Makefile.test test-integration

# 或直接运行测试脚本
./test/integration/run_integration_test.sh
```

### 测试配置

测试框架使用以下默认配置：
- **测试设备**: `wrj_dm-zqy`
- **测试端口**: `20000`
- **产品ID**: `feeb43bffb`
- **设备地址**: `wrj_dm-zqy-20000`

可以通过环境变量覆盖默认配置：
```bash
export TEST_DEVICE="my_device"
export TEST_PORT=30000
export TEST_PRODUCT="my_product"
./test/integration/run_integration_test.sh
```

## 测试循环

框架支持完整的测试循环：

1. **环境准备**
   - 清理测试环境
   - 检查系统状态
   - 验证插件加载

2. **设备注册测试**
   - 发送注册报文
   - 验证设备创建
   - 检查数据库记录

3. **数据上报测试**
   - 发送Modbus数据
   - 验证数据解析
   - 检查缓存更新

4. **API验证测试**
   - 查询实时数据
   - 验证API响应
   - 检查数据存储

5. **问题诊断**
   - 分析系统日志
   - 诊断问题原因
   - 提供修复建议

6. **热编译循环**
   - 编译代码修改
   - 热加载插件
   - 重新测试验证

## 编码规范

### 中文编码处理
根据`.clinerules/coding_standards.md`规范，所有Erlang代码中的中文输出必须使用`dgiot_utils:safe_format`函数或避免使用中文。测试脚本中的Shell脚本部分可以使用中文。

### 错误处理
- 使用统一的日志函数：`log_info`, `log_success`, `log_error`, `log_warning`
- 所有模块函数返回0表示成功，非0表示失败
- 详细的错误信息和诊断建议

### 模块化设计
- 每个模块独立，可单独测试
- 模块间通过标准接口通信
- 支持模块组合和自定义测试流程

## 最佳实践

### 1. 测试环境准备
```bash
# 确保系统正常运行
make run

# 检查端口监听
netstat -tlnp | grep :20000
```

### 2. 问题诊断流程
1. 运行完整测试循环
2. 查看失败模块的详细输出
3. 分析系统日志定位问题
4. 修改代码后热编译测试
5. 重新运行测试验证修复

### 3. 性能优化
- 测试脚本支持并行执行
- 日志分析使用实时监控
- 缓存检查避免重复查询

## 故障排除

### 常见问题

1. **插件检查失败**
   - 检查modbus插件是否编译
   - 验证插件加载状态
   - 查看系统日志错误信息

2. **设备注册失败**
   - 检查TCP连接是否正常
   - 验证产品配置是否正确
   - 查看设备数据库状态

3. **数据解析失败**
   - 检查Modbus数据格式
   - 验证协议解析逻辑
   - 查看缓存数据格式

4. **API查询失败**
   - 检查API服务状态
   - 验证认证token
   - 查看API响应格式

### 调试命令

```bash
# 查看系统状态
_build/emqx/rel/emqx/bin/emqx eval 'io:format("System status...~n").'

# 检查插件加载
_build/emqx/rel/emqx/bin/emqx eval 'io:format("Module: ~p~n", [code:which(modbus_rtu)]).'

# 查询设备信息
_build/emqx/rel/emqx/bin/emqx eval '
    DeviceAddr = <<"wrj_dm-zqy-20000">>,
    ProductId = <<"feeb43bffb">>,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    io:format("Device: ~p~n", [dgiot_device:lookup(DeviceId)]).
'
```

## 更新记录

### v2.0 (2025-12-26)
- 重构为模块化架构
- 支持完整测试循环
- 集成到Makefile.test
- 优化目录结构
- 修复中文编码问题
- 添加详细文档

### v1.0 (初始版本)
- 基础集成测试功能
- 支持设备注册和数据上报
- 基本的日志分析和API查询

## 相关文档

- [开发规则](../../../.clinerules/development_rules.md)
- [编码规范](../../../.clinerules/coding_standards.md)
- [集成测试工作流程规则](../../../.clinerules/integration_test_workflow_rules.md)
- [传感器数据工作流](../../../.clinerules/sensor_data_workflow.md)

## 技术支持

如有问题，请参考：
1. 查看系统日志：`tail -f logs/console.log`
2. 运行调试命令：参考故障排除章节
3. 查阅相关文档：参考相关文档章节
4. 联系技术支持团队
