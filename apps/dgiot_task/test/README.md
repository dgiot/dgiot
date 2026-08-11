# dgiot_task模块测试文档

## 概述

本文档描述了dgiot_task模块的测试体系，包括单元测试、集成测试和性能测试。

## 测试结构

```
apps/dgiot_task/test/
├── dgiot_task_test.erl              # 单元测试
├── dgiot_task_integration_test.erl  # 集成测试
├── run_tests.sh                     # 测试执行脚本
├── test_config.json                 # 测试配置
├── README.md                        # 测试文档
└── test_data/                       # 测试数据目录
```

## 测试类型

### 1. 单元测试
测试模块的各个函数功能，确保每个函数按预期工作。

**测试范围：**
- 数据保存函数（save_td, smart_save_td, save_td_no_match）
- 物模型函数（get_props, get_collection, get_calculated, get_storage）
- 统计计算函数（get_statistic, get_last_value, compare）
- 工具函数（string2value）
- 协议处理函数（needs_protocol_parsing, call_protocol_hook）
- 规则引擎函数（rule_engine_transform, register_rule, get_rules）
- 任务编排函数（schedule_tasks_from_thing_model, parse_task_parameters, stop_tasks）

### 2. 集成测试
测试模块的端到端工作流，确保各个组件协同工作。

**测试范围：**
- 数据保存工作流
- 物模型处理工作流
- 规则引擎工作流
- 任务编排工作流

### 3. 性能测试（待实现）
测试模块的性能指标，确保满足性能要求。

## 测试配置

测试配置位于 `test_config.json` 文件中，包含：
- 测试数据配置
- 测试覆盖范围
- 性能要求
- 测试用例定义
- 测试环境要求

## 运行测试

### 方法1：使用英文测试脚本（推荐）

由于emqx eval命令对中文字符支持问题，建议使用英文测试脚本：

```bash
# 给脚本执行权限
chmod +x apps/dgiot_task/test/english_test.sh

# 运行测试
./apps/dgiot_task/test/english_test.sh
```

### 方法2：手动运行单行命令

```bash
# 编译模块
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_task).'

# 测试基本函数
_build/emqx/rel/emqx/bin/emqx eval 'Result = dgiot_task:string2value("1+2", <<"int">>), io:format("Result: ~p~n", [Result]).'

# 测试物模型函数
_build/emqx/rel/emqx/bin/emqx eval 'Props = dgiot_task:get_props(<<"test">>), io:format("Props count: ~p~n", [length(Props)]).'

# 测试数据保存函数
_build/emqx/rel/emqx/bin/emqx eval 'Result = dgiot_task:save_td(<<"test">>, <<"device">>, #{<<"temp">> => 25}, #{<<"interval">> => 3}), io:format("Save result: ~p~n", [Result]).'
```

### 方法3：使用EUnit测试（需要修复eval命令问题）

目前EUnit测试由于eval命令的多行字符串问题无法直接运行，需要进一步调试。

### 已验证的功能

通过测试验证了以下功能正常工作：
1. ✅ 模块编译和加载
2. ✅ string2value函数（数值计算）
3. ✅ compare函数（比较运算）
4. ✅ get_props函数（物模型属性获取）
5. ✅ get_control函数（控制值生成）
6. ✅ save_td函数（数据保存）

## 测试数据

测试数据存储在 `test_data/` 目录中，包括：
- 有效数据样本
- 无效数据样本
- 边界条件数据
- 性能测试数据

## 测试覆盖率要求

- 单元测试覆盖率：≥80%
- 集成测试覆盖率：≥90%
- 关键路径覆盖率：100%

## 性能要求

- 最大响应时间：≤100ms
- 最小吞吐量：≥100次/秒
- 最大内存使用：≤50MB

## 测试报告

测试执行后会生成测试报告，包括：
- 测试执行时间
- 测试通过率
- 失败用例详情
- 性能指标
- 覆盖率统计

## 故障排除

### 常见问题

1. **测试失败：函数未定义**
   - 确保模块已正确编译
   - 检查函数名称和参数是否正确

2. **测试超时**
   - 检查测试环境是否正常
   - 增加测试超时时间

3. **依赖服务不可用**
   - 确保TDengine服务运行正常
   - 确保MQTT代理服务运行正常

### 调试方法

```bash
# 查看详细日志
_build/emqx/rel/emqx/bin/emqx eval '
    io:format("~s ~p 测试调试信息~n", [?FILE, ?LINE]),
    io:format("测试数据: ~p~n", [TestData]).
'

# 检查模块状态
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_task:module_info().'
```

## 持续集成

测试体系支持持续集成，可以在CI/CD流水线中集成：

```yaml
# .gitlab-ci.yml 示例
test_dgiot_task:
  stage: test
  script:
    - cd apps/dgiot_task/test
    - ./run_tests.sh
  artifacts:
    reports:
      junit: test-results.xml
```

## 更新记录

### v1.0.0 (2025-12-24)
- 创建完整的测试体系
- 实现单元测试和集成测试
- 创建测试执行脚本
- 编写测试文档

## 贡献指南

1. 添加新测试用例时，更新 `test_config.json`
2. 确保测试覆盖率满足要求
3. 运行所有测试确保没有回归
4. 更新测试文档

## 联系方式

如有测试相关问题，请联系：
- 项目负责人：dgaiot团队
- 问题反馈：通过GitHub Issues提交
