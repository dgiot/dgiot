# DG-IoT Modbus 测试框架

## 概述

本测试框架为 DG-IoT Modbus 插件提供完整的测试支持，包括：

- **单元测试** (EUnit): 测试单个函数和模块
- **集成测试** (Common Test): 测试模块间交互和完整流程
- **代码覆盖率**: 自动生成覆盖率报告
- **性能测试**: 测试关键路径性能
- **一键式测试**: 支持工程级一键测试

## 目录结构

```
test/
├── README.md                    # 本文档
├── eunit/                       # EUnit单元测试
│   ├── modbus_util_eunit.erl    # modbus_util模块测试
│   └── modbus_rtu_eunit.erl     # modbus_rtu模块测试
├── ct/                          # Common Test集成测试
│   ├── test.config              # 测试配置文件
│   ├── modbus_integration_SUITE.erl  # 集成测试套件
│   └── modbus_tcp_client_SUITE.erl   # TCP客户端连接和注册测试套件
├── data/                        # 测试数据
│   └── README.txt               # 测试数据说明
└── coverage/                    # 覆盖率报告（自动生成）
```

## 快速开始

### 1. 运行所有测试

```bash
# 在 dgiot_modbus 目录下
make -f Makefile.test test-all
```

### 2. 仅运行单元测试

```bash
make -f Makefile.test test-eunit
```

### 3. 仅运行集成测试

```bash
make -f Makefile.test test-ct
```

### 4. 生成覆盖率报告

```bash
make -f Makefile.test test-coverage
```

### 5. 测试特定模块

```bash
make -f Makefile.test test-module-modbus_util
```

## 测试命令详解

### 完整测试流程

```bash
# 1. 运行所有测试
make -f Makefile.test test-all

# 2. 查看覆盖率报告
# 报告位置: _build/test/cover/index.html
# 或使用: file:///path/to/dgiot_modbus/_build/test/cover/index.html
```

### 快速测试

```bash
# 快速运行基本测试
make -f Makefile.test test-quick
```

### 性能测试

```bash
# 运行性能测试
make -f Makefile.test test-performance
```

### 清理测试文件

```bash
# 清理测试生成的文件
make -f Makefile.test clean-test
```

## 测试覆盖率目标

- **行覆盖率**: ≥ 80%
- **分支覆盖率**: ≥ 70%
- **函数覆盖率**: ≥ 90%

## 编写新测试

### 1. 添加EUnit测试

在 `test/eunit/` 目录下创建新的测试文件：

```erlang
-module(my_module_eunit).
-include_lib("eunit/include/eunit.hrl").

my_function_test_() ->
    [
        {"测试描述",
            ?_assertEqual(Expected, my_module:my_function(Input))}
    ].
```

### 2. 添加Common Test测试

在 `test/ct/` 目录下创建新的测试套件：

```erlang
-module(my_integration_SUITE).
-compile(export_all).

all() -> [test_my_feature].

test_my_feature(_Config) ->
    % 测试代码
    ok.
```

## 测试数据管理

测试数据应放在 `test/data/` 目录下：

- 有效报文数据
- 配置文件示例
- 模拟设备数据
- 性能测试数据

## 集成到CI/CD

### 1. 在CI流水线中添加测试

```yaml
# .gitlab-ci.yml 示例
test_modbus:
  stage: test
  script:
    - cd apps/dgiot_modbus
    - make -f Makefile.test test-all
  artifacts:
    paths:
      - apps/dgiot_modbus/_build/test/cover/
```

### 2. 质量门禁

测试必须满足以下条件才能通过：

- 所有测试用例通过
- 代码覆盖率 ≥ 80%
- 无编译警告
- 性能测试通过

## 故障排除

### 常见问题

1. **测试失败：模块未加载**
   - 确保已编译插件：`_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'`

2. **覆盖率报告未生成**
   - 检查 `rebar.config` 中的覆盖率配置
   - 确保运行测试时启用了覆盖率：`--cover` 参数

3. **性能测试超时**
   - 调整测试超时时间
   - 检查系统资源使用情况

4. **注册包匹配失败**
   - 问题：注册包是十六进制字符串，需要先解码为ASCII
   - 修复：使用 `dgiot_utils:hex_to_binary/1` 解码十六进制字符串
   - 测试：新增 `modbus_rtu_tcp_eunit.erl` 测试十六进制解码和通配符匹配

### 调试测试

```bash
# 详细输出测试过程
rebar3 eunit -v

# 运行单个测试套件
rebar3 ct --suite test/ct/modbus_integration_SUITE

# 测试特定问题
make -f Makefile.test test-module-modbus_rtu_tcp_eunit
```

## 最佳实践

1. **测试独立性**: 每个测试用例应独立运行，不依赖其他测试的状态
2. **测试数据隔离**: 使用独立的测试数据，避免污染生产数据
3. **错误场景覆盖**: 测试正常流程和错误处理
4. **性能基准**: 为关键功能建立性能基准
5. **持续维护**: 定期更新测试用例，保持与代码同步

## 更新记录

- **2025-12-23**: 创建测试框架
  - 添加EUnit单元测试
  - 添加Common Test集成测试
  - 添加代码覆盖率支持
  - 添加一键式测试命令

## 相关文档

- [EUnit用户指南](http://erlang.org/doc/apps/eunit/chapter.html)
- [Common Test用户指南](http://erlang.org/doc/apps/common_test/users_guide.html)
- [DG-IoT开发规则](../.clinerules/development_rules.md)
