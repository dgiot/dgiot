# 日志系统运维命令参考

## 概述

本文档提供了DG-IoT平台日志系统的标准运维命令，确保团队统一使用现有OTP logger系统，避免创建自定义日志系统。

## 核心原则

### 1. 重用现有系统
- **禁止**创建自定义日志系统
- **必须**使用标准`?LOG`宏
- **继承**系统全局配置（`emqx.conf`中的`log.level`）

### 2. 商用与调试分离
- **商用环境**：默认`error`级别，只记录错误
- **调试环境**：动态调整为`debug`级别，查看详细日志
- **无需重启**：通过`logger:set_module_level/2`实时调整

## 运维命令

### 1. 查看日志级别

#### 查看系统全局级别
```bash
# 查看emqx.conf中的配置
grep "^log.level" _build/emqx/rel/emqx/etc/emqx.conf

# 查看当前生效的全局级别
_build/emqx/rel/emqx/bin/emqx eval '
    {ok, Config} = logger:get_primary_config(),
    Level = maps:get(level, Config, undefined),
    io:format("系统全局日志级别: ~p~n", [Level]).
'
```

#### 查看模块级别
```bash
# 查看特定模块的日志级别
_build/emqx/rel/emqx/bin/emqx eval '
    case logger:get_module_level(dgiot_modbusrtu_tcp) of
        {ok, Level} -> 
            io:format("模块 dgiot_modbusrtu_tcp 日志级别: ~p~n", [Level]);
        undefined -> 
            io:format("模块使用系统默认级别~n")
    end.
'

# 查看所有模块的日志级别设置
_build/emqx/rel/emqx/bin/emqx eval 'logger:get_module_level().'
```

### 2. 动态调整日志级别

#### 开启调试模式
```bash
# 将模块调整为debug级别（查看所有日志）
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, debug).'

# 验证调整结果
_build/emqx/rel/emqx/bin/emqx eval '
    {ok, Level} = logger:get_module_level(dgiot_modbusrtu_tcp),
    io:format("已调整为: ~p~n", [Level]).
'
```

#### 恢复商用模式
```bash
# 将模块恢复为error级别（只记录错误）
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, error).'

# 或恢复为warning级别（记录警告和错误）
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, warning).'
```

#### 批量调整多个模块
```bash
# 调整多个相关模块
_build/emqx/rel/emqx/bin/emqx eval '
    Modules = [dgiot_modbusrtu_tcp, modbus_rtu, dgiot_task],
    lists:foreach(
        fun(Module) -> 
            logger:set_module_level(Module, debug),
            io:format("已调整 ~p 为debug级别~n", [Module])
        end,
        Modules
    ).
'
```

### 3. 日志查看和监控

#### 实时查看日志
```bash
# 查看所有日志
tail -f logs/console.log

# 查看特定模块的日志
tail -f logs/console.log | grep "dgiot_modbusrtu_tcp"

# 查看特定级别的日志
tail -f logs/console.log | grep -E "(DEBUG|INFO|WARNING|ERROR)"

# 查看包含特定关键词的日志
tail -f logs/console.log | grep -E "(Received data|Device Connected|parse_frame)"
```

#### 日志文件管理
```bash
# 查看日志文件大小
ls -lh logs/*.log

# 清空日志文件（谨慎使用）
> logs/console.log

# 备份日志文件
cp logs/console.log logs/console.log.backup_$(date +%Y%m%d_%H%M%S)
```

### 4. 调试脚本

#### 调试模式启动脚本
```bash
#!/bin/bash
# debug_modbus.sh - 开启Modbus调试模式

echo "=== 开启Modbus调试日志 ==="

# 1. 调整日志级别
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, debug).'
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(modbus_rtu, debug).'

echo "✅ 已开启debug级别日志"
echo "模块: dgiot_modbusrtu_tcp, modbus_rtu"

# 2. 显示当前级别
echo "=== 当前日志级别 ==="
_build/emqx/rel/emqx/bin/emqx eval '
    lists:foreach(
        fun(Module) ->
            case logger:get_module_level(Module) of
                {ok, Level} -> io:format("~p: ~p~n", [Module, Level]);
                undefined -> io:format("~p: 使用系统默认~n", [Module])
            end
        end,
        [dgiot_modbusrtu_tcp, modbus_rtu]
    ).
'

# 3. 开始监控日志
echo "=== 开始监控日志（Ctrl+C停止）==="
tail -f logs/console.log | grep -E "(dgiot_modbusrtu_tcp|modbus_rtu|DEBUG|INFO|WARNING)"
```

#### 商用模式恢复脚本
```bash
#!/bin/bash
# production_modbus.sh - 恢复Modbus商用模式

echo "=== 恢复Modbus商用日志级别 ==="

# 恢复为error级别
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, error).'
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(modbus_rtu, error).'

echo "✅ 已恢复error级别日志"
echo "模块: dgiot_modbusrtu_tcp, modbus_rtu"

# 验证恢复结果
echo "=== 验证恢复结果 ==="
_build/emqx/rel/emqx/bin/emqx eval '
    lists:foreach(
        fun(Module) ->
            case logger:get_module_level(Module) of
                {ok, Level} -> io:format("~p: ~p~n", [Module, Level]);
                undefined -> io:format("~p: 使用系统默认~n", [Module])
            end
        end,
        [dgiot_modbusrtu_tcp, modbus_rtu]
    ).
'
```

## 常见问题排查

### 问题1：看不到debug日志
**症状**：设置了debug级别，但看不到debug日志

**排查步骤**：
```bash
# 1. 确认模块级别
_build/emqx/rel/emqx/bin/emqx eval 'logger:get_module_level(dgiot_modbusrtu_tcp).'

# 2. 确认全局级别
_build/emqx/rel/emqx/bin/emqx eval 'logger:get_primary_config().'

# 3. 测试日志输出
_build/emqx/rel/emqx/bin/emqx eval '?LOG(debug, "测试debug日志").'

# 4. 查看日志文件
tail -n 5 logs/console.log
```

### 问题2：日志输出过多
**症状**：商用环境日志太多，影响性能

**解决方案**：
```bash
# 恢复为error级别
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, error).'

# 或调整为warning级别
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, warning).'
```

### 问题3：日志文件过大
**症状**：日志文件占用过多磁盘空间

**解决方案**：
```bash
# 1. 调整日志轮转配置（emqx.conf）
# log.rotation.size = 10MB
# log.rotation.count = 5

# 2. 手动清理旧日志
find logs/ -name "*.log.*" -mtime +7 -delete

# 3. 调整日志级别减少输出
_build/emqx/rel/emqx/bin/emqx eval 'logger:set_module_level(dgiot_modbusrtu_tcp, error).'
```

## 最佳实践

### 1. 商用环境配置
```bash
# 保持默认配置，无需调整
# 系统全局级别：error
# 模块继承全局级别：error
```

### 2. 调试环境流程
```bash
# 1. 开启调试模式
./debug_modbus.sh

# 2. 执行测试操作
# 3. 分析日志
# 4. 恢复商用模式
./production_modbus.sh
```

### 3. 团队协作规范
- **统一使用标准命令**：避免个人自定义脚本
- **记录调整操作**：谁、何时、为何调整日志级别
- **及时恢复**：调试完成后立即恢复商用级别
- **定期审计**：检查日志级别是否符合环境要求

## 更新记录

- 2025-12-26：创建日志系统运维命令参考
- 基于编码规范第9节：日志系统重用规则

## 相关文档

- [编码规范](../.clinerules/coding_standards.md) - 第9节：日志系统重用规则
- [开发规则](../.clinerules/development_rules.md) - 热编译和热加载命令
- [emqx配置](../_build/emqx/rel/emqx/etc/emqx.conf) - 系统全局日志配置
