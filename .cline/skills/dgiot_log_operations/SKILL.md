---
name: dgiot_log_operations
description: DGIOT日志系统运维技能，提供日志级别管理、日志查看命令、日志文件管理，重用现有OTP logger系统，支持商用与调试环境分离管理
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-26
category: operations
tags: [dgiot, logs, operations, monitoring, debugging, otp, logger, maintenance]
trigger_phrases:
  - 日志查看
  - 日志级别调整
  - 日志运维
  - 查看日志
  - 日志文件管理
  - 日志级别设置
  - 调试日志
  - 商用日志
---

# DGIOT日志系统运维技能

## 概述

本技能提供DGIOT平台日志系统的标准运维命令，确保团队统一使用现有OTP logger系统，避免创建自定义日志系统。支持商用与调试环境分离管理，动态调整日志级别。

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

## 技能集成

### 与协议调试技能集成
```
dgiot_protocol_debug 激活
    ↓
[dgiot_log_operations] 日志运维技能
    ↓
调整日志级别为debug
    ↓
监控关键日志
    ↓
分析协议解析问题
```

### 与在线调试技能集成
```
dgiot_online_debug 激活
    ↓
[dgiot_log_operations] 日志运维技能
    ↓
实时查看日志
    ↓
动态调整日志级别
    ↓
输出调试结果
```

## 检查清单

### 日志级别管理检查清单
- [ ] 是否使用了标准`?LOG`宏？
- [ ] 是否避免了环境变量配置日志级别？
- [ ] 是否重用现有OTP logger系统？
- [ ] 是否通过动态调整而非代码修改来切换日志级别？
- [ ] 商用环境是否保持默认error级别？

### 调试环境检查清单
- [ ] 调试前是否记录了当前日志级别？
- [ ] 是否只调整了必要的模块？
- [ ] 调试完成后是否恢复了日志级别？
- [ ] 是否记录了调试操作和原因？

### 日志文件管理检查清单
- [ ] 日志文件大小是否在合理范围内？
- [ ] 是否定期备份重要日志？
- [ ] 是否清理了过期的日志文件？
- [ ] 日志轮转配置是否正确？

## 工具脚本

### 1. 日志分析工具
```bash
#!/bin/bash
# analyze_logs.sh

echo "=== 日志分析工具 ==="
echo ""

# 1. 统计错误日志
echo "1. 统计错误日志..."
ERROR_COUNT=$(grep -c "ERROR" logs/console.log)
WARNING_COUNT=$(grep -c "WARNING" logs/console.log)
echo "  错误日志: $ERROR_COUNT 条"
echo "  警告日志: $WARNING_COUNT 条"

# 2. 查看最近错误
echo "2. 查看最近错误..."
grep "ERROR" logs/console.log | tail -5

# 3. 分析模块日志分布
echo "3. 分析模块日志分布..."
echo "  dgiot_modbusrtu_tcp: $(grep -c "dgiot_modbusrtu_tcp" logs/console.log) 条"
echo "  modbus_rtu: $(grep -c "modbus_rtu" logs/console.log) 条"
echo "  dgiot_task: $(grep -c "dgiot_task" logs/console.log) 条"

# 4. 查看日志文件信息
echo "4. 日志文件信息..."
ls -lh logs/console.log
```

### 2. 日志级别批量管理工具
```bash
#!/bin/bash
# manage_log_levels.sh

ACTION="$1"
MODULE="$2"

case "$ACTION" in
    "debug")
        echo "将模块 $MODULE 调整为debug级别..."
        _build/emqx/rel/emqx/bin/emqx eval "logger:set_module_level($MODULE, debug)."
        ;;
    "info")
        echo "将模块 $MODULE 调整为info级别..."
        _build/emqx/rel/emqx/bin/emqx eval "logger:set_module_level($MODULE, info)."
        ;;
    "warning")
        echo "将模块 $MODULE 调整为warning级别..."
        _build/emqx/rel/emqx/bin/emqx eval "logger:set_module_level($MODULE, warning)."
        ;;
    "error")
        echo "将模块 $MODULE 调整为error级别..."
        _build/emqx/rel/emqx/bin/emqx eval "logger:set_module_level($MODULE, error)."
        ;;
    "check")
        echo "检查模块 $MODULE 日志级别..."
        _build/emqx/rel/emqx/bin/emqx eval "
            case logger:get_module_level($MODULE) of
                {ok, Level} -> io:format(\"~p: ~p~n\", [$MODULE, Level]);
                undefined -> io:format(\"~p: 使用系统默认~n\", [$MODULE])
            end.
        "
        ;;
    *)
        echo "用法: $0 {debug|info|warning|error|check} <模块名>"
        echo "示例: $0 debug dgiot_modbusrtu_tcp"
        exit 1
        ;;
esac
```

## 总结

通过本技能，可以：
1. **统一日志管理**：确保团队使用统一的日志系统
2. **动态调整级别**：无需重启即可调整日志级别
3. **商用调试分离**：支持商用和调试环境的不同需求
4. **提高运维效率**：提供标准化的运维命令和工具
5. **集成开发流程**：与调试、测试等技能无缝集成

**使用方式**：
```bash
# 当需要查看或调整日志时
use_skill dgiot_log_operations

# 运行调试脚本
.cline/skills/dgiot_log_operations/debug_modbus.sh

# 运行日志分析工具
.cline/skills/dgiot_log_operations/analyze_logs.sh