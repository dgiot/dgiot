
# DG-IoT通用开发规则

## 概述

本文件定义了DG-IoT平台开发的通用规则，适用于所有插件和模块的开发工作。

## 编译命令规范

### ✅ 允许使用的命令
1. **`make`** - 全量编译检查，验证编译是否通过
2. **`make run`** - 全量编译并启动在线调试环境

### ❌ 禁止使用的命令
1. **`make compile`** - 此命令不存在或不支持

### 🔥 日常开发推荐
```bash
# 热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot).'

# 热加载
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot).'
```

### ⚠️ 热编译质量要求
- **零警告原则**：热编译必须消除所有编译警告
- **检查清单**：
  - [ ] 没有未使用变量警告
  - [ ] 没有未使用函数警告
  - [ ] 没有语法错误
  - [ ] 没有类型不匹配

## 开发命令

### 1. 热编译和热加载
```bash
# 通用插件热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot).'

# 通用插件热加载
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot).'
```

### 2. 测试命令
```bash
# 在线测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_udp_test:test_multicast_with_tcpdump().'

# 全量编译调试
make run
```

## 工程建议

### 1. 文件生成策略
- **先生成插件所需要的所有文件**：在全量编译之前，确保所有必要的文件都已创建
- **全量编译之后就不再新增文件**：一旦完成全量编译，后续修改只做热编译
- **全部做热编译**：日常开发中使用热编译提高效率

## 快速检查清单
- [ ] 使用正确的编译命令
- [ ] 热编译零警告（必须满足）
- [ ] 日志格式符合规范
- [ ] 错误处理完整
- [ ] 有单元测试
- [ ] 遵循三层架构

## 更新记录
- 2025-12-19：融合全局规则，创建统一通用开发规则
- 2025-12-24：添加热编译质量要求，强调零警告原则
