# 编码过程中技能调用指南

## 问题描述

用户在编码过程中遇到以下问题：
1. 无法在编码过程中调用相关技能
2. 日志打印和中文编码问题需要手动处理
3. 缺少自动化的代码质量检查

## 解决方案

### 1. 自动技能检测Hook

已创建`CodeEditing` Hook，在编辑Erlang文件时自动检测以下问题：

#### 1.1 中文编码问题检测
- 检查缺少`/utf8`后缀的中文字符串
- 检查使用`~s`而不是`~ts`格式的中文打印
- 检查直接使用中文字符串而不是二进制格式

#### 1.2 日志打印问题检测
- 检查是否缺少日志头文件引入
- 检查是否使用`io:format`而不是`?LOG`宏
- 检查日志级别使用不规范

#### 1.3 编译问题检测
- 检查未使用的变量和函数
- 检查二进制匹配问题

### 2. 技能调用方法

#### 方法1：使用use_skill命令
```bash
# 调用中文编码修复技能
use_skill erlang_chinese_utf8

# 调用日志打印解决方案技能
use_skill erlang_logging_solution

# 调用编译调试技能
use_skill dgiot_compile_debug
```

#### 方法2：通过Hook自动推荐
当编辑Erlang文件时，Hook会自动检测问题并推荐相关技能：
```
=== 代码编辑检查 ===
文件: apps/dgiot_uav/src/channel/rudder_handler.erl
中文编码问题检测:
  - 发现缺少/utf8后缀的中文字符串
建议使用技能: erlang_chinese_utf8
修复命令: use_skill erlang_chinese_utf8
```

#### 方法3：手动触发检查
```bash
# 手动运行CodeEditing Hook检查文件
.clinerules/hooks/CodeEditing apps/dgiot_uav/src/channel/rudder_handler.erl
```

### 3. 技能集成到编码工作流

#### 3.1 编辑前检查
```bash
# 在编辑文件前运行检查
.clinerules/hooks/CodeEditing 要编辑的文件.erl
```

#### 3.2 编辑中实时检查
将Hook集成到编辑器中，实现实时检查：
- VSCode: 配置任务运行Hook
- Vim/Emacs: 配置自动命令
- 其他编辑器: 使用文件监视器

#### 3.3 编辑后修复
```bash
# 编辑完成后使用技能修复问题
use_skill erlang_chinese_utf8
# 或手动修复建议的问题
```

### 4. 具体问题修复示例

#### 4.1 中文编码问题修复
**问题代码**:
```erlang
io:format("测试中文打印"),
<<"中文字符串">>,
?LOG(info, "~s: ~p", ["中文日志", Data])
```

**修复后代码**:
```erlang
io:format("~ts", [<<"测试中文打印"/utf8>>]),
<<"中文字符串"/utf8>>,
?LOG(info, "~ts: ~p", [<<"中文日志"/utf8>>, Data])
```

**使用技能修复**:
```bash
use_skill erlang_chinese_utf8
```

#### 4.2 日志打印问题修复
**问题代码**:
```erlang
io:format("[INFO] 设备启动: ~p", [DeviceId]),
?LOG(Info, "通道创建: ~p", [ChannelId])
```

**修复后代码**:
```erlang
-include_lib("dgiot/include/logger.hrl").

?LOG(info, "设备启动: ~p", [DeviceId]),
?LOG(info, "通道创建: ~p", [ChannelId])
```

**使用技能修复**:
```bash
use_skill erlang_logging_solution
```

#### 4.3 编译问题修复
**问题代码**:
```erlang
init(Args) ->  % Args变量未使用
    {ok, #state{}}.

unused_function() ->  % 函数未导出
    ok.
```

**修复后代码**:
```erlang
init(_Args) ->  % 添加_前缀
    {ok, #state{}}.

% 删除未使用的函数或添加到导出列表
```

**使用技能修复**:
```bash
use_skill dgiot_compile_debug
```

### 5. 技能调用最佳实践

#### 5.1 按需调用
```bash
# 根据检测到的问题调用相应技能
if [检测到中文编码问题]; then
    use_skill erlang_chinese_utf8
fi

if [检测到日志打印问题]; then
    use_skill erlang_logging_solution
fi

if [检测到编译问题]; then
    use_skill dgiot_compile_debug
fi
```

#### 5.2 批量处理
```bash
# 批量检查目录下所有Erlang文件
find apps/ -name "*.erl" -exec .clinerules/hooks/CodeEditing {} \;

# 批量修复检测到的问题
for file in $(find apps/ -name "*.erl"); do
    .clinerules/hooks/CodeEditing "$file"
    # 根据检测结果调用相应技能
done
```

#### 5.3 集成到Makefile
```makefile
# 在Makefile中添加技能检查目标
check-skills:
	find apps/ -name "*.erl" -exec .clinerules/hooks/CodeEditing {} \;

fix-skills:
	@echo "请根据检查结果调用相应技能:"
	@echo "  use_skill erlang_chinese_utf8"
	@echo "  use_skill erlang_logging_solution"
	@echo "  use_skill dgiot_compile_debug"
```

### 6. 技能调用自动化脚本

创建自动化脚本`auto_fix_issues.sh`:
```bash
#!/bin/bash
# auto_fix_issues.sh - 自动检测并修复代码问题

FILE="$1"

if [ -z "$FILE" ]; then
    echo "用法: $0 <文件路径>"
    exit 1
fi

# 运行检查
echo "正在检查文件: $FILE"
ISSUES=$(.clinerules/hooks/CodeEditing "$FILE")

# 解析检查结果
if echo "$ISSUES" | grep -q "建议使用技能: erlang_chinese_utf8"; then
    echo "检测到中文编码问题，正在调用技能..."
    use_skill erlang_chinese_utf8
fi

if echo "$ISSUES" | grep -q "建议使用技能: erlang_logging_solution"; then
    echo "检测到日志打印问题，正在调用技能..."
    use_skill erlang_logging_solution
fi

if echo "$ISSUES" | grep -q "建议使用技能: dgiot_compile_debug"; then
    echo "检测到编译问题，正在调用技能..."
    use_skill dgiot_compile_debug
fi

echo "检查完成"
```

### 7. 常见问题解决

#### 7.1 技能调用失败
**问题**: `use_skill`命令无法找到技能
**解决**: 确保技能文件存在于`.cline/skills/`目录下

#### 7.2 Hook不触发
**问题**: CodeEditing Hook不自动触发
**解决**: 
1. 确保Hook文件有执行权限: `chmod +x .clinerules/hooks/CodeEditing`
2. 检查编辑器配置是否正确集成Hook

#### 7.3 检测不准确
**问题**: Hook检测结果不准确
**解决**: 
1. 更新Hook中的正则表达式
2. 手动检查文件确认问题

### 8. 扩展技能调用

#### 8.1 创建自定义技能调用
```bash
# 创建自定义技能调用脚本
cat > call_skill.sh << 'EOF'
#!/bin/bash
SKILL="$1"
FILE="$2"

case $SKILL in
    "chinese")
        use_skill erlang_chinese_utf8
        ;;
    "logging")
        use_skill erlang_logging_solution
        ;;
    "compile")
        use_skill dgiot_compile_debug
        ;;
    *)
        echo "未知技能: $SKILL"
        echo "可用技能: chinese, logging, compile"
        ;;
esac
EOF

chmod +x call_skill.sh
```

#### 8.2 集成到开发环境
```bash
# 在.bashrc或.zshrc中添加别名
alias check-code='.clinerules/hooks/CodeEditing'
alias fix-chinese='use_skill erlang_chinese_utf8'
alias fix-logging='use_skill erlang_logging_solution'
alias fix-compile='use_skill dgiot_compile_debug'
```

### 9. 监控和改进

#### 9.1 技能使用统计
```bash
# 记录技能使用情况
log_skill_usage() {
    local skill="$1"
    local file="$2"
    local timestamp=$(date +"%Y-%m-%d %H:%M:%S")
    echo "$timestamp | $skill | $file" >> .cline/skill_usage.log
}

# 在技能调用时记录
use_skill_with_log() {
    local skill="$1"
    log_skill_usage "$skill" "$(pwd)"
    use_skill "$skill"
}
```

#### 9.2 反馈和改进
```bash
# 收集技能使用反馈
collect_feedback() {
    echo "请为本次技能使用评分 (1-5):"
    read rating
    echo "请提供改进建议:"
    read feedback
    echo "$(date) | 评分: $rating | 反馈: $feedback" >> .cline/skill_feedback.log
}
```

### 10. 总结

通过以上方案，实现了在编码过程中自动检测问题并调用相应技能的功能：

1. **自动化检测**: CodeEditing Hook自动检测中文编码、日志打印、编译问题
2. **智能推荐**: 根据检测结果推荐相应技能
3. **一键修复**: 使用`use_skill`命令快速调用技能修复问题
4. **工作流集成**: 集成到编辑器和开发工作流中
5. **持续改进**: 收集使用反馈，不断优化技能和Hook

现在用户可以在编码过程中：
- 实时获得代码质量反馈
- 快速调用相关技能修复问题
- 提高编码效率和代码质量
- 避免常见的中文编码和日志打印问题

---
*最后更新: 2026年1月23日*
*版本: 1.0.0*
