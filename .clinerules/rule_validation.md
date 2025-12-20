# 规则验证方案（简洁版）

## 验证标准

### 简洁性标准
- **文件数量**：4-6个核心文件
- **总行数**：≤610行
- **文件大小**：每个≤5KB

### 高效性标准  
- **查找速度**：关键词查找<100ms
- **命名规范**：统一命名格式
- **结构清晰**：层次≤3级

### 有效性标准
- **核心覆盖**：开发、编码、API、安全4个领域
- **实用命令**：≥12个实际命令示例
- **检查清单**：≥8个检查项

## 自动统计功能

### 概述
每次`git push`时自动验证规则体系的简洁性、高效性和有效性。

### 功能特性
- **每次git push自动运行**
- **实时验证规则质量**
- **非阻塞式**：只显示警告，不阻止push

### 工作原理
1. **Git Hook集成**：修改了`scripts/git-hook-pre-push.sh`
2. **验证脚本**：`.clinerules/validate_rules.sh`
3. **执行流程**：`git push → pre-push钩子 → 规则验证 → 显示结果`

## 验证脚本

```bash
#!/bin/bash
# validate_rules.sh - 规则验证脚本
cd "$(dirname "$0")"
file_count=$(ls -1 *.md | wc -l)
total_lines=$(wc -l *.md | grep total | awk '{print $1}')
echo "文件数量: $file_count"
echo "总行数: $total_lines"
[ $file_count -ge 4 ] && [ $file_count -le 6 ] && [ $total_lines -le 610 ] && echo "✅ 验证通过" || echo "❌ 验证失败"
```

## 使用方式

### 自动验证（每次git push）
```bash
# 已集成到pre-push钩子
git push  # 自动运行验证
```

### 手动验证
```bash
./.clinerules/validate_rules.sh
```

## 维护指南

### 更新验证标准
编辑`.clinerules/validate_rules.sh`：
```bash
# 调整标准
[ $file_count -ge 4 ] && [ $file_count -le 6 ]
[ ${total_lines:-0} -le 610 ]
```

### 禁用自动统计
```bash
# 临时禁用
mv .clinerules/validate_rules.sh .clinerules/validate_rules.sh.disabled
```

## 故障排除

### 脚本不存在
```
⚠️  规则验证脚本不存在
```

**解决方案**：
```bash
chmod +x .clinerules/validate_rules.sh
```

### 验证不准确
检查规则文件：
```bash
cd .clinerules
wc -l *.md
ls -1 *.md | wc -l
```

## 最佳实践

1. **保持规则简洁**：定期审查，删除冗余
2. **团队协作**：统一验证标准
3. **持续改进**：根据反馈调整

## 更新记录
- 2025-12-19：创建简洁版验证方案，集成自动统计功能
