# 规则验证方案

## 验证标准
- **文件数量**：4-6个核心文件
- **总行数**：≤610行
- **核心覆盖**：开发、编码、API、安全4个领域

## 自动统计功能
- **每次git push自动运行**
- **实时验证规则质量**
- **非阻塞式**：只显示警告，不阻止push

## 验证脚本
```bash
#!/bin/bash
cd "$(dirname "$0")"
file_count=$(ls -1 *.md | wc -l)
total_lines=$(wc -l *.md | grep total | awk '{print $1}')
echo "文件数量: $file_count"
echo "总行数: $total_lines"
[ $file_count -ge 4 ] && [ $file_count -le 6 ] && [ $total_lines -le 610 ] && echo "✅ 验证通过" || echo "❌ 验证失败"
```

## 使用方式
```bash
# 自动验证（每次git push）
git push

# 手动验证
./.clinerules/validate_rules.sh
```

## 更新记录
- 2025-12-19：创建简洁版验证方案
