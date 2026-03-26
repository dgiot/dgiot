#!/bin/bash
# 修复第185-186行的空格问题

file="/root/gitee/dgiot/apps/dgiot_uav/src/business/dgiot_uav_auto_tester.erl"

# 备份文件
cp "$file" "${file}.bak"

# 修复第185行：在 [ 和 #{ 之间添加空格
sed -i '185s/\[ #/\[ #/' "$file"

# 修复第186行：在 [ 和 #{ 之间添加空格
sed -i '186s/\[ #/\[ #/' "$file"

echo "✅ 第185-186行已修复"
echo "文件: $file"
echo "备份: ${file}.bak"
