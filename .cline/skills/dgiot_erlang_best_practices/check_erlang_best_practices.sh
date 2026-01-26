#!/bin/bash
# check_erlang_best_practices.sh - 检查Erlang最佳实践

echo "=== Erlang最佳实践检查 ==="
echo ""

# 1. 检查头文件包含
echo "1. 检查头文件包含..."
echo "查找使用include(\"file.hrl\")而不是include_lib的文件："
grep -r "include(\"" apps/ --include="*.erl" | grep -v "include_lib" | head -10

echo ""
echo "建议：平台头文件应使用include_lib(\"dgiot/include/file.hrl\")"
echo ""

# 2. 检查中文打印
echo "2. 检查中文打印..."
echo "查找直接使用中文字符串的文件："
grep -r "io:format.*[\x80-\xFF]" apps/ --include="*.erl" | head -10

echo ""
echo "建议：使用io:format(\"~p ~n\", [<<\"中文\"/utf8>>])或dgiot_utils:safe_format"
echo ""

# 3. 检查test函数
echo "3. 检查test函数..."
echo "查找没有test/0函数的模块："
for file in apps/*/src/*.erl; do
    if [ -f "$file" ]; then
        module_name=$(basename "$file" .erl)
        if grep -q "test()" "$file"; then
            echo "✅ $module_name: 有test函数"
        else
            echo "❌ $module_name: 无test函数"
        fi
    fi
done | head -20

echo ""
echo "建议：每个模块都应添加test/0函数用于在线调试"
echo ""

# 4. 检查热编译使用
echo "4. 检查热编译使用..."
echo "查找编译命令使用情况："
echo "热编译命令：_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot).'"
echo "全量编译命令：make run (仅用于首次环境搭建或重大变更)"
echo ""

# 5. 检查Erlang编程风格
echo "5. 检查Erlang编程风格..."
echo "查找可能的问题模式："
echo "a) 使用for循环而不是列表推导："
grep -r "for.*in.*to.*do" apps/ --include="*.erl" | head -5

echo ""
echo "b) 使用if语句而不是模式匹配："
grep -r "if.*then.*else" apps/ --include="*.erl" | head -5

echo ""
echo "建议：使用函数式编程、模式匹配和列表推导"
echo ""

# 6. 检查DGIOT架构使用
echo "6. 检查DGIOT架构使用..."
echo "查找直接数据库操作："
grep -r "dgiot_tdengine_adapter:save" apps/ --include="*.erl" | head -5

echo ""
echo "查找Hook系统使用："
grep -r "dgiot_hook:run_hook" apps/ --include="*.erl" | head -5

echo ""
echo "建议：遵循七层架构，使用Hook系统，通过标准API操作数据"
echo ""

# 7. 检查插件开发规范
echo "7. 检查插件开发规范..."
echo "查找插件模块："
find apps/ -name "*_plugin.erl" -type f | head -10

echo ""
echo "建议：插件应遵循标准生命周期，使用插件配置系统"
echo ""

# 8. 总结
echo "=== 检查总结 ==="
echo "1. 头文件包含：使用include_lib而不是include"
echo "2. 中文打印：使用二进制格式配合/utf8标志"
echo "3. 测试函数：每个模块添加test/0函数"
echo "4. 热编译：日常开发使用热编译，避免频繁make run"
echo "5. 编程风格：使用函数式编程、模式匹配、列表推导"
echo "6. DGIOT架构：遵循七层架构，使用Hook系统"
echo "7. 插件开发：遵循插件生命周期和配置规范"
echo ""
echo "=== 修复建议 ==="
echo "1. 使用dgiot_erlang_best_practices技能确保最佳实践"
echo "2. 使用erlang_include_system技能处理头文件"
echo "3. 使用erlang_chinese_utf8技能解决中文乱码"
echo "4. 使用dgiot_compile_debug技能进行热编译"
echo "5. 使用dgiot_online_debug技能添加test函数"
echo "6. 使用dgiot_architecture_learning技能理解架构"
echo "7. 使用dgiot_code_reuse_solution技能查找现有实现"