#!/bin/bash

echo "开始运行TDengine范围查询测试..."
echo "========================================"

# 设置环境变量
export ERL_LIBS=/root/dgiot/apps

# 编译测试文件
echo "1. 编译测试文件..."
cd /root/dgiot
erlc -I apps/dgiot_tdengine/include -o apps/dgiot_tdengine/test apps/dgiot_tdengine/test/range_query_test.erl

# 运行测试
echo "2. 运行范围查询测试..."
erl -noshell -pa apps/dgiot_tdengine/ebin -pa apps/dgiot_tdengine/test \
    -eval "eunit:test(range_query_test, [verbose])" \
    -s init stop

echo "========================================"
echo "测试完成！"
