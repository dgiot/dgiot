#!/bin/bash
# test_dgiot_online_debug.sh - 测试dgiot_online_debug技能与Hook系统集成

echo "=== 测试dgiot_online_debug技能与Hook系统集成 ==="
echo ""

# 测试1: 模拟Erlang编程相关操作
echo "测试1: 模拟Erlang编程相关操作"
TEST_INPUT_1='{
  "preToolUse": {
    "toolName": "execute_command",
    "toolArgs": "erlc test.erl"
  },
  "taskId": "test-001",
  "clineVersion": "1.0.0",
  "timestamp": 1737412800000
}'

echo "输入: $TEST_INPUT_1"
echo "预期输出: 应该检测到Erlang编程操作，建议使用dgiot_online_debug技能"
echo ""

# 测试2: 模拟在线调试操作
echo "测试2: 模拟在线调试操作"
TEST_INPUT_2='{
  "preToolUse": {
    "toolName": "execute_command",
    "toolArgs": "_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:compile(dgiot_uav)."
  },
  "taskId": "test-002",
  "clineVersion": "1.0.0",
  "timestamp": 1737412800000
}'

echo "输入: $TEST_INPUT_2"
echo "预期输出: 应该检测到在线调试操作，确认dgiot_online_debug技能适用"
echo ""

# 测试3: 模拟TaskComplete评估
echo "测试3: 模拟TaskComplete评估"
TEST_INPUT_3='{
  "taskComplete": {
    "result": "成功使用dgiot_online_debug技能进行在线调试，添加了test函数并执行热编译测试"
  },
  "taskId": "test-003",
  "clineVersion": "1.0.0",
  "timestamp": 1737412800000
}'

echo "输入: $TEST_INPUT_3"
echo "预期输出: 应该评估为优秀，充分使用了在线调试技能"
echo ""

# 测试4: 模拟非Erlang操作
echo "测试4: 模拟非Erlang操作"
TEST_INPUT_4='{
  "preToolUse": {
    "toolName": "execute_command",
    "toolArgs": "ls -la"
  },
  "taskId": "test-004",
  "clineVersion": "1.0.0",
  "timestamp": 1737412800000
}'

echo "输入: $TEST_INPUT_4"
echo "预期输出: 应该不触发Erlang编程建议"
echo ""

echo "=== 测试完成 ==="
echo ""
echo "Hook集成验证要点:"
echo "1. Erlang编程操作自动检测"
echo "2. 在线调试操作识别"
echo "3. 技能使用情况评估"
echo "4. 智能建议提供"
echo "5. 标准化调试流程引导"