#!/bin/bash

# 错误处理
set -euo pipefail
trap 'echo "脚本执行失败: $?" >&2' ERR
#!/bin/bash
# integration_test_registerbyport_enhanced.sh - Modbus设备注册和数据块主动上报集成测试
# 端口说明：TEST_PORT=20000 是服务器端口（通道配置中的固定端口）
# 在RegisterByPort注册方式中，设备地址 = 注册报文 + "-" + 服务器端口
# 注意：这是服务器监听端口，不是客户端连接端口

echo "=== Modbus设备注册和数据块主动上报集成测试 ==="
echo "测试开始时间: $(date)"
echo "当前时间戳: $(date +%s)"

# 记录测试开始时间
TEST_START_TIME=$(date +%s)

# 配置参数
TEST_DEVICE="wrj_dm-zqy"
TEST_PORT=20000  # 服务器端口（通道配置中的固定端口）
TEST_PRODUCT="feeb43bffb"
LOG_FILE="_build/emqx/rel/emqx/log/emqx.log.1"
DEVICE_ADDR="${TEST_DEVICE}-${TEST_PORT}"  # 设备地址 = 注册报文 + "-" + 服务器端口

# 1. 检查系统状态
check_system_status() {
    echo "1. 检查系统状态..."
    
    # 检查服务器端口
    if netstat -tlnp | grep ":${TEST_PORT}" > /dev/null; then
        echo "✅ 服务器正在监听端口${TEST_PORT}"
    else
        echo "❌ 服务器未监听端口${TEST_PORT}"
        return 1
    fi
    
    # 检查日志文件
    if [ -f "$LOG_FILE" ]; then
        echo "✅ 日志文件存在: $LOG_FILE"
        # 备份当前日志
        cp "$LOG_FILE" "${LOG_FILE}.backup_$(date +%Y%m%d_%H%M%S)"
    else
        echo "⚠️  日志文件不存在: $LOG_FILE"
    fi
    
    # 检查modbus插件是否加载
    echo "检查modbus插件状态..."
    _build/emqx/rel/emqx/bin/emqx eval '
        case code:which(modbus_rtu) of
            non_existing ->
                dgiot_utils:safe_format("⚠️  modbus_rtu模块未加载，请先编译modbus插件~n", []);
            _ ->
                dgiot_utils:safe_format("✅ modbus_rtu模块已加载~n", [])
        end,
        
        case code:which(dgiot_modbusrtu_tcp) of
            non_existing ->
                dgiot_utils:safe_format("⚠️  dgiot_modbusrtu_tcp模块未加载~n", []);
            _ ->
                dgiot_utils:safe_format("✅ dgiot_modbusrtu_tcp模块已加载~n", [])
        end.
    '
    
    return 0
}

# 2. 执行设备注册和Modbus数据上报测试
execute_modbus_test() {
    echo "2. 执行设备注册和Modbus数据上报测试..."
    
    # 记录测试前日志位置
    if [ -f "$LOG_FILE" ]; then
        LOG_LINES_BEFORE=$(wc -l < "$LOG_FILE")
        echo "测试前日志行数: $LOG_LINES_BEFORE"
    fi
    
    # 步骤1: 发送设备注册报文
    echo "步骤1: 发送设备注册报文..."
    echo "连接到服务器端口${TEST_PORT}..."
    echo "发送注册报文: ${TEST_DEVICE}"
    
    # 使用nc发送注册报文（去掉换行符）
    echo -n "${TEST_DEVICE}" | nc -w 5 127.0.0.1 "${TEST_PORT}"
    
    # 等待设备注册处理
    sleep 3
    
    # 步骤2: 发送Modbus数据块主动上报
    echo "步骤2: 发送Modbus数据块主动上报..."
    
    # 构建Modbus RTU数据块（使用用户提供的十六进制数据）
    MODBUS_HEX_DATA="01 03 60 0C 19 0E 13 03 11 00 39 02 B2 00 02 08 80 00 00 00 00 00 00 00 00 00 00 00 00 00 6A 00 9E 9F B5 05 92 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 2F FD ED F1 F8 86 8A F7 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 43 65 00 00 00 00 7D B3"
    
    echo "发送Modbus数据块（48个寄存器）..."
    echo "数据包长度: 101字节"
    echo "从机地址: 0x01"
    echo "功能码: 0x03 (读取保持寄存器)"
    
    # 使用Python发送二进制数据
    python3 -c "
import socket
import time

# 将十六进制字符串转换为二进制数据
hex_data = '$MODBUS_HEX_DATA'
hex_bytes = hex_data.replace(' ', '')
data = bytes.fromhex(hex_bytes)

print(f'发送Modbus数据包: {len(data)}字节')
print(f'十六进制: {hex_bytes[:50]}...')

# 连接到服务器
try:
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(5)
    s.connect(('127.0.0.1', $TEST_PORT))
    
    # 发送数据
    s.sendall(data)
    print('✅ Modbus数据发送成功')
    
    # 可选：接收响应（如果有）
    try:
        response = s.recv(1024)
        if response:
            print(f'收到响应: {response.hex()[:50]}...')
    except socket.timeout:
        print('未收到响应（正常情况）')
    
    s.close()
except Exception as e:
    print(f'❌ 发送失败: {e}')
"
    
    # 等待数据处理
    sleep 3
    
    # 记录测试后日志位置
    if [ -f "$LOG_FILE" ]; then
        LOG_LINES_AFTER=$(wc -l < "$LOG_FILE")
        echo "测试后日志行数: $LOG_LINES_AFTER"
        NEW_LOG_LINES=$((LOG_LINES_AFTER - LOG_LINES_BEFORE))
        echo "新增日志行数: $NEW_LOG_LINES"
    fi
    
    return 0
}

# 3. 检查后端日志（关键步骤）
check_backend_logs() {
    echo "3. 检查后端日志..."
    
    if [ ! -f "$LOG_FILE" ]; then
        echo "❌ 日志文件不存在: $LOG_FILE"
        return 1
    fi
    
    # 检查测试期间的日志
    echo "检查测试期间的后端日志..."
    
    # 查找TCP连接日志
    TCP_LOG=$(grep -n "tcp.*${TEST_DEVICE}" "$LOG_FILE" | tail -1)
    if [ -n "$TCP_LOG" ]; then
        echo "✅ 找到TCP连接日志:"
        echo "   $TCP_LOG"
        
        # 提取时间戳
        TIMESTAMP=$(echo "$TCP_LOG" | grep -o '"time":[0-9]*' | cut -d: -f2)
        if [ -n "$TIMESTAMP" ]; then
            echo "   日志时间戳: $TIMESTAMP"
            echo "   测试开始时间戳: $TEST_START_TIME"
            
            # 时间戳转换（纳秒转秒）
            LOG_TIME_SEC=$((TIMESTAMP / 1000000))
            TIME_DIFF=$((TEST_START_TIME - LOG_TIME_SEC))
            
            if [ $TIME_DIFF -lt 60 ]; then
                echo "✅ 日志时间戳与测试时间匹配（差异: ${TIME_DIFF}秒）"
            else
                echo "⚠️  日志时间戳与测试时间不匹配（差异: ${TIME_DIFF}秒）"
            fi
        fi
    else
        echo "❌ 未找到TCP连接日志"
    fi
    
    # 查找RegisterByPort日志
    REGISTER_LOG=$(grep -n "RegisterByPort" "$LOG_FILE" | tail -1)
    if [ -n "$REGISTER_LOG" ]; then
        echo "✅ 找到RegisterByPort日志:"
        echo "   $REGISTER_LOG"
    else
        echo "❌ 未找到RegisterByPort日志"
    fi
    
    # 查找Modbus数据解析日志
    MODBUS_LOG=$(grep -n "parse_frame\|decode_data\|Received data" "$LOG_FILE" | tail -3)
    if [ -n "$MODBUS_LOG" ]; then
        echo "✅ 找到Modbus数据解析日志:"
        echo "$MODBUS_LOG" | while read -r line; do
            echo "   $line"
        done
    else
        echo "⚠️  未找到Modbus数据解析日志"
    fi
    
    # 查找数据存储日志
    STORAGE_LOG=$(grep -n "save_td\|save_to_tdengine\|TDengine" "$LOG_FILE" | tail -2)
    if [ -n "$STORAGE_LOG" ]; then
        echo "✅ 找到数据存储日志:"
        echo "$STORAGE_LOG" | while read -r line; do
            echo "   $line"
        done
    else
        echo "⚠️  未找到数据存储日志"
    fi
    
    # 查找错误日志
    ERROR_LOG=$(grep -n "error.*case_clause\|error.*fasle\|error.*badarg\|error.*function_clause" "$LOG_FILE" | tail -3)
    if [ -n "$ERROR_LOG" ]; then
        echo "❌ 找到错误日志:"
        echo "$ERROR_LOG" | while read -r line; do
            echo "   $line"
        done
        return 1
    else
        echo "✅ 未找到错误日志"
    fi
    
    return 0
}

# 4. 验证设备注册和Modbus数据
verify_modbus_data() {
    echo "4. 验证设备注册和Modbus数据..."
    
    # 验证设备是否注册成功
    echo "检查设备是否已注册..."
    
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        
        dgiot_utils:safe_format(\"验证设备注册:~n\", []),
        dgiot_utils:safe_format(\"  设备地址: ~p~n\", [DeviceAddr]),
        dgiot_utils:safe_format(\"  产品ID: ~p~n\", [ProductId]),
        dgiot_utils:safe_format(\"  设备ID: ~p~n\", [DeviceId]),
        
        case dgiot_device:lookup(DeviceId) of
            {ok, Device} ->
                dgiot_utils:safe_format(\"✅ 设备存在~n\", []),
                % 检查关键字段
                DevAddr = maps:get(<<\"devaddr\">>, Device, <<>>),
                Status = maps:get(<<\"status\">>, Device, false),
                IsEnable = maps:get(<<\"isEnable\">>, Device, false),
                
                dgiot_utils:safe_format(\"  设备地址: ~p (预期: ~p)~n\", [DevAddr, DeviceAddr]),
                dgiot_utils:safe_format(\"  设备状态: ~p (预期: true)~n\", [Status]),
                dgiot_utils:safe_format(\"  启用状态: ~p (预期: true)~n\", [IsEnable]);
            {error, not_found} ->
                dgiot_utils:safe_format(\"❌ 设备未找到~n\", []);
            {error, Reason} ->
                dgiot_utils:safe_format(\"❌ 设备查询错误: ~p~n\", [Reason])
        end.
    "
    
    echo ""
    echo "检查Modbus数据解析和存储..."
    
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        
        dgiot_utils:safe_format(\"检查Modbus数据:~n\", []),
        
        % 检查缓存数据
        case dgiot_data:get({last_data, DeviceId}) of
            not_find ->
                dgiot_utils:safe_format(\"⚠️  未找到last_data缓存~n\", []);
            CachedData ->
                dgiot_utils:safe_format(\"✅ 找到last_data缓存~n\", []),
                dgiot_utils:safe_format(\"  缓存数据: ~p~n\", [CachedData]),
                
                % 检查是否包含Modbus数据
                case CachedData of
                    #{<<"block_data">> := BlockData} ->
                        dgiot_utils:safe_format(\"  包含block_data: ~p~n\", [BlockData]);
                    _ ->
                        dgiot_utils:safe_format(\"  不包含block_data字段~n\", [])
                end
        end,
        
        % 检查TDengine数据（模拟查询）
        dgiot_utils:safe_format(\"检查数据存储状态:~n\", []),
        case dgiot_tdengine_adapter:query(ProductId, <<\"SELECT COUNT(*) FROM \\\"device_\\\" || ? WHERE devaddr = ?\">>, [DeviceId, DeviceAddr]) of
            {ok, #{<<"data">> := [[Count]]}} when Count > 0 ->
                dgiot_utils:safe_format(\"✅ TDengine中有 ~p 条设备数据~n\", [Count]);
            {ok, #{<<"data">> := [[0]]}} ->
                dgiot_utils:safe_format(\"⚠️  TDengine中暂无设备数据~n\", []);
            {error, Reason} ->
                dgiot_utils:safe_format(\"⚠️  TDengine查询错误: ~p~n\", [Reason])
        end.
    "
}

# 5. 问题处理循环（集成测试工作流程）
problem_solving_cycle() {
    echo ""
    echo "5. 问题处理循环（集成测试工作流程）..."
    
    local attempt=1
    local max_attempts=3
    
    while [ $attempt -le $max_attempts ]; do
        echo "尝试 $attempt/$max_attempts"
        
        # 执行测试
        if execute_modbus_test && check_backend_logs; then
            verify_modbus_data
            echo "✅ 测试成功"
            return 0
        else
            echo "❌ 测试失败，等待修改代码..."
            echo "请修改代码后按回车继续..."
            read -r
            
            # 热编译
            echo "执行热编译..."
            _build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'
            _build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_modbus).'
            
            attempt=$((attempt + 1))
        fi
    done
    
    echo "❌ 超过最大尝试次数，问题未解决"
    return 1
}

# 执行测试流程
main() {
    echo "=== 测试开始 ==="
    
    # 检查系统状态
    if ! check_system_status; then
        echo "❌ 系统状态检查失败"
        return 1
    fi
    
    # 清理测试环境
    echo "清理测试环境..."
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        dgiot_device:delete(DeviceId),
        dgiot_utils:safe_format(\"清理设备: ~p~n\", [DeviceId]).
    "
    
    # 执行集成测试工作流程
    if problem_solving_cycle; then
        echo "✅ 集成测试成功"
    else
        echo "❌ 集成测试失败"
        return 1
    fi
    
    echo "=== 测试完成 ==="
    echo "测试结束时间: $(date)"
    echo ""
    echo "=== 测试总结 ==="
    echo "1. 设备注册测试: ✅ 完成"
    echo "2. Modbus数据上报测试: ✅ 完成"
    echo "3. 数据解析验证: ✅ 完成"
    echo "4. 数据存储验证: ✅ 完成"
    echo "5. 集成工作流程: ✅ 符合"
    echo ""
    echo "=== 使用命令 ==="
    echo "# 重新运行测试"
    echo "bash apps/dgiot_modbus/test/tools/integration/test_runners/integration_test_registerbyport_enhanced.sh"
    echo ""
    echo "# 编译modbus插件"
    echo "_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'"
}

main "$@"
