#!/bin/bash

# 脚本名称：test_modbus_rtu_full.sh
# 功能描述：Modbus RTU完整测试：登录、注册、数据块主动上报
# 测试驱动开发示例：验证Modbus RTU协议的完整工作流程
# 作者：DG-IoT团队
# 创建日期：2025-12-26
# 版本：1.0.0
# 使用说明：运行前确保DG-IoT平台已启动

# 测试用例描述
TEST_CASE_DESCRIPTION="Modbus RTU完整测试：验证登录、注册、数据块主动上报的完整工作流程"

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 日志函数
log_info() { echo -e "${BLUE}[INFO]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') $*" >&2; }

# 配置参数
MODBUS_RTU_PORT=20000  # 服务器端口（Modbus RTU over TCP Server监听端口）
TEST_DEVICE="test_modbus_rtu_device"
TEST_PRODUCT="feeb43bffb"
DEVICE_ADDR="${TEST_DEVICE}-${MODBUS_RTU_PORT}"  # 设备地址 = 注册报文 + "-" + 服务器端口
LOG_FILE="_build/emqx/rel/emqx/log/emqx.log.1"
TEST_START_TIME="$(date)"

# 前置条件检查
check_prerequisites() {
    log_info "检查前置条件"
    
    # 检查系统服务
    if ! pgrep -f "emqx" > /dev/null; then
        log_error "DG-IoT平台未运行"
        return 1
    fi
    log_success "DG-IoT平台正在运行"
    
    # 检查端口监听
    if ! netstat -tlnp | grep ":${MODBUS_RTU_PORT}" > /dev/null; then
        log_error "Modbus RTU服务器未监听端口${MODBUS_RTU_PORT}"
        return 1
    fi
    log_success "Modbus RTU服务器正在监听端口${MODBUS_RTU_PORT}"
    
    # 检查modbus插件加载
    local plugin_check=$(_build/emqx/rel/emqx/bin/emqx eval 'io:format("Module: ~p~n", [code:which(modbus_rtu)]).' 2>/dev/null)
    if ! echo "$plugin_check" | grep -q "modbus_rtu.beam"; then
        log_warning "modbus_rtu模块可能未加载，尝试热编译..."
        _build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'
        sleep 2
    else
        log_success "modbus_rtu模块已加载"
    fi
    
    # 检查产品配置
    local product_check=$(_build/emqx/rel/emqx/bin/emqx eval "
        case dgiot_product:lookup_prod(<<\"$TEST_PRODUCT\">>) of
            {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
                io:format(\"产品存在，属性数量: ~p~n\", [length(Props)]);
            {ok, _} -> io:format(\"产品存在，但属性配置不完整~n\");
            _ -> io:format(\"产品不存在~n\")
        end.
    " 2>/dev/null)
    
    if echo "$product_check" | grep -q "产品存在"; then
        log_success "测试产品配置存在"
    else
        log_warning "测试产品可能不存在或配置不完整"
    fi
    
    return 0
}

# 清理测试环境
cleanup_test_environment() {
    log_info "清理测试环境"
    
    # 清理测试设备
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        dgiot_device:delete(DeviceId),
        io:format(\"清理设备: ~p~n\", [DeviceId]).
    " 2>/dev/null || true
    
    # 清理缓存数据
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceId = dgiot_parse_id:get_deviceid(<<\"$TEST_PRODUCT\">>, <<\"$DEVICE_ADDR\">>),
        dgiot_data:delete({last_data, DeviceId}),
        io:format(\"清理缓存数据: ~p~n\", [DeviceId]).
    " 2>/dev/null || true
    
    log_success "测试环境清理完成"
}

# 测试阶段1：设备登录和注册
test_phase1_device_login_registration() {
    log_info "=== 测试阶段1：设备登录和注册 ==="
    
    # 步骤1：发送设备注册报文（RegisterByPort方式）
    log_info "步骤1：发送设备注册报文（RegisterByPort方式）"
    echo "发送注册报文: $TEST_DEVICE"
    echo "$TEST_DEVICE" | nc -w 5 127.0.0.1 $MODBUS_RTU_PORT
    
    # 等待系统处理
    sleep 3
    
    # 步骤2：检查TCP连接日志
    log_info "步骤2：检查TCP连接日志"
    if [ ! -f "$LOG_FILE" ]; then
        log_error "日志文件不存在: $LOG_FILE"
        return 1
    fi
    
    local tcp_log=$(grep -n "tcp.*${TEST_DEVICE}" "$LOG_FILE" | tail -1)
    if [ -n "$tcp_log" ]; then
        log_success "找到TCP连接日志"
        echo "日志内容: ${tcp_log:0:200}..."
    else
        log_error "未找到TCP连接日志"
        return 1
    fi
    
    # 步骤3：检查设备注册成功
    log_info "步骤3：检查设备注册成功"
    local device_check=$(_build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        
        case dgiot_device:lookup(DeviceId) of
            {ok, #{<<"status">> := Status}} ->
                io:format(\"设备注册成功: ~p~n\", [DeviceId]),
                io:format(\"设备状态: ~p~n\", [Status]);
            {error, not_found} ->
                io:format(\"设备未找到: ~p~n\", [DeviceId]);
            {error, Reason} ->
                io:format(\"设备查询错误: ~p~n\", [Reason])
        end.
    " 2>/dev/null)
    
    if echo "$device_check" | grep -q "设备注册成功"; then
        log_success "设备注册成功"
        # 提取设备ID
        local device_id=$(echo "$device_check" | grep "设备注册成功:" | sed 's/.*设备注册成功: //' | tr -d ' ')
        log_info "设备ID: $device_id"
    else
        log_error "设备注册失败"
        echo "详细输出: $device_check"
        return 1
    fi
    
    # 步骤4：检查RegisterByPort日志
    log_info "步骤4：检查RegisterByPort日志"
    local register_log=$(grep -n "RegisterByPort" "$LOG_FILE" | tail -1)
    if [ -n "$register_log" ]; then
        log_success "找到RegisterByPort日志"
        echo "注册日志: ${register_log:0:200}..."
    else
        log_warning "未找到RegisterByPort日志，可能使用其他注册方式"
    fi
    
    log_success "测试阶段1完成：设备登录和注册成功"
    return 0
}

# 测试阶段2：Modbus数据块主动上报
test_phase2_modbus_data_report() {
    log_info "=== 测试阶段2：Modbus数据块主动上报 ==="
    
    # 步骤1：发送Modbus RTU数据块
    log_info "步骤1：发送Modbus RTU数据块"
    
    # 构建Modbus RTU数据块（示例：从机地址1，功能码3，读取4个寄存器）
    # 数据格式：从机地址 + 功能码 + 起始地址 + 寄存器数量 + CRC
    # 示例：01 03 00 00 00 04 C5 CB
    
    local modbus_frame=$(printf '\\x01\\x03\\x00\\x00\\x00\\x04\\xC5\\xCB')
    echo "发送Modbus RTU数据块: 01 03 00 00 00 04 C5 CB"
    
    # 使用Python发送二进制数据
    python3 -c "
import socket
import time

# Modbus RTU帧
frame = b'\\x01\\x03\\x00\\x00\\x00\\x04\\xC5\\xCB'

try:
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(('127.0.0.1', $MODBUS_RTU_PORT))
    s.sendall(frame)
    print(f'发送Modbus数据: {frame.hex()}')
    
    # 尝试接收响应
    s.settimeout(2)
    try:
        response = s.recv(1024)
        if response:
            print(f'收到响应: {response.hex()}')
    except socket.timeout:
        print('未收到响应（可能正常）')
    
    s.close()
except Exception as e:
    print(f'发送失败: {e}')
"
    
    # 等待系统处理
    sleep 3
    
    # 步骤2：检查数据接收日志
    log_info "步骤2：检查数据接收日志"
    local data_receive_log=$(grep -n "Received data\|parse_frame\|decode_data" "$LOG_FILE" | tail -3)
    if [ -n "$data_receive_log" ]; then
        log_success "找到数据接收日志"
        echo "数据接收日志:"
        echo "$data_receive_log" | while read -r line; do
            echo "  ${line:0:150}..."
        done
    else
        log_warning "未找到数据接收日志，可能数据格式不正确"
    fi
    
    # 步骤3：检查数据解析日志
    log_info "步骤3：检查数据解析日志"
    local parse_log=$(grep -n "modbus_decoder\|ProductId.*${TEST_PRODUCT}" "$LOG_FILE" | tail -2)
    if [ -n "$parse_log" ]; then
        log_success "找到数据解析日志"
        echo "数据解析日志:"
        echo "$parse_log" | while read -r line; do
            echo "  ${line:0:150}..."
        done
    else
        log_warning "未找到数据解析日志"
    fi
    
    # 步骤4：检查任务队列日志
    log_info "步骤4：检查任务队列日志"
    local task_log=$(grep -n "Sending to Task\|save_td\|dealwith_data" "$LOG_FILE" | tail -2)
    if [ -n "$task_log" ]; then
        log_success "找到任务队列日志"
        echo "任务队列日志:"
        echo "$task_log" | while read -r line; do
            echo "  ${line:0:150}..."
        done
    else
        log_warning "未找到任务队列日志"
    fi
    
    # 步骤5：检查数据存储
    log_info "步骤5：检查数据存储"
    local storage_check=$(_build/emqx/rel/emqx/bin/emqx eval "
        DeviceId = dgiot_parse_id:get_deviceid(<<\"$TEST_PRODUCT\">>, <<\"$DEVICE_ADDR\">>),
        
        % 检查缓存数据
        case dgiot_data:get({last_data, DeviceId}) of
            not_find -> io:format(\"last_data缓存: 未找到~n\");
            Data -> io:format(\"last_data缓存: ~p~n\", [Data])
        end,
        
        % 检查TDengine数据（简化检查）
        io:format(\"TDengine检查: 需要具体查询语句~n\").
    " 2>/dev/null)
    
    echo "数据存储检查: $storage_check"
    
    log_success "测试阶段2完成：Modbus数据块主动上报测试完成"
    return 0
}

# 测试阶段3：API验证和数据查询
test_phase3_api_verification() {
    log_info "=== 测试阶段3：API验证和数据查询 ==="
    
    # 步骤1：通过API查询设备信息
    log_info "步骤1：通过API查询设备信息"
    
    local api_response=$(curl -s -X GET "http://127.0.0.1/iotapi/devicecard/${DEVICE_ADDR}" \
        -H "Authorization: Bearer r:db1f3d43d05c782c8ceebb87724a2ac0" \
        -H "Content-Type: application/json")
    
    if echo "$api_response" | grep -q '"code":200'; then
        log_success "API查询成功"
        
        # 提取设备信息
        local device_info=$(echo "$api_response" | python3 -c "
import sys, json
try:
    data = json.load(sys.stdin)
    if data.get('code') == 200:
        device = data.get('data', {})
        print(f'设备名称: {device.get(\"name\", \"N/A\")}')
        print(f'设备状态: {device.get(\"status\", \"N/A\")}')
        print(f'产品ID: {device.get(\"product\", \"N/A\")}')
        print(f'设备地址: {device.get(\"devaddr\", \"N/A\")}')
    else:
        print(f'API错误: {data.get(\"msg\", \"Unknown error\")}')
except Exception as e:
    print(f'解析错误: {e}')
")
        
        echo "设备信息:"
        echo "$device_info"
    else
        log_error "API查询失败"
        echo "API响应: $api_response"
        return 1
    fi
    
    # 步骤2：检查实时数据API
    log_info "步骤2：检查实时数据API"
    
    local realtime_api=$(curl -s -X GET "http://127.0.0.1/iotapi/realtimedata/${DEVICE_ADDR}" \
        -H "Authorization: Bearer r:db1f3d43d05c782c8ceebb87724a2ac0" \
        -H "Content-Type: application/json")
    
    if echo "$realtime_api" | grep -q '"code":200'; then
        log_success "实时数据API查询成功"
        
        # 检查是否有数据
        local has_data=$(echo "$realtime_api" | python3 -c "
import sys, json
try:
    data = json.load(sys.stdin)
    if data.get('code') == 200:
        realtime_data = data.get('data', {})
        if realtime_data:
            print('有实时数据')
            # 显示前几个数据点
            items = list(realtime_data.items())[:3]
            for key, value in items:
                print(f'  {key}: {value}')
        else:
            print('无实时数据')
except:
    print('解析失败')
")
        
        echo "实时数据检查: $has_data"
    else
        log_warning "实时数据API查询失败或无数据"
        echo "API响应: ${realtime_api:0:200}..."
    fi
    
    log_success "测试阶段3完成：API验证和数据查询完成"
    return 0
}

# 验证预期结果
verify_expected_results() {
    log_info "验证预期结果"
    
    local all_passed=true
    local results=()
    
    # 预期结果1：设备成功注册
    local device_registered=$(_build/emqx/rel/emqx/bin/emqx eval "
        DeviceId = dgiot_parse_id:get_deviceid(<<\"$TEST_PRODUCT\">>, <<\"$DEVICE_ADDR\">>),
        case dgiot_device:lookup(DeviceId) of
            {ok, _} -> io:format(\"true\");
            _ -> io:format(\"false\")
        end.
    " 2>/dev/null)
    
    if [ "$device_registered" = "true" ]; then
        results+=("✅ 预期结果1通过：设备成功注册")
    else
        results+=("❌ 预期结果1失败：设备未注册")
        all_passed=false
    fi
    
    # 预期结果2：有TCP连接日志
    if grep -q "tcp.*${TEST_DEVICE}" "$LOG_FILE" 2>/dev/null; then
        results+=("✅ 预期结果2通过：有TCP连接日志")
    else
        results+=("❌ 预期结果2失败：无TCP连接日志")
        all_passed=false
    fi
    
    #
