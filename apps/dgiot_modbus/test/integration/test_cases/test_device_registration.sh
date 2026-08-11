#!/bin/bash
# 脚本名称: test_device_registration.sh
# 功能描述: 设备注册测试用例
# 作者: DG-IoT团队
# 创建日期: 2025-12-26
# 版本: 1.0.0
# 使用说明: 运行前确保DG-IoT平台已启动

# 错误处理
set -euo pipefail
trap 'echo "脚本执行失败: $?" >&2' ERR



#!/bin/bash
# test_device_registration.sh - 设备注册测试用例（测试驱动开发示例）

# 测试用例描述
TEST_CASE_DESCRIPTION="设备注册测试用例：验证设备能够成功注册到系统"

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
TEST_DEVICE="wrj_dm-zqy"
TEST_PORT=20000  # 服务器端口（Modbus RTU over TCP Server监听端口）
TEST_PRODUCT="feeb43bffb"
DEVICE_ADDR="${TEST_DEVICE}-${TEST_PORT}"  # 设备地址 = 注册报文 + "-" + 服务器端口
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
    if ! netstat -tlnp | grep ":${TEST_PORT}" > /dev/null; then
        log_error "服务器未监听端口${TEST_PORT}"
        return 1
    fi
    log_success "服务器正在监听端口${TEST_PORT}"
    
    # 检查插件加载
    local plugin_check=$(_build/emqx/rel/emqx/bin/emqx eval 'io:format("Module: ~p~n", [code:which(modbus_rtu)]).' 2>/dev/null)
    if ! echo "$plugin_check" | grep -q "modbus_rtu.beam"; then
        log_warning "modbus_rtu模块可能未加载"
    else
        log_success "modbus_rtu模块已加载"
    fi
    
    return 0
}

# 清理测试环境
cleanup_test_environment() {
    log_info "清理测试环境"
    
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        dgiot_device:delete(DeviceId),
        io:format(\"清理设备: ~p~n\", [DeviceId]).
    " 2>/dev/null || true
    
    log_success "测试环境清理完成"
}

# 测试步骤1：发送设备注册报文
execute_test_step1_send_registration() {
    log_info "测试步骤1：发送设备注册报文"
    
    echo "发送设备注册报文: $TEST_DEVICE"
    echo "$TEST_DEVICE" | nc -w 5 127.0.0.1 $TEST_PORT
    
    # 等待系统处理
    sleep 2
    
    log_success "设备注册报文发送完成"
    return 0
}

# 测试步骤2：检查TCP连接日志
execute_test_step2_check_tcp_log() {
    log_info "测试步骤2：检查TCP连接日志"
    
    if [ ! -f "$LOG_FILE" ]; then
        log_error "日志文件不存在: $LOG_FILE"
        return 1
    fi
    
    # 检查TCP连接日志
    local tcp_log=$(grep -n "tcp.*${TEST_DEVICE}" "$LOG_FILE" | tail -1)
    if [ -n "$tcp_log" ]; then
        log_success "找到TCP连接日志"
        
        # 提取日志详细信息
        local line_num=$(echo "$tcp_log" | cut -d: -f1)
        local content=$(echo "$tcp_log" | cut -d: -f2-)
        
        # 提取时间戳
        local timestamp=$(echo "$content" | grep -o '"time":[0-9]*' | cut -d: -f2)
        if [ -n "$timestamp" ]; then
            local log_time=$(date -d "@$((timestamp / 1000000))" "+%Y-%m-%d %H:%M:%S.%3N" 2>/dev/null || echo "未知时间")
            log_info "TCP连接时间: $log_time"
        fi
        
        # 检查设备标识
        if echo "$content" | grep -q "$TEST_DEVICE"; then
            log_success "日志包含设备标识: $TEST_DEVICE"
        fi
        
        echo "日志内容摘要: ${content:0:100}..."
    else
        log_error "未找到TCP连接日志"
        return 1
    fi
    
    return 0
}

# 测试步骤3：检查设备注册成功
execute_test_step3_check_device_registration() {
    log_info "测试步骤3：检查设备注册成功"
    
    local device_check=$(_build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        
        case dgiot_device:lookup(DeviceId) of
            {ok, Device} ->
                io:format(\"设备存在: ~p~n\", [DeviceId]),
                io:format(\"设备信息: ~p~n\", [Device]);
            {error, not_found} ->
                io:format(\"设备未找到: ~p~n\", [DeviceId]);
            {error, Reason} ->
                io:format(\"设备查询错误: ~p~n\", [Reason])
        end.
    " 2>/dev/null)
    
    if echo "$device_check" | grep -q "设备存在"; then
        log_success "设备注册成功: $DEVICE_ADDR"
        
        # 提取设备ID
        local device_id=$(echo "$device_check" | grep "设备存在:" | sed 's/.*设备存在: //' | tr -d ' ')
        if [ -n "$device_id" ]; then
            log_info "设备ID: $device_id"
        fi
    else
        log_error "设备注册失败"
        echo "详细输出: $device_check"
        return 1
    fi
    
    return 0
}

# 测试步骤执行
execute_test_steps() {
    log_info "开始执行测试步骤"
    
    # 步骤1：发送注册报文
    if ! execute_test_step1_send_registration; then
        return 1
    fi
    
    # 步骤2：检查TCP连接日志
    if ! execute_test_step2_check_tcp_log; then
        return 1
    fi
    
    # 步骤3：检查设备注册成功
    if ! execute_test_step3_check_device_registration; then
        return 1
    fi
    
    return 0
}

# 预期结果验证
verify_expected_results() {
    log_info "验证预期结果"
    
    local all_passed=true
    
    # 预期结果1：TCP连接建立成功
    if grep -q "tcp.*${TEST_DEVICE}" "$LOG_FILE" 2>/dev/null; then
        log_success "✅ 预期结果1通过：TCP连接建立成功"
    else
        log_error "❌ 预期结果1失败：未找到TCP连接日志"
        all_passed=false
    fi
    
    # 预期结果2：设备在数据库中存在
    local device_exists=$(_build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        case dgiot_device:lookup(DeviceId) of
            {ok, _} -> io:format(\"true\");
            _ -> io:format(\"false\")
        end.
    " 2>/dev/null)
    
    if [ "$device_exists" = "true" ]; then
        log_success "✅ 预期结果2通过：设备在数据库中存在"
    else
        log_error "❌ 预期结果2失败：设备未在数据库中找到"
        all_passed=false
    fi
    
    # 预期结果3：设备信息完整
    local device_info=$(_build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        case dgiot_device:lookup(DeviceId) of
            {ok, #{<<"product">> := ProductId, <<"devaddr">> := DevAddr}} ->
                io:format(\"产品ID匹配: ~p~n\", [ProductId == <<\"$TEST_PRODUCT\">>]),
                io:format(\"设备地址匹配: ~p~n\", [DevAddr == <<\"$DEVICE_ADDR\">>]);
            _ -> io:format(\"设备信息不完整\")
        end.
    " 2>/dev/null)
    
    if echo "$device_info" | grep -q "产品ID匹配: true" && echo "$device_info" | grep -q "设备地址匹配: true"; then
        log_success "✅ 预期结果3通过：设备信息完整"
    else
        log_warning "⚠️  预期结果3警告：设备信息可能不完整"
        echo "设备信息检查: $device_info"
    fi
    
    if [ "$all_passed" = true ]; then
        log_success "所有预期结果验证通过"
        return 0
    else
        log_error "部分预期结果验证失败"
        return 1
    fi
}

# 日志规范验证
verify_log_specification() {
    log_info "验证日志规范"
    
    if [ ! -f "$LOG_FILE" ]; then
        log_warning "日志文件不存在，跳过日志规范验证"
        return 0
    fi
    
    local log_spec_passed=true
    
    # 检查日志是否包含时间戳
    local timestamp_logs=$(grep -c '"time":[0-9]\+' "$LOG_FILE")
    if [ "$timestamp_logs" -gt 0 ]; then
        log_success "✅ 日志规范1：包含时间戳（找到 $timestamp_logs 个）"
    else
        log_warning "⚠️  日志规范1：未找到时间戳格式的日志"
        log_spec_passed=false
    fi
    
    # 检查设备相关日志是否包含设备标识
    local device_logs=$(grep -c "$TEST_DEVICE\|$DEVICE_ADDR\|$TEST_PRODUCT" "$LOG_FILE")
    if [ "$device_logs" -gt 0 ]; then
        log_success "✅ 日志规范2：设备相关日志包含设备标识（找到 $device_logs 个）"
    else
        log_warning "⚠️  日志规范2：设备相关日志未找到设备标识"
        log_spec_passed=false
    fi
    
    # 检查是否包含文件行号信息
    local file_line_logs=$(grep -c "\.erl:[0-9]\+" "$LOG_FILE")
    if [ "$file_line_logs" -gt 0 ]; then
        log_success "✅ 日志规范3：包含代码位置信息（找到 $file_line_logs 个）"
    else
        log_warning "⚠️  日志规范3：未找到代码位置信息"
        # 这不是致命错误，只是警告
    fi
    
    if [ "$log_spec_passed" = true ]; then
        log_success "日志规范验证基本通过"
        return 0
    else
        log_warning "日志规范验证发现一些问题"
        return 1
    fi
}

# 时空对应分析
analyze_spatiotemporal_correlation() {
    log_info "执行时空对应分析"
    
    echo "========================================"
    echo "时空对应分析报告"
    echo "========================================"
    echo "测试用例: $TEST_CASE_DESCRIPTION"
    echo "测试开始时间: $TEST_START_TIME"
    echo "测试设备: $TEST_DEVICE"
    echo "设备地址: $DEVICE_ADDR"
    echo "产品ID: $TEST_PRODUCT"
    echo ""
    
    # 分析时间线
    echo "时间线分析:"
    local test_start_sec=$(date -d "$TEST_START_TIME" +%s 2>/dev/null || date +%s)
    
    # 查找第一个相关日志的时间
    local first_log=$(grep -n "$TEST_DEVICE" "$LOG_FILE" | head -1)
    if [ -n "$first_log" ]; then
        local content=$(echo "$first_log" | cut -d: -f2-)
        local timestamp=$(echo "$content" | grep -o '"time":[0-9]*' | cut -d: -f2)
        if [ -n "$timestamp" ]; then
            local log_sec=$((timestamp / 1000000))
            local time_diff=$((log_sec - test_start_sec))
            local log_time=$(date -d "@$log_sec" "+%H:%M:%S.%3N" 2>/dev/null || echo "未知")
            echo "  第一个设备日志时间: $log_time (测试开始后 ${time_diff}.$((timestamp % 1000000))秒)"
        fi
    fi
    
    # 查找最后一个相关日志的时间
    local last_log=$(grep -n "$TEST_DEVICE" "$LOG_FILE" | tail -1)
    if [ -n "$last_log" ]; then
        local content=$(echo "$last_log" | cut -d: -f2-)
        local timestamp=$(echo "$content" | grep -o '"time":[0-9]*' | cut -d: -f2)
        if [ -n "$timestamp" ]; then
            local log_sec=$((timestamp / 1000000))
            local time_diff=$((log_sec - test_start_sec))
            local log_time=$(date -d "@$log_sec" "+%H:%M:%S.%3N" 2>/dev/null || echo "未知")
            echo "  最后一个设备日志时间: $log_time (测试开始后 ${time_diff}.$((timestamp % 1000000))秒)"
        fi
    fi
    
    echo ""
    echo "设备标识关联分析:"
    echo "  测试设备标识: $TEST_DEVICE"
    echo "  在日志中出现次数: $(grep -c "$TEST_DEVICE" "$LOG_FILE" 2>/dev/null || echo 0)"
    
    echo ""
    echo "代码位置追踪:"
    local code_locations=$(grep "\.erl:[0-9]\+" "$LOG_FILE" | grep "$TEST_DEVICE" | head -3)
    if [ -n "$code_locations" ]; then
        echo "$code_locations" | while read -r line; do
            local file_line=$(echo "$line" | grep -o "[a-zA-Z0-9_/\.]\+\.erl:[0-9]\+" | head -1)
            if [ -n "$file_line" ]; then
                echo "  $file_line"
            fi
        done
    else
        echo "  未找到包含代码位置的设备相关日志"
    fi
    
    return 0
}

# 主函数
main() {
    echo "========================================"
    echo "测试用例: $TEST_CASE_DESCRIPTION"
    echo "测试开始时间: $TEST_START_TIME"
    echo "========================================"
    echo ""
    
    # 前置条件检查
    if ! check_prerequisites; then
        log_error "前置条件检查失败，测试中止"
        return 1
    fi
    
    # 清理测试环境
    cleanup_test_environment
    
    # 执行测试步骤
    if ! execute_test_steps; then
        log_error "测试步骤执行失败"
        return 1
    fi
    
    # 验证预期结果
    if ! verify_expected_results; then
        log_error "预期结果验证失败"
        return 1
    fi
    
    # 验证日志规范
    verify_log_specification
    
    # 时空对应分析
    analyze_spatiotemporal_correlation
    
    echo ""
    echo "========================================"
    log_success "测试用例执行完成"
    echo "测试结束时间: $(date)"
    echo "========================================"
    
    return 0
}

# 运行主函数
main "$@"
