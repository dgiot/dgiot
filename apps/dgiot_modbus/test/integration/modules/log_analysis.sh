#!/bin/bash

# 错误处理
set -euo pipefail
trap 'echo "脚本执行失败: $?" >&2' ERR
#!/bin/bash
# log_analysis.sh - 日志分析模块

# 模块5: 日志分析
module_log_analysis() {
    log_info "模块5: 日志分析"
    
    if [ ! -f "$LOG_FILE" ]; then
        log_error "日志文件不存在: $LOG_FILE"
        return 1
    fi
    
    echo "5.1 检查关键日志..."
    
    # 获取测试开始时间
    local test_start_time=$(date -d "$TEST_START_TIME" +%s 2>/dev/null || echo "$(date +%s)")
    
    # 检查TCP连接日志
    echo "检查TCP连接日志..."
    TCP_LOG=$(grep -n "tcp.*${TEST_DEVICE}" "$LOG_FILE" | tail -1)
    if [ -n "$TCP_LOG" ]; then
        log_success "找到TCP连接日志"
        analyze_log_line "$TCP_LOG" "$test_start_time"
    else
        log_warning "未找到TCP连接日志"
    fi
    
    # 检查RegisterByPort日志
    echo "检查RegisterByPort日志..."
    REGISTER_LOG=$(grep -n "RegisterByPort" "$LOG_FILE" | tail -1)
    if [ -n "$REGISTER_LOG" ]; then
        log_success "找到RegisterByPort日志"
        analyze_log_line "$REGISTER_LOG" "$test_start_time"
    else
        log_warning "未找到RegisterByPort日志"
    fi
    
    # 检查Modbus数据解析日志
    echo "检查Modbus数据解析日志..."
    MODBUS_LOG=$(grep -n "parse_frame\|decode_data\|Received data" "$LOG_FILE" | tail -3)
    if [ -n "$MODBUS_LOG" ]; then
        log_success "找到Modbus数据解析日志"
        echo "$MODBUS_LOG" | while read -r line; do
            analyze_log_line "$line" "$test_start_time"
        done
    else
        log_warning "未找到Modbus数据解析日志"
    fi
    
    # 检查错误日志
    echo "检查错误日志..."
    ERROR_LOG=$(grep -n "error.*case_clause\|error.*fasle\|error.*badarg\|error.*function_clause" "$LOG_FILE" | tail -3)
    if [ -n "$ERROR_LOG" ]; then
        log_error "找到错误日志"
        echo "$ERROR_LOG" | while read -r line; do
            analyze_log_line "$line" "$test_start_time"
        done
        return 1
    else
        log_success "未找到错误日志"
    fi
    
    # 分析文件行号日志
    echo "5.2 分析文件行号日志..."
    analyze_file_line_logs "$test_start_time"
    
    return 0
}

# 分析日志行
analyze_log_line() {
    local log_line="$1"
    local test_start_time="$2"
    
    # 提取行号和内容
    local line_num=$(echo "$log_line" | cut -d: -f1)
    local content=$(echo "$log_line" | cut -d: -f2-)
    
    # 提取时间戳（如果存在）
    local timestamp=""
    if echo "$content" | grep -q '"time"'; then
        timestamp=$(echo "$content" | grep -o '"time":[0-9]*' | cut -d: -f2)
    fi
    
    # 提取设备ID（如果存在）
    local device_id=""
    if echo "$content" | grep -q "DeviceId\|device_id"; then
        device_id=$(echo "$content" | grep -o "DeviceId[^,}\"]*" | cut -d: -f2 | tr -d ' <<>>' | head -1)
        if [ -z "$device_id" ]; then
            device_id=$(echo "$content" | grep -o "device_id[^,}\"]*" | cut -d: -f2 | tr -d ' <<>>' | head -1)
        fi
    fi
    
    # 提取产品ID（如果存在）
    local product_id=""
    if echo "$content" | grep -q "ProductId\|product_id"; then
        product_id=$(echo "$content" | grep -o "ProductId[^,}\"]*" | cut -d: -f2 | tr -d ' <<>>' | head -1)
        if [ -z "$product_id" ]; then
            product_id=$(echo "$content" | grep -o "product_id[^,}\"]*" | cut -d: -f2 | tr -d ' <<>>' | head -1)
        fi
    fi
    
    # 提取设备地址（如果存在）
    local device_addr=""
    if echo "$content" | grep -q "DtuAddr\|DevAddr\|dtu_addr\|dev_addr"; then
        device_addr=$(echo "$content" | grep -o "DtuAddr[^,}\"]*" | cut -d: -f2 | tr -d ' <<>>' | head -1)
        if [ -z "$device_addr" ]; then
            device_addr=$(echo "$content" | grep -o "DevAddr[^,}\"]*" | cut -d: -f2 | tr -d ' <<>>' | head -1)
        fi
        if [ -z "$device_addr" ]; then
            device_addr=$(echo "$content" | grep -o "dtu_addr[^,}\"]*" | cut -d: -f2 | tr -d ' <<>>' | head -1)
        fi
        if [ -z "$device_addr" ]; then
            device_addr=$(echo "$content" | grep -o "dev_addr[^,}\"]*" | cut -d: -f2 | tr -d ' <<>>' | head -1)
        fi
    fi
    
    # 提取文件路径和行号（如果存在）
    local file_path=""
    local file_line=""
    if echo "$content" | grep -q "\.erl.*[0-9]\+"; then
        # 匹配类似 "dgiot_modbusrtu_tcp.erl:123" 的格式
        file_path=$(echo "$content" | grep -o "[a-zA-Z0-9_/\.]\+\.erl" | head -1)
        file_line=$(echo "$content" | grep -o "\.erl:[0-9]\+" | cut -d: -f2 | head -1)
    fi
    
    # 提取模块和函数（如果存在）
    local module_func=""
    if echo "$content" | grep -q "mfa"; then
        module_func=$(echo "$content" | grep -o '"mfa":"[^"]*"' | cut -d: -f2 | tr -d '"')
    fi
    
    # 打印分析结果
    echo "   日志行号: $line_num"
    
    # 时空对应分析
    if [ -n "$timestamp" ]; then
        # 转换时间戳为可读格式
        local log_time=$(date -d "@$((timestamp / 1000000))" "+%Y-%m-%d %H:%M:%S.%3N" 2>/dev/null || echo "未知时间")
        local time_diff=""
        if [ -n "$test_start_time" ] && [ "$test_start_time" -gt 0 ]; then
            local log_sec=$((timestamp / 1000000))
            local log_ns=$((timestamp % 1000000))
            time_diff=$((log_sec - test_start_time))
            echo "   精确时间: $log_time (测试开始后 ${time_diff}.${log_ns}秒)"
        else
            echo "   精确时间: $log_time"
        fi
    fi
    
    # 设备关联分析
    if [ -n "$device_id" ]; then
        echo "   设备ID: $device_id"
    fi
    if [ -n "$product_id" ]; then
        echo "   产品ID: $product_id"
    fi
    if [ -n "$device_addr" ]; then
        echo "   设备地址: $device_addr"
    fi
    
    # 代码位置分析
    if [ -n "$file_path" ] && [ -n "$file_line" ]; then
        echo "   代码位置: $file_path:$file_line"
    fi
    if [ -n "$module_func" ]; then
        echo "   模块函数: $module_func"
    fi
    
    # 提取关键信息
    echo "   内容摘要: ${content:0:120}..."
    echo ""
}

# 分析文件行号日志
analyze_file_line_logs() {
    local test_start_time="$1"
    
    echo "分析包含文件行号的日志..."
    
    # 查找包含文件路径和行号的日志
    local file_logs=$(grep -n "\.erl:[0-9]\+" "$LOG_FILE" | tail -5)
    
    if [ -n "$file_logs" ]; then
        log_success "找到包含文件行号的日志"
        echo "$file_logs" | while read -r line; do
            analyze_log_line "$line" "$test_start_time"
        done
    else
        log_warning "未找到包含文件行号的日志"
    fi
    
    # 查找时间戳相关的日志
    echo "分析时间戳日志..."
    local timestamp_logs=$(grep -n '"time":[0-9]\+' "$LOG_FILE" | tail -3)
    
    if [ -n "$timestamp_logs" ]; then
        log_success "找到时间戳日志"
        echo "$timestamp_logs" | while read -r line; do
            analyze_log_line "$line" "$test_start_time"
        done
    else
        log_warning "未找到时间戳日志"
    fi
}

# 精准时空对应分析
analyze_spatiotemporal_correlation() {
    log_info "执行精准时空对应分析..."
    
    if [ ! -f "$LOG_FILE" ]; then
        log_error "日志文件不存在"
        return 1
    fi
    
    local test_start_time=$(date -d "$TEST_START_TIME" +%s 2>/dev/null || echo "$(date +%s)")
    
    echo "5.3 精准时空对应分析"
    echo "========================================"
    
    # 1. 分析设备相关日志的时间线
    echo "1. 设备相关日志时间线分析:"
    local device_logs=$(grep -n "${TEST_DEVICE}\|${DEVICE_ADDR}\|${TEST_PRODUCT}" "$LOG_FILE" | tail -10)
    
    if [ -n "$device_logs" ]; then
        echo "找到设备相关日志:"
        echo "$device_logs" | while read -r line; do
            echo "  ----------------------------------------"
            analyze_log_line "$line" "$test_start_time"
        done
    else
        echo "未找到设备相关日志"
    fi
    
    # 2. 分析测试事件的时间对应关系
    echo ""
    echo "2. 测试事件时间对应关系:"
    
    # 查找发包时间相关的日志
    local send_time_logs=$(grep -n "send\|Send\|SEND\|发包\|发送" "$LOG_FILE" | tail -5)
    if [ -n "$send_time_logs" ]; then
        echo "发包相关日志:"
        echo "$send_time_logs" | while read -r line; do
            echo "  ----------------------------------------"
            analyze_log_line "$line" "$test_start_time"
        done
    fi
    
    # 3. 分析Modbus数据处理的时间线
    echo ""
    echo "3. Modbus数据处理时间线:"
    local modbus_flow_logs=$(grep -n "parse_frame\|decode_data\|Received data\|save_td\|dealwith_data" "$LOG_FILE" | tail -10)
    
    if [ -n "$modbus_flow_logs" ]; then
        echo "Modbus数据处理流程:"
        echo "$modbus_flow_logs" | while read -r line; do
            echo "  ----------------------------------------"
            analyze_log_line "$line" "$test_start_time"
        done
    fi
    
    # 4. 生成时空对应总结
    echo ""
    echo "4. 时空对应总结:"
    echo "   测试开始时间: $(date -d "@$test_start_time" "+%Y-%m-%d %H:%M:%S")"
    echo "   测试设备: $TEST_DEVICE"
    echo "   设备地址: $DEVICE_ADDR"
    echo "   产品ID: $TEST_PRODUCT"
    echo "   日志文件: $LOG_FILE"
    
    # 计算时间范围
    local first_log=$(head -1 "$LOG_FILE" | grep -o '"time":[0-9]*' | cut -d: -f2)
    local last_log=$(tail -1 "$LOG_FILE" | grep -o '"time":[0-9]*' | cut -d: -f2)
    
    if [ -n "$first_log" ] && [ -n "$last_log" ]; then
        local first_time=$(date -d "@$((first_log / 1000000))" "+%H:%M:%S" 2>/dev/null || echo "未知")
        local last_time=$(date -d "@$((last_log / 1000000))" "+%H:%M:%S" 2>/dev/null || echo "未知")
        local duration=$(((last_log - first_log) / 1000000))
        echo "   日志时间范围: $first_time - $last_time (持续 ${duration}秒)"
    fi
    
    return 0
}

# 检查错误日志
check_error_logs() {
    log_info "检查错误日志..."
    
    if [ ! -f "$LOG_FILE" ]; then
        log_warning "日志文件不存在，跳过错误日志检查"
        return 1
    fi
    
    # 检查是否有严重错误
    local error_count=$(grep -c "error.*case_clause\|error.*fasle\|error.*badarg\|error.*function_clause" "$LOG_FILE")
    
    if [ "$error_count" -gt 0 ]; then
        log_error "发现 $error_count 个错误日志"
        
        # 分析每个错误日志
        grep -n "error.*case_clause\|error.*fasle\|error.*badarg\|error.*function_clause" "$LOG_FILE" | tail -3 | while read -r line; do
            echo "  错误详情:"
            analyze_log_line "$line" "$(date +%s)"
        done
        
        return 0  # 返回0表示有错误
    else
        log_success "未发现错误日志"
        return 1  # 返回1表示没有错误
    fi
}
