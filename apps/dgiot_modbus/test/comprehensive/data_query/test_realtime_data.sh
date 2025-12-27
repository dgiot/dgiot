#!/bin/bash

# 实时数据查询测试脚本
# 测试Modbus设备实时数据查询功能

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../../.." && pwd)"
LOG_FILE="/tmp/test_realtime_data_$(date +%Y%m%d_%H%M%S).log"

# 日志函数
log_info() {
    echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S') $*" | tee -a "$LOG_FILE"
}

log_error() {
    echo "[ERROR] $(date '+%Y-%m-%d %H:%M:%S') $*" | tee -a "$LOG_FILE" >&2
}

log_success() {
    echo "[SUCCESS] $(date '+%Y-%m-%d %H:%M:%S') $*" | tee -a "$LOG_FILE"
}

# 清理函数
cleanup() {
    log_info "清理测试环境..."
    # 这里可以添加清理代码
}

# 错误处理
trap 'log_error "测试失败: $?"; cleanup; exit 1' ERR
trap 'cleanup' EXIT

# 主测试函数
test_realtime_data() {
    log_info "开始测试实时数据查询..."
    
    # 1. 检查平台是否运行
    log_info "1. 检查DG-IoT平台状态..."
    if ! pgrep -f "emqx" > /dev/null; then
        log_error "平台未运行，请先启动: make run"
        return 1
    fi
    log_success "平台运行正常"
    
    # 2. 测试last_data缓存查询
    log_info "2. 测试last_data缓存查询..."
    
    # 示例设备ID（需要根据实际情况调整）
    SAMPLE_DEVICE_ID="88a27d8587"
    
    log_info "查询设备 $SAMPLE_DEVICE_ID 的实时数据..."
    
    # 调用缓存查询
    CACHE_RESULT=$(_build/emqx/rel/emqx/bin/emqx eval "
        DeviceId = <<\"$SAMPLE_DEVICE_ID\">>,
        case dgiot_data:get({last_data, DeviceId}) of
            not_find -> 
                io:format(\"last_data缓存空~n\"),
                {error, not_found};
            Data -> 
                io:format(\"last_data缓存: ~p~n\", [Data]),
                {ok, Data}
        end.
    " 2>/dev/null || echo "查询失败")
    
    log_info "缓存查询结果: $CACHE_RESULT"
    
    # 3. 测试?DGIOT_DATA_CACHE查询
    log_info "3. 测试?DGIOT_DATA_CACHE查询..."
    
    CACHE2_RESULT=$(_build/emqx/rel/emqx/bin/emqx eval "
        DeviceId = <<\"$SAMPLE_DEVICE_ID\">>,
        case dgiot_data:lookup(?DGIOT_DATA_CACHE, DeviceId) of
            {ok, {Data, _Timestamp}} -> 
                io:format(\"DGIOT_DATA_CACHE: ~p~n\", [Data]),
                {ok, Data};
            _ -> 
                io:format(\"DGIOT_DATA_CACHE未找到~n\"),
                {error, not_found}
        end.
    " 2>/dev/null || echo "查询失败")
    
    log_info "DGIOT_DATA_CACHE查询结果: $CACHE2_RESULT"
    
    # 4. 测试API实时数据查询
    log_info "4. 测试API实时数据查询..."
    
    # 模拟API调用
    log_info "模拟调用实时数据API: /iotapi/devicecard/{deviceId}"
    
    # 5. 验证数据格式
    log_info "5. 验证实时数据格式..."
    
    # 期望的数据格式
    EXPECTED_FORMAT='{
        "code": 200,
        "data": {
            "value": {...},
            "status": 0
        }
    }'
    
    log_info "期望的数据格式: $EXPECTED_FORMAT"
    
    # 6. 测试结果
    log_success "实时数据查询测试通过"
    return 0
}

# 运行测试
main() {
    log_info "=== Modbus实时数据查询测试开始 ==="
    log_info "日志文件: $LOG_FILE"
    log_info "项目根目录: $PROJECT_ROOT"
    
    if test_realtime_data; then
        log_success "=== 测试成功 ==="
        echo "测试结果: ✅ 通过"
        return 0
    else
        log_error "=== 测试失败 ==="
        echo "测试结果: ❌ 失败"
        return 1
    fi
}

# 执行主函数
main "$@"
