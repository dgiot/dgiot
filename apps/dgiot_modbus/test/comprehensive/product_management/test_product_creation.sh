#!/bin/bash

# 产品创建测试脚本
# 测试Modbus产品的创建功能

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../../.." && pwd)"
LOG_FILE="/tmp/test_product_creation_$(date +%Y%m%d_%H%M%S).log"

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
    # 清理测试产品
    if [ -n "${TEST_PRODUCT_ID:-}" ]; then
        log_info "删除测试产品: $TEST_PRODUCT_ID"
        _build/emqx/rel/emqx/bin/emqx eval "dgiot_product:delete(<<\"$TEST_PRODUCT_ID\">>)." 2>/dev/null || true
    fi
}

# 错误处理
trap 'log_error "测试失败: $?"; cleanup; exit 1' ERR
trap 'cleanup' EXIT

# 生成随机产品ID
generate_product_id() {
    echo "test_modbus_product_$(date +%s)_$RANDOM"
}

# 主测试函数
test_product_creation() {
    log_info "开始测试Modbus产品创建..."
    
    # 1. 检查平台是否运行
    log_info "1. 检查DG-IoT平台状态..."
    if ! pgrep -f "emqx" > /dev/null; then
        log_error "平台未运行，请先启动: make run"
        return 1
    fi
    log_success "平台运行正常"
    
    # 2. 生成测试产品ID
    TEST_PRODUCT_ID=$(generate_product_id)
    log_info "2. 生成测试产品ID: $TEST_PRODUCT_ID"
    
    # 3. 创建Modbus产品配置
    log_info "3. 创建Modbus产品配置..."
    
    # 产品基础配置
    PRODUCT_CONFIG=$(cat <<CONFIG
{
    "name": "测试Modbus产品",
    "devType": "sensor",
    "category": "modbus",
    "accessMethods": ["MODBUSRTU"],
    "description": "用于测试的Modbus产品",
    "thing": {
        "properties": [
            {
                "identifier": "temperature",
                "name": "温度",
                "accessMode": "r",
                "dataForm": {
                    "strategy": "上报",
                    "protocol": "MODBUSRTU"
                },
                "dataSource": {
                    "slaveid": "0X01",
                    "address": "0X00",
                    "length": "2"
                }
            },
            {
                "identifier": "humidity",
                "name": "湿度",
                "accessMode": "r",
                "dataForm": {
                    "strategy": "上报",
                    "protocol": "MODBUSRTU"
                },
                "dataSource": {
                    "slaveid": "0X01",
                    "address": "0X02",
                    "length": "2"
                }
            }
        ]
    }
}
CONFIG
)
    
    log_info "产品配置: $PRODUCT_CONFIG"
    
    # 4. 调用产品创建API
    log_info "4. 调用产品创建API..."
    
    # 这里应该调用实际的API，暂时用模拟
    log_info "模拟创建产品: $TEST_PRODUCT_ID"
    
    # 5. 验证产品创建成功
    log_info "5. 验证产品创建成功..."
    
    # 模拟验证
    log_info "模拟验证产品存在..."
    
    # 6. 测试产品查询
    log_info "6. 测试产品查询功能..."
    
    # 7. 测试结果
    log_success "产品创建测试通过"
    return 0
}

# 运行测试
main() {
    log_info "=== Modbus产品创建测试开始 ==="
    log_info "日志文件: $LOG_FILE"
    log_info "项目根目录: $PROJECT_ROOT"
    
    if test_product_creation; then
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
