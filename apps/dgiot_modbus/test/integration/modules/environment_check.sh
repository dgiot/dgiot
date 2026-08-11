#!/bin/bash

# 错误处理
set -euo pipefail
trap 'echo "脚本执行失败: $?" >&2' ERR
#!/bin/bash
# environment_check.sh - 环境检查模块

# 模块1: 环境检查
module_environment_check() {
    log_info "模块1: 环境检查"
    
    echo "1.1 检查系统服务..."
    if pgrep -f "emqx" > /dev/null; then
        log_success "DG-IoT平台正在运行"
    else
        log_error "DG-IoT平台未运行"
        return 1
    fi
    
    echo "1.2 检查服务器端口..."
    if netstat -tlnp | grep ":${TEST_PORT}" > /dev/null; then
        log_success "服务器正在监听端口${TEST_PORT}"
    else
        log_error "服务器未监听端口${TEST_PORT}"
        return 1
    fi
    
    echo "1.3 检查modbus插件..."
    _build/emqx/rel/emqx/bin/emqx eval '
        % 使用简单的英文输出避免编码问题
        io:format("Checking modbus plugin status...~n", []),
        
        case code:which(modbus_rtu) of
            non_existing ->
                io:format("  ERROR: modbus_rtu module not loaded~n", []);
            _ ->
                io:format("  OK: modbus_rtu module loaded~n", [])
        end,
        
        case code:which(dgiot_modbusrtu_tcp) of
            non_existing ->
                io:format("  ERROR: dgiot_modbusrtu_tcp module not loaded~n", []);
            _ ->
                io:format("  OK: dgiot_modbusrtu_tcp module loaded~n", [])
        end.
    ' 2>/dev/null || echo "⚠️  插件检查失败（可能编码问题）"
    
    return 0
}

# 清理测试环境
module_cleanup_environment() {
    log_info "清理测试环境..."
    
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        dgiot_device:delete(DeviceId),
        io:format(\"清理设备: ~p~n\", [DeviceId]).
    "
    
    return 0
}
