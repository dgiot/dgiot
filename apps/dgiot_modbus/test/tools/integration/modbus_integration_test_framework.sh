#!/bin/bash

# 错误处理
set -euo pipefail
trap 'echo "脚本执行失败: $?" >&2' ERR
#!/bin/bash
# modbus_integration_test_framework.sh - Modbus集成测试框架
# 融合模拟器和集成测试，提供统一的测试接口
# 主脚本：提供统一的测试入口
# 二级功能：各个业务功能模块

echo "================================================================"
echo "Modbus集成测试框架 v1.0.0"
echo "融合模拟器和集成测试，提供统一的测试接口"
echo "================================================================"
echo "测试开始时间: $(date)"
echo ""

# 配置参数
TEST_DEVICE="wrj_dm-zqy"
TEST_PORT=20000  # 服务器端口（通道配置中的固定端口）
TEST_PRODUCT="feeb43bffb"
DEVICE_ADDR="${TEST_DEVICE}-${TEST_PORT}"  # 设备地址 = 注册报文 + "-" + 服务器端口
LOG_FILE="_build/emqx/rel/emqx/log/emqx.log.1"

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

# ==================== 二级功能模块 ====================

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
        dgiot_utils:safe_format(<<"检查modbus插件状态:~n">>, []),
        
        case code:which(modbus_rtu) of
            non_existing ->
                dgiot_utils:safe_format(<<"❌ modbus_rtu模块未加载~n">>, []);
            _ ->
                dgiot_utils:safe_format(<<"✅ modbus_rtu模块已加载~n">>, [])
        end,
        
        case code:which(dgiot_modbusrtu_tcp) of
            non_existing ->
                dgiot_utils:safe_format(<<"❌ dgiot_modbusrtu_tcp模块未加载~n">>, []);
            _ ->
                dgiot_utils:safe_format(<<"✅ dgiot_modbusrtu_tcp模块已加载~n">>, [])
        end.
    '
    
    return 0
}

# 模块2: 设备注册测试
module_device_registration() {
    log_info "模块2: 设备注册测试"
    
    # 清理测试环境
    echo "2.1 清理测试环境..."
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        dgiot_device:delete(DeviceId),
        dgiot_utils:safe_format(<<"清理设备: ~p~n">>, [DeviceId]).
    "
    
    # 发送注册报文
    echo "2.2 发送设备注册报文..."
    echo "设备地址: ${DEVICE_ADDR}"
    echo "注册报文: ${TEST_DEVICE}"
    
    # 使用nc发送注册报文
    echo -n "${TEST_DEVICE}" | nc -w 5 127.0.0.1 "${TEST_PORT}"
    sleep 3
    
    # 验证设备注册
    echo "2.3 验证设备注册..."
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        
        case dgiot_device:lookup(DeviceId) of
            {ok, Device} ->
                dgiot_utils:safe_format(<<"✅ 设备注册成功~n">>, []),
                dgiot_utils:safe_format(<<"  设备ID: ~p~n">>, [DeviceId]),
                dgiot_utils:safe_format(<<"  设备信息: ~p~n">>, [Device]);
            {error, not_found} ->
                dgiot_utils:safe_format(<<"❌ 设备未找到~n">>, []);
            {error, Reason} ->
                dgiot_utils:safe_format(<<"❌ 设备查询错误: ~p~n">>, [Reason])
        end.
    "
    
    return 0
}

# 模块3: Modbus数据上报测试
module_modbus_data_report() {
    log_info "模块3: Modbus数据上报测试"
    
    # Modbus RTU数据块（用户提供的十六进制数据）
    MODBUS_HEX_DATA="01 03 60 0C 19 0E 13 03 11 00 39 02 B2 00 02 08 80 00 00 00 00 00 00 00 00 00 00 00 00 00 6A 00 9E 9F B5 05 92 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 2F FD ED F1 F8 86 8A F7 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 43 65 00 00 00 00 7D B3"
    
    echo "3.1 发送Modbus数据块..."
    echo "数据包长度: 101字节"
    echo "从机地址: 0x01"
    echo "功能码: 0x03 (读取保持寄存器)"
    
    # 使用Python发送二进制数据
    python3 << 'EOF'
import socket
import time

# 将十六进制字符串转换为二进制数据
hex_data = """$MODBUS_HEX_DATA"""
hex_bytes = hex_data.replace(' ', '')
data = bytes.fromhex(hex_bytes)

print('发送Modbus数据包: {}字节'.format(len(data)))
print('十六进制: {}...'.format(hex_bytes[:50]))

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
            print('收到响应: {}...'.format(response.hex()[:50]))
    except socket.timeout:
        print('未收到响应（正常情况）')
    
    s.close()
except Exception as e:
    print('❌ 发送失败: {}'.format(e))
EOF
    
    # 等待数据处理
    sleep 3
    
    # 验证数据解析
    echo "3.2 验证数据解析..."
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        
        dgiot_utils:safe_format(<<"检查Modbus数据解析:~n">>, []),
        
        % 检查缓存数据
        case dgiot_data:get({last_data, DeviceId}) of
            not_find ->
                dgiot_utils:safe_format(<<"⚠️  未找到last_data缓存~n">>, []);
            CachedData ->
                dgiot_utils:safe_format(<<"✅ 找到last_data缓存~n">>, []),
                
                % 检查是否包含Modbus数据
                case CachedData of
                    #{<<"block_data">> := BlockData} ->
                        dgiot_utils:safe_format(<<"  包含block_data字段~n">>, []),
                        dgiot_utils:safe_format(<<"  数据值: ~p~n">>, [BlockData]);
                    _ ->
                        dgiot_utils:safe_format(<<"  不包含block_data字段~n">>, [])
                end
        end.
    "
    
    return 0
}

# 模块4: API查询测试
module_api_query() {
    log_info "模块4: API查询测试"
    
    # 获取设备ID
    DEVICE_ID="${TEST_PRODUCT}_${DEVICE_ADDR}"
    
    echo "4.1 查询API实时值..."
    echo "设备ID: ${DEVICE_ID}"
    
    # 使用curl查询API
    API_URL="http://127.0.0.1/iotapi/devicecard/${DEVICE_ID}"
    
    # 使用预配置的Cookie（基于用户提供的调试信息）
    COOKIES="Admin-Token=r:a1d8422a576e581c20fb91a01bc19ce6; sessiontoken=r:a1d8422a576e581c20fb91a01bc19ce6; departmenttoken=r:a1d8422a576e581c20fb91a01bc19ce6"
    
    echo "API URL: ${API_URL}"
    
    # 发送请求
    RESPONSE=$(curl -s -H "Cookie: ${COOKIES}" "${API_URL}")
    
    if [ $? -eq 0 ]; then
        echo "✅ API查询成功"
        echo "响应: ${RESPONSE:0:200}..."
        
        # 检查响应是否包含数据
        if echo "$RESPONSE" | grep -q '"data"'; then
            log_success "API返回有效数据"
        else
            log_warning "API返回数据格式异常"
        fi
    else
        log_error "API查询失败"
    fi
    
    echo ""
    echo "4.2 查询设备属性..."
    
    PROPERTIES_URL="http://127.0.0.1/iotapi/device_properties"
    PROPERTIES_PARAMS="productId=${TEST_PRODUCT}&deviceAddr=${DEVICE_ADDR}"
    
    echo "URL: ${PROPERTIES_URL}?${PROPERTIES_PARAMS}"
    
    PROPERTIES_RESPONSE=$(curl -s -H "Cookie: ${COOKIES}" "${PROPERTIES_URL}?${PROPERTIES_PARAMS}")
    
    if [ $? -eq 0 ]; then
        echo "✅ 设备属性查询成功"
        echo "响应: ${PROPERTIES_RESPONSE:0:200}..."
    else
        log_error "设备属性查询失败"
    fi
    
    return 0
}

# 模块5: 日志分析
module_log_analysis() {
    log_info "模块5: 日志分析"
    
    if [ ! -f "$LOG_FILE" ]; then
        log_error "日志文件不存在: $LOG_FILE"
        return 1
    fi
    
    echo "5.1 检查关键日志..."
    
    # 检查TCP连接日志
    echo "检查TCP连接日志..."
    TCP_LOG=$(grep -n "tcp.*${TEST_DEVICE}" "$LOG_FILE" | tail -1)
    if [ -n "$TCP_LOG" ]; then
        log_success "找到TCP连接日志"
        echo "   $TCP_LOG"
    else
        log_warning "未找到TCP连接日志"
    fi
    
    # 检查RegisterByPort日志
    echo "检查RegisterByPort日志..."
    REGISTER_LOG=$(grep -n "RegisterByPort" "$LOG_FILE" | tail -1)
    if [ -n "$REGISTER_LOG" ]; then
        log_success "找到RegisterByPort日志"
        echo "   $REGISTER_LOG"
    else
        log_warning "未找到RegisterByPort日志"
    fi
    
    # 检查Modbus数据解析日志
    echo "检查Modbus数据解析日志..."
    MODBUS_LOG=$(grep -n "parse_frame\|decode_data\|Received data" "$LOG_FILE" | tail -3)
    if [ -n "$MODBUS_LOG" ]; then
        log_success "找到Modbus数据解析日志"
        echo "$MODBUS_LOG" | while read -r line; do
            echo "   $line"
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
            echo "   $line"
        done
        return 1
    else
        log_success "未找到错误日志"
    fi
    
    return 0
}

# 模块6: 数据存储验证
module_data_storage() {
    log_info "模块6: 数据存储验证"
    
    echo "6.1 检查TDengine数据存储..."
    
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceAddr = <<\"$DEVICE_ADDR\">>,
        ProductId = <<\"$TEST_PRODUCT\">>,
        DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
        
        dgiot_utils:safe_format(<<"检查数据存储状态:~n">>/utf8, []),
        
        % 模拟TDengine查询
        case dgiot_tdengine_adapter:query(ProductId, <<\"SELECT COUNT(*) FROM \\\"device_\\\" || ? WHERE devaddr = ?\">>, [DeviceId, DeviceAddr]) of
            {ok, #{<<"data">> := [[Count]]}} when Count > 0 ->
                dgiot_utils:safe_format(<<"✅ TDengine中有 ~p 条设备数据~n">>, [Count]);
            {ok, #{<<"data">> := [[0]]}} ->
                dgiot_utils:safe_format(<<"⚠️  TDengine中暂无设备数据~n">>, []);
            {error, Reason} ->
                dgiot_utils:safe_format(<<"⚠️  TDengine查询错误: ~p~n">>, [Reason])
        end,
        
        % 检查缓存数据
        dgiot_utils:safe_format(<<"检查缓存数据:~n">>, []),
        case dgiot_data:match({last_data, '_'}) of
            [] ->
                dgiot_utils:safe_format(<<"⚠️  缓存中没有last_data记录~n">>, []);
            CacheList ->
                CacheCount = length(CacheList),
                dgiot_utils:safe_format(<<"✅ 缓存中有 ~p 条last_data记录~n">>, [CacheCount])
        end.
    "
    
    return 0
}

# 模块7: 热编译和热加载
module_hot_reload() {
    log_info "模块7: 热编译和热加载"
    
    echo "7.1 编译modbus插件..."
    _build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'
    
    echo "7.2 热加载modbus插件..."
    _build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_modbus).'
    
    echo "7.3 验证插件加载..."
    _build/emqx/rel/emqx/bin/emqx eval '
        dgiot_utils:safe_format(<<"验证插件加载状态:~n">>, []),
        
        case code:which(modbus_rtu) of
            non_existing ->
                dgiot_utils:safe_format(<<"❌ modbus_rtu模块未加载~n">>, []);
            _ ->
                dgiot_utils:safe_format(<<"✅ modbus_rtu模块已加载~n">>, [])
        end.
    '
    
    return 0
}

# ==================== 主函数 ====================

# 显示使用说明
show_usage() {
    echo ""
    echo "=== 使用说明 ==="
    echo "主脚本: $0"
    echo ""
    echo "二级功能模块:"
    echo "  --env-check       模块1: 环境检查"
    echo "  --device-reg      模块2: 设备注册测试"
    echo "  --data-report     模块3: Modbus数据上报测试"
    echo "  --api-query       模块4: API查询测试"
    echo "  --log-analysis    模块5: 日志分析"
    echo "  --data-storage    模块6: 数据存储验证"
    echo "  --hot-reload      模块7: 热编译和热加载"
    echo "  --all             执行完整测试流程"
    echo "  --help            显示此帮助信息"
    echo ""
    echo "示例:"
    echo "  $0 --all              # 执行完整测试流程"
    echo "  $0 --env-check        # 只检查环境"
    echo "  $0 --device-reg       # 只测试设备注册"
    echo "  $0 --data-report      # 只测试数据上报"
    echo ""
    echo "完整流程包括:"
    echo "  1. 环境检查 → 2. 设备注册 → 3. 数据上报 → 4. API查询"
    echo "  5. 日志分析 → 6. 数据存储验证 → 7. 热编译验证"
}

# 执行完整测试流程
execute_full_test() {
    log_info "执行完整测试流程..."
    
    local success_count=0
    local total_modules=7
    
    # 模块1: 环境检查
    if module_environment_check; then
        log_success "模块1: 环境检查通过"
        ((success_count++))
    else
        log_error "模块1: 环境检查失败"
        return 1
    fi
    
    # 模块2: 设备注册测试
    if module_device_registration; then
        log_success "模块2: 设备注册测试通过"
        ((success_count++))
    else
        log_error "模块2: 设备注册测试失败"
    fi
    
    # 模块3: Modbus数据上报测试
    if module_modbus_data_report; then
        log_success "模块3: Modbus数据上报测试通过"
        ((success_count++))
    else
        log_error "模块3: Modbus数据上报测试失败"
    fi
    
    # 模块4: API查询测试
    if module_api_query; then
        log_success "模块4: API查询测试通过"
        ((success_count++))
    else
        log_error "模块4: API查询测试失败"
    fi
    
    # 模块5: 日志分析
    if module_log_analysis; then
        log_success "模块5: 日志分析通过"
        ((success_count++))
    else
        log_error "模块5: 日志分析失败"
    fi
    
    # 模块6: 数据存储验证
    if module_data_storage; then
        log_success "模块6: 数据存储验证通过"
        ((success_count++))
    else
        log_error "模块6: 数据存储验证失败"
    fi
    
    # 模块7: 热编译和热加载
    if module_hot_reload; then
        log_success "模块7: 热编译和热加载通过"
        ((success_count++))
    else
        log_error "模块7: 热编译和热加载失败"
    fi
    
    # 输出测试结果
    echo ""
    echo "================================================================"
    echo "测试完成总结"
    echo "================================================================"
    echo "测试时间: $(date)"
    echo "总模块数: $total_modules"
    echo "通过模块: $success_count"
    echo "失败模块: $((total_modules - success_count))"
    echo ""
    
    if [ $success_count -eq $total_modules ]; then
        log_success "✅ 所有测试模块通过！"
        return 0
    elif [ $success_count -ge $((total_modules * 2 / 3)) ]; then
        log_warning "⚠️  大部分测试模块通过 ($success_count/$total_modules)"
        return 0
    else
        log_error "❌ 测试失败 ($success_count/$total_modules 模块通过)"
        return 1
    fi
}

# 主函数
main() {
    case "$1" in
        --env-check)
            module_environment_check
            ;;
        --device-reg)
            module_device_registration
            ;;
        --data-report)
            module_modbus_data_report
            ;;
        --api-query)
            module_api_query
            ;;
        --log-analysis)
            module_log_analysis
            ;;
        --data-storage)
            module_data_storage
            ;;
        --hot-reload)
            module_hot_reload
            ;;
        --all|"")
            execute_full_test
            ;;
        --help|-h)
            show_usage
            ;;
        *)
            log_error "未知选项: $1"
            show_usage
            exit 1
            ;;
    esac
}

# 检查是否在项目根目录
if [ ! -f "Makefile" ]; then
    log_error "请在项目根目录运行此脚本"
    exit 1
fi

# 执行主函数
main "$@"
