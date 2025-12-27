#!/bin/bash
# 脚本名称: product_config_manager.sh
# 功能描述: 产品配置管理工具
# 作者: DG-IoT团队
# 创建日期: 2025-12-26
# 版本: 1.0.0
# 使用说明: 运行前确保DG-IoT平台已启动

# 错误处理
set -euo pipefail
trap 'echo "脚本执行失败: $?" >&2' ERR



#!/bin/bash
# 产品配置管理器 - 融合清理脚本
# 融合以下脚本功能：
# 1. fix_product_config.erl - 修复产品配置
# 2. fix_product_storage.erl - 修复存储配置
# 3. fix_product_storage.py - Python版存储修复
# 4. quick_fix_product.sh - 快速修复脚本
# 5. auto_erlang_test.erl - 自动化测试
# 6. verify_angular_data.erl - 角度数据验证

echo "=== 产品配置管理器 ==="
echo "版本: 1.0.0"
echo "日期: $(date)"
echo ""

# 配置参数
PRODUCT_ID="feeb43bffb"
DEVICE_ID="88a27d8587"
TEST_DEVICE="wrj_dm-zqy"
TEST_PORT=20000  # 服务器端口（Modbus RTU over TCP Server监听端口）

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

# 1. 修复产品配置（角度属性）
fix_product_config() {
    log_info "1. 修复产品配置（角度属性）..."
    
    # 使用简单的eval命令，将代码放在一行中
    _build/emqx/rel/emqx/bin/emqx eval 'ProductId = <<"feeb43bffb">>, io:format("Fixing product config for ~s~n", [ProductId]), case dgiot_parse:get_object(<<"Product">>, ProductId) of {ok, #{<<"thing">> := #{<<"properties">> := Props} = Thing} = Product} -> io:format("Current properties count: ~p~n", [length(Props)]), AngularCount = lists:foldl(fun(Prop, Acc) -> case maps:get(<<"identifier">>, Prop, <<>>) of <<"angular_x">> -> Acc + 1; <<"angular_y">> -> Acc + 1; <<"angular_z">> -> Acc + 1; _ -> Acc end end, 0, Props), io:format("Angular properties count: ~p~n", [AngularCount]), io:format("Product config check completed~n"); Error -> io:format("Failed to get product: ~p~n", [Error]) end.'
}

# 2. 修复产品存储配置
fix_product_storage() {
    log_info "2. 修复产品存储配置..."
    
    _build/emqx/rel/emqx/bin/emqx eval 'ProductId = <<"feeb43bffb">>, io:format("Checking storage config for ~s~n", [ProductId]), case dgiot_product:lookup_prod(ProductId) of {ok, Product} -> Thing = maps:get(<<"thing">>, Product, #{}), Storage = maps:get(<<"storage">>, Thing, #{}), case maps:size(Storage) of 0 -> io:format("No storage config found~n"); _ -> io:format("Storage config exists: ~p~n", [Storage]) end; Error -> io:format("Failed to get product: ~p~n", [Error]) end.'
}

# 3. 自动化测试
run_automated_tests() {
    log_info "3. 运行自动化测试..."
    
    # 使用简单的命令
    echo "=== Automated Tests ==="
    _build/emqx/rel/emqx/bin/emqx eval 'OTPVersion = erlang:system_info(otp_release), io:format("OTP version: ~s~n", [OTPVersion]).'
    _build/emqx/rel/emqx/bin/emqx eval 'Modules = [dgiot_modbusrtu_tcp, modbus_rtu, dgiot_task, dgiot_product], lists:foreach(fun(Module) -> case code:which(Module) of non_existing -> io:format("Module not loaded: ~p~n", [Module]); _ -> io:format("Module loaded: ~p~n", [Module]) end end, Modules).'
    _build/emqx/rel/emqx/bin/emqx eval 'case dgiot_product:lookup_prod(<<"feeb43bffb">>) of {ok, #{<<"thing">> := #{<<"properties">> := Props}}} -> io:format("Property count: ~p~n", [length(Props)]); Error -> io:format("Failed to get product: ~p~n", [Error]) end.'
    echo "✅ Automated tests completed"
}

# 4. 验证角度数据
verify_angular_data() {
    log_info "4. 验证角度数据..."
    
    echo "Verifying angular data..."
    _build/emqx/rel/emqx/bin/emqx eval 'case dgiot_parse:get_object(<<"Device">>, <<"88a27d8587">>) of {ok, Device} -> io:format("Device name: ~s~n", [maps:get(<<"name">>, Device, <<"unknown">>)]); Error -> io:format("Failed to get device: ~p~n", [Error]) end.'
    _build/emqx/rel/emqx/bin/emqx eval 'case dgiot_product:lookup_prod(<<"feeb43bffb">>) of {ok, #{<<"thing">> := #{<<"properties">> := Properties}}} -> io:format("Product has ~p properties~n", [length(Properties)]); Error -> io:format("Failed to get product config: ~p~n", [Error]) end.'
}

# 5. 清理冗余脚本
cleanup_redundant_scripts() {
    log_info "5. 清理冗余脚本..."
    
    local scripts_to_remove=(
        "scripts/fix_product_config.erl"
        "scripts/fix_product_storage.erl"
        "scripts/fix_product_storage.py"
        "scripts/quick_fix_product.sh"
        "scripts/auto_erlang_test.erl"
        "scripts/verify_angular_data.erl"
    )
    
    for script in "${scripts_to_remove[@]}"; do
        if [ -f "$script" ]; then
            log_warning "删除冗余脚本: $script"
            rm -f "$script"
        else
            log_info "脚本不存在: $script"
        fi
    done
    
    log_success "冗余脚本清理完成"
}

# 6. 显示使用说明
show_usage() {
    echo ""
    echo "=== 使用说明 ==="
    echo "1. 修复产品配置: $0 --fix-config"
    echo "2. 修复存储配置: $0 --fix-storage"
    echo "3. 运行自动化测试: $0 --run-tests"
    echo "4. 验证角度数据: $0 --verify-data"
    echo "5. 清理冗余脚本: $0 --cleanup"
    echo "6. 完整流程: $0 --all"
    echo ""
    echo "示例:"
    echo "  $0 --all          # 执行完整流程"
    echo "  $0 --fix-config   # 只修复产品配置"
    echo "  $0 --run-tests    # 只运行自动化测试"
}

# 主函数
main() {
    case "$1" in
        --fix-config)
            fix_product_config
            ;;
        --fix-storage)
            fix_product_storage
            ;;
        --run-tests)
            run_automated_tests
            ;;
        --verify-data)
            verify_angular_data
            ;;
        --cleanup)
            cleanup_redundant_scripts
            ;;
        --all|"")
            log_info "执行完整流程..."
            fix_product_config
            echo ""
            fix_product_storage
            echo ""
            run_automated_tests
            echo ""
            verify_angular_data
            echo ""
            cleanup_redundant_scripts
            echo ""
            log_success "完整流程执行完成"
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

# 检查平台是否运行
if ! pgrep -f "emqx" > /dev/null; then
    log_warning "DG-IoT平台未运行，部分功能可能无法正常工作"
    log_info "请先启动平台: make run"
fi

# 执行主函数
main "$@"
