#!/bin/bash

# 简单UDP多播测试脚本
# 直接运行多播测试，不依赖复杂的tcpdump配置

set -e

# 动态路径计算
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

echo "=== 简单UDP多播测试 ==="
echo "开始时间: $(date)"

# 检查必要命令
echo "[INFO] 检查必要命令..."
for cmd in erl; do
    if ! command -v "$cmd" &> /dev/null; then
        echo "[ERROR] $cmd 未安装"
        exit 1
    fi
    echo "[INFO] $cmd 已安装"
done

echo ""
echo "[INFO] 运行UDP多播测试..."

# 切换到项目根目录并运行多播测试
cd "$PROJECT_ROOT"
erl -pa apps/dgiot/src/transport -pa apps/dgiot/src/transport/udp_test \
    -eval "
        io:format('=== UDP Multicast Test ===~n'),
        try
            Result = dgiot_udp_test_utils:test_multicast(),
            io:format('Test Result: ~p~n', [Result]),
            case Result of
                {ok, _} -> 
                    io:format('~n[SUCCESS] 多播测试通过~n'),
                    halt(0);
                _ -> 
                    io:format('~n[ERROR] 多播测试失败~n'),
                    halt(1)
            end
        catch
            _:Error ->
                io:format('~n[ERROR] 多播测试出错: ~p~n', [Error]),
                halt(1)
        end
    " -noshell

echo ""
echo "结束时间: $(date)"
echo "=== 测试完成 ==="
