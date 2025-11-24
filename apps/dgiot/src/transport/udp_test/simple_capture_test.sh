#!/bin/bash

# 简单的UDP多播抓包测试
echo "=== 简单UDP多播抓包测试 ==="
echo "开始时间: $(date)"

# 动态路径计算
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
CAPTURE_DIR="$PROJECT_ROOT/captures"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
PCAP_FILE="$CAPTURE_DIR/simple_multicast_test_${TIMESTAMP}.pcap"

# 创建目录
mkdir -p "$CAPTURE_DIR"

# 步骤1: 启动tcpdump抓包
echo "启动tcpdump抓包..."
tcpdump -i eth0 -w "$PCAP_FILE" udp and port 19000 &
TCPDUMP_PID=$!
sleep 2

# 检查tcpdump是否运行
if ! ps -p $TCPDUMP_PID > /dev/null; then
    echo "错误: tcpdump启动失败"
    exit 1
fi

echo "tcpdump启动成功 (PID: $TCPDUMP_PID)"

# 步骤2: 运行多播测试
echo "运行多播测试..."
cd "$PROJECT_ROOT"
timeout 10 erl -pa "$PROJECT_ROOT/apps/dgiot/src/transport" -pa "$PROJECT_ROOT/apps/dgiot/src/transport/udp_test" \
    -eval "
        io:format('=== Running Multicast Test ===~n'),
        try
            Result = dgiot_udp_test_utils:test_multicast(),
            io:format('Test Result: ~p~n', [Result]),
            halt(0)
        catch
            _:Error ->
                io:format('Test Error: ~p~n', [Error]),
                halt(1)
        end
    " -noshell

# 步骤3: 停止tcpdump
echo "停止tcpdump..."
sleep 2
kill $TCPDUMP_PID 2>/dev/null
wait $TCPDUMP_PID 2>/dev/null

# 步骤4: 分析抓包结果
echo "分析抓包结果..."
if [ -f "$PCAP_FILE" ]; then
    FILE_SIZE=$(stat -c%s "$PCAP_FILE" 2>/dev/null || stat -f%z "$PCAP_FILE" 2>/dev/null)
    if [ "$FILE_SIZE" -gt 24 ]; then
        echo "✅ 抓包成功！文件大小: $FILE_SIZE 字节"
        echo "抓包文件: $PCAP_FILE"
        
        # 显示多播报文统计
        PACKET_COUNT=$(tcpdump -r "$PCAP_FILE" 2>/dev/null | wc -l)
        MULTICAST_PACKETS=$(tcpdump -r "$PCAP_FILE" "multicast" 2>/dev/null | wc -l)
        UDP_PACKETS=$(tcpdump -r "$PCAP_FILE" "udp" 2>/dev/null | wc -l)
        
        echo "抓包统计:"
        echo "  - 总报文数: $PACKET_COUNT"
        echo "  - 多播报文数: $MULTICAST_PACKETS"
        echo "  - UDP报文数: $UDP_PACKETS"
        
        # 显示前几个多播报文
        echo ""
        echo "多播报文详情:"
        tcpdump -r "$PCAP_FILE" "multicast" 2>/dev/null | head -10
        
        # 检查是否包含测试报文
        if tcpdump -r "$PCAP_FILE" -A 2>/dev/null | grep -q "MULTICAST_TEST"; then
            echo ""
            echo "🎉 成功捕获测试多播报文！"
            echo "测试报文内容:"
            tcpdump -r "$PCAP_FILE" -A 2>/dev/null | grep -A2 -B2 "MULTICAST_TEST" | head -20
        else
            echo ""
            echo "⚠️  未找到测试报文内容，但有多播报文被捕获"
        fi
    else
        echo "❌ 抓包文件为空或太小: $FILE_SIZE 字节"
        exit 1
    fi
else
    echo "❌ 抓包文件不存在"
    exit 1
fi

echo ""
echo "结束时间: $(date)"
echo "=== 测试完成 ==="
