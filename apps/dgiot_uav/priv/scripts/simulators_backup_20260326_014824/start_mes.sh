#!/bin/bash
# MES模拟器启动脚本

cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators

echo "=========================================="
echo "启动MES模拟服务器"
echo "=========================================="

# 绑定MES IP地址（Erlang客户端访问nginx使用的IP）
MES_IP="172.1.2.222"
echo "绑定MES IP地址: $MES_IP"
ip addr add $MES_IP/24 dev eth0 2>/dev/null || echo "IP已存在"

# 检查端口是否被占用
if netstat -tunlp 2>/dev/null | grep -q :801; then
    echo "警告: 端口801已被占用"
    echo "尝试停止已有进程..."
    pkill -f mes_simulator.py
    sleep 1
fi

# 启动MES模拟器
python3 mes_simulator.py &

echo ""
echo "MES服务器已启动在后台"
echo "PID: $!"
echo "监听地址: 0.0.0.0:801（所有网卡）"
echo "MES IP: $MES_IP:80（Nginx代理）"
echo ""
echo "架构: Erlang客户端 → Nginx(80) → MES模拟器(801)"
echo ""
echo "测试API:"
echo "  健康检查: curl http://$MES_IP/mes/health"
echo "  直接访问: curl http://127.0.0.1:801/health"
echo ""
echo "停止MES: pkill -f mes_simulator.py"
