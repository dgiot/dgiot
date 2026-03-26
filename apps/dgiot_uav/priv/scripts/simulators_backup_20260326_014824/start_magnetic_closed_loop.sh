#!/bin/bash
#
# 磁航向工位闭环测试启动脚本
# 一键启动完整的闭环测试环境
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DGIOT_ROOT="/root/gitee/dgiot"

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${BLUE}======================================================================${NC}"
echo -e "${BLUE}磁航向工位闭环测试系统${NC}"
echo -e "${BLUE}======================================================================${NC}"

# 步骤1: 检查DG-IoT服务器
echo -e "\n${YELLOW}[步骤1] 检查DG-IoT服务器...${NC}"
if pgrep -f "emqx.*console" > /dev/null; then
    echo -e "${GREEN}✅ DG-IoT服务器正在运行${NC}"
else
    echo -e "${RED}❌ DG-IoT服务器未运行${NC}"
    echo -e "${YELLOW}请先启动DG-IoT服务器:${NC}"
    echo -e "  cd $DGIOT_ROOT"
    echo -e "  make run"
    exit 1
fi

# 步骤2: 绑定IP地址
echo -e "\n${YELLOW}[步骤2] 检查IP绑定状态...${NC}"
IPS_TO_BIND=(
    "192.168.100.20"
    "192.168.100.21"
    "192.168.100.23"
)

NEED_BIND=false
for ip in "${IPS_TO_BIND[@]}"; do
    if ! ip addr show eth0 | grep -q "$ip"; then
        echo -e "${YELLOW}⚠️  IP $ip 未绑定${NC}"
        NEED_BIND=true
    else
        echo -e "${GREEN}✅ IP $ip 已绑定${NC}"
    fi
done

if [ "$NEED_BIND" = true ]; then
    echo -e "\n${YELLOW}需要绑定IP地址。是否绑定？ (需要root权限) [y/N]${NC}"
    read -r response
    if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
        echo -e "${YELLOW}绑定IP地址...${NC}"
        for ip in "${IPS_TO_BIND[@]}"; do
            if ! ip addr show eth0 | grep -q "$ip"; then
                sudo ip addr add "$ip/24" dev eth0
                echo -e "${GREEN}✅ 已绑定 $ip${NC}"
            fi
        done
    else
        echo -e "${RED}❌ IP未绑定，测试可能失败${NC}"
        echo -e "${YELLOW}建议手动绑定:${NC}"
        for ip in "${IPS_TO_BIND[@]}"; do
            echo -e "  sudo ip addr add $ip/24 dev eth0"
        done
    fi
fi

# 步骤3: 启动磁航向PLC模拟器
echo -e "\n${YELLOW}[步骤3] 启动磁航向PLC模拟器...${NC}"
PLC_LOG="/tmp/plc_simulator.log"
MAGNETIC_PLC_IP="192.168.100.20"
MAGNETIC_PLC_PORT=502

if pgrep -f "plc_simulator.py" > /dev/null; then
    PLC_PID=$(pgrep -f "plc_simulator.py" | head -1)
    echo -e "${GREEN}✅ PLC模拟器已在运行${NC}"
    echo -e "${CYAN}   PID: $PLC_PID${NC}"
    echo -e "${CYAN}   监听: 0.0.0.0:$MAGNETIC_PLC_PORT${NC}"
    echo -e "${CYAN}   磁航向PLC: $MAGNETIC_PLC_IP:$MAGNETIC_PLC_PORT${NC}"
else
    echo -e "${YELLOW}正在启动PLC模拟器...${NC}"
    cd "$SCRIPT_DIR"
    python3 plc_simulator.py > "$PLC_LOG" 2>&1 &
    PLC_PID=$!
    sleep 3
    
    if kill -0 $PLC_PID 2>/dev/null; then
        echo -e "${GREEN}✅ PLC模拟器启动成功${NC}"
        echo -e "${CYAN}   PID: $PLC_PID${NC}"
        echo -e "${CYAN}   监听: 0.0.0.0:$MAGNETIC_PLC_PORT${NC}"
        echo -e "${CYAN}   磁航向PLC: $MAGNETIC_PLC_IP:$MAGNETIC_PLC_PORT${NC}"
        echo -e "${CYAN}   日志: $PLC_LOG${NC}"
    else
        echo -e "${RED}❌ PLC模拟器启动失败${NC}"
        echo -e "${YELLOW}查看日志: tail -50 $PLC_LOG${NC}"
        exit 1
    fi
fi

# 步骤4: 运行闭环测试
echo -e "\n${YELLOW}[步骤4] 启动闭环测试...${NC}"
echo -e "${BLUE}======================================================================${NC}"
echo -e "${BLUE}开始运行闭环测试脚本${NC}"
echo -e "${BLUE}======================================================================${NC}\n"

cd "$SCRIPT_DIR"
python3 magnetic_station_closed_loop_test.py

echo -e "\n${GREEN}✅ 闭环测试完成${NC}"
