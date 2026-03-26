#!/bin/bash
#
# 磁航向工位(1700) - 快速启动脚本
# 从模拟器目录直接调用磁航向工位的一键式测试
#
# 用法:
#   ./run_station_1700.sh [选项]
#
# 选项:
#   --bind-ips         绑定磁航向工位相关IP（需要root权限）
#   --start-simulators 启动PLC和治具模拟器
#   --help             显示帮助信息
#

# 磁航向工位目录
STATION_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/stations/1700_magnetic"

# 检查工位目录是否存在
if [ ! -d "$STATION_DIR" ]; then
    echo "错误: 磁航向工位目录不存在: $STATION_DIR"
    echo "请确保已正确设置工位场景目录结构"
    exit 1
fi

# 检查启动脚本是否存在
START_SCRIPT="$STATION_DIR/start_magnetic_test.sh"
if [ ! -f "$START_SCRIPT" ]; then
    echo "错误: 启动脚本不存在: $START_SCRIPT"
    echo "请确保已创建磁航向工位一键式测试脚本"
    exit 1
fi

# 传递所有参数给启动脚本
"$START_SCRIPT" "$@"