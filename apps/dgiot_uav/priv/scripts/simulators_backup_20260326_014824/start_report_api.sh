#!/bin/bash
#
# 测试报告生成服务启动脚本
#

SCRIPT_DIR="/root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators"
REPORT_API_SERVER="$SCRIPT_DIR/report_api_server.py"
PID_FILE="/tmp/report_api_server.pid"
LOG_FILE="/tmp/report_api_server.log"

# 启动服务
start() {
    if [ -f "$PID_FILE" ]; then
        PID=$(cat "$PID_FILE")
        if ps -p $PID > /dev/null 2>&1; then
            echo "报告API服务已在运行 (PID: $PID)"
            return 1
        else
            rm -f "$PID_FILE"
        fi
    fi

    echo "启动报告API服务..."
    cd "$SCRIPT_DIR"
    nohup python3 "$REPORT_API_SERVER" > "$LOG_FILE" 2>&1 &
    PID=$!
    echo $PID > "$PID_FILE"
    echo "报告API服务已启动 (PID: $PID)"
    echo "日志文件: $LOG_FILE"
}

# 停止服务
stop() {
    if [ -f "$PID_FILE" ]; then
        PID=$(cat "$PID_FILE")
        kill $PID 2>/dev/null
        rm -f "$PID_FILE"
        echo "报告API服务已停止"
    else
        echo "报告API服务未运行"
    fi
}

# 重启服务
restart() {
    stop
    sleep 1
    start
}

# 查看状态
status() {
    if [ -f "$PID_FILE" ]; then
        PID=$(cat "$PID_FILE")
        if ps -p $PID > /dev/null 2>&1; then
            echo "报告API服务正在运行 (PID: $PID)"
        else
            echo "报告API服务未运行 (PID文件存在但进程不存在)"
        fi
    else
        echo "报告API服务未运行"
    fi
}

# 查看日志
logs() {
    if [ -f "$LOG_FILE" ]; then
        tail -f "$LOG_FILE"
    else
        echo "日志文件不存在: $LOG_FILE"
    fi
}

# 主函数
case "$1" in
    start)
        start
        ;;
    stop)
        stop
        ;;
    restart)
        restart
        ;;
    status)
        status
        ;;
    logs)
        logs
        ;;
    *)
        echo "用法: $0 {start|stop|restart|status|logs}"
        exit 1
        ;;
esac

exit $?
