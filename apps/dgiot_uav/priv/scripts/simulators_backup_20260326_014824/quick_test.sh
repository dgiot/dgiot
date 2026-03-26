#!/bin/bash
# 快速启动产线测试

cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators

echo "=========================================="
echo "无人机测试产线 - 快速启动"
echo "=========================================="
echo ""

# 检查DGIOT是否运行
if ! pgrep -f "emqx" > /dev/null; then
    echo "警告: DGIOT服务器未运行"
    echo "请先启动: make run"
    echo ""
    read -p "是否继续？(y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# 显示菜单
echo "请选择测试场景:"
echo "1) 简化测试流程 (normal_flow)"
echo "2) 拷机测试"
echo "3) 全工位循环"
echo "4) 产线A路径1"
echo "5) 产线A路径2"
echo "6) 产线B路径1"
echo "7) 产线B路径2"
echo "8) 所有产线 (4条路径)"
echo "9) 磁航向工位测试"
echo "10) 动力检测+噪音测试"
echo "0) 退出"
echo ""

read -p "请输入选项 (0-10): " choice

case $choice in
    1) test_case="normal_flow" ;;
    2) test_case="拷机测试" ;;
    3) test_case="全工位循环" ;;
    4) test_case="complete_production_line_a1" ;;
    5) test_case="complete_production_line_a2" ;;
    6) test_case="complete_production_line_b1" ;;
    7) test_case="complete_production_line_b2" ;;
    8) test_case="all_production_lines" ;;
    9) test_case="magnetic_station_only" ;;
    10) test_case="power_test_with_noise" ;;
    0) echo "退出"; exit 0 ;;
    *) echo "无效选项"; exit 1 ;;
esac

echo ""
echo "执行测试: $test_case"
echo "=========================================="
echo ""

# 执行测试
python3 integrated_production_line.py \
    --test-case "$test_case" \
    --verify \
    --save-report

echo ""
echo "=========================================="
echo "测试完成"
echo "=========================================="

# 显示测试报告
latest_report=$(ls -t test_report_*.json 2>/dev/null | head -1)
if [ -n "$latest_report" ]; then
    echo "测试报告: $latest_report"
    python3 << EOF
import json
with open('$latest_report', 'r', encoding='utf-8') as f:
    report = json.load(f)
    if 'verification' in report:
        v = report['verification']
        print(f"验证结果: {v['passed']}/{v['total']} 通过, {v['failed']} 失败")
EOF
fi
