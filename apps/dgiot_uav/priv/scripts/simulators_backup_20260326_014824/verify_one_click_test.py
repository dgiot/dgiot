#!/usr/bin/env python3
"""
验证脚本 - 检查一键式测试系统的完整性
"""

import sys
import os
from pathlib import Path

# 添加simulators目录到路径
current_dir = Path(__file__).parent
sys.path.insert(0, str(current_dir))

def check_file_exists(filepath, description):
    """检查文件是否存在"""
    if os.path.exists(filepath):
        print(f"✓ {description}: {filepath}")
        return True
    else:
        print(f"✗ {description}: {filepath}")
        return False

def check_python_module(module_name):
    """检查Python模块是否可以导入"""
    try:
        __import__(module_name)
        print(f"✓ Python模块: {module_name}")
        return True
    except ImportError:
        print(f"✗ Python模块: {module_name}")
        return False

def main():
    """主函数"""
    print("=" * 60)
    print("一键式测试系统验证")
    print("=" * 60)
    print()

    all_checks_passed = True

    # 1. 检查脚本文件
    print("1. 检查脚本文件:")
    print("-" * 60)

    checks = [
        ("one_click_production_test.py", "主测试脚本"),
        ("README_ONE_CLICK_TEST.md", "使用文档"),
        ("start_one_click_test.sh", "启动脚本"),
    ]

    for filename, description in checks:
        filepath = os.path.join(current_dir, filename)
        if not check_file_exists(filepath, description):
            all_checks_passed = False

    print()

    # 2. 检查Python模块
    print("2. 检查Python模块:")
    print("-" * 60)

    modules = [
        "json",
        "logging",
        "time",
        "datetime",
        "pathlib",
        "argparse",
    ]

    for module in modules:
        if not check_python_module(module):
            all_checks_passed = False

    print()

    # 3. 检查配置
    print("3. 检查配置:")
    print("-" * 60)

    try:
        from one_click_production_test import (
            STATIONS_CONFIG,
            ALERT_MONITOR_CONFIG,
            DG_IOT_CONFIG,
            PRODUCTION_LINE_SEQUENCE,
        )

        print(f"✓ 工位配置数量: {len(STATIONS_CONFIG)}")
        print(f"✓ 产线测试顺序: {PRODUCTION_LINE_SEQUENCE}")
        print(f"✓ 告警监控配置: {ALERT_MONITOR_CONFIG['name']}")
        print(f"✓ DG-IoT配置: {DG_IOT_CONFIG['host']}")

    except Exception as e:
        print(f"✗ 配置检查失败: {e}")
        all_checks_passed = False

    print()

    # 4. 检查工位配置完整性
    print("4. 检查工位配置完整性:")
    print("-" * 60)

    try:
        from one_click_production_test import STATIONS_CONFIG

        for station_id, config in STATIONS_CONFIG.items():
            required_fields = ["station_id", "name", "ip", "description", "devices"]
            missing_fields = [f for f in required_fields if f not in config]

            if missing_fields:
                print(f"✗ 工位{station_id}缺少字段: {missing_fields}")
                all_checks_passed = False
            else:
                test_steps_count = len(config.get("test_steps", []))
                print(f"✓ 工位{station_id} ({config['name']}): {test_steps_count}个测试步骤")

    except Exception as e:
        print(f"✗ 工位配置检查失败: {e}")
        all_checks_passed = False

    print()

    # 5. 检查告警监控配置
    print("5. 检查告警监控配置:")
    print("-" * 60)

    try:
        from one_click_production_test import ALERT_MONITOR_CONFIG

        required_fields = ["station_id", "name", "description", "sensors", "thresholds"]
        missing_fields = [f for f in required_fields if f not in ALERT_MONITOR_CONFIG]

        if missing_fields:
            print(f"✗ 告警监控配置缺少字段: {missing_fields}")
            all_checks_passed = False
        else:
            print(f"✓ 告警监控配置: {ALERT_MONITOR_CONFIG['name']}")
            print(f"✓ 噪音传感器数量: {len(ALERT_MONITOR_CONFIG['sensors'])}")
            print(f"✓ 告警阈值: 警告 {ALERT_MONITOR_CONFIG['thresholds']['warning']}dB, 严重 {ALERT_MONITOR_CONFIG['thresholds']['critical']}dB")

    except Exception as e:
        print(f"✗ 告警监控配置检查失败: {e}")
        all_checks_passed = False

    print()

    # 6. 功能测试
    print("6. 功能测试:")
    print("-" * 60)

    try:
        from one_click_production_test import StationTestScenario, AlertMonitorScenario

        # 测试工位测试场景
        if 1500 in STATIONS_CONFIG:
            test_config = {
                "dgiot": DG_IOT_CONFIG,
                "stations": STATIONS_CONFIG,
                "alert_monitor": ALERT_MONITOR_CONFIG,
            }

            test_logger = logging.getLogger("Test")
            test_logger.setLevel(logging.WARNING)  # 静默模式

            scenario = StationTestScenario(
                1500,
                STATIONS_CONFIG[1500],
                DG_IOT_CONFIG,
                test_logger
            )
            print(f"✓ 工位测试场景创建成功")

        # 测试告警监控场景
        monitor = AlertMonitorScenario(ALERT_MONITOR_CONFIG, test_logger)
        print(f"✓ 告警监控场景创建成功")

    except Exception as e:
        print(f"✗ 功能测试失败: {e}")
        all_checks_passed = False

    print()
    print("=" * 60)
    if all_checks_passed:
        print("验证结果: 所有检查通过 ✓")
        print("=" * 60)
        return 0
    else:
        print("验证结果: 部分检查失败 ✗")
        print("=" * 60)
        return 1

if __name__ == "__main__":
    import logging
    sys.exit(main())
