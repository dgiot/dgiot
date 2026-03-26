#!/usr/bin/env python3
"""
无人机测试产线 - 一键式端到端测试脚本
非常具体化，每个工位一个场景，简单易用

用法示例:
    python3 one_click_test.py --station 1200     # 测试磁航向工位
    python3 one_click_test.py --station 1500     # 测试总测工位
    python3 one_click_test.py --station 1600     # 测试拷机工位
    python3 one_click_test.py --station 1100     # 测试桁架工位
    python3 one_click_test.py --station 1700     # 测试告警检测工位
    python3 one_click_test.py --full-line         # 测试完整产线
"""

import argparse
import json
import logging
import os
import sys
import time
from datetime import datetime
from pathlib import Path

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)


# ==================== 产线配置 ====================
# 端口即设备类型
DEVICE_PORTS = {
    10001: "舵面传感器1",
    10002: "舵面传感器2",
    10003: "舵面传感器3",
    10004: "舵面传感器4",
    10005: "舵面传感器5",
    10006: "单片机(治具)",
    10007: "无人机地测口",
    1234: "扫码枪",
    21000: "噪音传感器",
}

# 工位配置
STATIONS = {
    1200: {
        "name": "磁航向工位",
        "ip": "192.168.100.21",
        "description": "扫码绑定入口，磁航向校准测试",
        "ports": [10007, 1234],  # 地测口 + 扫码枪
        "test_steps": [
            {"name": "扫码获取编码", "expected": 5},
            {"name": "磁航向校准", "expected": 30},
            {"name": "磁场精度检测", "expected": 20},
            {"name": "磁偏补偿测试", "expected": 15},
        ],
    },
    1500: {
        "name": "总测工位",
        "ip": "192.168.100.47",
        "description": "10步测试流程：备检→检查→测量→链路→上电→夜灯→气压→电磁→航线",
        "ports": [10001, 10002, 10003, 10004, 10005, 10006, 10007],  # 5舵面+单片机+地测口
        "test_steps": [
            {"name": "备检并获取编码", "expected": 5},
            {"name": "机身静态测试前检查", "expected": 10},
            {"name": "机身及螺旋桨安装情况检查", "expected": 15},
            {"name": "电压测量检查", "expected": 10},
            {"name": "链路功能检查", "expected": 20},
            {"name": "上电参数检查", "expected": 15},
            {"name": "夜航灯测试", "expected": 10},
            {"name": "气压高度检测", "expected": 15},
            {"name": "系统电磁兼容性功能检查", "expected": 20},
            {"name": "航线加载及载荷功能检查", "expected": 30},
        ],
    },
    1600: {
        "name": "拷机工位",
        "ip": "192.168.100.50",
        "description": "舵面数据采集测试，拷机时长验证",
        "ports": [10001, 10002, 10003, 10004, 10005, 10006, 10007],
        "test_steps": [
            {"name": "拷机前检查", "expected": 10},
            {"name": "舵面数据采集", "expected": 120},
            {"name": "导引头功能测试", "expected": 30},
            {"name": "拷机时长验证", "expected": 10},
        ],
    },
    1100: {
        "name": "桁架工位",
        "ip": "192.168.100.51",
        "description": "桁架机械手测试，导引头功能验证",
        "ports": [10001, 10002, 10003, 10004, 10005, 10006, 10007],
        "test_steps": [
            {"name": "桁架机械手测试", "expected": 20},
            {"name": "舵面机械臂测试", "expected": 20},
            {"name": "导引头功能验证", "expected": 30},
        ],
    },
    1700: {
        "name": "告警检测工位",
        "description": "全程监控产线噪音，不绑定具体工位",
        "sensors": [
            {"ip": "192.168.100.35", "port": 21000, "name": "噪音传感器1"},
            {"ip": "192.168.100.36", "port": 21000, "name": "噪音传感器2"},
        ],
        "thresholds": {"warning": 75.0, "critical": 85.0},
    },
}

# 产线测试顺序
LINE_SEQUENCE = [1200, 1500, 1600, 1100]


# ==================== 测试执行器 ====================
class StationTester:
    """工位测试器"""

    def __init__(self, station_id: int):
        self.station_id = station_id
        self.station_config = STATIONS.get(station_id)
        if not self.station_config:
            raise ValueError(f"未知的工位ID: {station_id}")

        self.test_id = f"TEST-{station_id}-{datetime.now().strftime('%Y%m%d%H%M%S')}"
        self.start_time = None
        self.end_time = None
        self.status = "not_started"
        self.step_results = []
        self.errors = []

    def run(self) -> bool:
        """运行工位测试"""
        logger.info("=" * 60)
        logger.info(f"开始测试工位: {self.station_config['name']} (ID: {self.station_id})")
        logger.info(f"测试ID: {self.test_id}")
        logger.info(f"IP地址: {self.station_config.get('ip', 'N/A')}")
        logger.info(f"设备: {', '.join([DEVICE_PORTS[p] for p in self.station_config.get('ports', [])])}")
        logger.info("=" * 60)

        self.start_time = datetime.now()
        self.status = "running"

        try:
            # 模拟设备上线
            logger.info(f"正在连接DG-IoT服务器...")
            time.sleep(2)

            # 执行测试步骤
            if "test_steps" in self.station_config:
                for i, step in enumerate(self.station_config["test_steps"], 1):
                    step_start = time.time()

                    logger.info(f"[{i}/{len(self.station_config['test_steps'])}] {step['name']}")
                    logger.info(f"   预计耗时: {step['expected']}秒")

                    # 模拟测试执行
                    time.sleep(min(step["expected"], 5))  # 最多模拟5秒

                    step_duration = time.time() - step_start

                    self.step_results.append({
                        "step_no": i,
                        "step_name": step["name"],
                        "expected": step["expected"],
                        "actual": step_duration,
                        "status": "completed",
                    })

                    logger.info(f"   完成 (实际耗时: {step_duration:.2f}秒)")

            # 完成
            self.end_time = datetime.now()
            self.status = "completed"
            duration = (self.end_time - self.start_time).total_seconds()

            logger.info("=" * 60)
            logger.info(f"工位测试完成: {self.station_config['name']}")
            logger.info(f"总耗时: {duration:.2f}秒")
            logger.info(f"状态: {self.status}")
            logger.info("=" * 60)

            return True

        except Exception as e:
            self.end_time = datetime.now()
            self.status = "failed"
            self.errors.append(str(e))
            logger.exception(f"工位测试失败: {e}")
            return False


class AlertMonitorTester:
    """告警监控测试器"""

    def __init__(self):
        self.station_id = 1700
        self.station_config = STATIONS[self.station_id]
        self.test_id = f"TEST-{self.station_id}-{datetime.now().strftime('%Y%m%d%H%M%S')}"
        self.running = False

    def start(self) -> bool:
        """启动告警监控"""
        logger.info("=" * 60)
        logger.info(f"启动告警监控: {self.station_config['name']} (ID: {self.station_id})")
        logger.info(f"测试ID: {self.test_id}")
        logger.info("=" * 60)

        try:
            self.running = True

            for i, sensor in enumerate(self.station_config["sensors"], 1):
                logger.info(f"[{i}/{len(self.station_config['sensors'])}] 连接传感器: {sensor['name']}")
                logger.info(f"   IP地址: {sensor['ip']}")
                logger.info(f"   端口: {sensor['port']}")
                logger.info(f"   告警阈值: 警告 {self.station_config['thresholds']['warning']}dB, 严重 {self.station_config['thresholds']['critical']}dB")

                # 模拟连接
                time.sleep(1)

            logger.info("=" * 60)
            logger.info("告警监控启动成功")
            logger.info("=" * 60)

            return True

        except Exception as e:
            logger.exception(f"启动告警监控失败: {e}")
            return False

    def stop(self):
        """停止告警监控"""
        if self.running:
            logger.info("=" * 60)
            logger.info("停止告警监控")
            logger.info("=" * 60)
            self.running = False


# ==================== 完整产线测试 ====================
class ProductionLineTester:
    """完整产线测试器"""

    def __init__(self):
        self.test_id = f"LINE-{datetime.now().strftime('%Y%m%d%H%M%S')}"
        self.start_time = None
        self.end_time = None
        self.status = "not_started"
        self.station_results = {}
        self.alert_monitor = None

    def run(self) -> bool:
        """运行完整产线测试"""
        logger.info("=" * 60)
        logger.info("开始完整产线测试")
        logger.info(f"测试ID: {self.test_id}")
        logger.info(f"测试流程: {' → '.join([STATIONS[s]['name'] for s in LINE_SEQUENCE])}")
        logger.info("=" * 60)

        self.start_time = datetime.now()
        self.status = "running"

        try:
            # 启动告警监控
            alert_monitor = AlertMonitorTester()
            if not alert_monitor.start():
                logger.warning("告警监控启动失败，继续测试...")
            else:
                self.alert_monitor = alert_monitor

            # 依次执行工位测试
            all_success = True
            for station_id in LINE_SEQUENCE:
                tester = StationTester(station_id)
                success = tester.run()

                self.station_results[station_id] = {
                    "test_id": tester.test_id,
                    "name": tester.station_config["name"],
                    "status": tester.status,
                    "duration": (tester.end_time - tester.start_time).total_seconds() if tester.end_time else None,
                    "step_results": tester.step_results,
                    "errors": tester.errors,
                }

                if not success:
                    all_success = False
                    logger.error(f"工位 {station_id} 测试失败")
                    break

            # 停止告警监控
            if self.alert_monitor:
                self.alert_monitor.stop()

            # 完成
            self.end_time = datetime.now()
            self.status = "completed" if all_success else "failed"
            duration = (self.end_time - self.start_time).total_seconds()

            logger.info("=" * 60)
            logger.info(f"完整产线测试完成")
            logger.info(f"测试ID: {self.test_id}")
            logger.info(f"总耗时: {duration:.2f}秒")
            logger.info(f"最终状态: {self.status}")
            logger.info("=" * 60)

            # 生成测试报告
            self.generate_report()

            return all_success

        except Exception as e:
            self.end_time = datetime.now()
            self.status = "error"
            logger.exception(f"完整产线测试失败: {e}")
            return False

    def generate_report(self):
        """生成测试报告"""
        report = {
            "test_id": self.test_id,
            "start_time": self.start_time.isoformat(),
            "end_time": self.end_time.isoformat(),
            "status": self.status,
            "duration": (self.end_time - self.start_time).total_seconds(),
            "station_results": self.station_results,
        }

        # 保存到文件
        log_dir = Path("./test_logs")
        log_dir.mkdir(exist_ok=True)
        report_file = log_dir / f"report_{self.test_id}.json"

        with open(report_file, 'w', encoding='utf-8') as f:
            json.dump(report, f, ensure_ascii=False, indent=2)

        logger.info(f"测试报告已保存: {report_file}")


# ==================== 主程序 ====================
def main():
    """主函数"""
    parser = argparse.ArgumentParser(
        description="无人机测试产线 - 一键式端到端测试",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
工位列表:
  1200 - 磁航向工位 (扫码绑定入口)
  1500 - 总测工位 (10步测试流程)
  1600 - 拷机工位 (舵面数据采集)
  1100 - 桁架工位 (机械手测试)
  1700 - 告警检测工位 (噪音监控)

示例:
  python3 one_click_test.py --station 1200
  python3 one_click_test.py --station 1500
  python3 one_click_test.py --full-line
  python3 one_click_test.py --list-stations
        """
    )

    parser.add_argument(
        '--station', '-s',
        type=int,
        metavar='ID',
        choices=[1200, 1500, 1600, 1100, 1700],
        help='测试单个工位'
    )

    parser.add_argument(
        '--full-line', '-f',
        action='store_true',
        help='测试完整产线 (1200→1500→1600→1100)'
    )

    parser.add_argument(
        '--list-stations',
        action='store_true',
        help='列出所有工位信息'
    )

    args = parser.parse_args()

    # 列出工位
    if args.list_stations:
        print("\n工位列表:")
        print("-" * 60)
        for station_id, config in STATIONS.items():
            print(f"工位ID: {station_id}")
            print(f"  名称: {config['name']}")
            print(f"  IP: {config.get('ip', 'N/A')}")
            print(f"  设备: {', '.join([DEVICE_PORTS[p] for p in config.get('ports', [])]) if 'ports' in config else '噪音传感器×2'}")
            print(f"  描述: {config['description']}")
            if 'test_steps' in config:
                print(f"  测试步骤: {len(config['test_steps'])}步")
            print("-" * 60)
        return 0

    # 验证参数
    if not args.station and not args.full_line:
        parser.print_help()
        return 1

    if args.station and args.full_line:
        print("错误: --station 和 --full-line 不能同时使用")
        return 1

    # 执行测试
    if args.full_line:
        tester = ProductionLineTester()
        success = tester.run()
    else:
        if args.station == 1700:
            tester = AlertMonitorTester()
            success = tester.start()
            if success:
                logger.info("告警监控运行中，按Ctrl+C停止...")
                try:
                    while True:
                        time.sleep(1)
                except KeyboardInterrupt:
                    tester.stop()
        else:
            tester = StationTester(args.station)
            success = tester.run()

    return 0 if success else 1


if __name__ == "__main__":
    sys.exit(main())
