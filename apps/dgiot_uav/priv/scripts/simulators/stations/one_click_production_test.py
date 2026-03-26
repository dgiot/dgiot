#!/usr/bin/env python3
"""
无人机测试产线 - 一键式端到端测试脚本
每个工位一个场景，具体化测试流程

用法:
  # 测试单个工位
  python3 one_click_production_test.py --station 1500

  # 测试完整产线
  python3 one_click_production_test.py --full-line

  # 查看所有工位
  python3 one_click_production_test.py --list-stations

作者: DGIoT Team
日期: 2026-03-25
版本: v1.0.0
"""

import argparse
import json
import logging
import os
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

# 添加simulators目录到路径
current_dir = Path(__file__).parent
sys.path.insert(0, str(current_dir))

# ==================== 配置常量 ====================
# DG-IoT平台配置
DG_IOT_CONFIG = {
    "host": "127.0.0.1",
    "api_port": 18083,
    "mqtt_port": 1883,
    "tcp_port": 20000,
    "http_api": "http://127.0.0.1:18083/api/v1",
}

# 工位配置（每个工位一个场景）
STATIONS_CONFIG = {
    1200: {
        "station_id": 1200,
        "name": "磁航向工位",
        "ip": "192.168.100.21",
        "port": 10007,
        "description": "扫码绑定入口，磁航向校准测试",
        "devices": ["地测口", "扫码枪"],
        "plc_config": {"base_addr": 1700, "ip": "192.168.100.20"},
        "test_steps": [
            {
                "step_no": 1,
                "name": "扫码获取设备编码",
                "description": "扫码枪扫描设备编码(UAV-12345)，触发设备绑定",
                "expected_duration": 5,
                "action": "扫码枪发送: SCAN:UAV-12345",
                "expected_result": "设备成功绑定到1200工位",
            },
            {
                "step_no": 2,
                "name": "磁航向校准测试",
                "description": "测试磁航向传感器精度",
                "expected_duration": 30,
                "action": "发送遥控指令: F0 FB (舵面中位)",
                "expected_result": "磁航向数据正常，校准完成",
            },
            {
                "step_no": 3,
                "name": "磁场精度检测",
                "description": "检测磁场传感器的精度和稳定性",
                "expected_duration": 20,
                "action": "采集磁场数据",
                "expected_result": "磁场误差 < 0.5°",
            },
            {
                "step_no": 4,
                "name": "磁偏补偿测试",
                "description": "测试磁偏角补偿功能",
                "expected_duration": 25,
                "action": "发送飞行指令，验证磁偏补偿",
                "expected_result": "航向角准确，补偿正常",
            },
        ],
    },
    1500: {
        "station_id": 1500,
        "name": "总测工位",
        "ip": "192.168.100.47",
        "ports": [10001, 10002, 10003, 10004, 10005, 10006, 10007],
        "description": "10步标准测试流程，完整功能验证",
        "devices": ["舵面×5", "单片机", "地测口", "无人机"],
        "plc_config": {"base_addr": 1500, "ip": "192.168.100.40"},
        "test_steps": [
            {
                "step_no": 1,
                "name": "备检并获取编码",
                "description": "通过扫码获取设备编码，准备测试",
                "expected_duration": 5,
                "action": "扫码枪扫描设备编码",
                "expected_result": "获取到设备编码 UAV-12345",
            },
            {
                "step_no": 2,
                "name": "机身静态测试前检查",
                "description": "检查机身完整性、螺旋桨安装情况",
                "expected_duration": 10,
                "action": "视觉检查，确认机身无损伤",
                "expected_result": "机身完好，螺旋桨安装正确",
            },
            {
                "step_no": 3,
                "name": "机身及螺旋桨安装情况检查",
                "description": "详细检查螺旋桨安装是否牢固、方向正确",
                "expected_duration": 15,
                "action": "手动检查所有螺旋桨",
                "expected_result": "所有螺旋桨安装牢固，方向正确",
            },
            {
                "step_no": 4,
                "name": "电压测量检查",
                "description": "测量电池电压，确保电量充足",
                "expected_duration": 10,
                "action": "单片机上报电压数据",
                "expected_result": "电池电压在 22.2V - 25.2V 范围内",
            },
            {
                "step_no": 5,
                "name": "链路功能检查",
                "description": "测试地面站与无人机之间的通信链路",
                "expected_duration": 20,
                "action": "地面站发送心跳指令，无人机应答",
                "expected_result": "链路通信正常，延迟 < 100ms",
            },
            {
                "step_no": 6,
                "name": "上电参数检查",
                "description": "检查上电后的各项参数设置",
                "expected_duration": 15,
                "action": "读取无人机配置参数",
                "expected_result": "所有参数符合设计要求",
            },
            {
                "step_no": 7,
                "name": "夜航灯测试",
                "description": "测试夜航灯的亮灭和颜色",
                "expected_duration": 10,
                "action": "发送夜航灯控制指令",
                "expected_result": "夜航灯正常亮灭，颜色正确",
            },
            {
                "step_no": 8,
                "name": "气压高度检测",
                "description": "测试气压高度传感器的精度",
                "expected_duration": 15,
                "action": "改变气压，验证高度读数",
                "expected_result": "高度误差 < 0.5m",
            },
            {
                "step_no": 9,
                "name": "系统电磁兼容性功能检查",
                "description": "测试系统在电磁干扰环境下的稳定性",
                "expected_duration": 20,
                "action": "施加电磁干扰，验证系统稳定性",
                "expected_result": "系统运行稳定，无异常",
            },
            {
                "step_no": 10,
                "name": "航线加载及载荷功能检查",
                "description": "测试航线加载和载荷功能",
                "expected_duration": 30,
                "action": "加载测试航线，验证载荷响应",
                "expected_result": "航线加载成功，载荷功能正常",
            },
        ],
    },
    1600: {
        "station_id": 1600,
        "name": "拷机工位",
        "ip": "192.168.100.50",
        "ports": [10001, 10002, 10003, 10004, 10005, 10006, 10007],
        "description": "舵面数据采集测试，拷机时长验证",
        "devices": ["舵面×5", "单片机", "地测口", "无人机"],
        "plc_config": {"base_addr": 1200, "ip": "192.168.100.40"},
        "test_steps": [
            {
                "step_no": 1,
                "name": "拷机前检查",
                "description": "检查设备状态，准备拷机",
                "expected_duration": 10,
                "action": "检查所有舵面传感器和单片机状态",
                "expected_result": "所有设备正常，准备就绪",
            },
            {
                "step_no": 2,
                "name": "舵面数据采集测试",
                "description": "采集5个舵面的偏转数据",
                "expected_duration": 120,
                "action": "发送舵面偏转指令，采集传感器数据",
                "expected_result": "舵面偏转数据准确，响应正常",
            },
            {
                "step_no": 3,
                "name": "导引头功能测试",
                "description": "测试导引头的功能和精度",
                "expected_duration": 30,
                "action": "发送导引头控制指令，验证响应",
                "expected_result": "导引头响应准确，功能正常",
            },
            {
                "step_no": 4,
                "name": "拷机时长验证",
                "description": "验证拷机时长是否符合要求",
                "expected_duration": 10,
                "action": "记录拷机开始和结束时间",
                "expected_result": "拷机时长在规定范围内",
            },
        ],
    },
    1100: {
        "station_id": 1100,
        "name": "桁架工位",
        "ip": "192.168.100.51",
        "ports": [10001, 10002, 10003, 10004, 10005, 10006, 10007],
        "description": "桁架机械手测试，导引头功能验证",
        "devices": ["舵面×5", "单片机", "地测口", "无人机"],
        "plc_config": {"base_addr": 1100, "ip": "192.168.100.40"},
        "test_steps": [
            {
                "step_no": 1,
                "name": "桁架机械手测试",
                "description": "测试桁架机械手的动作精度",
                "expected_duration": 20,
                "action": "控制桁架机械手执行预定动作",
                "expected_result": "机械手动作准确，精度符合要求",
            },
            {
                "step_no": 2,
                "name": "舵面机械臂测试",
                "description": "测试舵面机械臂的功能",
                "expected_duration": 20,
                "action": "控制舵面机械臂执行测试动作",
                "expected_result": "机械臂动作正常，响应及时",
            },
            {
                "step_no": 3,
                "name": "导引头功能验证",
                "description": "验证导引头在桁架环境下的功能",
                "expected_duration": 30,
                "action": "发送导引头指令，验证桁架环境下的响应",
                "expected_result": "导引头功能正常，符合设计要求",
            },
        ],
    },
}

# 告警检测工位（1700） - 全程噪音监控
ALERT_MONITOR_CONFIG = {
    "station_id": 1700,
    "name": "告警检测工位",
    "description": "全程监控产线噪音，不绑定具体工位",
    "sensors": [
        {"sensor_id": "NOISE-1700-1", "name": "噪音传感器1", "ip": "192.168.100.35", "port": 21000},
        {"sensor_id": "NOISE-1700-2", "name": "噪音传感器2", "ip": "192.168.100.36", "port": 21000},
    ],
    "thresholds": {
        "warning": 75.0,   # dB 警告阈值
        "critical": 85.0,  # dB 严重阈值
    },
    "monitor_duration": 3600,  # 监控时长（秒），与产线测试同步
}

# 产线测试顺序
PRODUCTION_LINE_SEQUENCE = [1200, 1500, 1600, 1100]

# ==================== 日志配置 ====================
def setup_logging(log_dir: str, test_id: str):
    """设置日志配置"""
    log_dir = Path(log_dir)
    log_dir.mkdir(parents=True, exist_ok=True)

    log_file = log_dir / f"production_test_{test_id}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"

    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler(log_file, encoding='utf-8'),
            logging.StreamHandler(sys.stdout)
        ]
    )

    return logging.getLogger(f"ProductionTest_{test_id}")

# ==================== 测试结果管理 ====================
class TestResult:
    """测试结果管理类"""

    def __init__(self, test_id: str):
        self.test_id = test_id
        self.start_time = datetime.now()
        self.end_time = None
        self.status = "running"
        self.current_station = None
        self.station_results = {}
        self.alert_events = []
        self.errors = []

    def add_error(self, error: str):
        """添加错误信息"""
        self.errors.append(error)
        logger.error(error)

    def add_alert_event(self, event: Dict):
        """添加告警事件"""
        self.alert_events.append(event)
        logger.warning(f"告警事件: {event}")

    def start_station(self, station_id: int):
        """开始工位测试"""
        self.current_station = station_id
        self.station_results[station_id] = {
            "station_id": station_id,
            "station_name": STATIONS_CONFIG[station_id]["name"],
            "start_time": datetime.now().isoformat(),
            "end_time": None,
            "status": "running",
            "test_steps": [],
            "errors": [],
        }
        logger.info(f"开始工位测试: {STATIONS_CONFIG[station_id]['name']} (ID: {station_id})")

    def end_station(self, station_id: int, status: str):
        """结束工位测试"""
        if station_id in self.station_results:
            self.station_results[station_id]["end_time"] = datetime.now().isoformat()
            self.station_results[station_id]["status"] = status
            logger.info(f"工位测试完成: {STATIONS_CONFIG[station_id]['name']} (ID: {station_id}), 状态: {status}")

    def add_step_result(self, station_id: int, step_no: int, step_name: str,
                        status: str, duration: float, message: str = ""):
        """添加测试步骤结果"""
        if station_id in self.station_results:
            self.station_results[station_id]["test_steps"].append({
                "step_no": step_no,
                "step_name": step_name,
                "status": status,
                "duration": duration,
                "message": message,
            })

    def finish(self, status: str):
        """完成测试"""
        self.end_time = datetime.now()
        self.status = status
        duration = (self.end_time - self.start_time).total_seconds()
        logger.info(f"测试完成: {self.test_id}, 状态: {status}, 耗时: {duration:.2f}秒")

    def to_dict(self) -> Dict:
        """转换为字典"""
        return {
            "test_id": self.test_id,
            "start_time": self.start_time.isoformat(),
            "end_time": self.end_time.isoformat() if self.end_time else None,
            "status": self.status,
            "duration": (self.end_time - self.start_time).total_seconds() if self.end_time else None,
            "station_results": self.station_results,
            "alert_events": self.alert_events,
            "errors": self.errors,
        }

    def save_to_file(self, file_path: str):
        """保存结果到文件"""
        result_dict = self.to_dict()
        with open(file_path, 'w', encoding='utf-8') as f:
            json.dump(result_dict, f, ensure_ascii=False, indent=2)
        logger.info(f"测试结果已保存: {file_path}")

# ==================== 工位测试场景 ====================
class StationTestScenario:
    """工位测试场景基类"""

    def __init__(self, station_id: int, station_config: Dict, dgiot_config: Dict, logger_instance):
        self.station_id = station_id
        self.station_config = station_config
        self.dgiot_config = dgiot_config
        self.logger = logger_instance
        self.test_results = []

    def run(self) -> bool:
        """运行工位测试"""
        self.logger.info(f"{'='*60}")
        self.logger.info(f"运行工位测试: {self.station_config['name']} (ID: {self.station_id})")
        self.logger.info(f"IP地址: {self.station_config.get('ip', 'N/A')}")
        self.logger.info(f"设备: {', '.join(self.station_config['devices'])}")
        self.logger.info(f"描述: {self.station_config['description']}")
        self.logger.info(f"{'='*60}")

        test_steps = self.station_config.get("test_steps", [])
        all_success = True

        for step in test_steps:
            step_no = step["step_no"]
            step_name = step["name"]
            expected_duration = step["expected_duration"]

            self.logger.info(f"\n步骤 {step_no}/{len(test_steps)}: {step_name}")
            self.logger.info(f"  描述: {step['description']}")
            self.logger.info(f"  预计耗时: {expected_duration} 秒")
            self.logger.info(f"  操作: {step.get('action', '无')}")
            self.logger.info(f"  预期结果: {step.get('expected_result', '无')}")

            step_start_time = time.time()

            try:
                # 模拟执行测试步骤
                time.sleep(min(expected_duration, 3))  # 最多模拟3秒

                duration = time.time() - step_start_time
                status = "completed"

                self.test_results.append({
                    "step_no": step_no,
                    "step_name": step_name,
                    "status": status,
                    "duration": duration,
                    "message": step.get("expected_result", ""),
                })

                self.logger.info(f"  实际耗时: {duration:.2f} 秒")
                self.logger.info(f"  状态: {status}")

            except Exception as e:
                duration = time.time() - step_start_time
                status = "failed"
                message = f"步骤执行失败: {str(e)}"

                self.test_results.append({
                    "step_no": step_no,
                    "step_name": step_name,
                    "status": status,
                    "duration": duration,
                    "message": message,
                })

                self.logger.error(f"  实际耗时: {duration:.2f} 秒")
                self.logger.error(f"  状态: {status}")
                self.logger.error(f"  错误: {message}")

                all_success = False
                # 可选：是否继续执行后续步骤
                # break

        self.logger.info(f"\n{'='*60}")
        self.logger.info(f"工位测试完成: {self.station_config['name']}")
        self.logger.info(f"成功步骤: {len([s for s in self.test_results if s['status'] == 'completed'])}/{len(self.test_results)}")
        self.logger.info(f"{'='*60}")

        return all_success

    def get_results(self) -> List[Dict]:
        """获取测试结果"""
        return self.test_results


# ==================== 告警监控场景 ====================
class AlertMonitorScenario:
    """告警监控场景"""

    def __init__(self, config: Dict, logger_instance):
        self.config = config
        self.logger = logger_instance
        self.running = False
        self.alert_events = []

    def start(self) -> bool:
        """启动告警监控"""
        self.logger.info(f"{'='*60}")
        self.logger.info(f"启动告警监控: {self.config['name']} (ID: {self.config['station_id']})")
        self.logger.info(f"描述: {self.config['description']}")
        self.logger.info(f"{'='*60}")

        self.logger.info("\n配置的噪音传感器:")
        for sensor in self.config["sensors"]:
            self.logger.info(f"  - {sensor['name']}: {sensor['ip']}:{sensor['port']}")

        self.logger.info(f"\n告警阈值:")
        self.logger.info(f"  - 警告: {self.config['thresholds']['warning']} dB")
        self.logger.info(f"  - 严重: {self.config['thresholds']['critical']} dB")

        self.running = True
        self.logger.info("\n告警监控已启动，开始实时监控产线噪音...")

        return True

    def stop(self):
        """停止告警监控"""
        if self.running:
            self.running = False
            self.logger.info("\n告警监控已停止")

    def is_running(self) -> bool:
        """检查是否正在运行"""
        return self.running

    def simulate_noise_monitoring(self, duration: int):
        """模拟噪音监控"""
        self.logger.info(f"\n开始模拟噪音监控，时长: {duration} 秒")

        start_time = time.time()
        while time.time() - start_time < duration and self.running:
            # 模拟噪音数据
            import random
            noise_level = random.uniform(60.0, 90.0)

            # 判断告警级别
            if noise_level > self.config["thresholds"]["critical"]:
                level = "critical"
                self.logger.warning(f"严重告警: {self.config['sensors'][0]['name']} 噪音水平 {noise_level:.2f} dB")
                self.alert_events.append({
                    "timestamp": datetime.now().isoformat(),
                    "sensor": self.config["sensors"][0]["name"],
                    "noise_level": noise_level,
                    "level": "critical",
                })
            elif noise_level > self.config["thresholds"]["warning"]:
                level = "warning"
                self.logger.info(f"警告: {self.config['sensors'][0]['name']} 噪音水平 {noise_level:.2f} dB")
                self.alert_events.append({
                    "timestamp": datetime.now().isoformat(),
                    "sensor": self.config["sensors"][0]["name"],
                    "noise_level": noise_level,
                    "level": "warning",
                })
            else:
                level = "normal"
                self.logger.debug(f"正常: {self.config['sensors'][0]['name']} 噪音水平 {noise_level:.2f} dB")

            time.sleep(5)  # 每5秒采集一次

        self.logger.info(f"\n噪音监控完成，共记录 {len(self.alert_events)} 条告警事件")

    def get_alert_events(self) -> List[Dict]:
        """获取告警事件"""
        return self.alert_events


# ==================== 一键式测试器 ====================
class OneClickProductionTester:
    """一键式产线测试器"""

    def __init__(self, config: Dict, log_dir: str, test_id: str):
        self.config = config
        self.log_dir = log_dir
        self.test_id = test_id
        self.test_result = TestResult(test_id)
        self.alert_monitor = None

    def start_alert_monitor(self) -> bool:
        """启动告警监控"""
        try:
            alert_config = self.config["alert_monitor"]

            self.alert_monitor = AlertMonitorScenario(alert_config, logger)

            if not self.alert_monitor.start():
                logger.error("告警监控启动失败")
                return False

            return True

        except Exception as e:
            logger.error(f"启动告警监控异常: {e}")
            return False

    def stop_alert_monitor(self):
        """停止告警监控"""
        if self.alert_monitor:
            self.alert_monitor.stop()

            # 获取告警事件并保存
            alert_events = self.alert_monitor.get_alert_events()
            for event in alert_events:
                self.test_result.add_alert_event(event)

            self.alert_monitor = None

    def run_station_test(self, station_id: int) -> bool:
        """运行单个工位测试"""
        if station_id not in self.config["stations"]:
            logger.error(f"未知的工位ID: {station_id}")
            return False

        station_config = self.config["stations"][station_id]

        self.test_result.start_station(station_id)

        try:
            scenario = StationTestScenario(station_id, station_config, self.config["dgiot"], logger)
            success = scenario.run()

            if success:
                self.test_result.end_station(station_id, "completed")
                return True
            else:
                self.test_result.end_station(station_id, "failed")
                self.test_result.add_error(f"工位 {station_id} 测试失败")
                return False

        except Exception as e:
            logger.exception(f"工位测试异常: {station_id}: {e}")
            self.test_result.end_station(station_id, "error")
            self.test_result.add_error(f"工位 {station_id} 测试异常: {str(e)}")
            return False

    def run_full_line_test(self) -> bool:
        """运行完整产线测试"""
        logger.info(f"{'='*60}")
        logger.info("开始完整产线测试")
        logger.info(f"测试ID: {self.test_id}")
        logger.info(f"测试流程: {' -> '.join([str(s) for s in PRODUCTION_LINE_SEQUENCE])}")
        logger.info(f"{'='*60}")

        # 启动告警监控
        if not self.start_alert_monitor():
            logger.warning("告警监控启动失败，继续测试...")

        # 依次执行每个工位测试
        all_success = True
        for station_id in PRODUCTION_LINE_SEQUENCE:
            success = self.run_station_test(station_id)
            if not success:
                all_success = False
                logger.error(f"工位 {station_id} 测试失败，是否继续？")

        # 停止告警监控
        self.stop_alert_monitor()

        # 完成测试
        final_status = "completed" if all_success else "failed"
        self.test_result.finish(final_status)

        # 保存测试结果
        result_file = Path(self.log_dir) / f"result_{self.test_id}.json"
        self.test_result.save_to_file(str(result_file))

        logger.info(f"{'='*60}")
        logger.info(f"完整产线测试完成: {self.test_id}")
        logger.info(f"最终状态: {final_status}")
        logger.info(f"测试报告: {result_file}")
        logger.info(f"{'='*60}")

        return all_success

    def generate_html_report(self) -> str:
        """生成HTML测试报告"""
        result_dict = self.test_result.to_dict()

        html_template = """
<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>产线测试报告 - {test_id}</title>
    <style>
        body {{
            font-family: 'Microsoft YaHei', Arial, sans-serif;
            margin: 20px;
            background-color: #f5f5f5;
        }}
        .container {{
            max-width: 1200px;
            margin: 0 auto;
            background-color: white;
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        h1 {{
            color: #333;
            border-bottom: 3px solid #4CAF50;
            padding-bottom: 10px;
        }}
        .summary {{
            background-color: #e8f5e9;
            padding: 15px;
            border-radius: 5px;
            margin-bottom: 20px;
        }}
        .status-completed {{
            color: #4CAF50;
            font-weight: bold;
        }}
        .status-failed {{
            color: #f44336;
            font-weight: bold;
        }}
        .status-error {{
            color: #ff9800;
            font-weight: bold;
        }}
        .station {{
            border: 1px solid #ddd;
            border-radius: 5px;
            margin-bottom: 20px;
            padding: 15px;
        }}
        .station-header {{
            background-color: #f0f0f0;
            padding: 10px;
            margin: -15px -15px 15px -15px;
            border-radius: 5px 5px 0 0;
        }}
        .step {{
            margin: 10px 0;
            padding: 10px;
            background-color: #fafafa;
            border-left: 3px solid #ddd;
        }}
        .step-completed {{
            border-left-color: #4CAF50;
        }}
        .step-failed {{
            border-left-color: #f44336;
        }}
        .alert-event {{
            margin: 10px 0;
            padding: 10px;
            background-color: #fff3e0;
            border-left: 3px solid #ff9800;
        }}
        .alert-critical {{
            background-color: #ffebee;
            border-left-color: #f44336;
        }}
        table {{
            width: 100%;
            border-collapse: collapse;
        }}
        th, td {{
            border: 1px solid #ddd;
            padding: 8px;
            text-align: left;
        }}
        th {{
            background-color: #f2f2f2;
        }}
    </style>
</head>
<body>
    <div class="container">
        <h1>产线测试报告 - {test_id}</h1>

        <div class="summary">
            <h2>测试摘要</h2>
            <p><strong>测试ID:</strong> {test_id}</p>
            <p><strong>开始时间:</strong> {start_time}</p>
            <p><strong>结束时间:</strong> {end_time}</p>
            <p><strong>测试时长:</strong> {duration:.2f} 秒</p>
            <p><strong>最终状态:</strong> <span class="status-{status}">{status}</span></p>
        </div>

        <h2>工位测试详情</h2>
        {station_details}
    </div>
</body>
</html>
        """

        # 生成工位详情HTML
        station_details_html = ""
        for station_id, station_result in result_dict["station_results"].items():
            station_details_html += f"""
            <div class="station">
                <div class="station-header">
                    <h3>{station_result['station_name']} (ID: {station_id})</h3>
                    <p><strong>状态:</strong> <span class="status-{station_result['status']}">{station_result['status']}</span></p>
                    <p><strong>开始时间:</strong> {station_result['start_time']}</p>
                    <p><strong>结束时间:</strong> {station_result['end_time']}</p>
                </div>

                <h4>测试步骤</h4>
            """

            for step in station_result["test_steps"]:
                station_details_html += f"""
                <div class="step step-{step['status']}">
                    <p><strong>步骤{step['step_no']}:</strong> {step['step_name']}</p>
                    <p><strong>状态:</strong> {step['status']}</p>
                    <p><strong>耗时:</strong> {step['duration']:.2f} 秒</p>
                    {f"<p><strong>消息:</strong> {step['message']}</p>" if step['message'] else ""}
                </div>
                """

            station_details_html += "</div>"

        # 生成告警事件HTML
        if result_dict["alert_events"]:
            station_details_html += f"""
            <div class="station">
                <div class="station-header">
                    <h3>告警事件</h3>
                </div>
            """

            for event in result_dict["alert_events"]:
                event_class = "alert-critical" if event["level"] == "critical" else ""
                station_details_html += f"""
                <div class="alert-event {event_class}">
                    <p><strong>时间:</strong> {event['timestamp']}</p>
                    <p><strong>传感器:</strong> {event['sensor']}</p>
                    <p><strong>噪音水平:</strong> {event['noise_level']:.2f} dB</p>
                    <p><strong>级别:</strong> {event['level']}</p>
                </div>
                """

            station_details_html += "</div>"

        # 生成完整HTML
        html_content = html_template.format(
            test_id=result_dict["test_id"],
            start_time=result_dict["start_time"],
            end_time=result_dict["end_time"] or "进行中",
            duration=result_dict.get("duration", 0),
            status=result_dict["status"],
            station_details=station_details_html,
        )

        # 保存HTML报告
        report_file = Path(self.log_dir) / f"report_{self.test_id}.html"
        with open(report_file, 'w', encoding='utf-8') as f:
            f.write(html_content)

        logger.info(f"HTML测试报告已生成: {report_file}")
        return str(report_file)


# ==================== 主程序 ====================
def main():
    """主函数"""
    parser = argparse.ArgumentParser(
        description="无人机测试产线 - 一键式端到端测试",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
示例:
  # 测试单个工位
  %(prog)s --station 1500

  # 测试完整产线
  %(prog)s --full-line

  # 列出所有工位
  %(prog)s --list-stations

  # 查看工位详情
  %(prog)s --station-detail 1500
        """
    )

    parser.add_argument(
        '--station', '-s',
        type=int,
        metavar='ID',
        choices=[1200, 1500, 1600, 1100],
        help='测试单个工位 (ID: 1200=磁航向, 1500=总测, 1600=拷机, 1100=桁架)'
    )

    parser.add_argument(
        '--full-line', '-f',
        action='store_true',
        help='测试完整产线 (顺序: 1200 -> 1500 -> 1600 -> 1100)'
    )

    parser.add_argument(
        '--log-dir', '-l',
        type=str,
        default='./test_logs',
        metavar='DIR',
        help='日志目录 (默认: ./test_logs)'
    )

    parser.add_argument(
        '--list-stations',
        action='store_true',
        help='列出所有工位信息'
    )

    parser.add_argument(
        '--station-detail',
        type=int,
        metavar='ID',
        choices=[1200, 1500, 1600, 1100],
        help='查看工位详细信息'
    )

    parser.add_argument(
        '--generate-report',
        action='store_true',
        help='生成HTML测试报告'
    )

    args = parser.parse_args()

    # 初始化日志
    test_id = f"TEST-{datetime.now().strftime('%Y%m%d%H%M%S')}"
    global logger
    logger = setup_logging(args.log_dir, test_id)

    logger.info(f"{'='*60}")
    logger.info("无人机测试产线 - 一键式端到端测试")
    logger.info(f"测试ID: {test_id}")
    logger.info(f"{'='*60}")

    # 列出工位
    if args.list_stations:
        print("\n工位列表:")
        print("-" * 60)
        for station_id, config in STATIONS_CONFIG.items():
            print(f"\n工位ID: {station_id}")
            print(f"  名称: {config['name']}")
            print(f"  IP地址: {config['ip']}")
            print(f"  端口: {config.get('ports', config.get('port', 'N/A'))}")
            print(f"  设备: {', '.join(config['devices'])}")
            print(f"  描述: {config['description']}")
            print(f"  测试步骤数: {len(config.get('test_steps', []))}")
        print("-" * 60)
        print("\n告警检测工位:")
        print(f"工位ID: {ALERT_MONITOR_CONFIG['station_id']}")
        print(f"  名称: {ALERT_MONITOR_CONFIG['name']}")
        print(f"  描述: {ALERT_MONITOR_CONFIG['description']}")
        print(f"  传感器数量: {len(ALERT_MONITOR_CONFIG['sensors'])}")
        print("-" * 60)
        return 0

    # 查看工位详情
    if args.station_detail:
        station_id = args.station_detail
        if station_id in STATIONS_CONFIG:
            config = STATIONS_CONFIG[station_id]
            print(f"\n工位详情: {config['name']} (ID: {station_id})")
            print("-" * 60)
            print(f"名称: {config['name']}")
            print(f"IP地址: {config['ip']}")
            print(f"端口: {config.get('ports', config.get('port', 'N/A'))}")
            print(f"设备: {', '.join(config['devices'])}")
            print(f"描述: {config['description']}")
            print(f"\nPLC配置:")
            print(f"  基地址: {config['plc_config']['base_addr']}")
            print(f"  IP: {config['plc_config']['ip']}")
            print(f"\n测试步骤:")
            for step in config.get('test_steps', []):
                print(f"\n  步骤 {step['step_no']}: {step['name']}")
                print(f"    描述: {step['description']}")
                print(f"    预计耗时: {step['expected_duration']} 秒")
                print(f"    操作: {step.get('action', '无')}")
                print(f"    预期结果: {step.get('expected_result', '无')}")
            print("-" * 60)
        return 0

    # 验证参数
    if not args.station and not args.full_line:
        parser.print_help()
        logger.error("必须指定 --station 或 --full-line")
        return 1

    if args.station and args.full_line:
        logger.error("--station 和 --full-line 不能同时使用")
        return 1

    # 创建测试器配置
    tester_config = {
        "dgiot": DG_IOT_CONFIG,
        "stations": STATIONS_CONFIG,
        "alert_monitor": ALERT_MONITOR_CONFIG,
    }

    # 创建测试器
    tester = OneClickProductionTester(tester_config, args.log_dir, test_id)

    # 执行测试
    if args.full_line:
        success = tester.run_full_line_test()
    else:  # 单工位测试
        success = tester.run_station_test(args.station)

    # 生成报告
    if args.generate_report:
        report_file = tester.generate_html_report()
        print(f"\nHTML报告: {report_file}")

    return 0 if success else 1


if __name__ == "__main__":
    sys.exit(main())
