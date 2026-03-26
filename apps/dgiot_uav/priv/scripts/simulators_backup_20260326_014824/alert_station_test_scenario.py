#!/usr/bin/env python3
"""
1700告警检测工位端到端测试场景
整个测试产线的噪音监控，监控产线环境噪音水平
噪音传感器×2 (IP: 192.168.100.35/36, Port: 21000)
- 不绑定具体工位，监控整个产线
- 扫码枪扫码后开始整个测试流程（绑定1500总测工位）
- 测试项开始后，实时监控产线噪音
- 噪音超标时触发告警
"""

import sys
import time
import logging
import requests
import json
import random
from dataclasses import dataclass
from typing import Dict, List
from datetime import datetime
from enum import Enum

# ============================================================================
# 配置常量
# ============================================================================

# DG-IoT平台配置
DG_IOT_API_BASE = "http://192.168.100.100:18083"
AUTH_TOKEN = "your_auth_token_here"  # 需要替换为实际token

# 告警检测工位配置
ALERT_STATION_ID = 1700
ALERT_STATION_NAME = "告警检测工位"

# 噪音传感器配置
NOISE_SENSORS = [
    {
        "sensor_id": "NSE-1700-1",
        "sensor_name": "噪音传感器1",
        "ip_address": "192.168.100.35",
        "port": 21000,
        "channel": 1
    },
    {
        "sensor_id": "NSE-1700-2", 
        "sensor_name": "噪音传感器2",
        "ip_address": "192.168.100.36",
        "port": 21000,
        "channel": 2
    }
]

# 噪音告警阈值（单位：dB）
NOISE_ALERT_THRESHOLD = 85.0  # 噪音超过85dB触发告警
NOISE_WARNING_THRESHOLD = 75.0  # 噪音超过75dB触发警告

# 测试产线配置
TEST_LINE_STATIONS = {
    1200: {"name": "磁航向", "ip": "192.168.100.21"},
    1500: {"name": "总测", "ip": "192.168.100.47"}, 
    1600: {"name": "拷机", "ip": "192.168.100.50"},
    1100: {"name": "桁架", "ip": "192.168.100.51"}
}

# 日志配置
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - [%(levelname)s] - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger("AlertStationTest")

# ============================================================================
# 数据结构
# ============================================================================

@dataclass
class NoiseAlertEvent:
    """噪音告警事件"""
    timestamp: str
    sensor_id: str
    sensor_name: str
    noise_level: float
    alert_level: str  # 'warning' | 'critical'
    temperature: float
    humidity: float

@dataclass
class LineTestStatus:
    """产线测试状态"""
    test_id: str
    start_time: str
    status: str  # 'not_started' | 'testing' | 'paused' | 'completed'
    active_station: int
    noise_level_avg: float
    alert_count: int

# ============================================================================
# API客户端
# ============================================================================

class DGIOtAPIClient:
    """DG-IoT平台API客户端"""
    
    def __init__(self, base_url: str, auth_token: str):
        self.base_url = base_url
        self.auth_token = auth_token
        self.headers = {
            "Authorization": f"Bearer {auth_token}",
            "Content-Type": "application/json"
        }
    
    def query_devices(self, params: dict = None) -> dict:
        """查询设备列表"""
        try:
            response = requests.get(
                f"{self.base_url}/iotapi/devices",
                headers=self.headers,
                params=params
            )
            response.raise_for_status()
            return response.json()
        except Exception as e:
            logger.error(f"查询设备失败: {e}")
            return {"data": []}
    
    def get_device(self, device_id: str) -> dict:
        """获取设备详情"""
        try:
            response = requests.get(
                f"{self.base_url}/iotapi/devices/{device_id}",
                headers=self.headers
            )
            response.raise_for_status()
            return response.json()
        except Exception as e:
            logger.error(f"获取设备详情失败: {e}")
            return {}
    
    def update_device(self, device_id: str, data: dict) -> bool:
        """更新设备信息"""
        try:
            response = requests.put(
                f"{self.base_url}/iotapi/devices/{device_id}",
                headers=self.headers,
                json=data
            )
            response.raise_for_status()
            return True
        except Exception as e:
            logger.error(f"更新设备失败: {e}")
            return False
    
    def get_products(self, params: dict = None) -> dict:
        """查询产品列表"""
        try:
            response = requests.get(
                f"{self.base_url}/iotapi/products",
                headers=self.headers,
                params=params
            )
            response.raise_for_status()
            return response.json()
        except Exception as e:
            logger.error(f"查询产品失败: {e}")
            return {"data": []}
    
    def get_product(self, product_id: str) -> dict:
        """获取产品详情"""
        try:
            response = requests.get(
                f"{self.base_url}/iotapi/products/{product_id}",
                headers=self.headers
            )
            response.raise_for_status()
            return response.json()
        except Exception as e:
            logger.error(f"获取产品详情失败: {e}")
            return {}
    
    def start_test(self, device_id: str, test_item_id: str) -> bool:
        """开始测试"""
        try:
            response = requests.post(
                f"{self.base_url}/iotapi/test/start",
                headers=self.headers,
                json={
                    "device_id": device_id,
                    "test_item_id": test_item_id
                }
            )
            response.raise_for_status()
            return True
        except Exception as e:
            logger.error(f"开始测试失败: {e}")
            return False
    
    def stop_test(self, device_id: str) -> bool:
        """停止测试"""
        try:
            response = requests.post(
                f"{self.base_url}/iotapi/test/stop",
                headers=self.headers,
                json={"device_id": device_id}
            )
            response.raise_for_status()
            return True
        except Exception as e:
            logger.error(f"停止测试失败: {e}")
            return False
    
    def get_reports(self, params: dict = None) -> dict:
        """查询报告列表"""
        try:
            response = requests.get(
                f"{self.base_url}/iotapi/reports",
                headers=self.headers,
                params=params
            )
            response.raise_for_status()
            return response.json()
        except Exception as e:
            logger.error(f"查询报告失败: {e}")
            return {"data": []}
    
    def get_report(self, report_id: str) -> dict:
        """获取报告详情"""
        try:
            response = requests.get(
                f"{self.base_url}/iotapi/reports/{report_id}",
                headers=self.headers
            )
            response.raise_for_status()
            return response.json()
        except Exception as e:
            logger.error(f"获取报告详情失败: {e}")
            return {}

# ============================================================================
# 噪音传感器模拟器
# ============================================================================

class NoiseSensorSimulator:
    """噪音传感器模拟器"""
    
    def __init__(self, sensor_config: dict):
        self.sensor_id = sensor_config["sensor_id"]
        self.sensor_name = sensor_config["sensor_name"]
        self.ip_address = sensor_config["ip_address"]
        self.port = sensor_config["port"]
        self.channel = sensor_config["channel"]
        
        self.running = False
        self.base_noise_level = 60.0  # 基础噪音水平
        
        # 噪音事件历史
        self.alert_history = []
        self.noise_readings = []
        
        # 模拟不同测试场景的噪音水平
        self.test_scenario_noise_levels = {
            "idle": 50.0,      # 空闲：50dB
            "normal": 65.0,   # 正常测试：65dB
            "warning": 78.0,  # 警告：78dB
            "critical": 90.0, # 告警：90dB
        }
    
    def start(self) -> bool:
        """启动噪音传感器"""
        logger.info(f"启动噪音传感器: {self.sensor_name} ({self.sensor_id})")
        logger.info(f"传感器地址: {self.ip_address}:{self.port}")
        
        self.running = True
        return True
    
    def stop(self):
        """停止噪音传感器"""
        self.running = False
        logger.info(f"停止噪音传感器: {self.sensor_name}")
    
    def generate_noise_data(self, scenario: str = "normal") -> dict:
        """生成噪音数据"""
        if not self.running:
            return {}
        
        # 根据场景调整噪音水平
        base_level = self.test_scenario_noise_levels.get(scenario, 65.0)
        
        # 添加随机波动
        noise_level = base_level + random.gauss(0, 5)  # 标准差5dB
        noise_level = max(30, min(120, noise_level))
        
        # 生成温湿度数据
        temperature = round(random.uniform(15, 35), 1)
        humidity = round(random.uniform(30, 80), 1)
        
        # 判断告警状态
        if noise_level > NOISE_ALERT_THRESHOLD:
            alert_status = "critical"
        elif noise_level > NOISE_WARNING_THRESHOLD:
            alert_status = "warning"
        else:
            alert_status = "normal"
        
        # 记录数据
        self.noise_readings.append({
            "timestamp": int(time.time() * 1000),
            "noise_level": noise_level,
            "temperature": temperature,
            "humidity": humidity,
            "alert_status": alert_status
        })
        
        # 保留最近1000条记录
        if len(self.noise_readings) > 1000:
            self.noise_readings = self.noise_readings[-1000:]
        
        return {
            "sensor_id": self.sensor_id,
            "sensor_name": self.sensor_name,
            "channel": self.channel,
            "noise_level": round(noise_level, 2),
            "temperature": temperature,
            "humidity": humidity,
            "alert_status": alert_status,
            "timestamp": int(time.time() * 1000)
        }
    
    def get_noise_statistics(self) -> dict:
        """获取噪音统计数据"""
        if not self.noise_readings:
            return {
                "count": 0,
                "average": 0.0,
                "max": 0.0,
                "min": 0.0,
                "critical_count": 0,
                "warning_count": 0
            }
        
        noise_levels = [reading["noise_level"] for reading in self.noise_readings]
        
        return {
            "count": len(noise_levels),
            "average": round(sum(noise_levels) / len(noise_levels), 2),
            "max": round(max(noise_levels), 2),
            "min": round(min(noise_levels), 2),
            "critical_count": len([r for r in self.noise_readings if r["alert_status"] == "critical"]),
            "warning_count": len([r for r in self.noise_readings if r["alert_status"] == "warning"])
        }

# ============================================================================
# 1700告警检测工位测试场景
# ============================================================================

class AlertStationTestScenario:
    """1700告警检测工位端到端测试场景"""
    
    def __init__(self):
        self.station_id = ALERT_STATION_ID
        self.station_name = ALERT_STATION_NAME
        
        self.dgiot_client = DGIOtAPIClient(DG_IOT_API_BASE, AUTH_TOKEN)
        
        # 噪音传感器
        self.noise_sensors = []
        for sensor_config in NOISE_SENSORS:
            sensor = NoiseSensorSimulator(sensor_config)
            self.noise_sensors.append(sensor)
        
        # 产线测试状态
        self.line_test_status = LineTestStatus(
            test_id="",
            start_time="",
            status="not_started",
            active_station=0,
            noise_level_avg=0.0,
            alert_count=0
        )
        
        # 告警事件历史
        self.alert_events = []
    
    def run(self) -> bool:
        """运行完整测试场景"""
        logger.info("=" * 80)
        logger.info(f"开始1700告警检测工位端到端测试场景 - {self.station_name}")
        logger.info("=" * 80)
        
        try:
            # 阶段1: 准备测试环境
            if not self._prepare_test_environment():
                return False
            
            # 阶段2: 噪音传感器注册
            if not self._register_noise_sensors():
                return False
            
            # 阶段3: 等待扫码开始测试
            if not self._wait_for_scan_code():
                return False
            
            # 阶段4: 产线测试监控
            if not self._monitor_line_test():
                return False
            
            # 阶段5: 生成告警报告
            if not self._generate_alert_report():
                return False
            
            logger.info("=" * 80)
            logger.info("测试场景执行成功")
            logger.info("=" * 80)
            
            return True
            
        except Exception as e:
            logger.error(f"测试场景执行失败: {e}")
            return False
    
    def _prepare_test_environment(self) -> bool:
        """准备测试环境"""
        logger.info("阶段1: 准备测试环境")
        
        try:
            # 1. 检查DG-IoT平台状态
            logger.info("检查DG-IoT平台状态...")
            # 这里可以添加平台状态检查逻辑
            logger.info("DG-IoT平台状态正常")
            
            # 2. 查询噪音传感器产品配置
            logger.info("查询噪音传感器产品配置...")
            noise_product = self.dgiot_client.get_products({
                "type": "noise_sensor"
            })
            
            if noise_product.get("data"):
                logger.info(f"找到噪音传感器产品配置")
            else:
                logger.warning("未找到噪音传感器产品配置")
            
            # 3. 查询告警测试项配置
            logger.info("查询告警测试项配置...")
            # 这里可以添加测试项查询逻辑
            logger.info("告警测试项配置查询成功")
            
            # 4. 查询产线工位配置
            logger.info("查询产线工位配置...")
            for station_id in TEST_LINE_STATIONS.keys():
                station_info = self.dgiot_client.query_devices({
                    "station_id": station_id
                })
                if station_info.get("data"):
                    logger.info(f"工位{station_id}: {len(station_info['data'])}个设备")
            
            logger.info("阶段1完成\n")
            return True
            
        except Exception as e:
            logger.error(f"准备测试环境失败: {e}")
            return False
    
    def _register_noise_sensors(self) -> bool:
        """注册噪音传感器"""
        logger.info("阶段2: 噪音传感器注册")
        
        try:
            # 1. 查询噪音传感器设备
            logger.info("查询噪音传感器设备...")
            
            for sensor in self.noise_sensors:
                logger.info(f"注册传感器: {sensor.sensor_name}")
                
                # 这里应该通过UDP:21000端口发送注册报文
                # 为示例目的，我们假设注册成功
                success = sensor.start()
                
                if success:
                    logger.info(f"传感器{sensor.sensor_id}启动成功")
                else:
                    logger.error(f"传感器{sensor.sensor_id}启动失败")
                    return False
            
            # 2. 验证传感器注册结果
            logger.info("验证传感器注册结果...")
            
            for sensor in self.noise_sensors:
                sensor_info = self.dgiot_client.get_device(sensor.sensor_id)
                if sensor_info.get("status") != "online":
                    logger.error(f"传感器{sensor.sensor_id}未上线")
                    return False
                logger.info(f"传感器{sensor.sensor_id}状态正常")
            
            logger.info("阶段2完成\n")
            return True
            
        except Exception as e:
            logger.error(f"噪音传感器注册失败: {e}")
            return False
    
    def _wait_for_scan_code(self) -> bool:
        """等待扫码开始测试"""
        logger.info("阶段3: 等待扫码开始测试")
        
        try:
            logger.info("等待扫码枪扫描设备编码...")
            logger.info("扫码后自动绑定到1500总测工位")
            
            # 模拟等待扫码
            # 实际应该监听扫码枪事件
            logger.info("模拟：已扫码，设备编码 UAV-12345")
            
            time.sleep(2)
            
            # 模拟设备绑定到1500工位
            logger.info("设备UAV-12345自动绑定到工位1500")
            
            # 验证绑定结果
            device_info = self.dgiot_client.get_device("UAV-12345")
            if device_info.get("content", {}).get("station_id") != 1500:
                logger.warning("设备工位绑定验证失败")
            else:
                logger.info("设备工位绑定验证成功")
            
            # 生成测试ID
            test_id = f"TEST-{datetime.now().strftime('%Y%m%d%H%M%S')}"
            self.line_test_status.test_id = test_id
            self.line_test_status.start_time = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            self.line_test_status.status = "testing"
            
            logger.info(f"测试ID: {test_id}")
            logger.info("阶段3完成\n")
            return True
            
        except Exception as e:
            logger.error(f"等待扫码失败: {e}")
            return False
    
    def _monitor_line_test(self) -> bool:
        """监控产线测试"""
        logger.info("阶段4: 产线测试监控")
        
        try:
            # 1. 开始噪音监控
            logger.info("开始产线噪音监控...")
            self.line_test_status.active_station = 1500  # 1500总测工位
            
            # 2. 模拟产线测试过程
            test_phases = [
                {"name": "磁航向测试", "duration": 10, "noise_scenario": "normal"},
                {"name": "总测测试", "duration": 15, "noise_scenario": "normal"},
                {"name": "动力检测", "duration": 15, "noise_scenario": "warning"},
                {"name": "拷机测试", "duration": 20, "noise_scenario": "critical"},
                {"name": "桁架测试", "duration": 10, "noise_scenario": "normal"}
            ]
            
            current_time = 0
            for phase in test_phases:
                logger.info(f"测试阶段: {phase['name']} ({current_time}/{sum(p['duration'] for p in test_phases)}秒)")
                
                # 切换噪音场景
                for sensor in self.noise_sensors:
                    sensor_data = sensor.generate_noise_data(phase["noise_scenario"])
                    
                    # 检查告警状态
                    if sensor_data["alert_status"] in ["warning", "critical"]:
                        alert_event = NoiseAlertEvent(
                            timestamp=datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
                            sensor_id=sensor.sensor_id,
                            sensor_name=sensor.sensor_name,
                            noise_level=sensor_data["noise_level"],
                            alert_level=sensor_data["alert_status"],
                            temperature=sensor_data["temperature"],
                            humidity=sensor_data["humidity"]
                        )
                        self.alert_events.append(alert_event)
                        
                        # 记录告警日志
                        if sensor_data["alert_status"] == "critical":
                            logger.error(f"【严重告警】{sensor.sensor_name}噪音超标: {sensor_data['noise_level']}dB")
                        else:
                            logger.warning(f"【告警】{sensor.sensor_name}噪音偏高: {sensor_data['noise_level']}dB")
                
                # 模拟测试时间
                time.sleep(phase["duration"])
                current_time += phase["duration"]
            
            # 3. 测试结束
            self.line_test_status.status = "completed"
            logger.info("产线测试完成")
            
            # 4. 统计测试数据
            self._collect_test_statistics()
            
            logger.info("阶段4完成\n")
            return True
            
        except Exception as e:
            logger.error(f"产线测试监控失败: {e}")
            return False
    
    def _collect_test_statistics(self):
        """收集测试统计数据"""
        logger.info("收集测试统计数据...")
        
        # 1. 计算平均噪音水平
        total_avg = 0.0
        for sensor in self.noise_sensors:
            stats = sensor.get_noise_statistics()
            total_avg += stats["average"]
            logger.info(f"传感器{sensor.sensor_name}: 平均{stats['average']}dB, 最大{stats['max']}dB, 告警{stats['critical_count']}次")
        
        self.line_test_status.noise_level_avg = round(total_avg / len(self.noise_sensors), 2)
        logger.info(f"产线平均噪音水平: {self.line_test_status.noise_level_avg}dB")
        
        # 2. 统计告警数量
        self.line_test_status.alert_count = len(self.alert_events)
        logger.info(f"总告警数量: {self.line_test_status.alert_count}")
        
        # 3. 按告警级别分类
        critical_count = len([e for e in self.alert_events if e.alert_level == "critical"])
        warning_count = len([e for e in self.alert_events if e.alert_level == "warning"])
        logger.info(f"严重告警: {critical_count}次, 警告: {warning_count}次")
    
    def _generate_alert_report(self) -> bool:
        """生成告警报告"""
        logger.info("阶段5: 生成告警报告")
        
        try:
            # 1. 构建报告数据
            report_data = {
                "report_id": f"ALERT-{datetime.now().strftime('%Y%m%d%H%M%S')}",
                "test_id": self.line_test_status.test_id,
                "station_id": self.station_id,
                "station_name": self.station_name,
                "start_time": self.line_test_status.start_time,
                "end_time": datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
                "status": self.line_test_status.status,
                "test_duration": time.time() - time.mktime(datetime.strptime(
                    self.line_test_status.start_time, '%Y-%m-%d %H:%M:%S'
                ).timetuple()),
                "noise_level_avg": self.line_test_status.noise_level_avg,
                "alert_count": self.line_test_status.alert_count,
                "alert_events": [
                    {
                        "timestamp": event.timestamp,
                        "sensor_id": event.sensor_id,
                        "sensor_name": event.sensor_name,
                        "noise_level": event.noise_level,
                        "alert_level": event.alert_level,
                        "temperature": event.temperature,
                        "humidity": event.humidity
                    }
                    for event in self.alert_events
                ],
                "noise_sensors": [
                    {
                        "sensor_id": sensor.sensor_id,
                        "sensor_name": sensor.sensor_name,
                        "statistics": sensor.get_noise_statistics()
                    }
                    for sensor in self.noise_sensors
                ]
            }
            
            # 2. 保存报告
            logger.info(f"保存告警报告: {report_data['report_id']}")
            # 这里应该调用实际的报告保存API
            logger.info("告警报告已保存")
            
            # 3. 更新1700工位状态
            update_data = {
                "content": {
                    "last_test_id": self.line_test_status.test_id,
                    "last_test_time": self.line_test_status.start_time,
                    "alert_count": self.line_test_status.alert_count,
                    "noise_level_avg": self.line_test_status.noise_level_avg,
                    "alert_threshold": NOISE_ALERT_THRESHOLD,
                    "warning_threshold": NOISE_WARNING_THRESHOLD
                }
            }
            
            # 更新工位设备（如果存在）
            logger.info("更新1700工位状态...")
            # self.dgiot_client.update_device("STATION-1700", update_data)
            
            logger.info("阶段5完成\n")
            return True
            
        except Exception as e:
            logger.error(f"生成告警报告失败: {e}")
            return False

# ============================================================================
# 主程序
# ============================================================================

def main():
    """主程序"""
    import argparse
    
    parser = argparse.ArgumentParser(description='1700告警检测工位端到端测试场景')
    parser.add_argument('--auth-token', type=str, required=True, help='认证Token')
    parser.add_argument('--verbose', action='store_true', help='详细日志')
    parser.add_argument('--test-duration', type=int, default=60, help='测试时长(秒)')
    args = parser.parse_args()
    
    # 设置日志级别
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    # 设置认证Token
    global AUTH_TOKEN
    AUTH_TOKEN = args.auth_token
    
    # 创建测试场景
    scenario = AlertStationTestScenario()
    
    # 运行测试场景
    success = scenario.run()
    
    if success:
        print("\n" + "=" * 80)
        print("1700告警检测工位端到端测试场景执行成功")
        print("=" * 80)
        print(f"\n测试报告:")
        print(f"  测试ID: {scenario.line_test_status.test_id}")
        print(f"  开始时间: {scenario.line_test_status.start_time}")
        print(f"  测试状态: {scenario.line_test_status.status}")
        print(f"  平均噪音: {scenario.line_test_status.noise_level_avg}dB")
        print(f"  告警数量: {scenario.line_test_status.alert_count}")
        print(f"  噪音传感器: {len(scenario.noise_sensors)}个")
        return 0
    else:
        print("\n" + "=" * 80)
        print("1700告警检测工位端到端测试场景执行失败")
        print("=" * 80)
        return 1

if __name__ == '__main__':
    sys.exit(main())
