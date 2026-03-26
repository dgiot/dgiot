#!/usr/bin/env python3
"""
无人机测试产线集成调度器
协调三个模拟器（治具模拟器、PLC模拟器、无人机模拟器）按工位线路顺序上线/下线，
与PLC指令联动，避免重复上报，实现完整的测试用例循环验证。

设计思路：
1. 中央调度器管理所有模拟器实例
2. 测试用例定义工位顺序、设备上线时序、PLC指令触发
3. 通过直接调用模拟器类（而非子进程）实现精细控制
4. 标记机制确保每个设备只上报一次
5. 支持多个测试场景循环执行

用法：
python3 integrated_production_line.py --test-case normal --cycles 3
"""

import argparse
import json
import subprocess
from datetime import datetime
import threading
import signal
import time
import logging
import sys
import os
from datetime import datetime
from typing import Dict, List, Any, Optional, Callable
from enum import Enum

# 添加当前目录到路径，以便导入模拟器模块
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# 导入三个模拟器模块
try:
    from fixture_simulator import DeviceClient, create_devices, ensure_ips
    import fixture_simulator as fs
except ImportError as e:
    print(f"无法导入fixture_simulator: {e}")
    sys.exit(1)

try:
    # PLC模拟器作为服务器运行，我们需要pymodbus客户端来发送指令
    from pymodbus.client import ModbusTcpClient
except ImportError:
    print("请安装pymodbus: pip install pymodbus")
    sys.exit(1)

try:
    from uav_simulator import UAVSimulator, Drone, GroundStation, HandheldController
    from multicast_core import MulticastCore
except ImportError as e:
    print(f"无法导入uav_simulator或multicast_core: {e}")
    # UAV模拟器可选
    UAVSimulator = None
    Drone = None
    MulticastCore = None

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

# ==================== 配置常量 ====================
# 工位配置（IP到工位地址映射） - 根据用户提供的完整工位地址表
# 格式：工位IP -> {station_addr: 治具工位地址, name: 工位名称, type: 工位类型, plc_base_addr: PLC基地址}
STATION_CONFIG = {
    "192.168.100.21": {"station_addr": 1700, "name": "磁航向工位", "type": "磁航向", "plc_base_addr": 1700, "plc_ip": "192.168.100.20"},
    "192.168.100.52": {"station_addr": 255, "name": "上料台", "type": "上料台", "plc_base_addr": None, "plc_ip": None},
    "192.168.100.45": {"station_addr": 1, "name": "总测工位2", "type": "总测", "plc_base_addr": 1600, "plc_ip": "192.168.100.40"},
    "192.168.100.46": {"station_addr": 2, "name": "总测工位2-动力检测", "type": "动力检测", "plc_base_addr": 1600, "plc_ip": "192.168.100.40"},
    "192.168.100.47": {"station_addr": 3, "name": "总测工位1", "type": "总测", "plc_base_addr": 1500, "plc_ip": "192.168.100.40"},
    "192.168.100.48": {"station_addr": 4, "name": "总测工位1-动力检测", "type": "动力检测", "plc_base_addr": 1500, "plc_ip": "192.168.100.40"},
    "192.168.100.49": {"station_addr": 5, "name": "拷机工位2", "type": "拷机", "plc_base_addr": 1300, "plc_ip": "192.168.100.40"},
    "192.168.100.50": {"station_addr": 6, "name": "拷机工位1", "type": "拷机", "plc_base_addr": 1200, "plc_ip": "192.168.100.40"},
    "192.168.100.51": {"station_addr": 7, "name": "桁行架", "type": "桁架", "plc_base_addr": 1100, "plc_ip": "192.168.100.40"},
}

# 特殊设备配置
SPECIAL_DEVICES = {
    "noise_sensors": [
        {"ip": "192.168.100.35", "addr": 1, "port": 21000},
        {"ip": "192.168.100.36", "addr": 2, "port": 21000},
    ],
    "scanner": {
        "ip": "192.168.100.23",
        "port": 1234,
    },
    "magnetic_station": {
        "ip": "192.168.100.21",
        "port": 10007,
        "scanner_ip": "192.168.100.23",
        "scanner_port": 1234,
    },
}

# PLC三通道配置（根据用户提供的PLC IP和工位基地址）
# 注意：当调度器自己启动PLC模拟器时，PLC模拟器监听0.0.0.0:502
# 此时PLCClient应连接127.0.0.1:502而非远程PLC IP
PLC_CHANNELS = {
    "magnetic": {"ip": "127.0.0.1", "base_addr": 1700, "description": "磁航向台体", "stations": ["192.168.100.21", "192.168.100.52"]},
    "total_test": {"ip": "127.0.0.1", "base_addr": 1500, "description": "总测机械臂", "stations": ["192.168.100.45", "192.168.100.46", "192.168.100.47", "192.168.100.48"]},
    "bake_and_gantry": {"ip": "127.0.0.1", "base_addr": 1100, "description": "拷机气缸/桁架机械手", "stations": ["192.168.100.49", "192.168.100.50", "192.168.100.51"]},
}

# 完整的产线路径矩阵
# 流程：磁航向 -> 总测 -> 动力检测 -> 拷机 -> 桁架
# 根据MES配置，产线可能是A或B，这里定义所有可能的完整路径
PRODUCTION_LINE_MATRIX = {
    "line_a_path1": {
        "name": "产线A-路径1",
        "sequence": [
            {"stage": "磁航向", "ip": "192.168.100.21", "description": "磁航向工位（扫码绑定）"},
            {"stage": "总测", "ip": "192.168.100.45", "description": "总测工位2"},
            {"stage": "动力检测", "ip": "192.168.100.46", "description": "总测工位2-动力检测（含噪音传感器）"},
            {"stage": "拷机", "ip": "192.168.100.49", "description": "拷机工位2（含导引头）"},
            {"stage": "桁架", "ip": "192.168.100.51", "description": "桁行架（含导引头）"},
        ],
        "mes_line": "A",
        "special_devices": ["scanner", "noise_sensor", "guidance_head"],
    },
    "line_a_path2": {
        "name": "产线A-路径2", 
        "sequence": [
            {"stage": "磁航向", "ip": "192.168.100.21", "description": "磁航向工位（扫码绑定）"},
            {"stage": "总测", "ip": "192.168.100.47", "description": "总测工位1"},
            {"stage": "动力检测", "ip": "192.168.100.48", "description": "总测工位1-动力检测（含噪音传感器）"},
            {"stage": "拷机", "ip": "192.168.100.50", "description": "拷机工位1（含导引头）"},
            {"stage": "桁架", "ip": "192.168.100.51", "description": "桁行架（含导引头）"},
        ],
        "mes_line": "A",
        "special_devices": ["scanner", "noise_sensor", "guidance_head"],
    },
    "line_b_path1": {
        "name": "产线B-路径1",
        "sequence": [
            {"stage": "磁航向", "ip": "192.168.100.21", "description": "磁航向工位（扫码绑定）"},
            {"stage": "总测", "ip": "192.168.100.45", "description": "总测工位2"},
            {"stage": "动力检测", "ip": "192.168.100.46", "description": "总测工位2-动力检测（含噪音传感器）"},
            {"stage": "拷机", "ip": "192.168.100.49", "description": "拷机工位2（含导引头）"},
            {"stage": "桁架", "ip": "192.168.100.51", "description": "桁行架（含导引头）"},
        ],
        "mes_line": "B",
        "special_devices": ["scanner", "noise_sensor", "guidance_head"],
    },
    "line_b_path2": {
        "name": "产线B-路径2",
        "sequence": [
            {"stage": "磁航向", "ip": "192.168.100.21", "description": "磁航向工位（扫码绑定）"},
            {"stage": "总测", "ip": "192.168.100.47", "description": "总测工位1"},
            {"stage": "动力检测", "ip": "192.168.100.48", "description": "总测工位1-动力检测（含噪音传感器）"},
            {"stage": "拷机", "ip": "192.168.100.50", "description": "拷机工位1（含导引头）"},
            {"stage": "桁架", "ip": "192.168.100.51", "description": "桁行架（含导引头）"},
        ],
        "mes_line": "B",
        "special_devices": ["scanner", "noise_sensor", "guidance_head"],
    },
}

# 工位设备组合差异
STATION_DEVICE_CONFIG = {
    "磁航向": {
        "base_devices": ["地测口"],  # 10007端口
        "special_devices": ["scanner"],  # 1234端口扫描枪
        "description": "固定IP .21，地测口+扫描枪联动，扫码绑定业务",
        "plc_channel": "magnetic",
    },
    "总测": {
        "base_devices": ["舵面×5", "单片机", "地测口", "无人机"],
        "special_devices": [],
        "description": "基础组合：5个舵面传感器+单片机+地测口+无人机",
        "plc_channel": "total_test",
    },
    "动力检测": {
        "base_devices": ["舵面×5", "单片机", "地测口", "无人机"],
        "special_devices": ["noise_sensor"],  # 噪音传感器 .35/.36:21000
        "description": "基础组合+噪音传感器（声学检测）",
        "plc_channel": "total_test",
    },
    "拷机": {
        "base_devices": ["舵面×5", "单片机", "地测口", "无人机"],
        "special_devices": ["guidance_head"],  # 导引头（通过D3遥测数据体现）
        "description": "基础组合+导引头，遥控权限互斥逻辑",
        "plc_channel": "bake_and_gantry",
    },
    "桁架": {
        "base_devices": ["舵面×5", "单片机", "地测口", "无人机"],
        "special_devices": ["guidance_head"],  # 导引头
        "description": "基础组合+导引头，遥控权限互斥逻辑",
        "plc_channel": "bake_and_gantry",
    },
    "上料台": {
        "base_devices": [],
        "special_devices": [],
        "description": "上料准备工位",
        "plc_channel": None,
    },
}

# 设备类型映射
DEVICE_TYPES = {
    "wrj_dm_zqy": "左前翼舵面",
    "wrj_dm_yqy": "右前翼舵面",
    "wrj_dm_zcw": "左侧翼舵面",
    "wrj_dm_ycw": "右侧翼舵面",
    "wrj_dm_zhj": "治具基准舵面",
    "wrj_danpianji": "单片机",
    "wrj_dicekou": "地测口",
    "scanner": "扫描枪",
    "noise_sensor": "噪音传感器",
}

PLC_SERVER_PORT = 502

# ==================== MES配置 ====================
MES_ENABLED = False  # 默认不启用MES模拟
MES_IP = "172.1.2.222"
MES_PORT = 801
MES_API_ENDPOINT = f"http://{MES_IP}:{MES_PORT}/lezao/jymes/api/equip/proExec"
MES_SCRIPT_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "mes_simulator.py")

# ==================== 工位状态枚举 ====================
class StationState(Enum):
    IDLE = "idle"           # 空闲
    PREPARING = "preparing" # 准备上线
    ONLINE = "online"       # 设备已上线
    TESTING = "testing"     # 测试进行中
    COMPLETED = "completed" # 测试完成
    OFFLINE = "offline"     # 设备已下线
    ERROR = "error"         # 错误状态

# ==================== 测试阶段枚举 ====================
class TestStage(Enum):
    NOT_STARTED = "not_started"      # 未开始
    DEVICES_ONLINE = "devices_online" # 设备已上线
    PLC_COMMAND_SENT = "plc_command_sent" # PLC指令已发送
    TEST_EXECUTING = "test_executing" # 测试执行中
    TEST_COMPLETED = "test_completed" # 测试完成
    DEVICES_OFFLINE = "devices_offline" # 设备已下线

# ==================== 设备管理器 ====================
class DeviceManager:
    """管理治具模拟器设备和无人机模拟器的生命周期"""
    def __init__(self, dgiot_host: str, dgiot_port: int):
        self.dgiot_host = dgiot_host
        self.dgiot_port = dgiot_port
        self.devices: Dict[str, DeviceClient] = {}  # device_id -> DeviceClient
        self.device_states: Dict[str, str] = {}     # device_id -> state
        self.reported_devices: set = set()          # 已上报的设备ID
        self.uav_drones: Dict[str, Any] = {}        # station_ip -> drone实例
        self.uav_threads: Dict[str, Any] = {}        # station_ip -> (recv_thread, send_thread)
        self.uav_running_flags: Dict[str, List] = {}  # station_ip -> [running_flag]
        self.guidance_head_networked: Dict[str, bool] = {}  # station_ip -> 导引头是否已入网
        self._lock = threading.RLock()
        
    def create_station_devices(self, station_ip: str, station_addr: int) -> List[Dict]:
        """为指定工位创建设备配置"""
        devices = []
        
        # 舵面传感器 (10001-10005)
        sensor_ports = [10001, 10002, 10003, 10004, 10005]
        for i, (dev_id, dev_type) in enumerate(fs.SENSOR_DEVICES):
            port = sensor_ports[i]
            devices.append({
                'id': dev_id,
                'port': port,
                'type': f'{dev_type}_IP{station_ip}',
                'protocol': 'modbus_rtu',
                'send_data': False,
                'sensor_addr': station_addr,
                'src_ip': station_ip
            })
        
        # 单片机 (10006)
        devices.append({
            'id': 'wrj_danpianji',
            'port': 10006,
            'type': f'单片机_IP{station_ip}',
            'protocol': 'modbus_rtu',
            'send_data': False,
            'sensor_addr': station_addr,
            'src_ip': station_ip
        })
        
        # 地测口 (10007)
        devices.append({
            'id': 'wrj_dicekou',
            'port': 10007,
            'type': f'地测口_IP{station_ip}',
            'protocol': 'eb90',
            'send_data': True,
            'sensor_addr': station_addr,
            'src_ip': station_ip
        })
        
        return devices
    
    def start_devices(self, station_ip: str, station_addr: int) -> List[DeviceClient]:
        """启动指定工位的所有设备（包括无人机）"""
        device_configs = self.create_station_devices(station_ip, station_addr)
        started_devices = []
        
        with self._lock:
            for config in device_configs:
                device_id = config['id']
                if device_id in self.devices:
                    logger.warning(f"设备 {device_id} 已存在，跳过")
                    continue
                    
                # 创建设备客户端
                device = DeviceClient(
                    self.dgiot_host,
                    self.dgiot_port,
                    config,
                    no_bind=False  # 需要绑定IP
                )
                device.daemon = True
                device.start()
                
                self.devices[device_id] = device
                self.device_states[device_id] = "starting"
                started_devices.append(device)
                
                logger.info(f"启动设备: {device_id} (工位 {station_addr}, IP: {station_ip})")
                time.sleep(0.1)  # 避免同时连接过多
        
        # 等待设备连接成功
        time.sleep(2)
        
        # 启动无人机模拟器（如果可用）
        self.start_uav(station_ip, station_addr)
        
        return started_devices
    
    def stop_devices(self, station_ip: str = None):
        """停止设备，如果指定station_ip则只停止该工位的设备（包括无人机）"""
        with self._lock:
            to_stop = []
            for device_id, device in self.devices.items():
                if station_ip is None or (device.src_ip and device.src_ip == station_ip):
                    to_stop.append(device_id)
            
            for device_id in to_stop:
                device = self.devices[device_id]
                device.stop()
                device.join(timeout=2)
                del self.devices[device_id]
                del self.device_states[device_id]
                logger.info(f"停止设备: {device_id}")
            
            # 停止无人机模拟器
            if station_ip is None:
                for ip in list(self.uav_drones.keys()):
                    self.stop_uav(ip)
            else:
                self.stop_uav(station_ip)
    
    def start_uav(self, station_ip: str, station_addr: int):
        """启动无人机模拟器"""
        if Drone is None:
            logger.warning("无人机模拟器不可用，跳过")
            return
        
        with self._lock:
            if station_ip in self.uav_drones:
                logger.warning(f"无人机模拟器已存在，跳过")
                return
            
            # 配置无人机参数
            drone_id = 0x0853 + station_addr  # 基于工位地址生成无人机ID
            
            # 根据工位类型决定是否发送入网申请（导引头）
            station_type = STATION_CONFIG.get(station_ip, {}).get("type", "")
            network_apply = station_type in ["拷机", "桁架"]  # 仅拷机和桁架工位有导引头
            
            config = {
                "drone_id": drone_id,
                "telemetry_type": "D1",
                "network_apply": network_apply,
                "multicast_group": "226.0.0.80",
                "remote_port": 8002,
                "telemetry_port": 8001,
                "interval_ms": 1000,  # 发送间隔1秒
            }
            
            # 创建多播核心
            core = MulticastCore()
            drone = Drone(drone_id, config, core, "D1")

            # 创建停止标志（使用列表以便在线程中可修改）
            running_flag = [True]

            # 启动接收线程
            def recv_callback(data, addr):
                drone.handle_remote_frame(data, addr)

            recv_thread = threading.Thread(
                target=core.join_multicast_group,
                kwargs={
                    "group": config["multicast_group"],
                    "port": config["remote_port"],
                    "callback": recv_callback,
                    "timeout": None,
                    "running_flag": running_flag
                },
                daemon=True
            )
            recv_thread.start()

            # 启动发送线程
            send_thread = threading.Thread(target=drone.send_loop, args=(config["interval_ms"],), daemon=True)
            send_thread.start()

            self.uav_drones[station_ip] = drone
            self.uav_threads[station_ip] = (recv_thread, send_thread)
            self.uav_running_flags[station_ip] = running_flag
            
            logger.info(f"启动无人机模拟器: 工位 {station_addr} (IP: {station_ip}), 无人机ID: 0x{drone_id:04X}, 导引头入网申请: {'是' if network_apply else '否'}")
            
            # 如果导引头需要入网，启动定时器模拟入网成功后的权限切换
            if network_apply:
                def guidance_head_networked_callback():
                    time.sleep(5)  # 模拟5秒后入网成功
                    with self._lock:
                        self.guidance_head_networked[station_ip] = True
                    logger.info(f"导引头已成功入网 (工位 {station_addr}, IP: {station_ip})，遥控权限已切换至导引头，地测口遥控指令将被屏蔽")
                
                network_thread = threading.Thread(target=guidance_head_networked_callback, daemon=True)
                network_thread.start()
    
    def stop_uav(self, station_ip: str):
        """停止无人机模拟器"""
        with self._lock:
            if station_ip not in self.uav_drones:
                return

            # 停止无人机发送循环（通过设置内部标志）
            drone = self.uav_drones[station_ip]
            drone.running = False

            # 停止接收线程（通过设置 running_flag）
            if station_ip in self.uav_running_flags:
                self.uav_running_flags[station_ip][0] = False

            # 等待线程结束（非阻塞，因为线程是daemon）
            del self.uav_drones[station_ip]
            if station_ip in self.uav_threads:
                del self.uav_threads[station_ip]
            if station_ip in self.uav_running_flags:
                del self.uav_running_flags[station_ip]

            logger.info(f"停止无人机模拟器: IP: {station_ip}")
    
    def mark_reported(self, device_id: str):
        """标记设备已上报"""
        with self._lock:
            self.reported_devices.add(device_id)
    
    def is_reported(self, device_id: str) -> bool:
        """检查设备是否已上报"""
        with self._lock:
            return device_id in self.reported_devices
    
    def get_device_count(self, station_ip: str = None) -> int:
        """获取设备数量"""
        with self._lock:
            if station_ip is None:
                return len(self.devices)
            return sum(1 for d in self.devices.values() if d.src_ip == station_ip)
    
    def get_uav_count(self, station_ip: str = None) -> int:
        """获取无人机数量"""
        with self._lock:
            if station_ip is None:
                return len(self.uav_drones)
            return 1 if station_ip in self.uav_drones else 0

    def start_special_devices(self):
        """启动特殊设备（扫描枪、噪音传感器）"""
        # 使用fixture_simulator中的create_devices函数获取特殊设备配置
        special_device_configs = []
        
        # 获取扫描枪配置
        scanner_config = {
            'id': 'scanner',
            'port': SPECIAL_DEVICES['scanner']['port'],
            'type': '扫描枪',
            'protocol': 'unknown',
            'send_data': False,
            'sensor_addr': 0,
            'src_ip': SPECIAL_DEVICES['scanner']['ip']
        }
        special_device_configs.append(scanner_config)
        
        # 获取噪音传感器配置
        for noise_sensor in SPECIAL_DEVICES['noise_sensors']:
            noise_config = {
                'id': f'noise_sensor_{noise_sensor["addr"]}',
                'port': noise_sensor['port'],
                'type': '噪音传感器',
                'protocol': 'modbus_rtu',
                'send_data': False,
                'sensor_addr': noise_sensor['addr'],
                'src_ip': noise_sensor['ip']
            }
            special_device_configs.append(noise_config)
        
        started_devices = []
        with self._lock:
            for config in special_device_configs:
                device_id = config['id']
                if device_id in self.devices:
                    logger.warning(f"特殊设备 {device_id} 已存在，跳过")
                    continue
                    
                # 创建设备客户端
                device = DeviceClient(
                    self.dgiot_host,
                    self.dgiot_port,
                    config,
                    no_bind=False
                )
                device.daemon = True
                device.start()
                
                self.devices[device_id] = device
                self.device_states[device_id] = "starting"
                started_devices.append(device)
                
                logger.info(f"启动特殊设备: {device_id} (IP: {config['src_ip']}:{config['port']})")
                time.sleep(0.1)
        
        # 等待设备连接成功
        time.sleep(2)
        logger.info(f"特殊设备启动完成，共 {len(started_devices)} 个设备")
        return started_devices

    def can_dicekou_send_remote(self, station_ip: str) -> bool:
        """检查地测口是否可以发送遥控指令（导引头入网后禁止）"""
        with self._lock:
            # 如果导引头已入网，则地测口不能发送遥控指令
            if self.guidance_head_networked.get(station_ip, False):
                logger.debug(f"地测口遥控指令被屏蔽 (工位 IP: {station_ip})，导引头已入网")
                return False
            return True

# ==================== PLC客户端 ====================
class PLCClient:
    """PLC三通道客户端，支持磁航向、总测、拷机/桁架三个独立通道和7步握手流程"""
    def __init__(self):
        # 初始化三个通道的客户端
        self.channels: Dict[str, Optional[ModbusTcpClient]] = {
            "magnetic": None,
            "total_test": None,
            "bake_and_gantry": None,
        }
        self.channel_ips: Dict[str, str] = {
            "magnetic": PLC_CHANNELS["magnetic"]["ip"],
            "total_test": PLC_CHANNELS["total_test"]["ip"],
            "bake_and_gantry": PLC_CHANNELS["bake_and_gantry"]["ip"],
        }
        self.channel_ports: Dict[str, int] = {
            "magnetic": PLC_SERVER_PORT,
            "total_test": PLC_SERVER_PORT,
            "bake_and_gantry": PLC_SERVER_PORT,
        }
        self.channel_base_addrs: Dict[str, int] = {
            "magnetic": PLC_CHANNELS["magnetic"]["base_addr"],
            "total_test": PLC_CHANNELS["total_test"]["base_addr"],
            "bake_and_gantry": PLC_CHANNELS["bake_and_gantry"]["base_addr"],
        }
        
    def connect_all(self) -> bool:
        """连接到所有PLC通道"""
        success = True
        for channel_name in self.channels.keys():
            if not self.connect_channel(channel_name):
                logger.warning(f"PLC通道 {channel_name} 连接失败")
                success = False
        return success
    
    def connect_channel(self, channel_name: str) -> bool:
        """连接到指定PLC通道"""
        try:
            host = self.channel_ips[channel_name]
            port = self.channel_ports[channel_name]
            client = ModbusTcpClient(host, port=port)
            connected = client.connect()
            if connected:
                self.channels[channel_name] = client
                logger.info(f"连接到PLC通道 {channel_name}: {host}:{port}")
            else:
                logger.error(f"连接PLC通道 {channel_name} 失败: {host}:{port}")
            return connected
        except Exception as e:
            logger.error(f"连接PLC通道 {channel_name} 异常: {e}")
            return False
    
    def disconnect_all(self):
        """断开所有PLC通道连接"""
        for channel_name, client in self.channels.items():
            if client:
                client.close()
                self.channels[channel_name] = None
                logger.info(f"断开PLC通道 {channel_name} 连接")
    
    def get_client_for_channel(self, channel_name: str) -> Optional[ModbusTcpClient]:
        """获取指定通道的客户端"""
        client = self.channels.get(channel_name)
        if client and client.is_socket_open():
            return client
        # 如果客户端未连接，尝试重新连接
        if client is None:
            self.connect_channel(channel_name)
            return self.channels.get(channel_name)
        return None
    
    def send_plc_command_with_handshake(self, station_addr: int, station_type: str, command_code: int) -> bool:
        """
        发送PLC指令并执行完整的7步握手流程
        
        7步握手流程：
        1. 读取D0（状态寄存器）
        2. 写入D51（指令码寄存器）
        3. 读取D10（确认寄存器）
        4. 写入D0=0（清除状态）
        5. 写入D10=0（清除确认）
        6. 写入D60（触发寄存器）
        7. 写入D61=1（执行寄存器）
        
        返回：成功或失败
        """
        # 根据工位类型确定PLC通道
        station_config = STATION_DEVICE_CONFIG.get(station_type, {})
        channel_name = station_config.get("plc_channel")
        if not channel_name:
            logger.info(f"工位 {station_addr} ({station_type}) 无PLC通道，跳过握手")
            return True
        
        # 获取通道客户端
        client = self.get_client_for_channel(channel_name)
        if not client:
            logger.error(f"PLC通道 {channel_name} 未连接")
            return False
        
        # 获取基地址
        base_addr = self.channel_base_addrs[channel_name]
        logger.info(f"[PLC报文] 开始7步握手 | 工位{station_addr}({station_type}) | "
                    f"通道={channel_name} PLC={PLC_CHANNELS[channel_name]['ip']}:502 | "
                    f"基地址={base_addr} | 指令码=0x{command_code:04X}")
        
        try:
            # 步骤1：读取D0状态寄存器
            d0_addr = base_addr + 0
            d0_result = client.read_holding_registers(d0_addr, 1)
            if d0_result.isError():
                logger.error(f"[PLC报文] 步骤1/7 失败 | 工位{station_addr} | 读取D{d0_addr}错误: {d0_result}")
                return False
            d0_value = d0_result.registers[0]
            logger.info(f"[PLC报文] 步骤1/7 | 工位{station_addr}({station_type}) | "
                        f"读取D{d0_addr} 状态寄存器 | 返回值=0x{d0_value:04X} ({d0_value})")
            
            # 步骤2：写入D51指令码寄存器
            d51_addr = base_addr + 51
            write_result = client.write_register(d51_addr, command_code)
            if write_result.isError():
                logger.error(f"[PLC报文] 步骤2/7 失败 | 工位{station_addr} | 写入D{d51_addr}错误: {write_result}")
                return False
            logger.info(f"[PLC报文] 步骤2/7 | 工位{station_addr}({station_type}) | "
                        f"写入D{d51_addr} 指令码寄存器 | 值=0x{command_code:04X} ({command_code})")
            
            # 步骤3：读取D10确认寄存器
            d10_addr = base_addr + 10
            d10_result = client.read_holding_registers(d10_addr, 1)
            if d10_result.isError():
                logger.error(f"[PLC报文] 步骤3/7 失败 | 工位{station_addr} | 读取D{d10_addr}错误: {d10_result}")
                return False
            d10_value = d10_result.registers[0]
            logger.info(f"[PLC报文] 步骤3/7 | 工位{station_addr}({station_type}) | "
                        f"读取D{d10_addr} 确认寄存器 | 返回值=0x{d10_value:04X} ({d10_value})")
            
            # 步骤4：写入D0=0（清除状态）
            write_d0_result = client.write_register(d0_addr, 0)
            if write_d0_result.isError():
                logger.error(f"[PLC报文] 步骤4/7 失败 | 工位{station_addr} | 写入D{d0_addr}=0错误: {write_d0_result}")
                return False
            logger.info(f"[PLC报文] 步骤4/7 | 工位{station_addr}({station_type}) | "
                        f"写入D{d0_addr}=0 清除状态 | OK")
            
            # 步骤5：写入D10=0（清除确认）
            write_d10_result = client.write_register(d10_addr, 0)
            if write_d10_result.isError():
                logger.error(f"[PLC报文] 步骤5/7 失败 | 工位{station_addr} | 写入D{d10_addr}=0错误: {write_d10_result}")
                return False
            logger.info(f"[PLC报文] 步骤5/7 | 工位{station_addr}({station_type}) | "
                        f"写入D{d10_addr}=0 清除确认 | OK")
            
            # 步骤6：写入D60触发寄存器
            d60_addr = base_addr + 60
            write_d60_result = client.write_register(d60_addr, 1)
            if write_d60_result.isError():
                logger.error(f"[PLC报文] 步骤6/7 失败 | 工位{station_addr} | 写入D{d60_addr}=1错误: {write_d60_result}")
                return False
            logger.info(f"[PLC报文] 步骤6/7 | 工位{station_addr}({station_type}) | "
                        f"写入D{d60_addr}=1 触发寄存器 | OK")
            
            # 步骤7：写入D61=1执行寄存器
            d61_addr = base_addr + 61
            write_d61_result = client.write_register(d61_addr, 1)
            if write_d61_result.isError():
                logger.error(f"[PLC报文] 步骤7/7 失败 | 工位{station_addr} | 写入D{d61_addr}=1错误: {write_d61_result}")
                return False
            logger.info(f"[PLC报文] 步骤7/7 | 工位{station_addr}({station_type}) | "
                        f"写入D{d61_addr}=1 执行寄存器 | OK")
            
            logger.info(f"[PLC报文] === 7步握手完成 === | 工位{station_addr}({station_type}) | "
                        f"通道={channel_name} 基地址={base_addr} | 指令码=0x{command_code:04X} | 结果=成功")
            return True
            
        except Exception as e:
            logger.error(f"[PLC报文] 7步握手异常 | 工位{station_addr}({station_type}) | 异常: {e}")
            return False
    
    def send_plc_command(self, station_addr: int, command_code: int) -> bool:
        """
        向后兼容的简单PLC指令发送（使用总测通道）
        """
        logger.warning(f"使用向后兼容的PLC指令发送，推荐使用send_plc_command_with_handshake")
        # 默认为总测通道
        return self.send_plc_command_with_handshake(station_addr, "总测", command_code)
    
    def read_plc_register(self, address: int, channel_name: str = "total_test") -> Optional[int]:
        """
        读取PLC寄存器值（指定通道）
        """
        client = self.get_client_for_channel(channel_name)
        if not client:
            logger.error(f"PLC通道 {channel_name} 未连接")
            return None
        
        try:
            result = client.read_holding_registers(address, 1)
            if result.isError():
                logger.error(f"读取PLC寄存器失败: {result}")
                return None
            return result.registers[0]
        except Exception as e:
            logger.error(f"读取PLC寄存器异常: {e}")
            return None
    
    def get_station_plc_config(self, station_ip: str) -> Dict[str, Any]:
        """根据工位IP获取PLC配置"""
        station_info = STATION_CONFIG.get(station_ip, {})
        station_addr = station_info.get("station_addr", 0)
        station_type = station_info.get("type", "")
        station_config = STATION_DEVICE_CONFIG.get(station_type, {})
        channel_name = station_config.get("plc_channel")
        
        if channel_name:
            base_addr = self.channel_base_addrs[channel_name]
            return {
                "station_addr": station_addr,
                "station_type": station_type,
                "channel_name": channel_name,
                "base_addr": base_addr,
                "channel_ip": self.channel_ips[channel_name],
                "description": PLC_CHANNELS[channel_name]["description"],
            }
        
        return {
            "station_addr": station_addr,
            "station_type": station_type,
            "channel_name": None,
            "base_addr": None,
            "channel_ip": None,
            "description": "无PLC通道配置",
        }

# ==================== 工位管理器 ====================
class StationManager:
    """管理工位状态和迁移"""
    def __init__(self, device_manager: DeviceManager, plc_client: PLCClient):
        self.device_manager = device_manager
        self.plc_client = plc_client
        self.stations: Dict[str, Dict] = {}  # station_ip -> station info
        self.station_states: Dict[str, StationState] = {}  # station_ip -> state
        self.station_sequence: List[str] = []  # 工位上线顺序
        self.current_station_index = 0
        self._lock = threading.RLock()
        
        # 初始化工位配置
        for ip, config in STATION_CONFIG.items():
            self.stations[ip] = {
                "ip": ip,
                "station_addr": config["station_addr"],
                "name": config["name"],
                "devices": [],
                "last_online_time": None,
                "test_completed": False,
                "test_stage": TestStage.NOT_STARTED,
            }
            self.station_states[ip] = StationState.IDLE
    
    def set_station_sequence(self, sequence: List[str]):
        """设置工位上线顺序（IP列表）"""
        with self._lock:
            self.station_sequence = sequence
            self.current_station_index = 0
            logger.info(f"设置工位顺序: {sequence}")
    
    def start_next_station(self) -> bool:
        """启动下一个工位"""
        with self._lock:
            if self.current_station_index >= len(self.station_sequence):
                logger.info("所有工位已完成")
                return False
            
            station_ip = self.station_sequence[self.current_station_index]
            if station_ip not in self.stations:
                logger.error(f"未知工位IP: {station_ip}")
                return False
            
            # 启动工位设备
            station_info = self.stations[station_ip]
            station_addr = station_info["station_addr"]
            
            logger.info(f"启动工位 {station_addr} ({station_info['name']}), IP: {station_ip}")
            
            # 标记工位状态为准备中
            self.station_states[station_ip] = StationState.PREPARING
            
            # 启动设备（包括无人机、治具、舵面传感器）
            devices = self.device_manager.start_devices(station_ip, station_addr)
            station_info["devices"] = devices
            station_info["last_online_time"] = datetime.now()
            station_info["test_start_time"] = datetime.now()
            station_info["test_stage"] = TestStage.DEVICES_ONLINE
            station_info["test_completed"] = False
            self.station_states[station_ip] = StationState.ONLINE
            
            logger.info(f"工位 {station_addr} 设备上线完成 (设备数: {len(devices)})")
            
            # 发送PLC指令（触发测试开始），使用7步握手流程
            plc_success = self._trigger_plc_command(station_ip)
            if plc_success:
                station_info["test_stage"] = TestStage.PLC_COMMAND_SENT
                logger.info(f"工位 {station_addr} PLC指令发送成功，进入测试执行阶段")
            else:
                station_info["test_stage"] = TestStage.NOT_STARTED
                logger.warning(f"工位 {station_addr} PLC指令发送失败，测试未开始")
            
            self.current_station_index += 1
            return True
    
    def complete_current_station(self, station_ip: str):
        """完成当前工位测试，准备迁移到下一个工位"""
        with self._lock:
            if station_ip not in self.stations:
                return
            
            station_info = self.stations[station_ip]
            station_addr = station_info["station_addr"]
            
            # 标记测试完成
            station_info["test_completed"] = True
            station_info["test_stage"] = TestStage.TEST_COMPLETED
            self.station_states[station_ip] = StationState.COMPLETED
            
            logger.info(f"工位 {station_addr} ({station_info['name']}) 测试完成")
            
            # 停止设备（模拟向下迁移）
            self.device_manager.stop_devices(station_ip)
            station_info["test_stage"] = TestStage.DEVICES_OFFLINE
            self.station_states[station_ip] = StationState.OFFLINE
            
            logger.info(f"工位 {station_addr} 设备已下线")
    
    def _trigger_plc_command(self, station_ip: str) -> bool:
        """触发PLC指令，使用7步握手流程，返回是否成功"""
        logger.debug(f"_trigger_plc_command 被调用，station_ip: {station_ip}")
        if station_ip not in self.stations:
            logger.error(f"未知工位IP: {station_ip}")
            return False
        
        station_info = self.stations[station_ip]
        station_addr = station_info["station_addr"]
        station_name = station_info["name"]
        
        # 从STATION_CONFIG获取工位类型
        station_type = STATION_CONFIG.get(station_ip, {}).get("type", "")
        if not station_type:
            logger.error(f"工位 {station_ip} 未配置类型")
            return False
        
        # 根据工位类型确定PLC指令码
        command_codes = {
            "磁航向": 0x0101,
            "总测": 0x1001,
            "动力检测": 0x1002,
            "拷机": 0x2001,
            "桁架": 0x3001,
            "上料台": 0x0001,
        }
        
        command_code = command_codes.get(station_type, 0x1001)
        logger.info(f"触发PLC指令: 工位 {station_addr} ({station_name}, 类型: {station_type}), 指令码: 0x{command_code:04X}")
        
        # 使用7步握手流程发送PLC指令
        success = self.plc_client.send_plc_command_with_handshake(station_addr, station_type, command_code)
        if success:
            logger.info(f"PLC 7步握手成功: 工位 {station_addr} ({station_type})")
        else:
            logger.warning(f"PLC 7步握手失败: 工位 {station_addr} ({station_type})")
        return success
    
    def get_status(self) -> Dict:
        """获取所有工位状态"""
        with self._lock:
            status = {}
            for ip, station_info in self.stations.items():
                status[ip] = {
                    "name": station_info["name"],
                    "station_addr": station_info["station_addr"],
                    "state": self.station_states[ip].value,
                    "device_count": self.device_manager.get_device_count(ip),
                    "uav_count": self.device_manager.get_uav_count(ip),
                    "test_completed": station_info["test_completed"],
                    "last_online_time": station_info["last_online_time"].isoformat() if station_info["last_online_time"] else None,
                }
            return status

# ==================== 测试用例定义 ====================
class TestCase:
    """测试用例定义"""
    def __init__(self, name: str, description: str):
        self.name = name
        self.description = description
        self.station_sequence: List[str] = []
        self.plc_commands: List[Dict] = []
        self.uav_config: Optional[Dict] = None
        self.wait_times: Dict[str, int] = {}  # 阶段等待时间（秒）
    
    def add_station(self, station_ip: str, wait_after_online: int = 10):
        """添加工位到测试序列"""
        self.station_sequence.append(station_ip)
        self.wait_times[f"station_{station_ip}_online"] = wait_after_online
    
    def add_plc_command(self, station_addr: int, command_code: int, delay: int = 0):
        """添加PLC命令"""
        self.plc_commands.append({
            "station_addr": station_addr,
            "command_code": command_code,
            "delay": delay,  # 命令延迟（秒）
        })
    
    def set_uav_config(self, config: Dict):
        """设置无人机模拟器配置"""
        self.uav_config = config

# ==================== 测试用例库 ====================
class TestCaseLibrary:
    """测试用例库"""
    @staticmethod
    def get_test_case(name: str) -> TestCase:
        """获取预定义测试用例"""
        if name == "normal_flow":
            return TestCaseLibrary.normal_flow()
        elif name == "拷机测试":
            return TestCaseLibrary.bake_test()
        elif name == "全工位循环":
            return TestCaseLibrary.full_station_cycle()
        elif name == "异常场景":
            return TestCaseLibrary.error_scenario()
        elif name == "comprehensive":
            return TestCaseLibrary.comprehensive()
        elif name == "uav_fixture_surface_combo":
            return TestCaseLibrary.uav_fixture_surface_combo()
        elif name == "complete_production_line_a1":
            return TestCaseLibrary.complete_production_line_a1()
        elif name == "complete_production_line_a2":
            return TestCaseLibrary.complete_production_line_a2()
        elif name == "complete_production_line_b1":
            return TestCaseLibrary.complete_production_line_b1()
        elif name == "complete_production_line_b2":
            return TestCaseLibrary.complete_production_line_b2()
        elif name == "all_production_lines":
            return TestCaseLibrary.all_production_lines()
        elif name == "magnetic_station_only":
            return TestCaseLibrary.magnetic_station_only()
        elif name == "power_test_with_noise":
            return TestCaseLibrary.power_test_with_noise()
        elif name == "bake_with_guidance_head":
            return TestCaseLibrary.bake_with_guidance_head()
        else:
            return TestCaseLibrary.normal_flow()
    
    @staticmethod
    def normal_flow() -> TestCase:
        """正常流程测试用例（简化版：两个治具IP .45和.47）"""
        tc = TestCase("normal_flow", "简化测试流程：上料台 -> 磁航向 -> 总测工位2 -> 总测工位1")
        
        # 工位顺序（使用两个治具IP：.45和.47）
        tc.add_station("192.168.100.52", wait_after_online=5)  # 上料台
        tc.add_station("192.168.100.21", wait_after_online=10) # 磁航向工位（扫码绑定）
        tc.add_station("192.168.100.45", wait_after_online=15) # 总测工位2（治具IP .45）
        tc.add_station("192.168.100.47", wait_after_online=15) # 总测工位1（治具IP .47）
        
        # PLC指令
        tc.add_plc_command(255, 0x0001, delay=2)   # 上料台指令
        tc.add_plc_command(0, 0x0101, delay=2)     # 磁航向指令
        tc.add_plc_command(1, 0x1001, delay=2)     # 总测开始（工位地址1）
        tc.add_plc_command(3, 0x1001, delay=2)     # 总测开始（工位地址3）
        
        return tc
    
    @staticmethod
    def bake_test() -> TestCase:
        """拷机测试用例"""
        tc = TestCase("拷机测试", "拷机工位长时间测试")
        
        tc.add_station("192.168.100.49", wait_after_online=60)  # 拷机工位2
        tc.add_station("192.168.100.50", wait_after_online=60)  # 拷机工位1
        
        tc.add_plc_command(5, 0x2001, delay=2)
        tc.add_plc_command(6, 0x2001, delay=2)
        
        return tc
    
    @staticmethod
    def full_station_cycle() -> TestCase:
        """全工位循环测试"""
        tc = TestCase("全工位循环", "所有工位顺序测试")
        
        # 所有工位顺序
        all_stations = [
            "192.168.100.45", "192.168.100.46", "192.168.100.47",
            "192.168.100.48", "192.168.100.49", "192.168.100.50",
            "192.168.100.51", "192.168.100.52"
        ]
        
        for station_ip in all_stations:
            tc.add_station(station_ip, wait_after_online=10)
        
        return tc
    
    @staticmethod
    def error_scenario() -> TestCase:
        """异常场景测试（设备离线、PLC通信失败等）"""
        tc = TestCase("异常场景", "模拟异常情况")
        
        tc.add_station("192.168.100.45", wait_after_online=5)
        # 模拟设备快速下线
        tc.wait_times["station_192.168.100.45_offline"] = 3
        
        return tc
    
    @staticmethod
    def comprehensive() -> TestCase:
        """综合测试用例：覆盖所有工位和场景，长时间循环验证"""
        tc = TestCase("comprehensive", "综合测试：所有工位顺序测试，包含拷机、总测、异常场景组合")
        
        # 所有工位顺序，但分组执行
        # 第一阶段：上料台和总测
        tc.add_station("192.168.100.52", wait_after_online=10)  # 上料台
        tc.add_station("192.168.100.45", wait_after_online=20)  # 总测工位2
        tc.add_station("192.168.100.47", wait_after_online=20)  # 总测工位1
        # 第二阶段：拷机工位（长时间）
        tc.add_station("192.168.100.49", wait_after_online=40)  # 拷机工位2
        tc.add_station("192.168.100.50", wait_after_online=40)  # 拷机工位1
        # 第三阶段：其他工位
        tc.add_station("192.168.100.46", wait_after_online=15)  # 总测工位2-动力检测
        tc.add_station("192.168.100.48", wait_after_online=15)  # 总测工位1-动力检测
        tc.add_station("192.168.100.51", wait_after_online=25)  # 桁行架
        
        # PLC指令
        tc.add_plc_command(255, 0x0001, delay=2)   # 上料台指令
        tc.add_plc_command(1, 0x1001, delay=2)     # 总测开始
        tc.add_plc_command(3, 0x1001, delay=2)     # 总测开始
        tc.add_plc_command(5, 0x2001, delay=2)     # 拷机开始
        tc.add_plc_command(6, 0x2001, delay=2)     # 拷机开始
        tc.add_plc_command(2, 0x1002, delay=2)     # 动力检测
        tc.add_plc_command(4, 0x1002, delay=2)     # 动力检测
        tc.add_plc_command(7, 0x3001, delay=2)     # 桁行架操作
        
        return tc

    @staticmethod
    def uav_fixture_surface_combo() -> TestCase:
        """无人机+治具+舵面组合 - 遍历所有工位"""
        tc = TestCase("uav_fixture_surface_combo", "无人机+治具+舵面设备组合，顺序遍历所有测试工位")
        
        # 遍历所有工位IP (192.168.100.45 到 .52)
        all_station_ips = [
            "192.168.100.45", "192.168.100.46", "192.168.100.47",
            "192.168.100.48", "192.168.100.49", "192.168.100.50",
            "192.168.100.51", "192.168.100.52"
        ]
        
        for station_ip in all_station_ips:
            # 根据不同工位类型设置不同的测试等待时间
            config = STATION_CONFIG[station_ip]
            station_addr = config["station_addr"]
            if station_addr == 255:  # 上料台
                wait_time = 8
                plc_cmd = 0x0001
            elif station_addr in [5, 6]:  # 拷机工位
                wait_time = 40  # 拷机时间较长
                plc_cmd = 0x2001
            elif station_addr == 7:  # 桁行架
                wait_time = 25
                plc_cmd = 0x3001
            else:  # 总测工位及其动力检测
                wait_time = 20
                plc_cmd = 0x1001 if station_addr in [1,3] else 0x1002
            
            tc.add_station(station_ip, wait_after_online=wait_time)
            tc.add_plc_command(station_addr, plc_cmd, delay=2)
        
        return tc

    @staticmethod
    def complete_production_line_a1() -> TestCase:
        """完整产线A-路径1测试用例"""
        tc = TestCase("complete_production_line_a1", "完整产线流程A-路径1：磁航向->总测2->动力检测2->拷机2->桁架")
        line_config = PRODUCTION_LINE_MATRIX["line_a_path1"]
        
        for step in line_config["sequence"]:
            # 根据工位类型设置不同的等待时间
            stage_type = step["stage"]
            if stage_type == "磁航向":
                wait_time = 15  # 扫码绑定需要时间
            elif stage_type == "总测":
                wait_time = 20
            elif stage_type == "动力检测":
                wait_time = 20  # 包含噪音传感器
            elif stage_type == "拷机":
                wait_time = 40  # 拷机时间较长
            elif stage_type == "桁架":
                wait_time = 25
            else:
                wait_time = 15
            
            # 获取工位地址
            station_ip = step["ip"]
            station_addr = STATION_CONFIG[station_ip]["station_addr"]
            
            tc.add_station(station_ip, wait_after_online=wait_time)
            
            # 添加PLC指令
            plc_cmd = TestCaseLibrary._get_plc_command_for_station(station_addr, stage_type)
            tc.add_plc_command(station_addr, plc_cmd, delay=2)
        
        return tc

    @staticmethod
    def complete_production_line_a2() -> TestCase:
        """完整产线A-路径2测试用例"""
        tc = TestCase("complete_production_line_a2", "完整产线流程A-路径2：磁航向->总测1->动力检测1->拷机1->桁架")
        line_config = PRODUCTION_LINE_MATRIX["line_a_path2"]
        
        for step in line_config["sequence"]:
            stage_type = step["stage"]
            if stage_type == "磁航向":
                wait_time = 15
            elif stage_type == "总测":
                wait_time = 20
            elif stage_type == "动力检测":
                wait_time = 20
            elif stage_type == "拷机":
                wait_time = 40
            elif stage_type == "桁架":
                wait_time = 25
            else:
                wait_time = 15
            
            station_ip = step["ip"]
            station_addr = STATION_CONFIG[station_ip]["station_addr"]
            
            tc.add_station(station_ip, wait_after_online=wait_time)
            
            plc_cmd = TestCaseLibrary._get_plc_command_for_station(station_addr, stage_type)
            tc.add_plc_command(station_addr, plc_cmd, delay=2)
        
        return tc

    @staticmethod
    def complete_production_line_b1() -> TestCase:
        """完整产线B-路径1测试用例"""
        tc = TestCase("complete_production_line_b1", "完整产线流程B-路径1：磁航向->总测2->动力检测2->拷机2->桁架")
        line_config = PRODUCTION_LINE_MATRIX["line_b_path1"]
        
        for step in line_config["sequence"]:
            stage_type = step["stage"]
            if stage_type == "磁航向":
                wait_time = 15
            elif stage_type == "总测":
                wait_time = 20
            elif stage_type == "动力检测":
                wait_time = 20
            elif stage_type == "拷机":
                wait_time = 40
            elif stage_type == "桁架":
                wait_time = 25
            else:
                wait_time = 15
            
            station_ip = step["ip"]
            station_addr = STATION_CONFIG[station_ip]["station_addr"]
            
            tc.add_station(station_ip, wait_after_online=wait_time)
            
            plc_cmd = TestCaseLibrary._get_plc_command_for_station(station_addr, stage_type)
            tc.add_plc_command(station_addr, plc_cmd, delay=2)
        
        return tc

    @staticmethod
    def complete_production_line_b2() -> TestCase:
        """完整产线B-路径2测试用例"""
        tc = TestCase("complete_production_line_b2", "完整产线流程B-路径2：磁航向->总测1->动力检测1->拷机1->桁架")
        line_config = PRODUCTION_LINE_MATRIX["line_b_path2"]
        
        for step in line_config["sequence"]:
            stage_type = step["stage"]
            if stage_type == "磁航向":
                wait_time = 15
            elif stage_type == "总测":
                wait_time = 20
            elif stage_type == "动力检测":
                wait_time = 20
            elif stage_type == "拷机":
                wait_time = 40
            elif stage_type == "桁架":
                wait_time = 25
            else:
                wait_time = 15
            
            station_ip = step["ip"]
            station_addr = STATION_CONFIG[station_ip]["station_addr"]
            
            tc.add_station(station_ip, wait_after_online=wait_time)
            
            plc_cmd = TestCaseLibrary._get_plc_command_for_station(station_addr, stage_type)
            tc.add_plc_command(station_addr, plc_cmd, delay=2)
        
        return tc

    @staticmethod
    def all_production_lines() -> TestCase:
        """所有产线组合测试（依次执行4条完整路径）"""
        tc = TestCase("all_production_lines", "所有产线组合：依次执行4条完整路径，覆盖所有工位组合")
        
        # 依次执行四条路径
        line_keys = ["line_a_path1", "line_a_path2", "line_b_path1", "line_b_path2"]
        
        for line_key in line_keys:
            line_config = PRODUCTION_LINE_MATRIX[line_key]
            tc.add_station("192.168.100.52", wait_after_online=5)  # 每条路径开始前先上料
            
            for step in line_config["sequence"]:
                stage_type = step["stage"]
                if stage_type == "磁航向":
                    wait_time = 15
                elif stage_type == "总测":
                    wait_time = 20
                elif stage_type == "动力检测":
                    wait_time = 20
                elif stage_type == "拷机":
                    wait_time = 40
                elif stage_type == "桁架":
                    wait_time = 25
                else:
                    wait_time = 15
                
                station_ip = step["ip"]
                station_addr = STATION_CONFIG[station_ip]["station_addr"]
                
                tc.add_station(station_ip, wait_after_online=wait_time)
                
                plc_cmd = TestCaseLibrary._get_plc_command_for_station(station_addr, stage_type)
                tc.add_plc_command(station_addr, plc_cmd, delay=2)
            
            # 路径间间隔
            tc.wait_times[f"path_{line_key}_interval"] = 5
        
        return tc

    @staticmethod
    def magnetic_station_only() -> TestCase:
        """仅磁航向工位测试（扫码绑定）"""
        tc = TestCase("magnetic_station_only", "仅测试磁航向工位：地测口+扫描枪联动，扫码绑定业务")
        
        # 磁航向工位
        station_ip = "192.168.100.21"
        station_addr = STATION_CONFIG[station_ip]["station_addr"]
        tc.add_station(station_ip, wait_after_online=15)
        
        # PLC指令
        plc_cmd = TestCaseLibrary._get_plc_command_for_station(station_addr, "磁航向")
        tc.add_plc_command(station_addr, plc_cmd, delay=2)
        
        return tc

    @staticmethod
    def power_test_with_noise() -> TestCase:
        """动力检测工位带噪音传感器测试"""
        tc = TestCase("power_test_with_noise", "动力检测工位测试：包含噪音传感器（.35/.36:21000）")
        
        # 选择两个动力检测工位
        station_ips = ["192.168.100.46", "192.168.100.48"]
        
        for station_ip in station_ips:
            station_addr = STATION_CONFIG[station_ip]["station_addr"]
            tc.add_station(station_ip, wait_after_online=25)  # 动力检测需要更长时间
            
            plc_cmd = TestCaseLibrary._get_plc_command_for_station(station_addr, "动力检测")
            tc.add_plc_command(station_addr, plc_cmd, delay=2)
        
        return tc

    @staticmethod
    def bake_with_guidance_head() -> TestCase:
        """拷机工位带导引头测试（遥控权限互斥逻辑）"""
        tc = TestCase("bake_with_guidance_head", "拷机工位测试：包含导引头，模拟遥控权限互斥逻辑")
        
        # 两个拷机工位
        station_ips = ["192.168.100.49", "192.168.100.50"]
        
        for station_ip in station_ips:
            station_addr = STATION_CONFIG[station_ip]["station_addr"]
            tc.add_station(station_ip, wait_after_online=45)  # 拷机+导引头测试需要更长时间
            
            plc_cmd = TestCaseLibrary._get_plc_command_for_station(station_addr, "拷机")
            tc.add_plc_command(station_addr, plc_cmd, delay=2)
        
        return tc

    @staticmethod
    def _get_plc_command_for_station(station_addr: int, stage_type: str) -> int:
        """根据工位地址和工位类型获取PLC命令码"""
        # 简化映射，实际应根据PLC通道配置
        command_mapping = {
            "磁航向": 0x0101,
            "总测": 0x1001,
            "动力检测": 0x1002,
            "拷机": 0x2001,
            "桁架": 0x3001,
            "上料台": 0x0001,
        }
        
        # 默认命令码
        default_codes = {
            1: 0x1001,   # 总测工位2
            2: 0x1002,   # 总测工位2-动力检测
            3: 0x1001,   # 总测工位1
            4: 0x1002,   # 总测工位1-动力检测
            5: 0x2001,   # 拷机工位2
            6: 0x2001,   # 拷机工位1
            7: 0x3001,   # 桁行架
            255: 0x0001, # 上料台
        }
        
        # 优先使用工位类型映射
        if stage_type in command_mapping:
            return command_mapping[stage_type]
        
        # 回退到工位地址映射
        return default_codes.get(station_addr, 0x1001)

# ==================== 产线调度器 ====================
class ProductionLineScheduler:
    """产线调度器主类"""
    def __init__(self, dgiot_host: str, dgiot_port: int, plc_host: str = "0.0.0.0", plc_port: int = 502, enable_mes: bool = False):
        self.dgiot_host = dgiot_host
        self.dgiot_port = dgiot_port
        self.plc_host = plc_host  # 保留参数用于向后兼容
        self.plc_port = plc_port  # 保留参数用于向后兼容
        self.enable_mes = enable_mes  # 是否启用MES模拟
        
        # 初始化组件
        self.device_manager = DeviceManager(dgiot_host, dgiot_port)
        self.plc_client = PLCClient()  # 新的PLC客户端支持三通道和7步握手
        self.station_manager = StationManager(self.device_manager, self.plc_client)
        
        # PLC模拟器子进程
        self.plc_process = None
        
        # MES模拟器子进程
        self.mes_process = None
        
        # UAV模拟器（可选）
        self.uav_simulator = None
        if UAVSimulator:
            # 可以在这里初始化，但默认不启动
            pass
        
        self.running = False
        self.current_test_case: Optional[TestCase] = None
        self.cycle_count = 0
        self.max_cycles = 1

        # 闭环测试验证
        self.test_results = {
            'test_id': '',
            'start_time': None,
            'end_time': None,
            'steps': [],
            'errors': [],
            'warnings': []
        }
        
    def start(self):
        """启动调度器"""
        logger.info("启动产线调度器")

        # 初始化闭环测试结果
        self.test_results = {
            'test_id': f'test_{datetime.now().strftime("%Y%m%d_%H%M%S")}',
            'start_time': datetime.now().isoformat(),
            'end_time': None,
            'steps': [],
            'errors': [],
            'warnings': []
        }

        # 清理可能冲突的进程（独立的fixture_simulator或plc_simulator）
        my_pid = os.getpid()
        for proc_name in ["fixture_simulator.py", "plc_simulator.py"]:
            subprocess.run(["pkill", "-9", "-f", proc_name],
                          stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        # 清理其他集成测试进程（排除当前进程）
        try:
            result = subprocess.run(["pgrep", "-f", "integrated_production_line.py"],
                                   capture_output=True, text=True)
            for pid_str in result.stdout.strip().split('\n'):
                if pid_str and int(pid_str) != my_pid:
                    try:
                        os.kill(int(pid_str), signal.SIGKILL)
                    except:
                        pass
            time.sleep(1)
        except:
            pass
        
        # 修复dgiot_host: 0.0.0.0不能作为TCP连接目标，需替换为127.0.0.1
        if self.dgiot_host in ("0.0.0.0", ""):
            self.dgiot_host = "127.0.0.1"
            logger.info(f"DGIOT连接地址修正为: {self.dgiot_host}:{self.dgiot_port}")
        self.device_manager.dgiot_host = self.dgiot_host
        
        # 确保IP绑定（包含192.168.100.100，因为某些场景需要工控机IP）
        if not ensure_ips(auto_bind=True):
            logger.error("IP绑定失败，请检查网络配置")
            return False
        
        # 确保192.168.100.100已绑定（工控机IP，部分设备可能需要用此IP通信）
        try:
            subprocess.check_call(["ip", "addr", "add", "192.168.100.100/24", "dev", "eth0"],
                                  stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            logger.info("已绑定工控机IP 192.168.100.100")
        except subprocess.CalledProcessError:
            pass  # 已存在则忽略
        
        # 启动PLC模拟器子进程
        plc_script_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "plc_simulator.py")
        if os.path.exists(plc_script_path):
            logger.info(f"启动PLC模拟器: {plc_script_path}")
            try:
                self.plc_process = subprocess.Popen(
                    [sys.executable, plc_script_path],
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL
                )
                # 等待PLC模拟器启动（检测端口就绪）
                import socket
                for _ in range(10):
                    time.sleep(0.5)
                    try:
                        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                        s.settimeout(1)
                        s.connect(('127.0.0.1', PLC_SERVER_PORT))
                        s.close()
                        break
                    except:
                        pass
                logger.info(f"PLC模拟器已启动，PID: {self.plc_process.pid}")
            except Exception as e:
                logger.error(f"启动PLC模拟器失败: {e}")
                self.plc_process = None
        else:
            logger.warning(f"PLC模拟器脚本不存在: {plc_script_path}")
        
        # 连接PLC（三通道）
        if not self.plc_client.connect_all():
            logger.warning("PLC三通道连接失败，继续运行（可能PLC模拟器未启动）")
        
        # 启动特殊设备（扫描枪、噪音传感器）
        logger.info("启动特殊设备...")
        self.device_manager.start_special_devices()
        
        self.running = True
        return True
    
    def stop(self):
        """停止调度器"""
        logger.info("停止产线调度器")
        self.running = False
        
        # 停止所有设备
        self.device_manager.stop_devices()
        
        # 断开PLC连接（三通道）
        self.plc_client.disconnect_all()
        
        # 停止PLC模拟器子进程
        if self.plc_process:
            logger.info(f"停止PLC模拟器，PID: {self.plc_process.pid}")
            self.plc_process.terminate()
            try:
                self.plc_process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                logger.warning("PLC模拟器未正常退出，强制终止")
                self.plc_process.kill()
            self.plc_process = None
        
        # 停止UAV模拟器
        if self.uav_simulator:
            self.uav_simulator.stop()
    
    def run_test_case(self, test_case: TestCase):
        """执行测试用例"""
        logger.info(f"开始执行测试用例: {test_case.name}")
        logger.info(f"描述: {test_case.description}")
        
        self.current_test_case = test_case
        self.station_manager.set_station_sequence(test_case.station_sequence)
        
        # 数据汇聚统计
        stats = {
            "test_case": test_case.name,
            "start_time": datetime.now().isoformat(),
            "stations_total": len(test_case.station_sequence),
            "stations_completed": 0,
            "devices_online": 0,
            "devices_offline": 0,
            "plc_handshakes": 0,
            "plc_failures": 0,
            "uav_frames": 0,
            "scanner_qrcodes": 0,
        }
        
        # 执行工位序列
        for station_ip in test_case.station_sequence:
            if not self.running:
                break
            
            # 启动工位
            if not self.station_manager.start_next_station():
                break
            
            stats["stations_completed"] += 1
            
            # 等待工位上线完成
            wait_key = f"station_{station_ip}_online"
            wait_time = test_case.wait_times.get(wait_key, 10)
            logger.info(f"等待 {wait_time} 秒，模拟测试执行... (工位 {station_ip})")
            time.sleep(wait_time)
            
            # 完成当前工位
            self.station_manager.complete_current_station(station_ip)
            stats["devices_offline"] += 1
            
            # 工位间间隔
            time.sleep(2)
        
        stats["end_time"] = datetime.now().isoformat()
        
        # 汇聚设备统计
        status = self.get_status()
        stats["devices_online"] = status.get("total_devices", 0)
        stats["reported_devices"] = status.get("reported_devices", 0)
        stats["uav_count"] = status.get("total_uavs", 0)
        
        # 输出数据汇聚摘要（结构化JSON行格式，方便日志解析判定）
        logger.info(f"[TEST_RESULT] {json.dumps(stats, ensure_ascii=False)}")
        logger.info(f"测试用例 {test_case.name} 执行完成 | "
                    f"工位={stats['stations_completed']}/{stats['stations_total']} | "
                    f"设备={stats['devices_online']} | "
                    f"PLC={stats['plc_handshakes']}")
    
    def run_cycles(self, test_case_name: str, cycles: int):
        """循环执行测试用例"""
        logger.info(f"开始循环执行测试用例: {test_case_name}, 循环次数: {cycles}")
        
        self.max_cycles = cycles
        for cycle in range(1, cycles + 1):
            if not self.running:
                break
            
            logger.info(f"=== 第 {cycle}/{cycles} 轮循环 ===")
            self.cycle_count = cycle
            
            # 获取测试用例
            test_case = TestCaseLibrary.get_test_case(test_case_name)
            
            # 执行测试用例
            self.run_test_case(test_case)
            
            # 循环间隔
            if cycle < cycles:
                logger.info(f"等待 5 秒后开始下一轮循环...")
                time.sleep(5)
        
        logger.info(f"所有循环执行完成")
    
    def get_status(self) -> Dict:
        """获取调度器状态"""
        return {
            "running": self.running,
            "cycle_count": self.cycle_count,
            "max_cycles": self.max_cycles,
            "current_test_case": self.current_test_case.name if self.current_test_case else None,
            "stations": self.station_manager.get_status(),
            "total_devices": self.device_manager.get_device_count(),
            "total_uavs": self.device_manager.get_uav_count(),
            "reported_devices": len(self.device_manager.reported_devices),
        }

    def verify_test_results(self, test_name: str) -> Dict:
        """验证测试结果（闭环验证）"""
        logger.info(f"[CLOSED_LOOP] 开始验证测试结果: {test_name}")

        # 执行Erlang后端验证命令
        verification_commands = [
            {
                'name': '检查工位状态',
                'command': f'_build/emqx/rel/emqx/bin/emqx eval \'dgiot_uav_station_manager:check_station_status(1100).\'',
                'expected_keys': ['status', 'fixture_addr']
            },
            {
                'name': '检查设备监控器',
                'command': '_build/emqx/rel/emqx/bin/emqx eval \'dgiot_uav_device_monitor:test().\'',
                'expected_keys': []
            }
        ]

        results = {
            'test_name': test_name,
            'verification_time': datetime.now().isoformat(),
            'verifications': [],
            'total': len(verification_commands),
            'passed': 0,
            'failed': 0
        }

        for verify_item in verification_commands:
            logger.info(f"[CLOSED_LOOP] 执行验证: {verify_item['name']}")

            try:
                result = subprocess.run(
                    verify_item['command'],
                    shell=True,
                    capture_output=True,
                    text=True,
                    timeout=30
                )

                verification = {
                    'name': verify_item['name'],
                    'success': result.returncode == 0,
                    'returncode': result.returncode,
                    'output': result.stdout[:500] if result.stdout else result.stderr[:500],
                    'has_expected_keys': False
                }

                # 检查期望的键是否存在
                output = result.stdout + result.stderr
                for key in verify_item['expected_keys']:
                    if key in output:
                        verification['has_expected_keys'] = True
                        break

                if verification['success']:
                    results['passed'] += 1
                    logger.info(f"[CLOSED_LOOP] ✓ {verify_item['name']} 验证通过")
                else:
                    results['failed'] += 1
                    logger.warning(f"[CLOSED_LOOP] ✗ {verify_item['name']} 验证失败")

                results['verifications'].append(verification)

            except subprocess.TimeoutExpired:
                logger.error(f"[CLOSED_LOOP] ✗ {verify_item['name']} 验证超时")
                results['failed'] += 1
                results['verifications'].append({
                    'name': verify_item['name'],
                    'success': False,
                    'error': 'timeout'
                })
            except Exception as e:
                logger.error(f"[CLOSED_LOOP] ✗ {verify_item['name']} 验证异常: {e}")
                results['failed'] += 1
                results['verifications'].append({
                    'name': verify_item['name'],
                    'success': False,
                    'error': str(e)
                })

        # 输出验证结果
        logger.info(f"[CLOSED_LOOP] {json.dumps(results, ensure_ascii=False)}")
        logger.info(f"[CLOSED_LOOP] 验证完成: 通过 {results['passed']}/{results['total']}, 失败 {results['failed']}")

        return results

    def save_test_report(self, filename: str = None):
        """保存测试报告"""
        if filename is None:
            timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
            filename = f'test_report_{timestamp}.json'

        script_dir = os.path.dirname(os.path.abspath(__file__))
        filepath = os.path.join(script_dir, filename)

        report = {
            'test_id': self.test_results['test_id'],
            'start_time': self.test_results['start_time'],
            'end_time': datetime.now().isoformat(),
            'cycle_count': self.cycle_count,
            'steps': self.test_results['steps'],
            'errors': self.test_results['errors'],
            'warnings': self.test_results['warnings']
        }

        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(report, f, ensure_ascii=False, indent=2, default=str)

        logger.info(f"测试报告已保存: {filepath}")
        return filepath

# ==================== 主函数 ====================
def main():
    parser = argparse.ArgumentParser(description="无人机测试产线集成调度器")
    parser.add_argument("--test-case", default="normal_flow",
                       choices=["normal_flow", "拷机测试", "全工位循环", "异常场景", "comprehensive",
                                "uav_fixture_surface_combo", "complete_production_line_a1",
                                "complete_production_line_a2", "complete_production_line_b1",
                                "complete_production_line_b2", "all_production_lines",
                                "magnetic_station_only", "power_test_with_noise",
                                "bake_with_guidance_head"],
                       help="测试用例名称")
    parser.add_argument("--cycles", type=int, default=1,
                       help="循环执行次数")
    parser.add_argument("--dgiot-host", default="0.0.0.0",
                       help="DGIOT服务器地址")
    parser.add_argument("--dgiot-port", type=int, default=20000,
                       help="DGIOT服务器端口")
    parser.add_argument("--plc-host", default="0.0.0.0",
                       help="PLC服务器地址")
    parser.add_argument("--plc-port", type=int, default=502,
                       help="PLC服务器端口")
    parser.add_argument("--no-ip-bind", action="store_true",
                       help="不自动绑定IP")
    parser.add_argument("--status-interval", type=int, default=10,
                       help="状态报告间隔（秒）")
    parser.add_argument("--verify", action='store_true',
                       help="测试完成后执行闭环验证")
    parser.add_argument("--save-report", action='store_true',
                       help="保存测试报告")
    parser.add_argument("--enable-mes", action='store_true',
                       help="启用MES模拟器")

    args = parser.parse_args()

    # 创建调度器
    scheduler = ProductionLineScheduler(
        dgiot_host=args.dgiot_host,
        dgiot_port=args.dgiot_port,
        plc_host=args.plc_host,
        plc_port=args.plc_port,
        enable_mes=args.enable_mes
    )

    # 启动调度器
    if not scheduler.start():
        logger.error("调度器启动失败")
        return

    try:
        # 状态报告线程
        def status_report():
            while scheduler.running:
                time.sleep(args.status_interval)
                status = scheduler.get_status()
                logger.info("调度器状态:")
                logger.info(f"  运行状态: {'运行中' if status['running'] else '已停止'}")
                logger.info(f"  当前循环: {status['cycle_count']}/{status['max_cycles']}")
                logger.info(f"  测试用例: {status['current_test_case']}")
                logger.info(f"  设备总数: {status['total_devices']}")
                logger.info(f"  已上报设备: {status['reported_devices']}")

        report_thread = threading.Thread(target=status_report, daemon=True)
        report_thread.start()

        # 运行测试用例循环
        scheduler.run_cycles(args.test_case, args.cycles)

        # 闭环验证
        if args.verify:
            logger.info("=" * 80)
            logger.info("执行闭环验证")
            logger.info("=" * 80)
            verification_results = scheduler.verify_test_results(args.test_case)

            # 保存验证结果
            scheduler.test_results['verification'] = verification_results

        # 保存测试报告
        if args.save_report:
            scheduler.save_test_report()

        # 等待用户中断
        logger.info("测试完成，按 Ctrl+C 退出")
        while scheduler.running:
            time.sleep(1)
            
    except KeyboardInterrupt:
        logger.info("用户中断")
    finally:
        scheduler.stop()

if __name__ == "__main__":
    main()