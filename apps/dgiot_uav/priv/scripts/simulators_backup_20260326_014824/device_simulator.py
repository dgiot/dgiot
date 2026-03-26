#!/usr/bin/env python3
"""
无人机测试设备模拟器 - 完整版
实现设备注册、心跳、数据上报、控制响应等完整功能
支持多设备并发模拟和自动化测试场景
"""

import socket
import time
import threading
import logging
import struct
import json
from dataclasses import dataclass
from typing import Dict, List, Optional, Callable
from enum import Enum
import random
import hashlib

# ============================================================================
# 配置常量
# ============================================================================

# 服务器配置
SERVER_HOST = "0.0.0.0"
SERVER_PORT = 20000
MES_API_URL = "http://192.168.100.100:18083/api"

# 设备类型端口映射（固定端口作为设备类型标识符）
DEVICE_PORTS = {
    "rudder_sensor_1": 10001,      # 舵面传感器1
    "rudder_sensor_2": 10002,      # 舵面传感器2
    "rudder_sensor_3": 10003,      # 舵面传感器3
    "rudder_sensor_4": 10004,      # 舵面传感器4
    "rudder_sensor_5": 10005,      # 舵面传感器5
    "microcontroller": 10006,       # 单片机（治具）
    "ground_test_port": 10007,      # 地测口（无人机）
    "scanner": 1234,                # 扫码枪
    "noise_sensor": 21000,         # 噪音传感器
}

# 设备类型枚举
class DeviceType(Enum):
    RUDDER_SENSOR = "rudder_sensor"
    MICROCONTROLLER = "microcontroller"
    GROUND_TEST = "ground_test"
    SCANNER = "scanner"
    NOISE_SENSOR = "noise_sensor"

# 设备状态枚举
class DeviceStatus(Enum):
    OFFLINE = "offline"
    ONLINE = "online"
    BINDING = "binding"
    BOUND = "bound"
    TESTING = "testing"

# 协议常量
PROTO_MAGIC = 0xEB90
PROTO_VERSION = 0x01

# 报文类型
MSG_TYPE_REGISTER = 0x01       # 注册报文
MSG_TYPE_HEARTBEAT = 0x02      # 心跳报文
MSG_TYPE_DATA_REPORT = 0x03    # 数据上报
MSG_TYPE_CONTROL_CMD = 0x04    # 控制指令
MSG_TYPE_CONTROL_RESP = 0x05   # 控制响应
MSG_TYPE_STATUS_SYNC = 0x06     # 状态同步

# 控制指令类型
CTRL_CMD_START_TEST = 0x01      # 开始测试
CTRL_CMD_STOP_TEST = 0x02       # 停止测试
CTRL_CMD_RESET = 0x03           # 复位
CTRL_CMD_CALIBRATE = 0x04       # 校准
CTRL_CMD_QUERY_STATUS = 0x05     # 查询状态

# 测试步骤状态
TEST_STEP_PENDING = 0x00        # 待执行
TEST_STEP_RUNNING = 0x01        # 执行中
TEST_STEP_PASSED = 0x02        # 通过
TEST_STEP_FAILED = 0x03        # 失败
TEST_STEP_SKIPPED = 0x04        # 跳过

# 日志配置
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

# ============================================================================
# 数据结构
# ============================================================================

@dataclass
class DeviceInfo:
    """设备信息"""
    device_id: str
    device_name: str
    device_type: DeviceType
    ip_address: str
    port: int
    station_id: Optional[int] = None
    status: DeviceStatus = DeviceStatus.OFFLINE
    last_heartbeat: float = 0
    test_sequence: int = 0

@dataclass
class TelemetryData:
    """遥测数据"""
    device_id: str
    timestamp: int
    data_type: str
    data_value: dict
    data_hash: str = ""

# ============================================================================
# 报文编解码
# ============================================================================

class ProtocolCodec:
    """协议编解码器"""
    
    @staticmethod
    def calculate_checksum(data: bytes) -> int:
        """计算CRC16校验和"""
        crc = 0xFFFF
        for byte in data:
            crc ^= byte
            for _ in range(8):
                if crc & 0x0001:
                    crc >>= 1
                    crc ^= 0xA001
                else:
                    crc >>= 1
        return crc & 0xFFFF
    
    @staticmethod
    def encode_register_msg(device_info: DeviceInfo) -> bytes:
        """编码注册报文"""
        payload = json.dumps({
            "device_id": device_info.device_id,
            "device_name": device_info.device_name,
            "device_type": device_info.device_type.value,
            "ip_address": device_info.ip_address,
            "port": device_info.port
        }).encode('utf-8')
        
        # 报文头 (2字节魔数 + 2字节版本 + 2字节类型 + 2字节长度)
        header = struct.pack(
            '>HHHH',
            PROTO_MAGIC,
            PROTO_VERSION,
            MSG_TYPE_REGISTER,
            len(payload)
        )
        
        # 计算校验
        checksum = ProtocolCodec.calculate_checksum(header + payload)
        
        # 完整报文
        msg = header + payload + struct.pack('>H', checksum)
        return msg
    
    @staticmethod
    def decode_message(data: bytes) -> dict:
        """解码报文"""
        if len(data) < 10:
            raise ValueError("报文太短")
        
        magic, version, msg_type, length = struct.unpack('>HHHH', data[:8])
        
        if magic != PROTO_MAGIC:
            raise ValueError(f"无效的魔数: 0x{magic:04X}")
        
        if version != PROTO_VERSION:
            raise ValueError(f"不支持的协议版本: {version}")
        
        payload = data[8:8+length]
        received_checksum = struct.unpack('>H', data[8+length:10+length])[0]
        
        calculated_checksum = ProtocolCodec.calculate_checksum(data[:8+length])
        if received_checksum != calculated_checksum:
            raise ValueError("校验和错误")
        
        # 解析载荷
        if msg_type == MSG_TYPE_CONTROL_CMD:
            cmd_data = json.loads(payload.decode('utf-8'))
            return {
                'msg_type': msg_type,
                'cmd_type': cmd_data.get('cmd_type'),
                'params': cmd_data.get('params', {}),
                'sequence': cmd_data.get('sequence', 0)
            }
        
        return {
            'msg_type': msg_type,
            'payload': payload
        }
    
    @staticmethod
    def encode_response(msg_type: int, data: dict, sequence: int = 0) -> bytes:
        """编码响应报文"""
        data['sequence'] = sequence
        payload = json.dumps(data).encode('utf-8')
        
        header = struct.pack(
            '>HHHH',
            PROTO_MAGIC,
            PROTO_VERSION,
            msg_type,
            len(payload)
        )
        
        checksum = ProtocolCodec.calculate_checksum(header + payload)
        msg = header + payload + struct.pack('>H', checksum)
        return msg

# ============================================================================
# 设备模拟器基类
# ============================================================================

class DeviceSimulator:
    """设备模拟器基类"""
    
    def __init__(self, device_info: DeviceInfo, server_host: str = SERVER_HOST, server_port: int = SERVER_PORT):
        self.device_info = device_info
        self.server_host = server_host
        self.server_port = server_port
        self.socket: Optional[socket.socket] = None
        self.running = False
        self.sequence = 0
        self.test_callback: Optional[Callable] = None
        
    def connect(self) -> bool:
        """连接到服务器"""
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.connect((self.server_host, self.server_port))
            
            # 发送注册报文
            register_msg = ProtocolCodec.encode_register_msg(self.device_info)
            self.socket.sendall(register_msg)
            
            logger.info(f"[{self.device_info.device_id}] 设备注册成功")
            self.device_info.status = DeviceStatus.ONLINE
            return True
            
        except Exception as e:
            logger.error(f"[{self.device_info.device_id}] 连接失败: {e}")
            return False
    
    def start(self):
        """启动设备模拟器"""
        if not self.connect():
            return False
        
        self.running = True
        
        # 启动心跳线程
        heartbeat_thread = threading.Thread(target=self._heartbeat_loop, daemon=True)
        heartbeat_thread.start()
        
        # 启动数据上报线程
        data_thread = threading.Thread(target=self._data_report_loop, daemon=True)
        data_thread.start()
        
        # 启动消息接收线程
        receive_thread = threading.Thread(target=self._receive_loop, daemon=True)
        receive_thread.start()
        
        logger.info(f"[{self.device_info.device_id}] 设备模拟器启动成功")
        return True
    
    def stop(self):
        """停止设备模拟器"""
        self.running = False
        if self.socket:
            self.socket.close()
        logger.info(f"[{self.device_info.device_id}] 设备模拟器已停止")
    
    def _heartbeat_loop(self):
        """心跳循环"""
        while self.running:
            try:
                self._send_heartbeat()
                time.sleep(30)  # 30秒心跳间隔
            except Exception as e:
                logger.error(f"[{self.device_info.device_id}] 心跳发送失败: {e}")
                break
    
    def _send_heartbeat(self):
        """发送心跳报文"""
        heartbeat_data = {
            "device_id": self.device_info.device_id,
            "timestamp": int(time.time() * 1000),
            "status": self.device_info.status.value
        }
        
        msg = ProtocolCodec.encode_response(
            MSG_TYPE_HEARTBEAT,
            heartbeat_data,
            self.sequence
        )
        
        self.socket.sendall(msg)
        self.device_info.last_heartbeat = time.time()
    
    def _data_report_loop(self):
        """数据上报循环"""
        while self.running:
            try:
                if self.device_info.status == DeviceStatus.TESTING:
                    data = self._generate_telemetry_data()
                    self._send_data_report(data)
                time.sleep(1)  # 1秒数据上报间隔
            except Exception as e:
                logger.error(f"[{self.device_info.device_id}] 数据上报失败: {e}")
                break
    
    def _generate_telemetry_data(self) -> dict:
        """生成遥测数据（子类重写）"""
        return {}
    
    def _send_data_report(self, data: dict):
        """发送数据上报报文"""
        telemetry = TelemetryData(
            device_id=self.device_info.device_id,
            timestamp=int(time.time() * 1000),
            data_type=self.device_info.device_type.value,
            data_value=data
        )
        
        msg = ProtocolCodec.encode_response(
            MSG_TYPE_DATA_REPORT,
            {
                "telemetry": telemetry.__dict__,
                "data_hash": hashlib.md5(json.dumps(telemetry.__dict__).encode()).hexdigest()
            },
            self.sequence
        )
        
        self.socket.sendall(msg)
    
    def _receive_loop(self):
        """消息接收循环"""
        buffer = bytearray()
        
        while self.running:
            try:
                data = self.socket.recv(4096)
                if not data:
                    break
                
                buffer.extend(data)
                
                # 解析报文
                while len(buffer) >= 10:
                    try:
                        # 读取报文头
                        magic, version, msg_type, length = struct.unpack('>HHHH', buffer[:8])
                        
                        # 检查是否收到完整报文
                        if len(buffer) < 10 + length:
                            break
                        
                        # 提取完整报文
                        msg_data = bytes(buffer[:10 + length])
                        del buffer[:10 + length]
                        
                        # 解析报文
                        parsed_msg = ProtocolCodec.decode_message(msg_data)
                        
                        # 处理控制指令
                        if parsed_msg['msg_type'] == MSG_TYPE_CONTROL_CMD:
                            self._handle_control_command(parsed_msg)
                            
                    except ValueError as e:
                        logger.warning(f"[{self.device_info.device_id}] 报文解析错误: {e}")
                        buffer.clear()
                        break
                        
            except Exception as e:
                logger.error(f"[{self.device_info.device_id}] 接收消息失败: {e}")
                break
    
    def _handle_control_command(self, cmd_msg: dict):
        """处理控制指令"""
        cmd_type = cmd_msg['cmd_type']
        params = cmd_msg['params']
        sequence = cmd_msg['sequence']
        
        logger.info(f"[{self.device_info.device_id}] 收到控制指令: type={cmd_type}, params={params}")
        
        response = {"device_id": self.device_info.device_id, "result": "success"}
        
        try:
            if cmd_type == CTRL_CMD_START_TEST:
                self.device_info.status = DeviceStatus.TESTING
                self.test_sequence = params.get('sequence', 0)
                response['message'] = "测试开始"
                
            elif cmd_type == CTRL_CMD_STOP_TEST:
                self.device_info.status = DeviceStatus.BOUND
                response['message'] = "测试停止"
                
            elif cmd_type == CTRL_CMD_RESET:
                self.device_info.status = DeviceStatus.ONLINE
                response['message'] = "设备复位"
                
            elif cmd_type == CTRL_CMD_CALIBRATE:
                response['message'] = "校准完成"
                
            elif cmd_type == CTRL_CMD_QUERY_STATUS:
                response['status'] = {
                    'device_status': self.device_info.status.value,
                    'test_sequence': self.test_sequence,
                    'last_heartbeat': self.device_info.last_heartbeat
                }
                
            # 调用测试回调
            if self.test_callback:
                self.test_callback(cmd_type, params)
                
        except Exception as e:
            response['result'] = "failed"
            response['message'] = str(e)
            logger.error(f"[{self.device_info.device_id}] 控制指令执行失败: {e}")
        
        # 发送响应
        resp_msg = ProtocolCodec.encode_response(
            MSG_TYPE_CONTROL_RESP,
            response,
            sequence
        )
        self.socket.sendall(resp_msg)
        self.sequence += 1

# ============================================================================
# 舵面传感器模拟器
# ============================================================================

class RudderSensorSimulator(DeviceSimulator):
    """舵面传感器模拟器"""
    
    def __init__(self, sensor_id: str, sensor_name: str, sensor_index: int):
        device_info = DeviceInfo(
            device_id=f"RUD-{sensor_id}",
            device_name=sensor_name,
            device_type=DeviceType.RUDDER_SENSOR,
            ip_address="192.168.100.100",
            port=DEVICE_PORTS[f"rudder_sensor_{sensor_index}"]
        )
        super().__init__(device_info)
        self.sensor_index = sensor_index
        self.angle = 0.0  # 舵面角度
    
    def _generate_telemetry_data(self) -> dict:
        """生成舵面传感器数据"""
        # 模拟舵面角度变化
        self.angle = max(-90, min(90, self.angle + random.uniform(-5, 5)))
        
        return {
            "sensor_index": self.sensor_index,
            "angle": round(self.angle, 2),
            "voltage": round(random.uniform(4.8, 5.2), 2),
            "temperature": round(random.uniform(20, 40), 1),
            "signal_strength": random.randint(60, 100)
        }

# ============================================================================
# 单片机（治具）模拟器
# ============================================================================

class MicrocontrollerSimulator(DeviceSimulator):
    """单片机（治具）模拟器"""
    
    def __init__(self, mcu_id: str, mcu_name: str):
        device_info = DeviceInfo(
            device_id=f"MCU-{mcu_id}",
            device_name=mcu_name,
            device_type=DeviceType.MICROCONTROLLER,
            ip_address="192.168.100.100",
            port=DEVICE_PORTS["microcontroller"]
        )
        super().__init__(device_info)
        self.step_status = {i: TEST_STEP_PENDING for i in range(1, 11)}
        self.test_results = {}
    
    def _generate_telemetry_data(self) -> dict:
        """生成单片机测试数据"""
        # 模拟测试步骤状态变化
        current_step = self.test_sequence
        if current_step > 0 and current_step <= 10:
            self.step_status[current_step] = TEST_STEP_RUNNING
            
            # 模拟测试结果
            time.sleep(random.uniform(1, 3))  # 模拟测试耗时
            
            if random.random() < 0.95:  # 95%通过率
                self.step_status[current_step] = TEST_STEP_PASSED
                self.test_results[current_step] = {
                    "status": "passed",
                    "duration": random.randint(1000, 3000),
                    "telemetry": self._generate_step_telemetry(current_step)
                }
            else:
                self.step_status[current_step] = TEST_STEP_FAILED
                self.test_results[current_step] = {
                    "status": "failed",
                    "duration": random.randint(1000, 3000),
                    "error_message": self._generate_error_message(current_step)
                }
        
        return {
            "test_sequence": self.test_sequence,
            "step_status": self.step_status,
            "battery_voltage": round(random.uniform(11.8, 12.6), 2),
            "system_status": "normal" if all(s in [TEST_STEP_PASSED, TEST_STEP_PENDING] 
                                             for s in self.step_status.values()) else "error"
        }
    
    def _generate_step_telemetry(self, step: int) -> dict:
        """生成测试步骤遥测数据"""
        telemetry_templates = {
            1: {"name": "备检并获取编码", "data": {"code": f"UAV-{random.randint(1000, 9999)}"}},
            2: {"name": "机身静态测试前检查", "data": {"result": "passed"}},
            3: {"name": "机身及螺旋桨安装情况检查", "data": {"result": "passed"}},
            4: {"name": "电压测量检查", "data": {"voltage": round(random.uniform(11.8, 12.6), 2)}},
            5: {"name": "链路功能检查", "data": {"link_quality": random.randint(80, 100)}},
            6: {"name": "上电参数检查", "data": {"result": "passed"}},
            7: {"name": "夜航灯测试", "data": {"brightness": random.randint(50, 100)}},
            8: {"name": "气压高度检测", "data": {"altitude": round(random.uniform(0, 1000), 2)}},
            9: {"name": "系统电磁兼容性功能检查", "data": {"result": "passed"}},
            10: {"name": "航线加载及载荷功能检查", "data": {"waypoint_count": random.randint(1, 10)}}
        }
        
        return telemetry_templates.get(step, {"name": f"步骤{step}", "data": {}})
    
    def _generate_error_message(self, step: int) -> str:
        """生成错误消息"""
        error_messages = {
            1: "设备编码获取失败",
            2: "机身静态检查异常",
            3: "螺旋桨安装检查失败",
            4: "电压测量超出范围",
            5: "链路质量检测失败",
            6: "上电参数检查异常",
            7: "夜航灯测试失败",
            8: "气压高度检测超时",
            9: "电磁兼容性检测失败",
            10: "航线加载失败"
        }
        
        return error_messages.get(step, "未知错误")

# ============================================================================
# 地测口（无人机）模拟器
# ============================================================================

class GroundTestPortSimulator(DeviceSimulator):
    """地测口（无人机）模拟器"""
    
    def __init__(self, drone_id: str, drone_name: str):
        device_info = DeviceInfo(
            device_id=f"DRN-{drone_id}",
            device_name=drone_name,
            device_type=DeviceType.GROUND_TEST,
            ip_address="192.168.100.100",
            port=DEVICE_PORTS["ground_test_port"]
        )
        super().__init__(device_info)
        self.flight_state = {
            "in_air": False,
            "altitude": 0.0,
            "speed": 0.0,
            "heading": 0.0,
            "pitch": 0.0,
            "roll": 0.0
        }
    
    def _generate_telemetry_data(self) -> dict:
        """生成无人机遥测数据"""
        # 模拟飞行状态变化
        if self.device_info.status == DeviceStatus.TESTING:
            self.flight_state["altitude"] = round(self.flight_state["altitude"] + random.uniform(-2, 2), 2)
            self.flight_state["altitude"] = max(0, min(500, self.flight_state["altitude"]))
            
            self.flight_state["speed"] = round(self.flight_state["speed"] + random.uniform(-1, 1), 2)
            self.flight_state["speed"] = max(0, min(30, self.flight_state["speed"]))
            
            self.flight_state["heading"] = round(self.flight_state["heading"] + random.uniform(-5, 5), 2)
            self.flight_state["pitch"] = round(self.flight_state["pitch"] + random.uniform(-0.5, 0.5), 2)
            self.flight_state["roll"] = round(self.flight_state["roll"] + random.uniform(-0.5, 0.5), 2)
        
        return {
            "flight_state": self.flight_state,
            "battery_voltage": round(random.uniform(11.8, 12.6), 2),
            "battery_current": round(random.uniform(5, 15), 2),
            "gps_satellites": random.randint(8, 12),
            "gps_accuracy": round(random.uniform(0.5, 2.0), 2)
        }

# ============================================================================
# 扫码枪模拟器
# ============================================================================

class ScannerSimulator(DeviceSimulator):
    """扫码枪模拟器"""
    
    def __init__(self, scanner_id: str):
        device_info = DeviceInfo(
            device_id=f"SCN-{scanner_id}",
            device_name=f"扫码枪{scanner_id}",
            device_type=DeviceType.SCANNER,
            ip_address="192.168.100.100",
            port=DEVICE_PORTS["scanner"]
        )
        super().__init__(device_info)
        self.scanned_codes = []
    
    def _generate_telemetry_data(self) -> dict:
        """生成扫码枪数据（按需扫描）"""
        if random.random() < 0.1:  # 10%概率扫描到新码
            new_code = f"UAV-{random.randint(10000, 99999)}"
            self.scanned_codes.append({
                "code": new_code,
                "timestamp": int(time.time() * 1000)
            })
            logger.info(f"[{self.device_info.device_id}] 扫描到新码: {new_code}")
        
        return {
            "scanned_count": len(self.scanned_codes),
            "last_scanned": self.scanned_codes[-1] if self.scanned_codes else None,
            "battery_level": random.randint(80, 100)
        }

# ============================================================================
# 噪音传感器模拟器
# ============================================================================

class NoiseSensorSimulator(DeviceSimulator):
    """噪音传感器模拟器"""
    
    def __init__(self, sensor_id: str, sensor_index: int):
        device_info = DeviceInfo(
            device_id=f"NSE-{sensor_id}",
            device_name=f"噪音传感器{sensor_index}",
            device_type=DeviceType.NOISE_SENSOR,
            ip_address="192.168.100.100",
            port=DEVICE_PORTS["noise_sensor"]
        )
        super().__init__(device_info)
        self.sensor_index = sensor_index
    
    def _generate_telemetry_data(self) -> dict:
        """生成噪音传感器数据"""
        # 模拟噪音水平（dB）
        noise_level = random.gauss(65, 10)  # 均值65dB，标准差10dB
        noise_level = max(30, min(120, noise_level))
        
        return {
            "sensor_index": self.sensor_index,
            "noise_level": round(noise_level, 2),
            "temperature": round(random.uniform(15, 35), 1),
            "humidity": round(random.uniform(30, 80), 1),
            "alert": noise_level > 85
        }

# ============================================================================
# 设备管理器
# ============================================================================

class DeviceManager:
    """设备管理器"""
    
    def __init__(self):
        self.devices: Dict[str, DeviceSimulator] = {}
        self.lock = threading.Lock()
    
    def add_device(self, device: DeviceSimulator) -> bool:
        """添加设备"""
        with self.lock:
            if device.device_info.device_id in self.devices:
                logger.warning(f"设备 {device.device_info.device_id} 已存在")
                return False
            
            self.devices[device.device_info.device_id] = device
            logger.info(f"设备 {device.device_info.device_id} 已添加")
            return True
    
    def remove_device(self, device_id: str) -> bool:
        """移除设备"""
        with self.lock:
            if device_id not in self.devices:
                logger.warning(f"设备 {device_id} 不存在")
                return False
            
            self.devices[device_id].stop()
            del self.devices[device_id]
            logger.info(f"设备 {device_id} 已移除")
            return True
    
    def start_device(self, device_id: str) -> bool:
        """启动设备"""
        with self.lock:
            if device_id not in self.devices:
                logger.warning(f"设备 {device_id} 不存在")
                return False
            
            return self.devices[device_id].start()
    
    def stop_device(self, device_id: str) -> bool:
        """停止设备"""
        with self.lock:
            if device_id not in self.devices:
                logger.warning(f"设备 {device_id} 不存在")
                return False
            
            self.devices[device_id].stop()
            return True
    
    def start_all_devices(self) -> int:
        """启动所有设备"""
        count = 0
        with self.lock:
            for device_id, device in self.devices.items():
                if device.start():
                    count += 1
        return count
    
    def stop_all_devices(self) -> int:
        """停止所有设备"""
        count = 0
        with self.lock:
            for device_id, device in self.devices.items():
                if device.running:
                    device.stop()
                    count += 1
        return count
    
    def get_device_status(self) -> Dict:
        """获取设备状态"""
        with self.lock:
            return {
                device_id: {
                    "name": device.device_info.device_name,
                    "type": device.device_info.device_type.value,
                    "status": device.device_info.status.value,
                    "last_heartbeat": device.device_info.last_heartbeat,
                    "running": device.running
                }
                for device_id, device in self.devices.items()
            }

# ============================================================================
# 测试场景管理器
# ============================================================================

class TestScenarioManager:
    """测试场景管理器"""
    
    def __init__(self, device_manager: DeviceManager):
        self.device_manager = device_manager
        self.scenarios = {
            "normal_test": self._normal_test_scenario,
            "quick_test": self._quick_test_scenario,
            "failure_test": self._failure_test_scenario,
            "stress_test": self._stress_test_scenario
        }
    
    def run_scenario(self, scenario_name: str) -> bool:
        """运行测试场景"""
        if scenario_name not in self.scenarios:
            logger.error(f"测试场景 {scenario_name} 不存在")
            return False
        
        logger.info(f"开始运行测试场景: {scenario_name}")
        return self.scenarios[scenario_name]()
    
    def _normal_test_scenario(self) -> bool:
        """正常测试场景"""
        logger.info("执行正常测试场景")
        
        # 1. 启动所有设备
        started = self.device_manager.start_all_devices()
        logger.info(f"已启动 {started} 个设备")
        
        # 2. 等待设备上线
        time.sleep(5)
        
        # 3. 执行测试序列
        mcu_devices = [device for device in self.device_manager.devices.values()
                      if device.device_info.device_type == DeviceType.MICROCONTROLLER]
        
        for mcu in mcu_devices:
            logger.info(f"启动设备 {mcu.device_info.device_id} 的测试序列")
            mcu.device_info.status = DeviceStatus.TESTING
            mcu.test_sequence = 1
            
            # 等待测试完成
            time.sleep(30)
            
            # 停止测试
            mcu.device_info.status = DeviceStatus.BOUND
            logger.info(f"设备 {mcu.device_info.device_id} 测试完成")
        
        logger.info("正常测试场景完成")
        return True
    
    def _quick_test_scenario(self) -> bool:
        """快速测试场景"""
        logger.info("执行快速测试场景")
        
        # 只启动必要的设备
        mcu_devices = [device for device in self.device_manager.devices.values()
                      if device.device_info.device_type == DeviceType.MICROCONTROLLER]
        
        for mcu in mcu_devices:
            mcu.start()
            time.sleep(2)
            mcu.device_info.status = DeviceStatus.TESTING
            mcu.test_sequence = 1
            time.sleep(5)
            mcu.device_info.status = DeviceStatus.BOUND
            mcu.stop()
        
        logger.info("快速测试场景完成")
        return True
    
    def _failure_test_scenario(self) -> bool:
        """故障测试场景"""
        logger.info("执行故障测试场景")
        
        # 模拟部分设备故障
        mcu_devices = [device for device in self.device_manager.devices.values()
                      if device.device_info.device_type == DeviceType.MICROCONTROLLER]
        
        for i, mcu in enumerate(mcu_devices):
            mcu.start()
            time.sleep(2)
            
            if i < len(mcu_devices) // 2:
                # 前半部分设备正常测试
                mcu.device_info.status = DeviceStatus.TESTING
                mcu.test_sequence = 1
                time.sleep(10)
                mcu.device_info.status = DeviceStatus.BOUND
            else:
                # 后半部分设备模拟故障
                mcu.device_info.status = DeviceStatus.TESTING
                mcu.test_sequence = 4  # 模拟第4步失败
                time.sleep(5)
                logger.warning(f"设备 {mcu.device_info.device_id} 模拟故障")
                mcu.stop()
        
        logger.info("故障测试场景完成")
        return True
    
    def _stress_test_scenario(self) -> bool:
        """压力测试场景"""
        logger.info("执行压力测试场景")
        
        # 启动所有设备
        self.device_manager.start_all_devices()
        time.sleep(3)
        
        # 快速启动多个测试
        mcu_devices = [device for device in self.device_manager.devices.values()
                      if device.device_info.device_type == DeviceType.MICROCONTROLLER]
        
        for mcu in mcu_devices:
            mcu.device_info.status = DeviceStatus.TESTING
            mcu.test_sequence = 1
            time.sleep(1)
        
        # 等待所有测试完成
        time.sleep(60)
        
        # 停止所有测试
        for mcu in mcu_devices:
            mcu.device_info.status = DeviceStatus.BOUND
        
        logger.info("压力测试场景完成")
        return True

# ============================================================================
# 主程序
# ============================================================================

def create_test_station(station_id: int) -> DeviceManager:
    """创建测试工位设备集合"""
    manager = DeviceManager()
    
    # 根据工位ID创建不同的设备组合
    if station_id == 1100:  # 心跳检测工位
        for i in range(1, 6):
            sensor = RudderSensorSimulator(
                f"1100-{i}",
                f"舵面传感器{i}",
                i
            )
            manager.add_device(sensor)
    
    elif station_id == 1200:  # 磁航向工位
        mcu = MicrocontrollerSimulator(
            "1200-1",
            "磁航向单片机"
        )
        manager.add_device(mcu)
        
        scanner = ScannerSimulator("1200-1")
        manager.add_device(scanner)
    
    elif station_id == 1500:  # 治具测试工位
        mcu = MicrocontrollerSimulator(
            "1500-1",
            "治具单片机"
        )
        manager.add_device(mcu)
        
        drone = GroundTestPortSimulator(
            "1500-1",
            "测试无人机1"
        )
        manager.add_device(drone)
        
        for i in range(1, 6):
            sensor = RudderSensorSimulator(
                f"1500-{i}",
                f"舵面传感器{i}",
                i
            )
            manager.add_device(sensor)
    
    elif station_id == 1600:  # 舵面采集工位
        mcu = MicrocontrollerSimulator(
            "1600-1",
            "舵面采集单片机"
        )
        manager.add_device(mcu)
        
        for i in range(1, 6):
            sensor = RudderSensorSimulator(
                f"1600-{i}",
                f"舵面传感器{i}",
                i
            )
            manager.add_device(sensor)
    
    elif station_id == 1700:  # 告警检测工位
        for i in range(1, 3):
            noise_sensor = NoiseSensorSimulator(
                f"1700-{i}",
                i
            )
            manager.add_device(noise_sensor)
    
    return manager

def main():
    """主程序"""
    import argparse
    
    parser = argparse.ArgumentParser(description='无人机测试设备模拟器')
    parser.add_argument('--station', type=int, required=True, help='工位ID (1100-1700)')
    parser.add_argument('--scenario', type=str, default='normal_test', help='测试场景')
    parser.add_argument('--list-scenarios', action='store_true', help='列出所有测试场景')
    args = parser.parse_args()
    
    # 列出测试场景
    if args.list_scenarios:
        print("可用测试场景:")
        for scenario_name in ['normal_test', 'quick_test', 'failure_test', 'stress_test']:
            print(f"  - {scenario_name}")
        return
    
    # 创建设备管理器
    device_manager = create_test_station(args.station)
    
    # 创建测试场景管理器
    scenario_manager = TestScenarioManager(device_manager)
    
    # 运行测试场景
    success = scenario_manager.run_scenario(args.scenario)
    
    if success:
        logger.info("测试场景执行成功")
        
        # 显示设备状态
        status = device_manager.get_device_status()
        print("\n设备状态:")
        for device_id, info in status.items():
            print(f"  {device_id}: {info}")
        
        # 等待用户输入退出
        try:
            input("\n按Enter键停止所有设备...")
        except KeyboardInterrupt:
            pass
        
        device_manager.stop_all_devices()
    else:
        logger.error("测试场景执行失败")
        device_manager.stop_all_devices()
        return 1
    
    return 0

if __name__ == '__main__':
    import sys
    sys.exit(main())
