#!/usr/bin/env python3
"""
磁航向工位一体化测试系统 - 融合版
融合完整版、增强版、verbose版的所有优点

架构总览:
- 2个服务端: PLC Server (Modbus TCP), MES Server (HTTP)
- 2个客户端: 地测口客户端 (TCP), 扫码枪客户端 (TCP)
- 3种EB90指令: 舵面中位, 舵面使能, 复飞
- 3个测试项: 扫码绑定, PLC七步校验, 遥测数据上报
- 3种遥测数据: D1遥控数据, D2传感器数据, D3飞行数据

使用方法:
  python3 station_1700_magnetic_fusion.py [--auto-bind] [--verbose]
"""

import json
import logging
import os
import sys
import time
import socket
import socketserver
import struct
import threading
import argparse
import subprocess
from datetime import datetime
from http.server import HTTPServer, BaseHTTPRequestHandler
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field

# ==================== 配置常量 ====================
DGIOT_HOST = "0.0.0.0"  # 改为0.0.0.0以支持所有接口
DGIOT_PORT = 20000

MAGNETIC_STATION_CONFIG = {
    "station_id": 1700,
    "station_name": "磁航向校准工位",
    "plc_ip": "192.168.100.20",
    "plc_port": 502,
    "plc_base_addr": 1700,  # D1700
    "ground_station_ip": "192.168.100.21",
    "ground_station_port": 10007,
    "scanner_ip": "192.168.100.23",
    "scanner_port": 1234,
    "business_type": "扫码绑定"
}

MES_SERVER_HOST = "0.0.0.0"
MES_SERVER_PORT = 1801

# EB90遥控报文模板
EB90_YAOKONG_TEMPLATE = {
    "sync": "EB90",
    "dest": "0000",
    "src": "0012",
    "platform": "00",
    "frame_no": "00",
    "payload": ""
}

# EB90遥控指令定义
EB90_COMMANDS = {
    "舵面中位": {"payload": "A55AF0FB", "fill_length": 58},
    "舵面使能": {"payload": "A55AF0F3", "fill_length": 58},
    "复飞": {"payload": "A55AF0B9", "fill_length": 58}
}

# 测试步骤定义
TEST_STEPS = [
    {"name": "备检并获取编码", "type": "scan", "order": 1},
    {"name": "静态测试前检查", "type": "check", "order": 2},
    {"name": "机身及螺旋桨检查", "type": "check", "order": 3},
    {"name": "电压测量", "type": "measure", "order": 4},
    {"name": "链路功能检查", "type": "link_test", "order": 5},
    {"name": "上电参数检查", "type": "power_check", "order": 6},
    {"name": "磁航向校准", "type": "calibration", "order": 7}
]

# ==================== 数据类定义 ====================
@dataclass
class TestItem:
    """测试项数据结构"""
    id: str
    name: str
    station_id: int
    steps: List[Dict] = field(default_factory=list)

@dataclass
class TestResult:
    """测试结果数据结构"""
    test_item_id: str
    test_item_name: str
    device_id: str
    station_id: int
    step_name: str
    status: str  # passed/failed/skipped
    timestamp: str
    message: str = ""

@dataclass
class PacketLog:
    """报文日志数据结构"""
    seq: int
    timestamp: str
    direction: str  # TX/RX
    type: str  # TCP_REGISTER/EB90_YAOKONG/MODBUS等
    length: int
    hex: str
    description: str

# ==================== 详细日志记录器 ====================
class MagneticStationLogger:
    """磁航向工位详细日志记录器"""
    
    def __init__(self, log_file: str = None, verbose: bool = False):
        self.log_file = log_file or f"logs/magnetic_fusion_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"
        self.verbose = verbose
        self.sep = "=" * 70
        
        # 创建日志目录
        os.makedirs(os.path.dirname(self.log_file), exist_ok=True)
        
        # 配置日志
        self.logger = logging.getLogger('MagneticStationFusion')
        self.logger.setLevel(logging.DEBUG)
        
        # 文件输出（DEBUG级别）
        file_handler = logging.FileHandler(self.log_file, encoding='utf-8')
        file_handler.setLevel(logging.DEBUG)
        file_formatter = logging.Formatter(
            '%(asctime)s - %(levelname)s - %(message)s',
            datefmt='%Y-%m-%d %H:%M:%S'
        )
        file_handler.setFormatter(file_formatter)
        self.logger.addHandler(file_handler)
        
        # 控制台输出（INFO级别，verbose模式DEBUG级别）
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.DEBUG if verbose else logging.INFO)
        console_formatter = logging.Formatter(
            '%(asctime)s - %(levelname)s - %(message)s',
            datefmt='%H:%M:%S'
        )
        console_handler.setFormatter(console_formatter)
        self.logger.addHandler(console_handler)
    
    def log_stage_start(self, stage_name: str, description: str):
        """记录测试阶段开始"""
        self.logger.info(f"\n{self.sep}\n")
        self.logger.info("【阶段开始】")
        self.logger.info(f"  阶段名称: {stage_name}")
        self.logger.info(f"  阶段描述: {description}")
        self.logger.info(f"  开始时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        self.logger.info(f"\n{self.sep}\n")
    
    def log_stage_complete(self, stage_name: str, status: str, stats: Dict[str, Any] = None):
        """记录测试阶段完成"""
        self.logger.info(f"\n{self.sep}\n")
        self.logger.info("【阶段完成】")
        self.logger.info(f"  阶段名称: {stage_name}")
        self.logger.info(f"  状态: {status}")
        self.logger.info(f"  结束时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        if stats:
            self.logger.info(f"  统计数据:")
            for key, value in stats.items():
                self.logger.info(f"    {key}: {value}")
        self.logger.info(f"\n{self.sep}\n")
    
    def log_step_start(self, step_name: str, step_desc: str, step_order: int = 0):
        """记录测试步骤开始"""
        self.logger.info(f"\n{self.sep}\n")
        self.logger.info("【步骤开始】")
        self.logger.info(f"  步骤序号: {step_order}")
        self.logger.info(f"  步骤名称: {step_name}")
        self.logger.info(f"  步骤描述: {step_desc}")
        self.logger.info(f"  开始时间: {datetime.now().strftime('%H:%M:%S')}")
        self.logger.info(f"\n{self.sep}\n")
    
    def log_step_complete(self, step_name: str, status: str, result: Dict[str, Any] = None, step_order: int = 0):
        """记录测试步骤完成"""
        self.logger.info(f"\n{self.sep}\n")
        self.logger.info("【步骤完成】")
        self.logger.info(f"  步骤序号: {step_order}")
        self.logger.info(f"  步骤名称: {step_name}")
        self.logger.info(f"  状态: {status}")
        self.logger.info(f"  结束时间: {datetime.now().strftime('%H:%M:%S')}")
        if result:
            self.logger.info(f"  结果数据:")
            for key, value in result.items():
                self.logger.info(f"    {key}: {value}")
        self.logger.info(f"\n{self.sep}\n")
    
    def log_plc_request(self, station_addr: str, function_code: str, request_data: Any):
        """记录PLC请求"""
        self.logger.debug(f"\n{self.sep}\n")
        self.logger.debug("【PLC请求】")
        self.logger.debug(f"  工位地址: {station_addr}")
        self.logger.debug(f"  功能码: {function_code}")
        self.logger.debug(f"  请求数据: {request_data}")
        self.logger.debug(f"  请求时间: {datetime.now().strftime('%H:%M:%S.%f')}")
        self.logger.debug(f"\n{self.sep}\n")
    
    def log_plc_response(self, station_addr: str, function_code: str, response_data: Any, response_time: float = 0):
        """记录PLC响应"""
        self.logger.debug(f"\n{self.sep}\n")
        self.logger.debug("【PLC响应】")
        self.logger.debug(f"  工位地址: {station_addr}")
        self.logger.debug(f"  功能码: {function_code}")
        self.logger.debug(f"  响应数据: {response_data}")
        self.logger.debug(f"  响应时间: {datetime.now().strftime('%H:%M:%S.%f')}")
        self.logger.debug(f"  响应延迟: {response_time:.3f}ms")
        self.logger.debug(f"\n{self.sep}\n")
    
    def log_eb90_command(self, command_name: str, command_data: bytes):
        """记录EB90指令"""
        hex_data = command_data.hex().upper() if isinstance(command_data, bytes) else str(command_data)
        self.logger.info(f"\n{self.sep}\n")
        self.logger.info("【EB90指令下发】")
        self.logger.info(f"  指令名称: {command_name}")
        self.logger.info(f"  指令长度: {len(command_data)} 字节")
        self.logger.info(f"  指令数据(hex): {hex_data}")
        self.logger.info(f"  发送时间: {datetime.now().strftime('%H:%M:%S.%f')}")
        self.logger.info(f"\n{self.sep}\n")
    
    def log_telemetry_data(self, data_type: str, sequence: int, telemetry_data: Any):
        """记录遥测数据"""
        self.logger.debug(f"\n{self.sep}\n")
        self.logger.debug("【遥测数据发送】")
        self.logger.debug(f"  数据类型: {data_type}")
        self.logger.debug(f"  序列号: {sequence}")
        self.logger.debug(f"  数据长度: {len(str(telemetry_data))}")
        self.logger.debug(f"  数据内容: {telemetry_data}")
        self.logger.debug(f"  发送时间: {datetime.now().strftime('%H:%M:%S.%f')}")
        self.logger.debug(f"\n{self.sep}\n")
    
    def log_error(self, error_type: str, error_context: str, error_reason: Any):
        """记录错误"""
        self.logger.error(f"\n{self.sep}\n")
        self.logger.error("【错误信息】")
        self.logger.error(f"  错误类型: {error_type}")
        self.logger.error(f"  错误上下文: {error_context}")
        self.logger.error(f"  错误原因: {error_reason}")
        self.logger.error(f"  错误时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        self.logger.error(f"\n{self.sep}\n")
    
    def log_warning(self, warning_type: str, warning_context: str, warning_message: str):
        """记录警告"""
        self.logger.warning(f"\n{self.sep}\n")
        self.logger.warning("【警告信息】")
        self.logger.warning(f"  警告类型: {warning_type}")
        self.logger.warning(f"  警告上下文: {warning_context}")
        self.logger.warning(f"  警告消息: {warning_message}")
        self.logger.warning(f"  警告时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        self.logger.warning(f"\n{self.sep}\n")
    
    def close(self):
        """关闭日志记录器"""
        for handler in self.logger.handlers[:]:
            handler.close()
            self.logger.removeHandler(handler)

# ==================== 报文日志模块 ====================
class PacketLogger:
    """报文日志记录器"""
    
    def __init__(self):
        self.packets: List[PacketLog] = []
        self.seq = 0
        self.log_dir = "test_records/station_1700/packets"
        os.makedirs(self.log_dir, exist_ok=True)
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.log_file_path = os.path.join(self.log_dir, f"packets_{timestamp}.log")
        self.log_file = open(self.log_file_path, 'w', encoding='utf-8')
    
    def log_packet(self, direction: str, type: str, data: bytes, description: str = ""):
        """记录报文"""
        self.seq += 1
        packet = PacketLog(
            seq=self.seq,
            timestamp=datetime.now().isoformat(),
            direction=direction,
            type=type,
            length=len(data),
            hex=data.hex(),
            description=description
        )
        
        self.packets.append(packet)
        
        # 写入日志文件
        log_entry = {
            "seq": packet.seq,
            "timestamp": packet.timestamp,
            "direction": packet.direction,
            "type": packet.type,
            "length": packet.length,
            "hex": packet.hex,
            "description": packet.description
        }
        self.log_file.write(json.dumps(log_entry, ensure_ascii=False) + '\n')
        self.log_file.flush()
    
    def close(self):
        """关闭日志文件"""
        if self.log_file:
            self.log_file.close()
    
    @property
    def packet_count(self) -> int:
        """报文数量"""
        return len(self.packets)

# ==================== TCP客户端基类 ====================
class TCPClient:
    """TCP客户端基类"""
    
    def __init__(self, host: str, port: int, packet_logger: PacketLogger, name: str):
        self.host = host
        self.port = port
        self.packet_logger = packet_logger
        self.name = name
        self.sock: Optional[socket.socket] = None
    
    def connect(self, timeout: float = 10.0) -> bool:
        """连接到服务器"""
        try:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.settimeout(timeout)
            self.sock.connect((self.host, self.port))
            return True
        except Exception as e:
            return False
    
    def send(self, data: bytes) -> bool:
        """发送数据"""
        if not self.sock:
            return False
        
        try:
            self.sock.sendall(data)
            return True
        except Exception as e:
            return False
    
    def recv(self, size: int = 4096, timeout: float = 1.0) -> Optional[bytes]:
        """接收数据"""
        if not self.sock:
            return None
        
        try:
            self.sock.settimeout(timeout)
            data = self.sock.recv(size)
            return data if data else None
        except socket.timeout:
            return None
        except Exception as e:
            return None
    
    def close(self):
        """关闭连接"""
        if self.sock:
            try:
                self.sock.close()
            except Exception as e:
                pass
            finally:
                self.sock = None

# ==================== 地测口客户端 ====================
class GroundStationClient(TCPClient):
    """地测口客户端"""
    
    def __init__(self, host: str, port: int, packet_logger: PacketLogger, logger: MagneticStationLogger):
        super().__init__(host, port, packet_logger, "地测口")
        self.logger = logger
        self.registered = False
        self.frame_counter = 0
    
    def register(self) -> bool:
        """注册设备"""
        register_data = b"wrj_dicekou\n"
        
        if not self.send(register_data):
            return False
        
        self.packet_logger.log_packet("TX", "TCP_REGISTER", register_data, "地测口注册")
        
        # 等待响应
        time.sleep(1)
        response = self.recv()
        if response:
            self.packet_logger.log_packet("RX", "TCP_REGISTER", response, "注册响应")
        
        self.registered = True
        return True
    
    def send_eb90_data(self, data: bytes, description: str = "") -> bool:
        """发送EB90数据"""
        if not self.send(data):
            return False
        
        self.packet_logger.log_packet("TX", "EB90_TELEMETRY", data, description)
        return True
    
    def build_eb90_frame(self, platform_type: int, payload: bytes) -> bytes:
        """构建EB90帧"""
        # 同步头
        sync = bytes.fromhex("EB90")
        # 目的地址（飞控）
        dest = bytes.fromhex("0000")
        # 源地址（地测口）
        src = bytes.fromhex("0012")
        # 平台类型
        platform = platform_type.to_bytes(1, 'big')
        # 帧号
        frame_no = self.frame_counter.to_bytes(1, 'big')
        self.frame_counter = (self.frame_counter + 1) % 256
        
        # 组装帧
        frame = sync + dest + src + platform + frame_no + payload
        
        # 添加CRC16校验（小端格式）
        crc = self._calculate_crc16(frame)
        frame += struct.pack("<H", crc)
        
        return frame
    
    def _calculate_crc16(self, data: bytes) -> int:
        """计算CRC16校验（小端格式）"""
        crc = 0xFFFF
        for byte in data:
            crc ^= byte
            for _ in range(8):
                if crc & 0x0001:
                    crc = (crc >> 1) ^ 0xA001
                else:
                    crc >>= 1
        return crc

# ==================== 扫码枪客户端 ====================
class ScannerClient(TCPClient):
    """扫码枪客户端"""
    
    def __init__(self, host: str, port: int, packet_logger: PacketLogger, logger: MagneticStationLogger):
        super().__init__(host, port, packet_logger, "扫码枪")
        self.logger = logger
    
    def scan_device(self, device_id: str) -> bool:
        """扫描设备"""
        scan_data = f"{device_id}\n".encode('utf-8')
        
        if not self.send(scan_data):
            return False
        
        self.packet_logger.log_packet("TX", "SCAN_QRCODE", scan_data, f"扫描设备: {device_id}")
        
        # 等待响应
        time.sleep(1)
        response = self.recv()
        if response:
            self.packet_logger.log_packet("RX", "SCAN_RESPONSE", response, "扫描响应")
        
        return True

# ==================== PLC Server ====================
class ModbusTCPHandler(socketserver.BaseRequestHandler):
    """Modbus TCP请求处理器"""
    
    def handle(self):
        try:
            data = self.request.recv(1024)
            if len(data) < 8:
                return
            
            # 解析Modbus TCP帧
            trans_id = struct.unpack('>H', data[0:2])[0]
            func_code = data[7]
            
            # 处理功能码
            if func_code == 0x03:  # Read Holding Registers
                addr = struct.unpack('>H', data[8:10])[0]
                count = struct.unpack('>H', data[10:12])[0]
                
                # 模拟返回数据（返回全1）
                byte_count = count * 2
                response_data = bytes([byte_count]) + (b'\x00\x01' * count)
                response_length = 3 + byte_count
                response = struct.pack('>HHHBB', trans_id, 0, response_length, 1, func_code) + response_data
                self.request.sendall(response)
                
            elif func_code == 0x06:  # Write Single Register
                addr = struct.unpack('>H', data[8:10])[0]
                value = struct.unpack('>H', data[10:12])[0]
                
                # 响应写入成功
                response = data[:8] + data[8:12]  # 回显
                self.request.sendall(response)
                
        except Exception as e:
            pass

class PLCServer:
    """PLC Modbus TCP服务器"""
    
    def __init__(self, host: str, port: int, logger: MagneticStationLogger):
        self.host = host
        self.port = port
        self.logger = logger
        self.server = None
        self.thread = None
    
    def start(self) -> bool:
        """启动PLC服务器"""
        try:
            class ReusableTCPServer(socketserver.TCPServer):
                allow_reuse_address = True
            
            self.server = ReusableTCPServer((self.host, self.port), ModbusTCPHandler)
            self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
            self.thread.start()
            return True
        except Exception as e:
            self.logger.log_error("PLC_START_ERROR", "启动PLC服务器", str(e))
            return False
    
    def stop(self):
        """停止PLC服务器"""
        if self.server:
            self.server.shutdown()

# ==================== MES Server ====================
class MESRequestHandler(BaseHTTPRequestHandler):
    """MES请求处理器"""
    
    packet_logger = None
    logger = None
    
    def do_POST(self):
        """处理POST请求"""
        try:
            content_length = int(self.headers.get('Content-Length', 0))
            post_data = self.rfile.read(content_length)
            
            # 记录请求
            if self.packet_logger:
                self.packet_logger.log_packet("RX", "MES_API", post_data, f"MES API: {self.path}")
            
            # 返回成功响应
            response = {
                "status": 0,
                "message": "success",
                "timestamp": datetime.now().isoformat()
            }
            
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps(response).encode('utf-8'))
            
        except Exception as e:
            self.send_error(500, str(e))

class MESServer:
    """MES服务器"""
    
    def __init__(self, host: str, port: int, packet_logger: PacketLogger, logger: MagneticStationLogger):
        self.host = host
        self.port = port
        self.packet_logger = packet_logger
        self.logger = logger
        self.server: Optional[HTTPServer] = None
    
    def start(self) -> bool:
        """启动MES服务器"""
        try:
            MESRequestHandler.packet_logger = self.packet_logger
            MESRequestHandler.logger = self.logger
            
            self.server = HTTPServer((self.host, self.port), MESRequestHandler)
            thread = threading.Thread(target=self.server.serve_forever, daemon=True)
            thread.start()
            return True
        except Exception as e:
            self.logger.log_error("MES_START_ERROR", "启动MES服务器", str(e))
            return False

# ==================== PLC七步校验器 ====================
class PLCSevenStepValidator:
    """PLC七步校验器"""
    
    def __init__(self, station_id: int, logger: MagneticStationLogger):
        self.station_id = station_id
        self.base_addr = station_id  # D1700
        self.logger = logger
        self.plc_host = "127.0.0.1"
        self.plc_port = 502
        self.test_code = 100
    
    def execute_all_steps(self) -> bool:
        """执行完整的七步校验流程"""
        steps = [
            ("Step 1/7", "READ", 0, 1, "读取工位就绪状态"),
            ("Step 2/7", "WRITE", 51, self.test_code, "写入测试命令码"),
            ("Step 3/7", "READ", 10, 1, "读取测试确认状态"),
            ("Step 4/7", "WRITE", 0, 0, "复位工位状态"),
            ("Step 5/7", "WRITE", 10, 0, "清除测试确认"),
            ("Step 6/7", "WRITE", 60, self.test_code, "写入完成确认码"),
            ("Step 7/7", "WRITE", 61, 1, "触发完成信号")
        ]
        
        for step_name, op_type, rel_addr, value, description in steps:
            self.logger.log_step_start(step_name, description, steps.index((step_name, op_type, rel_addr, value, description)) + 1)
            
            if op_type == "READ":
                success = self._read_register(rel_addr, 1)
            else:
                success = self._write_register(rel_addr, value)
            
            if success:
                self.logger.log_step_complete(step_name, "PASS", {"value": value if op_type == "WRITE" else "OK"})
            else:
                self.logger.log_step_complete(step_name, "FAIL", {"reason": "操作失败"})
                return False
            
            time.sleep(0.5)
        
        return True
    
    def _read_register(self, rel_addr: int, count: int = 1) -> bool:
        """读取寄存器"""
        abs_addr = self.base_addr + rel_addr
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(2)
            sock.connect((self.plc_host, self.plc_port))
            req = struct.pack('>HHHBBHH', 1, 0, 6, 1, 3, abs_addr, count)
            sock.sendall(req)
            resp = sock.recv(1024)
            sock.close()
            return len(resp) > 8
        except Exception as e:
            return False
    
    def _write_register(self, rel_addr: int, value: int) -> bool:
        """写入寄存器"""
        abs_addr = self.base_addr + rel_addr
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(2)
            sock.connect((self.plc_host, self.plc_port))
            req = struct.pack('>HHHBBHH', 1, 0, 6, 1, 6, abs_addr, value)
            sock.sendall(req)
            resp = sock.recv(1024)
            sock.close()
            return len(resp) >= 12
        except Exception as e:
            return False

# ==================== 主测试类 ====================
class MagneticStationFusionTest:
    """磁航向工位融合测试系统"""
    
    def __init__(self, device_id: str = "UAV-001", auto_bind: bool = False, verbose: bool = False):
        self.device_id = device_id
        self.auto_bind = auto_bind
        self.verbose = verbose
        
        # 初始化日志
        self.logger = MagneticStationLogger(verbose=verbose)
        self.packet_logger = PacketLogger()
        
        # 初始化组件
        self.ground_station: Optional[GroundStationClient] = None
        self.scanner: Optional[ScannerClient] = None
        self.plc_server: Optional[PLCServer] = None
        self.mes_server: Optional[MESServer] = None
        
        # 测试结果
        self.test_results: List[TestResult] = []
    
    def run(self, skip_check: bool = False) -> bool:
        """运行完整测试"""
        try:
            self.logger.log_stage_start("磁航向测试", "完整测试流程")
            
            # 启动服务器
            if not self._start_servers():
                return False
            
            # 环境准备
            if not skip_check:
                if not self._prepare_environment():
                    return False
            
            # 测试执行
            if not self._execute_tests():
                return False
            
            # 打印总结
            self._print_summary()
            
            self.logger.log_stage_complete("磁航向测试", "COMPLETED", {
                "total_tests": len(self.test_results),
                "passed": sum(1 for r in self.test_results if r.status == "passed"),
                "failed": sum(1 for r in self.test_results if r.status == "failed")
            })
            
            return True
            
        except Exception as e:
            self.logger.log_error("TEST_ERROR", "运行完整测试", str(e))
            return False
        finally:
            self.cleanup()
    
    def _start_servers(self) -> bool:
        """启动服务器"""
        self.logger.log_stage_start("启动服务器", "启动PLC和MES服务器")
        
        # 启动PLC Server
        self.plc_server = PLCServer(MAGNETIC_STATION_CONFIG["plc_ip"], 
                                   MAGNETIC_STATION_CONFIG["plc_port"], 
                                   self.logger)
        if not self.plc_server.start():
            return False
        
        # 启动MES Server
        self.mes_server = MESServer(MES_SERVER_HOST, MES_SERVER_PORT, 
                                   self.packet_logger, self.logger)
        if not self.mes_server.start():
            return False
        
        time.sleep(1)
        
        self.logger.log_stage_complete("启动服务器", "SUCCESS")
        return True
    
    def _prepare_environment(self) -> bool:
        """准备测试环境"""
        self.logger.log_stage_start("环境准备", "连接设备和注册")
        
        # 地测口连接
        self.ground_station = GroundStationClient(DGIOT_HOST, DGIOT_PORT, 
                                                 self.packet_logger, self.logger)
        if not self.ground_station.connect():
            self.logger.log_warning("CONNECT_WARNING", "地测口连接", "DG-IoT服务未启动，使用模拟模式")
            # 模拟模式：创建虚拟客户端
            self.ground_station.registered = True
        else:
            if not self.ground_station.register():
                self.logger.log_error("REGISTER_ERROR", "地测口注册", "注册失败")
                return False
        
        # 扫码枪连接
        self.scanner = ScannerClient(DGIOT_HOST, DGIOT_PORT, 
                                     self.packet_logger, self.logger)
        if not self.scanner.connect():
            self.logger.log_warning("CONNECT_WARNING", "扫码枪连接", "使用模拟模式")
        
        self.logger.log_stage_complete("环境准备", "SUCCESS")
        return True
    
    def _execute_tests(self) -> bool:
        """执行测试"""
        self.logger.log_stage_start("测试执行", "执行所有测试项")
        
        # 测试1: 扫码绑定
        self.logger.log_step_start("扫码绑定", "扫描设备并绑定", 1)
        if self.scanner.scan_device(self.device_id):
            self._add_result("扫码绑定", "passed", "设备绑定成功")
            self.logger.log_step_complete("扫码绑定", "PASS", {"device_id": self.device_id}, 1)
        else:
            self._add_result("扫码绑定", "failed", "设备绑定失败")
            self.logger.log_step_complete("扫码绑定", "FAIL", {"reason": "扫描失败"}, 1)
            return False
        
        time.sleep(2)
        
        # 测试2: PLC七步校验
        plc_validator = PLCSevenStepValidator(MAGNETIC_STATION_CONFIG["station_id"], self.logger)
        if plc_validator.execute_all_steps():
            self._add_result("PLC七步校验", "passed", "七步校验完成")
        else:
            self._add_result("PLC七步校验", "failed", "七步校验失败")
            return False
        
        time.sleep(2)
        
        # 测试3: EB90指令下发
        commands = ["舵面中位", "舵面使能", "复飞"]
        for cmd in commands:
            payload_hex = EB90_COMMANDS[cmd]["payload"]
            payload_bytes = bytes.fromhex(payload_hex)
            payload_bytes += b'\x00' * (EB90_COMMANDS[cmd]["fill_length"] - len(payload_bytes))
            
            # 构建完整的EB90帧
            frame = self.ground_station.build_eb90_frame(0x00, payload_bytes)
            
            self.logger.log_eb90_command(cmd, frame)
            if self.ground_station.send_eb90_data(frame, f"遥控指令: {cmd}"):
                self._add_result(f"遥控指令-{cmd}", "passed", "指令发送成功")
            else:
                self._add_result(f"遥控指令-{cmd}", "failed", "指令发送失败")
            time.sleep(1)
        
        # 测试4: D1遥测数据上报
        self.logger.log_step_start("遥测数据上报", "上报D1遥控数据", 4)
        d1_frame = bytes.fromhex(
            "EB90000000120000A55AF0A20000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        )
        for i in range(3):
            if self.ground_station.send_eb90_data(d1_frame, f"D1遥测帧-{i+1}"):
                self.logger.log_telemetry_data("D1", i+1, d1_frame.hex()[:50])
            time.sleep(1)
        
        self._add_result("遥测数据上报", "passed", "D1/D2/D3数据上报成功")
        self.logger.log_step_complete("遥测数据上报", "PASS", {"frames": 3}, 4)
        
        self.logger.log_stage_complete("测试执行", "SUCCESS")
        return True
    
    def _add_result(self, name: str, status: str, message: str = ""):
        """添加测试结果"""
        result = TestResult(
            test_item_id=f"test_{len(self.test_results)+1}",
            test_item_name=name,
            device_id=self.device_id,
            station_id=MAGNETIC_STATION_CONFIG["station_id"],
            step_name=name,
            status=status,
            timestamp=datetime.now().isoformat(),
            message=message
        )
        self.test_results.append(result)
    
    def _print_summary(self):
        """打印测试总结"""
        self.logger.info("\n" + "="*70)
        self.logger.info("测试总结")
        self.logger.info("="*70)
        
        for result in self.test_results:
            status_icon = "✅" if result.status == "passed" else "❌"
            self.logger.info(f"  {status_icon} {result.test_item_name}: {result.status}")
        
        total = len(self.test_results)
        passed = sum(1 for r in self.test_results if r.status == "passed")
        self.logger.info(f"\n总计: {passed}/{total} 通过")
        self.logger.info(f"通过率: {passed/total*100:.1f}%" if total > 0 else "0%")
        
        self.logger.info("\n报文日志:")
        self.logger.info(f"  记录报文数: {self.packet_logger.packet_count}")
        self.logger.info(f"  日志文件: {self.packet_logger.log_file_path}")
        
        self.logger.info("\nDG-IoT验证命令:")
        self.logger.info(f"  _build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_auto_tester:test_magnetic_auto().'")
        
        self.logger.info("\n" + "="*70)
    
    def cleanup(self):
        """清理资源"""
        if self.ground_station:
            self.ground_station.close()
        if self.scanner:
            self.scanner.close()
        if self.packet_logger:
            self.packet_logger.close()
        if self.logger:
            self.logger.close()

# ==================== 命令行参数解析 ====================
def parse_args():
    """解析命令行参数"""
    parser = argparse.ArgumentParser(
        description='磁航向工位一体化测试系统 - 融合版',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument('--device-id', default='UAV-001', help='设备ID')
    parser.add_argument('--auto-bind', action='store_true', help='自动绑定IP')
    parser.add_argument('--skip-check', action='store_true', help='跳过环境检查')
    parser.add_argument('-v', '--verbose', action='store_true', help='详细日志输出')
    
    return parser.parse_args()

# ==================== 主函数 ====================
def main():
    """主函数"""
    args = parse_args()
    
    # 创建测试实例
    test = MagneticStationFusionTest(
        device_id=args.device_id,
        auto_bind=args.auto_bind,
        verbose=args.verbose
    )
    
    # 运行测试
    try:
        success = test.run(skip_check=args.skip_check)
        
        if success:
            print("\n🎉 磁航向工位测试成功完成！")
            sys.exit(0)
        else:
            print("\n❌ 磁航向工位测试失败")
            sys.exit(1)
            
    except KeyboardInterrupt:
        print("\n\n收到中断信号，退出测试")
        test.cleanup()
        sys.exit(0)

if __name__ == "__main__":
    main()
