#!/usr/bin/env python3
"""
无人机多设备客户端模拟器 - 最终版（增加扫描枪二维码模拟）
集成舵面、单片机、地测口、扫描枪、噪音传感器
新增功能：连接成功后主动发送注册报文（设备ID），完成一次注册。
扫描枪每隔10秒发送一条二维码数据（示例格式：123|12|336699|1|2001022||asdc|2356）
"""

import socket
import threading
import time
import logging
import struct
import subprocess
import re
import sys
import argparse
import random
import os

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# ==================== 配置区域 ====================
FIXTURE_IPS = [
    "192.168.100.45",
    "192.168.100.46",
    "192.168.100.47",
    "192.168.100.48",
    "192.168.100.49",
    "192.168.100.50",
    "192.168.100.51",
    "192.168.100.52",
]

MAGNETIC_IP = "192.168.100.21"

NOISE_SENSORS = [
    {"ip": "192.168.100.35", "addr": 1, "id": "noise_sensor"},
    {"ip": "192.168.100.36", "addr": 2, "id": "noise_sensor"},
]

SCANNER_IP = "192.168.100.23"  # 扫码枪固定IP

FIXTURE_PORT = 10006          # 单片机
DICEKOU_PORT = 10007          # 地测口
SENSOR_PORTS = [10001, 10002, 10003, 10004, 10005]  # 舵面
MAGNETIC_DICEKOU_PORT = 10007
SCANNER_PORT = 1234
NOISE_PORT = 21000

NOISE_REGISTER_COUNT = 256
NOISE_DATA_MULTIPLIER = 10

SENSOR_DEVICES = [
    ('wrj_dm_zqy', '左前翼舵面'),
    ('wrj_dm_yqy', '右前翼舵面'),
    ('wrj_dm_zcw', '左侧翼舵面'),
    ('wrj_dm_ycw', '右侧翼舵面'),
    ('wrj_dm_zhj', '治具基准舵面'),
]

STATION_ADDR_LIST = [1, 2, 3, 4, 5, 6, 7, 255]

STATION_NAME_MAP = {
    0: "磁航向工位",
    1: "总测工位2",
    2: "总测工位2-动力检测",
    3: "总测工位1",
    4: "总测工位1-动力检测",
    5: "拷机工位2",
    6: "拷机工位1",
    7: "桁行架",
    255: "上料台",
    1700: "磁航向工位(PLC地址)",
}

# 治具绑定相关寄存器（用于二次注册）
FIXTURE_BINDING_REGISTERS = {
    'BIND_STATUS': 0x000E,
    'BIND_COMMAND': 0x000F,
    'BIND_STATION_ID': 0x0010,
    'BIND_TIMESTAMP': 0x0011,
    'BIND_TIMESTAMP_LOW': 0x0012,
    'BIND_VERIFICATION': 0x0013,
}

FIXTURE_BINDING_STATES = {0: 'UNBOUND', 1: 'BOUND', 2: 'BINDING', 3: 'BIND_FAILED'}
BIND_COMMANDS = {0: 'NO_OP', 1: 'START_BIND', 2: 'CONFIRM_BIND', 3: 'CANCEL_BIND'}

DEFAULT_REG_VALUES = {
    0x00: 0x0000, 0x01: 0x0000, 0x04: 0x0002, 0x1A: 0x0050,
    0x1F: 0x0004, 0x20: 0x0003, 0x21: 0x0000, 0x23: 0x0000,
    0x24: 0x0000, 0x25: 0x001E, 0x2A: 0x01F4, 0x69: 0x0000,
    0x5A: 0x0000, 0x5B: 0x0000, 0x5E: 0x0000, 0x5F: 0x0000,
}

REGISTER_COUNT = 0x85

HARDCODED_FRAMES_HEX = [
    "09190b032008033efff400060800000000000000000000000000000020003ea87f0c0f00000000000000000000000000000000000000000000000000000000003d05006e00018f7d00000000000000000000000000000000000000436500000000",
    "09190b03290800aafff3000707fa00000000000000000000000000000020003da87f0c0c00000000000000000000000000000000000000000000000000000000003d05006c00008f7d000000000000000000000000e095ffffff9f6f5f088737",
    "09190b0329080186fff30007080000000000000000000000000000000020003da87f0c0900000000000000000000000000000000000000000000000000000000003d05006d00f8febfffbf6f7f2000000000000000436500000000f14a",
    "09190b0329080244fff400060804000000000000000000000000000021003da87f0c0f00000000000000000000000000000000000000000000000000000000003d0500fde5ffff9f7f5f100000000000000000000000436500000000756c",
]

def create_devices(no_bind=False):
    devices = []
    ip_to_addr = {ip: STATION_ADDR_LIST[i] for i, ip in enumerate(FIXTURE_IPS)}

    for idx, ip in enumerate(FIXTURE_IPS):
        station_addr = ip_to_addr[ip]
        src_ip = ip if not no_bind else None

        for i, (dev_id, dev_type) in enumerate(SENSOR_DEVICES):
            port = SENSOR_PORTS[i]
            devices.append({
                'id': dev_id,
                'port': port,
                'type': f'{dev_type}_IP{ip}',
                'protocol': 'modbus_rtu',
                'send_data': False,
                'sensor_addr': station_addr,
                'src_ip': src_ip
            })

        port = FIXTURE_PORT
        devices.append({
            'id': 'wrj_danpianji',
            'port': port,
            'type': f'单片机_IP{ip}',
            'protocol': 'modbus_rtu',
            'send_data': False,
            'sensor_addr': station_addr,
            'src_ip': src_ip
        })

        port = DICEKOU_PORT
        devices.append({
            'id': 'wrj_dicekou',
            'port': port,
            'type': f'地测口_IP{ip}',
            'protocol': 'eb90',
            'send_data': True,
            'sensor_addr': station_addr,
            'src_ip': src_ip
        })

    src_ip = MAGNETIC_IP if not no_bind else None
    devices.append({
        'id': 'wrj_dicekou',
        'port': MAGNETIC_DICEKOU_PORT,
        'type': '磁航向地测口',
        'protocol': 'eb90',
        'send_data': True,
        'sensor_addr': 0,
        'src_ip': src_ip
    })
    
    # 扫码枪使用固定IP 192.168.100.23
    scanner_src_ip = SCANNER_IP if not no_bind else None
    devices.append({
        'id': 'scanner',
        'port': SCANNER_PORT,
        'type': '扫描枪',
        'protocol': 'unknown',
        'send_data': False,
        'sensor_addr': 0,
        'src_ip': scanner_src_ip
    })

    for noise in NOISE_SENSORS:
        src_ip = noise['ip'] if not no_bind else None
        devices.append({
            'id': noise['id'],
            'port': NOISE_PORT,
            'type': '噪音传感器',
            'protocol': 'modbus_rtu',
            'send_data': False,
            'sensor_addr': noise['addr'],
            'src_ip': src_ip
        })

    return devices

# ==================== 网络诊断与IP绑定函数 ====================
def diagnose_network(target_ip, target_port):
    """执行网络诊断：显示本地IP、路由、ping、TCP端口测试"""
    logger.info("===== 开始网络诊断 =====")
    try:
        output = subprocess.check_output(["ip", "-4", "addr", "show"], universal_newlines=True)
        logger.info("本地IPv4地址:")
        for line in output.splitlines():
            if "inet " in line:
                logger.info(f"  {line.strip()}")
    except Exception as e:
        logger.error(f"获取IP信息失败: {e}")

    try:
        output = subprocess.check_output(["ip", "route", "get", target_ip], universal_newlines=True)
        logger.info(f"路由到 {target_ip}: {output.strip()}")
    except Exception as e:
        logger.error(f"路由查询失败: {e}")

    try:
        output = subprocess.check_output(["ping", "-c", "2", "-W", "2", target_ip],
                                          universal_newlines=True, stderr=subprocess.STDOUT)
        logger.info(f"Ping {target_ip}:")
        for line in output.splitlines():
            logger.info(f"  {line}")
    except Exception as e:
        logger.error(f"Ping失败: {e}")

    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(3)
        sock.connect((target_ip, target_port))
        sock.close()
        logger.info(f"TCP端口 {target_ip}:{target_port} 开放，连接成功")
    except Exception as e:
        logger.error(f"TCP端口连接失败: {e}")

    logger.info("===== 诊断完成 =====")

def get_default_interface():
    """获取默认网络接口名（用于添加IP）"""
    try:
        output = subprocess.check_output(["ip", "route", "show", "default"], universal_newlines=True)
        match = re.search(r'dev\s+(\S+)', output)
        return match.group(1) if match else None
    except Exception as e:
        logger.error(f"获取默认接口失败: {e}")
        return None

def ip_exists(ip):
    """检查指定IP是否已存在于系统中"""
    try:
        output = subprocess.check_output(["ip", "addr", "show"], universal_newlines=True)
        return ip in output
    except Exception as e:
        logger.error(f"检查IP {ip} 时出错: {e}")
        return False

def add_ip(ip, interface):
    """向指定接口添加IP地址（需要root权限）"""
    try:
        subprocess.check_call(["ip", "addr", "add", f"{ip}/24", "dev", interface],
                               stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        logger.info(f"已添加IP {ip} 到 {interface}")
        return True
    except Exception as e:
        logger.error(f"添加IP {ip} 失败: {e}")
        return False

def ensure_ips(auto_bind=False):
    """确保所有需要的IP地址都存在，若auto_bind为真则自动添加缺失的IP"""
    all_ips = FIXTURE_IPS + [MAGNETIC_IP] + [s['ip'] for s in NOISE_SENSORS] + [SCANNER_IP]
    interface = get_default_interface()
    if not interface:
        logger.error("无法确定网络接口，请手动添加IP")
        return False

    all_ok = True
    for ip in all_ips:
        if ip_exists(ip):
            logger.debug(f"IP {ip} 已存在")
        else:
            if auto_bind:
                logger.info(f"IP {ip} 不存在，尝试添加...")
                if add_ip(ip, interface):
                    # 验证是否添加成功
                    if not ip_exists(ip):
                        logger.error(f"添加后仍不存在: {ip}")
                        all_ok = False
                else:
                    all_ok = False
            else:
                logger.error(f"IP {ip} 未绑定，请先添加或使用 --auto-bind")
                all_ok = False
    return all_ok

# EB90报文
RAW_D1 = "EB 90 00 00 00 12 1F DD 00 00 00 79 90 A5 5A 73 01 53 08 D1 27 B3 0E 18 56 1F 14 45 68 08 8F FF 71 00 34 09 0D 01 A7 FE 00 00 B8 FD 00 00 DA 14 B0 11 FF FF F9 FF 00 00 00 E6 80 00 5D 0A 07 75 04 00 FD 19 19 0A 18 0A 2F 08 14 B9 00 E4 34 00 00 2C 00 00 00 00 00 00 00 00 00 00 FA 01 91 2C 2D 96 2B 2D 93 30 32 8B 34 39 01 31 00 FE 00 00 00 00 00 00 00 00 03 00 00 00 00 00 00 00 82 ED"
RAW_D2 = "EB 90 00 00 00 12 1F DD 00 00 00 79 90 A5 5A 73 01 53 08 D2 00 00 00 28 00 1D E8 4F 00 E3 03 00 00 00 00 01 0B 00 00 36 00 02 1A 00 00 32 05 05 46 09 4D 03 86 4C 00 EE C8 00 00 00 00 FF FF ED FF EE FF 2B FC 00 00 00 00 00 0B 4C 04 5E 01 04 00 00 00 00 00 00 00 00 00 A1 48 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 FF FF 00 F4 F8 05 06 FC 04 FF 00 FF 00 00 7E 3C"
RAW_D3 = "EB 90 00 00 00 12 1F DD 00 00 00 79 90 A5 5A 73 01 53 08 D3 79 07 00 00 00 00 00 00 00 00 00 00 00 03 94 FD B2 02 C4 FA 00 00 00 00 00 12 79 29 B8 B1 0E 18 0E 21 14 45 07 00 45 00 00 0B 6B 08 00 00 00 00 00 00 00 00 00 00 F5 FF A7 05 DE 00 DB 00 00 00 00 00 00 00 00 00 00 00 33 31 54 54 00 02 00 00 00 00 00 00 00 00 00 00 0C 00 00 00 00 00 00 4C 44 01 01 00 00 04 00 01 00 00 CA 11"

def clean_hex(hex_str): return ''.join(hex_str.split())
def hex_to_bin(hex_str): return bytes.fromhex(clean_hex(hex_str))

D1_BIN = hex_to_bin(RAW_D1)
D2_BIN = hex_to_bin(RAW_D2)
D3_BIN = hex_to_bin(RAW_D3)
REAL_PACKETS = [D1_BIN, D2_BIN, D3_BIN]

# EB90 帧类型映射
EB90_FRAME_TYPES = {
    0x0: "遥控帧",
    0x1: "飞控遥测D1",
    0xE: "链路遥测",
    0xF: "扩展数据",
}

def parse_eb90_header(packet: bytes) -> dict:
    """解析EB90帧头部，返回结构化信息"""
    info = {"raw_hex": packet.hex().upper(), "length": len(packet)}
    if len(packet) < 10:
        info["error"] = "帧太短"
        return info
    sync = (packet[0] << 8) | packet[1]
    if sync != 0xEB90:
        info["error"] = f"同步头错误: 0x{sync:04X}"
        return info
    info["sync"] = "EB90"
    info["dst"] = f"0x{(packet[2] << 8) | packet[3]:04X}"
    info["src"] = f"0x{(packet[4] << 8) | packet[5]:04X}"
    info["platform_type"] = f"0x{packet[6]:02X}"
    pt = packet[6] & 0x0F
    info["frame_type"] = EB90_FRAME_TYPES.get(pt, f"未知(0x{pt:X})")
    info["frame_no"] = packet[7]
    # 检测子帧类型（payload第一个字节）
    if len(packet) > 14:
        # EB90 + dst(2) + src(2) + platform(1) + frame_no(1) = 8字节头
        # 然后是 A5 5A 密钥(2字节) + 子类型(1字节)
        if packet[8] == 0xA5 and packet[9] == 0x5A:
            sub = packet[10] if len(packet) > 10 else 0
            sub_names = {0xD1: "D1遥控", 0xD2: "D2传感器", 0xD3: "D3飞行"}
            info["sub_frame"] = sub_names.get(sub, f"0x{sub:02X}")
    return info

def calculate_crc16(data: bytes) -> bytes:
    crc = 0xFFFF
    for b in data:
        crc ^= b
        for _ in range(8):
            if crc & 1:
                crc = (crc >> 1) ^ 0xA001
            else:
                crc >>= 1
    return struct.pack('<H', crc)

class DeviceClient(threading.Thread):
    def __init__(self, dgiot_host, dgiot_port, device_info, no_bind):
        super().__init__()
        self.dgiot_host = dgiot_host
        self.dgiot_port = dgiot_port
        self.device_id = device_info['id']
        self.device_type = device_info['type']
        self.protocol = device_info['protocol']
        self.local_port = device_info['port']
        self.port = device_info['port']  # 添加port属性，用于日志
        self.sensor_addr = device_info['sensor_addr']
        self.send_data = device_info.get('send_data', False)
        self.src_ip = device_info.get('src_ip', None)
        self.no_bind = no_bind
        self.running = True
        self.sock = None
        self.daemon = True
        self.packet_index = 0

        if self.protocol == 'modbus_rtu':
            self.registers = [0] * REGISTER_COUNT
            self.coils = {}
            self.unlocked = False
            self.lock = threading.RLock()
            self.is_surface = self.device_id in [d[0] for d in SENSOR_DEVICES]
            self.is_noise_sensor = self.device_id in ['noise_sensor1', 'noise_sensor2']
            self.angle_offset = {0x3D: 0, 0x3E: 0, 0x3F: 0}

            if self.is_surface:
                self.modbus_addr = 0x50
            elif self.is_noise_sensor:
                self.modbus_addr = self.sensor_addr
            else:
                self.modbus_addr = 0x02

            if self.is_surface:
                self.frames = []
                self.frame_index = 0
                self.load_surface_frames()
                self.init_registers()
                self.time_thread = threading.Thread(target=self.time_update_loop, daemon=True)
                self.time_thread.start()
                self.frame_thread = threading.Thread(target=self.frame_update_loop, daemon=True)
                self.frame_thread.start()
            elif self.is_noise_sensor:
                self.init_noise_sensor_registers()
            else:
                self.init_mcu_registers()
        else:
            self.is_surface = False
            self.is_noise_sensor = False

        # 扫描枪专用：二维码发送间隔（秒）和上次发送时间
        self.scanner_interval = 2  # 缩短为2秒以便调试
        self.last_scanner_send = 0

    def load_surface_frames(self):
        frames = []
        for hex_str in HARDCODED_FRAMES_HEX:
            try:
                data = bytes.fromhex(hex_str)
                if len(data) != 96:
                    continue
                regs = []
                for j in range(0, 96, 2):
                    val = (data[j] << 8) | data[j+1]
                    regs.append(val)
                frames.append(regs)
            except Exception:
                continue
        if not frames:
            frames = self.generate_mock_frames(10)
        logger.debug(f"设备 {self.device_id} 加载了 {len(frames)} 个数据帧")
        self.frames = frames

    def generate_mock_frames(self, count=10):
        frames = []
        for _ in range(count):
            regs = []
            for j in range(48):
                if j < 3:
                    val = random.randint(-32768, 32767) & 0xFFFF
                elif j < 6:
                    val = random.randint(-32768, 32767) & 0xFFFF
                elif j < 9:
                    val = random.randint(-32768, 32767) & 0xFFFF
                elif j < 10:
                    val = random.randint(2000, 3000)
                else:
                    val = random.randint(0, 65535)
                regs.append(val)
            frames.append(regs)
        return frames

    def init_registers(self):
        for addr, val in DEFAULT_REG_VALUES.items():
            if addr < REGISTER_COUNT:
                self.registers[addr] = val
        if self.frames:
            frame = self.frames[0]
            for i, val in enumerate(frame):
                addr = 0x34 + i
                if addr <= 0x5F:
                    self.registers[addr] = val
        self.update_time_registers()

    def init_mcu_registers(self):
        if len(self.registers) < 0x0014:
            self.registers = [0] * 0x0014
        if 0x000D < len(self.registers):
            self.registers[0x000D] = self.sensor_addr & 0xFFFF
        self.registers[0x0000] = (123456789 >> 16) & 0xFFFF
        self.registers[0x0001] = 123456789 & 0xFFFF
        self.registers[0x0002] = 0
        self.registers[0x0003] = 4
        self.registers[0x0004] = 0
        self.registers[0x0005] = 4500
        self.registers[0x0006] = (987654 >> 16) & 0xFFFF
        self.registers[0x0007] = 987654 & 0xFFFF
        self.registers[0x0008] = 0
        self.registers[0x0009] = 456
        self.registers[0x000A] = 5200
        self.registers[0x000B] = 24300

        for reg in FIXTURE_BINDING_REGISTERS.values():
            if reg < len(self.registers):
                self.registers[reg] = 0

        self.coils = {0x0000: False, 0x0001: False, 0x0002: False}
        self.binding_state = 0
        self.binding_station_id = 0
        self.binding_timestamp = 0
        self.binding_verification = 0
        station_name = STATION_NAME_MAP.get(self.sensor_addr, "未知工位")
        logger.info(f"单片机 {self.device_id} 初始化完成，工位地址={self.sensor_addr} ({station_name})")

    def init_noise_sensor_registers(self):
        self.registers = [0] * NOISE_REGISTER_COUNT
        noise_db = random.uniform(45.0, 65.0)
        self.registers[0x00] = int(noise_db * NOISE_DATA_MULTIPLIER)
        self.registers[0x64] = 0x0001
        self.registers[0x65] = 1
        self.registers[0x66] = self.modbus_addr
        self.registers[0x67] = 3
        self.registers[0x68] = 1
        self.registers[0x69] = 1
        self.registers[0x6B] = 0
        logger.info(f"噪音传感器 {self.device_id} 初始化完成，从站地址={self.modbus_addr:02X}")
        self.noise_thread = threading.Thread(target=self.noise_update_loop, daemon=True)
        self.noise_thread.start()

    def noise_update_loop(self):
        while self.running:
            time.sleep(1)
            noise_db = random.uniform(45.0, 65.0)
            with self.lock:
                self.registers[0x00] = int(noise_db * NOISE_DATA_MULTIPLIER)

    def update_time_registers(self):
        now = time.localtime()
        ms = int((time.time() - int(time.time())) * 1000)
        with self.lock:
            self.registers[0x30] = ((now.tm_year % 100) << 8) | now.tm_mon
            self.registers[0x31] = (now.tm_hour << 8) | now.tm_mday
            self.registers[0x32] = (now.tm_sec << 8) | now.tm_min
            self.registers[0x33] = ms

    def time_update_loop(self):
        while self.running:
            self.update_time_registers()
            time.sleep(0.05)

    def frame_update_loop(self):
        last_switch = time.time()
        while self.running:
            now = time.time()
            if now - last_switch >= 0.2:
                with self.lock:
                    if self.frames:
                        self.frame_index = (self.frame_index + 1) % len(self.frames)
                        frame = self.frames[self.frame_index]
                        for i, val in enumerate(frame):
                            addr = 0x34 + i
                            if addr <= 0x5F:
                                self.registers[addr] = val
                last_switch = now
            time.sleep(0.01)

    def connect(self, max_retries: int = 3) -> bool:
        """连接DGIOT服务器，支持自动重试"""
        for attempt in range(1, max_retries + 1):
            try:
                self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                # 设置SO_LINGER为0，关闭时立即释放端口（避免TIME_WAIT）
                linger = struct.pack('ii', 1, 0)
                self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_LINGER, linger)
                # 设置连接超时，避免阻塞无法响应stop
                self.sock.settimeout(2.0)
                # 始终绑定本地端口（客户端端口必须是固定的）
                bind_ip = self.src_ip if self.src_ip else "0.0.0.0"
                self.sock.bind((bind_ip, self.local_port))
                self.sock.connect((self.dgiot_host, self.dgiot_port))
                actual_ip, actual_port = self.sock.getsockname()
                logger.info(f"{self.device_id} 从 {actual_ip}:{actual_port} 连接成功"
                            f"{' (重试第' + str(attempt) + '次)' if attempt > 1 else ''}")
                return True
            except OSError as e:
                if attempt < max_retries:
                    logger.warning(f"{self.device_id} 连接失败(第{attempt}次): {e}, {1}s后重试...")
                    time.sleep(1)
                else:
                    logger.error(f"{self.device_id} 连接失败(已重试{max_retries}次): {e}")
            except Exception as e:
                logger.error(f"{self.device_id} 连接异常: {e}")
                return False
        return False

    def send_eb90_packet(self):
        packet = REAL_PACKETS[self.packet_index % len(REAL_PACKETS)]
        self.packet_index += 1
        # 解析并打印EB90报文详情
        info = parse_eb90_header(packet)
        logger.info(f"[报文-发送] EB90 {info['frame_type']} | {info['src']}->{info['dst']} | "
                    f"子帧={info.get('sub_frame', 'N/A')} | 帧号={info['frame_no']} | "
                    f"长度={info['length']}B | 索引=#{self.packet_index}")
        return packet

    # ---------- 一次注册：连接成功后发送设备ID ----------
    def send_registration_packet(self):
        """发送一次注册报文（设备ID + 换行）"""
        if self.is_surface or self.device_id in ['wrj_danpianji', 'wrj_dicekou', 'scanner'] or self.is_noise_sensor:
            reg_data = self.device_id.encode() + b'\n'
        else:
            return
        try:
            self.sock.send(reg_data)
            logger.info(f"{self.device_id} 发送一次注册报文: {reg_data!r}")
        except Exception as e:
            logger.error(f"{self.device_id} 发送注册报文失败: {e}")
    
    # ---------- 周期性注册：治具单片机和地测口定期发送设备ID ----------
    def periodic_registration(self, interval=60):
        """周期性发送注册报文（治具单片机和地测口每60秒发送一次）"""
        # 只有治具单片机和地测口需要周期性发送注册报文
        if self.device_id not in ['wrj_danpianji', 'wrj_dicekou']:
            return
        reg_data = self.device_id.encode() + b'\n'
        try:
            self.sock.send(reg_data)
            logger.info(f"{self.device_id} 周期性发送注册报文（间隔{interval}秒）: {reg_data!r}")
        except Exception as e:
            logger.error(f"{self.device_id} 周期性发送注册报文失败: {e}")

    def handle_modbus_request(self, data: bytes) -> bytes | None:
        if len(data) < 8:
            return None
        slave_id = data[0]
        func = data[1]

        if self.device_id == 'wrj_danpianji':
            allowed = [self.modbus_addr, 0x02]
            if slave_id not in allowed:
                logger.debug(f"单片机忽略地址 {slave_id:02X}")
                return None
        else:
            if slave_id != self.modbus_addr:
                logger.debug(f"设备 {self.device_id} 忽略地址 {slave_id:02X}")
                return None

        if func == 0x03:  # 读寄存器
            start_addr = struct.unpack('>H', data[2:4])[0]
            reg_count = struct.unpack('>H', data[4:6])[0]
            logger.info(f"[报文-治具] Modbus读请求 | {self.device_id} | "
                        f"从站=0x{slave_id:02X} 功能码=0x03 | "
                        f"起始地址=0x{start_addr:04X} 数量={reg_count} | "
                        f"原始帧={data.hex().upper()}")
            time.sleep(0.01)
            with self.lock:
                if start_addr + reg_count > len(self.registers):
                    return None
                values = self.registers[start_addr:start_addr+reg_count]
            # 角度偏移处理（舵面）
            if self.is_surface:
                for i, addr in enumerate(range(start_addr, start_addr+reg_count)):
                    if addr in self.angle_offset:
                        raw = values[i]
                        off = self.angle_offset[addr]
                        adj = raw - off
                        if adj < -32768:
                            adj += 65536
                        elif adj > 32767:
                            adj -= 65536
                        values[i] = adj & 0xFFFF
            resp = bytearray([slave_id, func, reg_count*2])
            for v in values:
                resp.extend(struct.pack('>H', v))
            resp.extend(calculate_crc16(resp))
            val_str = ','.join(f'0x{v:04X}' for v in values[:8])
            if len(values) > 8:
                val_str += f',...共{len(values)}个'
            logger.info(f"[报文-治具] Modbus读响应 | {self.device_id} | "
                        f"从站=0x{slave_id:02X} 功能码=0x03 | "
                        f"数据=[{val_str}] | "
                        f"响应帧={bytes(resp).hex().upper()}")
            return bytes(resp)

        elif func == 0x05:  # 写单个线圈
            addr = struct.unpack('>H', data[2:4])[0]
            val = struct.unpack('>H', data[4:6])[0]
            state = (val == 0xFF00)
            logger.info(f"[报文-治具] Modbus写线圈 | {self.device_id} | "
                        f"从站=0x{slave_id:02X} 功能码=0x05 | "
                        f"地址=0x{addr:04X} 值={'ON' if state else 'OFF'} | "
                        f"原始帧={data.hex().upper()}")
            with self.lock:
                self.coils[addr] = state
            resp = bytearray(data[:-2])
            resp.extend(calculate_crc16(resp))
            return bytes(resp)

        elif func == 0x06:  # 写单个寄存器
            addr = struct.unpack('>H', data[2:4])[0]
            val = struct.unpack('>H', data[4:6])[0]
            logger.info(f"[报文-治具] Modbus写寄存器 | {self.device_id} | "
                        f"从站=0x{slave_id:02X} 功能码=0x06 | "
                        f"地址=0x{addr:04X} 值=0x{val:04X} | "
                        f"原始帧={data.hex().upper()}")
            with self.lock:
                if addr < len(self.registers):
                    # 特殊功能：解锁、校准、绑定等
                    if addr == 0x69 and val == 0x8588:
                        self.unlocked = True
                        logger.info(f"{self.device_id} 解锁成功")
                    elif addr == 0x01 and self.unlocked:
                        cal_mode = val & 0x0F
                        if cal_mode == 0x08 and self.is_surface:
                            for a in self.angle_offset:
                                self.angle_offset[a] = self.registers[a]
                            logger.info(f"{self.device_id} 执行角度参考")
                    elif addr == 0x00 and self.unlocked:
                        if val == 0x0000:
                            logger.info(f"{self.device_id} 保存配置")
                        elif val == 0x0001:
                            if self.is_surface:
                                self.angle_offset = {k:0 for k in self.angle_offset}
                                self.unlocked = False
                                self.init_registers()
                            logger.info(f"{self.device_id} 恢复出厂")
                    elif self.is_noise_sensor and addr == 0x66:
                        self.modbus_addr = val & 0xFF
                        logger.info(f"噪音传感器地址改为 0x{self.modbus_addr:02X}")
                    elif addr == FIXTURE_BINDING_REGISTERS['BIND_COMMAND']:
                        self.handle_binding_command(val)
                    self.registers[addr] = val
            resp = bytearray(data[:-2])
            resp.extend(calculate_crc16(resp))
            return bytes(resp)

        logger.warning(f"不支持的功能码 0x{func:02X}")
        return None

    def handle_binding_command(self, cmd):
        if cmd == 0:
            return
        name = BIND_COMMANDS.get(cmd, f"未知({cmd})")
        logger.info(f"单片机 {self.device_id} 收到绑定命令: {name}")
        with self.lock:
            if cmd == 1:  # 开始绑定
                if self.binding_state == 0:
                    self.binding_state = 2
                    self.registers[FIXTURE_BINDING_REGISTERS['BIND_STATUS']] = 2
                    ts = int(time.time())
                    self.binding_timestamp = ts
                    self.binding_verification = (ts ^ hash(self.device_id)) & 0xFFFF
                    self.registers[FIXTURE_BINDING_REGISTERS['BIND_TIMESTAMP']] = (ts >> 16) & 0xFFFF
                    self.registers[FIXTURE_BINDING_REGISTERS['BIND_TIMESTAMP_LOW']] = ts & 0xFFFF
                    self.registers[FIXTURE_BINDING_REGISTERS['BIND_VERIFICATION']] = self.binding_verification
                    logger.info(f"单片机 {self.device_id} 开始绑定，验证码: 0x{self.binding_verification:04X}")
                else:
                    logger.warning(f"状态 {self.binding_state} 不能开始绑定")
            elif cmd == 2:  # 确认绑定
                if self.binding_state == 2:
                    station_id = self.registers[FIXTURE_BINDING_REGISTERS['BIND_STATION_ID']]
                    if station_id > 0 and station_id in STATION_NAME_MAP:
                        verif = self.registers[FIXTURE_BINDING_REGISTERS['BIND_VERIFICATION']]
                        if verif == self.binding_verification:
                            self.binding_state = 1
                            self.binding_station_id = station_id
                            self.registers[FIXTURE_BINDING_REGISTERS['BIND_STATUS']] = 1
                            self.registers[0x000D] = station_id
                            logger.info(f"单片机 {self.device_id} 绑定工位 {station_id} 成功")
                        else:
                            self.binding_state = 3
                            self.registers[FIXTURE_BINDING_REGISTERS['BIND_STATUS']] = 3
                            logger.error(f"验证码不匹配")
                    else:
                        self.binding_state = 3
                        self.registers[FIXTURE_BINDING_REGISTERS['BIND_STATUS']] = 3
                        logger.error(f"无效工位ID {station_id}")
                else:
                    logger.warning(f"状态 {self.binding_state} 不能确认绑定")
            elif cmd == 3:  # 取消绑定
                if self.binding_state == 2:
                    self.binding_state = 0
                    self.registers[FIXTURE_BINDING_REGISTERS['BIND_STATUS']] = 0
                    self.registers[FIXTURE_BINDING_REGISTERS['BIND_STATION_ID']] = 0
                    self.registers[FIXTURE_BINDING_REGISTERS['BIND_VERIFICATION']] = 0
                    logger.info(f"单片机 {self.device_id} 取消绑定")
            self.registers[FIXTURE_BINDING_REGISTERS['BIND_COMMAND']] = 0

    def run(self):
        try:
            while self.running:
                if self.connect():
                    break
                time.sleep(5)

            if not self.running:
                return

            # 发送一次注册报文
            self.send_registration_packet()

            self.sock.settimeout(1.0)
            interval = 5.0
            last_send = time.time()
            # 扫描枪专用上次发送时间
            self.last_scanner_send = time.time()
            # 注册报文周期性发送间隔（60秒）
            registration_interval = 60.0
            last_registration = time.time()

            while self.running:
                try:
                    data = self.sock.recv(1024)
                    if data:
                        if self.protocol == 'modbus_rtu':
                            # Modbus RTU 详细报文日志
                            if len(data) >= 2:
                                fc = data[1]
                                sid = data[0]
                                fc_names = {0x03: "读保持寄存器", 0x04: "读输入寄存器",
                                           0x05: "写单个线圈", 0x06: "写单个寄存器",
                                           0x10: "写多个寄存器", 0x0F: "写多个线圈"}
                                fc_name = fc_names.get(fc, f"未知(0x{fc:02X})")
                                logger.info(f"[报文-治具] Modbus请求 | {self.device_id} | "
                                            f"从站=0x{sid:02X} 功能码={fc_name} | "
                                            f"帧={data.hex().upper()} ({len(data)}B)")
                            resp = self.handle_modbus_request(data)
                            if resp:
                                self.sock.send(resp)
                        else:
                            logger.info(f"[报文-接收] {self.device_id} | "
                                        f"收到 {len(data)}B | HEX={data.hex().upper()}")
                except socket.timeout:
                    pass
                except Exception as e:
                    logger.error(f"[{self.src_ip}:{self.port}] {self.device_id} 异常: {e}")
                    break

                now = time.time()

                # EB90 设备周期性发送（地测口）
                if self.protocol == 'eb90' and self.send_data and now - last_send >= interval:
                    packet = self.send_eb90_packet()
                    try:
                        self.sock.send(packet)
                        logger.info(f"[报文-无人机] TCP发送 | {self.device_id} | "
                                    f"{self.src_ip}:{self.port} -> {self.dgiot_host}:{self.dgiot_port} | "
                                    f"{len(packet)}字节 | HEX={packet[:16].hex().upper()}...")
                    except Exception as e:
                        logger.error(f"[报文-无人机] TCP发送失败 | {self.device_id} | {e}")
                    last_send = now

                # 周期性发送注册报文（治具单片机和地测口）
                if self.device_id in ['wrj_danpianji', 'wrj_dicekou'] and now - last_registration >= registration_interval:
                    self.periodic_registration(registration_interval)
                    last_registration = now

                # 扫描枪周期性发送二维码
                if self.device_id == 'scanner' and now - self.last_scanner_send >= self.scanner_interval:
                    # 预定义的二维码示例数据（可自行扩展）
                    sample_qrcodes = [
                        "Test01|1|5000000020004|10|2026032502|||",  # 用户指定的测试数据
                        "123|12|336699|1|2001022||asdc|2356",
                        "456|34|778899|2|3002033||xyz|9876",
                        "789|56|112233|3|4003044||abc|1234",
                        "000|78|445566|4|5004055||def|5678",
                    ]
                    qrcode = random.choice(sample_qrcodes)
                    data = qrcode.encode() + b'\n'
                    try:
                        self.sock.send(data)
                        logger.info(f"[报文-扫描枪] 二维码发送 | scanner | "
                                    f"{self.src_ip}:{self.port} | 内容=\"{qrcode}\"")
                    except Exception as e:
                        logger.error(f"[报文-扫描枪] 二维码发送失败 | scanner | {e}")
                    self.last_scanner_send = now

                time.sleep(0.01)

        except Exception as e:
            logger.error(f"{self.device_id} 主线程异常: {e}")
        finally:
            if self.sock:
                self.sock.close()
            logger.info(f"{self.device_id} 结束")

    def stop(self):
        self.running = False
        if self.sock:
            try:
                self.sock.close()
            except:
                pass

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--dgiot-host', default='0.0.0.0', help='服务器地址')
    parser.add_argument('--dgiot-port', type=int, default=20000, help='服务器端口')
    parser.add_argument('--auto-bind', action='store_true', help='自动绑定缺失IP')
    parser.add_argument('--no-bind', action='store_true', help='不绑定源IP')
    parser.add_argument('--diagnose', action='store_true', help='诊断网络')
    args = parser.parse_args()

    if args.diagnose:
        diagnose_network(args.dgiot_host, args.dgiot_port)
        sys.exit(0)

    devices = create_devices(no_bind=args.no_bind)

    if not args.no_bind and not ensure_ips(auto_bind=args.auto_bind):
        sys.exit(1)

    threads = []
    for dev in devices:
        client = DeviceClient(args.dgiot_host, args.dgiot_port, dev, args.no_bind)
        client.start()
        threads.append(client)
        time.sleep(0.1)

    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        logger.info("停止")
        for t in threads:
            t.stop()
        for t in threads:
            t.join(2)

if __name__ == '__main__':
    main()