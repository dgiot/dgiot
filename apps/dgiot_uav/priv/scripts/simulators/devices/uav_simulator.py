#!/usr/bin/env python3
"""
UAV 多播模拟器（整合版）
支持角色：无人机 (drone)、地面站 (ground_station)、手持控制器 (handheld)
依赖：multicast_core.py
使用 EB90 协议，可配置入网申请、遥测数据、遥控指令等。
修复：遥控帧长度改为66字节（协议标准），填充46字节。
"""

import argparse
import json
import random
import struct
import threading
import time
import yaml
from datetime import datetime
from typing import Dict, Any, Optional, Callable, List

# 导入多播核心模块
from multicast_core import MulticastCore

# ==================== 协议常量 ====================
MAGIC_WORD = 0xEB90
PLATFORM_REMOTE = 0x00          # 遥控帧
PLATFORM_TELEMETRY = 0x0E        # 遥测帧

# 命令字
CMD_NETWORK_ALLOW = 0x9E         # 允许入网
CMD_NETWORK_REJECT = 0x9F        # 拒绝入网

# 帧长度（字节）
TELEMETRY_FRAME_LEN = 128
REMOTE_FRAME_LEN = 66            # 遥控帧总长度（含同步头）
COMPOSITE_FRAME_LEN = 128

# 默认 D1 遥测模板（来自抓包）
D1_TEMPLATE = bytes.fromhex(
    "EB90000000121FDD0000007990A55A73015308D127B30E18561F144568088FFF710034090D01A7FE0000B8FD0000"
    "DA14B011FFFFF9FF000000E680005D0A07750400FD19190A180A2F0814B900E43400002C00000000000000000000"
    "FA01912C2D962B2D9330328B3439013100FE00000000000000000000000000000000000082ED"
)

# D2 遥测模板（来自用户提供的测试报文）
D2_TEMPLATE = bytes.fromhex(
    "EB90000000121FDD0000007990A55A73015308D200000028001DE84F00E30300000000010B00003600021A000032"
    "050546094D03864C00EEC800000000FFFFEDFFEEFF2BFC00000000000B4C045E0104000000000000000000A14800"
    "0000000000000000000000000000000000000000000000000000000000FFFF00F4F80506FC04FF00FF00007E3C"
)

# D3 遥测模板（来自用户提供的测试报文）
D3_TEMPLATE = bytes.fromhex(
    "EB90000000121FDD0000007990A55A73015308D3790700000000000000000000000394FDB202C4FA000000000012"
    "7929B8B10E180E21144507004500000B6B080000000000000000000000F5FFA705DE00DB00000000000000000000"
    "0000333154540002000000000000000000000C000000000000004C44010100000400010000CA11"
)

# 遥测类型映射
TELEMETRY_TYPES = {
    "D1": D1_TEMPLATE,
    "D2": D2_TEMPLATE,
    "D3": D3_TEMPLATE,
}

# ==================== 辅助函数 ====================
def crc16(data: bytes) -> int:
    """计算 CRC-16 (Modbus RTU 算法)"""
    crc = 0xFFFF
    for byte in data:
        crc ^= byte
        for _ in range(8):
            if crc & 0x0001:
                crc = (crc >> 1) ^ 0xA001
            else:
                crc >>= 1
    return crc

def hex_dump(data: bytes, max_len: int = 64) -> str:
    """生成十六进制转储（用于日志）"""
    hex_str = data.hex()
    if len(hex_str) > max_len * 2:
        return hex_str[:max_len*2] + "..."
    return hex_str

# ==================== 无人机类 ====================
class Drone:
    """无人机模拟器"""
    def __init__(self, drone_id: int, config: Dict[str, Any], core: MulticastCore,
                 telemetry_type: str = "D1"):
        self.drone_id = drone_id
        self.config = config
        self.core = core
        self.sequence = random.randint(1, 1000000)
        self.running = True  # 控制发送循环退出
        self.network_apply = config.get("network_apply", False)   # 是否发送入网申请
        self.network_approved = False                             # 是否已获得入网许可
        self.apply_address = config.get("apply_address", drone_id)
        self.data_template = config.get("data_template", D1_TEMPLATE)
        self.telemetry_port = config.get("telemetry_port", 8001)
        self.multicast_group = config.get("multicast_group", "226.0.0.80")
        self.sent_count = 0

        # 遥测类型设置
        self.telemetry_type = telemetry_type
        if telemetry_type == "all":
            # 轮流发送所有类型
            self.telemetry_templates = [
                ("D1", D1_TEMPLATE),
                ("D2", D2_TEMPLATE),
                ("D3", D3_TEMPLATE),
            ]
            self.current_type_index = 0
            self.data_template = self.telemetry_templates[0][1]
        else:
            # 固定类型
            self.data_template = TELEMETRY_TYPES.get(telemetry_type, D1_TEMPLATE)
            self.telemetry_templates = None

    def build_telemetry_frame(self, frame_no: int) -> bytes:
        """构建遥测帧（128字节）"""
        frame = bytearray(TELEMETRY_FRAME_LEN)

        # 如果使用模板，先复制模板内容（模板已包含完整128字节）
        if self.data_template:
            frame[:] = self.data_template[:TELEMETRY_FRAME_LEN]

        # 强制设置平台类型为遥测帧类型 0x0E
        frame[6] = PLATFORM_TELEMETRY

        # 覆盖可变字段
        # 源地址（2字节），填写飞机ID
        frame[4:6] = struct.pack('>H', self.drone_id)
        # 帧编号（1字节）
        frame[7] = frame_no & 0xFF
        # 密钥（3字节，备用）保留为0
        frame[8:11] = b'\x00\x00\x00'

        # 入网申请字段（71H～73H）
        if self.network_apply and not self.network_approved:
            frame[113] = 0xAA                              # 有效标志
            frame[114:116] = struct.pack('>H', self.apply_address)
        else:
            frame[113:116] = b'\x00\x00\x00'

        # 重新计算CRC
        # CRC1（计算02H～0AH）
        crc1_data = frame[2:11]     # 02H=2, 0AH=10
        crc1 = crc16(crc1_data)
        frame[11:13] = struct.pack('>H', crc1)

        # CRC2（7CH～7DH，计算26H～7BH）
        crc2_data = frame[38:124]   # 26H=38, 7BH=123
        crc2 = crc16(crc2_data)
        frame[124:126] = struct.pack('>H', crc2)

        # CRC3（7EH～7FH，计算0DH～7DH）
        crc3_data = frame[13:126]   # 0DH=13, 7DH=125
        crc3 = crc16(crc3_data)
        frame[126:128] = struct.pack('>H', crc3)

        return bytes(frame)

    def handle_remote_frame(self, data: bytes, addr: tuple):
        """处理收到的遥控帧（检查是否为入网响应）"""
        if len(data) < REMOTE_FRAME_LEN:
            return
        try:
            magic = struct.unpack('>H', data[0:2])[0]
            if magic != MAGIC_WORD:
                return
            if data[6] != PLATFORM_REMOTE:
                return
            # 解析遥调地址和数据（14～17字节）
            adjust_addr1 = data[14]
            adjust_addr2 = data[15]
            adjust_data = struct.unpack('>H', data[16:18])[0]

            if adjust_addr1 != adjust_addr2:
                return
            if adjust_addr1 == CMD_NETWORK_ALLOW and adjust_data == self.drone_id:
                print(f"[{datetime.now().strftime('%H:%M:%S.%f')[:-3]}] ✅ 无人机 0x{self.drone_id:04X} 收到入网许可")
                self.network_approved = True
            elif adjust_addr1 == CMD_NETWORK_REJECT and adjust_data == self.drone_id:
                print(f"[{datetime.now().strftime('%H:%M:%S.%f')[:-3]}] ❌ 无人机 0x{self.drone_id:04X} 收到入网拒绝")
                self.network_approved = True  # 停止申请
            else:
                # 普通遥控指令
                print(f"[{datetime.now().strftime('%H:%M:%S.%f')[:-3]}] 无人机 0x{self.drone_id:04X} 收到遥控指令: 命令码=0x{adjust_addr1:02X}")
        except Exception as e:
            print(f"解析遥控帧异常: {e}")

    def send_loop(self, interval_ms: int):
        """发送遥测数据的主循环"""
        frame_no = 0
        while self.running:
            # 如果是轮流发送，切换模板
            if self.telemetry_templates:
                type_name, template = self.telemetry_templates[self.current_type_index]
                self.data_template = template
                self.current_type_index = (self.current_type_index + 1) % len(self.telemetry_templates)

            packet = self.build_telemetry_frame(frame_no)
            self.core.send_multicast(self.multicast_group, self.telemetry_port, packet)

            status = "有" if (self.network_apply and not self.network_approved) else "无"
            type_str = self.telemetry_type if self.telemetry_type != "all" else type_name
            print(f"[{datetime.now().strftime('%H:%M:%S.%f')[:-3]}] 无人机 0x{self.drone_id:04X} 发送 {type_str} 帧 #{frame_no} (入网申请:{status})")

            self.sent_count += 1
            frame_no = (frame_no + 1) % 256
            time.sleep(interval_ms / 1000.0)

    def stop(self):
        """停止无人机发送循环"""
        self.running = False

# ==================== 地面站类 ====================
class GroundStation:
    """地面站模拟器"""
    def __init__(self, config: Dict[str, Any], core: MulticastCore):
        self.config = config
        self.core = core
        self.sequence = random.randint(1, 1000000)
        self.multicast_group = config.get("multicast_group", "226.0.0.80")
        self.composite_port = config.get("composite_port", 8000)
        self.remote_port = config.get("remote_port", 8002)   # 用于发送入网响应
        self.telemetry_port = config.get("telemetry_port", 8001)   # 添加遥测端口属性

    def build_composite_frame(self, frame_no: int) -> bytes:
        """构建复合数据帧（示例）"""
        frame = bytearray(COMPOSITE_FRAME_LEN)
        frame[0:2] = struct.pack('>H', MAGIC_WORD)
        frame[2:4] = b'\x00\x00'                       # 长度占位（实际可省略）
        frame[4] = 0x01                                 # 命令字（复合数据）
        frame[5:9] = struct.pack('>I', frame_no)
        frame[9:17] = struct.pack('>Q', int(time.time() * 1000))
        # 填充随机数据
        for i in range(17, COMPOSITE_FRAME_LEN-2):
            frame[i] = random.randint(0, 255)
        crc = crc16(frame[:-2])
        frame[-2:] = struct.pack('>H', crc)
        return bytes(frame)

    def build_remote_allow_frame(self, drone_id: int, sequence: int, allow: bool = True) -> bytes:
        """构建入网允许/拒绝遥控帧（66字节）"""
        cmd = CMD_NETWORK_ALLOW if allow else CMD_NETWORK_REJECT
        frame = bytearray(REMOTE_FRAME_LEN)
        frame[0:2] = struct.pack('>H', MAGIC_WORD)
        frame[2:4] = struct.pack('>H', drone_id)       # 目的地址
        frame[4:6] = b'\x00\x01'                       # 源地址（地面站固定为0x0001）
        frame[6] = PLATFORM_REMOTE
        frame[7] = sequence & 0xFF
        frame[8:11] = b'\x00\x00\x00'
        frame[11:14] = b'\x00\x00\x00'
        frame[14] = cmd
        frame[15] = cmd
        frame[16:18] = struct.pack('>H', drone_id)
        # 填充剩余字节（12H-3FH，共46字节）
        frame[18:64] = b'\x00' * 46
        crc = crc16(frame[2:64])   # 计算02H-3FH
        frame[64:66] = struct.pack('>H', crc)
        return bytes(frame)

    def handle_telemetry_frame(self, data: bytes, addr: tuple):
        """处理收到的遥测帧，检查入网申请"""
        if len(data) < TELEMETRY_FRAME_LEN:
            return
        try:
            magic = struct.unpack('>H', data[0:2])[0]
            if magic != MAGIC_WORD:
                return
            # 检查入网申请字段（113～115字节）
            if data[113] == 0xAA:                         # 有申请
                apply_addr = struct.unpack('>H', data[114:116])[0]
                print(f"[{datetime.now().strftime('%H:%M:%S.%f')[:-3]}] 地面站收到无人机 0x{apply_addr:04X} 的入网申请")
                # 自动允许入网（可配置为随机拒绝）
                seq = self.sequence
                self.sequence += 1
                response = self.build_remote_allow_frame(apply_addr, seq, allow=True)
                self.core.send_multicast(self.multicast_group, self.remote_port, response)
                print(f"  发送入网许可到 0x{apply_addr:04X}")
        except Exception as e:
            print(f"处理遥测帧异常: {e}")

    def send_loop(self, interval_ms: int):
        """发送复合数据的主循环"""
        frame_no = 0
        while True:
            packet = self.build_composite_frame(frame_no)
            self.core.send_multicast(self.multicast_group, self.composite_port, packet)
            print(f"[{datetime.now().strftime('%H:%M:%S.%f')[:-3]}] 地面站发送复合数据帧 #{frame_no}")
            frame_no += 1
            time.sleep(interval_ms / 1000.0)

# ==================== 手持控制器类 ====================
class HandheldController:
    """手持控制器模拟器"""
    def __init__(self, config: Dict[str, Any], core: MulticastCore):
        self.config = config
        self.core = core
        self.sequence = random.randint(1, 1000000)
        self.multicast_group = config.get("multicast_group", "226.0.0.80")
        self.remote_port = config.get("remote_port", 8002)
        self.drone_id = config.get("target_drone_id", 0x0853)   # 目标无人机ID

    def build_remote_frame(self, command: int, sequence: int, target_addr: Optional[int] = None) -> bytes:
        """构建遥控帧（66字节）"""
        if target_addr is None:
            target_addr = self.drone_id
        frame = bytearray(REMOTE_FRAME_LEN)
        frame[0:2] = struct.pack('>H', MAGIC_WORD)
        frame[2:4] = struct.pack('>H', target_addr)    # 目的地址
        frame[4:6] = b'\x00\x02'                        # 源地址（手持控制器固定为0x0002）
        frame[6] = PLATFORM_REMOTE
        frame[7] = sequence & 0xFF
        frame[8:11] = b'\x00\x00\x00'
        frame[11:14] = b'\x00\x00\x00'
        frame[14] = command
        frame[15] = command
        frame[16:18] = struct.pack('>H', target_addr)   # 数据字段
        # 填充剩余字节（12H-3FH，共46字节）
        frame[18:64] = b'\x00' * 46
        crc = crc16(frame[2:64])   # 计算02H-3FH
        frame[64:66] = struct.pack('>H', crc)
        return bytes(frame)

    def send_loop(self, interval_ms: int):
        """发送遥控指令的主循环"""
        commands = [0x01, 0x02, 0x03, 0x04, 0x05]        # 起飞、降落、悬停等
        frame_no = 0
        while True:
            cmd = random.choice(commands)
            packet = self.build_remote_frame(cmd, frame_no)
            self.core.send_multicast(self.multicast_group, self.remote_port, packet)
            print(f"[{datetime.now().strftime('%H:%M:%S.%f')[:-3]}] 手持控制器发送遥控指令 0x{cmd:02X} 到 0x{self.drone_id:04X}")
            frame_no = (frame_no + 1) % 256
            time.sleep(interval_ms / 1000.0)

    def send_network_response(self, drone_id: int, allow: bool = True):
        """发送入网许可/拒绝遥控帧（主动调用）"""
        cmd = CMD_NETWORK_ALLOW if allow else CMD_NETWORK_REJECT
        seq = self.sequence
        self.sequence += 1
        packet = self.build_remote_frame(cmd, seq, drone_id)
        self.core.send_multicast(self.multicast_group, self.remote_port, packet)
        print(f"[{datetime.now().strftime('%H:%M:%S.%f')[:-3]}] 手持控制器发送入网{'许可' if allow else '拒绝'}到 0x{drone_id:04X}")

# ==================== 主模拟器类 ====================
class UAVSimulator:
    """无人机系统模拟器主类"""
    def __init__(self, role: str, config: Dict[str, Any]):
        self.role = role
        self.config = config
        self.core = MulticastCore()
        self.running = False
        self.threads = []
        self.device = None

    def start(self):
        """启动模拟器"""
        self.running = True
        interval_ms = self.config.get("interval_ms", 40)

        # 根据角色创建对应的设备对象
        if self.role == "drone":
            drone_id = self.config.get("drone_id", 0x0853)
            telemetry_type = self.config.get("telemetry_type", "D1")
            self.device = Drone(drone_id, self.config, self.core, telemetry_type)
            # 启动接收线程（监听遥控响应）
            def recv_callback(data, addr):
                if self.device:
                    self.device.handle_remote_frame(data, addr)
            recv_thread = threading.Thread(
                target=self.core.join_multicast_group,
                kwargs={
                    "group": self.config.get("multicast_group", "226.0.0.80"),
                    "port": self.config.get("remote_port", 8002),
                    "callback": recv_callback,
                    "timeout": None
                },
                daemon=True
            )
            recv_thread.start()
            self.threads.append(recv_thread)
            # 启动发送线程
            send_thread = threading.Thread(target=self.device.send_loop, args=(interval_ms,), daemon=True)
            send_thread.start()
            self.threads.append(send_thread)

        elif self.role == "ground_station":
            self.device = GroundStation(self.config, self.core)
            # 启动接收线程（监听遥测，处理入网申请）
            def recv_callback(data, addr):
                if self.device:
                    self.device.handle_telemetry_frame(data, addr)
            recv_thread = threading.Thread(
                target=self.core.join_multicast_group,
                kwargs={
                    "group": self.config.get("multicast_group", "226.0.0.80"),
                    "port": self.device.telemetry_port,
                    "callback": recv_callback,
                    "timeout": None
                },
                daemon=True
            )
            recv_thread.start()
            self.threads.append(recv_thread)
            # 启动发送线程（复合数据）
            send_thread = threading.Thread(target=self.device.send_loop, args=(interval_ms,), daemon=True)
            send_thread.start()
            self.threads.append(send_thread)

        elif self.role == "handheld":
            self.device = HandheldController(self.config, self.core)
            send_thread = threading.Thread(target=self.device.send_loop, args=(interval_ms,), daemon=True)
            send_thread.start()
            self.threads.append(send_thread)

        else:
            raise ValueError(f"未知角色: {self.role}")

        print(f"[{self.role}] 模拟器已启动，按 Ctrl+C 停止")
        try:
            while self.running:
                time.sleep(1)
        except KeyboardInterrupt:
            self.stop()

    def stop(self):
        self.running = False
        print(f"[{self.role}] 模拟器停止")

# ==================== 配置加载 ====================
def load_config(config_file: str) -> Dict[str, Any]:
    """从 JSON 或 YAML 文件加载配置"""
    with open(config_file, 'r') as f:
        if config_file.endswith('.yaml') or config_file.endswith('.yml'):
            return yaml.safe_load(f)
        else:
            return json.load(f)

# ==================== 主函数 ====================
def main():
    parser = argparse.ArgumentParser(description="UAV 多播模拟器（整合版）")
    parser.add_argument("--role", choices=["drone", "ground_station", "handheld"], required=True,
                        help="模拟角色")
    parser.add_argument("--config", type=str, default="simulation_config.yaml",
                        help="配置文件路径（YAML/JSON）")
    parser.add_argument("--drone-id", type=lambda x: int(x, 0), default=0x0853,
                        help="无人机ID（十六进制或十进制）")
    parser.add_argument("--interval-ms", type=int, default=40,
                        help="发送间隔（毫秒）")
    parser.add_argument("--network-apply", action="store_true",
                        help="无人机是否发送入网申请")
    parser.add_argument("--auto-response", action="store_true",
                        help="地面站/手持控制器是否自动响应入网申请")
    parser.add_argument("--telemetry-type", type=str, default="D1",
                        choices=["D1", "D2", "D3", "all"],
                        help="无人机遥测帧类型（D1/D2/D3/all），默认为D1")
    args = parser.parse_args()

    # 加载配置文件
    try:
        config = load_config(args.config)
    except FileNotFoundError:
        config = {}
        print(f"配置文件 {args.config} 未找到，使用默认配置")

    # 合并命令行参数
    config["role"] = args.role
    config["drone_id"] = args.drone_id
    config["interval_ms"] = args.interval_ms
    config["network_apply"] = args.network_apply
    config["auto_response"] = args.auto_response
    if args.role == "drone":
        config["telemetry_type"] = args.telemetry_type

    # 启动模拟器
    sim = UAVSimulator(args.role, config)
    sim.start()

if __name__ == "__main__":
    main()