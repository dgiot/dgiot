#!/usr/bin/env python3
"""
UDP Multicast Core Module
统一的多播通信核心模块，整合了多个脚本的通用功能

功能包括：
1. 网络接口管理（从udp_multicast_receiver.py迁移）
2. 多播通信基础（从udp_multicast_sender.py迁移）
3. 协议处理工具（CRC计算、数据格式化）
4. 配置管理（多播地址、端口配置）

使用方式：
    from multicast_core import MulticastCore, get_network_interfaces
    
    # 创建核心实例
    core = MulticastCore()
    
    # 获取网络接口
    interfaces = core.get_network_interfaces()
    
    # 发送多播数据
    core.send_multicast("226.0.0.80", 8001, b"test data")
"""

import socket
import struct
import time
import netifaces
import sys
from datetime import datetime

class MulticastCore:
    """多播通信核心类"""
    
    def __init__(self, default_group="226.0.0.80", default_port=8001):
        """
        初始化多播核心
        
        Args:
            default_group: 默认多播组地址
            default_port: 默认端口号
        """
        self.default_group = default_group
        self.default_port = default_port
        self.sockets = {}
        
    def get_network_interfaces(self):
        """
        获取系统网络接口信息
        
        Returns:
            list: 网络接口列表，每个元素为字典包含接口信息
        """
        interfaces = []
        
        try:
            # 使用netifaces获取接口信息
            for iface in netifaces.interfaces():
                try:
                    # 跳过回环接口
                    if iface == 'lo' or iface.startswith('lo'):
                        continue
                        
                    # 获取IPv4地址
                    addrs = netifaces.ifaddresses(iface)
                    if netifaces.AF_INET in addrs:
                        ipv4_info = addrs[netifaces.AF_INET][0]
                        interface_info = {
                            'name': iface,
                            'ip': ipv4_info['addr'],
                            'netmask': ipv4_info.get('netmask', '255.255.255.0'),
                            'broadcast': ipv4_info.get('broadcast', '')
                        }
                        interfaces.append(interface_info)
                except Exception as e:
                    print(f"获取接口 {iface} 信息失败: {e}", file=sys.stderr)
                    continue
                    
        except ImportError:
            # netifaces未安装，使用备用方法
            print("netifaces模块未安装，使用备用方法获取接口信息", file=sys.stderr)
            interfaces = self._get_interfaces_fallback()
        
        return interfaces
    
    def _get_interfaces_fallback(self):
        """
        备用方法获取网络接口信息（不使用netifaces）
        
        Returns:
            list: 网络接口列表
        """
        interfaces = []
        
        try:
            # 尝试通过socket获取本地IP
            s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            s.connect(("8.8.8.8", 80))
            local_ip = s.getsockname()[0]
            s.close()
            
            interfaces.append({
                'name': 'primary',
                'ip': local_ip,
                'netmask': '255.255.255.0',
                'broadcast': ''
            })
        except Exception as e:
            print(f"备用方法获取接口信息失败: {e}", file=sys.stderr)
        
        return interfaces
    
    def send_multicast(self, group=None, port=None, message=None, ttl=4):
        """
        发送多播消息
        
        Args:
            group: 多播组地址，如 "224.0.0.1"
            port: 端口号
            message: 要发送的消息（字节串）
            ttl: 生存时间
        """
        if group is None:
            group = self.default_group
        if port is None:
            port = self.default_port
        if message is None:
            message = b"test message"
        
        try:
            # 创建UDP socket
            sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
            
            # 设置TTL
            sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, ttl)
            
            # 启用环回
            sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_LOOP, 1)
            
            # 发送消息
            sock.sendto(message, (group, port))
            
            print(f"✓ Sent multicast message to {group}:{port}, size: {len(message)} bytes")
            
            sock.close()
            return True
            
        except Exception as e:
            print(f"✗ Failed to send multicast: {e}", file=sys.stderr)
            return False
    
    def join_multicast_group(self, group=None, port=None, interface_ip=None, timeout=30, callback=None, running_flag=None):
        """
        加入多播组并接收消息

        Args:
            group: 多播组地址
            port: 端口号
            interface_ip: 具体的网络接口IP地址
            timeout: 接收超时时间（秒），如果为None则永不超时
            callback: 接收到数据时的回调函数，接收参数 (data, addr)
            running_flag: 可选的布尔标志，用于外部控制停止（传入一个列表或可变对象）
        """
        if group is None:
            group = self.default_group
        if port is None:
            port = self.default_port
        
        print(f"加入多播组 {group}:{port}")
        print(f"接口: {interface_ip if interface_ip else '自动选择'}")
        
        # 如果没有指定接口，自动选择一个
        if interface_ip is None:
            interfaces = self.get_network_interfaces()
            if interfaces:
                interface_ip = interfaces[0]['ip']
                print(f"自动选择接口: {interfaces[0]['name']} ({interface_ip})")
            else:
                interface_ip = '0.0.0.0'
                print(f"⚠️ 警告: 未找到可用接口，使用0.0.0.0")
        
        try:
            # 创建UDP socket
            sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
            
            # 允许地址重用
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            
            # 绑定到端口
            sock.bind(('', port))
            
            # 加入多播组
            mreq = struct.pack("4s4s", 
                              socket.inet_aton(group), 
                              socket.inet_aton(interface_ip))
            sock.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
            
            print(f"✓ 成功加入多播组 {group}:{port}，接口 {interface_ip}")
            print("按 Ctrl+C 停止接收")
            
            # 设置接收超时
            sock.settimeout(1.0)
            
            start_time = time.time()
            packet_count = 0
            
            while True:
                # 检查外部停止标志
                if running_flag is not None:
                    try:
                        if not running_flag[0]:
                            print("收到停止信号，退出接收循环")
                            break
                    except (IndexError, TypeError):
                        pass

                try:
                    # 接收数据
                    data, addr = sock.recvfrom(65535)
                    packet_count += 1
                    
                    print(f"\n[{datetime.now().strftime('%H:%M:%S.%f')[:-3]}] 收到数据包 #{packet_count}")
                    print(f"  来源: {addr[0]}:{addr[1]}")
                    print(f"  大小: {len(data)}字节")
                    print(f"  数据: {data.hex()[:40]}...")
                    
                    if callback:
                        callback(data, addr)
                    
                except socket.timeout:
                    # 检查超时
                    if timeout is not None and time.time() - start_time > timeout:
                        print(f"\n⏰ 接收超时，共收到 {packet_count} 个数据包")
                        break
                    continue
                    
                except KeyboardInterrupt:
                    print(f"\n🛑 用户中断，共收到 {packet_count} 个数据包")
                    break
                    
                except Exception as e:
                    print(f"\n❌ 接收错误: {e}")
                    break
            
            sock.close()
            print(f"✅ 接收结束，共收到 {packet_count} 个数据包")
            return packet_count
            
        except Exception as e:
            print(f"❌ 加入多播组失败: {e}", file=sys.stderr)
            return 0
    
    def calculate_crc16(self, data):
        """
        计算CRC16校验码（Modbus RTU算法）
        
        Args:
            data: 要计算CRC的数据（字节串）
        
        Returns:
            int: CRC16校验码
        """
        crc = 0xFFFF
        for byte in data:
            crc ^= byte
            for _ in range(8):
                if crc & 0x0001:
                    crc = (crc >> 1) ^ 0xA001
                else:
                    crc >>= 1
        return crc
    
    def create_eb90_packet(self, command, data=b"", sequence=1):
        """
        创建EB90协议数据包（简化版）
        
        Args:
            command: 命令字（整数）
            data: 数据部分（字节串）
            sequence: 序列号
        
        Returns:
            bytes: EB90协议数据包
        """
        # 同步字
        sync = b"\xEB\x90"
        
        # 长度 = 命令(1) + 序列号(4) + 数据长度 + CRC(2)
        length = 1 + 4 + len(data) + 2
        
        # 构建数据部分
        packet_data = struct.pack(">BI", command, sequence) + data
        
        # 计算CRC
        crc = self.calculate_crc16(packet_data)
        
        # 构建完整数据包
        packet = sync + struct.pack(">H", length) + packet_data + struct.pack(">H", crc)
        
        return packet
    
    def parse_eb90_packet(self, packet):
        """
        解析EB90协议数据包
        
        Args:
            packet: EB90协议数据包（字节串）
        
        Returns:
            dict: 解析后的数据字典，包含以下字段：
                - valid: 是否有效
                - sync: 同步字 (0xEB90)
                - length: 数据包长度
                - command: 命令字
                - sequence: 序列号
                - data: 数据部分
                - crc: CRC校验码
                - crc_valid: CRC校验是否通过
        """
        if len(packet) < 9:  # 最小长度：同步字(2) + 长度(2) + 命令(1) + 序列号(4)
            return {
                'valid': False,
                'error': '数据包长度不足'
            }
        
        try:
            # 解析同步字
            sync = struct.unpack(">H", packet[0:2])[0]
            if sync != 0xEB90:
                return {
                    'valid': False,
                    'error': f'无效的同步字: 0x{sync:04X}'
                }
            
            # 解析长度
            length = struct.unpack(">H", packet[2:4])[0]
            
            # 解析命令和序列号
            command, sequence = struct.unpack(">BI", packet[4:9])
            
            # 解析数据部分
            data_start = 9
            data_end = len(packet) - 2  # 减去CRC长度
            data = packet[data_start:data_end] if data_end > data_start else b""
            
            # 解析CRC
            crc = struct.unpack(">H", packet[-2:])[0]
            
            # 验证CRC
            packet_without_crc = packet[:-2]
            crc_calculated = self.calculate_crc16(packet_without_crc[4:])  # 从命令字开始计算
            crc_valid = (crc == crc_calculated)
            
            return {
                'valid': True,
                'sync': sync,
                'length': length,
                'command': command,
                'sequence': sequence,
                'data': data,
                'crc': crc,
                'crc_valid': crc_valid,
                'hex': packet.hex()
            }
            
        except Exception as e:
            return {
                'valid': False,
                'error': f'解析异常: {e}'
            }

def get_network_interfaces():
    """便捷函数：获取网络接口列表"""
    core = MulticastCore()
    return core.get_network_interfaces()

if __name__ == "__main__":
    # 简单测试
    core = MulticastCore()
    interfaces = core.get_network_interfaces()
    print("网络接口:", interfaces)
    
    # 测试CRC
    test_data = b"test"
    crc = core.calculate_crc16(test_data)
    print(f"CRC16 of {test_data}: {crc:04X}")
    
    # 测试EB90包
    packet = core.create_eb90_packet(0x9E, b"test", 123)
    parsed = core.parse_eb90_packet(packet)
    print("解析结果:", parsed)