#!/usr/bin/env python3
"""
Modbus RTU Client模拟器
模拟设备连接到Modbus RTU over TCP Server
支持三种注册方式：RegisterByIp, RegisterByPort, RegisterByRegular
"""
import socket
import time
import struct
import threading
import sys
import argparse

class ModbusRTUClientSimulator:
    def __init__(self, host='127.0.0.1', port=20000, device_id='wrj_dm-zqy', reg_type='RegisterByPort'):
        self.host = host
        self.port = port
        self.device_id = device_id
        self.reg_type = reg_type
        self.running = False
        self.thread = None
        
    def send_registration(self):
        """发送注册报文"""
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.connect((self.host, self.port))
                
                if self.reg_type == 'RegisterByPort':
                    # 端口注册：直接发送设备ID
                    s.sendall(self.device_id.encode())
                    print(f"[RTU Client] 发送注册报文: {self.device_id}")
                    
                elif self.reg_type == 'RegisterByRegular':
                    # 正则匹配注册：发送特定格式的报文
                    registration_data = f"6D-5G-8I-{self.device_id}"
                    s.sendall(registration_data.encode())
                    print(f"[RTU Client] 发送正则注册报文: {registration_data}")
                    
                elif self.reg_type == 'RegisterByIp':
                    ***REMOVED***地址注册：发送包含IP信息的报文
                    ip_info = f"IP:{self.host}:{self.port}:{self.device_id}"
                    s.sendall(ip_info.encode())
                    print(f"[RTU Client] 发送IP注册报文: {ip_info}")
                
                # 接收响应
                response = s.recv(1024)
                if response:
                    print(f"[RTU Client] 收到响应: {response.decode()}")
                    
        except Exception as e:
            print(f"[RTU Client] 注册失败: {e}")
            
    def send_modbus_data(self, slave_id=1, function_code=3, data=b'\x00\x00\x00\x00'):
        """发送Modbus RTU数据"""
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.connect((self.host, self.port))
                
                # 构建Modbus RTU帧
                frame = struct.pack('BB', slave_id, function_code) + data
                # 添加CRC（简化示例）
                frame += b'\xC4\x0B'
                
                s.sendall(frame)
                print(f"[RTU Client] 发送Modbus数据: {frame.hex()}")
                
                # 接收响应
                response = s.recv(1024)
                if response:
                    print(f"[RTU Client] 收到Modbus响应: {response.hex()}")
                    
        except Exception as e:
            print(f"[RTU Client] 发送数据失败: {e}")
            
    def start_periodic_reporting(self, interval=5):
        """启动周期性数据上报"""
        self.running = True
        
        def reporting_loop():
            report_count = 0
            while self.running:
                try:
                    # 模拟温度数据（0-100度）
                    temperature = 20 + (report_count % 80)
                    # 模拟湿度数据（30-80%）
                    humidity = 50 + (report_count % 30)
                    
                    # 构建Modbus数据（4个寄存器：温度、湿度）
                    data = struct.pack('>HH', temperature * 10, humidity * 10) + b'\x00\x00'
                    
                    self.send_modbus_data(slave_id=1, function_code=3, data=data)
                    report_count += 1
                    
                    time.sleep(interval)
                    
                except Exception as e:
                    print(f"[RTU Client] 周期性上报错误: {e}")
                    time.sleep(1)
        
        self.thread = threading.Thread(target=reporting_loop)
        self.thread.daemon = True
        self.thread.start()
        print(f"[RTU Client] 启动周期性上报，间隔: {interval}秒")
        
    def stop(self):
        """停止模拟器"""
        self.running = False
        if self.thread:
            self.thread.join(timeout=2)
        print("[RTU Client] 模拟器已停止")

def main():
    parser = argparse.ArgumentParser(description='Modbus RTU Client模拟器')
    parser.add_argument('--host', default='127.0.0.1', help='服务器地址')
    parser.add_argument('--port', type=int, default=20000, help='服务器端口')
    parser.add_argument('--device', default='wrj_dm-zqy', help='设备ID')
    parser.add_argument('--regtype', default='RegisterByPort', 
                       choices=['RegisterByIp', 'RegisterByPort', 'RegisterByRegular'],
                       help='注册类型')
    parser.add_argument('--interval', type=int, default=5, help='上报间隔（秒）')
    
    args = parser.parse_args()
    
    simulator = ModbusRTUClientSimulator(
        host=args.host,
        port=args.port,
        device_id=args.device,
        reg_type=args.regtype
    )
    
    try:
        print(f"=== Modbus RTU Client模拟器 ===")
        print(f"服务器: {args.host}:{args.port}")
        print(f"设备ID: {args.device}")
        print(f"注册类型: {args.regtype}")
        print(f"上报间隔: {args.interval}秒")
        print("=" * 40)
        
        # 发送注册报文
        simulator.send_registration()
        time.sleep(1)
        
        # 启动周期性上报
        simulator.start_periodic_reporting(args.interval)
        
        # 保持运行
        while True:
            time.sleep(1)
            
    except KeyboardInterrupt:
        print("\n收到停止信号...")
        simulator.stop()
        print("模拟器已退出")

if __name__ == '__main__':
    main()
