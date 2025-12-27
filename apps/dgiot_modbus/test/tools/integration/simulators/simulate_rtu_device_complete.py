#!/usr/bin/env python3
"""
完整的RTU模拟器测试脚本
用于测试产品 feeb43bffb 的数据解析和API查询
"""

import socket
import time
import struct
import json
import requests
import sys
from datetime import datetime
import binascii

class RTUSimulator:
    def __init__(self, host='127.0.0.1', port=20000):
        self.host = host
        self.port = port
        self.socket = None
        self.product_id = "feeb43bffb"
        self.device_addr = None
        self.device_id = None
        
    def connect(self):
        """连接到RTU服务器"""
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(10)
            self.socket.connect((self.host, self.port))
            print(f"✅ 已连接到 {self.host}:{self.port}")
            return True
        except Exception as e:
            print(f"❌ 连接失败: {e}")
            return False
    
    def close(self):
        """关闭连接"""
        if self.socket:
            self.socket.close()
            print("🔌 连接已关闭")
    
    def send_registration(self):
        """发送注册报文"""
        # 注册报文: ASCII字符串 "wrj_dm-zqy"
        registration_data = b"wrj_dm-zqy"
        print(f"📤 发送注册报文: {registration_data.decode('ascii')}")
        
        try:
            self.socket.send(registration_data)
            
            # 等待响应（可能没有响应）
            try:
                response = self.socket.recv(1024)
                if response:
                    print(f"📥 收到响应: {response.hex()}")
                else:
                    print("📥 未收到响应")
            except socket.timeout:
                print("⏰ 接收超时（可能正常）")
                
            # 根据之前的代码，设备地址格式: 注册报文 + "-" + 端口号
            self.device_addr = f"wrj_dm-zqy-{self.port}"
            # 设备ID格式: dgiot_parse_id:get_deviceid(ProductId, DeviceAddr)
            self.device_id = f"{self.product_id}_{self.device_addr}"
            print(f"📝 生成的设备地址: {self.device_addr}")
            print(f"📝 生成的设备ID: {self.device_id}")
            return True
            
        except Exception as e:
            print(f"❌ 注册失败: {e}")
            return False
    
    def send_data_packet(self):
        """发送数据报文"""
        # 数据报文: HEX格式
        # 01 03 60 0C 19 0E 13 03 11 00 39 02 B2 00 02 08 80 00 00 00 00 00 00 00 00 00 00 00 00 00 6A 00 9E 9F B5 05 92 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 2F FD ED F1 F8 86 8A F7 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 43 65 00 00 00 00 7D B3
        
        hex_data = "0103600C190E130311003902B20002088000000000000000000000006A009E9FB50592000000000000000000000000000000000000000000000000000000002FFDEDF1F8868AF7000000000000000000000000000000004365000000007DB3"
        
        try:
            data_bytes = binascii.unhexlify(hex_data)
            print(f"📤 发送数据报文 ({len(data_bytes)} bytes): {hex_data[:50]}...")
            
            self.socket.send(data_bytes)
            
            # 等待响应（可能没有响应）
            try:
                response = self.socket.recv(1024)
                if response:
                    print(f"📥 收到响应: {response.hex()[:50]}...")
                else:
                    print("📥 未收到响应")
            except socket.timeout:
                print("⏰ 接收超时（可能正常）")
                
            return True
            
        except Exception as e:
            print(f"❌ 发送数据失败: {e}")
            return False
    
    def get_auth_cookies(self):
        """获取认证Cookie（基于用户提供的调试信息）"""
        # 基于用户提供的Cookie信息
        cookies = {
            "Admin-Token": "r:a1d8422a576e581c20fb91a01bc19ce6",
            "sessiontoken": "r:a1d8422a576e581c20fb91a01bc19ce6",
            "departmenttoken": "r:a1d8422a576e581c20fb91a01bc19ce6",
            "cna": "4292c8fec0554c6b8670a0a359e62809",
            "fileServer": "http://127.0.0.1",
            "handleRoute": "true",
            "expired_timestamp": "1766640009000",
            "Admin-Id": "4d867367b4",
            "Department-Id": "ccf5456562",
            "Department-Name": "%E5%BC%80%E5%8F%91%E8%80%85"
        }
        print("🔑 使用预配置的认证Cookie")
        return cookies
    
    def query_api_realtime(self, use_cookies=True):
        """查询API实时值"""
        if not self.device_id:
            print("❌ 设备ID未生成，请先注册")
            return False
            
        try:
            # 构建API URL
            api_url = f"http://127.0.0.1/iotapi/devicecard/{self.device_id}"
            
            print(f"🌐 查询API: {api_url}")
            
            # 准备请求
            if use_cookies:
                cookies = self.get_auth_cookies()
                response = requests.get(api_url, cookies=cookies, timeout=10)
            else:
                response = requests.get(api_url, timeout=10)
            
            if response.status_code == 200:
                data = response.json()
                print("✅ API查询成功")
                print(f"📊 响应数据: {json.dumps(data, indent=2, ensure_ascii=False)}")
                
                # 检查是否有实时数据
                if 'data' in data and isinstance(data['data'], list) and len(data['data']) > 0:
                    print(f"📈 找到 {len(data['data'])} 个数据点")
                    for i, item in enumerate(data['data'][:3]):  # 显示前3个
                        print(f"   {i+1}. {item.get('name', '未知')}: {item.get('value', '空')} {item.get('unit', '')}")
                else:
                    print("⚠️  响应中没有数据字段或数据为空")
                    
                return True
            else:
                print(f"❌ API查询失败: HTTP {response.status_code}")
                print(f"响应内容: {response.text}")
                return False
                
        except Exception as e:
            print(f"❌ API查询异常: {e}")
            return False
    
    def query_device_properties(self):
        """查询设备属性"""
        if not self.device_addr:
            print("❌ 设备地址未生成，请先注册")
            return False
            
        try:
            # 查询设备属性API
            api_url = f"http://127.0.0.1/iotapi/device_properties"
            params = {
                "productId": self.product_id,
                "deviceAddr": self.device_addr
            }
            
            print(f"🌐 查询设备属性API: {api_url}")
            print(f"📋 参数: {params}")
            
            response = requests.get(api_url, params=params, timeout=10)
            
            if response.status_code == 200:
                data = response.json()
                print("✅ 设备属性查询成功")
                print(f"📊 响应数据: {json.dumps(data, indent=2, ensure_ascii=False)}")
                return True
            else:
                print(f"❌ 设备属性查询失败: HTTP {response.status_code}")
                print(f"响应内容: {response.text}")
                return False
                
        except Exception as e:
            print(f"❌ 设备属性查询异常: {e}")
            return False

def main():
    """主函数"""
    print("=" * 60)
    print("RTU模拟器测试 - 产品 feeb43bffb")
    print("=" * 60)
    
    # 创建模拟器实例
    simulator = RTUSimulator(host='127.0.0.1', port=20000)
    
    try:
        # 1. 连接
        if not simulator.connect():
            return
        
        # 2. 发送注册报文
        print("\n" + "=" * 40)
        print("步骤1: 发送注册报文")
        print("=" * 40)
        if not simulator.send_registration():
            print("⚠️  注册失败，但继续测试...")
        
        # 等待一下，让服务器处理注册
        time.sleep(2)
        
        # 3. 发送数据报文
        print("\n" + "=" * 40)
        print("步骤2: 发送数据报文")
        print("=" * 40)
        if not simulator.send_data_packet():
            print("⚠️  发送数据失败")
        
        # 等待数据被处理
        print("\n⏳ 等待数据被处理...")
        time.sleep(3)
        
        # 4. 查询API实时值（带Cookie认证）
        print("\n" + "=" * 40)
        print("步骤3: 查询API实时值（带Cookie认证）")
        print("=" * 40)
        simulator.query_api_realtime(use_cookies=True)
        
        # 5. 查询设备属性
        print("\n" + "=" * 40)
        print("步骤4: 查询设备属性")
        print("=" * 40)
        simulator.query_device_properties()
        
        # 6. 再次发送数据（测试连续上报）
        print("\n" + "=" * 40)
        print("步骤5: 再次发送数据报文（测试连续上报）")
        print("=" * 40)
        simulator.send_data_packet()
        
        # 等待数据被处理
        time.sleep(2)
        
        # 7. 再次查询API（带Cookie认证）
        print("\n" + "=" * 40)
        print("步骤6: 再次查询API实时值（带Cookie认证）")
        print("=" * 40)
        simulator.query_api_realtime(use_cookies=True)
        
    except KeyboardInterrupt:
        print("\n\n⚠️  用户中断测试")
    except Exception as e:
        print(f"\n\n❌ 测试异常: {e}")
    finally:
        # 关闭连接
        simulator.close()
        
    print("\n" + "=" * 60)
    print("测试完成")
    print("=" * 60)

if __name__ == "__main__":
    main()
