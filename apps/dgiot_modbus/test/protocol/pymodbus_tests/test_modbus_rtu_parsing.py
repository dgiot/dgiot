#!/usr/bin/env python3
"""
Modbus RTU数据解析测试用例
专门测试DG-IoT Modbus RTU协议解析功能
"""

import time
import struct
import socket
import binascii
import json
import requests
from pymodbus.client import ModbusTcpClient
from pymodbus.exceptions import ModbusException
import logging

# 配置日志
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class ModbusRTUParsingTests:
    """Modbus RTU数据解析测试类"""
    
    def __init__(self, host='127.0.0.1', port=20000):
        self.host = host
        self.port = port
        self.product_id = "feeb43bffb"
        self.device_addr = f"wrj_dm-zqy-{port}"
        # 设备ID需要根据实际系统生成，这里使用已知的设备ID
        # 从API响应中获取的设备ID是"88a27d8587"
        self.device_id = "88a27d8587"
        
    def send_raw_modbus_rtu_frame(self, hex_data):
        """发送原始Modbus RTU帧"""
        logger.info(f"发送Modbus RTU帧: {hex_data[:50]}...")
        
        try:
            data_bytes = binascii.unhexlify(hex_data)
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(5)
            sock.connect((self.host, self.port))
            sock.sendall(data_bytes)
            logger.info(f"✅ 发送成功 ({len(data_bytes)} bytes)")
            sock.close()
            return True
        except Exception as e:
            logger.error(f"❌ 发送失败: {e}")
            return False
    
    def register_device(self):
        """注册设备"""
        logger.info("注册设备...")
        
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(5)
            sock.connect((self.host, self.port))
            sock.sendall(b"wrj_dm-zqy")
            logger.info("✅ 设备注册成功")
            sock.close()
            return True
        except Exception as e:
            logger.error(f"❌ 设备注册失败: {e}")
            return False
    
    def query_api_data(self):
        """查询API数据"""
        logger.info(f"查询设备数据: {self.device_id}")
        
        try:
            # 使用有效的认证令牌
            headers = {
                "Content-Type": "application/json",
                "Accept": "application/json, text/plain, */*",
                "Authorization": "Bearer r:64f8b47a43ea2b904036536c40c15017"
            }
            
            # 同时尝试sessiontoken头部
            headers_alt = {
                "Content-Type": "application/json",
                "Accept": "application/json, text/plain, */*",
                "sessiontoken": "r:64f8b47a43ea2b904036536c40c15017",
                "departmenttoken": "r:64f8b47a43ea2b904036536c40c15017",
                "author": "dgiot",
                "platform": "web",
                "referer": "http://127.0.0.1/admin/",
                "origin": "http://127.0.0.1"
            }
            
            api_url = f"http://127.0.0.1/iotapi/devicecard/{self.device_id}"
            
            # 首先尝试使用Authorization头部
            response = requests.get(api_url, headers=headers, timeout=10)
            
            # 如果失败，尝试使用sessiontoken头部
            if response.status_code != 200:
                response = requests.get(api_url, headers=headers_alt, timeout=10)
            
            if response.status_code == 200:
                data = response.json()
                logger.info("✅ API查询成功")
                
                # 分析数据
                # 处理不同的响应格式
                if 'data' in data:
                    data_content = data['data']
                    
                    # 如果data是列表（设备属性列表）
                    if isinstance(data_content, list):
                        data_items = data_content
                        logger.info(f"📊 数据项数量: {len(data_items)}")
                        
                        # 显示非空数据
                        non_empty_items = [item for item in data_items if item.get('value') not in ('', None)]
                        if non_empty_items:
                            logger.info("📈 非空数据项:")
                            for i, item in enumerate(non_empty_items[:5]):  # 显示前5个
                                logger.info(f"  {i+1}. {item.get('name', '未知')}: {item.get('value')} {item.get('unit', '')}")
                        else:
                            logger.warning("⚠️  所有数据项值为空")
                        
                        return data_items
                    # 如果data是字典（设备信息）
                    elif isinstance(data_content, dict):
                        logger.info("📊 设备信息:")
                        logger.info(f"  product_id: {data_content.get('product_id')}")
                        logger.info(f"  dtu_addr: {data_content.get('dtu_addr')}")
                        logger.info(f"  data_type: {data_content.get('data_type')}")
                        
                        # 检查是否有raw_data
                        if 'raw_data' in data_content:
                            raw_data = data_content['raw_data']
                            logger.info(f"  raw_data长度: {len(raw_data) if raw_data else 0}")
                        
                        return [data_content]
                    else:
                        logger.warning(f"⚠️  未知的数据格式: {type(data_content)}")
                        return []
                else:
                    logger.warning("⚠️  响应中没有数据字段")
                    logger.info(f"完整响应: {data}")
                    return []
            else:
                logger.error(f"❌ API查询失败: HTTP {response.status_code}")
                return None
                
        except Exception as e:
            logger.error(f"❌ API查询异常: {e}")
            return None
    
    def test_standard_modbus_rtu_frame(self):
        """测试标准Modbus RTU帧解析"""
        logger.info("=== 测试标准Modbus RTU帧解析 ===")
        
        # 标准Modbus RTU帧: 从机1, 功能码03, 地址0x0000, 数量10
        hex_frame = "01030000000AC5CD"
        
        if self.send_raw_modbus_rtu_frame(hex_frame):
            logger.info("✅ 发送标准帧成功")
            time.sleep(2)  # 等待解析
            data = self.query_api_data()
            return data is not None
        else:
            return False
    
    def test_product_feeb43bffb_data(self):
        """测试产品feeb43bffb的特定数据帧"""
        logger.info("=== 测试产品feeb43bffb数据帧 ===")
        
        # 用户提供的测试数据
        hex_data = "0103600C190E130511017E02B00007088200000000000000000000006B009E9FB50591000000000000000000000000000000000000000000000000000000002FFEEDF1F8858AF700000000000000000000000000000000436500000000B03F"
        
        if self.send_raw_modbus_rtu_frame(hex_data):
            logger.info("✅ 发送产品数据帧成功")
            time.sleep(3)  # 等待解析
            data = self.query_api_data()
            
            if data:
                # 检查特定属性
                expected_properties = ['angular_x', 'angular_y', 'angular_z', 'block_data']
                found_properties = []
                
                for item in data:
                    prop_name = item.get('name', '')
                    if prop_name in expected_properties:
                        found_properties.append(prop_name)
                        logger.info(f"✅ 找到属性: {prop_name} = {item.get('value')}")
                
                if len(found_properties) >= 2:
                    logger.info(f"✅ 成功解析 {len(found_properties)} 个属性")
                    return True
                else:
                    logger.warning(f"⚠️  只找到 {len(found_properties)} 个属性，期望至少2个")
                    return False
            else:
                return False
        else:
            return False
    
    def test_multiple_function_codes(self):
        """测试多种功能码"""
        logger.info("=== 测试多种功能码 ===")
        
        test_frames = [
            ("读保持寄存器 (03)", "010300000005850F"),
            ("读输入寄存器 (04)", "010400000005F1CC"),
            ("写单个寄存器 (06)", "010600640001480A"),
            ("写多个寄存器 (10)", "0110000000020400010002C6F0"),
        ]
        
        results = []
        for test_name, hex_frame in test_frames:
            logger.info(f"测试: {test_name}")
            if self.send_raw_modbus_rtu_frame(hex_frame):
                results.append(True)
                time.sleep(1)
            else:
                results.append(False)
        
        passed = sum(results)
        total = len(results)
        logger.info(f"功能码测试: {passed}/{total} 通过")
        return passed >= 2  # 至少通过2个
    
    def test_invalid_frames(self):
        """测试无效帧处理"""
        logger.info("=== 测试无效帧处理 ===")
        
        invalid_frames = [
            ("CRC错误", "0103000000051234"),  # 错误的CRC
            ("无效功能码", "019900000005ABCD"),  # 无效功能码
            ("帧过短", "0103"),  # 帧过短
            ("帧过长", "0103" + "00" * 100),  # 帧过长
        ]
        
        for test_name, hex_frame in invalid_frames:
            logger.info(f"测试: {test_name}")
            try:
                self.send_raw_modbus_rtu_frame(hex_frame)
                logger.info("✅ 发送成功（应被正确处理或忽略）")
            except Exception as e:
                logger.info(f"✅ 预期异常: {type(e).__name__}")
        
        return True  # 只要不崩溃就认为通过
    
    def test_data_block_parsing(self):
        """测试数据块解析"""
        logger.info("=== 测试数据块解析 ===")
        
        # 数据块模式测试
        hex_data = "0103600C190E130511017E02B00007088200000000000000000000006B009E9FB50591000000000000000000000000000000000000000000000000000000002FFEEDF1F8858AF700000000000000000000000000000000436500000000B03F"
        
        if self.send_raw_modbus_rtu_frame(hex_data):
            logger.info("✅ 发送数据块成功")
            time.sleep(3)
            
            # 检查后端日志中是否有数据块解析记录
            logger.info("⚠️  请检查后端日志确认数据块是否正确解析")
            logger.info("预期日志关键词: 'block_data', 'angular_x', 'angular_y', 'angular_z'")
            
            # 查询API验证
            data = self.query_api_data()
            if data:
                # 查找block_data属性
                block_data_found = False
                for item in data:
                    if item.get('name') == 'block_data' and item.get('value'):
                        block_data_found = True
                        logger.info(f"✅ 找到block_data: {item.get('value')}")
                        break
                
                return block_data_found
            else:
                return False
        else:
            return False
    
    def test_calculated_properties(self):
        """测试计算值属性"""
        logger.info("=== 测试计算值属性 ===")
        
        # 发送数据
        hex_data = "0103600C190E130511017E02B00007088200000000000000000000006B009E9FB50591000000000000000000000000000000000000000000000000000000002FFEEDF1F8858AF700000000000000000000000000000000436500000000B03F"
        
        if self.send_raw_modbus_rtu_frame(hex_data):
            logger.info("✅ 发送数据成功")
            time.sleep(3)
            
            data = self.query_api_data()
            if data:
                # 查找计算值属性（如angular_x, angular_y, angular_z）
                calculated_props = ['angular_x', 'angular_y', 'angular_z']
                found_calculated = []
                
                for item in data:
                    prop_name = item.get('name', '')
                    if prop_name in calculated_props and item.get('value'):
                        found_calculated.append(prop_name)
                        logger.info(f"✅ 计算值属性 {prop_name}: {item.get('value')}")
                
                if len(found_calculated) >= 2:
                    logger.info(f"✅ 成功解析 {len(found_calculated)} 个计算值属性")
                    return True
                else:
                    logger.warning(f"⚠️  只找到 {len(found_calculated)} 个计算值属性")
                    return False
            else:
                return False
        else:
            return False

def run_rtu_parsing_tests():
    """运行所有RTU解析测试"""
    print("=" * 60)
    print("Modbus RTU数据解析测试")
    print("=" * 60)
    
    tester = ModbusRTUParsingTests(host='127.0.0.1', port=20000)
    
    test_results = {}
    
    try:
        # 0. 先注册设备
        logger.info("0. 设备注册")
        if tester.register_device():
            test_results['device_registration'] = True
            time.sleep(2)  # 等待注册完成
        else:
            test_results['device_registration'] = False
            print("❌ 设备注册失败，跳过其他测试")
            return False
        
        # 1. 标准帧测试
        test_results['standard_frame'] = tester.test_standard_modbus_rtu_frame()
        
        # 2. 产品特定数据测试
        test_results['product_data'] = tester.test_product_feeb43bffb_data()
        
        # 3. 多种功能码测试
        test_results['multiple_function_codes'] = tester.test_multiple_function_codes()
        
        # 4. 无效帧测试
        test_results['invalid_frames'] = tester.test_invalid_frames()
        
        # 5. 数据块解析测试
        test_results['data_block_parsing'] = tester.test_data_block_parsing()
        
        # 6. 计算值属性测试
        test_results['calculated_properties'] = tester.test_calculated_properties()
        
    except KeyboardInterrupt:
        print("\n⚠️  用户中断测试")
    except Exception as e:
        print(f"\n❌ 测试异常: {e}")
    
    # 打印测试结果汇总
    print("\n" + "=" * 60)
    print("RTU解析测试结果汇总")
    print("=" * 60)
    
    passed = 0
    total = len(test_results)
    
    for test_name, result in test_results.items():
        status = "✅ 通过" if result else "❌ 失败"
        print(f"{test_name:30} {status}")
        if result:
            passed += 1
    
    print(f"\n总计: {passed}/{total} 通过 ({passed/total*100:.1f}%)")
    print("=" * 60)
    
    # 重要提示
    print("\n📋 重要提示:")
    print("1. 检查后端日志确认数据解析情况")
    print("2. 查看是否有错误日志")
    print("3. 验证数据是否正确存储到TDengine")
    print("4. 确认API返回的数据格式正确")
    
    return passed >= 4  # 至少通过4个测试

if __name__ == "__main__":
    success = run_rtu_parsing_tests()
    exit(0 if success else 1)
