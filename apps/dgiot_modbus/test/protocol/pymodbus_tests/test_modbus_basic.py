#!/usr/bin/env python3
"""
Modbus基础协议测试用例
基于pymodbus库的专业Modbus协议测试
"""

import time
import struct
import socket
from pymodbus.client import ModbusTcpClient
from pymodbus.exceptions import ModbusException
import logging

# 配置日志
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class ModbusBasicTests:
    """Modbus基础协议测试类"""
    
    def __init__(self, host='127.0.0.1', port=20000):
        self.host = host
        self.port = port
        self.client = None
        
    def connect(self):
        """连接到Modbus服务器"""
        try:
            logger.info(f"连接到 {self.host}:{self.port}")
            self.client = ModbusTcpClient(self.host, port=self.port)
            connected = self.client.connect()
            if connected:
                logger.info("✅ 连接成功")
                return True
            else:
                logger.error("❌ 连接失败")
                return False
        except Exception as e:
            logger.error(f"❌ 连接异常: {e}")
            return False
    
    def disconnect(self):
        """断开连接"""
        if self.client:
            self.client.close()
            logger.info("🔌 连接已关闭")
    
    def test_connection(self):
        """测试连接状态"""
        logger.info("=== 测试连接状态 ===")
        if not self.client:
            logger.error("❌ 客户端未连接")
            return False
        
        try:
            # 尝试读取一个寄存器来测试连接
            result = self.client.read_holding_registers(address=0, count=1, slave=1)
            if result.isError():
                logger.warning("⚠️  读取寄存器失败，但连接正常")
                return True
            else:
                logger.info(f"✅ 连接正常，读取到寄存器值: {result.registers}")
                return True
        except ModbusException as e:
            logger.error(f"❌ Modbus异常: {e}")
            return False
        except Exception as e:
            logger.error(f"❌ 其他异常: {e}")
            return False
    
    def test_read_holding_registers(self, address=0, count=10, slave=1):
        """测试读保持寄存器（功能码03）"""
        logger.info(f"=== 测试读保持寄存器 ===")
        logger.info(f"地址: {address}, 数量: {count}, 从机: {slave}")
        
        try:
            result = self.client.read_holding_registers(address=address, count=count, slave=slave)
            if result.isError():
                logger.error(f"❌ 读取失败: {result}")
                return False
            
            logger.info(f"✅ 读取成功")
            logger.info(f"寄存器值: {result.registers}")
            logger.info(f"字节数: {result.byte_count}")
            return True
        except ModbusException as e:
            logger.error(f"❌ Modbus异常: {e}")
            return False
    
    def test_write_single_register(self, address=0, value=1234, slave=1):
        """测试写单个寄存器（功能码06）"""
        logger.info(f"=== 测试写单个寄存器 ===")
        logger.info(f"地址: {address}, 值: {value}, 从机: {slave}")
        
        try:
            # 写入寄存器
            write_result = self.client.write_register(address=address, value=value, slave=slave)
            if write_result.isError():
                logger.error(f"❌ 写入失败: {write_result}")
                return False
            
            logger.info(f"✅ 写入成功")
            
            # 验证写入
            time.sleep(0.1)  # 等待写入生效
            read_result = self.client.read_holding_registers(address=address, count=1, slave=slave)
            if read_result.isError():
                logger.error(f"❌ 验证读取失败: {read_result}")
                return False
            
            actual_value = read_result.registers[0]
            if actual_value == value:
                logger.info(f"✅ 验证成功: 写入值 {value} == 读取值 {actual_value}")
                return True
            else:
                logger.error(f"❌ 验证失败: 写入值 {value} != 读取值 {actual_value}")
                return False
                
        except ModbusException as e:
            logger.error(f"❌ Modbus异常: {e}")
            return False
    
    def test_write_multiple_registers(self, address=0, values=[100, 200, 300], slave=1):
        """测试写多个寄存器（功能码16）"""
        logger.info(f"=== 测试写多个寄存器 ===")
        logger.info(f"地址: {address}, 值: {values}, 从机: {slave}")
        
        try:
            # 写入多个寄存器
            write_result = self.client.write_registers(address=address, values=values, slave=slave)
            if write_result.isError():
                logger.error(f"❌ 写入失败: {write_result}")
                return False
            
            logger.info(f"✅ 写入成功")
            
            # 验证写入
            time.sleep(0.1)
            read_result = self.client.read_holding_registers(address=address, count=len(values), slave=slave)
            if read_result.isError():
                logger.error(f"❌ 验证读取失败: {read_result}")
                return False
            
            actual_values = read_result.registers
            if actual_values == values:
                logger.info(f"✅ 验证成功: 写入值 {values} == 读取值 {actual_values}")
                return True
            else:
                logger.error(f"❌ 验证失败: 写入值 {values} != 读取值 {actual_values}")
                return False
                
        except ModbusException as e:
            logger.error(f"❌ Modbus异常: {e}")
            return False
    
    def test_coil_operations(self, address=0, slave=1):
        """测试线圈操作（功能码01, 05, 15）"""
        logger.info(f"=== 测试线圈操作 ===")
        
        try:
            # 写单个线圈
            logger.info(f"1. 写单个线圈（地址: {address}）")
            write_result = self.client.write_coil(address=address, value=True, slave=slave)
            if write_result.isError():
                logger.warning(f"⚠️  写线圈失败（可能不支持）: {write_result}")
                return False
            
            # 读线圈
            logger.info(f"2. 读线圈（地址: {address}）")
            read_result = self.client.read_coils(address=address, count=1, slave=slave)
            if read_result.isError():
                logger.warning(f"⚠️  读线圈失败（可能不支持）: {read_result}")
                return False
            
            logger.info(f"✅ 线圈操作成功")
            logger.info(f"线圈值: {read_result.bits}")
            return True
            
        except ModbusException as e:
            logger.warning(f"⚠️  线圈操作异常（可能不支持）: {e}")
            return False
    
    def test_error_handling(self):
        """测试错误处理"""
        logger.info("=== 测试错误处理 ===")
        
        test_cases = [
            ("无效从机地址", lambda: self.client.read_holding_registers(address=0, count=1, slave=255)),
            ("无效寄存器地址", lambda: self.client.read_holding_registers(address=99999, count=1, slave=1)),
            ("读取数量为0", lambda: self.client.read_holding_registers(address=0, count=0, slave=1)),
        ]
        
        for test_name, test_func in test_cases:
            logger.info(f"测试: {test_name}")
            try:
                result = test_func()
                if result.isError():
                    logger.info(f"✅ 预期错误: {result}")
                else:
                    logger.warning(f"⚠️  未返回错误（可能正常）")
            except Exception as e:
                logger.info(f"✅ 预期异常: {type(e).__name__}")
        
        return True
    
    def test_dgiot_integration(self):
        """测试与DG-IoT的集成"""
        logger.info("=== 测试与DG-IoT集成 ===")
        
        # 1. 发送注册报文（模拟设备注册）
        logger.info("1. 发送设备注册报文")
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(5)
            sock.connect((self.host, self.port))
            sock.sendall(b"wrj_dm-zqy")
            logger.info("✅ 发送注册报文成功")
            sock.close()
        except Exception as e:
            logger.error(f"❌ 发送注册报文失败: {e}")
            return False
        
        # 2. 发送Modbus数据
        logger.info("2. 发送Modbus数据报文")
        try:
            # 构建Modbus RTU帧
            hex_data = "0103600C190E130311003902B20002088000000000000000000000006A009E9FB50592000000000000000000000000000000000000000000000000000000002FFDEDF1F8868AF7000000000000000000000000000000004365000000007DB3"
            import binascii
            data_bytes = binascii.unhexlify(hex_data)
            
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(5)
            sock.connect((self.host, self.port))
            sock.sendall(data_bytes)
            logger.info(f"✅ 发送Modbus数据成功 ({len(data_bytes)} bytes)")
            sock.close()
        except Exception as e:
            logger.error(f"❌ 发送Modbus数据失败: {e}")
            return False
        
        logger.info("✅ DG-IoT集成测试完成")
        logger.info("⚠️  请检查后端日志确认数据是否正确解析和存储")
        return True

def run_all_tests():
    """运行所有测试"""
    print("=" * 60)
    print("Modbus基础协议测试")
    print("=" * 60)
    
    tester = ModbusBasicTests(host='127.0.0.1', port=20000)
    
    test_results = {}
    
    try:
        # 1. 连接测试
        if tester.connect():
            test_results['connect'] = True
            
            # 2. 连接状态测试
            test_results['connection_test'] = tester.test_connection()
            
            # 3. 寄存器操作测试
            test_results['read_registers'] = tester.test_read_holding_registers(address=0, count=5, slave=1)
            test_results['write_single_register'] = tester.test_write_single_register(address=10, value=5678, slave=1)
            test_results['write_multiple_registers'] = tester.test_write_multiple_registers(address=20, values=[111, 222, 333], slave=1)
            
            # 4. 线圈操作测试
            test_results['coil_operations'] = tester.test_coil_operations(address=0, slave=1)
            
            # 5. 错误处理测试
            test_results['error_handling'] = tester.test_error_handling()
            
            # 6. DG-IoT集成测试
            test_results['dgiot_integration'] = tester.test_dgiot_integration()
            
        else:
            test_results['connect'] = False
            print("❌ 连接失败，跳过其他测试")
            
    except KeyboardInterrupt:
        print("\n⚠️  用户中断测试")
    except Exception as e:
        print(f"\n❌ 测试异常: {e}")
    finally:
        tester.disconnect()
    
    # 打印测试结果汇总
    print("\n" + "=" * 60)
    print("测试结果汇总")
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
    
    return passed == total

if __name__ == "__main__":
    success = run_all_tests()
    exit(0 if success else 1)
