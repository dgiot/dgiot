#!/usr/bin/env python3
"""
Modbus TCP Server模拟器
模拟Modbus TCP服务器，供Modbus TCP Client连接
"""
import socket
import struct
import threading
import time
import argparse

class ModbusTCPServerSimulator:
    def __init__(self, host='0.0.0.0', port=502):
        self.host = host
        self.port = port
        self.server_socket = None
        self.running = False
        self.threads = []
        
        # 模拟的寄存器数据
        self.registers = {
            'holding': {i: 0 for i in range(100)},  # 保持寄存器
            'input': {i: 0 for i in range(100)}     # 输入寄存器
        }
        
        # 初始化一些测试数据
        for i in range(10):
            self.registers['holding'][i] = i * 100
            self.registers['input'][i] = i * 10
            
    def start(self):
        """启动服务器"""
        self.server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        
        try:
            self.server_socket.bind((self.host, self.port))
            self.server_socket.listen(5)
            self.running = True
            
            print(f"[TCP Server] Modbus TCP服务器启动在 {self.host}:{self.port}")
            
            # 启动客户端处理线程
            accept_thread = threading.Thread(target=self._accept_clients)
            accept_thread.daemon = True
            accept_thread.start()
            self.threads.append(accept_thread)
            
            # 启动数据更新线程
            update_thread = threading.Thread(target=self._update_registers)
            update_thread.daemon = True
            update_thread.start()
            self.threads.append(update_thread)
            
            return True
            
        except Exception as e:
            print(f"[TCP Server] 启动失败: {e}")
            return False
            
    def _accept_clients(self):
        """接受客户端连接"""
        while self.running:
            try:
                client_socket, client_address = self.server_socket.accept()
                print(f"[TCP Server] 客户端连接: {client_address}")
                
                # 为每个客户端启动处理线程
                client_thread = threading.Thread(
                    target=self._handle_client,
                    args=(client_socket, client_address)
                )
                client_thread.daemon = True
                client_thread.start()
                self.threads.append(client_thread)
                
            except Exception as e:
                if self.running:
                    print(f"[TCP Server] 接受连接错误: {e}")
                    
    def _handle_client(self, client_socket, client_address):
        """处理客户端请求"""
        try:
            while self.running:
                # 接收数据
                data = client_socket.recv(1024)
                if not data:
                    break
                    
                # 解析Modbus TCP请求
                response = self._process_modbus_request(data)
                
                # 发送响应
                if response:
                    client_socket.send(response)
                    
        except Exception as e:
            print(f"[TCP Server] 处理客户端 {client_address} 错误: {e}")
        finally:
            client_socket.close()
            print(f"[TCP Server] 客户端断开: {client_address}")
            
    def _process_modbus_request(self, data):
        """处理Modbus TCP请求"""
        try:
            if len(data) < 8:  # Modbus TCP头部至少8字节
                return None
                
            # 解析Modbus TCP头部
            transaction_id = struct.unpack('>H', data[0:2])[0]
            protocol_id = struct.unpack('>H', data[2:4])[0]
            length = struct.unpack('>H', data[4:6])[0]
            unit_id = data[6]
            function_code = data[7]
            
            print(f"[TCP Server] 收到请求: "
                  f"TID={transaction_id}, "
                  f"PID={protocol_id}, "
                  f"Len={length}, "
                  f"Unit={unit_id}, "
                  f"FC={function_code}")
            
            # 处理不同功能码
            if function_code == 0x03:  # 读保持寄存器
                return self._handle_read_holding_registers(transaction_id, unit_id, data[8:])
            elif function_code == 0x04:  # 读输入寄存器
                return self._handle_read_input_registers(transaction_id, unit_id, data[8:])
            elif function_code == 0x06:  # 写单个寄存器
                return self._handle_write_single_register(transaction_id, unit_id, data[8:])
            elif function_code == 0x10:  # 写多个寄存器
                return self._handle_write_multiple_registers(transaction_id, unit_id, data[8:])
            else:
                print(f"[TCP Server] 不支持的功能码: 0x{function_code:02x}")
                return self._create_error_response(transaction_id, unit_id, function_code, 0x01)
                
        except Exception as e:
            print(f"[TCP Server] 处理请求错误: {e}")
            return None
            
    def _handle_read_holding_registers(self, transaction_id, unit_id, data):
        """处理读保持寄存器请求"""
        try:
            if len(data) < 4:
                return self._create_error_response(transaction_id, unit_id, 0x03, 0x03)
                
            start_addr = struct.unpack('>H', data[0:2])[0]
            quantity = struct.unpack('>H', data[2:4])[0]
            
            if quantity < 1 or quantity > 125:
                return self._create_error_response(transaction_id, unit_id, 0x03, 0x03)
                
            # 读取寄存器值
            register_values = []
            for i in range(quantity):
                addr = start_addr + i
                if addr in self.registers['holding']:
                    register_values.append(self.registers['holding'][addr])
                else:
                    register_values.append(0)
                    
            # 构建响应
            byte_count = quantity * 2
            response_data = struct.pack('>B', byte_count)
            for value in register_values:
                response_data += struct.pack('>H', value)
                
            return self._create_success_response(transaction_id, unit_id, 0x03, response_data)
            
        except Exception as e:
            print(f"[TCP Server] 读保持寄存器错误: {e}")
            return self._create_error_response(transaction_id, unit_id, 0x03, 0x04)
            
    def _handle_read_input_registers(self, transaction_id, unit_id, data):
        """处理读输入寄存器请求"""
        try:
            if len(data) < 4:
                return self._create_error_response(transaction_id, unit_id, 0x04, 0x03)
                
            start_addr = struct.unpack('>H', data[0:2])[0]
            quantity = struct.unpack('>H', data[2:4])[0]
            
            if quantity < 1 or quantity > 125:
                return self._create_error_response(transaction_id, unit_id, 0x04, 0x03)
                
            # 读取寄存器值
            register_values = []
            for i in range(quantity):
                addr = start_addr + i
                if addr in self.registers['input']:
                    register_values.append(self.registers['input'][addr])
                else:
                    register_values.append(0)
                    
            # 构建响应
            byte_count = quantity * 2
            response_data = struct.pack('>B', byte_count)
            for value in register_values:
                response_data += struct.pack('>H', value)
                
            return self._create_success_response(transaction_id, unit_id, 0x04, response_data)
            
        except Exception as e:
            print(f"[TCP Server] 读输入寄存器错误: {e}")
            return self._create_error_response(transaction_id, unit_id, 0x04, 0x04)
            
    def _handle_write_single_register(self, transaction_id, unit_id, data):
        """处理写单个寄存器请求"""
        try:
            if len(data) < 4:
                return self._create_error_response(transaction_id, unit_id, 0x06, 0x03)
                
            register_addr = struct.unpack('>H', data[0:2])[0]
            register_value = struct.unpack('>H', data[2:4])[0]
            
            # 写入寄存器
            self.registers['holding'][register_addr] = register_value
            
            print(f"[TCP Server] 写入保持寄存器 [{register_addr}] = {register_value}")
            
            # 返回相同的请求作为响应
            return self._create_success_response(transaction_id, unit_id, 0x06, data[:4])
            
        except Exception as e:
            print(f"[TCP Server] 写单个寄存器错误: {e}")
            return self._create_error_response(transaction_id, unit_id, 0x06, 0x04)
            
    def _handle_write_multiple_registers(self, transaction_id, unit_id, data):
        """处理写多个寄存器请求"""
        try:
            if len(data) < 6:
                return self._create_error_response(transaction_id, unit_id, 0x10, 0x03)
                
            start_addr = struct.unpack('>H', data[0:2])[0]
            quantity = struct.unpack('>H', data[2:4])[0]
            byte_count = data[4]
            
            if byte_count != quantity * 2:
                return self._create_error_response(transaction_id, unit_id, 0x10, 0x03)
                
            # 写入寄存器
            for i in range(quantity):
                addr = start_addr + i
                value = struct.unpack('>H', data[5 + i*2:7 + i*2])[0]
                self.registers['holding'][addr] = value
                
            print(f"[TCP Server] 写入多个保持寄存器 [{start_addr}-{start_addr+quantity-1}]")
            
            # 返回响应
            response_data = struct.pack('>HH', start_addr, quantity)
            return self._create_success_response(transaction_id, unit_id, 0x10, response_data)
            
        except Exception as e:
            print(f"[TCP Server] 写多个寄存器错误: {e}")
            return self._create_error_response(transaction_id, unit_id, 0x10, 0x04)
            
    def _create_success_response(self, transaction_id, unit_id, function_code, data):
        """创建成功响应"""
        length = len(data) + 2  # unit_id + function_code + data
        response = struct.pack('>HHHBB', transaction_id, 0, length, unit_id, function_code)
        response += data
        return response
        
    def _create_error_response(self, transaction_id, unit_id, function_code, error_code):
        """创建错误响应"""
        length = 3  # unit_id + function_code + error_code
        response = struct.pack('>HHHBB', transaction_id, 0, length, unit_id, function_code + 0x80)
        response += struct.pack('>B', error_code)
        return response
        
    def _update_registers(self):
        """定期更新寄存器数据（模拟传感器数据变化）"""
        update_count = 0
        while self.running:
            try:
                # 模拟温度数据变化
                for i in range(5):
                    self.registers['input'][i] = 20 + (update_count % 80) + i * 5
                    
                # 模拟湿度数据变化
                for i in range(5, 10):
                    self.registers['input'][i] = 50 + (update_count % 30) + (i-5) * 3
                    
                update_count += 1
                time.sleep(2)
                
            except Exception as e:
                print(f"[TCP Server] 更新寄存器错误: {e}")
                time.sleep(1)
                
    def stop(self):
        """停止服务器"""
        self.running = False
        if self.server_socket:
            self.server_socket.close()
            
        for thread in self.threads:
            if thread.is_alive():
                thread.join(timeout=1)
                
        print("[TCP Server] 服务器已停止")

def main():
    parser = argparse.ArgumentParser(description='Modbus TCP Server模拟器')
    parser.add_argument('--host', default='0.0.0.0', help='监听地址')
    parser.add_argument('--port', type=int, default=502, help='监听端口')
    
    args = parser.parse_args()
    
    server = ModbusTCPServerSimulator(host=args.host, port=args.port)
    
    try:
        print(f"=== Modbus TCP Server模拟器 ===")
        print(f"监听地址: {args.host}:{args.port}")
        print("=" * 40)
        
        if server.start():
            # 保持运行
            while True:
                time.sleep(1)
        else:
            print("服务器启动失败")
            
    except KeyboardInterrupt:
        print("\n收到停止信号...")
        server.stop()
        print("模拟器已退出")

if __name__ == '__main__':
    main()
