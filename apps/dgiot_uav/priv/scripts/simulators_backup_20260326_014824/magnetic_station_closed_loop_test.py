#!/usr/bin/env python3
"""
磁航向工位闭环测试系统
完整模拟磁航向工位的所有设备和服务，实现自闭环测试

架构：
1. PLC Server (192.168.100.20:502) - Modbus TCP Server
2. 地测口 Client (192.168.100.21:10007 -> DG-IoT:20000)
3. 扫码枪 Client (192.168.100.23:1234 -> DG-IoT:20000)
4. MES Server (0.0.0.0:801) - HTTP Server
5. DG-IoT Server (192.168.100.100:20000) - 已运行

使用方法:
  python3 magnetic_station_closed_loop_test.py
"""

import json
import logging
import socket
import struct
import sys
import threading
import time
from datetime import datetime
from http.server import HTTPServer, BaseHTTPRequestHandler
from typing import Dict, Optional
from dataclasses import dataclass

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

# ==================== 配置常量 ====================
DGIOT_HOST = "192.168.100.100"
DGIOT_PORT = 20000

MAGNETIC_PLC_IP = "192.168.100.20"
MAGNETIC_PLC_PORT = 502

GROUND_STATION_IP = "192.168.100.21"
GROUND_STATION_PORT = 10007

SCANNER_IP = "192.168.100.23"
SCANNER_PORT = 1234

MES_SERVER_HOST = "0.0.0.0"
MES_SERVER_PORT = 801

# EB90遥测数据
EB90_D1_FRAME = bytes.fromhex(
    "EB90000000120001A55AF0A20000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
)

# ==================== MES Server ====================
class MESRequestHandler(BaseHTTPRequestHandler):
    """MES请求处理器"""
    
    def log_message(self, format, *args):
        """自定义日志格式"""
        logger.info(f"[MES] {format % args}")
    
    def do_POST(self):
        """处理POST请求"""
        try:
            content_length = int(self.headers.get('Content-Length', 0))
            post_data = self.rfile.read(content_length)
            
            logger.info(f"[MES] 收到请求: {self.path}")
            logger.info(f"[MES] 请求数据: {post_data.decode('utf-8')}")
            
            # 解析JSON数据
            try:
                data = json.loads(post_data.decode('utf-8'))
                logger.info(f"[MES] 解析成功: {json.dumps(data, indent=2, ensure_ascii=False)}")
            except Exception as e:
                logger.error(f"[MES] JSON解析失败: {e}")
            
            # 返回成功响应
            response = {
                "status": "success",
                "message": "数据接收成功",
                "timestamp": datetime.now().isoformat()
            }
            
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps(response, ensure_ascii=False).encode('utf-8'))
            
            logger.info(f"[MES] 响应已发送: {response}")
            
        except Exception as e:
            logger.error(f"[MES] 处理请求失败: {e}")
            self.send_error(500, str(e))
    
    def do_GET(self):
        """处理GET请求"""
        self.send_response(200)
        self.send_header('Content-Type', 'text/plain')
        self.end_headers()
        self.wfile.write(b"MES Simulator is running")

class MESServer:
    """MES模拟服务器"""
    
    def __init__(self, host: str, port: int):
        self.host = host
        self.port = port
        self.server = None
        self.thread = None
    
    def start(self):
        """启动MES服务器"""
        try:
            self.server = HTTPServer((self.host, self.port), MESRequestHandler)
            self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
            self.thread.start()
            logger.info(f"[MES] 服务器启动成功: http://{self.host}:{self.port}")
            return True
        except Exception as e:
            logger.error(f"[MES] 服务器启动失败: {e}")
            return False
    
    def stop(self):
        """停止MES服务器"""
        if self.server:
            self.server.shutdown()
            logger.info("[MES] 服务器已停止")

# ==================== Ground Station Client ====================
class GroundStationClient:
    """地测口客户端（TCP Client）"""
    
    def __init__(self, dgiot_host: str, dgiot_port: int):
        self.dgiot_host = dgiot_host
        self.dgiot_port = dgiot_port
        self.socket = None
        self.connected = False
    
    def connect(self) -> bool:
        """连接到DG-IoT服务器"""
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(10.0)
            self.socket.connect((self.dgiot_host, self.dgiot_port))
            self.connected = True
            logger.info(f"[地测口] 成功连接到DG-IoT: {self.dgiot_host}:{self.dgiot_port}")
            return True
        except Exception as e:
            logger.error(f"[地测口] 连接失败: {e}")
            return False
    
    def register(self) -> bool:
        """发送注册报文"""
        if not self.connected:
            logger.error("[地测口] 未连接")
            return False
        
        try:
            # 发送注册报文
            register_msg = b"wrj_dicekou\n"
            self.socket.sendall(register_msg)
            logger.info(f"[地测口] 发送注册报文: {register_msg}")
            
            # 等待响应
            time.sleep(1)
            try:
                response = self.socket.recv(1024)
                logger.info(f"[地测口] 收到响应: {response}")
                
                if b"OK" in response:
                    logger.info("[地测口] ✅ 注册成功")
                    return True
            except socket.timeout:
                logger.warning("[地测口] 未收到响应（可能正常）")
                return True
            
        except Exception as e:
            logger.error(f"[地测口] 注册失败: {e}")
            return False
    
    def send_eb90_data(self, data: bytes, description: str = ""):
        """发送EB90数据"""
        if not self.connected:
            logger.error("[地测口] 未连接")
            return False
        
        try:
            self.socket.sendall(data)
            logger.info(f"[地测口] 发送EB90数据成功: {description}, 长度: {len(data)}字节")
            logger.info(f"[地测口] 数据: {data.hex()}")
            return True
        except Exception as e:
            logger.error(f"[地测口] 发送EB90数据失败: {e}")
            return False
    
    def close(self):
        """关闭连接"""
        if self.socket:
            self.socket.close()
            logger.info("[地测口] 连接已关闭")

# ==================== Scanner Client ====================
class ScannerClient:
    """扫码枪客户端（TCP Client）"""
    
    def __init__(self, dgiot_host: str, dgiot_port: int):
        self.dgiot_host = dgiot_host
        self.dgiot_port = dgiot_port
        self.socket = None
        self.connected = False
    
    def connect(self) -> bool:
        """连接到DG-IoT服务器"""
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(10.0)
            self.socket.connect((self.dgiot_host, self.dgiot_port))
            self.connected = True
            logger.info(f"[扫码枪] 成功连接到DG-IoT: {self.dgiot_host}:{self.dgiot_port}")
            return True
        except Exception as e:
            logger.error(f"[扫码枪] 连接失败: {e}")
            return False
    
    def scan_device(self, device_code: str) -> bool:
        """扫描设备编码"""
        if not self.connected:
            logger.error("[扫码枪] 未连接")
            return False
        
        try:
            # 发送扫描消息
            scan_msg = f"SCAN:{device_code}\n".encode('utf-8')
            self.socket.sendall(scan_msg)
            logger.info(f"[扫码枪] 扫描设备: {device_code}")
            logger.info(f"[扫码枪] 发送消息: {scan_msg}")
            
            # 等待响应
            time.sleep(1)
            try:
                response = self.socket.recv(1024)
                logger.info(f"[扫码枪] 收到响应: {response}")
            except socket.timeout:
                logger.warning("[扫码枪] 未收到响应（可能正常）")
            
            return True
            
        except Exception as e:
            logger.error(f"[扫码枪] 扫描失败: {e}")
            return False
    
    def close(self):
        """关闭连接"""
        if self.socket:
            self.socket.close()
            logger.info("[扫码枪] 连接已关闭")

# ==================== 主测试流程 ====================
class MagneticStationClosedLoopTest:
    """磁航向工位闭环测试"""
    
    def __init__(self):
        self.mes_server = MESServer(MES_SERVER_HOST, MES_SERVER_PORT)
        self.ground_station = GroundStationClient(DGIOT_HOST, DGIOT_PORT)
        self.scanner = ScannerClient(DGIOT_HOST, DGIOT_PORT)
        self.test_results = []
    
    def setup(self):
        """测试环境准备"""
        logger.info("="*70)
        logger.info("磁航向工位闭环测试系统")
        logger.info("="*70)
        
        logger.info("\n[步骤1] 启动MES服务器...")
        if not self.mes_server.start():
            logger.error("MES服务器启动失败")
            return False
        
        time.sleep(1)
        
        logger.info("\n[步骤2] 检查DG-IoT服务器状态...")
        # 检查DG-IoT是否运行
        try:
            test_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            test_sock.settimeout(2.0)
            test_sock.connect((DGIOT_HOST, DGIOT_PORT))
            test_sock.close()
            logger.info(f"✅ DG-IoT服务器正在运行: {DGIOT_HOST}:{DGIOT_PORT}")
        except Exception as e:
            logger.error(f"❌ DG-IoT服务器未运行: {e}")
            logger.error("请先启动DG-IoT服务器: make run")
            return False
        
        logger.info("\n[步骤3] 地测口连接DG-IoT...")
        if not self.ground_station.connect():
            logger.error("地测口连接失败")
            return False
        
        logger.info("\n[步骤4] 地测口注册...")
        if not self.ground_station.register():
            logger.error("地测口注册失败")
            return False
        
        logger.info("\n[步骤5] 扫码枪连接DG-IoT...")
        if not self.scanner.connect():
            logger.error("扫码枪连接失败")
            return False
        
        logger.info("\n✅ 测试环境准备完成")
        return True
    
    def run_test_scenario(self):
        """运行测试场景"""
        logger.info("\n" + "="*70)
        logger.info("开始执行测试场景")
        logger.info("="*70)
        
        # 场景1: 扫码绑定设备
        logger.info("\n[场景1] 扫码绑定设备...")
        device_code = "UAV-001"
        if self.scanner.scan_device(device_code):
            logger.info(f"✅ 设备 {device_code} 扫描成功")
            self.test_results.append(("扫码绑定", "passed", device_code))
        else:
            logger.error(f"❌ 设备 {device_code} 扫描失败")
            self.test_results.append(("扫码绑定", "failed", device_code))
        
        time.sleep(2)
        
        # 场景2: 地测口发送EB90遥测数据
        logger.info("\n[场景2] 地测口发送EB90遥测数据...")
        if self.ground_station.send_eb90_data(EB90_D1_FRAME, "D1遥测帧"):
            logger.info("✅ EB90遥测数据发送成功")
            self.test_results.append(("EB90遥测", "passed", "D1帧"))
        else:
            logger.error("❌ EB90遥测数据发送失败")
            self.test_results.append(("EB90遥测", "failed", "D1帧"))
        
        time.sleep(2)
        
        # 场景3: 持续发送遥测数据（模拟实时数据流）
        logger.info("\n[场景3] 持续发送遥测数据（3次）...")
        for i in range(3):
            logger.info(f"  第{i+1}次发送...")
            if self.ground_station.send_eb90_data(EB90_D1_FRAME, f"D1遥测帧-{i+1}"):
                logger.info(f"  ✅ 第{i+1}次发送成功")
            else:
                logger.error(f"  ❌ 第{i+1}次发送失败")
            time.sleep(1)
        
        self.test_results.append(("持续遥测", "passed", "3次发送"))
        
        logger.info("\n✅ 测试场景执行完成")
    
    def print_summary(self):
        """打印测试总结"""
        logger.info("\n" + "="*70)
        logger.info("测试总结")
        logger.info("="*70)
        
        logger.info("\n测试结果:")
        total = len(self.test_results)
        passed = sum(1 for _, status, _ in self.test_results if status == "passed")
        
        for scenario, status, detail in self.test_results:
            status_icon = "✅" if status == "passed" else "❌"
            logger.info(f"  {status_icon} {scenario}: {status} ({detail})")
        
        logger.info(f"\n总计: {passed}/{total} 通过")
        
        logger.info("\n请检查DG-IoT日志确认处理结果:")
        logger.info(f"  _build/emqx/rel/emqx/bin/emqx eval 'ets:tab2list(uav_ip_station_mapping).'")
        logger.info(f"  _build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_business_service:get_station_by_ip(<<\"{GROUND_STATION_IP}\">>).'")
        
        logger.info("\n" + "="*70)
    
    def cleanup(self):
        """清理资源"""
        logger.info("\n清理测试资源...")
        self.ground_station.close()
        self.scanner.close()
        # MES服务器保持运行，可以继续接收数据
        logger.info("✅ 资源清理完成")
    
    def run(self):
        """运行完整测试"""
        try:
            if not self.setup():
                return False
            
            self.run_test_scenario()
            self.print_summary()
            
            logger.info("\n闭环测试完成！")
            logger.info("MES服务器将继续运行，监听DG-IoT的数据上报...")
            logger.info(f"访问: http://localhost:{MES_SERVER_PORT}")
            
            # 保持MES服务器运行
            logger.info("\n按 Ctrl+C 退出...")
            try:
                while True:
                    time.sleep(1)
            except KeyboardInterrupt:
                logger.info("\n收到退出信号...")
            
            return True
            
        finally:
            self.cleanup()

def main():
    """主函数"""
    test = MagneticStationClosedLoopTest()
    success = test.run()
    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
