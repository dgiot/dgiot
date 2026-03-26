#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
工位测试管理系统
支持：环境清理、单个工位测试、状态查询
"""

import os
import sys
import json
import time
import signal
import subprocess
import requests
from typing import Dict, List, Optional
from dataclasses import dataclass

# 配置
DGIOT_ROOT = "/root/gitee/dgiot"
DGIOT_HOST = "192.168.100.100"
DGIOT_PORT = 20000
MES_PORT = 80  # 通过nginx映射到801
PLC_PORT = 502
EMQX_CMD = f"{DGIOT_ROOT}/_build/emqx/rel/emqx/bin/emqx"

# 工位配置
STATIONS = {
    "1700": {
        "name": "磁航向",
        "plc_ip": "192.168.100.20",
        "ground_station_ip": "192.168.100.21",
        "scanner_ip": "192.168.100.23",
        "ports": [10007, 1234],  # 地测口、扫码枪
    },
    "1500": {
        "name": "总测1",
        "plc_ip": "192.168.100.40",
        "fixture_ip": "192.168.100.45",
        "ports": [10006, 10001, 10002, 10003, 10004, 10005],
    },
    "1600": {
        "name": "总测2",
        "plc_ip": "192.168.100.40",
        "fixture_ip": "192.168.100.46",
        "ports": [10006, 10001, 10002, 10003, 10004, 10005],
    },
    "1200": {
        "name": "拷机1",
        "plc_ip": "192.168.100.40",
        "fixture_ip": "192.168.100.47",
        "ports": [10006],
    },
    "1300": {
        "name": "拷机2",
        "plc_ip": "192.168.100.40",
        "fixture_ip": "192.168.100.48",
        "ports": [10006],
    },
}


@dataclass
class TestProcess:
    """测试进程信息"""
    station_id: str
    pid: int
    start_time: float
    log_file: str


class StationTestManager:
    """工位测试管理器"""
    
    def __init__(self):
        self.processes: Dict[str, TestProcess] = {}
        self.log_dir = "/tmp/station_tests"
        os.makedirs(self.log_dir, exist_ok=True)
    
    def clean_environment(self, station_id: Optional[str] = None):
        """
        清理测试环境
        
        Args:
            station_id: 工位ID，None表示清理所有
        """
        print("\n" + "="*60)
        print("清理测试环境")
        print("="*60)
        
        # 1. 停止所有测试进程
        self.stop_all_tests()
        
        # 2. 清理ETS表（通过DG-IoT eval）
        print("\n[1] 清理ETS表...")
        self._clean_ets_tables(station_id)
        
        # 3. 清理设备注册
        print("\n[2] 清理设备注册...")
        self._clean_device_registry(station_id)
        
        # 4. 清理工位映射
        print("\n[3] 清理工位映射...")
        self._clean_station_mappings(station_id)
        
        # 5. 清理测试日志
        print("\n[4] 清理测试日志...")
        self._clean_test_logs(station_id)
        
        print("\n[OK] 环境清理完成")
    
    def _clean_ets_tables(self, station_id: Optional[str]):
        """清理ETS表"""
        commands = [
            # 清理IP-工位映射
            "ets:delete_all_objects(uav_ip_station_mapping).",
            # 清理工位状态
            "ets:delete_all_objects(uav_station_status).",
            # 清理设备缓存
            "ets:delete_all_objects(dgiot_device_cache).",
        ]
        
        for cmd in commands:
            try:
                result = subprocess.run(
                    [EMQX_CMD, "eval", cmd],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                if result.returncode == 0:
                    print(f"  [OK] {cmd}")
                else:
                    print(f"  [WARN]  {cmd} - {result.stderr.strip()}")
            except Exception as e:
                print(f"  [ERROR] {cmd} - {e}")
    
    def _clean_device_registry(self, station_id: Optional[str]):
        """清理设备注册"""
        if station_id and station_id in STATIONS:
            # 清理特定工位的设备
            station_config = STATIONS[station_id]
            for port in station_config.get("ports", []):
                try:
                    # 删除设备
                    device_id = f"device_{station_id}_{port}"
                    cmd = f'dgiot_device:delete(<<"{device_id}">>).'
                    subprocess.run(
                        [EMQX_CMD, "eval", cmd],
                        capture_output=True,
                        text=True,
                        timeout=5
                    )
                    print(f"  [OK] 删除设备: {device_id}")
                except Exception as e:
                    print(f"  [WARN]  删除设备失败: {e}")
        else:
            print("  [SKIP]  跳过设备清理（需要指定工位ID）")
    
    def _clean_station_mappings(self, station_id: Optional[str]):
        """清理工位映射"""
        # 通过API清理工位设备绑定
        if station_id and station_id in STATIONS:
            station_config = STATIONS[station_id]
            ips = [
                station_config.get("ground_station_ip"),
                station_config.get("scanner_ip"),
                station_config.get("fixture_ip")
            ]
            
            for ip in ips:
                if ip:
                    try:
                        # 删除IP映射
                        cmd = f'ets:delete(uav_ip_station_mapping, <<"{ip}">>).'
                        subprocess.run(
                            [EMQX_CMD, "eval", cmd],
                            capture_output=True,
                            text=True,
                            timeout=5
                        )
                        print(f"  [OK] 清理IP映射: {ip}")
                    except Exception as e:
                        print(f"  [WARN]  清理IP映射失败: {e}")
    
    def _clean_test_logs(self, station_id: Optional[str]):
        """清理测试日志"""
        import glob
        
        if station_id:
            pattern = f"{self.log_dir}/station_{station_id}_*.log"
        else:
            pattern = f"{self.log_dir}/station_*.log"
        
        logs = glob.glob(pattern)
        for log in logs:
            try:
                os.remove(log)
                print(f"  [OK] 删除日志: {log}")
            except Exception as e:
                print(f"  [WARN]  删除日志失败: {e}")
    
    def start_test(self, station_id: str, duration: int = 300):
        """
        启动单个工位测试
        
        Args:
            station_id: 工位ID
            duration: 测试时长（秒）
        """
        if station_id not in STATIONS:
            print(f"[ERROR] 无效的工位ID: {station_id}")
            print(f"可用工位: {list(STATIONS.keys())}")
            return
        
        if station_id in self.processes:
            print(f"[WARN]  工位 {station_id} 测试已在运行")
            return
        
        print("\n" + "="*60)
        print(f"启动工位测试: {station_id} - {STATIONS[station_id]['name']}")
        print("="*60)
        
        # 1. 清理环境
        self.clean_environment(station_id)
        
        # 2. 绑定IP
        print("\n[5] 绑定测试IP...")
        self._bind_test_ips(station_id)
        
        # 3. 启动MES服务器
        print("\n[6] 启动MES服务器...")
        mes_pid = self._start_mes_server(station_id)
        
        # 4. 启动PLC服务器
        print("\n[7] 启动PLC服务器...")
        plc_pid = self._start_plc_server(station_id)
        
        # 5. 启动设备模拟器
        print("\n[8] 启动设备模拟器...")
        device_pid = self._start_device_simulator(station_id, duration)
        
        # 6. 记录进程信息
        log_file = f"{self.log_dir}/station_{station_id}_{int(time.time())}.log"
        self.processes[station_id] = TestProcess(
            station_id=station_id,
            pid=device_pid,
            start_time=time.time(),
            log_file=log_file
        )
        
        print(f"\n[OK] 工位 {station_id} 测试已启动")
        print(f"   PID: {device_pid}")
        print(f"   日志: {log_file}")
        print(f"   时长: {duration}秒")
        print(f"\n查看状态: python3 {__file__} status {station_id}")
        print(f"停止测试: python3 {__file__} stop {station_id}")
    
    def _bind_test_ips(self, station_id: str):
        """绑定测试IP"""
        station_config = STATIONS[station_id]
        ips = [
            station_config.get("plc_ip"),
            station_config.get("ground_station_ip"),
            station_config.get("scanner_ip"),
            station_config.get("fixture_ip")
        ]
        
        for ip in ips:
            if ip:
                try:
                    # 检查IP是否已绑定
                    result = subprocess.run(
                        ["ip", "addr", "show", "eth0"],
                        capture_output=True,
                        text=True
                    )
                    
                    if ip in result.stdout:
                        print(f"  [OK] IP {ip} 已绑定")
                    else:
                        # 绑定IP
                        subprocess.run(
                            ["sudo", "ip", "addr", "add", f"{ip}/24", "dev", "eth0"],
                            check=True
                        )
                        print(f"  [OK] 绑定IP: {ip}")
                except Exception as e:
                    print(f"  [WARN]  绑定IP {ip} 失败: {e}")
    
    def _start_mes_server(self, station_id: str) -> int:
        """启动MES服务器"""
        mes_script = f"""
import http.server
import json
import time

class MESHandler(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        content_length = int(self.headers['Content-Length'])
        post_data = self.rfile.read(content_length)
        
        print(f"[MES-{station_id}] 收到数据: {{post_data.decode()}}")
        
        # 保存到文件
        with open("{self.log_dir}/mes_{station_id}_data.jsonl", "a") as f:
            f.write(json.dumps({{
                "timestamp": time.time(),
                "data": post_data.decode()
            }}) + "\\n")
        
        # 返回成功响应
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()
        self.wfile.write(b'{{"status": "ok"}}')

if __name__ == "__main__":
    server = http.server.HTTPServer(('0.0.0.0', {MES_PORT}), MESHandler)
    print(f"[MES-{station_id}] 服务器启动在端口 {MES_PORT}")
    server.serve_forever()
"""
        
        mes_file = f"{self.log_dir}/mes_server_{station_id}.py"
        with open(mes_file, "w") as f:
            f.write(mes_script)
        
        # 启动进程
        process = subprocess.Popen(
            ["python3", mes_file],
            stdout=open(f"{self.log_dir}/mes_{station_id}.log", "w"),
            stderr=subprocess.STDOUT
        )
        
        print(f"  [OK] MES服务器启动 (PID: {process.pid}, 端口: {MES_PORT})")
        return process.pid
    
    def _start_plc_server(self, station_id: str) -> int:
        """启动PLC服务器"""
        station_config = STATIONS[station_id]
        plc_ip = station_config.get("plc_ip")
        
        if not plc_ip:
            print("  [SKIP]  此工位不需要PLC服务器")
            return 0
        
        # 使用现有的PLC模拟器
        plc_script = "/root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/plc_simulator.py"
        
        if os.path.exists(plc_script):
            process = subprocess.Popen(
                ["python3", plc_script, "--host", plc_ip, "--port", str(PLC_PORT)],
                stdout=open(f"{self.log_dir}/plc_{station_id}.log", "w"),
                stderr=subprocess.STDOUT
            )
            print(f"  [OK] PLC服务器启动 (PID: {process.pid}, IP: {plc_ip})")
            return process.pid
        else:
            print(f"  [WARN]  PLC模拟器不存在: {plc_script}")
            return 0
    
    def _start_device_simulator(self, station_id: str, duration: int) -> int:
        """启动设备模拟器"""
        # 根据工位类型选择不同的模拟器
        if station_id == "1700":
            # 磁航向工位：地测口 + 扫码枪
            return self._start_magnetic_simulator(station_id, duration)
        elif station_id in ["1500", "1600"]:
            # 总测工位：治具 + 舵面传感器
            return self._start_fixture_simulator(station_id, duration)
        elif station_id in ["1200", "1300"]:
            # 拷机工位：治具
            return self._start_burnin_simulator(station_id, duration)
        else:
            print(f"  [WARN]  未知工位类型: {station_id}")
            return 0
    
    def _start_magnetic_simulator(self, station_id: str, duration: int) -> int:
        """启动磁航向工位模拟器"""
        station_config = STATIONS[station_id]
        ground_station_ip = station_config.get("ground_station_ip")
        scanner_ip = station_config.get("scanner_ip")
        
        simulator_script = f"""
import socket
import time
import struct

# 地测口连接（绑定源IP和端口10007）
ground_station = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
ground_station.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
ground_station.bind(("{ground_station_ip}", 10007))
ground_station.connect(("{DGIOT_HOST}", {DGIOT_PORT}))

# 注册
ground_station.send(b"wrj_dicekou\\n")
print("[地测口] 注册成功 - IP: {ground_station_ip}, Port: 10007")
time.sleep(1)

# 扫码枪连接（绑定源IP和端口1234）
scanner = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
scanner.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
scanner.bind(("{scanner_ip}", 1234))
scanner.connect(("{DGIOT_HOST}", {DGIOT_PORT}))

# 注册
scanner.send(b"wrj_saomiao\\n")
print("[扫码枪] 注册成功 - IP: {scanner_ip}, Port: 1234")
time.sleep(1)

# 扫描绑定（使用二维码格式）
# 格式：测试ID|工位ID|物料编码|数量|设备序列号|||
qrcode_data = "Test01|1700|UAV-MATERIAL-001|1|UAV-001|||"
scanner.send(qrcode_data.encode() + b"\\n")
print("[扫码枪] 扫描设备二维码: " + qrcode_data)
time.sleep(1)

# 发送EB90遥测数据
start_time = time.time()
frame_count = 0

while time.time() - start_time < {duration}:
    # 构造EB90 D1遥测帧（简化版）
    frame_data = [
        0xEB, 0x90,  # 同步头
        0x00, 0x00,  # 目的地址
        0x00, 0x12,  # 源地址
        0x01,        # 平台类型（遥测）
        frame_count % 256,  # 帧号
    ] + [0x00] * 120 + [0x00, 0x00]  # 载荷 + CRC
    frame = bytes(frame_data)
    
    ground_station.send(frame)
    print("[地测口] 发送EB90帧 #" + str(frame_count))
    frame_count += 1
    
    time.sleep(1)  # 1秒间隔

print("[测试] 完成")
ground_station.close()
scanner.close()
"""
        
        script_file = f"{self.log_dir}/simulator_{station_id}.py"
        with open(script_file, "w") as f:
            f.write(simulator_script)
        
        process = subprocess.Popen(
            ["python3", script_file],
            stdout=open(f"{self.log_dir}/device_{station_id}.log", "w"),
            stderr=subprocess.STDOUT
        )
        
        print(f"  [OK] 设备模拟器启动 (PID: {process.pid})")
        return process.pid
    
    def _start_fixture_simulator(self, station_id: str, duration: int) -> int:
        """启动治具模拟器"""
        # 使用现有的fixture_simulator.py
        fixture_script = "/root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators/fixture_simulator.py"
        station_config = STATIONS[station_id]
        fixture_ip = station_config.get("fixture_ip")
        
        if os.path.exists(fixture_script):
            process = subprocess.Popen(
                ["python3", fixture_script, "--dgiot-host", DGIOT_HOST, 
                 "--station-ip", fixture_ip, "--duration", str(duration)],
                stdout=open(f"{self.log_dir}/device_{station_id}.log", "w"),
                stderr=subprocess.STDOUT
            )
            print(f"  [OK] 治具模拟器启动 (PID: {process.pid})")
            return process.pid
        else:
            print(f"  [WARN]  治具模拟器不存在: {fixture_script}")
            return 0
    
    def _start_burnin_simulator(self, station_id: str, duration: int) -> int:
        """启动拷机模拟器"""
        # 拷机工位使用治具模拟器
        return self._start_fixture_simulator(station_id, duration)
    
    def stop_test(self, station_id: str):
        """停止单个工位测试"""
        if station_id not in self.processes:
            print(f"[WARN]  工位 {station_id} 没有运行中的测试")
            return
        
        print(f"\n停止工位 {station_id} 测试...")
        
        # 停止所有相关进程
        process_info = self.processes[station_id]
        
        # 停止设备模拟器
        try:
            os.kill(process_info.pid, signal.SIGTERM)
            print(f"  [OK] 停止设备模拟器 (PID: {process_info.pid})")
        except Exception as e:
            print(f"  [WARN]  停止设备模拟器失败: {e}")
        
        # 停止MES和PLC（通过进程名查找）
        self._stop_related_processes(station_id)
        
        # 清理环境
        self.clean_environment(station_id)
        
        # 移除记录
        del self.processes[station_id]
        
        print(f"[OK] 工位 {station_id} 测试已停止")
    
    def _stop_related_processes(self, station_id: str):
        """停止相关进程"""
        import psutil
        
        # 查找并停止MES服务器
        for proc in psutil.process_iter(['pid', 'name', 'cmdline']):
            try:
                cmdline = ' '.join(proc.info['cmdline'] or [])
                if f"mes_server_{station_id}" in cmdline:
                    proc.terminate()
                    print(f"  [OK] 停止MES服务器 (PID: {proc.pid})")
            except:
                pass
    
    def stop_all_tests(self):
        """停止所有测试"""
        if not self.processes:
            print("没有运行中的测试")
            return
        
        print(f"\n停止 {len(self.processes)} 个测试...")
        for station_id in list(self.processes.keys()):
            self.stop_test(station_id)
    
    def show_status(self, station_id: Optional[str] = None):
        """显示测试状态"""
        print("\n" + "="*60)
        print("工位测试状态")
        print("="*60)
        
        if station_id:
            if station_id in self.processes:
                process_info = self.processes[station_id]
                elapsed = time.time() - process_info.start_time
                
                print(f"\n工位 {station_id} - {STATIONS[station_id]['name']}")
                print(f"  PID: {process_info.pid}")
                print(f"  运行时间: {int(elapsed)}秒")
                print(f"  日志文件: {process_info.log_file}")
                
                # 检查进程是否存活
                try:
                    os.kill(process_info.pid, 0)
                    print(f"  状态: [OK] 运行中")
                except:
                    print(f"  状态: [ERROR] 已停止")
            else:
                print(f"\n工位 {station_id} - 未运行")
        else:
            if self.processes:
                for sid, process_info in self.processes.items():
                    elapsed = time.time() - process_info.start_time
                    print(f"\n工位 {sid} - {STATIONS[sid]['name']}")
                    print(f"  PID: {process_info.pid}, 运行: {int(elapsed)}秒")
            else:
                print("\n没有运行中的测试")
        
        # 显示可用工位
        print(f"\n可用工位: {list(STATIONS.keys())}")


def main():
    """主函数"""
    if len(sys.argv) < 2:
        print("""
工位测试管理系统

用法:
    python3 station_test_manager.py <命令> [参数]

命令:
    clean [工位ID]          - 清理环境（不指定工位ID则清理所有）
    start <工位ID> [时长]   - 启动工位测试（默认300秒）
    stop <工位ID>           - 停止工位测试
    stop-all               - 停止所有测试
    status [工位ID]         - 查看状态
    
示例:
    python3 station_test_manager.py clean 1700
    python3 station_test_manager.py start 1700 600
    python3 station_test_manager.py status 1700
    python3 station_test_manager.py stop 1700
        """)
        return
    
    manager = StationTestManager()
    command = sys.argv[1]
    
    if command == "clean":
        station_id = sys.argv[2] if len(sys.argv) > 2 else None
        manager.clean_environment(station_id)
    
    elif command == "start":
        if len(sys.argv) < 3:
            print("[ERROR] 缺少工位ID")
            print("用法: python3 station_test_manager.py start <工位ID> [时长]")
            return
        station_id = sys.argv[2]
        duration = int(sys.argv[3]) if len(sys.argv) > 3 else 300
        manager.start_test(station_id, duration)
    
    elif command == "stop":
        if len(sys.argv) < 3:
            print("[ERROR] 缺少工位ID")
            print("用法: python3 station_test_manager.py stop <工位ID>")
            return
        station_id = sys.argv[2]
        manager.stop_test(station_id)
    
    elif command == "stop-all":
        manager.stop_all_tests()
    
    elif command == "status":
        station_id = sys.argv[2] if len(sys.argv) > 2 else None
        manager.show_status(station_id)
    
    else:
        print(f"[ERROR] 未知命令: {command}")


if __name__ == "__main__":
    main()
