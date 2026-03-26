#!/usr/bin/env python3
"""
磁航向工位闭环测试环境检查脚本
快速检查测试环境是否准备就绪

使用方法:
  python3 check_magnetic_environment.py
"""

import socket
import subprocess
import sys
from typing import List, Tuple

# 颜色定义
GREEN = '\033[0;32m'
RED = '\033[0;31m'
YELLOW = '\033[1;33m'
BLUE = '\033[0;34m'
NC = '\033[0m'

# 检查项
CHECKS = [
    ("DG-IoT服务器", "192.168.100.100", 20000, True),
    ("磁航向PLC IP", "192.168.100.20", None, False),
    ("地测口 IP", "192.168.100.21", None, False),
    ("扫码枪 IP", "192.168.100.23", None, False),
    ("MES服务端口", "0.0.0.0", 801, False),
]

def check_port(host: str, port: int, timeout: float = 2.0) -> bool:
    """检查端口是否可访问"""
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(timeout)
        sock.connect((host, port))
        sock.close()
        return True
    except:
        return False

def check_ip_binding(ip: str) -> bool:
    """检查IP是否已绑定"""
    try:
        result = subprocess.run(
            ['ip', 'addr', 'show', 'eth0'],
            capture_output=True,
            text=True
        )
        return ip in result.stdout
    except:
        return False

def check_process(name: str) -> bool:
    """检查进程是否运行"""
    try:
        result = subprocess.run(
            ['pgrep', '-f', name],
            capture_output=True
        )
        return result.returncode == 0
    except:
        return False

def print_header():
    """打印标题"""
    print(f"\n{BLUE}{'='*70}{NC}")
    print(f"{BLUE}磁航向工位闭环测试环境检查{NC}")
    print(f"{BLUE}{'='*70}{NC}\n")

def check_all() -> List[Tuple[str, str, bool]]:
    """执行所有检查"""
    results = []
    
    # 1. 检查DG-IoT服务器
    print(f"{YELLOW}[检查1] DG-IoT服务器状态...{NC}")
    if check_port("192.168.100.100", 20000):
        print(f"{GREEN}✅ DG-IoT服务器正在运行 (192.168.100.100:20000){NC}")
        results.append(("DG-IoT服务器", "运行中", True))
    else:
        print(f"{RED}❌ DG-IoT服务器未运行{NC}")
        print(f"{YELLOW}   请执行: cd /root/gitee/dgiot && make run{NC}")
        results.append(("DG-IoT服务器", "未运行", False))
    
    # 2. 检查IP绑定
    print(f"\n{YELLOW}[检查2] IP地址绑定状态...{NC}")
    ips = ["192.168.100.20", "192.168.100.21", "192.168.100.23"]
    all_bound = True
    for ip in ips:
        if check_ip_binding(ip):
            print(f"{GREEN}✅ IP {ip} 已绑定{NC}")
            results.append((f"IP {ip}", "已绑定", True))
        else:
            print(f"{RED}❌ IP {ip} 未绑定{NC}")
            results.append((f"IP {ip}", "未绑定", False))
            all_bound = False
    
    if not all_bound:
        print(f"{YELLOW}   绑定命令:{NC}")
        for ip in ips:
            print(f"   sudo ip addr add {ip}/24 dev eth0")
    
    # 3. 检查PLC模拟器
    print(f"\n{YELLOW}[检查3] PLC模拟器状态...{NC}")
    if check_process("plc_simulator.py"):
        print(f"{GREEN}✅ PLC模拟器正在运行{NC}")
        results.append(("PLC模拟器", "运行中", True))
    else:
        print(f"{YELLOW}⚠️  PLC模拟器未运行（可选）{NC}")
        print(f"{YELLOW}   启动命令: python3 plc_simulator.py &{NC}")
        results.append(("PLC模拟器", "未运行", False))
    
    # 4. 检查MES端口
    print(f"\n{YELLOW}[检查4] MES服务端口...{NC}")
    if check_port("0.0.0.0", 801):
        print(f"{YELLOW}⚠️  端口801已被占用{NC}")
        results.append(("MES端口", "已占用", False))
    else:
        print(f"{GREEN}✅ 端口801可用{NC}")
        results.append(("MES端口", "可用", True))
    
    # 5. 检查Python环境
    print(f"\n{YELLOW}[检查5] Python环境...{NC}")
    python_version = sys.version_info
    if python_version >= (3, 8):
        print(f"{GREEN}✅ Python版本: {python_version.major}.{python_version.minor}{NC}")
        results.append(("Python环境", f"{python_version.major}.{python_version.minor}", True))
    else:
        print(f"{RED}❌ Python版本过低: {python_version.major}.{python_version.minor}{NC}")
        print(f"{YELLOW}   需要Python 3.8+{NC}")
        results.append(("Python环境", f"{python_version.major}.{python_version.minor}", False))
    
    return results

def print_summary(results: List[Tuple[str, str, bool]]):
    """打印检查总结"""
    print(f"\n{BLUE}{'='*70}{NC}")
    print(f"{BLUE}检查总结{NC}")
    print(f"{BLUE}{'='*70}{NC}\n")
    
    total = len(results)
    passed = sum(1 for _, _, status in results if status)
    
    for item, detail, status in results:
        icon = f"{GREEN}✅{NC}" if status else f"{RED}❌{NC}"
        print(f"{icon} {item:20s} - {detail}")
    
    print(f"\n{BLUE}总计:{NC} {passed}/{total} 项通过")
    
    if passed == total:
        print(f"\n{GREEN}✅ 环境检查通过，可以运行闭环测试！{NC}")
        print(f"\n{BLUE}启动命令:{NC}")
        print(f"  cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators")
        print(f"  ./start_magnetic_closed_loop.sh")
    else:
        print(f"\n{RED}❌ 环境检查未通过，请先修复问题{NC}")
        
        # 提供修复建议
        print(f"\n{YELLOW}修复建议:{NC}")
        
        # DG-IoT服务器
        if not any(item == "DG-IoT服务器" and status for item, _, status in results):
            print(f"  1. 启动DG-IoT服务器:")
            print(f"     cd /root/gitee/dgiot && make run")
        
        ***REMOVED***绑定
        if not all(status for item, _, status in results if item.startswith("IP")):
            print(f"  2. 绑定IP地址:")
            print(f"     sudo ip addr add 192.168.100.20/24 dev eth0")
            print(f"     sudo ip addr add 192.168.100.21/24 dev eth0")
            print(f"     sudo ip addr add 192.168.100.23/24 dev eth0")
    
    print(f"\n{BLUE}{'='*70}{NC}\n")

def main():
    """主函数"""
    print_header()
    results = check_all()
    print_summary(results)
    
    # 返回退出码
    passed = sum(1 for _, _, status in results if status)
    total = len(results)
    sys.exit(0 if passed == total else 1)

if __name__ == "__main__":
    main()
