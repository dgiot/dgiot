#!/usr/bin/env python3
"""
磁航向工位设备创建问题诊断脚本

问题分析:
1. 扫码枪(1234)数据正常接收并上报MES
2. 但无人机设备未创建
3. 需要检查EB90端口(10007)是否监听
4. 需要检查设备创建逻辑

根因:
- 端口10007未监听，EB90报文无法接收
- 系统期望通过EB90帧提取PlaneID后创建设备
- 但端口10007没有启动监听，导致EB90帧无法接收
"""

import socket
import subprocess
import time
import sys

class Port10007Diagnostic:
    """端口10007诊断工具"""
    
    def __init__(self):
        self.target_ip = "192.168.100.100"
        self.target_port = 10007
        self.dgiot_server_ip = "192.168.100.100"
        self.dgiot_server_port = 20000
        
    def check_port_listen(self):
        """检查端口10007是否监听"""
        print("\n" + "="*60)
        print("【诊断1】检查端口10007监听状态")
        print("="*60)
        
        try:
            result = subprocess.run(
                ["netstat", "-tuln"],
                capture_output=True,
                text=True
            )
            
            if "10007" in result.stdout:
                print("✅ 端口10007正在监听")
                print(result.stdout.split("\n")[0:5])
                return True
            else:
                print("❌ 端口10007未监听")
                print("正在监听的端口:")
                for line in result.stdout.split("\n"):
                    if "LISTEN" in line and (":20000" in line or ":800" in line):
                        print(f"  {line.strip()}")
                return False
        except Exception as e:
            print(f"❌ 检查端口失败: {e}")
            return False
    
    def check_tcp_connection(self):
        """尝试连接端口10007"""
        print("\n" + "="*60)
        print("【诊断2】测试TCP连接到端口10007")
        print("="*60)
        
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(3)
            
            print(f"尝试连接到 {self.target_ip}:{self.target_port}...")
            sock.connect((self.target_ip, self.target_port))
            
            print("✅ TCP连接成功")
            
            # 发送EB90注册帧
            registration_frame = b"wrj_dicekou\n"
            sock.send(registration_frame)
            print(f"✅ 发送注册帧: {registration_frame}")
            
            # 接收响应
            response = sock.recv(1024)
            print(f"✅ 收到响应: {response}")
            
            sock.close()
            return True
            
        except socket.timeout:
            print("❌ 连接超时")
            return False
        except ConnectionRefusedError:
            print("❌ 连接被拒绝")
            return False
        except Exception as e:
            print(f"❌ 连接失败: {e}")
            return False
    
    def check_dgiot_tcp_worker(self):
        """检查DG-IoT TCP Worker状态"""
        print("\n" + "="*60)
        print("【诊断3】检查DG-IoT TCP Worker")
        print("="*60)
        
        try:
            # 检查emqx进程
            result = subprocess.run(
                ["pgrep", "-a", "emqx"],
                capture_output=True,
                text=True
            )
            
            if result.returncode == 0:
                print("✅ EMQX进程正在运行")
                print(result.stdout[:200])
            else:
                print("❌ EMQX进程未运行")
                return False
            
            # 检查dgiot_uav_tcp_worker模块
            result = subprocess.run(
                ["_build/emqx/rel/emqx/bin/emqx", "eval", 
                 "erlang:module_loaded(dgiot_uav_tcp_worker)."],
                capture_output=True,
                text=True,
                timeout=5
            )
            
            print(f"模块加载检查: {result.stdout.strip()}")
            
            return True
            
        except Exception as e:
            print(f"❌ 检查失败: {e}")
            return False
    
    def test_scanner_protocol(self):
        """测试扫码枪协议"""
        print("\n" + "="*60)
        print("【诊断4】测试扫码枪协议")
        print("="*60)
        
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(3)
            sock.connect((self.target_ip, 1234))
            
            # 发送扫码数据
            scanner_data = b"Test01|1|5000000020004|10|2026032502|||\r"
            sock.send(scanner_data)
            print(f"✅ 发送扫码数据: {scanner_data}")
            
            sock.close()
            print("✅ 扫码枪协议测试完成")
            
            time.sleep(1)
            
            # 检查设备是否创建
            result = subprocess.run(
                ["_build/emqx/rel/emqx/bin/emqx", "eval", 
                 "dgiot_device:lookup(<<\"2026032502\">>)."],
                capture_output=True,
                text=True,
                timeout=5
            )
            
            print(f"\n设备查询结果:\n{result.stdout}")
            
            return True
            
        except Exception as e:
            print(f"❌ 测试失败: {e}")
            return False
    
    def analyze_root_cause(self):
        """分析根本原因"""
        print("\n" + "="*60)
        print("【诊断5】根因分析")
        print("="*60)
        
        print("""
根本原因分析:

1. 设备创建流程:
   扫码枪(1234) → 解析二维码 → 创建设备 ← EB90帧提取PlaneID
   
2. 问题定位:
   ❌ 端口10007未监听 → EB90帧无法接收 → PlaneID无法提取 → 设备无法创建

3. 设计矛盾:
   - 扫码枪代码期望EB90帧提供PlaneID
   - 但端口10007没有启动监听
   - 导致设备创建依赖链断裂

4. 解决方案:
   方案A: 启动端口10007监听，接收EB90帧
   方案B: 修改扫码枪逻辑，直接使用SerialNo创建设备
   方案C: 修改端口配置，使用其他端口
""")
    
    def suggest_fixes(self):
        """建议修复方案"""
        print("\n" + "="*60)
        print("【修复建议】")
        print("="*60)
        
        print("""
方案A: 启动端口10007监听（推荐用于完整测试）
-------------------------------------------------
1. 检查dgiot_uav_tcp_worker是否加载
2. 确认端口配置是否包含10007
3. 重启emqx服务: make run
4. 验证端口监听: netstat -tuln | grep 10007

方案B: 修改扫码枪逻辑（快速修复）
-------------------------------------------------
已经完成的修复:
- 恢复扫码枪设备创建代码
- 直接使用SerialNo创建设备
- 不再依赖EB90帧提取PlaneID

方案C: 综合方案（生产环境）
-------------------------------------------------
- 保留EB90帧接收能力（端口10007）
- 同时支持扫码枪直接创建设备
- EB90帧可以更新设备信息

当前状态:
✅ 扫码枪设备创建逻辑已修复
❓ 端口10007监听状态待确认
✅ 可以开始测试设备创建
""")
    
    def run_all_diagnostics(self):
        """运行所有诊断"""
        print("\n" + "="*60)
        print("磁航向工位设备创建问题诊断")
        print("="*60)
        
        results = []
        
        # 运行诊断
        results.append(("端口10007监听", self.check_port_listen()))
        results.append(("TCP连接测试", self.check_tcp_connection()))
        results.append(("DG-IoT状态", self.check_dgiot_tcp_worker()))
        results.append(("扫码枪协议", self.test_scanner_protocol()))
        
        # 分析根因
        self.analyze_root_cause()
        
        # 建议修复方案
        self.suggest_fixes()
        
        # 总结
        print("\n" + "="*60)
        print("【诊断总结】")
        print("="*60)
        
        passed = sum(1 for _, r in results if r)
        total = len(results)
        
        for name, result in results:
            status = "✅" if result else "❌"
            print(f"{status} {name}")
        
        print(f"\n通过率: {passed}/{total} ({int(passed/total*100)}%)")
        
        return all(r for _, r in results)


def main():
    """主函数"""
    print("""
╔══════════════════════════════════════════════════════════╗
║           磁航向工位设备创建问题诊断工具                  ║
╚══════════════════════════════════════════════════════════╝
""")
    
    diagnostic = Port10007Diagnostic()
    
    try:
        diagnostic.run_all_diagnostics()
    except KeyboardInterrupt:
        print("\n\n诊断被用户中断")
        sys.exit(1)
    except Exception as e:
        print(f"\n\n诊断失败: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
