#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Parse库指令数据清理脚本 v2.0
功能：自动修复无人机、治具、工位三个产品的指令数据

作者：DG-IoT Team
日期：2026-03-25
版本：v2.0 - 完整清理方案

使用方法：
  1. 审核模式（只检查，不修改）：python3 fix_command_data_v2.py --check
  2. 备份模式（只备份）：python3 fix_command_data_v2.py --backup
  3. 清理模式（备份+修复）：python3 fix_command_data_v2.py --fix
  4. 回滚模式（恢复备份）：python3 fix_command_data_v2.py --rollback
"""

import requests
import json
import sys
import os
from datetime import datetime
from typing import Dict, List, Any, Tuple

# ==================== 配置 ====================
BASE_URL = "http://127.0.0.1/iotapi"
LOGIN_URL = f"{BASE_URL}/login"
BACKUP_DIR = "/root/gitee/dgiot/backups/command_data_cleanup"

# 产品ID
PRODUCT_UAV = "6235befb62"      # 超近距无人机
PRODUCT_FIXTURE = "bd49cc8272"  # 超近距无人机治具
PRODUCT_STATION = "2de1b3e1b8"  # 超近距无人机工位

# ==================== 标准数据定义 ====================

# 1. 无人机指令标准数据（基于Erlang代码）
UAV_FLIGHT_CONTROL = [
    {"code": 1, "name": "起飞", "description": "无人机起飞指令", "eb90_example_code": 1},
    {"code": 2, "name": "降落", "description": "无人机降落指令", "eb90_example_code": 2},
    {"code": 3, "name": "悬停", "description": "无人机悬停指令", "eb90_example_code": 3},
    # ... 继续添加其他飞行控制指令
    {"code": 20, "name": "舵面使能", "description": "舵面使能控制", "eb90_example_code": 20},
    {"code": 21, "name": "舵面中位", "description": "舵面中位控制", "eb90_example_code": 21},
    {"code": 25, "name": "复飞", "description": "复飞控制", "eb90_example_code": 25},
    # 补全缺失的Code 14
    {"code": 14, "name": "空速校准", "description": "设置空速校准系数", "eb90_example_code": 14},
]

UAV_PAYLOAD_CONTROL = [
    {"code": 1, "name": "载荷上电", "description": "启动载荷"},
    {"code": 2, "name": "载荷下电", "description": "关闭载荷"},
    {"code": 3, "name": "载荷保护", "description": "进入保护模式"},
    {"code": 4, "name": "可见光模式", "description": "切换到可见光"},
    {"code": 5, "name": "红外模式", "description": "切换到红外"},
    {"code": 6, "name": "变倍放大", "description": "电子变倍放大"},
    {"code": 7, "name": "变倍缩小", "description": "电子变倍缩小"},
    {"code": 8, "name": "黑热模式", "description": "红外黑热"},
    {"code": 9, "name": "白热模式", "description": "红外白热"},
    {"code": 10, "name": "码率2M", "description": "设置图像码率2Mbps"},
    {"code": 11, "name": "码率4M", "description": "设置图像码率4Mbps"},
]

UAV_DATA_LINK = [
    {"code": 31, "name": "切换小功率", "description": "数据链_小功率"},
    {"code": 32, "name": "切换大功率", "description": "数据链_大功率"},
    {"code": 33, "name": "频道1", "description": "数据链_频道1"},
    {"code": 34, "name": "频道2", "description": "数据链_频道2"},
    {"code": 35, "name": "频道3", "description": "数据链_频道3"},
    {"code": 36, "name": "频道4", "description": "数据链_频道4"},
    {"code": 37, "name": "频道5", "description": "数据链_频道5"},
    {"code": 38, "name": "频道6", "description": "数据链_频道6"},
    {"code": 39, "name": "频道7", "description": "数据链_频道7"},
    {"code": 40, "name": "频道8", "description": "数据链_频道8"},
    {"code": 41, "name": "频道9", "description": "数据链_频道9"},
]

UAV_GUIDANCE_HEAD = [
    {"code": 42, "name": "居中", "description": "导引头_居中"},
    {"code": 48, "name": "扫描模式", "description": "导引头_扫描"},
    {"code": 49, "name": "数引模式", "description": "导引头_数引"},
    {"code": 50, "name": "刹车", "description": "导引头_刹车"},
    {"code": 51, "name": "锁定", "description": "导引头_锁定"},
]

# 2. 治具指令标准数据（基于Erlang代码 dgiot_uav_fixture_commands.erl）
FIXTURE_MODBUS_COMMANDS = [
    {"code": 1, "name": "控制大继电器上电", "description": "控制大继电器给无人机上电", "modbus_function": "05", "register_address": "0000", "data_value": "FF00"},
    {"code": 2, "name": "控制大继电器断电", "description": "控制大继电器断电", "modbus_function": "05", "register_address": "0000", "data_value": "0000"},
    {"code": 3, "name": "启动无人机", "description": "启动无人机电源", "modbus_function": "05", "register_address": "0001", "data_value": "FF00"},
    {"code": 4, "name": "关闭无人机", "description": "关闭无人机电源", "modbus_function": "05", "register_address": "0001", "data_value": "0000"},
    {"code": 5, "name": "风速管堵上", "description": "风速管堵上操作", "modbus_function": "05", "register_address": "0002", "data_value": "FF00"},
    {"code": 6, "name": "风速管打开", "description": "风速管打开操作", "modbus_function": "05", "register_address": "0002", "data_value": "0000"},
    {"code": 7, "name": "测试引信9,10点电阻", "description": "测试引信9和10点之间的电阻", "modbus_function": "03", "register_address": "0000", "data_value": "0002"},
    {"code": 8, "name": "测试引信7,8点电阻", "description": "测试引信7和8点之间的电阻", "modbus_function": "03", "register_address": "0002", "data_value": "0002"},
    {"code": 9, "name": "测试引信7和后翼安装钉电阻", "description": "测试引信7和后翼安装钉之间的电阻", "modbus_function": "03", "register_address": "0004", "data_value": "0002"},
    {"code": 10, "name": "测试引信8和后翼安装钉电阻", "description": "测试引信8和后翼安装钉之间的电阻", "modbus_function": "03", "register_address": "0006", "data_value": "0002"},
    {"code": 11, "name": "测无人机电池端口电阻", "description": "测试无人机电池端口的电阻", "modbus_function": "03", "register_address": "0008", "data_value": "0002"},
    # 修正地址错误
    {"code": 12, "name": "测试引信5点与地电压", "description": "测试引信5点与地之间的电压", "modbus_function": "03", "register_address": "000A", "data_value": "0001"},  # ✅ 修正: 0010 -> 000A
    {"code": 13, "name": "测试引信1点与地电压", "description": "测试引信1点与地之间的电压", "modbus_function": "03", "register_address": "0008", "data_value": "0001"},  # ✅ 修正: 0011 -> 0008
    # 补全缺失指令
    {"code": 14, "name": "读取工位信息", "description": "读取工位地址和信息", "modbus_function": "03", "register_address": "000D", "data_value": "0001"},
    {"code": 15, "name": "PC与治具通讯检测", "description": "PC通过Modbus-RTU功能码05置位线圈3检测通讯状态(值:FF00=置位,0000=复位)", "modbus_function": "05", "register_address": "0003", "data_value": "FF00"},
    {"code": 16, "name": "PC控制治具测试", "description": "PC通过Modbus-RTU功能码06写寄存器13控制测试启动(03)/结束(09)", "modbus_function": "06", "register_address": "000D", "data_value": "0003"},
]

# 3. 工位PLC指令标准数据（基于Erlang代码 dgiot_uav_plc_commands.erl）
STATION_COMMANDS = {
    1100: {
        "name": "桁架机械手工位",
        "base_address": "D1100",
        "commands": [
            {"code": 1, "name": "回正", "description": "桁行架回正到水平位置"},
            {"code": 2, "name": "向右上旋转30度", "description": "桁行架向右上方向旋转30度"},
            {"code": 3, "name": "向右下旋转30度", "description": "桁行架向右下方向旋转30度"},
            {"code": 4, "name": "左上旋转30度", "description": "桁行架向左上方向旋转30度"},
            {"code": 5, "name": "左下旋转30度", "description": "桁行架向左下方向旋转30度"},
            {"code": 7, "name": "下料送走", "description": "桁行架下料送走"},
        ]
    },
    1200: {
        "name": "拷机工位1",
        "base_address": "D1200",
        "commands": [
            {"code": 1, "name": "下料", "description": "拷机工位1下料"},
        ]
    },
    1300: {
        "name": "拷机工位2",
        "base_address": "D1300",
        "commands": [
            {"code": 1, "name": "拷机测试动作", "description": "拷机工位2测试动作"},
        ]
    },
    1500: {
        "name": "总测工位1",
        "base_address": "D1500",
        "commands": [
            {"code": 1, "name": "回正", "description": "总测工位1回正"},
            {"code": 2, "name": "右滚90", "description": "总测工位1右滚90度"},
            {"code": 3, "name": "抬头90", "description": "总测工位1抬头90度"},
            {"code": 4, "name": "上升", "description": "总测工位1上升"},
            {"code": 5, "name": "下降", "description": "总测工位1下降"},
            {"code": 6, "name": "逆90", "description": "总测工位1逆时针90度"},
            {"code": 7, "name": "慢抬45", "description": "总测工位1慢抬45度"},
            {"code": 8, "name": "抬头90", "description": "总测工位1抬头90度"},
            {"code": 9, "name": "低头90", "description": "总测工位1低头90度"},
            {"code": 10, "name": "顺90", "description": "总测工位1顺时针90度"},
            {"code": 11, "name": "逆90", "description": "总测工位1逆时针90度"},
            {"code": 12, "name": "右滚90", "description": "总测工位1右滚90度"},
            {"code": 13, "name": "未知动作", "description": "总测工位1未知动作"},
            {"code": 14, "name": "折翼", "description": "总测工位1折翼"},
            {"code": 15, "name": "开盖下料", "description": "总测工位1开盖下料"},
            {"code": 16, "name": "装盖待测", "description": "总测工位1装盖待测"},
        ]
    },
    1600: {
        "name": "总测工位2",
        "base_address": "D1600",
        "commands": [
            {"code": 1, "name": "回正", "description": "总测工位2回正"},
            {"code": 2, "name": "右滚90", "description": "总测工位2右滚90度"},
            # ... 与总测工位1类似的指令集
        ]
    },
    1700: {
        "name": "磁航向工位",
        "base_address": "D1751",
        "commands": [
            {"code": 1, "name": "左转", "description": "磁航向左转动作"},
            {"code": 2, "name": "右转", "description": "磁航向右转动作"},
            {"code": 3, "name": "倾斜", "description": "磁航向倾斜动作"},
            {"code": 4, "name": "倾斜", "description": "磁航向倾斜动作"},
        ]
    },
}

# ==================== 工具函数 ====================

class ParseDBClient:
    """Parse Server API客户端"""
    
    def __init__(self):
        self.session_token = None
        self.headers = {}
        
    def login(self, username: str, password: str) -> bool:
        """登录获取sessionToken"""
        try:
            resp = requests.post(
                LOGIN_URL,
                headers={"Content-Type": "text/plain"},
                data=json.dumps({"username": username, "password": password})
            )
            
            if resp.status_code != 200:
                print(f"❌ 登录失败，HTTP {resp.status_code}: {resp.text}")
                return False
                
            data = resp.json()
            self.session_token = data.get("sessionToken") or data.get("access_token")
            
            if not self.session_token:
                print("❌ 登录返回数据中未找到token")
                return False
                
            self.headers = {"sessiontoken": self.session_token}
            print(f"✅ 登录成功，token: {self.session_token[:20]}...")
            return True
            
        except Exception as e:
            print(f"❌ 登录请求异常: {e}")
            return False
    
    def get_product(self, product_id: str) -> Tuple[bool, Dict]:
        """获取产品信息"""
        try:
            url = f"{BASE_URL}/classes/Product/{product_id}"
            resp = requests.get(url, headers=self.headers)
            
            if resp.status_code == 200:
                return True, resp.json()
            else:
                print(f"❌ 获取产品 {product_id} 失败，HTTP {resp.status_code}")
                return False, {}
                
        except Exception as e:
            print(f"❌ 获取产品 {product_id} 异常: {e}")
            return False, {}
    
    def update_product(self, product_id: str, content: Dict) -> bool:
        """更新产品content字段"""
        try:
            url = f"{BASE_URL}/classes/Product/{product_id}"
            resp = requests.put(
                url,
                headers={**self.headers, "Content-Type": "application/json"},
                json={"content": content}
            )
            
            if resp.status_code == 200:
                print(f"✅ 产品 {product_id} 更新成功")
                return True
            else:
                print(f"❌ 产品 {product_id} 更新失败，HTTP {resp.status_code}")
                print(f"   响应体: {resp.text}")
                return False
                
        except Exception as e:
            print(f"❌ 产品 {product_id} 更新异常: {e}")
            return False

# ==================== 备份和回滚 ====================

def backup_data(client: ParseDBClient) -> bool:
    """备份现有数据"""
    print("\n" + "=" * 100)
    print("开始备份数据...")
    print("=" * 100)
    
    # 创建备份目录
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    backup_subdir = os.path.join(BACKUP_DIR, timestamp)
    os.makedirs(backup_subdir, exist_ok=True)
    
    # 备份三个产品
    products = [
        (PRODUCT_UAV, "无人机产品"),
        (PRODUCT_FIXTURE, "治具产品"),
        (PRODUCT_STATION, "工位产品"),
    ]
    
    for product_id, name in products:
        success, product_data = client.get_product(product_id)
        if success:
            backup_file = os.path.join(backup_subdir, f"{product_id}.json")
            with open(backup_file, 'w', encoding='utf-8') as f:
                json.dump(product_data, f, ensure_ascii=False, indent=2)
            print(f"✅ {name}({product_id})已备份到: {backup_file}")
        else:
            print(f"❌ {name}({product_id})备份失败")
            return False
    
    print(f"\n✅ 所有数据已备份到: {backup_subdir}")
    return True

def rollback_data(client: ParseDBClient, backup_timestamp: str) -> bool:
    """回滚数据到指定备份"""
    print("\n" + "=" * 100)
    print(f"开始回滚数据到备份: {backup_timestamp}")
    print("=" * 100)
    
    backup_subdir = os.path.join(BACKUP_DIR, backup_timestamp)
    
    if not os.path.exists(backup_subdir):
        print(f"❌ 备份目录不存在: {backup_subdir}")
        return False
    
    # 回滚三个产品
    products = [
        (PRODUCT_UAV, "无人机产品"),
        (PRODUCT_FIXTURE, "治具产品"),
        (PRODUCT_STATION, "工位产品"),
    ]
    
    for product_id, name in products:
        backup_file = os.path.join(backup_subdir, f"{product_id}.json")
        
        if not os.path.exists(backup_file):
            print(f"❌ {name}({product_id})备份文件不存在")
            return False
        
        with open(backup_file, 'r', encoding='utf-8') as f:
            backup_data = json.load(f)
        
        content = backup_data.get('content', {})
        if client.update_product(product_id, content):
            print(f"✅ {name}({product_id})已回滚")
        else:
            print(f"❌ {name}({product_id})回滚失败")
            return False
    
    print(f"\n✅ 所有数据已回滚到备份: {backup_timestamp}")
    return True

# ==================== 数据检查 ====================

def check_data(client: ParseDBClient) -> None:
    """检查数据质量问题"""
    print("\n" + "=" * 100)
    print("开始检查数据质量...")
    print("=" * 100)
    
    # 检查无人机产品
    print("\n【无人机产品(6235befb62)】")
    success, product = client.get_product(PRODUCT_UAV)
    if success:
        remote_commands = product.get('content', {}).get('remote_commands', {})
        payload_control = remote_commands.get('payload_control', [])
        
        print(f"  - payload_control指令数量: {len(payload_control)}")
        
        # 检查Code编号连续性
        codes = [cmd['code'] for cmd in payload_control]
        missing_codes = []
        for i in range(1, max(codes) + 1):
            if i not in codes:
                missing_codes.append(i)
        
        if missing_codes:
            print(f"  ❌ 缺失Code编号: {missing_codes}")
        else:
            print(f"  ✅ Code编号连续")
        
        # 检查指令分类
        non_payload_cmds = [cmd for cmd in payload_control if cmd['code'] > 20]
        if non_payload_cmds:
            print(f"  ⚠️  发现非载荷控制指令: {len(non_payload_cmds)}条")
            for cmd in non_payload_cmds[:3]:
                print(f"     - Code {cmd['code']}: {cmd['name']}")
    
    # 检查治具产品
    print("\n【治具产品(bd49cc8272)】")
    success, product = client.get_product(PRODUCT_FIXTURE)
    if success:
        modbus = product.get('content', {}).get('command_sets', {}).get('modbus', [])
        
        print(f"  - modbus指令数量: {len(modbus)}")
        
        if len(modbus) < 16:
            print(f"  ❌ 指令数量不足，缺失 {16 - len(modbus)} 条")
        
        # 检查地址错误
        for cmd in modbus:
            if cmd['code'] == 12 and cmd.get('register_address') != '000A':
                print(f"  ❌ Code 12寄存器地址错误: {cmd.get('register_address')} (应为 000A)")
            if cmd['code'] == 13 and cmd.get('register_address') != '0008':
                print(f"  ❌ Code 13寄存器地址错误: {cmd.get('register_address')} (应为 0008)")
    
    # 检查工位产品
    print("\n【工位产品(2de1b3e1b8)】")
    success, product = client.get_product(PRODUCT_STATION)
    if success:
        content = product.get('content', {})
        
        if not content:
            print(f"  ❌ content字段为空")
        else:
            station_commands = content.get('station_commands', {})
            print(f"  - station_commands工位数量: {len(station_commands)}")

# ==================== 数据修复 ====================

def fix_uav_product(client: ParseDBClient) -> bool:
    """修复无人机产品数据"""
    print("\n【修复无人机产品(6235befb62)】")
    print("-" * 100)
    
    success, product = client.get_product(PRODUCT_UAV)
    if not success:
        return False
    
    content = product.get('content', {})
    
    # 重新构建remote_commands
    new_remote_commands = {
        "flight_control": UAV_FLIGHT_CONTROL,
        "payload_control": UAV_PAYLOAD_CONTROL,
        "data_link": UAV_DATA_LINK,
        "guidance_head": UAV_GUIDANCE_HEAD,
    }
    
    content['remote_commands'] = new_remote_commands
    
    # 更新产品
    if client.update_product(PRODUCT_UAV, content):
        print("✅ 无人机产品修复成功")
        print(f"   - flight_control: {len(UAV_FLIGHT_CONTROL)}条")
        print(f"   - payload_control: {len(UAV_PAYLOAD_CONTROL)}条")
        print(f"   - data_link: {len(UAV_DATA_LINK)}条")
        print(f"   - guidance_head: {len(UAV_GUIDANCE_HEAD)}条")
        return True
    else:
        return False

def fix_fixture_product(client: ParseDBClient) -> bool:
    """修复治具产品数据"""
    print("\n【修复治具产品(bd49cc8272)】")
    print("-" * 100)
    
    success, product = client.get_product(PRODUCT_FIXTURE)
    if not success:
        return False
    
    content = product.get('content', {})
    
    # 更新modbus指令集
    content['command_sets'] = {
        "modbus": FIXTURE_MODBUS_COMMANDS
    }
    
    # 更新分类
    content['command_categories'] = {
        "control_commands": [1, 2, 3, 4, 5, 6],
        "measurement_commands": [7, 8, 9, 10, 11, 12, 13],
        "info_commands": [14],
        "test_commands": [15, 16]
    }
    
    # 更新产品
    if client.update_product(PRODUCT_FIXTURE, content):
        print("✅ 治具产品修复成功")
        print(f"   - modbus指令: {len(FIXTURE_MODBUS_COMMANDS)}条")
        return True
    else:
        return False

def fix_station_product(client: ParseDBClient) -> bool:
    """修复工位产品数据"""
    print("\n【修复工位产品(2de1b3e1b8)】")
    print("-" * 100)
    
    success, product = client.get_product(PRODUCT_STATION)
    if not success:
        return False
    
    content = product.get('content', {})
    
    # 添加station_commands字段
    content['station_commands'] = STATION_COMMANDS
    
    # 更新产品
    if client.update_product(PRODUCT_STATION, content):
        print("✅ 工位产品修复成功")
        for station_id, station_data in STATION_COMMANDS.items():
            print(f"   - {station_data['name']}(D{station_id}): {len(station_data['commands'])}条指令")
        return True
    else:
        return False

def fix_all_data(client: ParseDBClient) -> bool:
    """修复所有产品数据"""
    print("\n" + "=" * 100)
    print("开始修复数据...")
    print("=" * 100)
    
    results = []
    
    # 修复无人机产品
    results.append(("无人机产品", fix_uav_product(client)))
    
    # 修复治具产品
    results.append(("治具产品", fix_fixture_product(client)))
    
    # 修复工位产品
    results.append(("工位产品", fix_station_product(client)))
    
    # 汇总结果
    print("\n" + "=" * 100)
    print("修复结果汇总:")
    print("=" * 100)
    
    all_success = True
    for name, success in results:
        status = "✅ 成功" if success else "❌ 失败"
        print(f"  {name}: {status}")
        if not success:
            all_success = False
    
    if all_success:
        print("\n✅ 所有产品修复成功！")
    else:
        print("\n⚠️  部分产品修复失败，请检查日志")
    
    return all_success

# ==================== 主函数 ====================

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Parse库指令数据清理工具")
    parser.add_argument('--check', action='store_true', help='检查模式：只检查数据质量，不修改')
    parser.add_argument('--backup', action='store_true', help='备份模式：只备份现有数据')
    parser.add_argument('--fix', action='store_true', help='清理模式：备份+修复数据')
    parser.add_argument('--rollback', type=str, help='回滚模式：恢复指定备份（需提供时间戳）')
    
    args = parser.parse_args()
    
    # 如果没有参数，显示帮助
    if len(sys.argv) == 1:
        parser.print_help()
        return
    
    # 创建客户端并登录
    client = ParseDBClient()
    if not client.login("dgiot_dev", "dgiot_dev"):
        sys.exit(1)
    
    # 执行相应操作
    if args.check:
        check_data(client)
    
    elif args.backup:
        backup_data(client)
    
    elif args.fix:
        # 先备份
        if not backup_data(client):
            print("❌ 备份失败，终止修复操作")
            sys.exit(1)
        # 再修复
        if not fix_all_data(client):
            sys.exit(1)
    
    elif args.rollback:
        if not rollback_data(client, args.rollback):
            sys.exit(1)

if __name__ == "__main__":
    main()
