#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Parse库完整备份脚本
功能：将Parse库中的所有测试项和产品数据备份到本地JSON文件

作者：DG-IoT Team
日期：2026-03-25
原则：Parse库是唯一数据源，本地JSON仅作备份

使用方法：
  python3 backup_parse_to_json.py [--output-dir /path/to/backup]
"""

import requests
import json
import sys
import os
from datetime import datetime
from typing import Dict, List, Any

# ==================== 配置 ====================
BASE_URL = "http://127.0.0.1/iotapi"
LOGIN_URL = f"{BASE_URL}/login"

# 产品ID
PRODUCT_UAV = "6235befb62"      # 超近距无人机
PRODUCT_FIXTURE = "bd49cc8272"  # 超近距无人机治具
PRODUCT_STATION = "2de1b3e1b8"  # 超近距无人机工位

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
                print(f"❌ 登录失败，HTTP {resp.status_code}")
                return False
                
            data = resp.json()
            self.session_token = data.get("sessionToken") or data.get("access_token")
            
            if not self.session_token:
                print("❌ 登录返回数据中未找到token")
                return False
                
            self.headers = {"sessiontoken": self.session_token}
            print(f"✅ 登录成功")
            return True
            
        except Exception as e:
            print(f"❌ 登录请求异常: {e}")
            return False
    
    def get_product(self, product_id: str) -> Dict:
        """获取产品信息"""
        try:
            url = f"{BASE_URL}/classes/Product/{product_id}"
            resp = requests.get(url, headers=self.headers)
            
            if resp.status_code == 200:
                return resp.json()
            else:
                print(f"❌ 获取产品 {product_id} 失败")
                return {}
                
        except Exception as e:
            print(f"❌ 获取产品异常: {e}")
            return {}
    
    def get_all_devices(self, limit: int = 1000) -> List[Dict]:
        """获取所有设备"""
        try:
            url = f"{BASE_URL}/classes/Device"
            params = {"limit": limit}
            resp = requests.get(url, headers=self.headers, params=params)
            
            if resp.status_code == 200:
                return resp.json().get('results', [])
            else:
                print(f"❌ 获取设备列表失败")
                return []
                
        except Exception as e:
            print(f"❌ 获取设备列表异常: {e}")
            return []
    
    def get_test_items(self) -> List[Dict]:
        """获取所有测试项设备"""
        all_devices = self.get_all_devices()
        test_items = [d for d in all_devices if d.get('content', {}).get('steps')]
        return test_items

# ==================== 备份函数 ====================

def backup_parse_data(client: ParseDBClient, output_dir: str) -> bool:
    """备份Parse库数据到JSON文件"""
    
    print("\n" + "=" * 100)
    print("开始备份Parse库数据...")
    print("=" * 100)
    
    # 创建备份目录
    os.makedirs(output_dir, exist_ok=True)
    
    # 1. 备份三个产品
    print("\n【备份产品数据】")
    print("-" * 100)
    
    products = [
        ("无人机产品", PRODUCT_UAV),
        ("治具产品", PRODUCT_FIXTURE),
        ("工位产品", PRODUCT_STATION)
    ]
    
    for name, product_id in products:
        product = client.get_product(product_id)
        
        if product:
            filename = os.path.join(output_dir, f"Product_{product_id}.json")
            with open(filename, 'w', encoding='utf-8') as f:
                json.dump(product, f, ensure_ascii=False, indent=2)
            
            # 统计指令数量
            content = product.get('content', {})
            
            if product_id == PRODUCT_UAV:
                remote_commands = content.get('remote_commands', {})
                total_cmds = sum(len(cmds) for cmds in remote_commands.values() if isinstance(cmds, list))
            elif product_id == PRODUCT_FIXTURE:
                modbus = content.get('command_sets', {}).get('modbus', [])
                total_cmds = len(modbus)
            elif product_id == PRODUCT_STATION:
                station_commands = content.get('station_commands', {})
                total_cmds = sum(len(s.get('commands', [])) for s in station_commands.values())
            else:
                total_cmds = 0
            
            print(f"✅ {name}({product_id}): {total_cmds}个指令 -> {filename}")
        else:
            print(f"❌ {name}({product_id}): 备份失败")
    
    # 2. 备份所有测试项设备
    print("\n【备份测试项设备】")
    print("-" * 100)
    
    test_items = client.get_test_items()
    
    if test_items:
        # 按工位分组
        test_items_by_station = {}
        
        for item in test_items:
            devaddr = item.get('devaddr', '')
            station = devaddr.split('_')[0] if '_' in devaddr else 'Unknown'
            
            if station not in test_items_by_station:
                test_items_by_station[station] = []
            
            test_items_by_station[station].append(item)
        
        # 保存分组文件
        for station, items in test_items_by_station.items():
            filename = os.path.join(output_dir, f"TestItems_{station}.json")
            with open(filename, 'w', encoding='utf-8') as f:
                json.dump(items, f, ensure_ascii=False, indent=2)
            
            print(f"✅ {station}: {len(items)}个测试项 -> {filename}")
        
        # 保存完整列表
        all_filename = os.path.join(output_dir, "TestItems_All.json")
        with open(all_filename, 'w', encoding='utf-8') as f:
            json.dump(test_items, f, ensure_ascii=False, indent=2)
        
        print(f"\n✅ 总计: {len(test_items)}个测试项 -> {all_filename}")
    
    # 3. 生成备份摘要
    print("\n【生成备份摘要】")
    print("-" * 100)
    
    summary = {
        "backup_time": datetime.now().isoformat(),
        "products": {
            PRODUCT_UAV: {"name": "无人机产品", "command_count": "见文件"},
            PRODUCT_FIXTURE: {"name": "治具产品", "command_count": "见文件"},
            PRODUCT_STATION: {"name": "工位产品", "command_count": "见文件"}
        },
        "test_items": {
            "total_count": len(test_items),
            "by_station": {station: len(items) for station, items in test_items_by_station.items()}
        },
        "files_created": [
            f"Product_{PRODUCT_UAV}.json",
            f"Product_{PRODUCT_FIXTURE}.json",
            f"Product_{PRODUCT_STATION}.json",
            "TestItems_All.json"
        ] + [f"TestItems_{station}.json" for station in test_items_by_station.keys()]
    }
    
    summary_filename = os.path.join(output_dir, "Backup_Summary.json")
    with open(summary_filename, 'w', encoding='utf-8') as f:
        json.dump(summary, f, ensure_ascii=False, indent=2)
    
    print(f"✅ 备份摘要 -> {summary_filename}")
    
    print("\n" + "=" * 100)
    print(f"✅ Parse库数据备份完成！")
    print(f"备份目录: {output_dir}")
    print("=" * 100)
    
    return True

# ==================== 主函数 ====================

def main():
    import argparse
    
    parser = argparse.ArgumentParser(description="Parse库完整备份工具")
    parser.add_argument('--output-dir', default='/root/gitee/dgiot/backups/parse_db', 
                       help='备份输出目录')
    
    args = parser.parse_args()
    
    # 创建客户端并登录
    client = ParseDBClient()
    if not client.login("dgiot_dev", "dgiot_dev"):
        sys.exit(1)
    
    # 执行备份
    if not backup_parse_data(client, args.output_dir):
        sys.exit(1)

if __name__ == "__main__":
    main()
