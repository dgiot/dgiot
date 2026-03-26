#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
更新Parse数据库中的工位指令
"""

import json
import requests
from datetime import datetime

PARSE_SERVER_URL = "http://192.168.8.153:8080"
PRODUCT_ID = "2de1b3e1b8"
UPDATED_FILE = '/root/gitee/dgiot/backups/parse_db/Product_2de1b3e1b8_final.json'

def update_parse_database():
    """更新Parse数据库"""
    print("=" * 80)
    print("🔧 Parse数据库更新工具")
    print("=" * 80)
    print()

    # 读取更新后的文件
    print("📋 步骤1：读取更新后的配置文件")
    print("-" * 80)
    with open(UPDATED_FILE, 'r', encoding='utf-8') as f:
        product_data = json.load(f)

    product_id = product_data.get('objectId', '')
    station_commands = product_data.get('content', {}).get('station_commands', {})

    print(f"✅ 产品ID: {product_id}")
    print(f"✅ 工位数量: {len(station_commands)}")
    print()

    # 准备更新数据
    print("📋 步骤2：准备更新数据")
    print("-" * 80)
    
    update_data = {
        "content": {
            "station_commands": station_commands
        }
    }

    # 读取当前产品数据以获取updatedAt
    print("📋 步骤3：获取当前产品数据")
    print("-" * 80)
    get_url = f"{PARSE_SERVER_URL}/parse/classes/Product/{PRODUCT_ID}"
    
    try:
        response = requests.get(get_url)
        if response.status_code == 200:
            current_data = response.json()
            updated_at = current_data.get('updatedAt')
            print(f"✅ 当前产品数据获取成功")
            print(f"✅ updatedAt: {updated_at}")
            update_data['updatedAt'] = updated_at
        else:
            print(f"❌ 获取产品数据失败: {response.status_code}")
            print(response.text)
            return
    except Exception as e:
        print(f"❌ 获取产品数据异常: {e}")
        return

    print()

    # 更新产品数据
    print("📋 步骤4：更新Parse数据库")
    print("-" * 80)
    update_url = f"{PARSE_SERVER_URL}/parse/classes/Product/{PRODUCT_ID}"
    
    headers = {
        'Content-Type': 'application/json'
    }

    print(f"更新URL: {update_url}")
    print(f"更新数据内容:")
    print(f"  - 工位1100: {len(station_commands.get('1100', {}).get('commands', []))} 条指令")
    print(f"  - 工位1200: {len(station_commands.get('1200', {}).get('commands', []))} 条指令")
    print(f"  - 工位1300: {len(station_commands.get('1300', {}).get('commands', []))} 条指令")
    print(f"  - 工位1500: {len(station_commands.get('1500', {}).get('commands', []))} 条指令")
    print(f"  - 工位1600: {len(station_commands.get('1600', {}).get('commands', []))} 条指令")
    print(f"  - 工位1700: {len(station_commands.get('1700', {}).get('commands', []))} 条指令")
    print()

    try:
        response = requests.put(update_url, json=update_data, headers=headers)
        
        if response.status_code == 200:
            print("✅ Parse数据库更新成功！")
            result = response.json()
            print(f"✅ updatedAt: {result.get('updatedAt')}")
            print()
            print("=" * 80)
            print("✅ 工位指令更新完成")
            print("=" * 80)
        else:
            print(f"❌ Parse数据库更新失败: {response.status_code}")
            print(response.text)
    except Exception as e:
        print(f"❌ Parse数据库更新异常: {e}")

if __name__ == '__main__':
    update_parse_database()
