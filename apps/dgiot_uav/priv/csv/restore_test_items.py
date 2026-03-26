#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
恢复测试项从备份JSON文件到Parse服务器
"""

import requests
import json
import sys
import os

# ========== 配置 ==========
BASE_URL = "http://127.0.0.1/iotapi"
LOGIN_URL = f"{BASE_URL}/login"
USERNAME = "dgiot_dev"
PASSWORD = "dgiot_dev"
PRODUCT_TEST_ITEM = "343cf21f82"   # 测试项产品 ID
BACKUP_FILE = "backups/test_items_backup_20260312_164639.json"  # 要恢复的备份文件

def login_and_get_token():
    print("正在登录...")
    headers = {"Content-Type": "text/plain"}
    payload = json.dumps({"username": USERNAME, "password": PASSWORD})
    try:
        resp = requests.post(LOGIN_URL, headers=headers, data=payload)
        if resp.status_code != 200:
            print(f"登录失败，HTTP {resp.status_code}: {resp.text}")
            return None
        data = resp.json()
        token = data.get("sessionToken") or data.get("access_token")
        if not token:
            print("登录返回数据中未找到 token")
            return None
        print(f"登录成功，token: {token[:20]}...")
        return token
    except Exception as e:
        print(f"登录请求异常: {e}")
        return None

def query_device_by_devaddr(devaddr, token):
    """根据devaddr查询设备是否存在"""
    where = {
        "devaddr": devaddr,
        "product": {"__type": "Pointer", "className": "Product", "objectId": PRODUCT_TEST_ITEM}
    }
    url = f"{BASE_URL}/classes/Device"
    headers = {"sessiontoken": token}
    params = {"where": json.dumps(where), "limit": 1}
    try:
        resp = requests.get(url, headers=headers, params=params)
        if resp.status_code != 200:
            return None
        data = resp.json()
        results = data.get("results", [])
        return results[0] if results else None
    except Exception as e:
        print(f"查询设备异常: {e}")
        return None

def create_device(device_data, token):
    """创建设备"""
    url = f"{BASE_URL}/classes/Device"
    headers = {"sessiontoken": token, "Content-Type": "application/json"}
    
    # 构建设备数据（只保留必要字段）
    device = {
        "name": device_data.get("name", ""),
        "devaddr": device_data.get("devaddr", ""),
        "product": {"__type": "Pointer", "className": "Product", "objectId": PRODUCT_TEST_ITEM},
        "address": device_data.get("address", ""),
        "ip": device_data.get("ip", ""),
        "isEnable": device_data.get("isEnable", True),
        "content": device_data.get("content", {})
    }
    
    try:
        resp = requests.post(url, headers=headers, json=device)
        if resp.status_code == 201 or resp.status_code == 200:
            result = resp.json()
            print(f"  ✅ 创建成功: {device_data.get('name')} - {result.get('objectId')}")
            return result.get("objectId")
        else:
            print(f"  ❌ 创建失败: {device_data.get('name')} - {resp.status_code} {resp.text}")
            return None
    except Exception as e:
        print(f"  ❌ 创建异常: {device_data.get('name')} - {e}")
        return None

def update_device(device_id, device_data, token):
    """更新设备"""
    url = f"{BASE_URL}/classes/Device/{device_id}"
    headers = {"sessiontoken": token, "Content-Type": "application/json"}
    
    # 更新content字段
    device = {
        "name": device_data.get("name", ""),
        "content": device_data.get("content", {})
    }
    
    try:
        resp = requests.put(url, headers=headers, json=device)
        if resp.status_code == 200:
            print(f"  ✅ 更新成功: {device_data.get('name')}")
            return True
        else:
            print(f"  ❌ 更新失败: {device_data.get('name')} - {resp.status_code}")
            return False
    except Exception as e:
        print(f"  ❌ 更新异常: {device_data.get('name')} - {e}")
        return False

def restore_from_backup(backup_file):
    """从备份文件恢复测试项"""
    # 加载备份数据
    if not os.path.exists(backup_file):
        print(f"❌ 备份文件不存在: {backup_file}")
        return
    
    with open(backup_file, 'r', encoding='utf-8') as f:
        devices = json.load(f)
    
    print(f"📁 加载备份文件: {backup_file}")
    print(f"📊 共 {len(devices)} 条测试项")
    print()
    
    # 登录
    token = login_and_get_token()
    if not token:
        return
    
    print()
    print("🔄 开始恢复测试项...")
    
    created = 0
    updated = 0
    skipped = 0
    
    for i, device in enumerate(devices):
        name = device.get("name", "未命名")
        devaddr = device.get("devaddr", "")
        
        print(f"[{i+1}/{len(devices)}] 处理: {name}")
        
        # 检查是否已存在
        existing = query_device_by_devaddr(devaddr, token)
        
        if existing:
            # 已存在，更新
            update_device(existing.get("objectId"), device, token)
            updated += 1
        else:
            # 不存在，创建
            result = create_device(device, token)
            if result:
                created += 1
            else:
                skipped += 1
    
    print()
    print("=" * 50)
    print(f"✅ 恢复完成!")
    print(f"   新建: {created} 条")
    print(f"   更新: {updated} 条")
    print(f"   跳过: {skipped} 条")
    print("=" * 50)

if __name__ == "__main__":
    # 可以指定备份文件路径
    if len(sys.argv) > 1:
        backup_file = sys.argv[1]
    else:
        backup_file = BACKUP_FILE
    
    restore_from_backup(backup_file)
