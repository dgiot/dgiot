#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import requests
import json
import sys
import re

# ========== 配置 ==========
BASE_URL = "http://127.0.0.1/iotapi"
LOGIN_URL = f"{BASE_URL}/login"
USERNAME = "dgiot_dev"
PASSWORD = "dgiot_dev"

PRODUCT_TEST_ITEM = "343cf21f82"   # 测试项产品 ID

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
        print(f"登录成功，token: {token}")
        return token
    except Exception as e:
        print(f"登录请求异常: {e}")
        return None

def query_devices(where, token):
    """通用设备查询"""
    url = f"{BASE_URL}/classes/Device"
    headers = {"sessiontoken": token}
    params = {"where": json.dumps(where), "limit": 200}
    try:
        resp = requests.get(url, headers=headers, params=params)
        if resp.status_code == 200:
            data = resp.json()
            return data.get("results", [])
        else:
            print(f"查询设备失败: {resp.status_code}, {resp.text}")
            return []
    except Exception as e:
        print(f"查询设备异常: {e}")
        return []

def create_device(devaddr, content, token):
    """创建新设备（测试项）"""
    url = f"{BASE_URL}/classes/Device"
    headers = {"sessiontoken": token, "Content-Type": "application/json"}
    payload = {
        "devaddr": devaddr,
        "product": {
            "__type": "Pointer",
            "className": "Product",
            "objectId": PRODUCT_TEST_ITEM
        },
        "content": content,
        "name": content.get("device_name", devaddr)  # 可选
    }
    try:
        resp = requests.post(url, headers=headers, json=payload)
        if resp.status_code == 201:
            print(f"✅ 创建设备 {devaddr} 成功")
            return resp.json()
        else:
            print(f"❌ 创建设备 {devaddr} 失败: {resp.status_code} {resp.text}")
            return None
    except Exception as e:
        print(f"❌ 创建设备 {devaddr} 异常: {e}")
        return None

def update_device(device_id, content, token):
    """更新设备 content"""
    url = f"{BASE_URL}/classes/Device/{device_id}"
    headers = {"sessiontoken": token, "Content-Type": "application/json"}
    payload = {"content": content}
    try:
        resp = requests.put(url, headers=headers, json=payload)
        if resp.status_code == 200:
            print(f"✅ 更新设备 {device_id} 成功")
            return True
        else:
            print(f"❌ 更新设备 {device_id} 失败: {resp.status_code} {resp.text}")
            return False
    except Exception as e:
        print(f"❌ 更新设备 {device_id} 异常: {e}")
        return False

def transform_content(total1_content):
    """将总测1的 content 转换为总测2的版本"""
    # 深拷贝
    new_content = json.loads(json.dumps(total1_content))

    # 修改设备地址和名称
    old_addr = new_content.get("device_address", "")
    if old_addr.startswith("总测1_"):
        new_addr = old_addr.replace("总测1_", "总测2_")
        new_content["device_address"] = new_addr
    old_name = new_content.get("device_name", "")
    if "总测1" in old_name:
        new_name = old_name.replace("总测1", "总测2")
        new_content["device_name"] = new_name
    new_content["station_name"] = "总测2"

    # 修改每个步骤中的机械臂地址 (1551 -> 1651)
    for step in new_content.get("test_steps", []):
        comm = step.get("communication")
        if not isinstance(comm, dict):
            continue
        # 仅当协议为 Modbus-TCP 且存在 address 字段时进行替换
        if comm.get("protocol") == "Modbus-TCP" and "address" in comm:
            addr = comm["address"]
            # 地址可能是字符串 "1551" 或数字 1551
            if isinstance(addr, str) and addr == "1551":
                comm["address"] = "1651"
            elif isinstance(addr, int) and addr == 1551:
                comm["address"] = 1651

    return new_content

def main():
    token = login_and_get_token()
    if not token:
        sys.exit(1)

    # 1. 查询所有总测1的测试项设备
    where_total1 = {
        "product": {"__type": "Pointer", "className": "Product", "objectId": PRODUCT_TEST_ITEM},
        "devaddr": {"$regex": "^总测1_"}   # 以“总测1_”开头
    }
    total1_devices = query_devices(where_total1, token)
    print(f"找到 {len(total1_devices)} 个总测1的测试项设备")

    # 2. 查询所有总测2的测试项设备（用于判断是否已存在）
    where_total2 = {
        "product": {"__type": "Pointer", "className": "Product", "objectId": PRODUCT_TEST_ITEM},
        "devaddr": {"$regex": "^总测2_"}
    }
    total2_devices = query_devices(where_total2, token)
    existing_addrs = {dev["devaddr"] for dev in total2_devices if "devaddr" in dev}
    print(f"已存在 {len(existing_addrs)} 个总测2的设备")

    # 3. 逐个处理
    created = 0
    updated = 0
    for dev in total1_devices:
        devaddr = dev.get("devaddr")
        if not devaddr:
            continue
        new_addr = devaddr.replace("总测1_", "总测2_")
        content = dev.get("content", {})
        if not content:
            print(f"⚠️ 设备 {devaddr} content 为空，跳过")
            continue

        new_content = transform_content(content)

        if new_addr in existing_addrs:
            # 更新
            target_dev = next(d for d in total2_devices if d["devaddr"] == new_addr)
            if update_device(target_dev["objectId"], new_content, token):
                updated += 1
        else:
            # 创建
            if create_device(new_addr, new_content, token):
                created += 1

    print(f"\n处理完成：创建 {created} 个，更新 {updated} 个")

if __name__ == "__main__":
    main()