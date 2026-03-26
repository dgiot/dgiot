#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import requests
import json
import sys

BASE_URL = "http://127.0.0.1/iotapi"
TOKEN = "r:0a6a9d566ce3d970e80dd891c879da0f"  # 使用刚才获取的 token
PRODUCT_TEST_ITEM = "343cf21f82"
JSON_FILE = "test_items_full_updated.json"  # 确保使用正确的 JSON 文件

HEADERS = {"sessiontoken": TOKEN, "Content-Type": "application/json"}

# 失败的设备地址列表（根据输出整理）
FAILED_DEVADDRS = [
    "磁航向_磁航向校准",
    "总测1_弹翼开关与引信通信调试",
    "总测1_空速调试",
    "总测1_铁电故障调试",
    "总测1_引信24V供电调试",
    "拷机1_数据链检查",
    "总测1_原点装订功能调试",
    "总测1_航线装订功能",
    "总测1_加速度校准",
    "总测1_气压高度检查",
    "总测1_姿态测试",
    "总测1_左前翼校准",
    "总测1_空速标定",
    "总测1_舵面极性调试",
    "总测1_动力测试",
    "总测1_左垂尾校准",
    "总测1_右前翼校准",
    "总测1_右垂尾校准",
    "桁架_电子变倍功能调试",
    "拷机2_数据链检查",
    "桁架_扫描与刹车测试",
]

def get_devices():
    url = f"{BASE_URL}/classes/Device"
    params = {"where": json.dumps({"product": PRODUCT_TEST_ITEM}), "limit": 100}
    resp = requests.get(url, headers=HEADERS, params=params)
    if resp.status_code != 200:
        print(f"查询设备失败: {resp.status_code}")
        return []
    data = resp.json()
    return data.get("results", [])

def update_device(dev, steps):
    url = f"{BASE_URL}/classes/Device/{dev['objectId']}"
    current_content = dev.get("content", {})
    current_content["steps"] = steps
    payload = {"content": current_content}
    try:
        resp = requests.put(url, headers=HEADERS, json=payload)
        if resp.status_code == 200:
            print(f"✅ 成功: {dev['devaddr']}")
            return True
        else:
            print(f"❌ 失败: {dev['devaddr']} 状态码 {resp.status_code}")
            print(f"   响应: {resp.text}")
            return False
    except Exception as e:
        print(f"❌ 异常: {dev['devaddr']} - {e}")
        return False

def main():
    # 加载 JSON 步骤数据
    with open(JSON_FILE, 'r', encoding='utf-8') as f:
        items = json.load(f)
    item_map = {item["device_address"]: item for item in items if "device_address" in item}

    devices = get_devices()
    dev_map = {d["devaddr"]: d for d in devices if d.get("devaddr")}

    success = 0
    for addr in FAILED_DEVADDRS:
        if addr not in dev_map:
            print(f"设备 {addr} 未找到，跳过")
            continue
        if addr not in item_map:
            print(f"JSON 中无 {addr} 的数据，跳过")
            continue
        dev = dev_map[addr]
        steps = item_map[addr].get("test_steps", [])
        if update_device(dev, steps):
            success += 1

    print(f"重试完成，成功 {success}/{len(FAILED_DEVADDRS)}")

if __name__ == "__main__":
    main()