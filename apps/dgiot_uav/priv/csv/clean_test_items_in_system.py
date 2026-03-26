#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import requests
import json
import sys

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

def query_all_test_devices(token):
    """查询所有测试项设备"""
    where = {
        "product": {"__type": "Pointer", "className": "Product", "objectId": PRODUCT_TEST_ITEM}
    }
    url = f"{BASE_URL}/classes/Device"
    headers = {"sessiontoken": token}
    params = {"where": json.dumps(where), "limit": 200}
    try:
        resp = requests.get(url, headers=headers, params=params)
        if resp.status_code != 200:
            print(f"查询设备失败: {resp.status_code} {resp.text}")
            return []
        data = resp.json()
        return data.get("results", [])
    except Exception as e:
        print(f"查询设备异常: {e}")
        return []

def update_device(device_id, content, token):
    """更新设备 content"""
    url = f"{BASE_URL}/classes/Device/{device_id}"
    headers = {"sessiontoken": token, "Content-Type": "application/json"}
    payload = {"content": content}
    try:
        resp = requests.put(url, headers=headers, json=payload)
        if resp.status_code == 200:
            return True
        else:
            print(f"更新设备 {device_id} 失败: {resp.status_code} {resp.text}")
            return False
    except Exception as e:
        print(f"更新设备 {device_id} 异常: {e}")
        return False

def clean_step(step):
    """清理单个步骤，返回清理后的步骤字典"""
    cleaned = {}

    # 保留必要字段
    if "step_number" in step:
        cleaned["step_number"] = step["step_number"]
    if "action_type" in step:
        cleaned["action_type"] = step["action_type"]
    if "description" in step:
        cleaned["description"] = step["description"]
    if "target" in step:
        cleaned["target"] = step["target"]

    # 处理 send
    if "send" in step:
        send_val = step["send"]
        if isinstance(send_val, dict) and "content" in send_val:
            cleaned["send"] = send_val["content"]
        elif isinstance(send_val, str):
            cleaned["send"] = send_val
        else:
            cleaned["send"] = ""   # 空字符串占位
    else:
        # 如果动作类型是 send 或 request_response，可能需要一个空 send
        if step.get("action_type") in ["send", "request_response"]:
            cleaned["send"] = ""

    # 保留 judge（如果有）
    if "judge" in step:
        cleaned["judge"] = step["judge"]

    # 其他字段（receive, wait, notes, communication 等）全部丢弃
    return cleaned

def clean_content(content):
    """清理设备 content 中的步骤数组"""
    # 找出 steps 所在的字段名（可能是 steps 或 test_steps）
    steps_key = None
    if "steps" in content:
        steps_key = "steps"
    elif "test_steps" in content:
        steps_key = "test_steps"
    else:
        # 没有步骤数组，直接返回原内容
        return content

    # 清理每个步骤
    old_steps = content[steps_key]
    new_steps = [clean_step(step) for step in old_steps]

    # 替换步骤数组，保留其他字段
    content[steps_key] = new_steps
    return content

def main():
    token = login_and_get_token()
    if not token:
        sys.exit(1)

    devices = query_all_test_devices(token)
    print(f"找到 {len(devices)} 个测试项设备")

    success = 0
    for dev in devices:
        devaddr = dev.get("devaddr", "未知")
        object_id = dev["objectId"]
        content = dev.get("content", {})
        if not isinstance(content, dict):
            print(f"⚠️ 设备 {devaddr} content 不是字典，跳过")
            continue

        # 深拷贝，避免修改原数据影响后续判断
        new_content = json.loads(json.dumps(content))
        new_content = clean_content(new_content)

        # 更新设备
        if update_device(object_id, new_content, token):
            print(f"✅ 设备 {devaddr} 更新成功")
            success += 1
        else:
            print(f"❌ 设备 {devaddr} 更新失败")

    print(f"\n清理完成，成功更新 {success}/{len(devices)} 个设备")

if __name__ == "__main__":
    main()#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import requests
import json
import sys
import os
from datetime import datetime

# ========== 配置 ==========
BASE_URL = "http://127.0.0.1/iotapi"
LOGIN_URL = f"{BASE_URL}/login"
USERNAME = "dgiot_dev"
PASSWORD = "dgiot_dev"
PRODUCT_TEST_ITEM = "343cf21f82"   # 测试项产品 ID
BACKUP_DIR = "backups"             # 备份文件存放目录

# ========== 清理模式 ==========
# 保守模式：只删除冗余字段，保留 send 对象原样
# 激进模式：将 send 对象简化为 content 字符串（可能丢失信息）
CLEAN_MODE = "conservative"   # 可选 "conservative" 或 "aggressive"

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

def query_all_test_devices(token):
    """查询所有测试项设备"""
    where = {
        "product": {"__type": "Pointer", "className": "Product", "objectId": PRODUCT_TEST_ITEM}
    }
    url = f"{BASE_URL}/classes/Device"
    headers = {"sessiontoken": token}
    params = {"where": json.dumps(where), "limit": 200}
    try:
        resp = requests.get(url, headers=headers, params=params)
        if resp.status_code != 200:
            print(f"查询设备失败: {resp.status_code} {resp.text}")
            return []
        data = resp.json()
        return data.get("results", [])
    except Exception as e:
        print(f"查询设备异常: {e}")
        return []

def backup_devices(devices):
    """将设备数据备份到本地文件"""
    if not os.path.exists(BACKUP_DIR):
        os.makedirs(BACKUP_DIR)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"test_items_backup_{timestamp}.json"
    filepath = os.path.join(BACKUP_DIR, filename)
    try:
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(devices, f, ensure_ascii=False, indent=2)
        print(f"✅ 备份完成，保存至 {filepath}")
        return filepath
    except Exception as e:
        print(f"❌ 备份失败: {e}")
        return None

def update_device(device_id, content, token):
    """更新设备 content"""
    url = f"{BASE_URL}/classes/Device/{device_id}"
    headers = {"sessiontoken": token, "Content-Type": "application/json"}
    payload = {"content": content}
    try:
        resp = requests.put(url, headers=headers, json=payload)
        if resp.status_code == 200:
            return True
        else:
            print(f"更新设备 {device_id} 失败: {resp.status_code} {resp.text}")
            return False
    except Exception as e:
        print(f"更新设备 {device_id} 异常: {e}")
        return False

def clean_step(step, mode):
    """清理单个步骤，返回清理后的步骤字典"""
    cleaned = {}

    # 保留必要字段
    if "step_number" in step:
        cleaned["step_number"] = step["step_number"]
    if "action_type" in step:
        cleaned["action_type"] = step["action_type"]
    if "description" in step:
        cleaned["description"] = step["description"]
    if "target" in step:
        cleaned["target"] = step["target"]

    # 处理 send（根据模式）
    if "send" in step:
        if mode == "aggressive":
            # 激进模式：只取 content 字符串
            send_val = step["send"]
            if isinstance(send_val, dict) and "content" in send_val:
                cleaned["send"] = send_val["content"]
            elif isinstance(send_val, str):
                cleaned["send"] = send_val
            else:
                cleaned["send"] = ""
        else:
            # 保守模式：保留 send 原样
            cleaned["send"] = step["send"]

    # 保留 judge（如果有）
    if "judge" in step:
        cleaned["judge"] = step["judge"]

    # 冗余字段不复制：receive, wait, notes, communication 等
    return cleaned

def clean_content(content, mode):
    """清理设备 content 中的步骤数组"""
    # 找出 steps 所在的字段名（可能是 steps 或 test_steps）
    steps_key = None
    if "steps" in content:
        steps_key = "steps"
    elif "test_steps" in content:
        steps_key = "test_steps"
    else:
        return content

    old_steps = content[steps_key]
    new_steps = [clean_step(step, mode) for step in old_steps]
    content[steps_key] = new_steps
    return content

def main():
    token = login_and_get_token()
    if not token:
        sys.exit(1)

    print(f"清理模式：{CLEAN_MODE}")

    devices = query_all_test_devices(token)
    print(f"找到 {len(devices)} 个测试项设备")

    if not devices:
        print("没有设备需要处理，退出。")
        return

    # 备份
    backup_file = backup_devices(devices)
    if not backup_file:
        print("备份失败，终止操作以保护数据。")
        return

    # 更新
    success = 0
    for dev in devices:
        devaddr = dev.get("devaddr", "未知")
        object_id = dev["objectId"]
        content = dev.get("content", {})
        if not isinstance(content, dict):
            print(f"⚠️ 设备 {devaddr} content 不是字典，跳过")
            continue

        new_content = json.loads(json.dumps(content))  # 深拷贝
        new_content = clean_content(new_content, CLEAN_MODE)

        if update_device(object_id, new_content, token):
            print(f"✅ 设备 {devaddr} 更新成功")
            success += 1
        else:
            print(f"❌ 设备 {devaddr} 更新失败")

    print(f"\n清理完成，成功更新 {success}/{len(devices)} 个设备")
    print(f"备份文件位于: {backup_file}")

if __name__ == "__main__":
    main()