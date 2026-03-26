#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import requests
import json
import sys

BASE_URL = "http://127.0.0.1/iotapi"
LOGIN_URL = f"{BASE_URL}/login"
USERNAME = "dgiot_dev"
PASSWORD = "dgiot_dev"

STATION_INSTRUCTIONS = {
    1500: {
        "51": {
            "name": "总测1指令码",
            "meanings": {
                1: "水平", 2: "右滚90", 3: "抬头90", 4: "上升H1-5", 5: "上升H6-9",
                6: "绕X轴", 7: "2°/s", 8: "抬头", 9: "低头", 10: "左滚", 11: "右滚",
                12: "左偏航", 13: "右偏航", 14: "折翼", 15: "噪音", 16: "转速"
            }
        }
    },
    1600: {
        "51": {
            "name": "总测2指令码",
            "meanings": {
                1: "水平", 2: "右滚90", 3: "抬头90", 4: "上升H1-5", 5: "上升H6-9",
                6: "绕X轴", 7: "2°/s", 8: "抬头", 9: "低头", 10: "左滚", 11: "右滚",
                12: "左偏航", 13: "右偏航", 14: "折翼", 15: "噪音", 16: "转速"
            }
        }
    }
}

def login_and_get_token():
    print("正在登录...")
    headers = {"Content-Type": "text/plain"}
    payload = json.dumps({"username": USERNAME, "password": PASSWORD})
    try:
        resp = requests.post(LOGIN_URL, headers=headers, data=payload)
        if resp.status_code != 200:
            print(f"登录失败，HTTP {resp.status_code}: {resp.text}")
            sys.exit(1)
        data = resp.json()
        token = data.get("sessionToken") or data.get("access_token")
        if not token:
            print("登录返回数据中未找到 token")
            sys.exit(1)
        print(f"登录成功，token: {token}")
        return token
    except Exception as e:
        print(f"登录请求异常: {e}")
        sys.exit(1)

def clean_content(content):
    if "registers" in content and isinstance(content["registers"], list):
        for reg in content["registers"]:
            if "value_range" in reg and isinstance(reg["value_range"], list):
                reg["value_range"] = " / ".join(str(v) for v in reg["value_range"])
    return content

def main():
    token = login_and_get_token()
    for devaddr in ["D1500", "D1600"]:
        # 获取设备
        resp = requests.get(f"{BASE_URL}/classes/Device?where={json.dumps({'devaddr': devaddr})}",
                            headers={"sessiontoken": token})
        data = resp.json()
        if not data.get("results"):
            print(f"未找到 {devaddr}")
            continue
        dev = data["results"][0]
        station_id = int(devaddr.replace("D", ""))
        current_content = dev.get("content", {})
        if station_id in STATION_INSTRUCTIONS:
            if "instructions" in current_content:
                current_content["instructions"].update(STATION_INSTRUCTIONS[station_id])
            else:
                current_content["instructions"] = STATION_INSTRUCTIONS[station_id]
        if "baseAddress" not in current_content:
            current_content["baseAddress"] = station_id
        current_content = clean_content(current_content)
        url = f"{BASE_URL}/classes/Device/{dev['objectId']}"
        payload = {"content": current_content}
        resp = requests.put(url, headers={"sessiontoken": token, "Content-Type": "application/json"}, json=payload)
        if resp.status_code == 200:
            print(f"✅ {devaddr} 更新成功")
        else:
            print(f"❌ {devaddr} 失败: {resp.status_code} {resp.text}")

if __name__ == "__main__":
    main()