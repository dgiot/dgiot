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

def login():
    resp = requests.post(LOGIN_URL,
                         headers={"Content-Type": "text/plain"},
                         data=json.dumps({"username": USERNAME, "password": PASSWORD}))
    if resp.status_code != 200:
        print("登录失败", resp.text)
        sys.exit(1)
    data = resp.json()
    token = data.get("sessionToken") or data.get("access_token")
    return token

def clean_content(content):
    if "registers" in content and isinstance(content["registers"], list):
        for reg in content["registers"]:
            if "value_range" in reg and isinstance(reg["value_range"], list):
                reg["value_range"] = " / ".join(str(v) for v in reg["value_range"])
    return content

def update_station(device, token):
    devaddr = device["devaddr"]
    try:
        station_id = int(devaddr.replace("D", ""))
    except:
        return
    if station_id not in [1500, 1600]:
        return

    content = device.get("content", {})
    if "instructions" in content:
        content["instructions"].update(STATION_INSTRUCTIONS[station_id])
    else:
        content["instructions"] = STATION_INSTRUCTIONS[station_id]
    content = clean_content(content)

    url = f"{BASE_URL}/classes/Device/{device['objectId']}"
    headers = {"sessiontoken": token, "Content-Type": "application/json"}
    payload = {"content": content}
    print(f"正在更新 {devaddr}...")
    resp = requests.put(url, headers=headers, json=payload)
    if resp.status_code == 200:
        print(f"✅ {devaddr} 更新成功")
    else:
        print(f"❌ {devaddr} 更新失败，状态码 {resp.status_code}")
        print("响应体:", resp.text)

def main():
    token = login()
    # 查询工位设备
    where = json.dumps({
        "product": {"__type": "Pointer", "className": "Product", "objectId": "2de1b3e1b8"},
        "devaddr": {"$in": ["D1500", "D1600"]}
    })
    resp = requests.get(f"{BASE_URL}/classes/Device", headers={"sessiontoken": token}, params={"where": where})
    devices = resp.json().get("results", [])
    for dev in devices:
        update_station(dev, token)

if __name__ == "__main__":
    main()