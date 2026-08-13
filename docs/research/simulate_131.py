#!/usr/bin/env python3
"""
131 IO Server 全链路模拟
基于: IO ServerOnLine 2047文件分析 + dgiot_tdengine源码 + DLAS本体
"""
import json, time, random, os

# ── 真实 IO Server 拓扑 ──
SITE = "oil_field_01"
GATEWAY = "gw_131"
IO_IP = "192.168.10.131"

# ── 进程 (from psNTService.csv) ──
PROCESSES = {
    "RTDBServer64":  {"ver":"6.0.1.9",  "hb":60, "role":"实时库"},
    "IOFileServer":    {"ver":"6.0.0.1",  "hb":60, "role":"文件服务(:7001)"},
    "IOMonitor":       {"ver":"6.0.0.1",  "hb":60, "role":"IO采集(300ms/批)"},
    "CalcEngine":      {"ver":"6.0.0.1",  "hb":60, "role":"计算引擎"},
    "CalcFileServer":  {"ver":"6.0.0.1",  "hb":60, "role":"计算文件服务"},
    "SyncTaskManager": {"ver":"0.0.0.1",  "hb":30, "role":"同步平台"},
}

# ── 设备定义 (from Device.ini, 12类保护继电器) ──
RELAY_FORMULAS = {
    "I": "Y * 170 / 8192",   # 电流 A
    "U": "Y * 170 / 8192",   # 电压 V
    "P": "Y * 170 * 8.5 * 1.732 / 8192",  # 功率 W
}

# ── 油井设备 (from Event.txt + runBack1.zio) ──
WELLS = [
    {"id":"rtu_001","devaddr":"DEV-001","name":"K1_51","ip":"192.168.10.131:502","protocol":"Modbus TCP",
     "points":[
        {"id":"oil_pressure","addr":40300,"fmt":"float32_AB","unit":"MPa","lo":0,"hi":10,"alarm_hi":3.0},
        {"id":"casing_pressure","addr":40302,"fmt":"float32_AB","unit":"MPa","lo":0,"hi":10},
        {"id":"temperature","addr":40304,"fmt":"float32_AB","unit":"C","lo":-20,"hi":85,"alarm_hi":75},
        {"id":"flow_rate","addr":40306,"fmt":"float32_AB","unit":"m3/h","lo":0,"hi":100},
        {"id":"pump_status","addr":40308,"fmt":"uint16","unit":"","lo":0,"hi":1},
    ]},
    {"id":"rtu_002","devaddr":"02110150041","name":"M5","ip":"192.168.10.131:502","protocol":"Modbus TCP",
     "points":[
        {"id":"max_load","addr":40300,"fmt":"float32_AB","unit":"kN","lo":0,"hi":100,"alarm_hi":80},
        {"id":"min_load","addr":40302,"fmt":"float32_AB","unit":"kN"},
        {"id":"up_current","addr":40304,"fmt":"float32_AB","unit":"A","alarm_hi":20},
        {"id":"down_current","addr":40306,"fmt":"float32_AB","unit":"A"},
    ]},
]

# ── 数据导出 (from Data Servers) ──
DATA_EXPORTS = {
    "Oracle": "192.168.10.129:1521/orcl (INDUSTRYPROD, 966 wells)",
    "RTDB": "192.168.10.141:8889 (GENERIC_VENDOR实时库)",
}

# ── 协议驱动 (from IO Servers) ──
PROTOCOLS = {
    "IM_A11_RTU": "A11采油厂RTU采集(功图模块)",
    "OPC_FC_Client": "OPC DA Client(KEPware.KEPServerEx.V4)",
    "FORCE_HLS_SIM": "仿真模拟器(测试用, 18 pumps)",
    "Standard_Umodbus": "Modbus RTU/TCP(空/未启用)",
}

def simulate_modbus_read(point):
    """模拟Modbus RTU读取"""
    lo = point.get("lo", 0)
    hi = point.get("hi", 100)
    base = (hi - lo) / 2 + lo  # 正常范围中点
    noise = random.gauss(0, (hi - lo) * 0.05)  # 5%噪声
    return round(base + noise, 2)

def simulate_alarm_scenario(point, value):
    """模拟告警场景: 20%概率触发"""
    alarm_hi = point.get("alarm_hi")
    if alarm_hi and random.random() < 0.15:
        return round(alarm_hi + random.uniform(1, 15), 1), "ALARM"
    return value, None

def evaluate_rules(point, value, rules):
    """模拟规则评估"""
    for rule in rules:
        if rule["when"]["property"] == point["id"]:
            if rule["when"]["op"] == ">" and value > rule["when"]["value"]:
                return rule["then"]
    return None

def format_mqtt_topic(device_id, point_id):
    """构建DLAS MQTT topic"""
    return f"dgiot/{SITE}/{GATEWAY}/{device_id}/{point_id}/data"

def format_tdengine_insert(device, point, value, quality=192):
    """模拟TDengine写入格式"""
    return (f"INSERT INTO _{device['id']}_{point['id']} "
            f"USING _{device['id'][:8]} "
            f"TAGS('{device['devaddr']}') "
            f"VALUES (NOW, {value}, {quality})")

# ── 主模拟 ──
def main():
    print("=" * 70)
    print("  131 IO Server 全链路模拟")
    print(f"  Site: {SITE}  Gateway: {GATEWAY}  IP: {IO_IP}")
    print("=" * 70)
    print()

    # Layer 0: Process health
    print("── Layer 0: Process Health ──")
    for name, proc in PROCESSES.items():
        hb = "OK" if random.random() > 0.05 else "MISSED"
        print(f"  {name:20s} v{proc['ver']:10s} hb={proc['hb']}s  {proc['role']:30s} [{hb}]")
    print(f"  Total: {len(PROCESSES)} daemons, all running")
    print()

    # Layer 1: Protocol scan
    print("── Layer 1: Protocol Scan ──")
    for proto, desc in PROTOCOLS.items():
        status = "active" if proto != "Standard_Umodbus" else "inactive"
        print(f"  {proto:25s} {desc:50s} [{status}]")
    print()

    # Layer 2: Modbus scan (simulated)
    print("── Layer 2: Modbus Scan (simulated) ──")
    for device in WELLS:
        print(f"  Device: {device['id']} ({device['name']}) @ {device['ip']}")
        print(f"    devaddr: {device['devaddr']}")
        for point in device["points"]:
            value = simulate_modbus_read(point)
            val_str = f"{value}"
            print(f"    ├── {point['id']:20s} addr={point['addr']:5d} {point['fmt']:12s} = {val_str:>8} {point['unit']}")
        print(f"    Total: {len(device['points'])} points")
    print()

    # Layer 3: DLAS MQTT Pub (simulated)
    print("── Layer 3: DLAS MQTT Publish (simulated) ──")
    mqtt_msgs = []
    for device in WELLS:
        for point in device["points"]:
            value = simulate_modbus_read(point)
            alarm_value, alarm_flag = simulate_alarm_scenario(point, value)
            topic = format_mqtt_topic(device["id"], point["id"])
            q = 0 if alarm_flag else 192
            final_val = alarm_value if alarm_flag else value
            msg = {"ts":int(time.time()*1000),"v":final_val,"q":q}
            mqtt_msgs.append((topic, msg, alarm_flag))
            flag = " [ALARM!]" if alarm_flag else ""
            print(f"  {topic:60s} -> v={final_val:6.2f}{point['unit']:4s} q={q}{flag}")
    print(f"  Total: {len(mqtt_msgs)} MQTT messages")
    print()

    # Layer 4: Shadow Evaluate (simulated)
    print("── Layer 4: Shadow gen_statem Evaluate (simulated) ──")
    RULES = [
        {"id":"R_HIGH_TEMP","when":{"property":"temperature","op":">","value":75},
         "then":{"state":"alarm","severity":"L1","action":"notify"}},
        {"id":"R_HIGH_PRESS","when":{"property":"oil_pressure","op":">","value":3.0},
         "then":{"state":"alarm","severity":"L2","action":"shutdown"}},
    ]
    for device in WELLS:
        state = "online"
        err_count = 0
        for point in device["points"]:
            value = simulate_modbus_read(point)
            alarm_val, _ = simulate_alarm_scenario(point, value)
            final_val = alarm_val if random.random() < 0.15 else value
            result = evaluate_rules(point, final_val, RULES)
            if result:
                state = "alarm"
                err_count += 1
                print(f"  [{device['id']}] {point['id']}={final_val} -> RULE: {result['state']} "
                      f"severity={result['severity']} action={result['action']}")
        if err_count == 0:
            print(f"  [{device['id']}] All points normal -> state={state}")
    print()

    # Layer 5: Storage (simulated)
    print("── Layer 5: Storage (simulated) ──")
    print(f"  Parse/PG:        objectId=rtu_001, status=online, last_update={time.strftime('%H:%M:%S')}")
    for device in WELLS[:1]:  # first device only for brevity
        for point in device["points"][:2]:  # first 2 points
            td_sql = format_tdengine_insert(device, point, simulate_modbus_read(point))
            print(f"  TDengine:        {td_sql}")
    print(f"  Data Exports:     Oracle {DATA_EXPORTS['Oracle'][:40]}")
    print(f"                   RTDB {DATA_EXPORTS['RTDB'][:30]}")
    print()

    # Summary
    print("=" * 70)
    print(f"  Simulation Complete")
    print(f"  Devices: {len(WELLS)}  Points: {sum(len(d['points']) for d in WELLS)}")
    print(f"  MQTT messages: {len(mqtt_msgs)}")
    alarms = [m for (_, _, f) in mqtt_msgs if f]
    print(f"  Alarms triggered: {len(alarms)}")
    print(f"  Pipeline: Modbus(:502) -> MQTT(:1883) -> Shadow -> Parse + TDengine")
    print(f"  DTU GPRS: 16 protocols available (from IO Server analysis)")
    print(f"  OPC DA:    KEPware.KEPServerEx.V4 (DCOM :135)")
    print("=" * 70)

if __name__ == "__main__":
    main()
