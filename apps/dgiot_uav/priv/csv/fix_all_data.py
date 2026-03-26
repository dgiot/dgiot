#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import requests
import json
import sys
import time

# ========== 配置 ==========
BASE_URL = "http://127.0.0.1/iotapi"
LOGIN_URL = f"{BASE_URL}/login"
USERNAME = "dgiot_dev"
PASSWORD = "dgiot_dev"

# 产品 ID
PRODUCT_STATION = "2de1b3e1b8"      # 超近距无人机工位
PRODUCT_UAV = "6235befb62"          # 超近距无人机
PRODUCT_FIXTURE = "bd49cc8272"      # 超近距无人机治具
PRODUCT_TEST_ITEM = "343cf21f82"    # 超近距无人机测试项

# 工位指令映射（与之前一致）
STATION_INSTRUCTIONS = {
    1100: {  # 桁行架工位
        "51": {
            "name": "桁行架指令码",
            "meanings": {
                1: "取料",
                2: "右上30",
                3: "右下30",
                4: "左上30",
                5: "左下30",
                6: "水平",
                7: "完成"
            }
        }
    },
    1200: {  # 拷机1工位
        "51": {
            "name": "拷机1指令码",
            "meanings": {1: "下料"}
        }
    },
    1300: {  # 拷机2工位
        "51": {
            "name": "拷机2指令码",
            "meanings": {1: "下料"}
        }
    },
    1500: {  # 总测1工位
        "51": {
            "name": "总测1指令码",
            "meanings": {
                1: "水平",
                2: "右滚90",
                3: "抬头90",
                4: "上升H1-5",
                5: "上升H6-9",
                6: "绕X轴",
                7: "2°/s",
                8: "抬头",
                9: "低头",
                10: "左滚",
                11: "右滚",
                12: "左偏航",
                13: "右偏航",
                14: "折翼",
                15: "噪音",
                16: "转速"
            }
        }
    },
    1600: {  # 总测2工位（与总测1相同）
        "51": {
            "name": "总测2指令码",
            "meanings": {
                1: "水平",
                2: "右滚90",
                3: "抬头90",
                4: "上升H1-5",
                5: "上升H6-9",
                6: "绕X轴",
                7: "2°/s",
                8: "抬头",
                9: "低头",
                10: "左滚",
                11: "右滚",
                12: "左偏航",
                13: "右偏航",
                14: "折翼",
                15: "噪音",
                16: "转速"
            }
        }
    },
    1700: {  # 磁航向工位
        "51": {
            "name": "磁航向指令码",
            "meanings": {
                1: "顺360",
                2: "复位",
                3: "机翼翻转",
                4: "反向翻转",
                5: "下料"
            }
        }
    }
}

# 遥控指令集（无人机）
UAV_REMOTE_COMMANDS = {
    "remote_commands": {
        "flight_control": [
            {"code": 0xF0, "name": "开关命令", "description": "通用开关命令，子命令需额外指定", "params": ["sub_command"]},
            {"code": 0x07, "name": "航点切换", "description": "切换到指定航点", "params": ["waypoint_index"]},
            {"code": 0xFD, "name": "载荷控制", "description": "载荷基本控制（归中、跟踪等）", "params": ["sub_command"]},
            {"code": 0xFE, "name": "载荷连续控制", "description": "连续控制载荷角度", "params": ["sub_command", "elevation", "azimuth"]},
            {"code": 0xC3, "name": "航线上传", "description": "上传单个航点", "params": ["latitude", "longitude", "altitude", "total_waypoints", "waypoint_seq"]},
            {"code": 0xD4, "name": "飞行时间架次", "description": "设置/查询飞行时间和架次", "params": ["total_time", "sortie"]},
            {"code": 0xDC, "name": "舵机校准", "description": "校准舵机参数", "params": ["channel", "pwm_center", "up_ratio", "down_ratio"]},
            {"code": 0xD3, "name": "空速校准", "description": "设置空速校准系数", "params": ["scale", "offset"]},
            {"code": 0xD7, "name": "ET航线上传", "description": "另一种航线上传命令", "params": ["latitude", "longitude", "altitude", "total_waypoints", "waypoint_seq"]},
            {"code": 0xD8, "name": "ID设置", "description": "设置飞机类型和ID", "params": ["new_plane_type", "new_plane_id"]}
        ],
        "payload_control": [
            {"code": 0x01, "name": "载荷工作", "description": "启动载荷"},
            {"code": 0x02, "name": "载荷休眠", "description": "休眠载荷"},
            {"code": 0x03, "name": "载荷保护", "description": "进入保护模式"},
            {"code": 0x04, "name": "可见光模式", "description": "切换到可见光"},
            {"code": 0x05, "name": "红外模式", "description": "切换到红外"},
            {"code": 0x06, "name": "变倍放大", "description": "电子变倍放大"},
            {"code": 0x07, "name": "变倍缩小", "description": "电子变倍缩小"},
            {"code": 0x08, "name": "黑热模式", "description": "红外黑热"},
            {"code": 0x09, "name": "白热模式", "description": "红外白热"},
            {"code": 0x0A, "name": "码率2M", "description": "设置图像码率2Mbps"},
            {"code": 0x0B, "name": "码率4M", "description": "设置图像码率4Mbps"}
        ]
    }
}

# 治具指令集（治具产品）
FIXTURE_COMMANDS = {
    "command_sets": {
        "modbus": [
            {"code": 1, "name": "控制大继电器上电", "description": "控制大继电器给无人机上电", "modbus_function": "05", "register_address": "0000", "data_value": "FF00"},
            {"code": 2, "name": "控制大继电器断电", "description": "控制大继电器断电", "modbus_function": "05", "register_address": "0000", "data_value": "0000"},
            {"code": 3, "name": "测试引信9,10点电阻", "description": "测试引信9和10点之间的电阻", "modbus_function": "03", "register_address": "0000", "data_value": "0002"},
            {"code": 4, "name": "测试引信7,8点电阻", "description": "测试引信7和8点之间的电阻", "modbus_function": "03", "register_address": "0002", "data_value": "0002"},
            {"code": 5, "name": "测试引信5点与地电压", "description": "测试引信5点与地之间的电压", "modbus_function": "03", "register_address": "000A", "data_value": "0001"},
            # 可根据需要继续添加更多
        ]
    }
}

# 无人机物模型（带group分组）
UAV_THING = {
    "properties": [
        # D1 属性
        {"identifier": "latitude", "name": "纬度", "dataType": {"type": "double", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "longitude", "name": "经度", "dataType": {"type": "double", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "heading", "name": "航向角", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "pitch", "name": "俯仰角", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "roll", "name": "横滚角", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "relative_altitude", "name": "相对高度", "dataType": {"type": "float", "specs": {"unit": "m"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "gps_altitude", "name": "卫导高度", "dataType": {"type": "float", "specs": {"unit": "m"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "baro_altitude", "name": "气压高度", "dataType": {"type": "float", "specs": {"unit": "m"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "airspeed", "name": "空速", "dataType": {"type": "float", "specs": {"unit": "m/s"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "east_velocity", "name": "东向速度", "dataType": {"type": "float", "specs": {"unit": "m/s"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "north_velocity", "name": "北向速度", "dataType": {"type": "float", "specs": {"unit": "m/s"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "elevator_angle", "name": "升降舵角", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "rudder_angle", "name": "方向舵角", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "aileron_angle", "name": "副翼舵角", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "throttle_angle", "name": "油门舵角", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "flight_time", "name": "飞行时间", "dataType": {"type": "int", "specs": {"unit": "s"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "battery_voltage", "name": "电池电压", "dataType": {"type": "float", "specs": {"unit": "V"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "climb_rate", "name": "爬升率", "dataType": {"type": "float", "specs": {"unit": "m/s"}}, "accessMode": "r", "group": "d1"},
        {"identifier": "flight_mode", "name": "飞行模式", "dataType": {"type": "int"}, "accessMode": "r", "group": "d1"},
        {"identifier": "gps_satellite_count", "name": "卫星数量", "dataType": {"type": "int"}, "accessMode": "r", "group": "d1"},
        {"identifier": "fault_status", "name": "故障状态", "dataType": {"type": "int"}, "accessMode": "r", "group": "d1"},
        {"identifier": "warning_flag", "name": "警告标识", "dataType": {"type": "int"}, "accessMode": "r", "group": "d1"},
        {"identifier": "ferroelectric_fault", "name": "铁电故障", "dataType": {"type": "int"}, "accessMode": "r", "group": "d1"},

        # D2 属性
        {"identifier": "warhead_frame_freq", "name": "弹头帧频率", "dataType": {"type": "int", "specs": {"unit": "Hz"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "remote_frame_freq", "name": "遥控帧频率", "dataType": {"type": "int", "specs": {"unit": "Hz"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "launch_tube_frame_freq", "name": "发射管帧频率", "dataType": {"type": "int", "specs": {"unit": "Hz"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "payload_frame_freq", "name": "载荷帧频率", "dataType": {"type": "int", "specs": {"unit": "Hz"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "magnetic_heading_frame_freq", "name": "磁航向帧频率", "dataType": {"type": "int", "specs": {"unit": "Hz"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "gps_frame_freq", "name": "GPS帧频率", "dataType": {"type": "int", "specs": {"unit": "Hz"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "beidou_frame_freq", "name": "北斗帧频率", "dataType": {"type": "int", "specs": {"unit": "Hz"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "roll_angle_given", "name": "横滚角指令", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "pitch_angle_given", "name": "俯仰角指令", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "airspeed_given", "name": "空速指令", "dataType": {"type": "int", "specs": {"unit": "0.1m/s"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "onboard_nav_status", "name": "机载导航状态", "dataType": {"type": "int"}, "accessMode": "r", "group": "d2"},
        {"identifier": "origin_distance", "name": "原点距离", "dataType": {"type": "int", "specs": {"unit": "m"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "origin_azimuth", "name": "原点方位角", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "distance_to_go", "name": "剩余距离", "dataType": {"type": "int", "specs": {"unit": "m"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "lateral_deviation", "name": "横向偏差", "dataType": {"type": "float", "specs": {"unit": "m"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "command_altitude", "name": "指令高度", "dataType": {"type": "float", "specs": {"unit": "m"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "rotation_speed", "name": "转速", "dataType": {"type": "int", "specs": {"unit": "rpm"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "total_flight_time", "name": "总飞行时间", "dataType": {"type": "int", "specs": {"unit": "min"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "flight_sortie", "name": "架次", "dataType": {"type": "int"}, "accessMode": "r", "group": "d2"},
        {"identifier": "gps_relative_altitude", "name": "GPS相对高度", "dataType": {"type": "float", "specs": {"unit": "m"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "baro_relative_altitude", "name": "气压相对高度", "dataType": {"type": "float", "specs": {"unit": "m"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "angular_rate_x", "name": "X轴角速率", "dataType": {"type": "float", "specs": {"unit": "°/s"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "angular_rate_y", "name": "Y轴角速率", "dataType": {"type": "float", "specs": {"unit": "°/s"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "angular_rate_z", "name": "Z轴角速率", "dataType": {"type": "float", "specs": {"unit": "°/s"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "acceleration_x", "name": "X轴加速度", "dataType": {"type": "float", "specs": {"unit": "m/s²"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "acceleration_y", "name": "Y轴加速度", "dataType": {"type": "float", "specs": {"unit": "m/s²"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "acceleration_z", "name": "Z轴加速度", "dataType": {"type": "float", "specs": {"unit": "m/s²"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "pitch_calibration", "name": "俯仰校准", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "roll_calibration", "name": "横滚校准", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "heading_calibration", "name": "航向校准", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "elevator_calibration", "name": "升降舵校准", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "aileron_calibration", "name": "副翼校准", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "rudder_calibration", "name": "方向舵校准", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "airspeed_calibration_coef", "name": "空速校准系数", "dataType": {"type": "float"}, "accessMode": "r", "group": "d2"},
        {"identifier": "airspeed_calibration_offset", "name": "空速校准偏移", "dataType": {"type": "float", "specs": {"unit": "0.1m/s"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "airspeed_zero_offset", "name": "空速零偏", "dataType": {"type": "float", "specs": {"unit": "0.1m/s"}}, "accessMode": "r", "group": "d2"},
        {"identifier": "payload_status0", "name": "载荷状态字0", "dataType": {"type": "int"}, "accessMode": "r", "group": "d2"},
        {"identifier": "payload_status1", "name": "载荷状态字1", "dataType": {"type": "int"}, "accessMode": "r", "group": "d2"},
        {"identifier": "control_surface_status", "name": "控制面状态", "dataType": {"type": "int"}, "accessMode": "r", "group": "d2"},

        # D3 属性
        {"identifier": "ground_speed_direction", "name": "地速方向", "dataType": {"type": "float", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "beidou_snr_gt46_count", "name": "信噪比>46卫星数", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "beidou_snr_gt44_count", "name": "信噪比>44卫星数", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "beidou_snr_gt42_count", "name": "信噪比>42卫星数", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "beidou_snr_gt40_count", "name": "信噪比>40卫星数", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "beidou_snr_gt38_count", "name": "信噪比>38卫星数", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "beidou_snr_gt35_count", "name": "信噪比>35卫星数", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "magnetic_x", "name": "X轴磁力计", "dataType": {"type": "float", "specs": {"unit": "0.1"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "magnetic_y", "name": "Y轴磁力计", "dataType": {"type": "float", "specs": {"unit": "0.1"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "magnetic_z", "name": "Z轴磁力计", "dataType": {"type": "float", "specs": {"unit": "0.1"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "beidou_self_destruct_status", "name": "北斗自毁状态", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "data_select_flag", "name": "数据选择标志", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "beidou_pdop", "name": "北斗PDOP", "dataType": {"type": "float", "specs": {"unit": "0.2"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "main_loop_time", "name": "主循环时间", "dataType": {"type": "float", "specs": {"unit": "0.1ms"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "gps_beidou_altitude", "name": "GPS/北斗高度", "dataType": {"type": "float", "specs": {"unit": "0.1m"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "gps_beidou_latitude", "name": "GPS/北斗纬度", "dataType": {"type": "float", "specs": {"unit": "10⁻⁷°"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "gps_beidou_longitude", "name": "GPS/北斗经度", "dataType": {"type": "float", "specs": {"unit": "10⁻⁷°"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "gps_satellite_count", "name": "GPS卫星数", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "beidou_satellite_count", "name": "北斗卫星数", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "payload_switch_command", "name": "载荷开关指令", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "warhead_switch_command", "name": "战斗部开关指令", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "launch_tube_command", "name": "发射筒指令", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "gps_pdop", "name": "GPS PDOP", "dataType": {"type": "float", "specs": {"unit": "0.2"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "magnetic_heading", "name": "磁航向", "dataType": {"type": "float", "specs": {"unit": "0.1°"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "magnetic_calibration_status", "name": "磁力计校准状态", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "seeker_elevation_angle", "name": "导引头俯仰角", "dataType": {"type": "float", "specs": {"unit": "0.1°"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "seeker_azimuth_angle", "name": "导引头方位角", "dataType": {"type": "float", "specs": {"unit": "0.1°"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "seeker_elevation_rate", "name": "导引头俯仰角速率", "dataType": {"type": "float", "specs": {"unit": "0.1°/s"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "seeker_azimuth_rate", "name": "导引头方位角速率", "dataType": {"type": "float", "specs": {"unit": "0.1°/s"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "line_of_sight_elevation", "name": "视线俯仰角", "dataType": {"type": "float", "specs": {"unit": "0.1°"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "line_of_sight_azimuth", "name": "视线方位角", "dataType": {"type": "float", "specs": {"unit": "0.1°"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "flight_control_temp1", "name": "飞控温度1", "dataType": {"type": "float", "specs": {"unit": "0.1°C"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "flight_control_temp2", "name": "飞控温度2", "dataType": {"type": "float", "specs": {"unit": "0.1°C"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "warhead_voltage", "name": "战斗部电压", "dataType": {"type": "float", "specs": {"unit": "0.1V"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "payload_voltage", "name": "载荷电压", "dataType": {"type": "float", "specs": {"unit": "0.1V"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "night_flight_voltage", "name": "夜航电压", "dataType": {"type": "float", "specs": {"unit": "0.1V"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "power_5v2", "name": "5.2V电源", "dataType": {"type": "float", "specs": {"unit": "0.1V"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "power_5v0", "name": "5.0V电源", "dataType": {"type": "float", "specs": {"unit": "0.1V"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "power_8v4_1", "name": "8.4V电源1", "dataType": {"type": "float", "specs": {"unit": "0.1V"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "power_8v4_2", "name": "8.4V电源2", "dataType": {"type": "float", "specs": {"unit": "0.1V"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "hard_switch_voltage", "name": "硬开关电压", "dataType": {"type": "float", "specs": {"unit": "0.1V"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "switch_status", "name": "开关状态", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "fuze_charging_voltage", "name": "引信充电电压", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "wind_speed1", "name": "风速1", "dataType": {"type": "float", "specs": {"unit": "0.2m/s"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "wind_direction1", "name": "风向1", "dataType": {"type": "float", "specs": {"unit": "0.1°"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "wind_speed2", "name": "风速2", "dataType": {"type": "float", "specs": {"unit": "0.2m/s"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "wind_direction2", "name": "风向2", "dataType": {"type": "float", "specs": {"unit": "0.1°"}}, "accessMode": "r", "group": "d3"},
        {"identifier": "payload_electronic_zoom", "name": "载荷电子变焦", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "payload_tracking_flag", "name": "载荷跟踪标志", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "drone_type", "name": "无人机类型", "dataType": {"type": "int"}, "accessMode": "r", "group": "d3"},
        {"identifier": "sight_azimuth_heading_deviation", "name": "视线方位航向偏差", "dataType": {"type": "int", "specs": {"unit": "°"}}, "accessMode": "r", "group": "d3"},
    ]
}

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

def get_product(product_id, token):
    url = f"{BASE_URL}/classes/Product/{product_id}"
    headers = {"sessiontoken": token}
    try:
        resp = requests.get(url, headers=headers)
        if resp.status_code == 200:
            return resp.json()
        else:
            print(f"获取产品 {product_id} 失败，HTTP {resp.status_code}: {resp.text}")
            return None
    except Exception as e:
        print(f"获取产品 {product_id} 异常: {e}")
        return None

def update_product(product_id, data, token):
    url = f"{BASE_URL}/classes/Product/{product_id}"
    headers = {"sessiontoken": token, "Content-Type": "application/json"}
    try:
        resp = requests.put(url, headers=headers, json=data)
        if resp.status_code == 200:
            print(f"✅ 产品 {product_id} 更新成功")
        else:
            print(f"❌ 产品 {product_id} 更新失败，状态码 {resp.status_code}")
            print(f"   响应体: {resp.text}")
    except Exception as e:
        print(f"❌ 产品 {product_id} 更新异常: {e}")

def query_devices(product_id, where_extra=None, token=None):
    """查询指定产品下的设备"""
    where = {"product": {"__type": "Pointer", "className": "Product", "objectId": product_id}}
    if where_extra:
        where.update(where_extra)
    params = {"where": json.dumps(where), "limit": 200}
    url = f"{BASE_URL}/classes/Device"
    headers = {"sessiontoken": token}
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

def update_device(device_id, content, token):
    url = f"{BASE_URL}/amis/Device/{device_id}"
    headers = {"sessiontoken": token, "Content-Type": "application/json"}
    payload = {"content": content}
    try:
        resp = requests.put(url, headers=headers, json=payload)
        if resp.status_code == 200:
            return True
        else:
            print(f"更新设备 {device_id} 失败: {resp.status_code}, {resp.text}")
            return False
    except Exception as e:
        print(f"更新设备 {device_id} 异常: {e}")
        return False

def update_station_devices(token):
    """更新所有工位设备的 instructions"""
    devices = query_devices(PRODUCT_STATION, where_extra={"devaddr": {"$regex": "^D"}}, token=token)
    print(f"找到 {len(devices)} 个工位设备")
    for dev in devices:
        devaddr = dev.get("devaddr")
        if not devaddr:
            continue
        try:
            station_id = int(devaddr.replace("D", ""))
        except:
            continue
        if station_id not in STATION_INSTRUCTIONS:
            print(f"跳过未知工位 {devaddr}")
            continue
        current_content = dev.get("content", {})
        # 合并 instructions
        if "instructions" in current_content:
            print(f"工位 {devaddr} 已有 instructions，合并")
            current_content["instructions"].update(STATION_INSTRUCTIONS[station_id])
        else:
            current_content["instructions"] = STATION_INSTRUCTIONS[station_id]
        # 确保 baseAddress
        if "baseAddress" not in current_content:
            current_content["baseAddress"] = station_id
        if update_device(dev["objectId"], current_content, token):
            print(f"✅ 工位 {devaddr} 更新成功")
        else:
            print(f"❌ 工位 {devaddr} 更新失败")

def update_uav_product(token):
    """更新无人机产品的物模型和遥控指令集"""
    product = get_product(PRODUCT_UAV, token)
    if not product:
        return
    # 更新 thing
    current_thing = product.get("thing", {})
    # 合并属性（保留原有，添加 group）
    if "properties" in current_thing:
        # 建立现有属性映射
        existing_props = {p["identifier"]: p for p in current_thing["properties"]}
        for new_prop in UAV_THING["properties"]:
            ident = new_prop["identifier"]
            if ident in existing_props:
                # 更新现有属性的 group
                existing_props[ident]["group"] = new_prop["group"]
            else:
                # 添加新属性
                current_thing["properties"].append(new_prop)
    else:
        current_thing["properties"] = UAV_THING["properties"]
    # 更新 content.remote_commands
    current_content = product.get("content", {})
    if "remote_commands" in current_content:
        print("无人机产品已有 remote_commands，合并")
        # 简单合并，不处理冲突
        current_content["remote_commands"].update(UAV_REMOTE_COMMANDS["remote_commands"])
    else:
        current_content["remote_commands"] = UAV_REMOTE_COMMANDS["remote_commands"]
    # 提交更新
    update_payload = {"thing": current_thing, "content": current_content}
    update_product(PRODUCT_UAV, update_payload, token)

def update_fixture_product(token):
    """更新治具产品的指令集"""
    product = get_product(PRODUCT_FIXTURE, token)
    if not product:
        return
    current_content = product.get("content", {})
    if "command_sets" in current_content:
        print("治具产品已有 command_sets，合并")
        current_content["command_sets"].update(FIXTURE_COMMANDS["command_sets"])
    else:
        current_content["command_sets"] = FIXTURE_COMMANDS["command_sets"]
    update_payload = {"content": current_content}
    update_product(PRODUCT_FIXTURE, update_payload, token)

def update_test_items(token, test_items_json_path):
    """从 test_items_full.json 更新测试项设备的 content.steps"""
    try:
        with open(test_items_json_path, 'r', encoding='utf-8') as f:
            test_items_data = json.load(f)
    except Exception as e:
        print(f"读取测试项 JSON 文件失败: {e}")
        return

    # 按 device_address 建立映射
    item_map = {item["device_address"]: item for item in test_items_data if "device_address" in item}

    devices = query_devices(PRODUCT_TEST_ITEM, token=token)
    print(f"找到 {len(devices)} 个测试项设备")
    updated_count = 0
    for dev in devices:
        devaddr = dev.get("devaddr")
        if not devaddr:
            continue
        if devaddr not in item_map:
            print(f"设备 {devaddr} 在测试项 JSON 中无对应数据，跳过")
            continue
        item_data = item_map[devaddr]
        steps = item_data.get("test_steps", [])
        current_content = dev.get("content", {})
        # 保留原有其他字段（如 common_params），只替换 steps
        current_content["steps"] = steps
        # 可选：更新 test_item_count 等
        if update_device(dev["objectId"], current_content, token):
            print(f"✅ 测试项 {devaddr} 更新成功")
            updated_count += 1
        else:
            print(f"❌ 测试项 {devaddr} 更新失败")
    print(f"总计更新 {updated_count} 个测试项设备")

def main():
    token = login_and_get_token()
    if not token:
        sys.exit(1)

    print("\n====== 更新工位设备指令集 ======")
    update_station_devices(token)

    print("\n====== 更新无人机产品物模型和遥控指令 ======")
    update_uav_product(token)

    print("\n====== 更新治具产品指令集 ======")
    update_fixture_product(token)

    print("\n====== 更新测试项设备步骤 ======")
    # 请确保 test_items_full.json 文件存在，路径可根据实际情况调整
    test_json_path = "test_items_full.json"  # 修改为实际路径
    update_test_items(token, test_json_path)

    print("\n所有更新完成！")

if __name__ == "__main__":
    main()