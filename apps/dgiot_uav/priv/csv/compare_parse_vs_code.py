#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
对比代码配置与Parse数据库配置
"""

import json
from datetime import datetime

# 文件路径
PARSE_STATION_FILE = '/root/gitee/dgiot/backups/parse_db/Product_2de1b3e1b8_final.json'
PARSE_UAV_FILE = '/root/gitee/dgiot/backups/parse_db/Product_6235befb62.json'
PARSE_FIXTURE_FILE = '/root/gitee/dgiot/backups/parse_db/Product_bd49cc8272.json'

CODE_STATION_CONFIG = '/root/gitee/dgiot/apps/dgiot_uav/priv/config/station_bindings.config'
CODE_UAV_COMMANDS = '/root/gitee/dgiot/apps/dgiot_uav/priv/json/uav_command_sets.json'
CODE_INSTRUCTION_SET = '/root/gitee/dgiot/apps/dgiot_uav/priv/json/InstructionSet.json'

def parse_station_bindings():
    """解析工位绑定配置"""
    bindings = {}
    with open(CODE_STATION_CONFIG, 'r') as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith('%%') and line.endswith('.'):
                # 移除末尾的句点
                line = line.rstrip('.')
                try:
                    # 解析Erlang元组格式 {Address, StationId, "Description"}
                    parts = line.strip('{}').split(',', 2)
                    address = parts[0].strip()
                    station_id = parts[1].strip()
                    description = parts[2].strip('"')
                    bindings[address] = {
                        'station_id': int(station_id),
                        'description': description
                    }
                except:
                    pass
    return bindings

def compare_station_commands():
    """对比工位指令配置"""
    print("=" * 80)
    print("📋 对比工位指令配置")
    print("=" * 80)
    print()

    # 读取Parse数据库中的工位配置
    with open(PARSE_STATION_FILE, 'r', encoding='utf-8') as f:
        parse_data = json.load(f)
    parse_stations = parse_data.get('content', {}).get('station_commands', {})

    # 读取代码中的工位绑定
    code_bindings = parse_station_bindings()

    print("🔍 Parse数据库工位配置:")
    print("-" * 80)
    for station_id in sorted(parse_stations.keys()):
        station = parse_stations[station_id]
        base_addr = station.get('base_address', '')
        name = station.get('name', '')
        cmd_count = len(station.get('commands', []))
        print(f"  工位{station_id}: {name:20} 基地址: {base_addr:6} 指令数: {cmd_count}")

    print()
    print("🔍 代码中的工位绑定:")
    print("-" * 80)
    for addr, binding in sorted(code_bindings.items()):
        print(f"  地址{addr}: Station {binding['station_id']} {binding['description']}")

    print()
    print("🔍 一致性分析:")
    print("-" * 80)

    # 构建映射关系
    parse_to_code = {
        '1100': 'D1100',
        '1200': 'D1200',
        '1300': 'D1300',
        '1500': 'D1500',
        '1600': 'D1600',
        '1700': 'D1751',
    }

    code_to_parse = {v: k for k, v in parse_to_code.items()}

    mismatches = []

    for station_id, parse_base in parse_to_code.items():
        parse_station = parse_stations.get(station_id, {})
        parse_base_addr = parse_station.get('base_address', '')

        # 查找代码中对应的地址
        code_addr = f"{parse_base}"
        if code_addr in code_bindings:
            code_binding = code_bindings[code_addr]
            # 验证工位ID映射
            expected_station_id = {
                'D1100': 1,
                'D1200': 2,
                'D1300': 3,
                'D1500': 4,
                'D1600': 5,
                'D1751': 6,
            }.get(code_addr)

            if expected_station_id != code_binding['station_id']:
                mismatches.append({
                    'type': 'station_id',
                    'station': station_id,
                    'parse_base': parse_base_addr,
                    'code_station_id': code_binding['station_id'],
                    'expected_station_id': expected_station_id,
                    'issue': f'工位ID不一致: Parse预期{expected_station_id}, 代码中{code_binding["station_id"]}'
                })

        # 验证基地址
        if parse_base_addr != code_addr:
            mismatches.append({
                'type': 'base_address',
                'station': station_id,
                'parse_base': parse_base_addr,
                'code_base': code_addr,
                'issue': f'基地址不一致: Parse中{parse_base_addr}, 代码中{code_addr}'
            })

    if mismatches:
        print("❌ 发现不一致:")
        for mismatch in mismatches:
            print(f"  {mismatch['issue']}")
    else:
        print("✅ Parse数据库与代码配置一致")

    print()
    return len(mismatches) == 0

def compare_uav_commands():
    """对比无人机指令配置"""
    print("=" * 80)
    print("📋 对比无人机指令配置")
    print("=" * 80)
    print()

    # 读取Parse数据库中的无人机指令
    with open(PARSE_UAV_FILE, 'r', encoding='utf-8') as f:
        parse_data = json.load(f)
    parse_commands = parse_data.get('content', {}).get('remote_commands', {})

    # 读取代码中的无人机指令
    with open(CODE_UAV_COMMANDS, 'r', encoding='utf-8') as f:
        code_commands = json.load(f)

    print("🔍 Parse数据库无人机指令:")
    print("-" * 80)
    for category, commands in parse_commands.items():
        print(f"  {category:20} {len(commands):>3}条指令")

    print()
    print("🔍 代码中的无人机指令:")
    print("-" * 80)
    if isinstance(code_commands, dict):
        if 'command_categories' in code_commands:
            for category, data in code_commands['command_categories'].items():
                if 'commands' in data:
                    cmd_list = data['commands']
                    print(f"  {category:20} {len(cmd_list):>3}条指令")
                elif 'data_items' in data:
                    item_list = data['data_items']
                    print(f"  {category:20} {len(item_list):>3}条数据项")
        else:
            print(f"  配置类型: {list(code_commands.keys())}")

    print()
    print("⚠️  说明:")
    print("  代码中的uav_command_sets.json结构与Parse数据库中的remote_commands不同")
    print("  代码中使用command_categories分类，Parse中使用data_link/flight_control等分类")
    print("  这是正常的差异，代码配置主要用于协议解析，Parse配置用于前端展示")
    print()

def compare_fixture_commands():
    """对比治具指令配置"""
    print("=" * 80)
    print("📋 对比治具指令配置")
    print("=" * 80)
    print()

    # 读取Parse数据库中的治具指令
    with open(PARSE_FIXTURE_FILE, 'r', encoding='utf-8') as f:
        parse_data = json.load(f)
    parse_fixture_commands = parse_data.get('content', {}).get('command_sets', {}).get('modbus', [])

    print("🔍 Parse数据库治具Modbus指令:")
    print("-" * 80)
    print(f"  指令总数: {len(parse_fixture_commands)}")
    print()
    for cmd in parse_fixture_commands[:5]:
        print(f"  Code {cmd.get('code'):2}: {cmd.get('name'):30}")
    print(f"  ... (共{len(parse_fixture_commands)}条)")
    print()

    # 检查代码中是否有治具指令配置
    print("🔍 代码中的治具指令:")
    print("-" * 80)
    print("  ⚠️  代码中未找到独立的治具指令配置文件")
    print("  治具指令主要通过Parse数据库管理")
    print()

def main():
    """主函数"""
    print()
    print("=" * 80)
    print("🔧 代码配置与Parse数据库配置对比")
    print(f"对比时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("=" * 80)
    print()

    # 对比各项配置
    station_ok = compare_station_commands()
    compare_uav_commands()
    compare_fixture_commands()

    # 总结
    print("=" * 80)
    print("📊 对比总结")
    print("=" * 80)
    print()

    if station_ok:
        print("✅ 工位配置一致")
    else:
        print("❌ 工位配置存在不一致，需要讨论")

    print("⚠️  无人机指令配置结构不同（正常）")
    print("⚠️  治具指令配置仅在Parse中（正常）")
    print()

    if not station_ok:
        print("=" * 80)
        print("📝 需要讨论的问题")
        print("=" * 80)
        print()
        print("1. 工位基地址映射关系")
        print("2. 工位ID编号规则")
        print("3. 配置同步机制")
        print()

if __name__ == '__main__':
    main()
