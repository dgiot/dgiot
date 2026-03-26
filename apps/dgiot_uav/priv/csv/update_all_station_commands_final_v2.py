#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
工位指令完整更新工具（最终版本）
合并所有更新，生成最终的工位指令配置
"""

import json

BACKUP_FILE = '/root/gitee/dgiot/backups/parse_db/Product_2de1b3e1b8.json'
UPDATED_FILE = '/root/gitee/dgiot/backups/parse_db/Product_2de1b3e1b8_updated.json'
OUTPUT_FILE = '/root/gitee/dgiot/backups/parse_db/Product_2de1b3e1b8_final.json'

# 工位1100的完整指令（补充Code 6）
COMMANDS_1100_FINAL = [
    {"code": 1, "name": "回正", "description": "桁行架回正到水平位置"},
    {"code": 2, "name": "向右上旋转30度", "description": "桁行架向右上方向旋转30度"},
    {"code": 3, "name": "向右下旋转30度", "description": "桁行架向右下方向旋转30度"},
    {"code": 4, "name": "左上旋转30度", "description": "桁行架向左上方向旋转30度"},
    {"code": 5, "name": "左下旋转30度", "description": "桁行架向左下方向旋转30度"},
    {"code": 6, "name": "水平位置", "description": "桁行架水平位置"},
    {"code": 7, "name": "下料送走", "description": "桁行架下料送走"}
]

# 工位1700的完整指令（更新为用户提供的正确指令）
COMMANDS_1700_FINAL = [
    {"code": 1, "name": "顺时针360度", "description": "磁航向测试辅具带动无人机顺时针旋转360度"},
    {"code": 2, "name": "复位", "description": "复位指令，磁航向测试辅具带动无人机逆时针旋转360度"},
    {"code": 3, "name": "机翼方向翻转90度", "description": "机翼方向翻转90度"},
    {"code": 4, "name": "辅具向机翼方向反向翻转90度", "description": "辅具向机翼方向反向翻转90度"},
    {"code": 5, "name": "成品下料", "description": "成品下料"}
]

# 工位1500和1600的完整指令（正确名称版本）
COMMANDS_1500_1600_CORRECT = [
    {"code": 1, "name": "水平", "description": "水平"},
    {"code": 2, "name": "右滚90", "description": "右滚90"},
    {"code": 3, "name": "抬头90", "description": "抬头90"},
    {"code": 4, "name": "上升H1-5", "description": "上升H1-5"},
    {"code": 5, "name": "上升H6-9", "description": "上升H6-9"},
    {"code": 6, "name": "绕X轴", "description": "绕X轴"},
    {"code": 7, "name": "2°/s", "description": "2°/s"},
    {"code": 8, "name": "抬头", "description": "抬头"},
    {"code": 9, "name": "低头", "description": "低头"},
    {"code": 10, "name": "左滚", "description": "左滚"},
    {"code": 11, "name": "右滚", "description": "右滚"},
    {"code": 12, "name": "左偏航", "description": "左偏航"},
    {"code": 13, "name": "右偏航", "description": "右偏航"},
    {"code": 14, "name": "折翼", "description": "折翼"},
    {"code": 15, "name": "噪音", "description": "噪音"},
    {"code": 16, "name": "转速", "description": "转速"}
]

def merge_all_station_commands():
    """合并所有工位指令更新"""
    print("=" * 80)
    print("🔧 工位指令完整更新工具（最终版本）")
    print("=" * 80)
    print()

    # 读取备份文件
    print("📋 步骤1：读取备份文件")
    print("-" * 80)
    with open(BACKUP_FILE, 'r', encoding='utf-8') as f:
        product = json.load(f)

    product_id = product.get('objectId', '')
    print(f"✅ 产品ID: {product_id}")
    print()

    # 获取工位指令配置
    content = product.get('content', {})
    station_commands = content.get('station_commands', {})

    # 更新工位1100
    print("📋 步骤2：更新工位1100的指令（补充Code 6）")
    print("-" * 80)
    new_station_1100 = {
        'base_address': 'D1100',
        'commands': COMMANDS_1100_FINAL,
        'name': '桁架机械手工位'
    }
    station_commands['1100'] = new_station_1100
    print(f"✅ 工位1100已更新为 {len(COMMANDS_1100_FINAL)} 条指令")
    print()

    # 更新工位1500
    print("📋 步骤3：更新工位1500的指令（正确名称版本）")
    print("-" * 80)
    new_station_1500 = {
        'base_address': 'D1500',
        'commands': COMMANDS_1500_1600_CORRECT,
        'name': '总测工位1'
    }
    station_commands['1500'] = new_station_1500
    print(f"✅ 工位1500已更新为 {len(COMMANDS_1500_1600_CORRECT)} 条指令")
    print()

    # 更新工位1600（镜像工位1500）
    print("📋 步骤4：更新工位1600的指令（镜像工位1500）")
    print("-" * 80)
    new_commands_1600 = []
    for cmd in COMMANDS_1500_1600_CORRECT:
        new_commands_1600.append({
            'code': cmd['code'],
            'name': cmd['name'],
            'description': cmd['description']
        })

    new_station_1600 = {
        'base_address': 'D1600',
        'commands': new_commands_1600,
        'name': '总测工位2'
    }
    station_commands['1600'] = new_station_1600
    print(f"✅ 工位1600已更新为 {len(new_commands_1600)} 条指令")
    print()

    # 更新工位1700
    print("📋 步骤5：更新工位1700的指令")
    print("-" * 80)
    new_station_1700 = {
        'base_address': 'D1751',
        'commands': COMMANDS_1700_FINAL,
        'name': '磁航向工位'
    }
    station_commands['1700'] = new_station_1700
    print(f"✅ 工位1700已更新为 {len(COMMANDS_1700_FINAL)} 条指令")
    print()

    # 更新content
    content['station_commands'] = station_commands
    product['content'] = content

    # 保存到文件
    print("📋 步骤6：保存最终数据")
    print("-" * 80)
    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        json.dump(product, f, ensure_ascii=False, indent=2)
    print(f"✅ 最终数据已保存到: {OUTPUT_FILE}")
    print()

    # 显示所有工位的指令清单
    print("📋 步骤7：显示所有工位的完整指令清单")
    print("-" * 80)
    print()

    for station_id in sorted(station_commands.keys()):
        station = station_commands[station_id]
        commands = station.get('commands', [])
        print(f"工位{station_id}（{station.get('name', '')}）- 基地址：{station.get('base_address', '')}")
        for cmd in commands:
            print(f"  Code {cmd['code']}: {cmd['name']} - {cmd['description']}")
        print()

    # 验证镜像一致性
    print("📋 步骤8：验证工位1500和1600的镜像一致性")
    print("-" * 80)
    commands_1500 = station_commands['1500']['commands']
    commands_1600 = station_commands['1600']['commands']

    if len(commands_1500) == len(commands_1600):
        print(f"✅ 指令数量一致: {len(commands_1500)} 条")

    all_match = True
    for i, (cmd1500, cmd1600) in enumerate(zip(commands_1500, commands_1600)):
        if cmd1500['name'] != cmd1600['name']:
            print(f"❌ Code {i+1} 不一致: {cmd1500['name']} vs {cmd1600['name']}")
            all_match = False

    if all_match:
        print("✅ 所有指令名称一致")
        print("✅ 工位1500和工位1600的镜像一致性已建立")

    print()
    print("=" * 80)
    print("✅ 所有工位指令更新完成")
    print("=" * 80)

if __name__ == '__main__':
    merge_all_station_commands()
