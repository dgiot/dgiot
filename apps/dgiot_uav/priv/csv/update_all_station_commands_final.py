#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
工位指令完整更新工具
更新所有工位的指令定义
"""

import json

BACKUP_FILE = '/root/gitee/dgiot/backups/parse_db/Product_2de1b3e1b8.json'
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

def update_all_station_commands():
    """更新所有工位的指令"""
    print("=" * 80)
    print("🔧 工位指令完整更新工具")
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

    print("📋 步骤2：更新工位1100的指令（补充Code 6）")
    print("-" * 80)
    print("工位1100（桁架机械手工位）:")
    print(f"  工位名称: 桁架机械手工位")
    print(f"  基地址: D1100")
    print(f"  指令数量: {len(COMMANDS_1100_FINAL)}")

    new_station_1100 = {
        'base_address': 'D1100',
        'commands': COMMANDS_1100_FINAL,
        'name': '桁架机械手工位'
    }
    station_commands['1100'] = new_station_1100
    print("✅ 工位1100的指令已更新")
    print()

    print("📋 步骤3：更新工位1700的指令")
    print("-" * 80)
    print("工位1700（磁航向工位）:")
    print(f"  工位名称: 磁航向工位")
    print(f"  基地址: D1751")
    print(f"  指令数量: {len(COMMANDS_1700_FINAL)}")

    for cmd in COMMANDS_1700_FINAL:
        print(f"  Code {cmd['code']}: {cmd['name']} - {cmd['description']}")

    new_station_1700 = {
        'base_address': 'D1751',
        'commands': COMMANDS_1700_FINAL,
        'name': '磁航向工位'
    }
    station_commands['1700'] = new_station_1700
    print("✅ 工位1700的指令已更新")
    print()

    # 确认工位1500和1600已经是最新的
    print("📋 步骤4：确认工位1500和1600的指令")
    print("-" * 80)
    station_1500 = station_commands.get('1500', {})
    station_1600 = station_commands.get('1600', {})

    print(f"工位1500指令数量: {len(station_1500.get('commands', []))}")
    print(f"工位1600指令数量: {len(station_1600.get('commands', []))}")
    print()

    # 更新content
    content['station_commands'] = station_commands
    product['content'] = content

    # 保存到文件
    print("📋 步骤5：保存最终数据")
    print("-" * 80)
    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        json.dump(product, f, ensure_ascii=False, indent=2)
    print(f"✅ 最终数据已保存到: {OUTPUT_FILE}")
    print()

    # 显示所有工位的指令清单
    print("📋 步骤6：显示所有工位的完整指令清单")
    print("-" * 80)
    print()

    for station_id in sorted(station_commands.keys()):
        station = station_commands[station_id]
        commands = station.get('commands', [])
        print(f"工位{station_id}（{station.get('name', '')}）- 基地址：{station.get('base_address', '')}")
        for cmd in commands:
            print(f"  Code {cmd['code']}: {cmd['name']} - {cmd['description']}")
        print()

    print("=" * 80)
    print("✅ 所有工位指令更新完成")
    print("=" * 80)

if __name__ == '__main__':
    update_all_station_commands()
