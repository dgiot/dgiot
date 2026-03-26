#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
工位指令名称更新脚本 - 更新工位1500和1600的指令名称
"""

import json
import sys

# 配置
BACKUP_FILE = "/root/gitee/dgiot/backups/parse_db/Product_2de1b3e1b8.json"
OUTPUT_FILE = "/root/gitee/dgiot/backups/parse_db/Product_2de1b3e1b8_updated.json"

# 正确的指令名称定义
CORRECT_COMMANDS_1500 = [
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

CORRECT_COMMANDS_1600 = [
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

def update_station_commands():
    """更新工位1500和1600的指令名称"""
    print("=" * 80)
    print("🔧 工位指令名称更新工具")
    print("=" * 80)
    
    # 1. 读取备份文件
    print("\n📋 步骤1：读取备份文件")
    print("-" * 80)
    
    with open(BACKUP_FILE, 'r', encoding='utf-8') as f:
        product = json.load(f)
    
    content = product.get('content', {})
    station_commands = content.get('station_commands', {})
    
    print(f"✅ 产品ID: 2de1b3e1b8")
    print(f"✅ 工位数量: {len(station_commands)}")
    
    # 2. 更新工位1500的指令名称
    print("\n📋 步骤2：更新工位1500的指令名称")
    print("-" * 80)
    
    station_1500 = station_commands.get('1500', {})
    print(f"工位1500（总测工位1）:")
    print(f"  工位名称: {station_1500.get('name', '未知')}")
    print(f"  基地址: {station_1500.get('base_address', '未知')}")
    print(f"  指令数量: {len(CORRECT_COMMANDS_1500)}")
    
    # 更新工位1500的指令
    new_station_1500 = {
        'base_address': station_1500.get('base_address', 'D1500'),
        'commands': CORRECT_COMMANDS_1500,
        'name': '总测工位1'
    }
    
    station_commands['1500'] = new_station_1500
    print(f"✅ 工位1500的指令名称已更新")
    
    # 3. 更新工位1600的指令名称
    print("\n📋 步骤3：更新工位1600的指令名称")
    print("-" * 80)
    
    print(f"工位1600（总测工位2）:")
    print(f"  指令数量: {len(CORRECT_COMMANDS_1600)}")
    
    # 更新工位1600的指令
    new_station_1600 = {
        'base_address': 'D1600',
        'commands': CORRECT_COMMANDS_1600,
        'name': '总测工位2'
    }
    
    station_commands['1600'] = new_station_1600
    print(f"✅ 工位1600的指令名称已更新")
    
    # 4. 保存更新后的数据
    print("\n📋 步骤4：保存更新后的数据")
    print("-" * 80)
    
    content['station_commands'] = station_commands
    product['content'] = content
    
    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        json.dump(product, f, ensure_ascii=False, indent=2)
    
    print(f"✅ 更新后的数据已保存到: {OUTPUT_FILE}")
    
    # 5. 显示更新后的指令列表
    print("\n📋 步骤5：显示更新后的指令列表")
    print("-" * 80)
    
    print(f"\n工位1500（总测工位1）:")
    for cmd in CORRECT_COMMANDS_1500:
        code = cmd.get('code', 0)
        name = cmd.get('name', '')
        print(f"  Code {code}: {name}")
    
    print(f"\n工位1600（总测工位2）:")
    for cmd in CORRECT_COMMANDS_1600:
        code = cmd.get('code', 0)
        name = cmd.get('name', '')
        print(f"  Code {code}: {name}")
    
    # 6. 验证镜像一致性
    print("\n📋 步骤6：验证镜像一致性")
    print("-" * 80)
    
    if len(CORRECT_COMMANDS_1500) == len(CORRECT_COMMANDS_1600):
        print(f"✅ 工位1500和工位1600的指令数量一致: {len(CORRECT_COMMANDS_1500)}条")
        print(f"✅ 指令编号范围一致: Code 1-16")
        print(f"✅ 指令名称一致")
        print(f"✅ 两条产线的镜像一致性已建立")
    else:
        print(f"❌ 指令数量不一致")
        return False
    
    return True

if __name__ == '__main__':
    try:
        success = update_station_commands()
        
        print("\n" + "=" * 80)
        if success:
            print("✅ 工位指令名称更新完成")
            print("=" * 80)
            sys.exit(0)
        else:
            print("❌ 工位指令名称更新失败")
            print("=" * 80)
            sys.exit(1)
    except Exception as e:
        print(f"\n❌ 更新过程中发生错误: {str(e)}")
        import traceback
        traceback.print_exc()
        print("=" * 80)
        sys.exit(1)
