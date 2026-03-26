#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
工位指令修复脚本 - 复制工位1500的16条指令到工位1600
确保两条产线的镜像一致性
"""

import json
import sys

# 配置
BACKUP_FILE = "/root/gitee/dgiot/backups/parse_db/Product_2de1b3e1b8.json"
OUTPUT_FILE = "/root/gitee/dgiot/backups/parse_db/Product_2de1b3e1b8_fixed.json"

def fix_station_commands():
    """修复工位1600的指令，使其与工位1500保持一致"""
    print("=" * 80)
    print("🔧 工位指令修复工具")
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
    
    # 2. 检查工位1500和工位1600的指令
    print("\n📋 步骤2：检查工位1500和工位1600的指令")
    print("-" * 80)
    
    station_1500 = station_commands.get('1500', {})
    station_1600 = station_commands.get('1600', {})
    
    commands_1500 = station_1500.get('commands', [])
    commands_1600 = station_1600.get('commands', [])
    
    print(f"工位1500（总测工位1）:")
    print(f"  工位名称: {station_1500.get('name', '未知')}")
    print(f"  基地址: {station_1500.get('base_address', '未知')}")
    print(f"  指令数量: {len(commands_1500)}")
    print(f"  指令编号: {', '.join([str(cmd.get('code', 0)) for cmd in commands_1500])}")
    
    print(f"\n工位1600（总测工位2）:")
    print(f"  工位名称: {station_1600.get('name', '未知')}")
    print(f"  基地址: {station_1600.get('base_address', '未知')}")
    print(f"  指令数量: {len(commands_1600)}")
    print(f"  指令编号: {', '.join([str(cmd.get('code', 0)) for cmd in commands_1600])}")
    
    # 3. 复制工位1500的指令到工位1600
    print("\n📋 步骤3：复制工位1500的指令到工位1600")
    print("-" * 80)
    
    new_commands = []
    for cmd in commands_1500:
        code = cmd.get('code', 0)
        name = cmd.get('name', '')
        description = cmd.get('description', '')
        
        # 修改描述，将"总测工位1"改为"总测工位2"
        new_description = description.replace('总测工位1', '总测工位2')
        
        new_commands.append({
            'code': code,
            'name': name,
            'description': new_description
        })
    
    # 更新工位1600的指令
    new_station_1600 = {
        'base_address': 'D1600',
        'commands': new_commands,
        'name': '总测工位2'
    }
    
    station_commands['1600'] = new_station_1600
    content['station_commands'] = station_commands
    product['content'] = content
    
    print(f"✅ 已复制 {len(new_commands)} 条指令到工位1600")
    print(f"✅ 指令编号: {', '.join([str(cmd.get('code', 0)) for cmd in new_commands])}")
    
    # 4. 保存修复后的数据
    print("\n📋 步骤4：保存修复后的数据")
    print("-" * 80)
    
    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        json.dump(product, f, ensure_ascii=False, indent=2)
    
    print(f"✅ 修复后的数据已保存到: {OUTPUT_FILE}")
    
    # 5. 显示修复后的工位1600指令列表
    print("\n📋 步骤5：显示修复后的工位1600指令列表")
    print("-" * 80)
    
    print(f"\n工位1600（总测工位2）修复后:")
    print(f"  工位名称: {new_station_1600.get('name', '未知')}")
    print(f"  基地址: {new_station_1600.get('base_address', '未知')}")
    print(f"  指令数量: {len(new_commands)}")
    print(f"\n详细指令列表:")
    
    for cmd in new_commands:
        code = cmd.get('code', 0)
        name = cmd.get('name', '')
        description = cmd.get('description', '')
        print(f"  Code {code}: {name} - {description}")
    
    # 6. 验证镜像一致性
    print("\n📋 步骤6：验证镜像一致性")
    print("-" * 80)
    
    if len(new_commands) == len(commands_1500):
        print(f"✅ 工位1500和工位1600的指令数量一致: {len(new_commands)}条")
        print(f"✅ 两条产线的镜像一致性已建立")
    else:
        print(f"❌ 工位1500有{len(commands_1500)}条指令，工位1600有{len(new_commands)}条指令")
        print(f"❌ 镜像一致性未建立")
        return False
    
    return True

if __name__ == '__main__':
    try:
        success = fix_station_commands()
        
        print("\n" + "=" * 80)
        if success:
            print("✅ 工位指令修复完成")
            print("=" * 80)
            sys.exit(0)
        else:
            print("❌ 工位指令修复失败")
            print("=" * 80)
            sys.exit(1)
    except Exception as e:
        print(f"\n❌ 修复过程中发生错误: {str(e)}")
        import traceback
        traceback.print_exc()
        print("=" * 80)
        sys.exit(1)
