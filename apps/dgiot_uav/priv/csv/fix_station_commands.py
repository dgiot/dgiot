#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
工位指令修复脚本
功能：将工位1500的16条指令复制到工位1600，确保两条产线的镜像一致性
"""

import json
import sys
import os

# 添加项目根目录到路径
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..', '..'))

try:
    from dgiot_parse import dgiot_parse
except ImportError:
    print("❌ 无法导入dgiot_parse模块，请确保DG-IoT系统正在运行")
    sys.exit(1)

def fix_station_commands():
    """
    修复工位1600的指令，使其与工位1500保持一致
    """
    print("=" * 80)
    print("🔧 工位指令修复工具")
    print("=" * 80)
    
    try:
        # 1. 查询工位产品
        print("\n📋 步骤1：查询工位产品数据")
        print("-" * 80)
        
        result = dgiot_parse.get_object("Product", "2de1b3e1b8")
        if not result:
            print("❌ 查询工位产品失败")
            return False
        
        content = result.get('content', {})
        station_commands = content.get('station_commands', {})
        
        print(f"✅ 工位产品ID: 2de1b3e1b8")
        print(f"✅ 工位数量: {len(station_commands)}")
        
        # 2. 检查工位1500和工位1600的指令
        print("\n📋 步骤2：检查工位1500和工位1600的指令")
        print("-" * 80)
        
        station_1500 = station_commands.get('1500', {})
        station_1600 = station_commands.get('1600', {})
        
        commands_1500 = station_1500.get('commands', [])
        commands_1600 = station_1600.get('commands', [])
        
        print(f"工位1500（总测1）:")
        print(f"  指令数量: {len(commands_1500)}")
        print(f"  指令编号: {', '.join([str(cmd.get('code', 0)) for cmd in commands_1500])}")
        
        print(f"\n工位1600（总测2）:")
        print(f"  指令数量: {len(commands_1600)}")
        print(f"  指令编号: {', '.join([str(cmd.get('code', 0)) for cmd in commands_1600])}")
        
        # 3. 确认修复
        print("\n📋 步骤3：确认修复操作")
        print("-" * 80)
        
        if len(commands_1500) == len(commands_1600):
            print("✅ 工位1500和工位1600的指令数量已经一致，无需修复")
            return True
        
        print(f"\n⚠️ 工位1500有 {len(commands_1500)} 条指令")
        print(f"⚠️ 工位1600有 {len(commands_1600)} 条指令")
        print(f"⚠️ 需要为工位1600添加 {len(commands_1500) - len(commands_1600)} 条指令")
        
        # 4. 复制指令
        print("\n📋 步骤4：复制工位1500的指令到工位1600")
        print("-" * 80)
        
        new_commands = []
        for cmd in commands_1500:
            new_cmd = {
                'code': cmd.get('code', 0),
                'name': cmd.get('name', ''),
                'description': cmd.get('description', ''),
                'modbus': {
                    'function_code': cmd.get('modbus', {}).get('function_code', '03'),
                    'register_address': update_address(cmd.get('modbus', {}).get('register_address', '0000')),
                    'register_value': cmd.get('modbus', {}).get('register_value', '0000'),
                    'register_count': cmd.get('modbus', {}).get('register_count', 1)
                }
            }
            new_commands.append(new_cmd)
        
        # 更新工位1600的指令
        station_1600['commands'] = new_commands
        station_commands['1600'] = station_1600
        content['station_commands'] = station_commands
        
        print(f"✅ 已复制 {len(new_commands)} 条指令到工位1600")
        
        # 5. 备份并更新
        print("\n📋 步骤5：备份并更新Parse库数据")
        print("-" * 80)
        
        # 备份原始数据
        backup_file = f"/root/gitee/dgiot/backups/parse_db/Product_2de1b3e1b8_backup_{int(os.path.time())}.json"
        with open(backup_file, 'w', encoding='utf-8') as f:
            json.dump(result, f, ensure_ascii=False, indent=2)
        print(f"✅ 原始数据已备份到: {backup_file}")
        
        # 更新Parse库
        result['content'] = content
        update_result = dgiot_parse.update_object("Product", "2de1b3e1b8", result)
        
        if update_result:
            print("✅ Parse库数据更新成功")
        else:
            print("❌ Parse库数据更新失败")
            return False
        
        # 6. 验证修复结果
        print("\n📋 步骤6：验证修复结果")
        print("-" * 80)
        
        result = dgiot_parse.get_object("Product", "2de1b3e1b8")
        content = result.get('content', {})
        station_commands = content.get('station_commands', {})
        station_1600 = station_commands.get('1600', {})
        commands_1600 = station_1600.get('commands', [])
        
        print(f"工位1600（总测2）:")
        print(f"  指令数量: {len(commands_1600)}")
        print(f"  指令编号: {', '.join([str(cmd.get('code', 0)) for cmd in commands_1600])}")
        
        if len(commands_1600) == len(commands_1500):
            print("\n✅ 工位1600的指令数量已与工位1500保持一致")
            return True
        else:
            print(f"\n❌ 工位1600的指令数量不正确，预期 {len(commands_1500)}，实际 {len(commands_1600)}")
            return False
        
    except Exception as e:
        print(f"\n❌ 修复过程中发生错误: {str(e)}")
        import traceback
        traceback.print_exc()
        return False

def update_address(address_str):
    """
    更新寄存器地址，将工位1500的地址映射到工位1600的地址范围
    
    工位1500地址范围: 1000-5999
    工位1600地址范围: 1100-6099
    """
    try:
        address = int(address_str, 16)
        
        # 映射地址范围
        if 1000 <= address <= 1999:  # 控制寄存器
            return hex(address + 100)[2:].upper().zfill(4)
        elif 2000 <= address <= 2999:  # 数据寄存器
            return hex(address + 100)[2:].upper().zfill(4)
        elif 3000 <= address <= 3999:  # 参数寄存器
            return hex(address + 100)[2:].upper().zfill(4)
        elif 4000 <= address <= 4999:  # 程序寄存器
            return hex(address + 100)[2:].upper().zfill(4)
        elif 5000 <= address <= 5999:  # 结果寄存器
            return hex(address + 100)[2:].upper().zfill(4)
        else:
            return address_str  # 其他地址不映射
            
    except (ValueError, AttributeError):
        return address_str

if __name__ == '__main__':
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
