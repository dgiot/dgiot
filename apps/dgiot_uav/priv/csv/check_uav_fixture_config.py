#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
核对无人机和治具的配置
"""

import json
from datetime import datetime

UAV_FILE = '/root/gitee/dgiot/backups/parse_db/Product_6235befb62.json'
FIXTURE_FILE = '/root/gitee/dgiot/backups/parse_db/Product_bd49cc8272.json'
OUTPUT_FILE = '/root/gitee/dgiot/apps/dgiot_uav/priv/csv/无人机治具配置核对报告.txt'

def check_uav_config():
    """核对无人机配置"""
    with open(UAV_FILE, 'r', encoding='utf-8') as f:
        data = json.load(f)

    print("=" * 80)
    print("🚁 无人机产品配置核对")
    print("=" * 80)
    print()

    product_name = data.get('name', '')
    product_id = data.get('objectId', '')
    content = data.get('content', {})
    remote_commands = content.get('remote_commands', {})

    print(f"产品名称: {product_name}")
    print(f"产品ID: {product_id}")
    print(f"更新时间: {data.get('updatedAt', '')}")
    print()

    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("=" * 80 + "\n")
        f.write(f"🚁 无人机产品配置核对\n")
        f.write(f"核对时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write("=" * 80 + "\n\n")

        f.write(f"产品名称: {product_name}\n")
        f.write(f"产品ID: {product_id}\n")
        f.write(f"更新时间: {data.get('updatedAt', '')}\n\n")

    # 遥控数据链指令
    print("📡 遥控数据链指令 (data_link)")
    print("-" * 80)
    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("📡 遥控数据链指令 (data_link)\n")
        f.write("-" * 80 + "\n")

    for cmd in remote_commands.get('data_link', []):
        code = cmd.get('code')
        name = cmd.get('name')
        description = cmd.get('description')
        print(f"  Code {code}: {name} - {description}")
        with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
            f.write(f"Code {code}: {name} - {description}\n")

    print()
    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("\n")

    # 飞控指令
    print("✈️ 飞控指令 (flight_control)")
    print("-" * 80)
    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("✈️ 飞控指令 (flight_control)\n")
        f.write("-" * 80 + "\n")

    for cmd in remote_commands.get('flight_control', []):
        code = cmd.get('code')
        name = cmd.get('name')
        description = cmd.get('description')
        print(f"  Code {code}: {name} - {description}")
        with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
            f.write(f"Code {code}: {name} - {description}\n")

    print()
    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("\n")

    # 导引头指令
    print("🎯 导引头指令 (guidance_head)")
    print("-" * 80)
    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("🎯 导引头指令 (guidance_head)\n")
        f.write("-" * 80 + "\n")

    for cmd in remote_commands.get('guidance_head', []):
        code = cmd.get('code')
        name = cmd.get('name')
        description = cmd.get('description')
        print(f"  Code {code}: {name} - {description}")
        with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
            f.write(f"Code {code}: {name} - {description}\n")

    print()
    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("\n")

    # 载荷控制指令
    print("📦 载荷控制指令 (payload_control)")
    print("-" * 80)
    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("📦 载荷控制指令 (payload_control)\n")
        f.write("-" * 80 + "\n")

    for cmd in remote_commands.get('payload_control', []):
        code = cmd.get('code')
        name = cmd.get('name')
        description = cmd.get('description')
        print(f"  Code {code}: {name} - {description}")
        with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
            f.write(f"Code {code}: {name} - {description}\n")

    print()
    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("\n\n")

def check_fixture_config():
    """核对治具配置"""
    with open(FIXTURE_FILE, 'r', encoding='utf-8') as f:
        data = json.load(f)

    print("=" * 80)
    print("🔧 治具产品配置核对")
    print("=" * 80)
    print()

    product_name = data.get('name', '')
    product_id = data.get('objectId', '')
    content = data.get('content', {})

    print(f"产品名称: {product_name}")
    print(f"产品ID: {product_id}")
    print(f"更新时间: {data.get('updatedAt', '')}")
    print()

    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("=" * 80 + "\n")
        f.write(f"🔧 治具产品配置核对\n")
        f.write(f"核对时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write("=" * 80 + "\n\n")

        f.write(f"产品名称: {product_name}\n")
        f.write(f"产品ID: {product_id}\n")
        f.write(f"更新时间: {data.get('updatedAt', '')}\n\n")

    # 治具配置信息
    print("⚙️ 治具配置信息")
    print("-" * 80)
    print(f"治具类型: {content.get('fixture_type', 'N/A')}")
    print(f"协议: {content.get('protocol', 'N/A')}")
    print(f"Slave ID: {content.get('slave_id', 'N/A')}")
    print(f"指令总数: {content.get('total_commands', 'N/A')}")
    print()

    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("⚙️ 治具配置信息\n")
        f.write("-" * 80 + "\n")
        f.write(f"治具类型: {content.get('fixture_type', 'N/A')}\n")
        f.write(f"协议: {content.get('protocol', 'N/A')}\n")
        f.write(f"Slave ID: {content.get('slave_id', 'N/A')}\n")
        f.write(f"指令总数: {content.get('total_commands', 'N/A')}\n\n")

    # 治具指令
    print("🔧 治具指令 (fixture_commands)")
    print("-" * 80)
    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("🔧 治具指令 (fixture_commands)\n")
        f.write("-" * 80 + "\n")

    fixture_commands = content.get('fixture_commands', [])
    print(f"指令数量: {len(fixture_commands)}")

    for i, cmd in enumerate(fixture_commands, 1):
        print(f"\n指令 {i}:")
        print(f"  Code: {cmd.get('code', 'N/A')}")
        print(f"  Name: {cmd.get('name', 'N/A')}")
        print(f"  Description: {cmd.get('description', 'N/A')}")

        with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
            f.write(f"\n指令 {i}:\n")
            f.write(f"  Code: {cmd.get('code', 'N/A')}\n")
            f.write(f"  Name: {cmd.get('name', 'N/A')}\n")
            f.write(f"  Description: {cmd.get('description', 'N/A')}\n")

    # 治具指令集
    command_sets = content.get('command_sets', {})
    print()
    print("📚 治具指令集 (command_sets)")
    print("-" * 80)
    print(f"指令集数量: {len(command_sets)}")

    for key, value in command_sets.items():
        print(f"\n{key}: {len(value)}条指令")

        with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
            f.write(f"\n{key}: {len(value)}条指令\n")

    print()
    with open(OUTPUT_FILE, 'a', encoding='utf-8') as f:
        f.write("\n\n")

if __name__ == '__main__':
    # 清空输出文件
    with open(OUTPUT_FILE, 'w', encoding='utf-8') as f:
        f.write("")

    check_uav_config()
    check_fixture_config()

    print("=" * 80)
    print("✅ 无人机和治具配置核对完成")
    print(f"✅ 核对报告已保存到: {OUTPUT_FILE}")
    print("=" * 80)
