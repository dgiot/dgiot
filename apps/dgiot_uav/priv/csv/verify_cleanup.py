#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
核对清理后的配置文件
"""

import os
import json
from datetime import datetime

# 路径定义
BASE_DIR = '/root/gitee/dgiot/apps/dgiot_uav'
JSON_DIR = os.path.join(BASE_DIR, 'priv/json')
CSV_DIR = os.path.join(BASE_DIR, 'priv/csv')
CONFIG_DIR = os.path.join(BASE_DIR, 'priv/config')
BACKUP_DIR = os.path.join(BASE_DIR, 'priv/config_backup')

# 重要的配置文件清单
IMPORTANT_JSON_FILES = {
    'uav_protocol.json': '无人机协议配置',
    'UAVConfig.json': '无人机配置',
    'uav_command_sets.json': '无人机指令集（Parse相关）',
    'InstructionSet.json': '指令集（Parse相关）',
    'test_items_full.json': '测试项完整版（Parse相关）',
    'test_items_summary.json': '测试项摘要（Parse相关）',
    'test_judge_rules.json': '判据规则（Parse相关）',
}

IMPORTANT_CONFIG_FILES = {
    'station_bindings.config': '工位绑定配置',
}

IMPORTANT_CSV_FILES = {
    '工位指令更新完成报告.md': '工位指令更新报告',
    '无人机治具配置核对结果.md': '无人机治具配置核对结果',
}

def verify_json_files():
    """验证JSON文件"""
    print("=" * 80)
    print("📋 验证JSON配置文件")
    print("=" * 80)
    print()

    all_valid = True

    for filename, description in IMPORTANT_JSON_FILES.items():
        filepath = os.path.join(JSON_DIR, filename)
        status = "❌ 缺失"
        size = 0
        valid = False

        if os.path.exists(filepath):
            size = os.path.getsize(filepath)
            status = "✅ 存在"
            
            # 验证JSON格式
            try:
                with open(filepath, 'r', encoding='utf-8') as f:
                    json.load(f)
                status = "✅ 有效"
                valid = True
            except json.JSONDecodeError as e:
                status = f"❌ JSON错误: {str(e)[:30]}"
                all_valid = False

        print(f"{status:15} {filename:30} {description:30} {size:>8} bytes")
        
        if not valid:
            all_valid = False

    print()
    return all_valid

def verify_config_files():
    """验证配置文件"""
    print("=" * 80)
    print("📋 验证CONFIG配置文件")
    print("=" * 80)
    print()

    all_valid = True

    for filename, description in IMPORTANT_CONFIG_FILES.items():
        filepath = os.path.join(CONFIG_DIR, filename)
        status = "❌ 缺失"
        size = 0
        valid = False

        if os.path.exists(filepath):
            size = os.path.getsize(filepath)
            status = "✅ 存在"
            valid = True

        print(f"{status:15} {filename:30} {description:30} {size:>8} bytes")
        
        if not valid:
            all_valid = False

    print()
    return all_valid

def verify_csv_files():
    """验证CSV文件"""
    print("=" * 80)
    print("📋 验证CSV文档文件")
    print("=" * 80)
    print()

    all_valid = True

    for filename, description in IMPORTANT_CSV_FILES.items():
        filepath = os.path.join(CSV_DIR, filename)
        status = "❌ 缺失"
        size = 0
        valid = False

        if os.path.exists(filepath):
            size = os.path.getsize(filepath)
            status = "✅ 存在"
            valid = True

        print(f"{status:15} {filename:30} {description:30} {size:>8} bytes")
        
        if not valid:
            all_valid = False

    print()
    return all_valid

def verify_backup():
    """验证备份文件"""
    print("=" * 80)
    print("📋 验证备份目录")
    print("=" * 80)
    print()

    if not os.path.exists(BACKUP_DIR):
        print(f"❌ 备份目录不存在: {BACKUP_DIR}")
        return False

    backup_dirs = [d for d in os.listdir(BACKUP_DIR) if os.path.isdir(os.path.join(BACKUP_DIR, d))]
    
    if not backup_dirs:
        print(f"⚠️  备份目录为空: {BACKUP_DIR}")
        return False

    latest_backup = sorted(backup_dirs)[-1]
    backup_path = os.path.join(BACKUP_DIR, latest_backup)
    
    print(f"✅ 备份目录存在: {BACKUP_DIR}")
    print(f"✅ 备份版本数量: {len(backup_dirs)}")
    print(f"✅ 最新备份: {latest_backup}")
    print()

    # 检查最新备份的子目录
    print("最新备份内容:")
    print("-" * 80)
    
    for item in os.listdir(backup_path):
        item_path = os.path.join(backup_path, item)
        if os.path.isdir(item_path):
            count = len([f for f in os.listdir(item_path)])
            print(f"  📁 {item:20} {count:>4} 文件")
        else:
            size = os.path.getsize(item_path)
            print(f"  📄 {item:20} {size:>8} bytes")
    
    print()
    return True

def check_json_content():
    """检查JSON文件内容"""
    print("=" * 80)
    print("📋 检查JSON文件内容")
    print("=" * 80)
    print()

    # 检查无人机协议
    uav_protocol_file = os.path.join(JSON_DIR, 'uav_protocol.json')
    if os.path.exists(uav_protocol_file):
        with open(uav_protocol_file, 'r', encoding='utf-8') as f:
            uav_protocol = json.load(f)
        
        print("✅ uav_protocol.json:")
        print(f"  - 协议类型: {uav_protocol.get('protocol', 'N/A')}")
        print(f"  - 版本: {uav_protocol.get('version', 'N/A')}")
        print()

    # 检查测试项
    test_items_file = os.path.join(JSON_DIR, 'test_items_full.json')
    if os.path.exists(test_items_file):
        with open(test_items_file, 'r', encoding='utf-8') as f:
            test_items = json.load(f)
        
        print("✅ test_items_full.json:")
        print(f"  - 测试项数量: {len(test_items) if isinstance(test_items, list) else 'N/A'}")
        
        if isinstance(test_items, list) and len(test_items) > 0:
            print(f"  - 第一个测试项: {test_items[0].get('name', 'N/A')}")
        print()

def generate_verification_report():
    """生成核对报告"""
    print("=" * 80)
    print("📋 配置文件核对报告")
    print("=" * 80)
    print()

    # 验证各类文件
    json_valid = verify_json_files()
    config_valid = verify_config_files()
    csv_valid = verify_csv_files()
    backup_valid = verify_backup()

    # 检查内容
    check_json_content()

    # 总体评估
    print("=" * 80)
    print("📊 总体评估")
    print("=" * 80)
    print()

    results = {
        "JSON配置文件": json_valid,
        "CONFIG配置文件": config_valid,
        "CSV文档文件": csv_valid,
        "备份文件": backup_valid,
    }

    for category, valid in results.items():
        status = "✅ 正常" if valid else "❌ 异常"
        print(f"{status:10} {category}")

    all_valid = all(results.values())

    print()
    if all_valid:
        print("=" * 80)
        print("✅ 所有配置文件核对通过")
        print("=" * 80)
        print()
        print("✅ 配置文件完整且有效")
        print("✅ 备份文件完整")
        print("✅ 可以开始本地测试")
    else:
        print("=" * 80)
        print("⚠️  配置文件存在问题")
        print("=" * 80)
        print()
        print("⚠️  部分配置文件缺失或损坏")
        print("⚠️  请检查并修复后继续")

    print()

if __name__ == '__main__':
    generate_verification_report()
