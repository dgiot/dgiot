#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
备份和清理与Parse库无关的配置文件
"""

import os
import shutil
import json
from datetime import datetime

# 基础路径
BASE_DIR = '/root/gitee/dgiot/apps/dgiot_uav'
BACKUP_DIR = '/root/gitee/dgiot/apps/dgiot_uav/priv/config_backup'
CSV_DIR = '/root/gitee/dgiot/apps/dgiot_uav/priv/csv'
JSON_DIR = '/root/gitee/dgiot_uav/priv/json'
CONFIG_DIR = '/root/gitee/dgiot_uav/priv/config'

# 与Parse库相关的配置文件（不清理）
PARSE_RELATED_FILES = {
    'json/uav_command_sets.json',
    'json/InstructionSet.json',
    'json/test_items_full.json',
    'json/test_items_summary.json',
    'json/test_judge_rules.json',
    'csv/*.md',  # 核对报告
}

# 需要清理的文件分类
FILES_TO_BACKUP = {
    'json': [
        'fixture_commands.json',
        'fixture_devices.json',
        'fixture_devices_ip_port.json',
        'mes.json',
        'Stations.json',
        'StatusCodes.json',
        '1100.json',
        '1200.json',
        '1300.json',
        '1500.json',
        '1600.json',
        '1700.json',
    ],
    'csv': [
        '*.py',  # 处理脚本
        '*.txt',
        '*.xlsx',
    ],
    'scripts': [
        '*.log',  # 日志文件
        '*.md',   # 部分文档
    ]
}

# 不删除的文件
KEEP_FILES = {
    'json/uav_protocol.json',
    'json/UAVConfig.json',
    'config/station_bindings.config',
}

def create_backup_dir():
    """创建备份目录"""
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    backup_path = os.path.join(BACKUP_DIR, timestamp)
    os.makedirs(backup_path, exist_ok=True)
    print(f"✅ 创建备份目录: {backup_path}")
    return backup_path

def analyze_config_files():
    """分析配置文件"""
    print("=" * 80)
    print("📋 分析配置文件")
    print("=" * 80)
    print()

    # 分析JSON文件
    print("📁 JSON文件分析:")
    print("-" * 80)
    json_files = []
    for filename in os.listdir(JSON_DIR):
        if filename.endswith('.json'):
            filepath = os.path.join(JSON_DIR, filename)
            size = os.path.getsize(filepath)
            json_files.append((filename, size))

    for filename, size in sorted(json_files):
        status = "✅ 保留" if f'json/{filename}' in KEEP_FILES or f'json/{filename}' in PARSE_RELATED_FILES else "📦 备份"
        print(f"  {status:10} {filename:30} {size:>8} bytes")

    print()

    # 分析CSV文件
    print("📁 CSV文件分析:")
    print("-" * 80)
    csv_files = []
    for filename in os.listdir(CSV_DIR):
        if not filename.endswith('.py'):
            filepath = os.path.join(CSV_DIR, filename)
            size = os.path.getsize(filepath)
            csv_files.append((filename, size))

    for filename, size in sorted(csv_files):
        status = "✅ 保留" if filename.endswith('.md') else "📦 备份"
        print(f"  {status:10} {filename:30} {size:>8} bytes")

    print()

    # 分析CONFIG文件
    print("📁 CONFIG文件分析:")
    print("-" * 80)
    for filename in os.listdir(CONFIG_DIR):
        filepath = os.path.join(CONFIG_DIR, filename)
        size = os.path.getsize(filepath)
        status = "✅ 保留" if f'config/{filename}' in KEEP_FILES else "📦 备份"
        print(f"  {status:10} {filename:30} {size:>8} bytes")

    print()

def backup_files(backup_path):
    """备份文件"""
    print("=" * 80)
    print("📦 备份配置文件")
    print("=" * 80)
    print()

    backup_count = 0

    # 备份JSON文件
    json_backup_dir = os.path.join(backup_path, 'json')
    os.makedirs(json_backup_dir, exist_ok=True)

    for filename in FILES_TO_BACKUP['json']:
        src = os.path.join(JSON_DIR, filename)
        if os.path.exists(src):
            dst = os.path.join(json_backup_dir, filename)
            shutil.copy2(src, dst)
            print(f"✅ 备份: json/{filename}")
            backup_count += 1

    # 备份CSV文件
    csv_backup_dir = os.path.join(backup_path, 'csv')
    os.makedirs(csv_backup_dir, exist_ok=True)

    for filename in os.listdir(CSV_DIR):
        if not filename.endswith('.py') and not filename.endswith('.md'):
            src = os.path.join(CSV_DIR, filename)
            dst = os.path.join(csv_backup_dir, filename)
            shutil.copy2(src, dst)
            print(f"✅ 备份: csv/{filename}")
            backup_count += 1

    # 备份日志文件
    log_backup_dir = os.path.join(backup_path, 'logs')
    os.makedirs(log_backup_dir, exist_ok=True)

    scripts_dir = os.path.join(BASE_DIR, 'priv/scripts')
    for root, dirs, files in os.walk(scripts_dir):
        for filename in files:
            if filename.endswith('.log'):
                src = os.path.join(root, filename)
                rel_path = os.path.relpath(src, scripts_dir)
                dst_dir = os.path.join(log_backup_dir, os.path.dirname(rel_path))
                os.makedirs(dst_dir, exist_ok=True)
                dst = os.path.join(dst_dir, filename)
                shutil.copy2(src, dst)
                print(f"✅ 备份: scripts/{rel_path}")
                backup_count += 1

    print()
    print(f"📊 备份文件总数: {backup_count}")
    print(f"📁 备份目录: {backup_path}")
    print()

def cleanup_files():
    """清理已备份的文件"""
    print("=" * 80)
    print("🧹 清理配置文件")
    print("=" * 80)
    print()

    cleanup_count = 0

    # 清理JSON文件
    for filename in FILES_TO_BACKUP['json']:
        src = os.path.join(JSON_DIR, filename)
        if os.path.exists(src):
            os.remove(src)
            print(f"🗑️  删除: json/{filename}")
            cleanup_count += 1

    # 清理CSV文件
    for filename in os.listdir(CSV_DIR):
        if not filename.endswith('.py') and not filename.endswith('.md'):
            src = os.path.join(CSV_DIR, filename)
            os.remove(src)
            print(f"🗑️  删除: csv/{filename}")
            cleanup_count += 1

    # 清理日志文件
    scripts_dir = os.path.join(BASE_DIR, 'priv/scripts')
    for root, dirs, files in os.walk(scripts_dir):
        for filename in files:
            if filename.endswith('.log'):
                src = os.path.join(root, filename)
                os.remove(src)
                rel_path = os.path.relpath(src, BASE_DIR)
                print(f"🗑️  删除: {rel_path}")
                cleanup_count += 1

    print()
    print(f"📊 清理文件总数: {cleanup_count}")
    print()

def generate_cleanup_report(backup_path, backup_count, cleanup_count):
    """生成清理报告"""
    report_file = os.path.join(backup_path, 'cleanup_report.md')

    with open(report_file, 'w', encoding='utf-8') as f:
        f.write("# 配置文件清理报告\n\n")
        f.write(f"## 清理时间\n{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
        f.write(f"## 备份目录\n`{backup_path}`\n\n")
        f.write(f"## 处理统计\n")
        f.write(f"- 备份文件数: {backup_count}\n")
        f.write(f"- 清理文件数: {cleanup_count}\n\n")
        f.write(f"## 保留的配置文件\n")
        f.write(f"- `json/uav_protocol.json` - 无人机协议配置\n")
        f.write(f"- `json/UAVConfig.json` - 无人机配置\n")
        f.write(f"- `config/station_bindings.config` - 工位绑定配置\n")
        f.write(f"- `json/uav_command_sets.json` - Parse相关指令集\n")
        f.write(f"- `json/InstructionSet.json` - Parse相关指令集\n")
        f.write(f"- `json/test_items_full.json` - Parse相关测试项\n")
        f.write(f"- `json/test_items_summary.json` - Parse相关测试摘要\n")
        f.write(f"- `json/test_judge_rules.json` - Parse相关判据规则\n\n")
        f.write(f"## 清理的文件类型\n")
        f.write(f"- JSON配置文件（非Parse相关）\n")
        f.write(f"- CSV临时文件\n")
        f.write(f"- 日志文件（*.log）\n\n")
        f.write(f"## 说明\n")
        f.write(f"本次清理仅删除与Parse库无关的临时配置文件和日志文件，\n")
        f.write(f"所有重要配置文件已备份，可随时恢复。\n")

    print(f"✅ 清理报告已生成: {report_file}")

def main():
    """主函数"""
    print()
    print("=" * 80)
    print("🔧 配置文件备份和清理工具")
    print("=" * 80)
    print()

    # 分析文件
    analyze_config_files()

    # 创建备份目录
    backup_path = create_backup_dir()

    # 备份文件
    backup_files(backup_path)
    backup_count = sum([
        len(FILES_TO_BACKUP['json']),
        len([f for f in os.listdir(CSV_DIR) if not f.endswith('.py') and not f.endswith('.md')]),
    ])
    # 统计日志文件数量
    scripts_dir = os.path.join(BASE_DIR, 'priv/scripts')
    log_count = 0
    for root, dirs, files in os.walk(scripts_dir):
        log_count += len([f for f in files if f.endswith('.log')])
    backup_count += log_count

    # 清理文件
    cleanup_files()
    cleanup_count = backup_count  # 备份的文件都被清理

    # 生成报告
    generate_cleanup_report(backup_path, backup_count, cleanup_count)

    print("=" * 80)
    print("✅ 配置文件备份和清理完成")
    print("=" * 80)
    print()
    print(f"📁 备份目录: {backup_path}")
    print(f"📄 清理报告: {os.path.join(backup_path, 'cleanup_report.md')}")
    print()

if __name__ == '__main__':
    main()
