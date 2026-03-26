#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
本地文件对齐到Parse库脚本
功能：清理本地JSON和代码，确保与Parse库一致

作者：DG-IoT Team
日期：2026-03-25
原则：Parse库是唯一数据源，本地文件必须对齐

使用方法：
  python3 align_local_to_parse.py --check   # 检查不一致
  python3 align_local_to_parse.py --clean   # 清理本地冗余文件
  python3 align_local_to_parse.py --report  # 生成对齐报告
"""

import requests
import json
import os
import sys
from datetime import datetime
from typing import Dict, List, Set, Tuple

# ==================== 配置 ====================
BASE_URL = "http://127.0.0.1/iotapi"
LOGIN_URL = f"{BASE_URL}/login"

# 产品ID
PRODUCT_UAV = "6235befb62"
PRODUCT_FIXTURE = "bd49cc8272"
PRODUCT_STATION = "2de1b3e1b8"

# 本地JSON文件目录
LOCAL_JSON_DIRS = [
    "/root/gitee/dgiot/apps/dgiot_uav/priv/json",
    "/root/gitee/dgiot/apps/dgiot_uav/priv/csv"
]

# 本地Erlang代码文件
LOCAL_ERLANG_FILES = {
    "uav_commands": "/root/gitee/dgiot/apps/dgiot_uav/src/business/command/dgiot_uav_command_examples.erl",
    "fixture_commands": "/root/gitee/dgiot/apps/dgiot_uav/src/business/command/dgiot_uav_fixture_commands.erl",
    "plc_commands": "/root/gitee/dgiot/apps/dgiot_uav/src/business/command/dgiot_uav_plc_commands.erl"
}

# ==================== 工具函数 ====================

class ParseDBClient:
    """Parse Server API客户端"""
    
    def __init__(self):
        self.session_token = None
        self.headers = {}
        
    def login(self, username: str, password: str) -> bool:
        """登录获取sessionToken"""
        try:
            resp = requests.post(
                LOGIN_URL,
                headers={"Content-Type": "text/plain"},
                data=json.dumps({"username": username, "password": password})
            )
            
            if resp.status_code != 200:
                return False
                
            data = resp.json()
            self.session_token = data.get("sessionToken") or data.get("access_token")
            
            if not self.session_token:
                return False
                
            self.headers = {"sessiontoken": self.session_token}
            return True
            
        except Exception as e:
            print(f"❌ 登录失败: {e}")
            return False
    
    def get_product(self, product_id: str) -> Dict:
        """获取产品信息"""
        try:
            url = f"{BASE_URL}/classes/Product/{product_id}"
            resp = requests.get(url, headers=self.headers)
            
            if resp.status_code == 200:
                return resp.json()
            else:
                return {}
                
        except Exception as e:
            return {}

# ==================== 检查函数 ====================

def check_local_json_files() -> List[Dict]:
    """检查本地JSON文件"""
    
    issues = []
    
    # 检查指令相关的JSON文件
    command_files = [
        "fixture_commands.json",
        "uav_command_sets.json",
        "test_items_full.json",
        "test_items_summary.json",
        "1100.json",
        "1200.json",
        "1300.json",
        "1500.json",
        "1600.json",
        "1700.json"
    ]
    
    for json_dir in LOCAL_JSON_DIRS:
        for filename in command_files:
            filepath = os.path.join(json_dir, filename)
            
            if os.path.exists(filepath):
                issues.append({
                    'type': 'local_json_exists',
                    'file': filepath,
                    'message': f"本地JSON文件存在，应使用Parse库数据",
                    'recommendation': '删除或归档'
                })
    
    return issues

def check_parse_vs_local(client: ParseDBClient) -> List[Dict]:
    """检查Parse库与本地代码的不一致"""
    
    issues = []
    
    # 1. 检查治具指令
    parse_fixture = client.get_product(PRODUCT_FIXTURE)
    parse_fixture_codes = set()
    
    if parse_fixture:
        modbus = parse_fixture.get('content', {}).get('command_sets', {}).get('modbus', [])
        parse_fixture_codes = {c['code'] for c in modbus if 'code' in c}
    
    # Erlang代码定义的治具指令
    erlang_fixture_codes = set(range(1, 17))  # 根据dgiot_uav_fixture_commands.erl
    
    if erlang_fixture_codes != parse_fixture_codes:
        issues.append({
            'type': 'code_parse_mismatch',
            'product': '治具',
            'parse_codes': sorted(parse_fixture_codes),
            'erlang_codes': sorted(erlang_fixture_codes),
            'missing_in_parse': sorted(erlang_fixture_codes - parse_fixture_codes),
            'extra_in_parse': sorted(parse_fixture_codes - erlang_fixture_codes)
        })
    
    # 2. 检查工位指令
    parse_station = client.get_product(PRODUCT_STATION)
    parse_station_codes = set()
    
    if parse_station:
        station_commands = parse_station.get('content', {}).get('station_commands', {})
        for station_data in station_commands.values():
            commands = station_data.get('commands', [])
            parse_station_codes.update({c['code'] for c in commands if 'code' in c})
    
    # Erlang代码定义的PLC指令（每个工位不同）
    # 如果Parse库为空，这是主要问题
    if not parse_station_codes:
        issues.append({
            'type': 'parse_data_missing',
            'product': '工位',
            'message': 'Parse库工位产品content为空，需要从Erlang代码导入'
        })
    
    return issues

# ==================== 清理函数 ====================

def clean_local_json_files(backup_dir: str) -> None:
    """清理本地JSON文件（归档到备份目录）"""
    
    print("\n【清理本地JSON文件】")
    print("-" * 100)
    
    # 创建归档目录
    archive_dir = os.path.join(backup_dir, "archived_local_json")
    os.makedirs(archive_dir, exist_ok=True)
    
    # 要归档的文件
    files_to_archive = [
        "fixture_commands.json",
        "uav_command_sets.json",
        "InstructionSet.json"
    ]
    
    for json_dir in LOCAL_JSON_DIRS:
        for filename in files_to_archive:
            filepath = os.path.join(json_dir, filename)
            
            if os.path.exists(filepath):
                # 归档文件
                archive_path = os.path.join(archive_dir, f"{datetime.now().strftime('%Y%m%d')}_{filename}")
                
                import shutil
                shutil.move(filepath, archive_path)
                
                print(f"✅ 已归档: {filepath} -> {archive_path}")
    
    print(f"\n✅ 本地JSON文件清理完成，归档到: {archive_dir}")

# ==================== 报告生成 ====================

def generate_alignment_report(client: ParseDBClient, output_file: str) -> None:
    """生成对齐报告"""
    
    print("\n【生成对齐报告】")
    print("-" * 100)
    
    report = {
        "generated_at": datetime.now().isoformat(),
        "principle": "Parse库是唯一数据源，本地文件必须对齐",
        "parse_db_status": {},
        "local_file_status": {},
        "alignment_issues": [],
        "recommendations": []
    }
    
    # 1. Parse库状态
    for name, product_id in [("无人机", PRODUCT_UAV), ("治具", PRODUCT_FIXTURE), ("工位", PRODUCT_STATION)]:
        product = client.get_product(product_id)
        
        if product:
            content = product.get('content', {})
            
            if product_id == PRODUCT_UAV:
                remote_commands = content.get('remote_commands', {})
                total_cmds = sum(len(cmds) for cmds in remote_commands.values() if isinstance(cmds, list))
            elif product_id == PRODUCT_FIXTURE:
                modbus = content.get('command_sets', {}).get('modbus', [])
                total_cmds = len(modbus)
            elif product_id == PRODUCT_STATION:
                station_commands = content.get('station_commands', {})
                total_cmds = sum(len(s.get('commands', [])) for s in station_commands.values())
            else:
                total_cmds = 0
            
            report["parse_db_status"][name] = {
                "product_id": product_id,
                "command_count": total_cmds,
                "content_fields": list(content.keys())
            }
    
    # 2. 本地文件状态
    report["local_file_status"] = {
        "json_files": check_local_json_files(),
        "erlang_files": list(LOCAL_ERLANG_FILES.keys())
    }
    
    # 3. 对齐问题
    report["alignment_issues"] = check_parse_vs_local(client)
    
    # 4. 建议
    report["recommendations"] = [
        "保留Parse库JSON备份，定期备份到 /root/gitee/dgiot/backups/parse_db/",
        "删除本地冗余JSON配置文件，使用备份脚本生成的JSON作为参考",
        "Erlang代码定义应该与Parse库同步，建议使用脚本自动同步",
        "测试项和测试步骤只在Parse库中维护，本地不保留副本"
    ]
    
    # 保存报告
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(report, f, ensure_ascii=False, indent=2)
    
    print(f"✅ 对齐报告生成: {output_file}")
    
    # 打印摘要
    print("\n对齐摘要：")
    for name, status in report["parse_db_status"].items():
        print(f"  {name}: {status['command_count']}个指令")
    
    if report["alignment_issues"]:
        print(f"\n⚠️  发现 {len(report['alignment_issues'])} 个对齐问题：")
        for issue in report["alignment_issues"]:
            print(f"  - {issue['type']}: {issue.get('message', '')}")

# ==================== 主函数 ====================

def main():
    import argparse
    
    parser = argparse.ArgumentParser(description="本地文件对齐到Parse库")
    parser.add_argument('--check', action='store_true', help='检查不一致')
    parser.add_argument('--clean', action='store_true', help='清理本地冗余文件')
    parser.add_argument('--report', action='store_true', help='生成对齐报告')
    parser.add_argument('--backup-dir', default='/root/gitee/dgiot/backups', help='备份目录')
    
    args = parser.parse_args()
    
    # 如果没有参数，显示帮助
    if len(sys.argv) == 1:
        parser.print_help()
        return
    
    # 创建客户端并登录
    client = ParseDBClient()
    if not client.login("dgiot_dev", "dgiot_dev"):
        sys.exit(1)
    
    # 执行相应操作
    if args.check:
        issues = check_parse_vs_local(client)
        
        if issues:
            print("\n❌ 发现不一致：")
            for issue in issues:
                print(f"  {issue}")
        else:
            print("\n✅ 本地文件与Parse库一致")
    
    elif args.clean:
        clean_local_json_files(args.backup_dir)
    
    elif args.report:
        output_file = os.path.join(args.backup_dir, "alignment_report.json")
        generate_alignment_report(client, output_file)

if __name__ == "__main__":
    main()
