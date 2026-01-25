#!/usr/bin/env python3
"""
列出所有技能及其状态
"""

import os
import sys
import json
from datetime import datetime

def get_skill_info(skill_dir):
    """获取技能信息"""
    skill_path = os.path.join(skill_dir, "SKILL.md")
    
    if not os.path.exists(skill_path):
        return None
    
    with open(skill_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # 提取YAML frontmatter
    import re
    import yaml
    
    frontmatter_pattern = r'^---\s*\n(.*?)\n---\s*\n'
    match = re.search(frontmatter_pattern, content, re.DOTALL | re.MULTILINE)
    
    if not match:
        return None
    
    try:
        metadata = yaml.safe_load(match.group(1))
        
        # 获取技能状态
        status = "✅ 正常"
        issues = []
        
        # 检查必需字段
        required_fields = ["name", "description"]
        for field in required_fields:
            if field not in metadata:
                issues.append(f"缺少{field}")
                status = "⚠️ 需要维护"
        
        # 检查文件结构
        required_files = ["SKILL.md"]
        optional_files = ["README.md", "scripts/", "config/", "examples/"]
        
        files_present = []
        for file in required_files:
            if os.path.exists(os.path.join(skill_dir, file)):
                files_present.append(file)
        
        for file in optional_files:
            if os.path.exists(os.path.join(skill_dir, file.rstrip('/'))):
                files_present.append(file)
        
        return {
            "name": metadata.get("name", os.path.basename(skill_dir)),
            "description": metadata.get("description", ""),
            "version": metadata.get("version", "未知"),
            "author": metadata.get("author", "未知"),
            "created_date": metadata.get("created_date", "未知"),
            "category": metadata.get("category", "未知"),
            "tags": metadata.get("tags", []),
            "trigger_phrases_count": len(metadata.get("trigger_phrases", [])),
            "status": status,
            "issues": issues,
            "files": files_present,
            "skill_dir": skill_dir
        }
    except:
        return None

def list_all_skills(skills_dir):
    """列出所有技能"""
    print(f"📁 技能目录: {skills_dir}")
    
    if not os.path.exists(skills_dir):
        print(f"❌ 技能目录不存在: {skills_dir}")
        return []
    
    skill_folders = [f for f in os.listdir(skills_dir) 
                    if os.path.isdir(os.path.join(skills_dir, f))]
    
    print(f"📊 发现 {len(skill_folders)} 个技能目录")
    
    skills_info = []
    for skill_folder in skill_folders:
        skill_dir = os.path.join(skills_dir, skill_folder)
        info = get_skill_info(skill_dir)
        
        if info:
            skills_info.append(info)
        else:
            # 无效技能
            skills_info.append({
                "name": skill_folder,
                "description": "无效技能格式",
                "version": "未知",
                "author": "未知",
                "created_date": "未知",
                "category": "未知",
                "tags": [],
                "trigger_phrases_count": 0,
                "status": "❌ 无效",
                "issues": ["技能文件格式错误"],
                "files": [],
                "skill_dir": skill_dir
            })
    
    return skills_info

def print_skills_table(skills_info):
    """打印技能表格"""
    print("\n" + "=" * 80)
    print("📋 技能列表")
    print("=" * 80)
    
    # 表头
    print(f"{'序号':<4} {'技能名称':<20} {'状态':<10} {'版本':<10} {'类别':<15} {'触发短语':<10} {'描述'}")
    print("-" * 80)
    
    for i, skill in enumerate(skills_info, 1):
        # 截断描述
        desc = skill["description"]
        if len(desc) > 40:
            desc = desc[:37] + "..."
        
        print(f"{i:<4} {skill['name']:<20} {skill['status']:<10} {skill['version']:<10} "
              f"{skill['category']:<15} {skill['trigger_phrases_count']:<10} {desc}")
    
    print("-" * 80)
    
    # 统计
    total = len(skills_info)
    normal = sum(1 for s in skills_info if s["status"] == "✅ 正常")
    needs_maintenance = sum(1 for s in skills_info if s["status"] == "⚠️ 需要维护")
    invalid = sum(1 for s in skills_info if s["status"] == "❌ 无效")
    
    print(f"📊 统计: 共{total}个技能 | ✅ 正常: {normal}个 | ⚠️ 需要维护: {needs_maintenance}个 | ❌ 无效: {invalid}个")
    
    # 显示需要维护的技能
    if needs_maintenance > 0 or invalid > 0:
        print("\n🔧 需要关注的技能:")
        for skill in skills_info:
            if skill["status"] != "✅ 正常":
                print(f"   {skill['name']}: {skill['status']}")
                if skill["issues"]:
                    print(f"      问题: {', '.join(skill['issues'])}")

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description="列出Cline技能")
    parser.add_argument("--format", choices=["table", "json", "simple"], default="table", help="输出格式")
    parser.add_argument("--report", help="生成HTML报告文件")
    parser.add_argument("--verbose", action="store_true", help="详细输出")
    
    args = parser.parse_args()
    
    # 技能目录
    skills_dir = "/root/test/drone/drone_control_system/.cline/skills"
    
    # 获取技能信息
    skills_info = list_all_skills(skills_dir)
    
    # 按名称排序
    skills_info.sort(key=lambda x: x["name"])
    
    # 输出
    if args.format == "json":
        print(json.dumps(skills_info, ensure_ascii=False, indent=2))
    elif args.format == "simple":
        for skill in skills_info:
            print(f"{skill['name']}: {skill['description']}")
    else:
        print_skills_table(skills_info)
    
    # 生成报告
    if args.report:
        generate_html_report(skills_info, args.report)
        print(f"\n📄 HTML报告已生成: {args.report}")

def generate_html_report(skills_info, output_file):
    """生成HTML报告"""
    html = """<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Cline技能管理报告</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        h1 { color: #333; }
        table { border-collapse: collapse; width: 100%; margin: 20px 0; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
        .status-ok { color: green; }
        .status-warning { color: orange; }
        .status-error { color: red; }
        .stats { background-color: #f9f9f9; padding: 15px; border-radius: 5px; margin: 20px 0; }
    </style>
</head>
<body>
    <h1>📋 Cline技能管理报告</h1>
    <p>生成时间: """ + datetime.now().strftime("%Y-%m-%d %H:%M:%S") + """</p>
    
    <div class="stats">
        <h3>📊 统计信息</h3>
        <p>总技能数: """ + str(len(skills_info)) + """</p>
        <p>正常技能: """ + str(sum(1 for s in skills_info if s["status"] == "✅ 正常")) + """</p>
        <p>需要维护: """ + str(sum(1 for s in skills_info if s["status"] == "⚠️ 需要维护")) + """</p>
        <p>无效技能: """ + str(sum(1 for s in skills_info if s["status"] == "❌ 无效")) + """</p>
    </div>
    
    <h3>技能列表</h3>
    <table>
        <tr>
            <th>技能名称</th>
            <th>状态</th>
            <th>版本</th>
            <th>类别</th>
            <th>触发短语</th>
            <th>描述</th>
            <th>问题</th>
        </tr>
"""
    
    for skill in skills_info:
        status_class = ""
        if skill["status"] == "✅ 正常":
            status_class = "status-ok"
        elif skill["status"] == "⚠️ 需要维护":
            status_class = "status-warning"
        else:
            status_class = "status-error"
        
        html += f"""
        <tr>
            <td><strong>{skill['name']}</strong></td>
            <td class="{status_class}">{skill['status']}</td>
            <td>{skill['version']}</td>
            <td>{skill['category']}</td>
            <td>{skill['trigger_phrases_count']}</td>
            <td>{skill['description']}</td>
            <td>{', '.join(skill['issues']) if skill['issues'] else '无'}</td>
        </tr>
"""
    
    html += """
    </table>
    
    <h3>💡 建议</h3>
    <ul>
        <li>定期运行技能验证: <code>python3 scripts/validate_skill.py --all</code></li>
        <li>修复格式问题: <code>python3 scripts/fix_skill_format.py --all</code></li>
        <li>创建新技能: <code>python3 scripts/create_skill.py</code></li>
    </ul>
</body>
</html>
"""
    
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(html)

if __name__ == "__main__":
    main()