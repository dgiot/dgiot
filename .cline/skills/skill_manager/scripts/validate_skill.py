#!/usr/bin/env python3
"""
技能格式验证脚本
验证Cline技能文件的格式是否符合标准
"""

import os
import sys
import yaml
import re
import json
from pathlib import Path

def load_validation_rules():
    """加载验证规则"""
    rules = {
        "required_fields": ["name", "description"],
        "optional_fields": ["version", "author", "created_date", "category", "tags", "trigger_phrases"],
        "format_rules": {
            "yaml_frontmatter": {
                "required": True,
                "pattern": r'^---\s*\n(.*?)\n---\s*\n'
            },
            "name": {
                "pattern": r'^[a-z0-9_]+$',
                "max_length": 50
            },
            "description": {
                "min_length": 10,
                "max_length": 200
            },
            "trigger_phrases": {
                "min_items": 1,
                "max_items": 20
            }
        }
    }
    return rules

def validate_skill_file(skill_path):
    """验证单个技能文件"""
    print(f"🔍 验证技能: {skill_path}")
    
    if not os.path.exists(skill_path):
        return {"valid": False, "errors": ["文件不存在"], "warnings": []}
    
    with open(skill_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    rules = load_validation_rules()
    errors = []
    warnings = []
    metadata = {}
    
    # 检查YAML frontmatter
    frontmatter_pattern = rules["format_rules"]["yaml_frontmatter"]["pattern"]
    match = re.search(frontmatter_pattern, content, re.DOTALL | re.MULTILINE)
    
    if not match:
        errors.append("缺少YAML frontmatter (--- 分隔符)")
        return {"valid": False, "errors": errors, "warnings": warnings}
    
    frontmatter_text = match.group(1)
    
    try:
        metadata = yaml.safe_load(frontmatter_text)
        print(f"   ✅ 找到YAML frontmatter")
    except yaml.YAMLError as e:
        errors.append(f"YAML解析错误: {e}")
        return {"valid": False, "errors": errors, "warnings": warnings}
    
    # 检查必需字段
    for field in rules["required_fields"]:
        if field not in metadata:
            errors.append(f"缺少必需字段: {field}")
        else:
            print(f"   📋 {field}: {metadata[field]}")
    
    # 检查可选字段
    for field in rules["optional_fields"]:
        if field in metadata:
            print(f"   📋 {field}: {metadata[field]}")
    
    # 检查名称格式
    if "name" in metadata:
        name = metadata["name"]
        if not re.match(rules["format_rules"]["name"]["pattern"], name):
            errors.append(f"技能名称格式不正确: {name} (只能包含小写字母、数字和下划线)")
        if len(name) > rules["format_rules"]["name"]["max_length"]:
            warnings.append(f"技能名称过长: {len(name)}字符 (最大{rules['format_rules']['name']['max_length']})")
    
    # 检查描述格式
    if "description" in metadata:
        desc = metadata["description"]
        if len(desc) < rules["format_rules"]["description"]["min_length"]:
            warnings.append(f"技能描述过短: {len(desc)}字符 (最小{rules['format_rules']['description']['min_length']})")
        if len(desc) > rules["format_rules"]["description"]["max_length"]:
            warnings.append(f"技能描述过长: {len(desc)}字符 (最大{rules['format_rules']['description']['max_length']})")
    
    # 检查触发短语
    if "trigger_phrases" in metadata:
        phrases = metadata["trigger_phrases"]
        if not isinstance(phrases, list):
            errors.append("trigger_phrases必须是列表")
        else:
            if len(phrases) < rules["format_rules"]["trigger_phrases"]["min_items"]:
                warnings.append(f"触发短语过少: {len(phrases)}个 (最小{rules['format_rules']['trigger_phrases']['min_items']})")
            if len(phrases) > rules["format_rules"]["trigger_phrases"]["max_items"]:
                warnings.append(f"触发短语过多: {len(phrases)}个 (最大{rules['format_rules']['trigger_phrases']['max_items']})")
            print(f"   📋 trigger_phrases: {len(phrases)}个短语")
    
    # 计算评分
    score = 100
    if errors:
        score -= len(errors) * 20
    if warnings:
        score -= len(warnings) * 5
    score = max(0, min(100, score))
    
    return {
        "valid": len(errors) == 0,
        "errors": errors,
        "warnings": warnings,
        "metadata": metadata,
        "score": score
    }

def validate_all_skills(skills_dir):
    """验证所有技能"""
    print(f"📁 扫描技能目录: {skills_dir}")
    
    if not os.path.exists(skills_dir):
        print(f"❌ 技能目录不存在: {skills_dir}")
        return []
    
    skill_folders = [f for f in os.listdir(skills_dir) 
                    if os.path.isdir(os.path.join(skills_dir, f))]
    
    print(f"📊 发现 {len(skill_folders)} 个技能目录")
    
    results = []
    for skill_folder in skill_folders:
        skill_path = os.path.join(skills_dir, skill_folder, "SKILL.md")
        
        if os.path.exists(skill_path):
            result = validate_skill_file(skill_path)
            result["skill_name"] = skill_folder
            result["skill_path"] = skill_path
            results.append(result)
        else:
            print(f"   ❌ {skill_folder}: 缺少SKILL.md文件")
            results.append({
                "skill_name": skill_folder,
                "skill_path": skill_path,
                "valid": False,
                "errors": ["缺少SKILL.md文件"],
                "warnings": [],
                "score": 0
            })
    
    return results

def print_validation_report(results):
    """打印验证报告"""
    print("\n" + "=" * 60)
    print("📊 技能验证报告")
    print("=" * 60)
    
    valid_count = sum(1 for r in results if r["valid"])
    total_count = len(results)
    
    print(f"✅ 有效技能: {valid_count}/{total_count}")
    print(f"❌ 无效技能: {total_count - valid_count}/{total_count}")
    
    for result in results:
        print(f"\n📋 {result['skill_name']}:")
        if result["valid"]:
            print(f"   ✅ 格式正确 (评分: {result['score']}/100)")
        else:
            print(f"   ❌ 格式错误 (评分: {result['score']}/100)")
        
        if result["errors"]:
            for error in result["errors"]:
                print(f"      ❌ {error}")
        
        if result["warnings"]:
            for warning in result["warnings"]:
                print(f"      ⚠️ {warning}")
    
    # 总体统计
    print("\n" + "=" * 60)
    print("📈 总体统计:")
    
    all_errors = sum(len(r["errors"]) for r in results)
    all_warnings = sum(len(r["warnings"]) for r in results)
    avg_score = sum(r["score"] for r in results) / total_count if total_count > 0 else 0
    
    print(f"   总错误数: {all_errors}")
    print(f"   总警告数: {all_warnings}")
    print(f"   平均评分: {avg_score:.1f}/100")
    
    # 建议
    if all_errors > 0:
        print("\n💡 建议:")
        print("   1. 运行修复脚本: python3 scripts/fix_skill_format.py --all")
        print("   2. 检查技能格式规范")
        print("   3. 参考示例技能文件")

def main():
    """主函数"""
    import argparse
    
    parser = argparse.ArgumentParser(description="验证Cline技能格式")
    parser.add_argument("--skill", help="验证特定技能名称")
    parser.add_argument("--all", action="store_true", help="验证所有技能")
    parser.add_argument("--output", choices=["text", "json"], default="text", help="输出格式")
    parser.add_argument("--verbose", action="store_true", help="详细输出")
    
    args = parser.parse_args()
    
    # 技能目录
    skills_dir = "/root/test/drone/drone_control_system/.cline/skills"
    
    if args.skill:
        # 验证特定技能
        skill_path = os.path.join(skills_dir, args.skill, "SKILL.md")
        result = validate_skill_file(skill_path)
        result["skill_name"] = args.skill
        
        if args.output == "json":
            print(json.dumps(result, ensure_ascii=False, indent=2))
        else:
            print(f"\n🔍 验证结果: {args.skill}")
            if result["valid"]:
                print(f"   ✅ 格式正确 (评分: {result['score']}/100)")
            else:
                print(f"   ❌ 格式错误 (评分: {result['score']}/100)")
            
            if result["errors"]:
                print("   错误:")
                for error in result["errors"]:
                    print(f"      ❌ {error}")
            
            if result["warnings"]:
                print("   警告:")
                for warning in result["warnings"]:
                    print(f"      ⚠️ {warning}")
    
    elif args.all or not args.skill:
        # 验证所有技能
        results = validate_all_skills(skills_dir)
        
        if args.output == "json":
            print(json.dumps(results, ensure_ascii=False, indent=2))
        else:
            print_validation_report(results)
    
    else:
        parser.print_help()

if __name__ == "__main__":
    main()