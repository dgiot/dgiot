#!/usr/bin/env python3
"""
主目标跟踪技能使用示例
演示如何判断任务是否偏离主目标，以及如何管理todo list
"""

import json
import os
from datetime import datetime
from typing import Dict, List, Any, Optional

class MainObjectiveTracker:
    """主目标跟踪器"""
    
    def __init__(self, skill_path: str = "skills/main_objective_tracker.json"):
        """初始化跟踪器"""
        self.skill_path = skill_path
        self.skill_data = self._load_skill()
        self.todo_list = {
            "核心任务": [],
            "优化任务": [],
            "文档任务": [],
            "低优先级任务": []
        }
        
    def _load_skill(self) -> Dict[str, Any]:
        """加载技能配置"""
        try:
            with open(self.skill_path, 'r', encoding='utf-8') as f:
                return json.load(f)
        except FileNotFoundError:
            print(f"技能文件不存在: {self.skill_path}")
            return {}
        except json.JSONDecodeError as e:
            print(f"JSON解析错误: {e}")
            return {}
    
    def get_current_objective(self) -> Dict[str, Any]:
        """获取当前主目标"""
        return self.skill_data.get("current_main_objective", {})
    
    def analyze_task(self, task_description: str) -> Dict[str, Any]:
        """分析任务类型和优先级"""
        current_obj = self.get_current_objective()
        obj_title = current_obj.get("title", "未知目标")
        obj_desc = current_obj.get("description", "")
        
        analysis = {
            "task": task_description,
            "current_main_objective": obj_title,
            "relevance_score": 0,
            "priority": "UNKNOWN",
            "recommendation": "",
            "rule_applied": None
        }
        
        # 检查任务是否与主目标直接相关
        obj_keywords = ["uav_protocol", "业务层", "协议解析", "报文摘要", "无人机协议"]
        task_lower = task_description.lower()
        
        # 计算相关性分数
        relevance_keywords = 0
        for keyword in obj_keywords:
            if keyword in task_lower:
                relevance_keywords += 1
        
        # 应用规则判断
        rules = self.skill_data.get("main_objective_rules", [])
        for rule in rules:
            rule_name = rule.get("name", "")
            rule_examples = rule.get("examples", [])
            
            # 检查任务是否匹配规则示例
            for example in rule_examples:
                if example in task_description:
                    analysis["rule_applied"] = rule.get("rule_id")
                    analysis["priority"] = rule.get("priority", "UNKNOWN")
                    break
            
            if analysis["rule_applied"]:
                break
        
        # 计算最终相关性分数
        analysis["relevance_score"] = min(relevance_keywords * 25, 100)
        
        # 生成建议
        if analysis["relevance_score"] >= 75:
            analysis["recommendation"] = "✅ 高度相关，应立即执行"
        elif analysis["relevance_score"] >= 50:
            analysis["recommendation"] = "⚠️ 中等相关，可考虑执行"
        elif analysis["relevance_score"] >= 25:
            analysis["recommendation"] = "📝 低相关，建议添加到todo list"
        else:
            analysis["recommendation"] = "❌ 不相关，应避免执行"
            
            # 检查是否偏离主目标
            deviation_indicators = self.skill_data.get("deviation_detection", {}).get("indicators", [])
            for indicator in deviation_indicators:
                if indicator in task_description:
                    analysis["recommendation"] = f"🚨 检测到目标偏离: {indicator}"
                    break
        
        return analysis
    
    def add_to_todo_list(self, task: str, category: str = "低优先级任务") -> bool:
        """添加任务到todo list"""
        if category not in self.todo_list:
            print(f"无效的分类: {category}")
            return False
        
        max_items = self.skill_data.get("todo_list_management", {}).get("max_items_per_category", 10)
        if len(self.todo_list[category]) >= max_items:
            print(f"分类 {category} 已达到最大项目数")
            return False
        
        todo_item = {
            "task": task,
            "added_at": datetime.now().isoformat(),
            "completed": False
        }
        
        self.todo_list[category].append(todo_item)
        return True
    
    def check_deviation(self, current_task: str, task_duration_minutes: int = 0) -> Dict[str, Any]:
        """检查是否偏离主目标"""
        analysis = self.analyze_task(current_task)
        current_obj = self.get_current_objective()
        
        deviation_result = {
            "is_deviated": False,
            "deviation_reason": "",
            "correction_action": "",
            "relevance_score": analysis["relevance_score"]
        }
        
        # 检查偏离条件
        if analysis["relevance_score"] < 25:
            deviation_result["is_deviated"] = True
            deviation_result["deviation_reason"] = "任务与主目标相关性太低"
        
        # 检查任务时长
        deviation_threshold = self.skill_data.get("hooks_integration", {}).get("deviation_threshold", "")
        if "10分钟" in deviation_threshold and task_duration_minutes > 10:
            if analysis["relevance_score"] < 50:
                deviation_result["is_deviated"] = True
                deviation_result["deviation_reason"] = f"在低相关性任务上花费了{task_duration_minutes}分钟"
        
        # 提供纠正建议
        if deviation_result["is_deviated"]:
            correction_actions = self.skill_data.get("deviation_detection", {}).get("correction_actions", [])
            if correction_actions:
                deviation_result["correction_action"] = correction_actions[0]
        
        return deviation_result
    
    def print_analysis_report(self, task_description: str):
        """打印分析报告"""
        print("=" * 60)
        print("主目标跟踪分析报告")
        print("=" * 60)
        
        # 当前主目标
        current_obj = self.get_current_objective()
        print(f"\n📋 当前主目标: {current_obj.get('title')}")
        print(f"📝 描述: {current_obj.get('description')}")
        print(f"🎯 状态: {current_obj.get('status')}")
        print(f"⭐ 优先级: {current_obj.get('priority')}")
        
        # 任务分析
        print(f"\n🔍 分析任务: {task_description}")
        analysis = self.analyze_task(task_description)
        
        print(f"📊 相关性分数: {analysis['relevance_score']}/100")
        print(f"🏷️ 应用规则: {analysis['rule_applied'] or '无'}")
        print(f"⚡ 优先级: {analysis['priority']}")
        print(f"💡 建议: {analysis['recommendation']}")
        
        # 偏离检查
        deviation = self.check_deviation(task_description)
        if deviation["is_deviated"]:
            print(f"\n🚨 偏离检测: {deviation['deviation_reason']}")
            print(f"🔄 纠正动作: {deviation['correction_action']}")
        else:
            print(f"\n✅ 任务与主目标保持一致")
        
        print("=" * 60)
    
    def print_todo_list(self):
        """打印todo list"""
        print("=" * 60)
        print("待办事项列表 (Todo List)")
        print("=" * 60)
        
        total_tasks = 0
        completed_tasks = 0
        
        for category, tasks in self.todo_list.items():
            print(f"\n📁 {category}:")
            if not tasks:
                print("  (空)")
                continue
                
            for i, task in enumerate(tasks, 1):
                status = "✅" if task.get("completed", False) else "◻️"
                task_desc = task.get("task", "未知任务")
                added_at = task.get("added_at", "未知时间")
                
                print(f"  {status} {i}. {task_desc}")
                print(f"     添加时间: {added_at}")
                
                total_tasks += 1
                if task.get("completed", False):
                    completed_tasks += 1
        
        print(f"\n📈 统计: 共{total_tasks}个任务，已完成{completed_tasks}个")
        print("=" * 60)


# 使用示例
def main():
    """主函数"""
    tracker = MainObjectiveTracker()
    
    print("🚁 无人机协议解析系统 - 主目标跟踪器")
    print("版本: 1.0.0")
    print()
    
    # 示例任务分析
    test_tasks = [
        "修改src/protocols/uav_protocol.py以支持业务层解析",
        "清理src/backend目录中的重复文件",
        "创建一个临时的测试脚本",
        "优化前端UI界面",
        "编写API使用文档",
        "修复协议解析器的bug"
    ]
    
    print("📋 示例任务分析:")
    print("-" * 40)
    
    for i, task in enumerate(test_tasks, 1):
        print(f"\n{i}. 任务: {task}")
        analysis = tracker.analyze_task(task)
        print(f"   相关性: {analysis['relevance_score']}/100 | 建议: {analysis['recommendation']}")
        
        # 如果是低优先级任务，添加到todo list
        if analysis['relevance_score'] < 50 and "临时" not in task:
            tracker.add_to_todo_list(task, "低优先级任务")
    
    print("\n" + "=" * 60)
    
    # 详细分析一个任务
    sample_task = "创建一个临时的测试脚本"
    tracker.print_analysis_report(sample_task)
    
    # 显示todo list
    tracker.print_todo_list()
    
    # 偏离检测示例
    print("\n🚨 偏离检测示例:")
    print("-" * 40)
    
    deviation_check = tracker.check_deviation("优化前端UI界面", task_duration_minutes=15)
    if deviation_check["is_deviated"]:
        print(f"检测到偏离: {deviation_check['deviation_reason']}")
        print(f"建议: {deviation_check['correction_action']}")
    else:
        print("任务与主目标保持一致")
    
    print("\n✅ 主目标跟踪器演示完成")


if __name__ == "__main__":
    main()