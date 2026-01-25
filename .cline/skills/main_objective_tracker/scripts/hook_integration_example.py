#!/usr/bin/env python3
"""
Hook集成示例
演示如何在任务执行前后集成主目标检查
"""

import json
import time
from datetime import datetime
from typing import Dict, Any, Callable, Optional

class TaskHook:
    """任务Hook管理器"""
    
    def __init__(self, skill_path: str = "skills/main_objective_tracker.json"):
        """初始化Hook管理器"""
        self.skill_path = skill_path
        self.skill_data = self._load_skill()
        self.task_start_time = None
        self.current_task = None
        
    def _load_skill(self) -> Dict[str, Any]:
        """加载技能配置"""
        try:
            with open(self.skill_path, 'r', encoding='utf-8') as f:
                return json.load(f)
        except:
            return {}
    
    def pre_task_check(self, task_description: str) -> Dict[str, Any]:
        """任务执行前检查"""
        print(f"\n🔍 任务前检查: {task_description}")
        
        current_obj = self.skill_data.get("current_main_objective", {})
        obj_title = current_obj.get("title", "未知目标")
        
        # 简单相关性检查
        obj_keywords = ["uav_protocol", "业务层", "协议解析", "报文摘要"]
        relevance = any(keyword in task_description for keyword in obj_keywords)
        
        check_result = {
            "task": task_description,
            "current_objective": obj_title,
            "is_relevant": relevance,
            "recommendation": "",
            "should_proceed": True
        }
        
        if relevance:
            check_result["recommendation"] = "✅ 任务与主目标相关，可以执行"
        else:
            check_result["recommendation"] = "⚠️ 任务与主目标相关性较低"
            check_result["should_proceed"] = False
            
            # 检查是否是必须执行的任务
            must_do_keywords = ["用户要求", "必须", "紧急", "bug", "错误"]
            if any(keyword in task_description for keyword in must_do_keywords):
                check_result["recommendation"] = "⚠️ 任务相关性低，但可能是用户要求的必要任务"
                check_result["should_proceed"] = True
        
        print(f"   当前主目标: {obj_title}")
        print(f"   相关性: {'是' if relevance else '否'}")
        print(f"   建议: {check_result['recommendation']}")
        
        return check_result
    
    def mid_task_check(self, task_description: str, elapsed_minutes: int) -> Dict[str, Any]:
        """任务执行中检查"""
        print(f"\n⏱️ 任务中检查: 已执行{elapsed_minutes}分钟")
        
        # 检查是否偏离
        deviation_threshold = 10  # 10分钟
        is_deviated = elapsed_minutes > deviation_threshold
        
        check_result = {
            "task": task_description,
            "elapsed_minutes": elapsed_minutes,
            "is_deviated": is_deviated,
            "deviation_reason": "",
            "action": ""
        }
        
        if is_deviated:
            check_result["deviation_reason"] = f"任务执行时间超过{deviation_threshold}分钟"
            check_result["action"] = "考虑暂停并重新评估任务优先级"
            print(f"   🚨 检测到偏离: {check_result['deviation_reason']}")
            print(f"   🔄 建议: {check_result['action']}")
        else:
            print(f"   ✅ 任务执行正常")
        
        return check_result
    
    def post_task_check(self, task_description: str, success: bool) -> Dict[str, Any]:
        """任务执行后检查"""
        print(f"\n📊 任务后检查: {'成功' if success else '失败'}")
        
        # 检查是否应该回到主目标
        current_obj = self.skill_data.get("current_main_objective", {})
        obj_title = current_obj.get("title", "未知目标")
        
        check_result = {
            "task": task_description,
            "completed_successfully": success,
            "current_objective": obj_title,
            "should_return_to_main": True,
            "next_action": f"回到主目标: {obj_title}"
        }
        
        print(f"   任务完成状态: {'成功' if success else '失败'}")
        print(f"   当前主目标: {obj_title}")
        print(f"   下一步: {check_result['next_action']}")
        
        return check_result
    
    def execute_with_hooks(self, task_description: str, task_func: Callable, *args, **kwargs) -> Any:
        """使用Hook执行任务"""
        print("=" * 60)
        print(f"🚀 开始执行任务: {task_description}")
        print("=" * 60)
        
        # 1. 任务前检查
        pre_check = self.pre_task_check(task_description)
        if not pre_check["should_proceed"]:
            print("❌ 任务被阻止执行")
            return None
        
        # 2. 记录开始时间
        self.task_start_time = time.time()
        self.current_task = task_description
        
        try:
            # 3. 执行任务（模拟）
            print(f"\n🛠️ 执行任务中...")
            result = task_func(*args, **kwargs) if task_func else None
            
            # 4. 任务中检查（模拟）
            elapsed_seconds = time.time() - self.task_start_time
            elapsed_minutes = int(elapsed_seconds / 60)
            
            if elapsed_minutes > 0:
                self.mid_task_check(task_description, elapsed_minutes)
            
            # 5. 任务后检查
            self.post_task_check(task_description, success=True)
            
            print(f"\n✅ 任务执行完成")
            return result
            
        except Exception as e:
            print(f"\n❌ 任务执行失败: {e}")
            self.post_task_check(task_description, success=False)
            return None
        
        finally:
            print("=" * 60)
            self.task_start_time = None
            self.current_task = None


# 示例任务函数
def example_task_high_priority():
    """高优先级任务示例"""
    print("执行高优先级任务: 修改uav_protocol.py")
    time.sleep(1)  # 模拟任务执行
    return "任务完成"

def example_task_low_priority():
    """低优先级任务示例"""
    print("执行低优先级任务: 优化UI界面")
    time.sleep(3)  # 模拟长时间任务
    return "任务完成"

def example_user_requested_task():
    """用户要求的任务示例"""
    print("执行用户要求的任务: 清理目录")
    time.sleep(2)
    return "任务完成"


# 主函数
def main():
    """主函数"""
    print("🚁 Hook集成示例 - 主目标跟踪")
    print("版本: 1.0.0")
    print()
    
    hook = TaskHook()
    
    # 示例1: 高优先级任务
    print("\n" + "=" * 60)
    print("示例1: 高优先级任务")
    print("=" * 60)
    hook.execute_with_hooks(
        "修改src/protocols/uav_protocol.py以支持业务层解析",
        example_task_high_priority
    )
    
    # 示例2: 低优先级任务
    print("\n" + "=" * 60)
    print("示例2: 低优先级任务")
    print("=" * 60)
    hook.execute_with_hooks(
        "优化前端UI界面设计",
        example_task_low_priority
    )
    
    # 示例3: 用户要求的任务
    print("\n" + "=" * 60)
    print("示例3: 用户要求的任务")
    print("=" * 60)
    hook.execute_with_hooks(
        "用户要求清理src/backend目录",
        example_user_requested_task
    )
    
    # 演示偏离检测
    print("\n" + "=" * 60)
    print("偏离检测演示")
    print("=" * 60)
    
    # 模拟长时间执行低优先级任务
    print("\n模拟长时间执行低优先级任务...")
    time.sleep(12)  # 模拟12分钟执行时间
    
    mid_check = hook.mid_task_check("长时间低优先级任务", 12)
    if mid_check["is_deviated"]:
        print(f"\n🚨 检测到严重偏离!")
        print(f"原因: {mid_check['deviation_reason']}")
        print(f"建议: {mid_check['action']}")
        print("立即停止当前任务，回到主目标!")
    
    print("\n✅ Hook集成示例完成")


if __name__ == "__main__":
    main()