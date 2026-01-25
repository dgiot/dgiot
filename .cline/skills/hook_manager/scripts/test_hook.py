#!/usr/bin/env python3
"""
测试UserPromptSubmit Hook的主目标检查功能
"""

import json
import subprocess
import sys

def test_hook(prompt_text):
    """测试hook对特定prompt的响应"""
    
    # 构建输入JSON
    input_data = {
        "userPromptSubmit": {
            "prompt": prompt_text
        }
    }
    
    # 将JSON转换为字符串
    input_json = json.dumps(input_data)
    
    # 运行hook脚本
    hook_path = "/root/test/drone/drone_control_system/.clinerules/hooks/UserPromptSubmit"
    
    try:
        result = subprocess.run(
            [hook_path],
            input=input_json,
            text=True,
            capture_output=True,
            timeout=5
        )
        
        if result.returncode != 0:
            print(f"❌ Hook执行失败 (exit code: {result.returncode})")
            print(f"stderr: {result.stderr}")
            return None
        
        # 解析输出
        output = json.loads(result.stdout)
        return output
        
    except subprocess.TimeoutExpired:
        print("❌ Hook执行超时")
        return None
    except json.JSONDecodeError as e:
        print(f"❌ 输出JSON解析失败: {e}")
        print(f"原始输出: {result.stdout}")
        return None
    except Exception as e:
        print(f"❌ 执行失败: {e}")
        return None

def main():
    """主测试函数"""
    
    test_cases = [
        {
            "name": "高相关性任务",
            "prompt": "修改src/protocols/uav_protocol.py以支持业务层解析",
            "expected": "高相关性"
        },
        {
            "name": "中等相关性任务",
            "prompt": "修复协议解析器的bug",
            "expected": "中等相关性"
        },
        {
            "name": "低相关性任务",
            "prompt": "优化前端UI界面设计",
            "expected": "低相关性"
        },
        {
            "name": "无人机协议检测",
            "prompt": "解析EB90报文",
            "expected": "无人机协议"
        },
        {
            "name": "偏离检测",
            "prompt": "创建一个临时的测试脚本",
            "expected": "偏离警告"
        }
    ]
    
    print("🚀 开始测试UserPromptSubmit Hook的主目标检查功能")
    print("=" * 60)
    
    for i, test_case in enumerate(test_cases, 1):
        print(f"\n📋 测试用例 {i}: {test_case['name']}")
        print(f"   Prompt: {test_case['prompt']}")
        print(f"   期望: {test_case['expected']}")
        
        result = test_hook(test_case['prompt'])
        
        if result is None:
            print("   ❌ 测试失败")
            continue
            
        if result.get("cancel", False):
            print("   ⚠️ Hook建议取消任务")
        else:
            context_mod = result.get("contextModification", "")
            if context_mod:
                # 解码JSON字符串（context_mod已经是转义后的JSON字符串）
                try:
                    # 直接使用，因为jq -aRs .已经转义了
                    context_text = context_mod
                    print("   ✅ Hook返回上下文修改")
                    
                    # 检查是否包含期望的内容
                    if test_case['expected'] == "高相关性" and "高相关性任务" in context_text:
                        print("   ✅ 检测到高相关性检查")
                    elif test_case['expected'] == "中等相关性" and "中等相关性任务" in context_text:
                        print("   ✅ 检测到中等相关性检查")
                    elif test_case['expected'] == "低相关性" and "低相关性任务" in context_text:
                        print("   ✅ 检测到低相关性检查")
                    elif test_case['expected'] == "无人机协议" and "无人机协议检测结果" in context_text:
                        print("   ✅ 检测到无人机协议检查")
                    elif test_case['expected'] == "偏离警告" and "偏离检测警告" in context_text:
                        print("   ✅ 检测到偏离警告")
                    else:
                        print(f"   ⚠️ 未检测到期望内容，实际内容片段: {context_text[:100]}...")
                except:
                    print(f"   ⚠️ 上下文解析失败: {context_mod[:100]}...")
            else:
                print("   ℹ️ Hook未返回上下文修改")
    
    print("\n" + "=" * 60)
    print("🎯 测试完成")
    
    # 演示hook的实际效果
    print("\n📊 演示实际hook效果:")
    demo_prompts = [
        "帮我修改uav_protocol.py以支持业务层解析",
        "我想优化前端界面",
        "解析这个EB90报文: EB90 01 02 03",
        "创建一个临时的调试脚本"
    ]
    
    for demo_prompt in demo_prompts:
        print(f"\n💬 用户请求: {demo_prompt}")
        result = test_hook(demo_prompt)
        if result and result.get("contextModification"):
            context = result["contextModification"]
            # 提取第一行作为摘要
            lines = context.split('\n')
            for line in lines:
                if line.strip() and not line.startswith('#'):
                    print(f"   📝 Hook建议: {line[:80]}...")
                    break

if __name__ == "__main__":
    main()