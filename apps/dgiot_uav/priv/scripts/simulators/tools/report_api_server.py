#!/usr/bin/env python3
"""
测试报告生成API服务
提供HTTP接口调用报告生成脚本
"""

from flask import Flask, request, jsonify
import os
import sys
import subprocess
import json
from datetime import datetime

# 添加脚本目录到路径
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, SCRIPT_DIR)

app = Flask(__name__)

# 配置
REPORT_GENERATOR_SCRIPT = os.path.join(SCRIPT_DIR, 'generate_uav_report.py')

@app.route('/reports/generate', methods=['POST', 'GET'])
def generate_report():
    """生成测试报告API"""
    try:
        # 获取参数
        if request.method == 'POST':
            data = request.get_json()
            device_id = data.get('device_id')
            session_token = data.get('session_token')
            generate_pdf = data.get('pdf', False)
        else:
            device_id = request.args.get('device_id')
            session_token = request.args.get('session_token')
            generate_pdf = request.args.get('pdf', 'false').lower() == 'true'

        if not device_id:
            return jsonify({
                'success': False,
                'error': '缺少device_id参数'
            }), 400

        # 构建命令
        cmd = ['python3', REPORT_GENERATOR_SCRIPT, '--device-id', device_id]
        if session_token:
            cmd.extend(['--session-token', session_token])
        if generate_pdf:
            cmd.append('--pdf')

        # 执行命令
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=30
        )

        # 解析输出
        if result.returncode == 0:
            output = result.stdout
            try:
                # 尝试解析最后一行的JSON输出
                lines = output.strip().split('\n')
                json_line = lines[-1]
                report_result = json.loads(json_line)

                return jsonify({
                    'success': True,
                    'data': report_result
                })
            except json.JSONDecodeError:
                # 如果无法解析JSON,返回原始输出
                return jsonify({
                    'success': True,
                    'message': '报告生成成功',
                    'output': output
                })
        else:
            return jsonify({
                'success': False,
                'error': f'报告生成失败: {result.stderr}'
            }), 500

    except subprocess.TimeoutExpired:
        return jsonify({
            'success': False,
            'error': '报告生成超时'
        }), 504
    except Exception as e:
        return jsonify({
            'success': False,
            'error': str(e)
        }), 500

@app.route('/health', methods=['GET'])
def health():
    """健康检查"""
    return jsonify({
        'status': 'ok',
        'service': 'report-generator',
        'timestamp': datetime.now().isoformat()
    })

if __name__ == '__main__':
    print("启动测试报告生成API服务...")
    print(f"脚本路径: {REPORT_GENERATOR_SCRIPT}")
    app.run(host='127.0.0.1', port=5555, debug=False)
