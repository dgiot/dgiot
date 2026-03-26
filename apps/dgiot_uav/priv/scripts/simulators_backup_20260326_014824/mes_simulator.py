#!/usr/bin/env python3
"""
MES模拟服务器
模拟制造执行系统API，接收产线测试结果并返回响应

配置:
- 监听端口: 8010
- 协议: HTTP REST API
- 数据格式: JSON

功能:
1. 接收设备测试结果
2. 接收产线状态更新
3. 返回MES确认响应
4. 支持数据持久化（可选）
"""

import json
import logging
import sys
import os
from datetime import datetime
from http.server import HTTPServer, BaseHTTPRequestHandler
from typing import Dict, List
from dataclasses import dataclass, asdict
import threading

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)

# 配置常量
MES_SERVER_PORT = 801  # MES服务器端口（非80端口）
MES_SERVER_HOST = '0.0.0.0'  # 监听所有网卡（允许通过nginx代理访问）

@dataclass
class TestResult:
    """测试结果数据模型"""
    device_id: str
    station_id: str
    test_time: str
    test_result: str  # passed/failed
    test_data: Dict
    mes_line: str = ""

@dataclass
class LineStatus:
    """产线状态数据模型"""
    line_id: str
    status: str  # running/stopped/error
    current_station: str
    devices_completed: int
    devices_total: int


class MESDatabase:
    """MES数据库模拟（内存存储）"""

    def __init__(self):
        self.test_results: List[TestResult] = []
        self.line_status: Dict[str, LineStatus] = {}
        self.lock = threading.Lock()

    def add_test_result(self, result: TestResult):
        """添加测试结果"""
        with self.lock:
            self.test_results.append(result)
            logger.info(f"[MES_DB] 添加测试结果: 设备={result.device_id}, 结果={result.test_result}")

    def get_test_results(self, device_id: str = None, limit: int = 100) -> List[Dict]:
        """查询测试结果"""
        with self.lock:
            if device_id:
                results = [r for r in self.test_results if r.device_id == device_id]
            else:
                results = self.test_results[-limit:]
            return [asdict(r) for r in results]

    def update_line_status(self, status: LineStatus):
        """更新产线状态"""
        with self.lock:
            self.line_status[status.line_id] = status
            logger.info(f"[MES_DB] 更新产线状态: 产线={status.line_id}, 状态={status.status}")

    def get_line_status(self, line_id: str) -> Dict:
        """查询产线状态"""
        with self.lock:
            status = self.line_status.get(line_id)
            return asdict(status) if status else {}

    def get_statistics(self) -> Dict:
        """获取统计数据"""
        with self.lock:
            total = len(self.test_results)
            passed = sum(1 for r in self.test_results if r.test_result == 'passed')
            failed = sum(1 for r in self.test_results if r.test_result == 'failed')
            return {
                'total_tests': total,
                'passed': passed,
                'failed': failed,
                'pass_rate': f"{(passed/total*100):.2f}%" if total > 0 else "0%"
            }


class MESRequestHandler(BaseHTTPRequestHandler):
    """MES HTTP请求处理器"""

    # 全局数据库实例
    database = MESDatabase()

    def _send_json_response(self, data: Dict, status_code: int = 200):
        """发送JSON响应"""
        self.send_response(status_code)
        self.send_header('Content-Type', 'application/json; charset=utf-8')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
        response = json.dumps(data, ensure_ascii=False, indent=2)
        self.wfile.write(response.encode('utf-8'))

    def _send_error_response(self, message: str, status_code: int = 400):
        """发送错误响应"""
        self._send_json_response({
            'success': False,
            'error': message,
            'timestamp': datetime.now().isoformat()
        }, status_code)

    def _parse_request_body(self) -> Dict:
        """解析请求体"""
        content_length = int(self.headers.get('Content-Length', 0))
        if content_length == 0:
            return {}

        try:
            body = self.rfile.read(content_length)
            return json.loads(body.decode('utf-8'))
        except json.JSONDecodeError as e:
            logger.error(f"JSON解析失败: {e}")
            return {}

    def do_POST(self):
        """处理POST请求"""
        try:
            # 解析路径
            if self.path == '/api/test/result':
                self.handle_test_result()
            elif self.path == '/api/line/status':
                self.handle_line_status()
            elif self.path == '/lezao/jymes/api/equip/proExec':
                self.handle_equip_proexec()  # 兼容Erlang MES API
            else:
                self._send_error_response(f"未知的API路径: {self.path}", 404)

        except Exception as e:
            logger.error(f"处理POST请求异常: {e}")
            self._send_error_response(f"服务器内部错误: {str(e)}", 500)

    def do_GET(self):
        """处理GET请求"""
        try:
            # 解析路径
            if self.path == '/api/test/results':
                self.handle_get_test_results()
            elif self.path == '/api/line/status':
                self.handle_get_line_status()
            elif self.path == '/api/statistics':
                self.handle_get_statistics()
            elif self.path == '/health':
                self.handle_health_check()
            else:
                self._send_error_response(f"未知的API路径: {self.path}", 404)

        except Exception as e:
            logger.error(f"处理GET请求异常: {e}")
            self._send_error_response(f"服务器内部错误: {str(e)}", 500)

    def handle_test_result(self):
        """处理测试结果上报"""
        logger.info(f"[MES_API] 接收到测试结果上报")

        # 解析请求体
        data = self._parse_request_body()
        if not data:
            self._send_error_response("请求体为空")
            return

        # 验证必填字段
        required_fields = ['device_id', 'station_id', 'test_result']
        for field in required_fields:
            if field not in data:
                self._send_error_response(f"缺少必填字段: {field}")
                return

        # 创建测试结果
        result = TestResult(
            device_id=data['device_id'],
            station_id=data['station_id'],
            test_time=data.get('test_time', datetime.now().isoformat()),
            test_result=data['test_result'],
            test_data=data.get('test_data', {}),
            mes_line=data.get('mes_line', '')
        )

        # 保存到数据库
        self.database.add_test_result(result)

        # 返回成功响应
        response = {
            'success': True,
            'message': '测试结果已接收',
            'result_id': len(self.database.test_results),
            'timestamp': datetime.now().isoformat()
        }
        self._send_json_response(response)
        logger.info(f"[MES_API] 测试结果处理完成: {data['device_id']} - {data['test_result']}")

    def handle_line_status(self):
        """处理产线状态更新"""
        logger.info(f"[MES_API] 接收到产线状态更新")

        # 解析请求体
        data = self._parse_request_body()
        if not data:
            self._send_error_response("请求体为空")
            return

        # 验证必填字段
        if 'line_id' not in data or 'status' not in data:
            self._send_error_response("缺少必填字段: line_id, status")
            return

        # 创建产线状态
        status = LineStatus(
            line_id=data['line_id'],
            status=data['status'],
            current_station=data.get('current_station', ''),
            devices_completed=data.get('devices_completed', 0),
            devices_total=data.get('devices_total', 0)
        )

        # 更新数据库
        self.database.update_line_status(status)

        # 返回成功响应
        response = {
            'success': True,
            'message': '产线状态已更新',
            'timestamp': datetime.now().isoformat()
        }
        self._send_json_response(response)
        logger.info(f"[MES_API] 产线状态更新完成: {data['line_id']} - {data['status']}")

    def handle_equip_proexec(self):
        """处理设备执行上报（兼容Erlang MES API）"""
        logger.info(f"[MES_API] 接收到设备执行上报（Erlang MES API）")

        # 解析请求体
        data = self._parse_request_body()
        if not data:
            self._send_error_response("请求体为空")
            return

        # 创建测试结果
        result = TestResult(
            device_id=data.get('device_id', 'unknown'),
            station_id=data.get('station_id', 'unknown'),
            test_time=data.get('test_time', datetime.now().isoformat()),
            test_result=data.get('result', data.get('status', 'unknown')),
            test_data=data,
            mes_line=data.get('mes_line', '')
        )

        # 保存到数据库
        self.database.add_test_result(result)

        # 返回MES格式响应（兼容Erlang代码期望）
        response = {
            'code': 200,
            'message': 'success',
            'data': {
                'result_id': len(self.database.test_results),
                'timestamp': datetime.now().isoformat()
            }
        }
        self._send_json_response(response)
        logger.info(f"[MES_API] 设备执行上报完成: {result.device_id} - {result.test_result}")

    def handle_get_test_results(self):
        """查询测试结果"""
        logger.info(f"[MES_API] 查询测试结果")

        # 获取查询参数
        from urllib.parse import urlparse, parse_qs
        query = parse_qs(urlparse(self.path).query)
        device_id = query.get('device_id', [None])[0]
        limit = int(query.get('limit', [100])[0])

        # 查询数据库
        results = self.database.get_test_results(device_id, limit)

        response = {
            'success': True,
            'count': len(results),
            'results': results,
            'timestamp': datetime.now().isoformat()
        }
        self._send_json_response(response)

    def handle_get_line_status(self):
        """查询产线状态"""
        logger.info(f"[MES_API] 查询产线状态")

        # 获取查询参数
        from urllib.parse import urlparse, parse_qs
        query = parse_qs(urlparse(self.path).query)
        line_id = query.get('line_id', [None])[0]

        if not line_id:
            self._send_error_response("缺少参数: line_id")
            return

        # 查询数据库
        status = self.database.get_line_status(line_id)

        response = {
            'success': True,
            'line_status': status,
            'timestamp': datetime.now().isoformat()
        }
        self._send_json_response(response)

    def handle_get_statistics(self):
        """获取统计数据"""
        logger.info(f"[MES_API] 查询统计数据")

        # 查询数据库
        stats = self.database.get_statistics()

        response = {
            'success': True,
            'statistics': stats,
            'timestamp': datetime.now().isoformat()
        }
        self._send_json_response(response)

    def handle_health_check(self):
        """健康检查"""
        response = {
            'success': True,
            'service': 'MES模拟服务器',
            'status': 'running',
            'timestamp': datetime.now().isoformat()
        }
        self._send_json_response(response)

    def log_message(self, format, *args):
        """重写日志方法，使用自定义日志"""
        logger.info(f"[HTTP] {format % args}")


class MESServer:
    """MES服务器主类"""

    def __init__(self, host: str = MES_SERVER_HOST, port: int = MES_SERVER_PORT):
        self.host = host
        self.port = port
        self.server = None
        self.server_thread = None
        self.running = False

    def start(self):
        """启动MES服务器"""
        logger.info(f"启动MES模拟服务器: {self.host}:{self.port}")

        self.server = HTTPServer((self.host, self.port), MESRequestHandler)
        self.running = True

        # 在独立线程中运行服务器
        self.server_thread = threading.Thread(target=self._run_server, daemon=True)
        self.server_thread.start()

        logger.info(f"MES服务器已启动，PID: {os.getpid()}")
        logger.info(f"API端点:")
        logger.info(f"  POST http://{self.host}:{self.port}/api/test/result - 上报测试结果")
        logger.info(f"  POST http://{self.host}:{self.port}/api/line/status - 更新产线状态")
        logger.info(f"  GET  http://{self.host}:{self.port}/api/test/results - 查询测试结果")
        logger.info(f"  GET  http://{self.host}:{self.port}/api/line/status?line_id=X - 查询产线状态")
        logger.info(f"  GET  http://{self.host}:{self.port}/api/statistics - 查询统计数据")
        logger.info(f"  GET  http://{self.host}:{self.port}/health - 健康检查")

    def _run_server(self):
        """运行服务器（在独立线程中）"""
        try:
            self.server.serve_forever()
        except KeyboardInterrupt:
            logger.info("MES服务器收到中断信号")
        except Exception as e:
            logger.error(f"MES服务器异常: {e}")
        finally:
            self.stop()

    def stop(self):
        """停止MES服务器"""
        if self.server:
            logger.info("停止MES服务器")
            self.running = False
            self.server.shutdown()
            self.server.server_close()

    def wait(self):
        """等待服务器运行"""
        try:
            while self.running:
                pass
        except KeyboardInterrupt:
            self.stop()


def main():
    """主函数"""
    import argparse

    parser = argparse.ArgumentParser(description="MES模拟服务器")
    parser.add_argument("--host", default=MES_SERVER_HOST, help="监听地址")
    parser.add_argument("--port", type=int, default=MES_SERVER_PORT, help="监听端口")

    args = parser.parse_args()

    # 创建并启动服务器
    server = MESServer(args.host, args.port)
    server.start()

    # 等待中断信号
    server.wait()


if __name__ == "__main__":
    main()
