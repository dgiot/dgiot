#!/usr/bin/env python3
"""
工位测试场景基类
定义所有工位测试场景的通用接口和基础功能
"""

import logging
import time
from abc import ABC, abstractmethod
from typing import Dict, List, Optional


class BaseStationScenario(ABC):
    """工位测试场景基类"""

    def __init__(self, station_id: int, station_config: Dict, dgiot_config: Dict):
        """
        初始化工位测试场景

        Args:
            station_id: 工位ID
            station_config: 工位配置
            dgiot_config: DG-IoT配置
        """
        self.station_id = station_id
        self.station_config = station_config
        self.dgiot_config = dgiot_config
        self.logger = logging.getLogger(f"Station{station_id}")
        self.running = False
        self.errors = []

    @abstractmethod
    def run(self, scenario_type: str = "normal") -> bool:
        """
        运行测试场景

        Args:
            scenario_type: 场景类型 (normal, quick, stress)

        Returns:
            测试是否成功
        """
        pass

    def get_errors(self) -> List[str]:
        """获取错误列表"""
        return self.errors

    def add_error(self, error: str):
        """添加错误信息"""
        self.errors.append(error)
        self.logger.error(error)

    def is_running(self) -> bool:
        """检查是否在运行"""
        return self.running

    def _connect_to_dgiot(self) -> bool:
        """
        连接到DG-IoT服务器

        Returns:
            是否连接成功
        """
        try:
            # 这里实现实际的连接逻辑
            # 例如：TCP连接、MQTT连接等
            self.logger.info(f"连接到DG-IoT服务器: {self.dgiot_config['host']}:{self.dgiot_config['tcp_port']}")
            # TODO: 实现实际连接
            return True
        except Exception as e:
            self.add_error(f"连接DG-IoT失败: {e}")
            return False

    def _register_devices(self) -> bool:
        """
        注册工位设备

        Returns:
            是否注册成功
        """
        try:
            devices = self.station_config.get('devices', [])
            self.logger.info(f"注册工位设备: {', '.join(devices)}")
            # TODO: 实现设备注册
            return True
        except Exception as e:
            self.add_error(f"设备注册失败: {e}")
            return False

    def _execute_test_step(self, step_no: int, step_name: str,
                          step_func, timeout: float = 30.0) -> Dict:
        """
        执行单个测试步骤

        Args:
            step_no: 步骤编号
            step_name: 步骤名称
            step_func: 步骤执行函数
            timeout: 超时时间(秒)

        Returns:
            步骤结果: {"status": "completed/failed", "duration": float, "message": str}
        """
        self.logger.info(f"执行步骤{step_no}: {step_name}")
        start_time = time.time()

        try:
            # 执行步骤函数
            step_func()
            duration = time.time() - start_time
            self.logger.info(f"步骤{step_no}完成: {step_name} (耗时: {duration:.2f}秒)")
            return {
                "status": "completed",
                "duration": duration,
                "message": "",
            }
        except TimeoutError:
            duration = time.time() - start_time
            self.add_error(f"步骤{step_no}超时: {step_name}")
            self.logger.error(f"步骤{step_no}超时: {step_name} (耗时: {duration:.2f}秒)")
            return {
                "status": "failed",
                "duration": duration,
                "message": "执行超时",
            }
        except Exception as e:
            duration = time.time() - start_time
            self.add_error(f"步骤{step_no}异常: {step_name} - {e}")
            self.logger.error(f"步骤{step_no}异常: {step_name} - {e}")
            return {
                "status": "failed",
                "duration": duration,
                "message": str(e),
            }

    def _wait_for_condition(self, condition_func, timeout: float = 30.0,
                          check_interval: float = 0.5) -> bool:
        """
        等待条件满足

        Args:
            condition_func: 条件检查函数，返回True表示满足
            timeout: 超时时间(秒)
            check_interval: 检查间隔(秒)

        Returns:
            是否在超时前满足条件
        """
        start_time = time.time()
        while time.time() - start_time < timeout:
            try:
                if condition_func():
                    return True
            except Exception as e:
                self.logger.warning(f"条件检查异常: {e}")

            time.sleep(check_interval)

        self.logger.warning(f"等待条件超时 (超时: {timeout}秒)")
        return False
