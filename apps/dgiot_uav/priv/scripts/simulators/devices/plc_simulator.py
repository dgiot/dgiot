#!/usr/bin/env python3
"""
Modbus TCP Server for DGIOT UAV PLC Simulator
Simulates multiple PLC workstations with five-step validation state machine.
Supports Modbus TCP protocol (port 502) and dynamic register responses.
(调试增强版 - 修复 validate 方法缺失)
(虚拟工位告警处理版 - 新增告警、心跳、手动/自动模式)

虚拟工位分配：
1. 192.168.100.20 - 磁航向工位，专门负责告警读取和状态监控（与虚拟告警检测工位共用IP）
2. 192.168.100.40 - 其他工位，负责心跳、状态和测试流程（包括虚拟心跳检测工位）

告警地址映射（磁航向PLC）：
- D1730.0 → D5000.0: 有急停按下，请复位后再运行机台！
- D1730.1 → D5035.0: 旋转伺服_1驱动器报警，请复位回零后在进行操作
- D1730.2 → D5035.1: 旋转伺服_1定位模块出错，请复位回零后在进行操作
- D1730.3 → D5035.2: 翻转步进_2驱动器报警，请复位回零后在进行操作
- D1730.4 → D5035.3: 翻转步进_2定位模块出错，请复位回零后在进行操作
- D1730.5 → D5035.0: 旋转伺服_1驱动器报警，请复位回零后在进行操作
- D1730.6 → D5035.1: 旋转伺服_1定位模块出错，请复位回零后在进行操作
- D1730.7 → D5035.2: 翻转步进_2驱动器报警，请复位回零后在进行操作
- D1730.8 → D5035.3: 翻转步进_2定位模块出错，请复位回零后在进行操作
"""

import logging
import random
import time
import threading
import socket
from datetime import datetime, timedelta
from pymodbus.server import StartTcpServer
from pymodbus.datastore import ModbusSequentialDataBlock
from pymodbus.datastore import ModbusSlaveContext, ModbusServerContext
from pymodbus.transaction import ModbusSocketFramer
from pymodbus.exceptions import ModbusException
from pymodbus.pdu import ModbusExceptions
from pymodbus.pdu import ExceptionResponse

# Configure logging with detailed format
logging.basicConfig(
    level=logging.INFO,  # 生产环境使用 INFO 级别，减少日志量
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
logger = logging.getLogger(__name__)
# pymodbus 内部日志级别
logging.getLogger('pymodbus').setLevel(logging.WARNING)

# 虚拟工位配置 - 基于用户提供的完整告警配置
VIRTUAL_WORKSTATIONS = {
    # ==================== 192.168.100.20 - 磁航向工位（告警专用）====================
    "磁航向PLC": {
        "ip": "192.168.100.20",
        "start_address": 1700,
        "end_address": 1799,
        "description": "磁航向PLC (D1700-D1799)",
        "type": "alarm_monitor",  # 告警监控类型
        "mode_register": 1720,    # D1720: 1自动中/2手动中
        "heartbeat_register": 1749,  # D1749: 心跳
        "alarm_registers": [
            {"address": 1730, "bits": 9, "description": "磁航向告警寄存器"}  # D1730.0-D1730.8
        ],
        # 告警映射：D1730.x → D5000.0-5035.3（根据用户提供的表格）
        "alarm_mappings": [
            {"bit": 0, "code": "D5000.0", "message": "有急停按下，请复位后再运行机台！", "level": 1},
            {"bit": 1, "code": "D5035.0", "message": "旋转伺服_1驱动器报警，请复位回零后在进行操作", "level": 2},
            {"bit": 2, "code": "D5035.1", "message": "旋转伺服_1定位模块出错，请复位回零后在进行操作", "level": 2},
            {"bit": 3, "code": "D5035.2", "message": "翻转步进_2驱动器报警，请复位回零后在进行操作", "level": 2},
            {"bit": 4, "code": "D5035.3", "message": "翻转步进_2定位模块出错，请复位回零后在进行操作", "level": 2},
            {"bit": 5, "code": "D5035.0", "message": "旋转伺服_1驱动器报警，请复位回零后在进行操作", "level": 2},
            {"bit": 6, "code": "D5035.1", "message": "旋转伺服_1定位模块出错，请复位回零后在进行操作", "level": 2},
            {"bit": 7, "code": "D5035.2", "message": "翻转步进_2驱动器报警，请复位回零后在进行操作", "level": 2},
            {"bit": 8, "code": "D5035.3", "message": "翻转步进_2定位模块出错，请复位回零后在进行操作", "level": 2}
        ]
    },
    
    # ==================== 192.168.100.40 - 测试线PLC（综合工位）====================
    "测试线PLC": {
        "ip": "192.168.100.40",
        "start_address": 1100,
        "end_address": 1199,
        "description": "测试线PLC (D1100-D1199)",
        "type": "comprehensive",  # 综合类型
        "mode_register": 1120,    # D1120: 1自动中/2手动中
        "heartbeat_register": 1149,  # D1149: 心跳
        "alarm_registers": [
            {"address": 1130, "bits": 16, "description": "测试线告警寄存器1 (D1130.0-D1130.15)"},
            {"address": 1131, "bits": 16, "description": "测试线告警寄存器2 (D1131.0-D1131.15)"},
            {"address": 1132, "bits": 16, "description": "测试线告警寄存器3 (D1132.0-D1132.15)"},
            {"address": 1133, "bits": 16, "description": "测试线告警寄存器4 (D1133.0-D1133.15)"},
            {"address": 1134, "bits": 16, "description": "测试线告警寄存器5 (D1134.0-D1134.15)"}
        ],
        # 告警映射（测试线PLC - 基于用户提供的详细表格）
        "alarm_mappings": [
            # D1130 告警 (16位)
            {"address": 1130, "bit": 0, "code": "D5000.0", "message": "有急停按下，请复位后再运行机台！", "level": 1},
            {"address": 1130, "bit": 1, "code": "D5000.1", "message": "安全门打开,请关闭安全门!", "level": 1},
            {"address": 1130, "bit": 2, "code": "D5000.2", "message": "Y轴伺服驱动报警,请复位后再进行操作", "level": 2},
            {"address": 1130, "bit": 3, "code": "D5000.3", "message": "Z轴伺服驱动报警,请复位后再进行操作", "level": 2},
            {"address": 1130, "bit": 4, "code": "D5000.4", "message": "1#电动缸伺服驱动报警,请复位后再进行操作", "level": 2},
            {"address": 1130, "bit": 5, "code": "D5000.5", "message": "2#电动缸伺服驱动报警,请复位后再进行操作", "level": 2},
            {"address": 1130, "bit": 6, "code": "D5000.6", "message": "3#电动缸伺服驱动报警,请复位后再进行操作", "level": 2},
            {"address": 1130, "bit": 7, "code": "D5000.7", "message": "4#电动缸伺服驱动报警,请复位后再进行操作", "level": 2},
            {"address": 1130, "bit": 8, "code": "D5001.0", "message": "中继模块检测异常,请联系供应商", "level": 3},
            {"address": 1130, "bit": 9, "code": "D5001.1", "message": "温度模块检测异常,请联系供应商", "level": 3},
            {"address": 1130, "bit": 10, "code": "D5001.2", "message": "扫码枪通讯异常,请检查扫码枪连接", "level": 3},
            {"address": 1130, "bit": 11, "code": "D5001.3", "message": "扫码枪检测异常,请检查扫码枪连接", "level": 3},
            {"address": 1130, "bit": 12, "code": "D5001.4", "message": "产品取放异常,请检查产品是否放置正确", "level": 3},
            {"address": 1130, "bit": 13, "code": "D5001.5", "message": "1#温箱通讯异常,请检查1#温箱设备连接", "level": 3},
            {"address": 1130, "bit": 14, "code": "D5001.6", "message": "2#温箱通讯异常,请检查2#温箱设备连接", "level": 3},
            {"address": 1130, "bit": 15, "code": "D5001.7", "message": "陀螺仪通讯异常,请检查陀螺仪设备连接", "level": 3},
            
            # D1131 告警 (16位)
            {"address": 1131, "bit": 0, "code": "D5002.0", "message": "三轴飞行转台通讯异常,请检查三轴飞行转台连接", "level": 3},
            {"address": 1131, "bit": 1, "code": "D5002.1", "message": "磁感应传感器通讯异常,请检查磁感应传感器连接", "level": 3},
            {"address": 1131, "bit": 2, "code": "D5002.2", "message": "磁场干扰,请检查测试环境", "level": 3},
            {"address": 1131, "bit": 3, "code": "D5002.3", "message": "磁偏置超限,请检查测试环境", "level": 3},
            {"address": 1131, "bit": 4, "code": "D5002.4", "message": "1#噪声工位通讯异常,请检查1#噪声工位设备连接", "level": 3},
            {"address": 1131, "bit": 5, "code": "D5002.5", "message": "2#噪声工位通讯异常,请检查2#噪声工位设备连接", "level": 3},
            {"address": 1131, "bit": 6, "code": "D5002.6", "message": "推力测试工位通讯异常,请检查推力测试工位设备连接", "level": 3},
            {"address": 1131, "bit": 7, "code": "D5002.7", "message": "动力参数异常,请检查无人机配置", "level": 3},
            
            # D1132 告警 (16位)
            {"address": 1132, "bit": 0, "code": "D5003.0", "message": "桨叶磨损异常,请更换桨叶", "level": 3},
            {"address": 1132, "bit": 1, "code": "D5003.1", "message": "桨叶动平衡异常,请重新平衡桨叶", "level": 3},
            {"address": 1132, "bit": 2, "code": "D5003.2", "message": "电机温度过高,请检查电机散热", "level": 2},
            {"address": 1132, "bit": 3, "code": "D5003.3", "message": "电调温度过高,请检查电调散热", "level": 2},
            {"address": 1132, "bit": 4, "code": "D5003.4", "message": "电机转速异常,请检查电机状态", "level": 2},
            {"address": 1132, "bit": 5, "code": "D5003.5", "message": "电调电流过大,请检查负载", "level": 2},
            {"address": 1132, "bit": 6, "code": "D5003.6", "message": "电池电压过低,请更换电池", "level": 2},
            {"address": 1132, "bit": 7, "code": "D5003.7", "message": "电池温度过高,请检查电池状态", "level": 2},
            
            # D1133 告警 (16位) - 保留位，用于扩展
            {"address": 1133, "bit": 0, "code": "D5004.0", "message": "RFID读写器通讯异常,请检查RFID连接", "level": 3},
            {"address": 1133, "bit": 1, "code": "D5004.1", "message": "相机模块通讯异常,请检查相机连接", "level": 3},
            {"address": 1133, "bit": 2, "code": "D5004.2", "message": "视觉检测异常,请检查产品位置", "level": 3},
            {"address": 1133, "bit": 3, "code": "D5004.3", "message": "尺寸测量异常,请检查测量机构", "level": 3},
            
            # D1134 告警 (16位) - 保留位，用于扩展
            {"address": 1134, "bit": 0, "code": "D5005.0", "message": "压力传感器异常,请检查传感器连接", "level": 3},
            {"address": 1134, "bit": 1, "code": "D5005.1", "message": "位移传感器异常,请检查传感器连接", "level": 3},
            {"address": 1134, "bit": 2, "code": "D5005.2", "message": "力传感器异常,请检查传感器连接", "level": 3},
            {"address": 1134, "bit": 3, "code": "D5005.3", "message": "扭矩传感器异常,请检查传感器连接", "level": 3}
        ]
    },
    
    # ==================== 192.168.100.40 - 机器人1 ====================
    "机器人1": {
        "ip": "192.168.100.40",
        "start_address": 1500,
        "end_address": 1599,
        "description": "机器人1 (D1500-D1599)",
        "type": "robot",  # 机器人类型
        "mode_register": 1520,    # D1520: 1自动中/2手动中
        "heartbeat_register": 1549,  # D1549: 心跳
        "alarm_registers": [
            {"address": 1530, "bits": 16, "description": "机器人1告警寄存器 (D1530.0-D1530.15)"}
        ],
        # 机器人1告警映射
        "alarm_mappings": [
            {"address": 1530, "bit": 0, "code": "D5100.0", "message": "机器人1伺服驱动器报警,请复位后再进行操作", "level": 2},
            {"address": 1530, "bit": 1, "code": "D5100.1", "message": "机器人1碰撞检测报警,请检查路径", "level": 2},
            {"address": 1530, "bit": 2, "code": "D5100.2", "message": "机器人1超限报警,请检查工作范围", "level": 2},
            {"address": 1530, "bit": 3, "code": "D5100.3", "message": "机器人1通讯异常,请检查通讯线路", "level": 3},
            {"address": 1530, "bit": 4, "code": "D5100.4", "message": "机器人1真空异常,请检查吸盘", "level": 3},
            {"address": 1530, "bit": 5, "code": "D5100.5", "message": "机器人1抓手异常,请检查夹爪", "level": 3},
            {"address": 1530, "bit": 6, "code": "D5100.6", "message": "机器人1定位偏差过大,请重新标定", "level": 3}
        ]
    },
    
    # ==================== 192.168.100.40 - 机器人2 ====================
    "机器人2": {
        "ip": "192.168.100.40",
        "start_address": 1600,
        "end_address": 1699,
        "description": "机器人2 (D1600-D1699)",
        "type": "robot",  # 机器人类型
        "mode_register": 1620,    # D1620: 1自动中/2手动中
        "heartbeat_register": 1649,  # D1649: 心跳
        "alarm_registers": [
            {"address": 1630, "bits": 16, "description": "机器人2告警寄存器 (D1630.0-D1630.15)"}
        ],
        # 机器人2告警映射
        "alarm_mappings": [
            {"address": 1630, "bit": 0, "code": "D5110.0", "message": "机器人2伺服驱动器报警,请复位后再进行操作", "level": 2},
            {"address": 1630, "bit": 1, "code": "D5110.1", "message": "机器人2碰撞检测报警,请检查路径", "level": 2},
            {"address": 1630, "bit": 2, "code": "D5110.2", "message": "机器人2超限报警,请检查工作范围", "level": 2},
            {"address": 1630, "bit": 3, "code": "D5110.3", "message": "机器人2通讯异常,请检查通讯线路", "level": 3},
            {"address": 1630, "bit": 4, "code": "D5110.4", "message": "机器人2真空异常,请检查吸盘", "level": 3},
            {"address": 1630, "bit": 5, "code": "D5110.5", "message": "机器人2抓手异常,请检查夹爪", "level": 3},
            {"address": 1630, "bit": 6, "code": "D5110.6", "message": "机器人2定位偏差过大,请重新标定", "level": 3}
        ]
    },
    
    # ==================== 原有工位（保持兼容）====================
    "桁行架工位": {
        "ip": "192.168.100.40",
        "start_address": 1100,
        "end_address": 1199,
        "description": "桁行架 (D1100-D1199)",
        "type": "legacy"
    },
    "拷机1": {
        "ip": "192.168.100.40",
        "start_address": 1200,
        "end_address": 1299,
        "description": "拷机1 (D1200-D1299)",
        "type": "legacy"
    },
    "拷机2": {
        "ip": "192.168.100.40",
        "start_address": 1300,
        "end_address": 1399,
        "description": "拷机2 (D1300-D1399)",
        "type": "legacy"
    },
    "总测1": {
        "ip": "192.168.100.40",
        "start_address": 1500,
        "end_address": 1599,
        "description": "总测1 (D1500-D1599)",
        "type": "legacy"
    },
    "总测2": {
        "ip": "192.168.100.40",
        "start_address": 1600,
        "end_address": 1699,
        "description": "总测2 (D1600-D1699)",
        "type": "legacy"
    }
}

# 使用虚拟工位配置作为主配置
WORKSTATION_REGISTERS = VIRTUAL_WORKSTATIONS

# 工作站状态管理
workstation_states = {}
# 全局锁，保护所有工位状态
state_lock = threading.RLock()

class WorkstationState:
    """工位状态机 - 基于PLC五步校验流程 (线程安全)"""
    def __init__(self, name, start_address):
        self.name = name
        self.start_address = start_address
        self.last_update = time.time()
        
        # 关键寄存器（相对地址）
        self.address_00 = 1          # Dx00: PLC等待命令（初始为1）
        self.address_10 = 0           # Dx10: 收到回复（由PLC自动设置）
        self.address_51 = 0           # Dx51: 指令码（工控机写入）
        self.address_60 = 0           # Dx60: 判定回复（工控机写入）
        self.address_61 = 0           # Dx61: 测试结果（工控机写入）
        
        # 延迟任务跟踪
        self.delayed_tasks = []
        # 每个工位内部的锁，用于细粒度控制
        self._lock = threading.RLock()
        
    def schedule_delayed_task(self, delay_seconds, callback, *args):
        """调度延迟任务（线程安全）"""
        with self._lock:
            timer = threading.Timer(delay_seconds, self._safe_callback, args=(callback,) + args)
            timer.daemon = True  # 设置为守护线程，避免阻止程序退出
            timer.start()
            self.delayed_tasks.append(timer)
            logger.info(f"工位 {self.name} 调度延迟任务: {delay_seconds}秒后执行")
        
    def _safe_callback(self, callback, *args):
        """在锁保护下执行回调，并处理可能的异常"""
        try:
            with self._lock:
                callback(*args)
        except Exception as e:
            logger.error(f"工位 {self.name} 延迟任务回调异常: {e}", exc_info=True)
        
    def cancel_all_delayed_tasks(self):
        """取消所有延迟任务"""
        with self._lock:
            for timer in self.delayed_tasks:
                timer.cancel()
            self.delayed_tasks = []
            logger.info(f"工位 {self.name} 取消所有延迟任务")
        
    def handle_address_51_write(self, value):
        """处理地址51写入（工控机发送指令码）"""
        with self._lock:
            logger.info(f"工位 {self.name} 地址51收到指令码: {value}")
            self.address_51 = value
            
            # 调度延迟任务：在地址10写入相同的指令码（模拟PLC执行完成）
            def delayed_write_address_10():
                with self._lock:
                    self.address_10 = value
                    logger.info(f"工位 {self.name} 延迟写入地址10: {value}")
                    
            self.schedule_delayed_task(random.uniform(1.0, 2.0), delayed_write_address_10)
        
    def handle_address_60_write(self, value):
        """处理地址60写入（工控机回复完成的对应动作指令）"""
        with self._lock:
            logger.info(f"工位 {self.name} 地址60收到回复指令: {value}")
            self.address_60 = value
            # 根据协议，PLC收到地址60后应将其置零（模拟PLC程序置零）
            self.address_60 = 0
            logger.info(f"工位 {self.name} 地址60立即置零")
        
    def handle_address_61_write(self, value):
        """处理地址61写入（工控机回复状态码 1:OK, 2:NG）"""
        with self._lock:
            logger.info(f"工位 {self.name} 地址61收到状态码: {value}")
            self.address_61 = value
            # 收到地址61后，将地址00设置为1，表示可接收下一条指令
            self.address_00 = 1
            logger.info(f"工位 {self.name} 收到地址61状态码后，自动设置地址00为1")
        
    def get_address_value(self, relative_address):
        """根据相对地址获取当前值（线程安全）"""
        with self._lock:
            if relative_address == 0:
                return self.address_00
            elif relative_address == 10:
                return self.address_10
            elif relative_address == 51:
                return self.address_51
            elif relative_address == 60:
                return self.address_60
            elif relative_address == 61:
                return self.address_61
            else:
                # 其他地址返回随机测试值（0-100）
                return random.randint(0, 100)
            
    def log_current_state(self):
        """记录当前工位状态"""
        with self._lock:
            logger.info(f"工位 {self.name} 当前状态: "
                       f"地址00={self.address_00}, "
                       f"地址10={self.address_10}, "
                       f"地址51={self.address_51}, "
                       f"地址60={self.address_60}, "
                       f"地址61={self.address_61}")

class VirtualWorkstation:
    """虚拟工位管理类 - 负责告警生成、心跳模拟和模式切换"""
    def __init__(self, name, config):
        self.name = name
        self.config = config
        self.ip = config["ip"]
        self.type = config.get("type", "legacy")
        
        # 状态跟踪
        self.alarm_state = {}  # 存储每个告警位的当前状态
        self.heartbeat_counter = 0
        self.mode = 1  # 1:自动中, 2:手动中
        self.last_mode_change = time.time()
        self.alarm_generation_interval = 30.0  # 告警生成间隔（秒）
        self.last_alarm_generation = time.time()
        
        # 初始化告警状态
        self._initialize_alarm_states()
        
        # 启动后台任务
        self._start_background_tasks()
        
        logger.info(f"虚拟工位 {name} ({self.type}) 初始化完成，IP: {self.ip}")
        
    def _initialize_alarm_states(self):
        """初始化告警状态"""
        if "alarm_mappings" in self.config:
            for mapping in self.config["alarm_mappings"]:
                key = f"{mapping.get('address', 1730)}.{mapping.get('bit', 0)}"
                self.alarm_state[key] = {
                    "active": False,
                    "last_triggered": 0,
                    "mapping": mapping
                }
                
    def _start_background_tasks(self):
        """启动后台任务：心跳和告警生成"""
        # 心跳任务
        heartbeat_thread = threading.Thread(target=self._heartbeat_task, daemon=True)
        heartbeat_thread.start()
        
        # 告警生成任务（如果配置了告警）
        if "alarm_mappings" in self.config:
            alarm_thread = threading.Thread(target=self._alarm_generation_task, daemon=True)
            alarm_thread.start()
            
        # 模式切换任务（随机切换自动/手动模式）
        mode_thread = threading.Thread(target=self._mode_switching_task, daemon=True)
        mode_thread.start()
        
    def _heartbeat_task(self):
        """心跳任务 - 定期更新心跳寄存器"""
        heartbeat_reg = self.config.get("heartbeat_register")
        if not heartbeat_reg:
            return
            
        while True:
            try:
                # 递增心跳计数器
                self.heartbeat_counter = (self.heartbeat_counter + 1) % 65535
                
                # 模拟心跳值：0-10随机波动
                heartbeat_value = random.randint(0, 10)
                
                # 更新全局数据块（模拟PLC寄存器）
                if hasattr(self, 'data_block'):
                    with state_lock:
                        self.data_block.setValues(heartbeat_reg, [heartbeat_value])
                        logger.debug(f"虚拟工位 {self.name} 心跳更新: D{heartbeat_reg}={heartbeat_value}")
                        
                time.sleep(2.0)  # 2秒心跳间隔
                
            except Exception as e:
                logger.error(f"虚拟工位 {self.name} 心跳任务异常: {e}")
                time.sleep(5.0)
                
    def _alarm_generation_task(self):
        """告警生成任务 - 随机触发告警"""
        while True:
            try:
                current_time = time.time()
                # 检查是否需要生成告警
                if current_time - self.last_alarm_generation >= self.alarm_generation_interval:
                    self._generate_random_alarms()
                    self.last_alarm_generation = current_time
                    
                time.sleep(1.0)  # 每秒检查一次
                
            except Exception as e:
                logger.error(f"虚拟工位 {self.name} 告警生成任务异常: {e}")
                time.sleep(5.0)
                
    def _generate_random_alarms(self):
        """随机生成告警"""
        if "alarm_mappings" not in self.config or not self.config["alarm_mappings"]:
            return
            
        # 随机选择1-3个告警触发
        num_alarms = random.randint(1, 3)
        mappings = self.config["alarm_mappings"]
        
        for _ in range(num_alarms):
            # 随机选择一个告警
            mapping = random.choice(mappings)
            address = mapping.get("address", 1730)
            bit = mapping.get("bit", 0)
            key = f"{address}.{bit}"
            
            if key in self.alarm_state:
                # 随机决定是否触发告警（30%概率触发）
                if random.random() < 0.3:
                    self._trigger_alarm(address, bit)
                    
    def _trigger_alarm(self, address, bit):
        """触发指定地址和位的告警"""
        key = f"{address}.{bit}"
        if key not in self.alarm_state:
            return
            
        state = self.alarm_state[key]
        mapping = state["mapping"]
        
        # 设置告警状态
        state["active"] = True
        state["last_triggered"] = time.time()
        
        # 更新全局数据块（设置对应位为1）
        if hasattr(self, 'data_block'):
            # 读取当前值
            current_value = self.data_block.getValues(address, 1)[0]
            # 设置对应位为1
            alarm_value = current_value | (1 << bit)
            self.data_block.setValues(address, [alarm_value])
            
            logger.info(f"虚拟工位 {self.name} 触发告警: D{address}.{bit} = 1, "
                       f"告警代码: {mapping['code']}, "
                       f"告警级别: {mapping['level']}, "
                       f"告警信息: {mapping['message']}")
        
        # 调度告警清除任务（30-120秒后自动清除）
        clear_delay = random.uniform(30.0, 120.0)
        def clear_alarm_task():
            self._clear_alarm(address, bit)
            logger.info(f"虚拟工位 {self.name} 自动清除告警: D{address}.{bit} (延迟{clear_delay:.1f}秒)")
        
        clear_timer = threading.Timer(clear_delay, clear_alarm_task)
        clear_timer.daemon = True
        clear_timer.start()
        logger.debug(f"虚拟工位 {self.name} 调度告警清除任务: {clear_delay:.1f}秒后清除 D{address}.{bit}")
                       
    def _clear_alarm(self, address, bit):
        """清除指定地址和位的告警"""
        key = f"{address}.{bit}"
        if key not in self.alarm_state:
            return
            
        state = self.alarm_state[key]
        mapping = state["mapping"]
        
        # 清除告警状态
        state["active"] = False
        
        # 更新全局数据块（清除对应位）
        if hasattr(self, 'data_block'):
            # 读取当前值
            current_value = self.data_block.getValues(address, 1)[0]
            # 清除对应位（设为0）
            alarm_value = current_value & ~(1 << bit)
            self.data_block.setValues(address, [alarm_value])
            
            logger.info(f"虚拟工位 {self.name} 清除告警: D{address}.{bit} = 0, "
                       f"告警代码: {mapping['code']}")
                       
    def _mode_switching_task(self):
        """模式切换任务 - 随机切换自动/手动模式"""
        while True:
            try:
                # 每60-180秒随机切换一次模式
                sleep_time = random.randint(60, 180)
                time.sleep(sleep_time)
                
                # 切换模式：1↔2
                new_mode = 2 if self.mode == 1 else 1
                self.mode = new_mode
                self.last_mode_change = time.time()
                
                # 更新模式寄存器
                mode_reg = self.config.get("mode_register")
                if mode_reg and hasattr(self, 'data_block'):
                    with state_lock:
                        self.data_block.setValues(mode_reg, [new_mode])
                        mode_text = "自动中" if new_mode == 1 else "手动中"
                        logger.info(f"虚拟工位 {self.name} 模式切换: D{mode_reg}={new_mode} ({mode_text})")
                        
            except Exception as e:
                logger.error(f"虚拟工位 {self.name} 模式切换任务异常: {e}")
                time.sleep(5.0)
                
    def get_alarm_summary(self):
        """获取告警摘要"""
        active_alarms = []
        for key, state in self.alarm_state.items():
            if state["active"]:
                mapping = state["mapping"]
                active_alarms.append({
                    "address_bit": key,
                    "code": mapping["code"],
                    "message": mapping["message"],
                    "level": mapping["level"],
                    "last_triggered": state["last_triggered"]
                })
                
        return {
            "name": self.name,
            "ip": self.ip,
            "type": self.type,
            "mode": self.mode,
            "heartbeat_counter": self.heartbeat_counter,
            "active_alarms": active_alarms,
            "total_alarms": len(self.alarm_state)
        }
        
    def set_data_block(self, data_block):
        """设置数据块引用"""
        self.data_block = data_block

# 虚拟工位管理器
virtual_workstations = {}

def initialize_workstation_states():
    """初始化工位状态"""
    for name, config in WORKSTATION_REGISTERS.items():
        # 创建标准工位状态
        with state_lock:
            workstation_states[name] = WorkstationState(name, config["start_address"])
            
        # 创建虚拟工位（对于告警监控类型和综合类型）
        if config.get("type") in ["alarm_monitor", "comprehensive", "robot"]:
            with state_lock:
                virtual_workstations[name] = VirtualWorkstation(name, config)
                
        logger.info(f"初始化工位: {name}, 类型: {config.get('type', 'legacy')}, "
                   f"地址段: {config['start_address']}-{config['end_address']}")

def get_workstation_by_address(address):
    """根据寄存器地址查找所属工位"""
    for name, config in WORKSTATION_REGISTERS.items():
        if config["start_address"] <= address <= config["end_address"]:
            return name, config
    return None, None

def handle_modbus_write(address, value):
    """处理Modbus写入请求 - 根据地址值进行状态机处理"""
    workstation_name, config = get_workstation_by_address(address)
    if workstation_name:
        with state_lock:
            state = workstation_states.get(workstation_name)
        if state:
            relative_address = address - config["start_address"]
            logger.info(f"DEBUG: 工位写入: 工位={workstation_name}, 绝对地址={address}(D{address}), "
                       f"相对地址={relative_address}, 值={value}")
            if relative_address == 0:
                with state._lock:
                    state.address_00 = value
                logger.info(f"工位 {workstation_name} 地址 {address} (D{address}) 写入值: {value}")
                logger.info(f"DEBUG: 工位写入地址0处理完成")
            elif relative_address == 51:
                state.handle_address_51_write(value)
                logger.info(f"DEBUG: 工位写入地址51处理完成")
            elif relative_address == 60:
                state.handle_address_60_write(value)
                logger.info(f"DEBUG: 工位写入地址60处理完成")
            elif relative_address == 61:
                state.handle_address_61_write(value)
                logger.info(f"DEBUG: 工位写入地址61处理完成")
            else:
                logger.debug(f"工位 {workstation_name} 其他地址 {address} 写入值: {value}")
                logger.info(f"DEBUG: 工位其他地址写入: 地址={address}, 值={value}")
            return True
    logger.info(f"DEBUG: 非工位地址写入: 地址={address}(D{address}), 值={value}")
    return False

def handle_modbus_read(address, count):
    """处理Modbus读取请求 - 返回对应地址的值"""
    workstation_name, config = get_workstation_by_address(address)
    if workstation_name:
        with state_lock:
            state = workstation_states.get(workstation_name)
        if state:
            relative_address = address - config["start_address"]
            logger.info(f"DEBUG: 工位读取: 工位={workstation_name}, 绝对地址={address}(D{address}), "
                       f"相对地址={relative_address}, 数量={count}")
            # 单个地址读取
            if count == 1:
                value = state.get_address_value(relative_address)
                logger.info(f"DEBUG: 工位单个读取结果: 值={value}")
                return [value]
            else:
                # 批量读取
                values = []
                for i in range(count):
                    current_rel = relative_address + i
                    value = state.get_address_value(current_rel)
                    values.append(value)
                logger.info(f"DEBUG: 工位批量读取结果: 值={values}")
                return values
    # 非工位地址，返回简单递增数据（用于测试）
    logger.info(f"DEBUG: 非工位地址读取: 地址={address}(D{address}), 数量={count}")
    values = [i + 1 for i in range(count)]
    logger.info(f"DEBUG: 非工位地址读取结果: 值={values}")
    return values

class CustomDataBlock:
    """自定义数据块，支持动态响应、工位状态机和虚拟工位告警"""
    def __init__(self, size=10000):
        self.size = size
        self.values = [0] * size
        # 初始化测试数据
        for i in range(min(100, size)):
            self.values[i] = i + 1  # 地址0-99的值为1-100
            
        # 初始化虚拟工位的告警数据
        self._initialize_virtual_workstation_data()
        
    def _initialize_virtual_workstation_data(self):
        """初始化虚拟工位数据"""
        for name, vw in virtual_workstations.items():
            # 设置数据块引用
            vw.set_data_block(self)
            
            config = VIRTUAL_WORKSTATIONS[name]
            
            # 设置心跳寄存器初始值
            heartbeat_reg = config.get("heartbeat_register")
            if heartbeat_reg:
                self.values[heartbeat_reg] = random.randint(0, 10)
                
            # 设置模式寄存器初始值
            mode_reg = config.get("mode_register")
            if mode_reg:
                self.values[mode_reg] = 1  # 默认自动模式
                
            # 设置告警寄存器初始值
            alarm_regs = config.get("alarm_registers", [])
            for alarm_reg in alarm_regs:
                addr = alarm_reg["address"]
                bits = alarm_reg["bits"]
                # 初始化告警寄存器为0
                self.values[addr] = 0
                
            logger.debug(f"虚拟工位 {name} 数据初始化完成")
            
    def update_virtual_workstation_data(self):
        """更新虚拟工位数据（用于定期更新心跳等动态数据）"""
        for name, vw in virtual_workstations.items():
            config = VIRTUAL_WORKSTATIONS[name]
            
            # 更新心跳寄存器
            heartbeat_reg = config.get("heartbeat_register")
            if heartbeat_reg:
                # 简单的心跳值：基于时间的正弦波
                import math
                heartbeat_value = int(5 + 5 * math.sin(time.time() / 2.0))
                self.values[heartbeat_reg] = heartbeat_value

    def validate(self, address, count=1):
        """验证地址范围是否有效，pymodbus 需要此方法"""
        return 0 <= address < self.size and 0 <= address + count - 1 < self.size

    def getValues(self, address, count=1):
        """获取值（Modbus读请求）"""
        if not self.validate(address, count):
            logger.error(f"读请求越界: address={address}, count={count}, size={self.size}")
            return [0] * count

        # 记录读取请求
        logger.debug(f"DEBUG: PLC模拟器收到读请求: address={address}, count={count}")
        
        # 添加详细的调试信息
        logger.info(f"=== PLC模拟器收到读请求: 地址={address}(D{address}), 数量={count} ===")
        logger.info(f"DEBUG: 客户端IP: {self._get_client_ip() if hasattr(self, '_get_client_ip') else 'unknown'}")
        
        # 检查是否为工位地址
        result = handle_modbus_read(address, count)
        if result:
            logger.info(f"DEBUG: 返回工位地址数据: {result}")
            logger.debug(f"DEBUG: 返回工位地址数据: {result}")
            return result

        # 返回存储的值
        result_values = self.values[address:address + count]
        logger.info(f"DEBUG: 返回存储的值: {result_values}")
        logger.debug(f"DEBUG: 返回存储的值: {result_values}")
        return result_values

    def setValues(self, address, values):
        """设置值（Modbus写请求）"""
        if not self.validate(address, len(values)):
            logger.error(f"写请求越界: address={address}, len={len(values)}, size={self.size}")
            return False

        logger.debug(f"DEBUG: PLC模拟器收到写请求: address={address}, values={values}")
        
        # 添加详细的调试信息
        logger.info(f"=== PLC模拟器收到写请求: 地址={address}(D{address}), 值={values} ===")
        logger.info(f"DEBUG: 客户端IP: {self._get_client_ip() if hasattr(self, '_get_client_ip') else 'unknown'}")
        
        for i, value in enumerate(values):
            current_address = address + i
            if not handle_modbus_write(current_address, value):
                # 如果不是工位地址，存储到数据块
                self.values[current_address] = value
                logger.info(f"DEBUG: 存储到数据块: address={current_address}(D{current_address}), value={value}")
                logger.debug(f"DEBUG: 存储到数据块: address={current_address}, value={value}")
        return True

class CustomModbusServer:
    """自定义Modbus服务器，添加连接和报文日志，并增强异常处理"""
    def __init__(self, context, address):
        self.context = context
        self.address = address
        self.active_clients = {}
        self._client_lock = threading.RLock()
        # 启动后台线程定期清理不活动的客户端
        self._cleanup_timer = threading.Timer(60.0, self._cleanup_clients)
        self._cleanup_timer.daemon = True
        self._cleanup_timer.start()
        
    def _cleanup_clients(self):
        """清理超过5分钟没有活动的客户端"""
        with self._client_lock:
            now = datetime.now()
            to_remove = []
            for client_id, info in self.active_clients.items():
                if now - info['last_activity'] > timedelta(minutes=5):
                    to_remove.append(client_id)
            for client_id in to_remove:
                del self.active_clients[client_id]
                logger.info(f"客户端超时移除: {client_id} (当前连接数: {len(self.active_clients)})")
        # 重新调度
        self._cleanup_timer = threading.Timer(60.0, self._cleanup_clients)
        self._cleanup_timer.daemon = True
        self._cleanup_timer.start()
        
    def start(self):
        """启动服务器（装饰 StartTcpServer）"""
        original_StartTcpServer = StartTcpServer
        
        def CustomStartTcpServer(**kwargs):
            context = kwargs.get('context')
            address = kwargs.get('address', ("0.0.0.0", 502))
            framer = kwargs.get('framer', ModbusSocketFramer)
            
            class CustomFramer(framer):
                def __init__(self, *args, **framer_kwargs):
                    super().__init__(*args, **framer_kwargs)
                    
                def process(self, request):
                    client_addr = None
                    try:
                        client_addr = request.client_address
                        client_id = f"{client_addr[0]}:{client_addr[1]}"
                        
                        with self._client_lock:
                            if client_id not in self.active_clients:
                                self.active_clients[client_id] = {
                                    'connected_at': datetime.now(),
                                    'last_activity': datetime.now()
                                }
                                logger.info(f"客户端上线: {client_id} (当前连接数: {len(self.active_clients)})")
                            else:
                                self.active_clients[client_id]['last_activity'] = datetime.now()
                        
                        logger.info(f"收到来自 {client_id} 的请求: {request}")
                        # 添加详细的Modbus请求信息
                        logger.debug(f"DEBUG: 请求类型: {type(request)}")
                        logger.debug(f"DEBUG: 请求属性: {dir(request)}")
                        if hasattr(request, 'function_code'):
                            logger.debug(f"DEBUG: 功能码: {request.function_code}")
                        if hasattr(request, 'address'):
                            logger.debug(f"DEBUG: 地址: {request.address}")
                        if hasattr(request, 'count'):
                            logger.debug(f"DEBUG: 数量: {request.count}")
                        
                        # 调用父类处理请求
                        response = super().process(request)
                        
                        if response:
                            logger.info(f"发送给 {client_id} 的响应: {response}")
                            logger.debug(f"DEBUG: 响应类型: {type(response)}")
                            if hasattr(response, 'function_code'):
                                logger.debug(f"DEBUG: 响应功能码: {response.function_code}")
                            if hasattr(response, 'data'):
                                logger.debug(f"DEBUG: 响应数据: {response.data}")
                        else:
                            logger.warning(f"父类处理未返回响应: {client_id}")
                            
                        return response
                        
                    except Exception as e:
                        logger.error(f"处理请求时发生错误: {e}", exc_info=True)
                        # 构造一个Modbus异常响应返回，避免客户端挂起
                        try:
                            # 尝试获取原始请求的功能码
                            func_code = request.function_code if hasattr(request, 'function_code') else 1
                            # 返回一个通用的从机设备故障异常
                            error_response = ExceptionResponse(func_code, ModbusExceptions.SlaveDeviceFailure)
                            logger.info(f"返回异常响应给 {client_addr}: {error_response}")
                            return error_response
                        except Exception as e2:
                            logger.error(f"构造异常响应失败: {e2}", exc_info=True)
                            return None
            
            kwargs['framer'] = CustomFramer
            logger.info(f"启动自定义Modbus服务器，监听地址: {address}")
            return original_StartTcpServer(**kwargs)
        
        try:
            CustomStartTcpServer(
                context=self.context,
                address=self.address,
                framer=ModbusSocketFramer  # 注意：这里传入的是类，CustomFramer会覆盖它
            )
        except KeyboardInterrupt:
            logger.info("服务器被用户中断")
        except Exception as e:
            logger.error(f"服务器启动失败: {e}", exc_info=True)
        finally:
            # 停止清理定时器
            self._cleanup_timer.cancel()
            # 取消所有工位的延迟任务
            with state_lock:
                for state in workstation_states.values():
                    state.cancel_all_delayed_tasks()

def report_virtual_workstation_status():
    """报告虚拟工位状态（定期调用）"""
    logger.info("=" * 60)
    logger.info("虚拟工位状态报告")
    logger.info("=" * 60)
    
    for name, vw in virtual_workstations.items():
        summary = vw.get_alarm_summary()
        mode_text = "自动中" if summary["mode"] == 1 else "手动中"
        
        logger.info(f"工位: {summary['name']} ({summary['type']})")
        logger.info(f"  IP: {summary['ip']}")
        logger.info(f"  模式: {summary['mode']} ({mode_text})")
        logger.info(f"  心跳计数器: {summary['heartbeat_counter']}")
        logger.info(f"  总告警数: {summary['total_alarms']}")
        logger.info(f"  活动告警数: {len(summary['active_alarms'])}")
        
        if summary["active_alarms"]:
            logger.info("  活动告警详情:")
            for alarm in summary["active_alarms"]:
                level_text = ["紧急", "警告", "提示"][alarm["level"] - 1]
                logger.info(f"    - {alarm['address_bit']}: {alarm['code']} ({level_text})")
                logger.info(f"      信息: {alarm['message']}")
                
    logger.info("=" * 60)
    
def start_status_reporting():
    """启动状态报告线程（每60秒报告一次）"""
    def reporting_task():
        while True:
            time.sleep(60.0)  # 每60秒报告一次
            report_virtual_workstation_status()
            
    report_thread = threading.Thread(target=reporting_task, daemon=True)
    report_thread.start()
    logger.info("状态报告线程已启动（每60秒报告一次）")

def run_modbus_server():
    """运行Modbus TCP服务器"""
    logger.info("=" * 60)
    logger.info("启动DGIOT UAV Modbus TCP PLC 模拟器 (虚拟工位告警处理版)")
    logger.info("=" * 60)
    
    # 初始化工位状态
    initialize_workstation_states()
    
    # 创建自定义数据块
    store = CustomDataBlock(size=10000)
    
    # 创建Modbus上下文
    context = ModbusSlaveContext(
        hr=store,   # 保持寄存器
        ir=store,   # 输入寄存器
        zero_mode=True
    )
    
    # 创建服务器上下文
    server_context = ModbusServerContext(slaves=context, single=True)
    
    # 启动服务器
    logger.info("服务器正在启动...")
    logger.info(f"监听地址: 0.0.0.0:502")
    logger.info(f"支持地址范围: 0-9999")
    logger.info(f"支持的工位数量: {len(WORKSTATION_REGISTERS)}")
    
    logger.info("虚拟工位配置:")
    for name, config in WORKSTATION_REGISTERS.items():
        if config.get("type") in ["alarm_monitor", "comprehensive", "robot"]:
            alarm_count = len(config.get("alarm_mappings", []))
            logger.info(f"  - {name}: {config['description']} ({config['type']}, 告警数: {alarm_count})")
        else:
            logger.info(f"  - {name}: {config['description']} ({config.get('type', 'legacy')})")
    
    logger.info("=" * 60)
    
    # 启动状态报告
    start_status_reporting()
    
    try:
        server = CustomModbusServer(server_context, ("0.0.0.0", 502))
        server.start()
    except KeyboardInterrupt:
        logger.info("服务器被用户中断")
    except Exception as e:
        logger.error(f"服务器启动失败: {e}", exc_info=True)

if __name__ == "__main__":
    run_modbus_server()