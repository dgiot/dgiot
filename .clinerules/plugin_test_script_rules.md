# 插件测试脚本管理规则

## 概述

本规则定义了DG-IoT平台中插件测试脚本的创建、管理和使用规范。**自动化测试用例是整个软件系统的防护底线**，每一个测试用例都必须严格评审，一旦测试通过就不允许轻易改动，必须经过项目负责人审批才能修改。

## 核心原则

### 1. 测试用例是系统防护底线
- **严格评审**：每一个测试用例都必须经过严格的技术评审
- **禁止随意改动**：一旦测试通过，就不允许轻易改动测试逻辑
- **项目负责人审批**：任何测试用例修改必须经过项目负责人审批

### 2. 禁止随意创建
- **审批流程**：所有新测试脚本必须经过插件负责人和项目负责人双重审批
- **需求明确**：必须有明确的测试需求和目标，作为系统防护的一部分
- **功能唯一**：禁止创建功能重复的脚本，确保测试覆盖无冗余

### 3. 分类存放
- **插件特定脚本**：必须存放在对应插件的测试目录中
- **通用测试脚本**：如需跨插件使用，需特别审批并记录在案

### 4. 文档齐全
- **防护说明**：每个测试脚本必须说明其防护的系统功能
- **使用说明**：每个脚本必须有完整的使用文档和预期结果
- **变更记录**：所有修改必须有详细的变更记录和审批记录

## 目录结构规范

### 1. 标准目录结构
```
apps/{插件名}/test/tools/integration/
├── README.md                 # 插件测试说明文档（必须）
├── test_runners/            # 测试运行脚本
│   ├── test_{功能}_env.sh   # 环境检查脚本
│   ├── test_{功能}_data.sh  # 数据测试脚本
│   └── test_{功能}_reg.sh   # 回归测试脚本
├── simulators/              # 模拟器脚本
│   ├── {设备}_simulator.py  # 设备模拟器
│   └── {协议}_generator.sh  # 数据生成器
└── analysis/                # 分析工具脚本
    ├── analyze_{功能}.sh    # 功能分析脚本
    └── debug_{问题}.sh      # 问题调试脚本
```

### 2. 目录职责说明

#### test_runners/ - 测试运行脚本
- **环境检查**：检查测试环境是否就绪
- **功能测试**：测试特定功能是否正常
- **回归测试**：确保修改不影响现有功能
- **性能测试**：测试系统性能指标

#### simulators/ - 模拟器脚本
- **设备模拟**：模拟真实设备行为
- **数据生成**：生成测试数据
- **压力测试**：模拟高并发场景
- **异常模拟**：模拟异常情况

#### analysis/ - 分析工具脚本
- **日志分析**：分析系统日志发现问题
- **性能分析**：分析系统性能瓶颈
- **数据验证**：验证数据正确性
- **问题诊断**：诊断特定问题原因

## 命名规范

### 1. 脚本命名格式
```
{类型}_{功能}_{描述}.{扩展名}
```

### 2. 类型前缀
- **test_**：测试运行脚本（如 `test_modbus_env.sh`）
- **simulate_**：模拟器脚本（如 `simulate_rtu_device.py`）
- **analyze_**：分析工具脚本（如 `analyze_modbus_flow.sh`）
- **debug_**：调试工具脚本（如 `debug_self_closed.sh`）

### 3. 扩展名规范
- **.sh**：Shell脚本
- **.py**：Python脚本
- **.erl**：Erlang脚本
- **.js**：JavaScript脚本

## 脚本内容规范

### 1. 头注释（必须）
```bash
#!/bin/bash

# 脚本名称：test_modbus_env.sh
# 功能描述：Modbus环境检查和测试脚本
# 作者：DG-IoT团队
# 创建日期：2025-12-26
# 版本：1.0.0
# 使用说明：运行前确保DG-IoT平台已启动
```

### 2. 参数说明
```bash
# 参数说明：
#   -h, --help     显示帮助信息
#   -v, --verbose  显示详细输出
#   -p, --port     指定端口（默认：20000）
```

### 3. 错误处理
```bash
# 错误处理
set -euo pipefail
trap 'echo "脚本执行失败: $?" >&2' ERR
```

### 4. 日志输出
```bash
# 日志函数
log_info() { echo "[INFO] $(date '+%Y-%m-%d %H:%M:%S') $*"; }
log_error() { echo "[ERROR] $(date '+%Y-%m-%d %H:%M:%S') $*" >&2; }
log_debug() { [ "${DEBUG:-0}" = "1" ] && echo "[DEBUG] $(date '+%Y-%m-%d %H:%M:%S') $*"; }
```

## 创建和发布流程

### 1. 创建审批流程
```
需求提出 → 技术评审 → 脚本开发 → 功能测试 → 文档编写 → 代码审查 → 合并发布
```

### 2. 代码审查要求
- **功能验证**：确保脚本功能符合需求
- **代码质量**：符合编码规范，无安全隐患
- **文档完整**：使用说明和示例齐全
- **测试覆盖**：有对应的测试用例

### 3. 发布管控
- **版本管理**：重要脚本需要版本号
- **权限控制**：敏感脚本设置执行权限
- **备份机制**：重要脚本定期备份

## 使用规范

### 1. 环境要求
```bash
# 检查环境
check_environment() {
    # 检查平台是否运行
    if ! pgrep -f "emqx" > /dev/null; then
        log_error "DG-IoT平台未运行，请先启动：make run"
        exit 1
    fi
    
    # 检查必要工具
    for cmd in curl jq nc; do
        if ! command -v $cmd > /dev/null; then
            log_error "缺少必要工具：$cmd"
            exit 1
        fi
    done
}
```

### 2. 参数验证
```bash
# 验证参数
validate_parameters() {
    if [ -z "${PORT:-}" ]; then
        PORT=20000
    fi
    
    if ! [[ "$PORT" =~ ^[0-9]+$ ]] || [ "$PORT" -lt 1 ] || [ "$PORT" -gt 65535 ]; then
        log_error "端口号无效：$PORT"
        exit 1
    fi
}
```

### 3. 清理工作
```bash
# 清理临时文件
cleanup() {
    rm -f /tmp/test_*.log /tmp/test_*.tmp
    log_info "清理完成"
}
trap cleanup EXIT
```

## 示例脚本

### 1. 环境检查脚本示例
```bash
#!/bin/bash

# test_modbus_env.sh - Modbus环境检查脚本

set -euo pipefail

# 参数解析
while [[ $# -gt 0 ]]; do
    case $1 in
        -p|--port)
            PORT="$2"
            shift 2
            ;;
        -h|--help)
            echo "用法: $0 [-p PORT]"
            exit 0
            ;;
        *)
            echo "未知参数: $1"
            exit 1
            ;;
    esac
done

# 默认值
PORT=${PORT:-20000}

# 主函数
main() {
    echo "=== Modbus环境检查 ==="
    echo "端口: $PORT"
    
    # 检查平台
    check_platform
    
    # 检查模块
    check_modules
    
    # 检查端口
    check_port
    
    echo "✅ 环境检查通过"
}

# 运行主函数
main "$@"
```

### 2. 模拟器脚本示例
```python
#!/usr/bin/env python3
# simulate_rtu_device.py - RTU设备模拟器

import socket
import time
import struct

class RTUSimulator:
    def __init__(self, host='127.0.0.1', port=20000):
        self.host = host
        self.port = port
        
    def send_registration(self, device_id='wrj_dm-zqy'):
        """发送注册报文"""
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.connect((self.host, self.port))
            s.sendall(device_id.encode())
            print(f"发送注册报文: {device_id}")
            
    def send_modbus_data(self, slave_id=1, function_code=3, data=b'\x00\x00\x00\x00'):
        """发送Modbus数据"""
        # 构建Modbus RTU帧
        frame = struct.pack('BB', slave_id, function_code) + data
        # 添加CRC（简化示例）
        frame += b'\xC4\x0B'
        
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.connect((self.host, self.port))
            s.sendall(frame)
            print(f"发送Modbus数据: {frame.hex()}")

if __name__ == '__main__':
    simulator = RTUSimulator()
    simulator.send_registration()
    time.sleep(1)
    simulator.send_modbus_data()
```

## 检查清单

### 新脚本创建检查清单
- [ ] 是否有明确的测试需求？
- [ ] 是否经过插件负责人审批？
- [ ] 是否放在正确的目录中？
- [ ] 命名是否符合规范？
- [ ] 头注释是否完整？
- [ ] 参数验证是否完善？
- [ ] 错误处理是否完整？
- [ ] 日志输出是否规范？
- [ ] 使用文档是否齐全？
- [ ] 是否有测试示例？

### 脚本发布检查清单
- [ ] 功能测试是否通过？
- [ ] 代码审查是否通过？
- [ ] 文档是否更新？
- [ ] 版本号是否设置？
- [ ] 权限是否正确？
- [ ] 备份是否完成？

## 违规处理

### 1. 轻度违规
- 命名不规范
- 文档不完整
- 代码格式问题

**处理方式**：要求限期整改

### 2. 中度违规
- 未经审批创建脚本
- 功能重复的脚本
- 存在安全隐患

**处理方式**：暂停使用，重新审查

### 3. 严重违规
- 恶意脚本
- 造成系统故障
- 泄露敏感信息

**处理方式**：立即删除，追究责任

## 更新记录

- **v1.0.0 (2025-12-26)**：创建插件测试脚本管理规则
- **下次评审**：2026-01-26

## 附录

### A. 相关文档
- [开发规则](../development_rules.md)
- [编码规范](../coding_standards.md)
- [测试用例模板](../templates/test_case.md)

### B. 工具参考
```bash
# 检查脚本语法
bash -n script.sh

# 检查Shell脚本规范
shellcheck script.sh

# 检查Python脚本规范
pylint script.py

# 检查脚本权限
ls -la script.sh
```

### C. 联系方式
- 规则负责人：插件技术负责人
- 问题反馈：通过GitHub Issues或团队群组
- 紧急联系：直接联系规则负责人

---

**提示**：严格遵守本规则，确保测试脚本的质量和可维护性。如有疑问，及时咨询规则负责人。
