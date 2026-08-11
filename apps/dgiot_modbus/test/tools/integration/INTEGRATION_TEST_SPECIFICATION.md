# DG-IoT 集成测试规范

## 概述

本规范定义了DG-IoT平台集成测试的分层架构、通用测试步骤和插件专用测试要求，确保测试的一致性和可维护性。

## 1. 测试分层架构

### 1.1 目录结构
```
apps/
├── dgiot/                              # 平台核心
│   └── test/tools/integration/
│       ├── lib/                        # 通用测试函数库
│       │   ├── common_functions.sh     # 通用测试函数
│       │   ├── api_test_functions.sh   # API测试函数
│       │   └── data_validation.sh      # 数据验证函数
│       └── test_runners/common/        # 通用测试脚本
│           ├── test_environment.sh     # 环境检查
│           ├── test_channel_config.sh  # 通道配置测试
│           └── test_api_basics.sh      # API基础测试
│
└── dgiot_<plugin>/                     # 插件目录
    └── test/tools/integration/
        ├── test_runners/               # 插件专用测试脚本
        │   ├── test_<plugin>_protocol.sh  # 协议测试
        │   └── test_<plugin>_closed_loop.sh # 闭环测试
        ├── simulators/                 # 模拟器脚本
        └── analysis/                   # 分析工具
```

### 1.2 各层职责

#### 通用层（平台核心）
- **环境检查**：Erlang节点、平台状态、依赖服务
- **通道管理**：通道配置、创建、验证
- **产品管理**：产品配置、导入、验证
- **数据存储**：Parse、TDengine、缓存验证
- **API测试**：基础API接口验证
- **报告生成**：统一测试报告格式

#### 插件层（插件专用）
- **协议解析**：特定协议的数据解析测试
- **设备模拟**：设备行为模拟器
- **功能验证**：插件特有功能测试
- **性能测试**：插件性能基准测试
- **集成测试**：与平台其他组件的集成测试

## 2. 通用测试步骤（所有插件适用）

### 2.1 环境检查
```bash
# 检查Erlang节点状态
check_erlang_node() {
    if ps aux | grep -q "[e]mqx@127.0.0.1"; then
        return 0
    else
        return 1
    fi
}

# 检查平台服务
check_platform_services() {
    # 检查Parse服务
    # 检查TDengine服务
    # 检查缓存服务
}
```

### 2.2 通道配置验证/自动创建
```bash
# 通用通道配置函数
configure_channel() {
    local channel_id="$1"
    local channel_config="$2"
    
    # 检查通道是否存在
    # 如果不存在，自动创建
    # 验证通道配置
}
```

### 2.3 产品配置验证/自动导入
```bash
# 通用产品配置函数
configure_product() {
    local product_id="$1"
    local product_config="$2"
    
    # 检查产品是否存在
    # 如果不存在，自动导入
    # 验证产品配置
}
```

### 2.4 数据存储验证
```bash
# 验证数据存储
validate_data_storage() {
    local device_id="$1"
    local expected_data="$2"
    
    # 1. 验证Parse存储
    # 2. 验证TDengine存储
    # 3. 验证缓存数据
}
```

### 2.5 API接口验证
```bash
# 基础API测试
test_basic_apis() {
    # 1. 健康检查API
    # 2. 设备查询API
    # 3. 数据查询API
    # 4. 缓存查询API
}
```

### 2.6 测试报告生成
```bash
# 生成统一格式的测试报告
generate_test_report() {
    local plugin_name="$1"
    local test_results="$2"
    
    # 生成HTML/文本格式报告
    # 包含测试摘要、详细结果、建议
}
```

## 3. Modbus插件专用测试步骤

### 3.1 Modbus RTU协议解析测试
```bash
# 测试Modbus RTU协议解析
test_modbus_rtu_parsing() {
    # 1. 测试有效报文解析
    # 2. 测试无效报文处理
    # 3. 测试CRC校验
    # 4. 测试功能码解析
}
```

### 3.2 DTU模拟器测试
```bash
# DTU设备模拟器测试
test_dtu_simulator() {
    # 1. 设备注册测试
    # 2. 数据上报测试
    # 3. 控制指令测试
    # 4. 异常场景测试
}
```

### 3.3 计算值属性验证
```bash
# 验证计算值属性
test_calculated_properties() {
    # 1. 角度X/Y/Z计算验证
    # 2. 公式计算正确性
    # 3. 数据依赖关系验证
}
```

### 3.4 数据块模式测试
```bash
# 数据块模式测试
test_block_data_mode() {
    # 1. 数据块解析
    # 2. 子属性提取
    # 3. 多寄存器读取
}
```

### 3.5 寄存器地址映射验证
```bash
# 寄存器地址映射测试
test_register_mapping() {
    # 1. 地址偏移验证
    # 2. 数据类型转换
    # 3. 字节序处理
}
```

## 4. 测试脚本模板

### 4.1 通用测试脚本模板
```bash
#!/bin/bash
# test_<plugin>_integration.sh - 插件集成测试模板

# 加载通用函数库
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../../.." && pwd)"
source "$PROJECT_ROOT/apps/dgiot/test/tools/integration/lib/common_functions.sh"

# 插件配置
PLUGIN_NAME="modbus"
PRODUCT_ID="feeb43bffb"
CHANNEL_PORT=20000

# 主测试函数
main() {
    log_info "开始 $PLUGIN_NAME 插件集成测试"
    
    # 1. 环境检查
    check_environment || return 1
    
    # 2. 配置验证
    configure_test_environment
    
    # 3. 插件专用测试
    run_plugin_specific_tests
    
    # 4. 数据验证
    validate_test_data
    
    # 5. 生成报告
    generate_test_report
    
    log_info "$PLUGIN_NAME 插件集成测试完成"
}

# 执行主函数
main "$@"
```

### 4.2 闭环测试脚本模板
```bash
#!/bin/bash
# test_<plugin>_closed_loop.sh - 插件闭环测试模板

# 加载通用函数
source_common_functions

# 测试阶段定义
test_phases=(
    "phase1_environment_check"
    "phase2_configuration_setup"
    "phase3_protocol_testing"
    "phase4_data_validation"
    "phase5_api_verification"
    "phase6_report_generation"
)

# 执行所有测试阶段
run_all_test_phases() {
    for phase in "${test_phases[@]}"; do
        log_info "执行测试阶段: $phase"
        $phase || {
            log_error "测试阶段 $phase 失败"
            return 1
        }
    done
}
```

## 5. 最佳实践

### 5.1 避免硬编码路径
```bash
# ❌ 错误：硬编码路径
cd /root/gitee/dgiot

# ✅ 正确：使用相对路径
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../../../.." && pwd)"
cd "$PROJECT_ROOT"
```

### 5.2 错误处理
```bash
# 设置错误处理
set -euo pipefail
trap 'handle_error $LINENO' ERR

handle_error() {
    local line=$1
    log_error "脚本执行失败，行号: $line"
    exit 1
}
```

### 5.3 日志记录
```bash
# 统一日志格式
log_info() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [INFO] $1"
}

log_error() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [ERROR] $1" >&2
}
```

### 5.4 参数化配置
```bash
# 使用参数和环境变量
PRODUCT_ID="${TEST_PRODUCT_ID:-feeb43bffb}"
CHANNEL_PORT="${TEST_CHANNEL_PORT:-20000}"
API_HOST="${API_HOST:-localhost}"
API_PORT="${API_PORT:-8080}"
```

## 6. 检查清单

### 6.1 通用测试检查清单
- [ ] 环境检查通过（Erlang节点、平台服务）
- [ ] 通道配置正确（自动创建或验证）
- [ ] 产品配置正确（自动导入或验证）
- [ ] 数据存储验证通过（Parse、TDengine、缓存）
- [ ] API接口测试通过（健康检查、数据查询）
- [ ] 测试报告生成完整

### 6.2 Modbus插件测试检查清单
- [ ] Modbus RTU协议解析正确
- [ ] DTU模拟器功能正常
- [ ] 计算值属性计算准确
- [ ] 数据块模式解析完整
- [ ] 寄存器地址映射正确

### 6.3 代码质量检查清单
- [ ] 没有硬编码路径
- [ ] 错误处理完善
- [ ] 日志记录规范
- [ ] 参数化配置
- [ ] 代码注释完整

## 7. 更新记录

- **v1.0.0 (2025-12-26)**：创建集成测试规范
  - 定义测试分层架构
  - 明确通用测试步骤
  - 制定Modbus插件测试要求
  - 提供测试脚本模板

- **下次评审**：2026-01-26

## 附录

### A. 相关文档
- [编码规范](../../../../.clinerules/coding_standards.md)
- [插件测试脚本管理规则](../../../../.clinerules/plugin_test_script_rules.md)
- [开发规则](../../../../.clinerules/development_rules.md)

### B. 工具参考
```bash
# 运行集成测试
./test_runners/test_modbus_closed_loop.sh

# 检查脚本语法
bash -n test_script.sh

# 检查代码规范
shellcheck test_script.sh
```

### C. 联系方式
- 测试规范负责人：插件技术负责人
- 问题反馈：通过GitHub Issues或团队群组
- 紧急联系：直接联系规范负责人
