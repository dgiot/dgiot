# 脚本开发规范指南

## 概述

本文档定义了 `dgiot_modbus` 插件测试脚本的开发规范，确保脚本质量、可维护性和一致性。

## 1. 脚本组织规范

### 1.1 目录结构
```
test/tools/
├── integration/          # 集成测试脚本
│   ├── simulators/      # 模拟器脚本
│   ├── test_runners/    # 测试运行脚本
│   ├── analysis/        # 分析工具
│   └── utils/          # 工具函数
├── unit/               # 单元测试工具
├── e2e/               # 端到端测试
└── SCRIPT_DEVELOPMENT_GUIDE.md  # 本文档
```

### 1.2 脚本分类
- **模拟器脚本**：模拟设备行为的脚本（Python/Erlang）
- **测试运行脚本**：运行测试的Shell脚本
- **分析工具**：数据分析、日志分析工具
- **工具函数**：通用工具函数库

## 2. 命名规范

### 2.1 文件命名
- **小写字母+下划线**：`rtu_simulator_complete.py`
- **描述性名称**：清晰描述脚本功能
- **扩展名明确**：`.sh`、`.py`、`.erl`

### 2.2 命名模式
```
<功能>_<描述>.<扩展名>
示例：
- rtu_simulator_complete.py
- test_with_log_monitor.sh
- analyze_modbus_flow.sh
```

## 3. 脚本头规范

### 3.1 Shell脚本头
```bash
#!/bin/bash
# 脚本名称：test_with_log_monitor.sh
# 功能描述：带实时日志监控的RTU测试
# 作者：<姓名>
# 创建日期：2025-12-25
# 版本：1.0.0
# 使用方法：./test_with_log_monitor.sh
# 依赖：python3, curl, grep, tail
```

### 3.2 Python脚本头
```python
#!/usr/bin/env python3
"""
脚本名称：rtu_simulator_complete.py
功能描述：完整的RTU模拟器，模拟Modbus RTU设备
作者：<姓名>
创建日期：2025-12-25
版本：1.0.0
使用方法：python3 rtu_simulator_complete.py
依赖：requests, socket, time
"""
```

### 3.3 Erlang脚本头
```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% 脚本名称：dgiot_modbus_dtu_simulator.erl
%%% 功能描述：DTU模拟器模块
%%% 作者：<姓名>
%%% 创建日期：2025-12-25
%%% 版本：1.0.0
%%% 使用方法：dgiot_modbus_dtu_simulator:test_full_flow()
%%% @end
%%%-------------------------------------------------------------------
```

## 4. 代码质量规范

### 4.1 Shell脚本规范
- **错误处理**：使用 `set -e` 或显式错误检查
- **变量引用**：使用双引号引用变量 `"$VAR"`
- **函数定义**：使用小写字母和下划线
- **返回值**：成功返回0，失败返回非0

### 4.2 Python脚本规范
- **PEP 8**：遵循Python代码风格指南
- **类型提示**：尽可能使用类型提示
- **异常处理**：适当的try-catch块
- **文档字符串**：函数和类有完整的docstring

### 4.3 日志输出规范
```bash
# 标准日志格式
echo "✅ 操作成功"
echo "❌ 操作失败: 错误信息"
echo "⚠️  警告信息"
echo "📊 数据信息: 值"
```

## 5. 参数处理规范

### 5.1 命令行参数
```bash
# 使用getopts处理参数
while getopts "hvf:" opt; do
    case $opt in
        h) show_help; exit 0 ;;
        v) VERBOSE=true ;;
        f) CONFIG_FILE="$OPTARG" ;;
        *) echo "无效参数"; exit 1 ;;
    esac
done
```

### 5.2 配置文件
- 使用JSON/YAML格式的配置文件
- 提供配置模板
- 支持环境变量覆盖

## 6. 错误处理规范

### 6.1 错误检查
```bash
# 检查命令执行结果
if ! command; then
    echo "❌ 命令执行失败"
    exit 1
fi

# 检查文件存在
if [ ! -f "$FILE" ]; then
    echo "❌ 文件不存在: $FILE"
    exit 1
fi
```

### 6.2 错误信息
- **清晰明确**：说明错误原因
- **可操作**：提供解决方案建议
- **上下文**：包含相关参数信息

## 7. 文档规范

### 7.1 脚本文档
每个脚本必须包含：
1. **功能描述**：脚本的主要功能
2. **使用方法**：命令行参数和示例
3. **依赖说明**：需要的软件和库
4. **输出说明**：脚本的输出格式
5. **错误代码**：可能的错误代码和含义

### 7.2 README文档
每个目录必须包含 `README.md`：
1. **目录概述**：目录内容和目的
2. **脚本列表**：所有脚本的简要说明
3. **使用示例**：典型使用场景
4. **维护说明**：如何维护和更新

## 8. 测试规范

### 8.1 自测试
```bash
# 脚本应包含自测试功能
if [ "$1" = "--test" ]; then
    run_self_test
    exit $?
fi
```

### 8.2 兼容性测试
- 测试不同环境下的兼容性
- 验证依赖版本
- 检查边界条件

## 9. 版本控制

### 9.1 版本号
使用语义化版本号：`主版本.次版本.修订号`
- **主版本**：不兼容的API修改
- **次版本**：向下兼容的功能性新增
- **修订号**：向下兼容的问题修正

### 9.2 变更日志
```markdown
## 1.1.0 (2025-12-25)
### 新增
- 添加了日志监控功能
- 支持新的API参数

### 修复
- 修复了路径引用问题
- 修复了错误处理逻辑

### 变更
- 更新了配置文件格式
```

## 10. 维护规范

### 10.1 代码审查
- 所有脚本必须经过代码审查
- 检查是否符合本规范
- 验证功能正确性

### 10.2 定期更新
- 每季度审查一次脚本
- 更新依赖版本
- 修复已知问题

## 11. 示例模板

### 11.1 Shell脚本模板
```bash
#!/bin/bash
# 脚本名称：template.sh
# 功能描述：脚本模板
# 作者：<姓名>
# 创建日期：2025-12-25
# 版本：1.0.0

set -euo pipefail

# 配置参数
readonly SCRIPT_NAME="$(basename "$0")"
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 颜色输出
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly NC='\033[0m'

# 日志函数
log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# 显示帮助
show_help() {
    cat << EOF
使用方法: $SCRIPT_NAME [选项]

选项:
    -h, --help      显示此帮助信息
    -v, --verbose   详细输出
    -c, --config    配置文件路径

示例:
    $SCRIPT_NAME --config config.json
EOF
}

# 主函数
main() {
    log_info "脚本开始执行"
    
    # 脚本逻辑
    
    log_info "脚本执行完成"
}

# 脚本入口
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
```

## 12. 检查清单

### 新脚本开发检查清单
- [ ] 脚本头信息完整
- [ ] 遵循命名规范
- [ ] 错误处理完善
- [ ] 日志输出规范
- [ ] 参数处理正确
- [ ] 文档完整
- [ ] 自测试通过
- [ ] 代码审查通过

### 脚本更新检查清单
- [ ] 更新版本号
- [ ] 更新变更日志
- [ ] 更新文档
- [ ] 测试兼容性
- [ ] 验证功能正确性

---
*最后更新：2025年12月25日*
