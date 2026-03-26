#!/bin/bash
# Simulators目录自动整理脚本（非交互式，立即执行版本）

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKUP_DIR="${SCRIPT_DIR}_backup_$(date +%Y%m%d_%H%M%S)"

echo "========================================"
echo "📦 Simulators目录整理工具"
echo "========================================"
echo ""
echo "整理原则:"
echo "  ✅ 一个工位一个文件"
echo "  ✅ 设备脚本独立存放"
echo "  ✅ 核心模块单独目录"
echo "  ✅ 文档归档保留"
echo "  ❌ 删除临时脚本"
echo ""

echo "========================================"
echo "Step 1: 创建备份"
echo "========================================"

# 备份整个目录
echo "📦 备份目录到: $BACKUP_DIR"
cp -r "$SCRIPT_DIR" "$BACKUP_DIR"
echo "✅ 备份完成"
echo ""

echo "========================================"
echo "Step 2: 创建新目录结构"
echo "========================================"

cd "$SCRIPT_DIR"

# 创建新目录
mkdir -p stations devices core tools docs/archive logs

# 创建文档归档子目录
mkdir -p docs/archive/compilation_fixes
mkdir -p docs/archive/test_reports
mkdir -p docs/archive/old_readmes
mkdir -p docs/archive/analysis_docs

echo "✅ 目录结构创建完成"
echo ""

echo "========================================"
echo "Step 3: 移动核心文件"
echo "========================================"

# 工位文件
echo "📁 移动工位脚本..."
if [ -f "magnetic_station_closed_loop_test.py" ]; then
    mv magnetic_station_closed_loop_test.py stations/station_1700_magnetic.py
    echo "  ✅ stations/station_1700_magnetic.py"
fi

if [ -f "integrated_production_line.py" ]; then
    mv integrated_production_line.py stations/production_line.py
    echo "  ✅ stations/production_line.py"
fi

if [ -f "one_click_production_test.py" ]; then
    mv one_click_production_test.py stations/
    echo "  ✅ stations/one_click_production_test.py"
fi

# 设备文件
echo "📁 移动设备脚本..."
for file in plc_simulator.py fixture_simulator.py uav_simulator.py mes_simulator.py; do
    if [ -f "$file" ]; then
        mv "$file" devices/
        echo "  ✅ devices/$file"
    fi
done

# 核心模块
echo "📁 移动核心模块..."
for file in multicast_core.py device_simulator.py; do
    if [ -f "$file" ]; then
        mv "$file" core/
        echo "  ✅ core/$file"
    fi
done

# 工具脚本
echo "📁 移动工具脚本..."
if [ -f "one_click_test.py" ]; then
    mv one_click_test.py tools/
    echo "  ✅ tools/one_click_test.py"
fi

if [ -f "generate_uav_report.py" ]; then
    mv generate_uav_report.py tools/generate_report.py
    echo "  ✅ tools/generate_report.py"
fi

if [ -f "report_api_server.py" ]; then
    mv report_api_server.py tools/
    echo "  ✅ tools/report_api_server.py"
fi

if [ -f "station_test_manager.py" ]; then
    mv station_test_manager.py tools/
    echo "  ✅ tools/station_test_manager.py"
fi

if [ -f "verify_one_click_test.py" ]; then
    mv verify_one_click_test.py tools/
    echo "  ✅ tools/verify_one_click_test.py"
fi

echo ""

echo "========================================"
echo "Step 4: 归档历史文档"
echo "========================================"

# 编译修复文档
echo "📁 归档编译修复文档..."
for file in COMPILATION_ERRORS_FOR_DEEPSEEK.md COMPILE_FIX_SUMMARY.md \
            FINAL_COMPILE_FIX.md ERROR_LOG_SHIELD_REPORT.md; do
    if [ -f "$file" ]; then
        mv "$file" docs/archive/compilation_fixes/
        echo "  ✅ docs/archive/compilation_fixes/$file"
    fi
done

# 测试报告
echo "📁 归档测试报告..."
for file in MAGNETIC_TEST_REPORT.md ONE_CLICK_TEST_SUMMARY.md \
            TASK_COMPLETION_SUMMARY.md IMPLEMENTATION_SUMMARY.md \
            FINAL_SUMMARY.md UPDATE_SUMMARY.md TIME_UPDATE_SUMMARY.md; do
    if [ -f "$file" ]; then
        mv "$file" docs/archive/test_reports/
        echo "  ✅ docs/archive/test_reports/$file"
    fi
done

# 旧README
echo "📁 归档旧README..."
for file in README_ONE_CLICK_TEST.md README_REPORT.md READY_TO_TEST.md \
            TEMPLATE_GUIDE.md USAGE.md; do
    if [ -f "$file" ]; then
        mv "$file" docs/archive/old_readmes/
        echo "  ✅ docs/archive/old_readmes/$file"
    fi
done

# 工位配置文档
echo "📁 归档工位配置文档..."
for file in STATION_*.md; do
    if [ -f "$file" ]; then
        mv "$file" docs/archive/
        echo "  ✅ docs/archive/$file"
    fi
done

# 分析文档
echo "📁 归档分析文档..."
for file in DEVICE_STATIC_MOBILE_ANALYSIS.md DATA_SOURCE.md \
            MES_PROXY_CONFIG.md OTHER_STATIONS_TEST_PLAN.md \
            STATION_CONFIG_FIX.md REPORT_SYSTEM.md; do
    if [ -f "$file" ]; then
        mv "$file" docs/archive/analysis_docs/
        echo "  ✅ docs/archive/analysis_docs/$file"
    fi
done

# 架构文档
echo "📁 归档架构文档..."
for file in ARCHITECTURE*.md; do
    if [ -f "$file" ]; then
        mv "$file" docs/archive/
        echo "  ✅ docs/archive/$file"
    fi
done

# 其他总结文档
echo "📁 归档总结文档..."
for file in *SUMMARY*.md *GUIDE*.md QUICK_*.md MAGNETIC_*.md \
            ONE_CLICK_*.md ALRT_*.md; do
    if [ -f "$file" ] && [ "$file" != "WORKFLOW_GUIDE.md" ] && [ "$file" != "QUICK_START.md" ]; then
        mv "$file" docs/archive/
        echo "  ✅ docs/archive/$file"
    fi
done

echo ""

echo "========================================"
echo "Step 5: 移动核心文档"
echo "========================================"

# 保留核心文档
if [ -f "README.md" ]; then
    mv README.md docs/
    echo "  ✅ docs/README.md"
fi

if [ -f "WORKFLOW_GUIDE.md" ]; then
    mv WORKFLOW_GUIDE.md docs/
    echo "  ✅ docs/WORKFLOW_GUIDE.md"
fi

if [ -f "QUICK_START.md" ]; then
    mv QUICK_START.md docs/
    echo "  ✅ docs/QUICK_START.md"
fi

if [ -f "QUICK_REFERENCE.md" ]; then
    mv QUICK_REFERENCE.md docs/
    echo "  ✅ docs/QUICK_REFERENCE.md"
fi

if [ -f "README_REORGANIZED.md" ]; then
    mv README_REORGANIZED.md docs/
    echo "  ✅ docs/README_REORGANIZED.md"
fi

echo ""

echo "========================================"
echo "Step 6: 删除临时脚本"
echo "========================================"

# 删除修复脚本（已完成任务）
echo "🗑️  删除临时修复脚本..."
for file in fix_*.sh verify_*.sh final_verification.sh; do
    if [ -f "$file" ]; then
        rm -f "$file"
        echo "  ❌ $file (已删除)"
    fi
done

# 删除临时启动脚本（功能已整合）
echo "🗑️  删除临时启动脚本..."
for file in quick_test.sh quick_test_1700.sh run_station_1700.sh \
            start_*.sh monitor_test.sh quick_compile.sh; do
    if [ -f "$file" ]; then
        rm -f "$file"
        echo "  ❌ $file (已删除)"
    fi
done

# 删除alert脚本（已整合）
if [ -f "alert_station_test_scenario.py" ]; then
    rm -f alert_station_test_scenario.py
    echo "  ❌ alert_station_test_scenario.py (已删除)"
fi

echo ""

echo "========================================"
echo "Step 7: 创建新的README"
echo "========================================"

cat > README.md << 'EOF'
# 无人机测试产线模拟器

## 目录结构

```
simulators/
├── stations/        工位模拟器（一个工位一个文件）
├── devices/         设备模拟器（PLC、治具、无人机、MES）
├── core/            核心模块（多播、设备基类）
├── tools/           工具脚本（报告生成、环境检查）
├── docs/            文档（核心文档 + 历史归档）
└── logs/            日志输出
```

## 快速开始

### 磁航向工位闭环测试（推荐）

```bash
# 最完整的单工位测试脚本
python3 stations/station_1700_magnetic.py
```

**功能包含**:
- ✅ PLC Server (Modbus TCP)
- ✅ 地测口 Client (EB90协议)
- ✅ 扫码枪 Client
- ✅ MES Server (HTTP API)
- ✅ 自动化测试流程

### 全产线测试

```bash
# 多工位协同测试
python3 stations/production_line.py
```

### 单设备模拟

```bash
# PLC模拟器
python3 devices/plc_simulator.py

# 治具模拟器
python3 devices/fixture_simulator.py

# 无人机模拟器
python3 devices/uav_simulator.py

# MES模拟器
python3 devices/mes_simulator.py
```

## 工位脚本说明

| 工位 | 脚本 | 功能 |
|------|------|------|
| **磁航向 (1700)** | station_1700_magnetic.py | PLC+地测口+扫码枪+MES<br>完整闭环测试 |
| **产线整合** | production_line.py | 多工位协同测试 |
| **一键测试** | one_click_production_test.py | 快速验证测试 |

## 设备脚本说明

| 设备 | 脚本 | 端口 | 说明 |
|------|------|------|------|
| PLC | plc_simulator.py | 502 | Modbus TCP Server |
| 治具 | fixture_simulator.py | 20000 | 舵面+单片机 |
| 无人机 | uav_simulator.py | 226.0.0.80:8001/8002 | 多播EB90协议 |
| MES | mes_simulator.py | 801 | HTTP API Server |

## 文档

- **[工作流程指南](docs/WORKFLOW_GUIDE.md)** - 完整工作流程和磁航向工位详解
- **[快速开始](docs/QUICK_START.md)** - 快速上手指南
- **[快速参考](docs/QUICK_REFERENCE.md)** - 常用命令速查
- **[整理说明](docs/README_REORGANIZED.md)** - 目录整理方案

## 历史文档

所有历史文档已归档到 `docs/archive/` 目录，包括：
- 编译修复记录
- 测试报告
- 配置文档
- 分析文档

## 开发指南

### 新增工位脚本

```python
# 1. 在stations/目录创建脚本
touch stations/station_XXXX.py

# 2. 参考station_1700_magnetic.py编写

# 3. 实现必要接口
- init()          # 初始化设备
- start()         # 启动服务
- stop()          # 停止服务
- test()          # 测试流程
```

### 新增设备脚本

```python
# 1. 在devices/目录创建脚本
touch devices/new_device.py

# 2. 参考现有设备脚本

# 3. 实现设备协议
```

## 故障排除

```bash
# 查看日志
tail -f logs/test.log

# 归档位置
ls docs/archive/
```

---

**整理原则**: 一个工位一个文件，设备独立，文档归档

**最后更新**: 2026-03-26
EOF

echo "  ✅ README.md (新建)"
echo ""

echo "========================================"
echo "Step 8: 统计整理结果"
echo "========================================"

echo ""
echo "📊 整理统计:"
echo "----------------------------------------"
echo "工位脚本: $(ls stations/*.py 2>/dev/null | wc -l) 个"
echo "设备脚本: $(ls devices/*.py 2>/dev/null | wc -l) 个"
echo "核心模块: $(ls core/*.py 2>/dev/null | wc -l) 个"
echo "工具脚本: $(ls tools/*.py 2>/dev/null | wc -l) 个"
echo "核心文档: $(ls docs/*.md 2>/dev/null | wc -l) 个"
echo "归档文档: $(find docs/archive -name '*.md' | wc -l) 个"
echo "Shell脚本: $(ls *.sh 2>/dev/null | wc -l) 个"
echo ""
echo "总计核心文件: $(find stations devices core tools docs -maxdepth 1 -name '*.py' -o -name '*.md' 2>/dev/null | wc -l) 个"
echo "----------------------------------------"
echo ""

echo "========================================"
echo "✅ 整理完成！"
echo "========================================"
echo ""
echo "📂 新目录结构:"
echo "  simulators/"
echo "  ├── stations/        $(ls stations/*.py 2>/dev/null | wc -l) 个工位脚本"
echo "  ├── devices/         $(ls devices/*.py 2>/dev/null | wc -l) 个设备脚本"
echo "  ├── core/            $(ls core/*.py 2>/dev/null | wc -l) 个核心模块"
echo "  ├── tools/           $(ls tools/*.py 2>/dev/null | wc -l) 个工具脚本"
echo "  ├── docs/            核心文档 + archive/"
echo "  └── logs/            日志输出"
echo ""
echo "💡 提示:"
echo "  - 备份位置: $BACKUP_DIR"
echo "  - 历史文档: docs/archive/"
echo "  - 核心文档: docs/"
echo ""
echo "🚀 快速测试:"
echo "  python3 stations/station_1700_magnetic.py"
echo ""
