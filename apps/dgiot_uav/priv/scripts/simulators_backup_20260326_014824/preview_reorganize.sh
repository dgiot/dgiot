#!/bin/bash
# 预览整理操作（不实际执行）

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "========================================"
echo "📋 Simulators目录整理预览"
echo "========================================"
echo ""

echo "📊 当前文件统计:"
echo "----------------------------------------"
cd "$SCRIPT_DIR"
echo "Python脚本: $(ls *.py 2>/dev/null | wc -l) 个"
echo "Markdown文档: $(ls *.md 2>/dev/null | wc -l) 个"
echo "Shell脚本: $(ls *.sh 2>/dev/null | wc -l) 个"
echo "总计: $(ls -1 | wc -l) 个文件/目录"
echo ""

echo "========================================"
echo "Step 1: 工位脚本移动"
echo "========================================"
echo ""
echo "📁 将移动到 stations/:"
for file in magnetic_station_closed_loop_test.py integrated_production_line.py one_click_production_test.py; do
    if [ -f "$file" ]; then
        size=$(ls -lh "$file" | awk '{print $5}')
        echo "  ✅ $file ($size) → stations/"
    fi
done
echo ""

echo "========================================"
echo "Step 2: 设备脚本移动"
echo "========================================"
echo ""
echo "📁 将移动到 devices/:"
for file in plc_simulator.py fixture_simulator.py uav_simulator.py mes_simulator.py; do
    if [ -f "$file" ]; then
        size=$(ls -lh "$file" | awk '{print $5}')
        echo "  ✅ $file ($size) → devices/"
    fi
done
echo ""

echo "========================================"
echo "Step 3: 核心模块移动"
echo "========================================"
echo ""
echo "📁 将移动到 core/:"
for file in multicast_core.py device_simulator.py; do
    if [ -f "$file" ]; then
        size=$(ls -lh "$file" | awk '{print $5}')
        echo "  ✅ $file ($size) → core/"
    fi
done
echo ""

echo "========================================"
echo "Step 4: 工具脚本移动"
echo "========================================"
echo ""
echo "📁 将移动到 tools/:"
for file in one_click_test.py generate_uav_report.py check_magnetic_environment.py report_api_server.py station_test_manager.py verify_one_click_test.py; do
    if [ -f "$file" ]; then
        size=$(ls -lh "$file" | awk '{print $5}')
        echo "  ✅ $file ($size) → tools/"
    fi
done
echo ""

echo "========================================"
echo "Step 5: 核心文档保留"
echo "========================================"
echo ""
echo "📁 将移动到 docs/ (保留):"
for file in README.md WORKFLOW_GUIDE.md QUICK_START.md QUICK_REFERENCE.md README_REORGANIZED.md; do
    if [ -f "$file" ]; then
        size=$(ls -lh "$file" | awk '{print $5}')
        echo "  ✅ $file ($size) → docs/"
    fi
done
echo ""

echo "========================================"
echo "Step 6: 历史文档归档"
echo "========================================"
echo ""

echo "📁 编译修复文档 → docs/archive/compilation_fixes/:"
for file in COMPILATION_ERRORS_FOR_DEEPSEEK.md COMPILE_FIX_SUMMARY.md FINAL_COMPILE_FIX.md ERROR_LOG_SHIELD_REPORT.md; do
    if [ -f "$file" ]; then
        echo "  📦 $file"
    fi
done | head -10

echo ""
echo "📁 测试报告 → docs/archive/test_reports/:"
for file in MAGNETIC_TEST_REPORT.md ONE_CLICK_TEST_SUMMARY.md TASK_COMPLETION_SUMMARY.md; do
    if [ -f "$file" ]; then
        echo "  📦 $file"
    fi
done | head -10

echo ""
echo "📁 旧README → docs/archive/old_readmes/:"
for file in README_ONE_CLICK_TEST.md README_REPORT.md READY_TO_TEST.md USAGE.md; do
    if [ -f "$file" ]; then
        echo "  📦 $file"
    fi
done | head -10

echo ""
echo "📁 工位配置文档 → docs/archive/:"
ls STATION_*.md 2>/dev/null | head -5 | while read file; do
    echo "  📦 $file"
done

echo ""
echo "📁 分析文档 → docs/archive/analysis_docs/:"
for file in DEVICE_STATIC_MOBILE_ANALYSIS.md DATA_SOURCE.md MES_PROXY_CONFIG.md OTHER_STATIONS_TEST_PLAN.md; do
    if [ -f "$file" ]; then
        echo "  📦 $file"
    fi
done | head -10

echo ""
echo "📁 其他文档 → docs/archive/:"
ls *SUMMARY*.md *GUIDE*.md QUICK_*.md 2>/dev/null | grep -v WORKFLOW_GUIDE | grep -v QUICK_START | grep -v QUICK_REFERENCE | head -10 | while read file; do
    echo "  📦 $file"
done

echo ""

echo "========================================"
echo "Step 7: 临时脚本删除"
echo "========================================"
echo ""
echo "🗑️  将删除以下临时脚本:"
echo ""
echo "修复脚本:"
for file in fix_*.sh verify_*.sh final_verification.sh; do
    if [ -f "$file" ]; then
        echo "  ❌ $file"
    fi
done

echo ""
echo "启动脚本:"
for file in quick_test.sh quick_test_1700.sh run_station_1700.sh start_*.sh monitor_test.sh quick_compile.sh; do
    if [ -f "$file" ]; then
        echo "  ❌ $file"
    fi
done

echo ""
echo "临时测试脚本:"
if [ -f "alert_station_test_scenario.py" ]; then
    echo "  ❌ alert_station_test_scenario.py"
fi

echo ""

echo "========================================"
echo "📊 整理后预期结果"
echo "========================================"
echo ""

# 计算预期数量
station_count=$(ls magnetic_station_closed_loop_test.py integrated_production_line.py one_click_production_test.py 2>/dev/null | wc -l)
device_count=$(ls plc_simulator.py fixture_simulator.py uav_simulator.py mes_simulator.py 2>/dev/null | wc -l)
core_count=$(ls multicast_core.py device_simulator.py 2>/dev/null | wc -l)
tool_count=$(ls one_click_test.py generate_uav_report.py check_magnetic_environment.py report_api_server.py station_test_manager.py verify_one_click_test.py 2>/dev/null | wc -l)
doc_count=5
archive_count=$(ls *.md 2>/dev/null | grep -v README.md | grep -v WORKFLOW_GUIDE | grep -v QUICK_START | grep -v QUICK_REFERENCE | grep -v README_REORGANIZED | wc -l)
shell_count=$(ls *.sh 2>/dev/null | grep -v reorganize_simulators.sh | grep -v preview_reorganize.sh | wc -l)

total_core=$((station_count + device_count + core_count + tool_count + doc_count))

echo "核心文件:"
echo "  ├── stations/     $station_count 个工位脚本"
echo "  ├── devices/      $device_count 个设备脚本"
echo "  ├── core/         $core_count 个核心模块"
echo "  ├── tools/        $tool_count 个工具脚本"
echo "  ├── docs/         $doc_count 个核心文档"
echo "  └── archive/      ~$archive_count 个历史文档"
echo ""
echo "总计核心文件: $total_core 个 (精简后)"
echo "原文件总数: $(ls -1 | wc -l) 个"
echo "删除临时文件: ~$shell_count 个"
echo ""

echo "========================================"
echo "⚠️  重要提示"
echo "========================================"
echo ""
echo "1. 整理前会自动备份整个目录"
echo "2. 所有文件只是移动和归档，不会删除重要内容"
echo "3. 临时脚本会被删除（fix_*.sh, start_*.sh等）"
echo "4. 整理后需要更新文档中的路径引用"
echo ""

echo "========================================"
echo "🚀 执行整理"
echo "========================================"
echo ""
echo "预览完成！如需执行整理，请运行:"
echo ""
echo "  ./reorganize_simulators.sh"
echo ""
echo "查看详细整理方案:"
echo ""
echo "  cat README_REORGANIZED.md"
echo ""
