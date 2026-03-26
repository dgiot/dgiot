# Simulators目录整理方案

## 当前问题

- **文件过多**: 71个文件（16个Python + 40个Markdown + 15个Shell）
- **结构混乱**: 文档、脚本混杂
- **重复内容**: 多个README和总结文档

## 整理原则

1. **一个工位一个文件**: 每个工位一个核心Python脚本
2. **文档归档**: 历史文档移至`docs/archive/`
3. **脚本整合**: 功能相近的脚本合并
4. **结构清晰**: 按功能分类

## 新目录结构

```
simulators/
├── stations/                    # 工位模拟器（核心）
│   ├── station_1700_magnetic.py       # 磁航向工位（综合）
│   ├── station_1100_truss.py          # 桁架工位
│   ├── station_1200_copy1.py          # 拷机1工位
│   ├── station_1300_copy2.py          # 拷机2工位
│   ├── station_1500_total1.py         # 总测1工位
│   ├── station_1600_total2.py         # 总测2工位
│   └── production_line.py             # 产线整合（多工位）
│
├── devices/                     # 设备模拟器（基础）
│   ├── plc_simulator.py               # PLC模拟器
│   ├── fixture_simulator.py           # 治具模拟器
│   ├── uav_simulator.py               # 无人机模拟器
│   └── mes_simulator.py               # MES模拟器
│
├── core/                        # 核心模块
│   ├── multicast_core.py              # 多播核心
│   └── device_simulator.py            # 设备基类
│
├── tools/                       # 工具脚本
│   ├── check_environment.py           # 环境检查
│   ├── generate_report.py             # 报告生成
│   └── one_click_test.py              # 一键测试
│
├── docs/                        # 文档目录
│   ├── README.md                      # 主文档
│   ├── WORKFLOW_GUIDE.md              # 工作流程
│   ├── QUICK_START.md                 # 快速开始
│   └── archive/                       # 历史文档归档
│       ├── compilation_fixes/         # 编译修复记录
│       ├── test_reports/              # 测试报告
│       └── old_readmes/               # 旧版README
│
└── logs/                        # 日志目录
    └── test_logs/
```

## 核心文件映射

### 工位文件整合

| 工位 | 原文件 | 新文件 | 功能 |
|------|--------|--------|------|
| **磁航向 (1700)** | magnetic_station_closed_loop_test.py<br>run_station_1700.sh<br>quick_test_1700.sh | **station_1700_magnetic.py** | PLC+地测口+扫码枪+MES<br>完整闭环测试 |
| **产线整合** | integrated_production_line.py<br>one_click_production_test.py | **production_line.py** | 全产线多工位测试 |
| **通用工具** | device_simulator.py<br>multicast_core.py | **core/** | 基础模块 |

### 设备文件保留

| 设备 | 文件 | 说明 |
|------|------|------|
| PLC | plc_simulator.py | PLC模拟器（独立） |
| 治具 | fixture_simulator.py | 治具模拟器（独立） |
| 无人机 | uav_simulator.py | 无人机模拟器（独立） |
| MES | mes_simulator.py | MES模拟器（独立） |

### 文档整合

| 类别 | 保留文档 | 归档文档 |
|------|----------|----------|
| **核心文档** | README.md<br>WORKFLOW_GUIDE.md<br>QUICK_START.md | COMPILATION_ERRORS_FOR_DEEPSEEK.md<br>COMPILE_FIX_SUMMARY.md<br>FINAL_COMPILE_FIX.md |
| **工位文档** | 各工位README（如需要） | STATION_*_*.md系列 |
| **测试报告** | 保留最新报告 | MAGNETIC_TEST_REPORT.md等 |

## 整理步骤

### Step 1: 创建新目录结构

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
mkdir -p stations devices core tools docs/archive logs
```

### Step 2: 移动核心文件

```bash
# 工位文件
mv magnetic_station_closed_loop_test.py stations/station_1700_magnetic.py
mv integrated_production_line.py stations/production_line.py

# 设备文件
mv plc_simulator.py devices/
mv fixture_simulator.py devices/
mv uav_simulator.py devices/
mv mes_simulator.py devices/

# 核心模块
mv multicast_core.py core/
mv device_simulator.py core/

# 工具脚本
mv one_click_production_test.py tools/
mv one_click_test.py tools/
mv generate_uav_report.py tools/generate_report.py
```

### Step 3: 归档历史文档

```bash
# 编译修复文档
mkdir -p docs/archive/compilation_fixes
mv COMPILATION_ERRORS_FOR_DEEPSEEK.md docs/archive/compilation_fixes/
mv COMPILE_FIX_SUMMARY.md docs/archive/compilation_fixes/
mv FINAL_COMPILE_FIX.md docs/archive/compilation_fixes/

# 测试报告
mkdir -p docs/archive/test_reports
mv MAGNETIC_TEST_REPORT.md docs/archive/test_reports/
mv ONE_CLICK_TEST_SUMMARY.md docs/archive/test_reports/

# 旧README
mkdir -p docs/archive/old_readmes
mv README_ONE_CLICK_TEST.md docs/archive/old_readmes/
mv README_REPORT.md docs/archive/old_readmes/

# 其他旧文档
mv STATION_*.md docs/archive/
mv *_SUMMARY.md docs/archive/
mv *_CONFIG.md docs/archive/
mv *_ANALYSIS.md docs/archive/
```

### Step 4: 删除临时脚本

```bash
# 删除修复脚本（已完成）
rm -f fix_*.sh
rm -f verify_*.sh
rm -f final_verification.sh

# 删除临时启动脚本（已整合）
rm -f quick_test.sh
rm -f quick_test_1700.sh
rm -f run_station_1700.sh
rm -f start_*.sh
rm -f monitor_test.sh
```

### Step 5: 创建核心文档

```bash
# 保留核心文档
mv README.md docs/
mv WORKFLOW_GUIDE.md docs/
mv QUICK_START.md docs/

# 或创建新的精简版README
cat > docs/README.md << 'EOF'
# 无人机测试产线模拟器

## 快速开始

### 磁航向工位闭环测试
python3 stations/station_1700_magnetic.py

### 全产线测试
python3 stations/production_line.py

### 单设备模拟
python3 devices/plc_simulator.py
python3 devices/fixture_simulator.py

## 文档

- [工作流程指南](docs/WORKFLOW_GUIDE.md)
- [快速开始](docs/QUICK_START.md)
EOF
```

## 预期结果

### 整理前（当前）
```
71个文件（混乱）
├── 16个Python脚本（混杂）
├── 40个Markdown文档（冗余）
└── 15个Shell脚本（临时）
```

### 整理后（目标）
```
~25个核心文件（清晰）
simulators/
├── stations/        7个工位脚本
├── devices/         4个设备脚本
├── core/            2个核心模块
├── tools/           3个工具脚本
├── docs/            3个核心文档 + archive/
└── logs/            日志输出
```

## 磁航向工位文件说明

**station_1700_magnetic.py** 是磁航向工位的完整闭环测试脚本，包含：

1. **PLC Server** (192.168.100.20:502)
   - Modbus TCP Server
   - 七步校验流程模拟

2. **地测口 Client** (192.168.100.21:10007)
   - EB90协议遥测数据上报
   - 连接DG-IoT:20000

3. **扫码枪 Client** (192.168.100.23:1234)
   - 二维码扫描模拟
   - 设备创建触发

4. **MES Server** (0.0.0.0:801)
   - HTTP API Server
   - 测试结果接收

5. **自动化流程**
   - 设备上线检测
   - 工位绑定
   - 测试执行
   - 数据上报

**使用方法**:
```bash
python3 stations/station_1700_magnetic.py
```

## 注意事项

1. **备份**: 整理前先备份整个目录
2. **测试**: 整理后验证脚本功能正常
3. **文档**: 更新相关文档的路径引用
4. **Git**: 提交前检查是否有遗漏的重要文件

## 执行整理

```bash
# 1. 备份
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts
cp -r simulators simulators_backup_$(date +%Y%m%d)

# 2. 执行整理脚本
cd simulators
./reorganize_simulators.sh

# 3. 验证
python3 stations/station_1700_magnetic.py --test
```

---

**整理原则**: 一个工位一个文件，文档归档保留，删除临时脚本

**预期效果**: 从71个文件精简到25个核心文件，结构清晰易维护
