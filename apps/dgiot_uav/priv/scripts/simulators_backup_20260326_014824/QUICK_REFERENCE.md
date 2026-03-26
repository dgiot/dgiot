# 无人机测试产线 - 快速参考指南

## 快速开始

```bash
# 1. 进入目录
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators

# 2. 验证系统
python3 verify_one_click_test.py

# 3. 查看工位列表
python3 one_click_production_test.py --list-stations

# 4. 测试单个工位
python3 one_click_production_test.py --station 1500

# 5. 测试完整产线
python3 one_click_production_test.py --full-line --generate-report
```

## 常用命令

### 查看工位

```bash
# 列出所有工位
python3 one_click_production_test.py --list-stations

# 查看工位详情
python3 one_click_production_test.py --station-detail 1200  # 磁航向
python3 one_click_production_test.py --station-detail 1500  # 总测
python3 one_click_production_test.py --station-detail 1600  # 拷机
python3 one_click_production_test.py --station-detail 1100  # 桁架
```

### 测试工位

```bash
# 测试磁航向工位
python3 one_click_production_test.py --station 1200

# 测试总测工位
python3 one_click_production_test.py --station 1500

# 测试拷机工位
python3 one_click_production_test.py --station 1600

# 测试桁架工位
python3 one_click_production_test.py --station 1100
```

### 完整产线测试

```bash
# 测试完整产线
python3 one_click_production_test.py --full-line

# 测试完整产线并生成HTML报告
python3 one_click_production_test.py --full-line --generate-report
```

### 查看结果

```bash
# 查看最新测试结果
ls -lt ./test_logs/result_*.json | head -1
cat ./test_logs/result_*.json | tail -1

# 查看最新日志
tail -f ./test_logs/production_test_*.log

# 在浏览器中查看HTML报告
firefox ./test_logs/report_*.html
```

## 工位配置

| 工位ID | 名称 | 测试步骤 | 耗时(秒) | 命令 |
|--------|------|----------|----------|------|
| 1200 | 磁航向 | 4步 | ~80 | --station 1200 |
| 1500 | 总测 | 10步 | ~150 | --station 1500 |
| 1600 | 拷机 | 4步 | ~170 | --station 1600 |
| 1100 | 桁架 | 3步 | ~70 | --station 1100 |
| 1700 | 告警监控 | 全程监控 | 全程 | 自动启动 |

**完整产线**：1200 → 1500 → 1600 → 1100，总耗时约 **470秒** (~8分钟)

## 测试步骤概览

### 1200 磁航向工位 (4步)

1. 扫码获取设备编码 (5秒)
2. 磁航向校准测试 (30秒)
3. 磁场精度检测 (20秒)
4. 磁偏补偿测试 (25秒)

### 1500 总测工位 (10步)

1. 备检并获取编码 (5秒)
2. 机身静态测试前检查 (10秒)
3. 机身及螺旋桨安装情况检查 (15秒)
4. 电压测量检查 (10秒)
5. 链路功能检查 (20秒)
6. 上电参数检查 (15秒)
7. 夜航灯测试 (10秒)
8. 气压高度检测 (15秒)
9. 系统电磁兼容性功能检查 (20秒)
10. 航线加载及载荷功能检查 (30秒)

### 1600 拷机工位 (4步)

1. 拷机前检查 (10秒)
2. 舵面数据采集测试 (120秒)
3. 导引头功能测试 (30秒)
4. 拷机时长验证 (10秒)

### 1100 桁架工位 (3步)

1. 桁架机械手测试 (20秒)
2. 舵面机械臂测试 (20秒)
3. 导引头功能验证 (30秒)

## 日志文件

```
test_logs/
├── production_test_TESTID_YYYYMMDD_HHMMSS.log  # 测试日志
├── result_TESTID.json                           # 测试结果
└── report_TESTID.html                           # HTML报告
```

## 故障排查

### 命令找不到

```bash
# 确认当前目录
pwd
# 应该在: /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators
```

### 测试失败

```bash
# 查看详细日志
tail -100 ./test_logs/production_test_*.log

# 查看错误信息
cat ./test_logs/result_*.json | grep -A 10 "errors"
```

### 权限问题

```bash
# 给启动脚本添加执行权限
chmod +x start_one_click_test.sh
```

## 系统验证

```bash
# 运行验证脚本
python3 verify_one_click_test.py

# 预期输出: 验证结果: 所有检查通过 ✓
```

## 交互式使用

```bash
# 启动交互式菜单
./start_one_click_test.sh
```

菜单选项：
1. 查看工位列表
2. 查看工位详情
3. 测试单个工位
4. 测试完整产线
5. 查看最新测试结果
6. 查看最新日志
0. 退出

## 文档索引

- **快速参考**: QUICK_REFERENCE.md (本文档)
- **使用文档**: README_ONE_CLICK_TEST.md
- **系统总结**: ONE_CLICK_TEST_SUMMARY.md
- **主脚本**: one_click_production_test.py

## 获取帮助

```bash
# 查看帮助信息
python3 one_click_production_test.py --help

# 或使用启动脚本
./start_one_click_test.sh --help
```

## 测试示例

### 示例1: 快速验证

```bash
# 验证系统
python3 verify_one_click_test.py

# 查看工位
python3 one_click_production_test.py --list-stations
```

### 示例2: 单工位测试

```bash
# 测试总测工位
python3 one_click_production_test.py --station 1500

# 查看结果
cat ./test_logs/result_*.json | python3 -m json.tool
```

### 示例3: 完整产线测试

```bash
# 完整产线测试
python3 one_click_production_test.py --full-line --generate-report

# 查看报告
firefox ./test_logs/report_*.html
```

### 示例4: 调试测试

```bash
# 实时查看日志
tail -f ./test_logs/production_test_*.log

# 在另一个终端运行测试
python3 one_click_production_test.py --station 1500
```

## 性能优化

- **快速测试**: 使用`--station`参数测试单个工位
- **并行测试**: 可以在不同终端同时运行多个工位测试
- **日志管理**: 定期清理旧的测试日志文件

## 注意事项

1. **日志目录**: 确保`./test_logs`目录存在且有写权限
2. **Python版本**: 需要Python 3.6+
3. **网络连接**: 确保可以连接到DG-IoT服务器
4. **端口占用**: 确保测试端口没有被占用

## 版本信息

- **版本**: v1.0.0
- **创建日期**: 2026-03-25
- **维护团队**: DGIoT Team

---

**快速提示**: 如果遇到问题，先运行`python3 verify_one_click_test.py`验证系统！
