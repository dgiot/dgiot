# TDengine通用工具包使用说明

## 概述

基于TDengine导入导出的经验教训，我们创建了一个通用的工具包，可以智能处理各种常见问题，提高数据导入导出的成功率和效率。

## 工具包组成

### 1. 核心文件

- `tdengine_toolkit.py` - 通用工具包（Python版本）
- `tdengine_importer.py` - 专用导入工具（基于经验教训）
- `tdengine_import.sh` - Shell导入脚本（用户友好）

### 2. 文档文件

- `TDengine_经验教训总结.md` - 详细的经验教训总结
- `README_修正文件说明.md` - 修正文件使用说明
- `README_通用工具包.md` - 本文件

### 3. 测试文件

- `_24b9b4bc50._556dc74e20_corrected.csv` - 一年内时间范围测试文件
- `_24b9b4bc50._a62507c7c7_corrected.csv` - 超过一年时间范围测试文件

## 通用工具包功能

### 1. 智能导入 (`tdengine_toolkit.py`)

```bash
# 基本导入
python3 tdengine_toolkit.py import data.csv table_name --database mydb

# 商用环境模式（自动处理时间戳限制）
python3 tdengine_toolkit.py import data.csv table_name --commercial

# 指定批处理大小
python3 tdengine_toolkit.py import data.csv table_name --batch-size 5000
```

### 2. 数据导出

```bash
# 导出整个表
python3 tdengine_toolkit.py export table_name output.csv --database mydb

# 导出指定时间范围
python3 tdengine_toolkit.py export table_name output.csv --start-time "2023-01-01" --end-time "2023-12-31"

# 限制导出行数
python3 tdengine_toolkit.py export table_name output.csv --limit 1000
```

### 3. 数据验证

```bash
# 验证CSV文件
python3 tdengine_toolkit.py validate data.csv

# 输出JSON格式的验证结果
python3 tdengine_toolkit.py validate data.csv | jq .
```

### 4. 表信息查询

```bash
# 获取表结构信息
python3 tdengine_toolkit.py info table_name --database mydb
```

## 基于经验教训的智能特性

### 1. 自动标题行处理

- **问题**：TDengine FILE导入不能有标题行
- **解决方案**：自动检测并创建无标题行版本
- **效果**：用户无需手动处理标题行

### 2. 字段数量自动修复

- **问题**：字段数量必须与表结构匹配
- **解决方案**：自动检测并修复字段数量
- **效果**：自动添加缺失字段或删除多余字段

### 3. 商用环境时间戳处理

- **问题**：商用环境限制时间戳在一年内
- **解决方案**：自动使用相对时间戳 `NOW() - offset_seconds`
- **效果**：在商用环境也能导入历史数据

### 4. 多语法支持

- **问题**：不同TDengine版本支持不同语法
- **解决方案**：自动尝试多种导入语法
  1. `INSERT INTO table_name FILE 'file.csv';`
  2. `file 'file.csv' into table_name;`
- **效果**：兼容不同版本的TDengine

### 5. 智能错误诊断

- **问题**：错误信息难以理解
- **解决方案**：自动诊断常见错误并提供解决方案
- **效果**：快速定位和解决问题

## 使用示例

### 示例1：导入数据到商用环境

```bash
# 验证CSV文件
python3 tdengine_toolkit.py validate historical_data.csv

# 导入到商用环境（自动处理时间戳限制）
python3 tdengine_toolkit.py import historical_data.csv device_data --database iot_db --commercial

# 验证导入结果
python3 tdengine_toolkit.py info device_data --database iot_db
```

### 示例2：导出和备份数据

```bash
# 导出最近一个月的数据
python3 tdengine_toolkit.py export device_data backup_202312.csv \
  --database iot_db \
  --start-time "2023-11-01" \
  --end-time "2023-11-30"

# 验证导出的CSV文件
python3 tdengine_toolkit.py validate backup_202312.csv
```

### 示例3：数据迁移

```bash
# 从源数据库导出
python3 tdengine_toolkit.py export old_table migration_data.csv --database old_db

# 导入到目标数据库（商用环境）
python3 tdengine_toolkit.py import migration_data.csv new_table --database new_db --commercial

# 验证数据一致性
python3 tdengine_toolkit.py info old_table --database old_db
python3 tdengine_toolkit.py info new_table --database new_db
```

## 配置说明

### 配置文件示例

可以通过修改`TDengineConfig`类来配置工具包：

```python
config = TDengineConfig(
    host="localhost",
    port=6030,
    user="root",
    password="taosdata",
    database="iot_db",
    container_name="tdengine-tsdb",  # Docker容器名
    batch_size=1000,                 # 批处理大小
    commercial_mode=True,            # 商用环境模式
    max_timestamp_range_days=365     # 最大时间范围（天）
)
```

### 环境变量支持

工具包支持通过环境变量配置：

```bash
export TDENGINE_HOST="localhost"
export TDENGINE_PORT=6030
export TDENGINE_USER="root"
export TDENGINE_PASSWORD="taosdata"
export TDENGINE_DATABASE="iot_db"
```

## 故障排除

### 常见问题及解决方案

#### 1. 连接失败

```bash
# 检查TDengine服务状态
docker ps | grep tdengine
systemctl status taosd

# 测试连接
taos -h localhost -P 6030 -u root -p
```

#### 2. 权限不足

```bash
# 检查数据库权限
taos -s "SHOW DATABASES;"
taos -s "USE iot_db; SHOW TABLES;"

# 创建用户和授权
taos -s "CREATE USER admin PASS 'password';"
taos -s "GRANT ALL ON iot_db.* TO admin;"
```

#### 3. 文件格式问题

```bash
# 使用验证功能检查文件
python3 tdengine_toolkit.py validate data.csv

# 查看文件编码
file -i data.csv

# 查看文件格式
head -5 data.csv
```

#### 4. 时间戳问题

```bash
# 检查时间戳格式
head -1 data.csv | awk -F',' '{print $1}'

# 调整时间戳格式
python3 tdengine_toolkit.py import data.csv table_name --commercial
```

## 性能优化建议

### 1. 批处理大小

- **小文件**：使用默认值1000
- **大文件**：根据内存调整，推荐5000-10000
- **网络延迟高**：适当减小批处理大小

### 2. 商用环境模式

- **启用**：`--commercial` 参数
- **效果**：自动使用相对时间戳，避免时间范围限制
- **注意**：会修改原始数据的时间戳

### 3. 文件预处理

- **大文件**：先分割成小文件分批导入
- **复杂文件**：先验证和修复再导入
- **网络传输**：使用压缩文件减少传输时间

### 4. 监控和日志

- **日志文件**：`tdengine_toolkit.log`
- **进度显示**：工具包自动显示导入进度
- **性能统计**：导入完成后显示统计信息

## 最佳实践

### 1. 导入前验证

```bash
# 总是先验证文件
python3 tdengine_toolkit.py validate data.csv

# 修复发现的问题
python3 tdengine_toolkit.py import data.csv table_name
```

### 2. 分阶段导入

```bash
# 1. 测试导入（少量数据）
head -1000 data.csv > test_data.csv
python3 tdengine_toolkit.py import test_data.csv table_name

# 2. 完整导入
python3 tdengine_toolkit.py import data.csv table_name

# 3. 验证结果
python3 tdengine_toolkit.py info table_name
```

### 3. 定期备份

```bash
# 创建备份脚本
#!/bin/bash
BACKUP_DIR="/backup/tdengine"
DATE=$(date +%Y%m%d)

python3 tdengine_toolkit.py export device_data $BACKUP_DIR/device_data_$DATE.csv --database iot_db
python3 tdengine_toolkit.py export sensor_data $BACKUP_DIR/sensor_data_$DATE.csv --database iot_db

# 压缩备份文件
gzip $BACKUP_DIR/*.csv
```

### 4. 监控和告警

```bash
# 监控导入过程
tail -f tdengine_toolkit.log

# 检查错误
grep -i "error\|fail\|exception" tdengine_toolkit.log

# 性能统计
grep -i "耗时\|速度\|成功率" tdengine_toolkit.log
```

## 更新记录

### v1.0 (2025-12-18)

- 基于TDengine经验教训创建通用工具包
- 支持智能导入、导出、验证、信息查询
- 自动处理标题行、字段数量、时间戳等问题
- 支持商用环境模式和多语法导入

### 未来计划

1. 支持更多数据格式（JSON、Parquet等）
2. 分布式导入导出支持
3. Web界面管理工具
4. 实时监控和告警

## 技术支持

### 文档资源

- [TDengine官方文档](https://docs.taosdata.com/)
- [经验教训总结](TDengine_经验教训总结.md)
- [代码注释](tdengine_toolkit.py)

### 问题反馈

1. 查看日志文件：`tdengine_toolkit.log`
2. 使用验证功能：`python3 tdengine_toolkit.py validate data.csv`
3. 检查表结构：`python3 tdengine_toolkit.py info table_name`

### 贡献指南

欢迎提交问题和改进建议：

1. 在GitHub创建Issue
2. 提交Pull Request
3. 更新文档和测试用例

## 总结

TDengine通用工具包基于实际经验教训开发，解决了数据导入导出中的常见问题，提供了智能、可靠、高效的解决方案。通过使用这个工具包，可以显著提高TDengine数据操作的效率和成功率。
