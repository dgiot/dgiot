# TDengine数据修正文件说明

## 概述

基于TDengine商用环境测试经验，我们创建了两份修正后的CSV文件，用于测试时间范围限制。

## 关键发现

### 1. TDengine FILE导入限制

- **不能有标题行**：FILE导入时CSV文件不能包含标题行
- **字段顺序必须匹配**：CSV字段顺序必须与表结构完全一致
- **不支持SKIP参数**：无法在导入时跳过标题行

### 2. 商用环境时间限制

- **一年时间范围**：商用环境通常限制时间戳在一年内
- **相对时间戳**：使用`NOW() - offset_seconds`格式
- **时间戳格式**：`"YYYY-MM-DD HH:MM:SS.mmm"`（带引号）

## 修正文件说明

### 文件1：一年内时间范围

- **文件名**：`_24b9b4bc50._556dc74e20_corrected.csv`
- **时间范围**：2025-06-18 到 2025-10-18（一年内）
- **用途**：测试商用环境是否支持一年内时间范围
- **预期结果**：应该导入成功

### 文件2：超过一年时间范围

- **文件名**：`_24b9b4bc50._a62507c7c7_corrected.csv`
- **时间范围**：2023-12-18 到 2025-12-18（超过一年）
- **用途**：测试商用环境的时间范围限制
- **预期结果**：
  - 如果商用环境有一年限制：导入失败
  - 如果商用环境无限制：导入成功

## 文件结构

两个文件都有相同的结构（31个字段）：

| 字段名 | 类型 | 说明 |
|--------|------|------|
| createdat | TIMESTAMP | 时间戳，带引号的ISO格式 |
| v410_0 | INT | 字段1 |
| v410_2 | INT | 字段2 |
| v410_5 | INT | 字段3 |
| v410_6 | INT | 字段4 |
| vd1000 | DOUBLE | 字段5 |
| vd1012 | DOUBLE | 字段6 |
| vd1016 | DOUBLE | 字段7 |
| vd1020 | DOUBLE | 字段8 |
| vd1024 | DOUBLE | 字段9 |
| vd1028 | DOUBLE | 字段10 |
| vw300 | INT | 字段11 |
| vw304 | INT | 字段12 |
| vw308 | INT | 字段13 |
| vw310 | INT | 字段14 |
| vw312 | INT | 字段15 |
| vw314 | INT | 字段16 |
| vw316 | INT | 字段17 |
| vw318 | INT | 字段18 |
| vw320 | INT | 字段19 |
| vw322 | INT | 字段20 |
| vw326 | INT | 字段21 |
| vw328 | INT | 字段22 |
| vw330 | INT | 字段23 |
| vw332 | INT | 字段24 |
| vw334 | INT | 字段25 |
| vw336 | INT | 字段26 |
| vw458 | DOUBLE | 字段27 |
| vw460 | DOUBLE | 字段28 |
| vw462 | DOUBLE | 字段29 |
| vw464 | DOUBLE | 字段30 |

## 使用方法

### 方法1：使用tdengine_import.sh脚本（推荐）

```bash
# 导入一年内时间范围文件
./tdengine_import.sh _24b9b4bc50._556dc74e20_corrected.csv _556dc74e20

# 导入超过一年时间范围文件
./tdengine_import.sh _24b9b4bc50._a62507c7c7_corrected.csv _a62507c7c7

# 指定导入模式
./tdengine_import.sh _24b9b4bc50._556dc74e20_corrected.csv _556dc74e20 --mode file
./tdengine_import.sh _24b9b4bc50._556dc74e20_corrected.csv _556dc74e20 --mode python
```

### 方法2：使用tdengine_importer.py脚本

```bash
# 导入一年内时间范围文件
python3 tdengine_importer.py _24b9b4bc50._556dc74e20_corrected.csv _556dc74e20

# 导入超过一年时间范围文件
python3 tdengine_importer.py _24b9b4bc50._a62507c7c7_corrected.csv _a62507c7c7

# 指定批处理大小
python3 tdengine_importer.py _24b9b4bc50._556dc74e20_corrected.csv _556dc74e20 --batch-size 5000
```

### 方法3：手动导入（用于调试）

```bash
# 创建无标题行文件
tail -n +2 _24b9b4bc50._556dc74e20_corrected.csv > _24b9b4bc50._556dc74e20_noheader.csv

# 复制到Docker容器
docker cp _24b9b4bc50._556dc74e20_noheader.csv tdengine-tsdb:/tmp/

# 执行FILE导入
docker exec tdengine-tsdb taos -s "
USE _24b9b4bc50;
DELETE FROM _556dc74e20;
INSERT INTO _556dc74e20 FILE '/tmp/_24b9b4bc50._556dc74e20_noheader.csv';
"

# 验证导入结果
docker exec tdengine-tsdb taos -s "
USE _24b9b4bc50;
SELECT COUNT(*) FROM _556dc74e20;
SELECT createdat FROM _556dc74e20 LIMIT 2;
"
```

## 测试预期结果

### 场景1：商用环境有一年时间限制

- 文件1（一年内）：导入成功
- 文件2（超过一年）：导入失败，错误信息包含"Timestamp data out of range"

### 场景2：商用环境无时间限制

- 文件1：导入成功
- 文件2：导入成功

### 场景3：其他问题

- 两个文件都失败：问题不是时间范围，需要检查其他配置

## 故障排除

### 常见错误

1. **语法错误 near 'createdat'**
   - 原因：CSV文件有标题行
   - 解决：使用无标题行文件

2. **Timestamp data out of range**
   - 原因：时间戳超过一年范围
   - 解决：使用一年内的时间戳

3. **invalid data or symbol**
   - 原因：字段数量不匹配
   - 解决：确保CSV有31个字段

4. **字段数量错误**
   - 原因：CSV字段数量不是31个
   - 解决：使用Python导入模式自动修复

## 脚本功能

### tdengine_import.sh

- 自动检测标题行并创建无标题行版本
- 支持三种导入模式：auto、file、python
- 自动处理常见错误
- 提供详细的进度和结果报告

### tdengine_importer.py

- 优先尝试FILE导入
- FILE导入失败时自动切换到Python批量导入
- 自动修复字段数量（确保31个字段）
- 使用相对时间戳适应商用环境限制

## 文件列表

```
apps/dgiot_tdengine/priv/python/
├── tdengine_importer.py          # Python导入脚本
├── tdengine_import.sh            # Shell导入脚本
├── _24b9b4bc50._556dc74e20_corrected.csv      # 一年内时间范围
├── _24b9b4bc50._a62507c7c7_corrected.csv      # 超过一年时间范围
└── README_修正文件说明.md        # 本说明文档
```

## 更新记录

- 2025-12-18：基于TDengine商用环境测试经验创建
- 整合了关键发现：FILE导入不能有标题行
- 创建了两份测试文件用于验证时间范围限制
