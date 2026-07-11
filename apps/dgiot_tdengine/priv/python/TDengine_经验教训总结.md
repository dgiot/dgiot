# TDengine导入导出经验教训总结

## 概述

本文档总结了在DG-IoT项目中使用TDengine进行数据导入导出的经验教训，基于实际测试和商用环境验证。

## 核心发现

### 1. FILE导入的限制和最佳实践

#### 1.1 标题行问题

- **问题**：TDengine的`FILE`导入命令**不能有标题行**
- **现象**：如果CSV文件包含标题行，会报错：`syntax error near 'createdat'`
- **解决方案**：
  - 导入前移除标题行：`tail -n +2 file.csv > file_noheader.csv`
  - 在脚本中自动检测并处理标题行

#### 1.2 字段数量必须匹配

- **问题**：CSV文件的字段数量必须与表结构完全一致
- **现象**：如果字段数量不匹配，会报错：`invalid data or symbol`
- **解决方案**：
  - 确保CSV有31个字段（根据具体表结构）
  - 使用Python脚本自动修复字段数量

#### 1.3 时间戳格式

- **问题**：时间戳必须使用正确的格式
- **推荐格式**：`"YYYY-MM-DD HH:MM:SS.mmm"`（带引号）
- **商用环境限制**：时间戳通常限制在一年内（31536000秒）

### 2. 商用环境限制

#### 2.1 时间范围限制

- **限制**：商用环境通常限制时间戳在一年内
- **现象**：超过一年的时间戳会报错：`Timestamp data out of range`
- **解决方案**：
  - 使用相对时间戳：`NOW() - offset_seconds`
  - 确保`offset_seconds` ≤ 31536000（一年秒数）

#### 2.2 导入语法差异

- **发现**：不同TDengine版本可能支持不同的导入语法
- **支持的语法**：
  1. `INSERT INTO table_name FILE 'file.csv';`
  2. `file 'file.csv' into table_name;`
- **最佳实践**：在脚本中尝试多种语法

### 3. 性能优化

#### 3.1 批量导入 vs 单行导入

- **FILE导入**：最快，但有限制（无标题行，字段匹配）
- **Python批量导入**：较慢，但更灵活（可处理标题行，修复字段）
- **推荐策略**：先尝试FILE导入，失败时使用Python批量导入

#### 3.2 批处理大小

- **推荐值**：1000-5000行/批
- **考虑因素**：
  - 内存使用
  - 网络延迟
  - 数据库性能

## 通用解决方案

### 1. 智能导入脚本设计

#### 1.1 自动检测和处理

```python
# 检测标题行
if 'createdat' in first_line.lower():
    # 创建无标题行版本
    create_noheader_file(csv_file)

# 检查字段数量
if field_count != expected_fields:
    # 自动修复字段数量
    fix_field_count(csv_file, expected_fields)

# 检查时间戳范围
if timestamp_out_of_range:
    # 使用相对时间戳
    use_relative_timestamps(csv_file)
```

#### 1.2 多语法支持

```bash
# 尝试多种导入语法
syntaxes=(
    "INSERT INTO table_name FILE 'file.csv';"
    "file 'file.csv' into table_name;"
)

for syntax in "${syntaxes[@]}"; do
    if execute_sql "$syntax"; then
        echo "使用语法成功: $syntax"
        break
    fi
done
```

### 2. 错误处理和诊断

#### 2.1 常见错误识别

```python
error_messages = {
    "Timestamp data out of range": "时间戳超出一年范围",
    "syntax error near 'createdat'": "CSV文件有标题行",
    "invalid data or symbol": "字段数量或格式不匹配",
    "invalid timestamp": "时间戳格式错误"
}

def diagnose_error(error):
    for pattern, diagnosis in error_messages.items():
        if pattern in error:
            return diagnosis
    return "未知错误"
```

#### 2.2 自动恢复策略

1. **标题行错误** → 创建无标题行版本重试
2. **字段数量错误** → 修复字段数量重试
3. **时间戳错误** → 使用相对时间戳重试
4. **语法错误** → 尝试其他语法

## 最佳实践

### 1. 数据准备阶段

#### 1.1 CSV文件规范

- 使用UTF-8编码
- 字段用逗号分隔
- 字符串字段用双引号包围
- 时间戳格式：`"YYYY-MM-DD HH:MM:SS.mmm"`

#### 1.2 数据验证

```bash
# 验证CSV文件
validate_csv() {
    # 检查编码
    file -i "$1"
    
    # 检查行数
    wc -l "$1"
    
    # 检查字段数量
    head -1 "$1" | tr ',' '\n' | wc -l
    
    # 检查时间戳格式
    head -5 "$1" | awk -F',' '{print $1}'
}
```

### 2. 导入执行阶段

#### 2.1 分阶段导入

1. **测试导入**：先导入少量数据验证
2. **完整导入**：确认无误后导入全部数据
3. **验证结果**：检查导入的行数和数据完整性

#### 2.2 进度监控

```python
def show_progress(current, total, start_time):
    elapsed = time.time() - start_time
    percent = current / total * 100
    rate = current / elapsed if elapsed > 0 else 0
    remaining = (total - current) / rate if rate > 0 else 0
    
    print(f"进度: {current}/{total} ({percent:.1f}%)")
    print(f"速度: {rate:.1f} 行/秒")
    print(f"预计剩余: {remaining:.0f} 秒")
```

### 3. 故障排除

#### 3.1 诊断步骤

1. **检查文件格式**：编码、分隔符、引号
2. **检查字段数量**：与表结构对比
3. **检查时间戳**：格式和范围
4. **检查权限**：文件访问权限和数据库权限

#### 3.2 调试命令

```bash
# 查看表结构
docker exec tdengine-tsdb taos -s "DESCRIBE database.table_name;"

# 测试少量数据导入
head -10 file.csv > test.csv
./import_script.sh test.csv table_name

# 查看详细错误
docker exec tdengine-tsdb taos -s "INSERT INTO table_name FILE 'file.csv';" 2>&1
```

## 通用脚本设计

### 1. 设计原则

#### 1.1 模块化设计

- **文件处理模块**：处理CSV文件格式
- **数据库连接模块**：管理TDengine连接
- **导入逻辑模块**：实现多种导入策略
- **错误处理模块**：诊断和恢复错误

#### 1.2 配置驱动

```yaml
# config.yaml
database:
  host: localhost
  port: 6030
  user: root
  password: taosdata
  
import:
  batch_size: 1000
  max_retries: 3
  timeout: 300
  
csv:
  encoding: utf-8
  delimiter: ','
  quote_char: '"'
  has_header: true
```

### 2. 功能特性

#### 2.1 智能导入

- 自动检测文件格式
- 自动修复常见问题
- 自动选择最佳导入方法

#### 2.2 详细日志

```python
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('import.log'),
        logging.StreamHandler()
    ]
)
```

#### 2.3 性能监控

- 导入速度统计
- 内存使用监控
- 错误率统计

## 实际案例

### 案例1：商用环境时间限制

#### 问题

在商用TDengine环境导入历史数据失败，错误：`Timestamp data out of range`

#### 分析

商用环境限制时间戳在一年内，而历史数据超过一年

#### 解决方案

1. 使用相对时间戳：`NOW() - offset_seconds`
2. 确保`offset_seconds` ≤ 31536000
3. 分批导入，每批时间范围不超过一年

#### 代码实现

```python
def adjust_timestamps_for_commercial(csv_file, output_file):
    """调整时间戳适应商用环境限制"""
    with open(csv_file, 'r') as f_in, open(output_file, 'w') as f_out:
        # 处理标题行
        header = f_in.readline()
        f_out.write(header)
        
        # 处理数据行
        total_lines = sum(1 for _ in f_in)
        f_in.seek(0)
        f_in.readline()  # 跳过标题行
        
        for i, line in enumerate(f_in, 1):
            fields = line.strip().split(',')
            
            # 使用相对时间戳
            offset_seconds = min((total_lines - i) * 10, 31536000)
            fields[0] = f"NOW() - {offset_seconds}s"
            
            f_out.write(','.join(fields) + '\n')
```

### 案例2：字段数量不匹配

#### 问题

CSV文件有30个字段，但表结构需要31个字段

#### 解决方案

1. 自动检测字段数量
2. 添加缺失的字段（使用默认值）
3. 记录修复的字段位置

#### 代码实现

```python
def fix_field_count(csv_file, expected_fields=31, default_value='0'):
    """修复CSV文件的字段数量"""
    fixed_lines = []
    
    with open(csv_file, 'r') as f:
        for line in f:
            fields = line.strip().split(',')
            
            # 添加或删除字段以达到预期数量
            if len(fields) < expected_fields:
                # 添加缺失字段
                fields.extend([default_value] * (expected_fields - len(fields)))
            elif len(fields) > expected_fields:
                # 删除多余字段
                fields = fields[:expected_fields]
            
            fixed_lines.append(','.join(fields))
    
    # 写回文件
    with open(csv_file, 'w') as f:
        f.write('\n'.join(fixed_lines))
```

## 总结

### 关键经验

1. **了解限制**：熟悉TDengine的各种限制（标题行、时间范围、字段数量）
2. **自动化处理**：在脚本中自动处理常见问题
3. **灵活应对**：支持多种导入语法和策略
4. **详细日志**：记录所有操作和错误，便于调试

### 推荐工具

1. **智能导入脚本**：自动处理各种问题
2. **数据验证工具**：导入前验证数据质量
3. **性能监控工具**：监控导入过程和资源使用

### 未来改进

1. **支持更多数据格式**：JSON、Parquet等
2. **分布式导入**：支持多节点并行导入
3. **实时监控**：Web界面监控导入进度
4. **智能优化**：基于历史数据自动优化导入参数

通过遵循这些经验教训和最佳实践，可以显著提高TDengine数据导入的成功率和效率。
