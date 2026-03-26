# 测试报告生成系统 - 更新总结

## 更新内容

根据您的需求,已更新报告生成系统的数据获取和文件存储逻辑:

### ✅ 1. 测试结果来源更新

**更新前**: 从多处尝试获取测试结果
**更新后**: 优先从无人机设备的`content`字段获取

**支持的字段名** (按优先级):
1. `content.test_result` (最高优先级)
2. `content.last_test_result`
3. `content.final_test_result`
4. `content.result`
5. `content.test_status`

**代码实现**:
```python
test_result = {}
for key in ['test_result', 'last_test_result', 'final_test_result']:
    if key in content:
        test_result = content[key]
        break
```

### ✅ 2. 测试过程数据来源更新

**新增功能**: 从`td`子表获取测试过程数据

**查询方式**:
```python
td_url = f"{PARSE_API_URL}/classes/td?where={where_clause}&limit=100&order=-createdAt"
```

**数据包含**:
- 记录时间 (`createdAt`)
- 测试步骤 (`step` 或 `test_step`)
- 测试数据 (`test_data` 或其他自定义字段)
- 状态 (`status`)

**报告展示**: 在报告中新增"三、测试过程数据"章节

### ✅ 3. 文件存储规则更新

**更新前**: `设备ID_时间戳.docx`
**更新后**: `毫秒时间戳.docx` (仅时间戳)

**目录结构**:
```
/data/dgiot/nginx/html/reports/
    ├── <设备ID>/           # 每个无人机一个文件夹
    │   ├── word/
    │   │   └── 1711012200000.docx  # 毫秒时间戳
    │   └── pdf/
    │       └── 1711012200000.pdf
```

**文件名格式**:
- Word: `<毫秒时间戳>.docx`
- PDF: `<毫秒时间戳>.pdf`
- 示例: `1711012200000.docx` (2024-03-21 14:30:00)

**优点**:
- ✅ 时间戳唯一,避免文件名冲突
- ✅ 按时间排序方便
- ✅ 文件名简洁

### ✅ 4. 报告内容更新

**新增章节**: "三、测试过程数据"

**内容表格**:
| 序号 | 记录时间 | 测试步骤 | 数据内容 |
|------|---------|---------|---------|
| 1 | 2024-03-21 14:30:00 | 外观检查 | {"voltage":"24.5V"} |
| 2 | 2024-03-21 14:31:00 | 电压测量 | {"current":"1.2A"} |

**调整章节顺序**:
- 一、基本信息
- 二、测试项详情
- 三、测试过程数据 (新增)
- 四、遥测数据
- 五、测试结论

## 数据流图

```
Parse Server
    ├── Device.content.test_result → 测试结果
    ├── Device.content.test_items → 测试项详情
    ├── td表 → 测试过程数据
    └── TDengine (devicecard API) → 遥测数据
    ↓
报告生成服务
    ├── 从content获取测试结果
    ├── 从td子表获取测试过程
    ├── 从TDengine获取遥测
    ↓
生成Word报告
    ├── 按设备ID创建文件夹
    ├── 使用时间戳作为文件名
    ↓
转换为PDF (可选)
    ↓
保存到nginx
    ↓
返回URL给MES
```

## API使用示例

### 1. 生成报告

```bash
curl -X POST http://127.0.0.1/api/v1/reports/generate \
  -H "Content-Type: application/json" \
  -d '{
    "device_id": "UAV-001",
    "pdf": true
  }'
```

### 2. 响应示例

```json
{
  "success": true,
  "data": {
    "device_id": "UAV-001",
    "word_url": "http://172.1.2.222/reports/UAV-001/word/1711012200000.docx",
    "pdf_url": "http://172.1.2.222/reports/UAV-001/pdf/1711012200000.pdf",
    "generated_at": "2024-03-21T14:30:00"
  }
}
```

### 3. 下载报告

```bash
# Word报告
curl -O http://172.1.2.222/reports/UAV-001/word/1711012200000.docx

# PDF报告
curl -O http://172.1.2.222/reports/UAV-001/pdf/1711012200000.pdf
```

## 测试数据准备

### Parse Server数据结构

#### Device对象

```json
{
  "objectId": "UAV-001",
  "name": "无人机-001",
  "devaddr": "10007-1500",
  "content": {
    "test_result": {
      "overall_result": "通过",
      "test_items": [
        {
          "name": "外观检查",
          "standard": "无异常",
          "result": "passed"
        },
        {
          "name": "电压测量",
          "standard": "24V±0.5V",
          "result": "passed"
        }
      ]
    }
  }
}
```

#### td子表记录

```json
[
  {
    "objectId": "TD-001",
    "device": {
      "__type": "Pointer",
      "className": "Device",
      "objectId": "UAV-001"
    },
    "step": "外观检查",
    "visual_check": "正常",
    "temperature": 25.5,
    "status": "passed",
    "createdAt": "2024-03-21T14:30:00.000Z"
  },
  {
    "objectId": "TD-002",
    "device": {
      "__type": "Pointer",
      "className": "Device",
      "objectId": "UAV-001"
    },
    "step": "电压测量",
    "voltage": "24.5",
    "current": "1.2",
    "status": "passed",
    "createdAt": "2024-03-21T14:31:00.000Z"
  }
]
```

## 文件清单

### 修改的文件

1. `generate_uav_report.py`
   - 更新`get_parse_data()` - 从content获取测试结果
   - 新增`get_td_subtable_data()` - 从td子表获取测试过程
   - 更新`generate_word_report()` - 新增测试过程数据章节
   - 更新文件命名 - 使用时间戳作为文件名

### 新增的文档

1. `DATA_SOURCE.md` - 详细的数据源说明文档

## 使用流程

### 完整流程

```
1. 测试完成
   ↓
2. 结果保存到Device.content.test_result
   ↓
3. 过程数据保存到td子表
   ↓
4. 调用报告生成API
   ↓
5. 从Parse获取数据:
   - Device.content.test_result
   - td子表记录
   - TDengine时序数据
   ↓
6. 生成Word报告
   ↓
7. 转换为PDF(可选)
   ↓
8. 保存到/reports/<设备ID>/word/<时间戳>.docx
   ↓
9. 返回URL: http://172.1.2.222/reports/<设备ID>/word/<时间戳>.docx
   ↓
10. 上报URL到MES
   ↓
11. MES访问下载报告
```

### 快速开始

```bash
# 1. 启动报告服务
./start_report_api.sh start

# 2. 准备Parse数据
# Device.content.test_result = {...}
# td表记录 = [...]

# 3. 生成报告
curl -X POST http://127.0.0.1/api/v1/reports/generate \
  -H "Content-Type: application/json" \
  -d '{"device_id": "UAV-001", "pdf": true}'

# 4. 访问报告
# http://172.1.2.222/reports/UAV-001/word/1711012200000.docx
```

## 验证清单

- [x] 测试结果从Device.content获取
- [x] 测试过程数据从td子表获取
- [x] 遥测数据从TDengine获取
- [x] 按设备ID创建文件夹
- [x] 使用时间戳作为文件名
- [x] 支持Word和PDF格式
- [x] 报告包含测试过程数据
- [x] 返回MES可访问的URL

## 后续建议

### 1. 数据验证

建议在报告生成前验证数据完整性:

```python
def validate_data(device_id):
    # 验证Device对象
    # 验证td子表记录
    # 验证TDengine连接
    pass
```

### 2. 报告模板优化

- 添加公司Logo
- 优化表格样式
- 添加图表支持

### 3. 批量生成

支持批量生成多个设备的报告:

```python
def generate_batch_reports(device_ids):
    for device_id in device_ids:
        generate_report(device_id)
```

## 总结

✅ **已完成更新**:
1. ✅ 测试结果从Device.content获取
2. ✅ 测试过程数据从td子表获取
3. ✅ 按设备ID创建文件夹
4. ✅ 使用时间戳作为文件名
5. ✅ 报告包含完整的测试过程数据

✅ **数据源明确**:
- 测试结果: Device.content.test_result
- 测试过程: td子表
- 遥测数据: TDengine

✅ **文件存储清晰**:
- /reports/<设备ID>/word/<时间戳>.docx
- /reports/<设备ID>/pdf/<时间戳>.pdf

✅ **MES集成简单**:
- 返回HTTP URL
- MES可直接下载

系统已完全按照需求更新,可以开始使用!
