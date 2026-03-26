# 无人机测试报告生成系统 - 数据源说明

## 数据源说明

### 1. 测试结果 - 从无人机设备content获取

测试结果直接存储在无人机设备的`content`字段中:

```json
{
  "objectId": "<设备ID>",
  "name": "<设备名称>",
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

**支持的字段名**:
- `content.test_result` (优先级最高)
- `content.last_test_result`
- `content.final_test_result`
- `content.result`
- `content.test_status`

### 2. 测试过程数据 - 从td子表获取

测试过程中的详细数据存储在`td`子表中,关联到设备:

```json
// td表数据示例
{
  "objectId": "<记录ID>",
  "device": {
    "__type": "Pointer",
    "className": "Device",
    "objectId": "<设备ID>"
  },
  "step": "外观检查",
  "test_data": {
    "voltage": "24.5V",
    "current": "1.2A"
  },
  "status": "passed",
  "createdAt": "2026-03-21T14:30:00.000Z"
}
```

**查询方式**:
```bash
GET /iotapi/classes/td?where={"device":{"__type":"Pointer","className":"Device","objectId":"<设备ID>"}}&limit=100&order=-createdAt
```

### 3. 遥测数据 - 从TDengine获取

实时时序数据从TDengine通过devicecard API获取:

```bash
GET /iotapi/devicecard/<设备ID>
```

返回格式:
```json
{
  "data": {
    "voltage": 24.5,
    "current": 1.2,
    "temperature": 35.5,
    ...
  }
}
```

## 文件存储规则

### 目录结构

```
/data/dgiot/nginx/html/reports/
    ├── <设备ID>/           # 每个无人机一个文件夹
    │   ├── word/
    │   │   └── <毫秒时间戳>.docx  # 时间戳作为文件名
    │   └── pdf/
    │       └── <毫秒时间戳>.pdf
```

### 文件命名

- **格式**: `<毫秒时间戳>.docx` / `<毫秒时间戳>.pdf`
- **示例**: `1711012200000.docx` (2024-03-21 14:30:00)
- **优点**: 时间戳唯一,易于排序,避免文件名冲突

### URL格式

```
Word报告: http://172.1.2.222/reports/<设备ID>/word/<时间戳>.docx
PDF报告: http://172.1.2.222/reports/<设备ID>/pdf/<时间戳>.pdf
```

## 报告内容结构

### 1. 基本信息

- 无人机编号
- 设备地址
- 测试日期
- 测试时间
- 测试结果
- 测试人员

**数据源**: Device.name, Device.devaddr, content.test_result

### 2. 测试项详情

包含所有测试项目的结果:

| 序号 | 测试项目 | 测试标准 | 测试结果 |
|------|---------|---------|---------|
| 1 | 外观检查 | 无异常 | 通过 |
| 2 | 电压测量 | 24V±0.5V | 通过 |

**数据源**: content.test_result.test_items 或 content.test_items

### 3. 测试过程数据

td子表中的所有测试记录:

| 序号 | 记录时间 | 测试步骤 | 数据内容 |
|------|---------|---------|---------|
| 1 | 2024-03-21 14:30:00 | 外观检查 | {"voltage":"24.5V"} |
| 2 | 2024-03-21 14:31:00 | 电压测量 | {"current":"1.2A"} |

**数据源**: td表关联到设备的记录

### 4. 遥测数据

当前设备的状态数据:

| 参数名称 | 数值 |
|---------|------|
| voltage | 24.5 |
| current | 1.2 |
| temperature | 35.5 |

**数据源**: /iotapi/devicecard/{deviceId} → TDengine

### 5. 测试结论

基于测试结果的整体评估

**数据源**: content.test_result.overall_result

## API调用示例

### 生成报告

```bash
curl -X POST http://127.0.0.1/api/v1/reports/generate \
  -H "Content-Type: application/json" \
  -d '{
    "device_id": "UAV-001",
    "pdf": true
  }'
```

### 响应示例

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

## 数据获取流程

```
1. 获取设备信息
   ↓ GET /iotapi/classes/Device/<设备ID>
   
2. 从content获取测试结果
   ↓ content.test_result
   
3. 查询td子表获取测试过程数据
   ↓ GET /iotapi/classes/td?where={"device":{"__type":"Pointer","className":"Device","objectId":"<设备ID>"}}
   
4. 获取遥测数据
   ↓ GET /iotapi/devicecard/<设备ID>
   
5. 生成Word报告
   ↓ 保存到 /reports/<设备ID>/word/<时间戳>.docx
   
6. 转换为PDF(可选)
   ↓ 保存到 /reports/<设备ID>/pdf/<时间戳>.pdf
   
7. 返回URL
   ↓ MES访问下载
```

## MES集成

### 上报报告URL

测试完成后,将报告URL上报到MES:

```json
{
  "func_id": "REPORT",
  "line_no": "1500",
  "drone_no": "UAV-001",
  "report_url": "http://172.1.2.222/reports/UAV-001/word/1711012200000.docx",
  "pdf_url": "http://172.1.2.222/reports/UAV-001/pdf/1711012200000.pdf"
}
```

### MES下载报告

MES系统可以直接访问URL下载报告:

```bash
# 下载Word报告
curl -O http://172.1.2.222/reports/UAV-001/word/1711012200000.docx

# 下载PDF报告
curl -O http://172.1.2.222/reports/UAV-001/pdf/1711012200000.pdf
```

## 完整示例

### Parse数据示例

**Device对象**:
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
        }
      ]
    }
  }
}
```

**td子表记录**:
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
    "test_data": {
      "visual_check": "正常"
    },
    "status": "passed",
    "createdAt": "2024-03-21T14:30:00.000Z"
  }
]
```

### 生成的报告

**文件路径**:
- Word: `/data/dgiot/nginx/html/reports/UAV-001/word/1711012200000.docx`
- PDF: `/data/dgiot/nginx/html/reports/UAV-001/pdf/1711012200000.pdf`

**访问URL**:
- Word: `http://172.1.2.222/reports/UAV-001/word/1711012200000.docx`
- PDF: `http://172.1.2.222/reports/UAV-001/pdf/1711012200000.pdf`

## 总结

✅ **数据源**:
- 测试结果: Device.content.test_result
- 测试过程: td子表
- 遥测数据: TDengine (devicecard API)

✅ **文件存储**:
- 按设备ID创建文件夹
- 时间戳作为文件名
- Word和PDF分开存储

✅ **MES集成**:
- 返回HTTP URL
- MES可直接下载
- 支持批量归档

完整的数据流:
Parse → 报告生成 → nginx → MES
