# 无人机测试报告自动生成系统

## 快速开始

### 1. 启动报告生成服务

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators

# 启动服务
./start_report_api.sh start

# 查看状态
./start_report_api.sh status
```

### 2. 生成报告

```bash
# 方式1: 直接调用Python脚本
python3 generate_uav_report.py --device-id <设备ID>

# 方式2: HTTP API
curl -X POST http://127.0.0.1/api/v1/reports/generate \
  -H "Content-Type: application/json" \
  -d '{"device_id": "<设备ID>", "pdf": true}'
```

### 3. 访问报告

```bash
# 浏览器访问
http://172.1.2.222/reports/<设备ID>/word/<文件名>.docx
http://172.1.2.222/reports/<设备ID>/pdf/<文件名>.pdf
```

## 核心特点

### 1. 以Parse为准

所有数据来自Parse Server,是唯一数据源:
- 设备信息: `Device.content.last_test_result`
- 产品配置: `Product.content.command_sets`
- 时序数据: 通过`/iotapi/devicecard/{deviceId}`从TDengine获取

### 2. 报告内容

- 基本信息: 设备编号、测试日期/时间、测试结果
- 测试项详情: 测试项目、标准、结果
- 遥测数据: 实时数据展示
- 测试结论: 整体评估和建议

### 3. 文件存储

```
/data/dgiot/nginx/html/reports/
    ├── <设备ID>/
    │   ├── word/
    │   │   └── <设备ID>_<时间戳>.docx
    │   └── pdf/
    │       └── <设备ID>_<时间戳>.pdf
```

### 4. MES集成

测试完成后,将报告URL上报到MES:

```json
{
  "device_id": "UAV-001",
  "word_url": "http://172.1.2.222/reports/UAV-001/word/UAV-001_20260321.docx",
  "pdf_url": "http://172.1.2.222/reports/UAV-001/pdf/UAV-001_20260321.pdf"
}
```

MES可以直接访问这些URL下载报告。

## 架构

```
Parse Server (数据源)
    ↓
报告生成服务 (Python Flask:5555)
    ↓
nginx (80:/reports/*)
    ↓
MES系统 (下载报告)
```

## 配置

### Nginx

已配置报告目录访问:

```nginx
location /reports/ {
    alias /data/dgiot/nginx/html/reports/;
    autoindex on;
    add_header Access-Control-Allow-Origin *;
}
```

### Python脚本

配置位于`generate_uav_report.py`:

```python
PARSE_API_URL = "http://127.0.0.1/iotapi"
NGINX_REPORTS_DIR = "/data/dgiot/nginx/html/reports"
MES_REPORT_BASE_URL = "http://172.1.2.222/reports"
```

## 依赖安装

```bash
pip install python-docx requests flask
apt-get install libreoffice unoconv  # PDF转换(可选)
```

## 完整流程

1. 测试完成 → 结果保存到Parse
2. 调用报告生成API
3. 从Parse获取测试数据
4. 生成Word报告
5. 可选转换为PDF
6. 保存到nginx目录
7. 返回报告URL
8. 上报URL到MES
9. MES访问下载报告

## 故障排除

### 报告生成失败

```bash
# 检查服务状态
./start_report_api.sh status

# 查看日志
./start_report_api.sh logs

# 测试Parse连接
curl http://127.0.0.1/iotapi/classes/Device/<设备ID>
```

### PDF转换失败

安装LibreOffice:
```bash
apt-get install libreoffice
```

### MES无法访问

检查nginx配置:
```bash
/data/dgiot/nginx/sbin/nginx -t
/data/dgiot/nginx/sbin/nginx -s reload
```

## 一键启动

包含所有模拟器和报告服务:

```bash
python3 integrated_production_line.py --enable-mes
```

这会自动启动:
- 治具模拟器
- PLC模拟器
- 无人机模拟器
- MES模拟器
- 报告生成服务(需手动启动: `./start_report_api.sh start`)
