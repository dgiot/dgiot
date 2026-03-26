# 无人机测试产线模拟器 - 完整使用指南

## 概述

无人机测试产线模拟器提供完整的测试环境模拟,包括:
- **治具模拟器** (fixture_simulator.py)
- **PLC模拟器** (plc_simulator.py)
- **无人机模拟器** (uav_simulator.py)
- **MES模拟器** (mes_simulator.py)
- **报告生成服务** (report_api_server.py)

## 一键启动

### 启动所有模拟器

```bash
cd /root/gitee/dgiot/apps/dgiot_uav/priv/scripts/simulators

# 不包含MES
python3 integrated_production_line.py

# 包含MES
python3 integrated_production_line.py --enable-mes

# 指定测试场景
python3 integrated_production_line.py --test-case magnetic --enable-mes
```

### 启动报告生成服务

```bash
# 启动报告API服务
./start_report_api.sh start

# 查看状态
./start_report_api.sh status

# 查看日志
./start_report_api.sh logs

# 停止服务
./start_report_api.sh stop
```

## 测试报告生成

### 功能说明

测试报告生成系统会:
1. 从**Parse Server**获取设备测试数据(唯一数据源)
2. 生成Word格式的测试报告
3. 可选转换为PDF格式
4. 报告保存到nginx目录,可通过HTTP访问
5. 生成MES可访问的报告URL

### 报告内容

报告包含以下内容:
1. **基本信息**: 设备编号、设备地址、测试日期/时间、测试结果
2. **测试项详情**: 所有测试项目、测试标准、测试结果
3. **遥测数据**: 设备实时数据(从TDengine获取)
4. **测试结论**: 整体测试结果和建议

### 数据来源(以Parse为准)

所有配置信息和测试结果都来自Parse Server:

```
Parse Server (唯一数据源)
    ├── Device表: 设备基本信息、测试结果(content.last_test_result)
    ├── Product表: 产品配置、指令集(content.command_sets)
    └── TDengine: 时序数据(通过devicecard API获取)
```

### API调用方式

#### 方式1: 直接调用Python脚本

```bash
# 生成Word报告
python3 generate_uav_report.py --device-id <设备ID> --session-token <Token>

# 生成Word + PDF报告
python3 generate_uav_report.py --device-id <设备ID> --session-token <Token> --pdf
```

#### 方式2: 通过HTTP API

```bash
# POST请求
curl -X POST http://127.0.0.1/api/v1/reports/generate \
  -H "Content-Type: application/json" \
  -d '{
    "device_id": "<设备ID>",
    "session_token": "<Token>",
    "pdf": true
  }'

# GET请求
curl "http://127.0.0.1/api/v1/reports/generate?device_id=<设备ID>&session_token=<Token>&pdf=true"
```

#### 方式3: 通过Erlang后端调用

在Erlang代码中调用报告生成:

```erlang
%% 生成报告
ReportData = #{
    <<"deviceId">> => <<"UAV-001">>,
    <<"testId">> => <<"TEST-001">>,
    <<"stationName">> => <<"总测1">>,
    <<"testResult">> => <<"通过">>,
    <<"testItems">> => [
        #{<<"name">> => <<"外观检查">>, <<"result">> => <<"passed">>},
        #{<<"name">> => <<"电压测量">>, <<"result">> => <<"passed">>}
    ]
},
{ok, ReportInfo} = dgiot_uav_report_service:generate_word_report(ReportData).

%% 获取报告URL
ReportUrl = maps:get(<<"wordUrl">>, ReportInfo).
```

### 报告文件存储

报告文件存储在nginx目录:

```
/data/dgiot/nginx/html/reports/
    ├── <设备ID>/
    │   ├── word/
    │   │   └── <设备ID>_<时间戳>.docx
    │   └── pdf/
    │       └── <设备ID>_<时间戳>.pdf
```

### 访问报告

#### HTTP访问

```bash
# 访问报告目录
curl http://172.1.2.222/reports/<设备ID>/word/<文件名>.docx

# 浏览器访问
http://172.1.2.222/reports/<设备ID>/word/<文件名>.docx
```

#### MES集成

报告生成后会返回MES可访问的URL:

```json
{
  "device_id": "UAV-001",
  "word_url": "http://172.1.2.222/reports/UAV-001/word/UAV-001_20260321_143000.docx",
  "pdf_url": "http://172.1.2.222/reports/UAV-001/pdf/UAV-001_20260321_143000.pdf",
  "generated_at": "2026-03-21T14:30:00"
}
```

将`word_url`或`pdf_url`上报到MES系统,MES即可直接下载报告。

## MES报告集成

### 在Erlang后端上报报告URL

测试完成后,将报告URL上报到MES:

```erlang
%% 获取报告信息
{ok, ReportInfo} = dgiot_uav_report_service:generate_word_report(TestData),
ReportUrl = maps:get(<<"wordUrl">>, ReportInfo),

%% 上报报告URL到MES
MesData = #{
    <<"func_id">> => <<"REPORT">>,
    <<"line_no">> => LineNo,
    <<"drone_no">> => DroneNo,
    <<"report_url">> => ReportUrl
},
dgiot_uav_mes_api:send_to_mes(MesData).
```

### MES接收报告URL

MES系统收到报告URL后,可以通过以下方式访问:

1. **直接下载**: 使用HTTP GET请求下载报告文件
2. **展示链接**: 在MES系统中显示报告链接,用户点击下载
3. **自动归档**: MES定期下载报告并归档到自己的存储系统

## 配置说明

### Parse Server配置

确保Parse Server中包含以下数据:

1. **Device对象**:
   ```json
   {
     "objectId": "<设备ID>",
     "devaddr": "<设备地址>",
     "name": "<设备名称>",
     "content": {
       "last_test_result": {
         "overall_result": "通过",
         "test_items": [...]
       }
     }
   }
   ```

2. **Product对象**:
   ```json
   {
     "objectId": "<产品ID>",
     "name": "<产品名称>",
     "content": {
       "command_sets": {...}
     }
   }
   ```

### 报告生成配置

报告生成器配置位于`generate_uav_report.py`:

```python
# Parse API地址
PARSE_API_URL = "http://127.0.0.1/iotapi"

# 报告存储目录
NGINX_REPORTS_DIR = "/data/dgiot/nginx/html/reports"

# MES报告访问URL
MES_REPORT_BASE_URL = "http://172.1.2.222/reports"
```

### Nginx配置

Nginx配置已添加报告访问:

```nginx
location /reports/ {
    alias /data/dgiot/nginx/html/reports/;
    autoindex on;
    add_header Access-Control-Allow-Origin *;
}
```

## 故障排除

### 报告生成失败

1. 检查Python依赖:
   ```bash
   pip install python-docx requests flask
   ```

2. 检查Parse Server连接:
   ```bash
   curl http://127.0.0.1/iotapi/classes/Device/<设备ID>
   ```

3. 查看报告服务日志:
   ```bash
   ./start_report_api.sh logs
   ```

### PDF转换失败

安装PDF转换工具:

```bash
# Ubuntu/Debian
apt-get install libreoffice
apt-get install unoconv

# CentOS/RHEL
yum install libreoffice
yum install unoconv
```

### MES无法访问报告

1. 检查nginx配置:
   ```bash
   /data/dgiot/nginx/sbin/nginx -t
   ```

2. 检查报告目录权限:
   ```bash
   ls -la /data/dgiot/nginx/html/reports/
   chmod -R 755 /data/dgiot/nginx/html/reports/
   ```

3. 测试HTTP访问:
   ```bash
   curl http://172.1.2.222/reports/
   ```

## 架构图

```
前端(客户界面)
    ↓ HTTP
Nginx (80端口)
    ├─→ /iotapi/* → Parse Server (配置、测试结果)
    ├─→ /devicecard/* → TDengine (时序数据)
    ├─→ /reports/* → 报告文件目录
    └─→ /api/v1/reports/* → 报告生成服务(5555)
    ├─→ /lezao/jymes/api/* → MES模拟器(801)

Erlang后端 (DG-IoT)
    ├─→ dgiot_parse → Parse Server (CRUD)
    ├─→ dgiot_tdengine → TDengine (时序数据)
    ├─→ dgiot_uav_report_service → 报告生成
    └─→ dgiot_uav_mes_api → MES上报

报告生成服务 (Python Flask:5555)
    └─→ generate_uav_report.py → Word/PDF生成

设备模拟器
    ├─→ fixture_simulator.py → 治具设备
    ├─→ plc_simulator.py → PLC设备
    ├─→ uav_simulator.py → 无人机设备
    └─→ mes_simulator.py → MES服务器
```

## 总结

1. **数据源**: 所有配置和测试结果以Parse Server为准
2. **报告生成**: 从Parse获取数据 → 生成Word/PDF → 保存到nginx
3. **MES集成**: 生成报告URL → 上报到MES → MES访问下载
4. **一键启动**: `integrated_production_line.py --enable-mes`
5. **报告服务**: `./start_report_api.sh start`

完整的测试流程:
```
设备上线 → 测试执行 → 结果保存到Parse → 生成报告 → 上报URL到MES → MES下载报告
```
