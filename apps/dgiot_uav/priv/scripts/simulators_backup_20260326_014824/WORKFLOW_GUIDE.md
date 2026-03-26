# 无人机测试报告生成系统 - 完整工作流程

## 工作流程概述

```
无人机上线 → 绑定工位 → 查询测试项 → 执行测试 → 数据汇聚 → 生成报告 → MES下载
```

## 详细流程

### 1. 无人机上线并绑定工位

**Erlang后端**:
```erlang
%% 设备上线
dgiot_device:online(DeviceId).

%% 绑定工位
StationId = 1500,
dgiot_uav_station_manager:bind_device_to_station(DeviceId, StationId).
```

**Parse Server数据**:
```json
{
  "objectId": "UAV-001",
  "name": "无人机-001",
  "devaddr": "10007-1500",
  "content": {
    "station_id": "1500",
    "station_name": "总测工位1"
  }
}
```

### 2. 通过工位查询测试项

**Python报告生成器**:
```python
def get_test_items_by_station(self, station_id, station_name):
    """通过工位查询测试项"""
    where_clause = json.dumps({
        "product": {
            "__type": "Pointer",
            "className": "Product",
            "objectId": "343cf21f82"  # 测试项产品ID
        },
        "name": {
            "$regex": f"^{station_name}_"  # 工位名称前缀
        }
    })

    test_items_url = f"{PARSE_API_URL}/classes/Device?where={where_clause}&limit=100"
    response = requests.get(test_items_url, headers=headers)
    return response.json().get('results', [])
```

**测试项命名规则**:
- 格式: `{工位名称}_{测试项名称}`
- 示例:
  - `总测工位1_外观检查`
  - `总测工位1_电压测量`
  - `总测工位1_通信测试`

**Parse Server数据**:
```json
[
  {
    "objectId": "TEST-001",
    "name": "总测工位1_外观检查",
    "content": {
      "order": 1,
      "standard": "无异常",
      "criteria": "外观完整,无损坏",
      "action": "visual_check",
      "timeout": 30
    }
  },
  {
    "objectId": "TEST-002",
    "name": "总测工位1_电压测量",
    "content": {
      "order": 2,
      "standard": "24V±0.5V",
      "action": "measure_voltage",
      "timeout": 10
    }
  }
]
```

### 3. 自动下发测试指令

**Erlang后端**:
```erlang
%% 自动测试启动
dgiot_uav_auto_tester:start_auto_test(DeviceId, StationId).

%% 查询测试项
TestItems = dgiot_uav_test_item_loader:load_test_items_by_station(StationId).

%% 下发测试指令
lists:foreach(fun(TestItem) ->
    #{<<"content">> := Content} = TestItem,
    Action = maps:get(<<"action">>, Content),
    dgiot_uav_command_scheduler:send_command(DeviceId, Action, Content)
end, TestItems).
```

### 4. 数据汇聚到td子表

**Erlang后端**:
```erlang
%% 汇聚测试数据到td子表
dgiot_uav_data_collector:collect_test_data(DeviceId, TestData).

%% 保存到Parse
Record = #{
    <<"device">> => #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"Device">>, <<"objectId">> => DeviceId},
    <<"step">> => StepName,
    <<"test_data">> => TestData,
    <<"status">> => Status
},
dgiot_parse:create_object(<<"td">>, Record).
```

**Parse Server数据**:
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
      "visual_check": "正常",
      "temperature": 25.5
    },
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
    "test_data": {
      "voltage": "24.5",
      "current": "1.2"
    },
    "status": "passed",
    "createdAt": "2024-03-21T14:31:00.000Z"
  }
]
```

### 5. 生成Word报告

**Python报告生成器**:
```python
# 1. 从Parse获取数据
report_data = generator.get_parse_data(device_id)

# 2. 构建模板变量
variables = generator.build_template_variables(report_data)

# 3. 加载Word模板
doc = Document(TEMPLATE_PATH)

# 4. 替换模板变量
doc = generator.replace_template_variables(doc, variables)

# 5. 保存Word报告
doc.save(word_filepath)
```

**Word模板变量**:
- `{无人机编号}`: 设备名称
- `{工位名称}`: 工位名称
- `{测试日期}`: 测试日期
- `{测试时间}`: 测试时间
- `{测试结果}`: 整体结果
- `{测试项表格}`: 测试项详情
- `{测试过程表格}`: 测试过程数据
- `{电压}`, `{电流}`, `{温度}`: 遥测数据

### 6. 返回报告URL

**响应示例**:
```json
{
  "device_id": "UAV-001",
  "station_name": "总测工位1",
  "word_url": "http://172.1.2.222/reports/UAV-001/word/1711012200000.docx",
  "pdf_url": "http://172.1.2.222/reports/UAV-001/pdf/1711012200000.pdf",
  "generated_at": "2024-03-21T14:30:00"
}
```

### 7. MES下载报告

**MES系统**:
```bash
# 下载Word报告
curl -O http://172.1.2.222/reports/UAV-001/word/1711012200000.docx

# 下载PDF报告
curl -O http://172.1.2.222/reports/UAV-001/pdf/1711012200000.pdf
```

## API调用示例

### 生成报告

```bash
curl -X POST http://127.0.0.1/api/v1/reports/generate \
  -H "Content-Type": application/json" \
  -d '{
    "device_id": "UAV-001",
    "pdf": true
  }'
```

### 查询测试项

```bash
# 通过工位查询测试项
curl "http://127.0.0.1/iotapi/classes/Device?where={\"product\":{\"__type\":\"Pointer\",\"className\":\"Product\",\"objectId\":\"343cf21f82\"},\"name\":{\"$regex\":\"^总测工位1_\"}}"
```

### 查询td子表

```bash
# 查询设备的测试过程数据
curl "http://127.0.0.1/iotapi/classes/td?where={\"device\":{\"__type\":\"Pointer\",\"className\":\"Device\",\"objectId\":\"UAV-001\"}}"
```

## 数据流图

```
┌─────────────────────────────────────────────────────────────┐
│                    无人机测试流程                            │
└─────────────────────────────────────────────────────────────┘

1. 设备上线
   ↓
   ├─→ Device对象创建/更新
   ├─→ content.station_id = "1500"
   └─→ content.station_name = "总测工位1"

2. 查询测试项
   ↓
   ├─→ WHERE product = "343cf21f82"
   ├─→ AND name LIKE "总测工位1_%"
   └─→ ORDER BY content.order

3. 执行测试
   ↓
   ├─→ 循环测试项
   ├─→ 下发指令 → 执行动作
   ├─→ 收集数据
   └─→ 记录结果

4. 数据汇聚
   ↓
   ├─→ td子表记录 (测试过程)
   ├─→ Device.content.test_result (测试结果)
   └─→ TDengine (遥测数据)

5. 生成报告
   ↓
   ├─→ 从Parse获取所有数据
   ├─→ 构建模板变量
   ├─→ 加载Word模板
   ├─→ 替换变量
   ├─→ 保存Word报告
   └─→ 转换为PDF

6. MES集成
   ↓
   ├─→ 返回报告URL
   ├─→ MES访问下载
   └─→ 归档存储
```

## 文件组织

### 报告文件结构

```
/data/dgiot/nginx/html/reports/
    ├── <设备ID>/
    │   ├── word/
    │   │   └── <毫秒时间戳>.docx
    │   └── pdf/
    │       └── <毫秒时间戳>.pdf
```

### 文件命名

- 格式: `<毫秒时间戳>.docx` / `<毫秒时间戳>.pdf`
- 示例: `1711012200000.docx`
- 优点: 唯一、可排序、无冲突

## 配置说明

### Python脚本配置

```python
# generate_uav_report.py
PARSE_API_URL = "http://127.0.0.1/iotapi"
NGINX_REPORTS_DIR = "/data/dgiot/nginx/html/reports"
MES_REPORT_BASE_URL = "http://172.1.2.222/reports"
TEMPLATE_PATH = "/root/gitee/dgiot/apps/dgiot_uav/priv/scripts/test_report_template.docx"
TEST_ITEM_PRODUCT_ID = "343cf21f82"
```

### Nginx配置

```nginx
location /reports/ {
    alias /data/dgiot/nginx/html/reports/;
    autoindex on;
    add_header Access-Control-Allow-Origin *;
}
```

## 使用示例

### 完整流程

```bash
# 1. 启动报告服务
./start_report_api.sh start

# 2. 设备上线并绑定工位 (通过前端或API)

# 3. 自动测试 (Erlang后端自动执行)

# 4. 数据汇聚 (自动汇聚到td子表)

# 5. 生成报告
curl -X POST http://127.0.0.1/api/v1/reports/generate \
  -H "Content-Type": application/json" \
  -d '{"device_id": "UAV-001", "pdf": true}'

# 6. 查看报告
# http://172.1.2.222/reports/UAV-001/word/1711012200000.docx

# 7. MES下载
curl -O http://172.1.2.222/reports/UAV-001/pdf/1711012200000.pdf
```

## 故障排除

### 测试项查询失败

1. 检查测试项产品ID: `343cf21f82`
2. 检查工位名称是否正确
3. 检查测试项命名规则: `工位名称_测试项名称`

### 数据汇聚失败

1. 检查td子表是否存在
2. 检查设备关联是否正确
3. 查看Erlang日志

### 报告生成失败

1. 检查Word模板是否存在
2. 检查模板变量是否正确
3. 检查Parse数据是否完整

### MES无法下载

1. 检查nginx配置
2. 检查报告文件是否存在
3. 检查文件权限

## 磁航向工位 - PLC七步校验流程

### 工位概述

**磁航向工位 (Station ID: 1700)** 是无人机测试产线的关键工位，通过PLC七步校验流程完成磁航向校准测试。

**核心特点**:
- 工位ID: 1700
- 基地址: D1700
- PLC通信: Modbus TCP协议
- 测试流程: 七步校验（Step 1-7）

### 七步校验流程详解

| 步骤 | 操作类型 | 相对地址 | 绝对地址 | 说明 |
|------|----------|----------|----------|------|
| **Step 1** | READ | D+0 | D1700 | 读取工位就绪状态 |
| **Step 2** | WRITE | D+51 | D1751 | 写入测试命令码 |
| **Step 3** | READ | D+10 | D1710 | 读取测试确认状态 |
| **Step 4** | WRITE | D+0 | D1700 | 复位工位状态 (写入0) |
| **Step 5** | WRITE | D+10 | D1710 | 清除测试确认 (写入0) |
| **Step 6** | WRITE | D+60 | D1760 | 写入完成确认码 |
| **Step 7** | WRITE | D+61 | D1761 | 触发完成信号 (写入1) |

### 地址计算规则

```erlang
%% 地址计算公式
AbsoluteAddress = BaseAddress + RelativeAddress

%% 示例
StationId = 1700,
BaseAddress = 1700,
RelativeAddress = 51,
AbsoluteAddress = 1700 + 51 = 1751  % D1751
```

### 日志示例

#### 开始标志

```
========================================
🎯 【PLC七步校验】开始执行
========================================
Station ID: 1700
Command Index: 1
Command Code: 100
Interval: 1000 ms
========================================
```

#### Step 1: 读取工位就绪状态

```
----------------------------------------
📌 Step 1/7: 读取工位就绪状态 (Read D+0, 1 register)
----------------------------------------

========================================
📖  [PLC Read Command] Detailed Message
========================================
Station ID: 1700
Base Addr: D1700
Relative Addr: 0
Absolute Addr: D1700 (1700)
Read Count: 1 registers
----------------------------------------
Modbus TCP Frame (12 bytes):
  Transaction ID: 0000
  Protocol ID:    0000
  Length:         0006 (6 bytes)
  Slave ID:       01 (1)
  Function Code:  03 (Read Holding Registers)
  Register Addr:  06A4 (1700)
  Register Count: 0001 (1)
----------------------------------------
Hex: 00000000000601036A40001
========================================

📤 Send Result: {send,<<0,0,0,0,0,6,1,3,106,164,0,1>>}
```

#### Step 1 响应

```
========================================
📥 【PLC TCP响应】收到Modbus响应报文
========================================
Station ID: 1700
Step ID: 1/7
Command Index: 1
Command Code: 100
----------------------------------------
响应报文 (11 bytes):
  Hex: 0000000000050103020001
  Binary: <<0,0,0,0,0,5,1,3,2,0,1>>
  Transaction ID: 0000
  Protocol ID:    0000
  Length:         0005 (5 bytes)
  Slave ID:       01 (1)
  Function Code:  03 (Read Holding Registers)
  Byte Count:     2
  Registers:      [1]
========================================

✅ Modbus响应解析成功: #{registers => [1]}
```

#### Step 2: 写入测试命令码

```
----------------------------------------
📌 Step 2/7: 写入测试命令码 100 (Write D+51)
----------------------------------------

========================================
✏️  [PLC Write Command] Detailed Message
========================================
Station ID: 1700
Base Addr: D1700
Relative Addr: 51
Absolute Addr: D1751 (1751)
Write Value: 100
----------------------------------------
Modbus TCP Frame (12 bytes):
  Transaction ID: 0000
  Protocol ID:    0000
  Length:         0006 (6 bytes)
  Slave ID:       01 (1)
  Function Code:  06 (Write Single Register)
  Register Addr:  06D7 (1751)
  Value:          0064 (100)
----------------------------------------
Hex: 00000000000601066D70064
========================================

📤 Send Result: {send,<<0,0,0,0,0,6,1,6,109,119,0,100>>}
```

#### 完成标志

```
========================================
✅ 【PLC七步校验】全部完成
========================================
```

### Erlang API

#### 高级API（自动处理相对地址）

```erlang
%% 读取告警状态（自动使用相对地址30）
dgiot_uav_plc_tcp_client:read_alarms(1700).
% 自动读取D1730告警区域

%% 读取心跳状态（自动使用相对地址49）
dgiot_uav_plc_tcp_client:read_heartbeat(1700).
% 自动读取D1749心跳寄存器

%% 读取工位状态（自动使用相对地址48）
dgiot_uav_plc_tcp_client:read_station_status(1700).
% 自动读取D1748工位状态

%% 读取运行模式（自动使用相对地址50）
dgiot_uav_plc_tcp_client:read_operation_mode(1700).
% 自动读取D1750运行模式
```

#### 底层API（手动指定地址）

```erlang
%% 读取操作
dgiot_uav_plc_tcp_client:read(StationId, RelativeAddr, RegisterCount).

%% 示例：读取D1700（相对地址0），读取1个寄存器
dgiot_uav_plc_tcp_client:read(1700, 0, 1).

%% 写入操作
dgiot_uav_plc_tcp_client:write(StationId, RelativeAddr, Value).

%% 示例：写入D1751（相对地址51），写入值100
dgiot_uav_plc_tcp_client:write(1700, 51, 100).
```

#### 连续测试API

```erlang
%% 启动七步校验连续测试
dgiot_uav_plc_tcp_client:start_continuous_test(1700, [100, 200, 300]).

%% 查看测试状态
dgiot_uav_plc_tcp_client:get_continuous_test_status(1700).

%% 停止连续测试
dgiot_uav_plc_tcp_client:stop_continuous_test(1700).
```

### 测试命令

```bash
# 查看PLC客户端注册状态
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_plc_utils:test_client().'

# 启动七步校验测试（磁航向工位）
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_plc_tcp_client:start_continuous_test(1700, [100]).'

# 执行演示函数（查看日志格式）
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_plc_tcp_client:test_7step().'

# 演示脚本
./demo_plc_7step_log.sh
```

### Modbus协议说明

#### Modbus TCP帧格式

```
| 字段              | 长度 | 说明                        |
|-------------------|------|-----------------------------|
| Transaction ID    | 2字节| 事务标识符                  |
| Protocol ID       | 2字节| 协议标识符（0x0000）        |
| Length            | 2字节| 后续字节数                  |
| Slave ID          | 1字节| 从站地址                    |
| Function Code     | 1字节| 功能码                      |
| Data              | N字节| 数据域                      |
```

#### 常用功能码

| 功能码 | 名称                      | 说明             |
|--------|---------------------------|------------------|
| 0x03   | Read Holding Registers    | 读保持寄存器     |
| 0x06   | Write Single Register     | 写单个寄存器     |
| 0x10   | Write Multiple Registers  | 写多个寄存器     |

### 代码实现

**文件位置**: `apps/dgiot_uav/src/channel/dgiot_uav_plc_tcp_client.erl`

**核心函数**:

```erlang
%% @doc 处理七步校验流程
handle_step(StepId, ChildState, Dclient) ->
    %% Step 1: 开始标志
    %% Step 2-6: 执行具体操作
    %% Step 7: 完成标志
    
    case StepId of
        1 -> read(StationId, 0, 1);
        2 -> write(StationId, 51, CurrentCode);
        3 -> read(StationId, 10, 1);
        4 -> write(StationId, 0, 0);
        5 -> write(StationId, 10, 0);
        6 -> write(StationId, 60, CurrentCode);
        7 -> write(StationId, 61, 1)
    end.

%% @doc 获取步骤描述
get_step_description(1, _Code) -> <<"读取工位就绪状态"/utf8>>;
get_step_description(2, Code) -> io_lib:format(<<"写入测试命令码 ~p"/utf8>>, [Code]);
%% ... 其他步骤
```

### 相关文档

- **日志示例**: `/root/gitee/dgiot/test_plc_7step_log.md`
- **演示脚本**: `/root/gitee/dgiot/demo_plc_7step_log.sh`
- **PLC工具函数**: `apps/dgiot_uav/src/business/plc/dgiot_uav_plc_utils.erl`
- **步骤执行器**: `apps/dgiot_uav/src/business/plc/dgiot_uav_plc_step_executor.erl`

### 故障排除

#### PLC客户端未注册

```bash
# 检查PLC进程
_build/emqx/rel/emqx/bin/emqx eval 'global:whereis_name({plc, 1700}).'

# 如果返回undefined，检查通道配置
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_plc_utils:test_client().'
```

#### 地址计算错误

```erlang
%% 错误示例：传入绝对地址作为相对地址
read(1700, 1730, 60).  %% ❌ 错误：1730是绝对地址，会计算为D3430

%% 正确做法：使用高级API或相对地址
read_alarms(1700).     %% ✅ 正确：自动使用相对地址30
read(1700, 30, 60).    %% ✅ 正确：显式使用相对地址30
```

#### Modbus响应超时

1. 检查PLC设备连接状态
2. 检查网络连通性
3. 查看Erlang日志错误信息

## 总结

✅ **完整流程**:
1. 设备上线并绑定工位
2. 通过工位查询测试项
3. 自动下发测试指令
4. 数据汇聚到td子表
5. 使用Word模板生成报告
6. MES下载报告

✅ **磁航向工位特色**:
- PLC七步校验流程
- Modbus TCP通信
- 详细的报文日志
- 自动地址计算
- 高级API封装

✅ **数据来源**:
- 测试结果: Device.content.test_result
- 测试项: 通过工位查询Device表
- 测试过程: td子表
- 遥测数据: TDengine
- PLC状态: Modbus寄存器

✅ **报告生成**:
- Word模板变量替换
- 按设备ID创建文件夹
- 时间戳作为文件名
- 支持PDF转换

完整的自动化测试和报告生成流程已实现!
