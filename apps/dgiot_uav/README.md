# dgiot_uav - 无人机测试系统插件

## 概述

`dgiot_uav` 是基于DG-IoT平台的无人机自动化测试系统插件，专为981A无人机测试流程设计。本插件实现了完整的测试流程管理、硬件控制、数据采集和报告生成功能。

## 功能特性

### 1. 测试流程管理
- **12个标准测试项目**：覆盖从机臂静态测试到整机联调的全流程
- **自动化测试执行**：支持扫码开始、自动测试、报告生成
- **实时状态监控**：测试进度、设备状态、数据采集实时显示

### 2. 硬件控制
- **程控电源管理**：Modbus/TCP协议控制电源输出
- **无人机通信**：UAV981A协议与地面站通信
- **扫码设备集成**：二维码扫码识别无人机编号
- **安灯系统控制**：测试状态可视化指示

### 3. 协议支持
- **Modbus协议**：用于程控电源控制
- **UAV981A协议**：用于无人机地面站通信
- **自定义协议**：支持981A测试系统专用协议

### 4. Web界面
- **测试流程可视化**：实时显示测试进度
- **数据监控面板**：电压、电流、信号强度等参数实时显示
- **测试报告查看**：PDF报告在线预览和下载
- **设备状态管理**：设备连接状态、故障报警

## 系统架构

### 模块结构
```
apps/dgiot_uav/
├── src/
│   ├── dgiot_uav_app.erl          # 应用启动模块
│   ├── dgiot_uav_sup.erl          # 监控树
│   ├── dgiot_uav_http.erl         # Cowboy HTTP服务器
│   ├── dgiot_uav_handler.erl      # REST API处理器
│   ├── dgiot_uav_ws_handler.erl   # WebSocket处理器
│   ├── dgiot_uav_test_service.erl # 测试逻辑服务
│   ├── dgiot_uav_protocol.erl     # 协议解析模块
│   └── dgiot_uav_power_controller.erl # 电源控制器
├── include/
│   └── dgiot_uav.hrl              # 头文件定义
├── priv/
│   ├── www/                       # Web界面文件
│   │   └── index.html             # 主界面
│   ├── doc/                       # 文档资料
│   │   ├── 981A测试系统细化实施方案-批注版20230610.docx
│   │   ├── 需求分析报告.md
│   │   └── 需求规格说明书.md
│   └── UAVAssessor/               # 测试评估工具
└── test/                          # 测试代码
```

### 通信架构
```
Web浏览器 → HTTP/WebSocket → dgiot_uav_http
    ↓                              ↓
用户交互                    dgiot_uav_handler
                                ↓
                        dgiot_uav_test_service
                    ↓               ↓               ↓
            dgiot_uav_protocol  硬件控制接口  数据存储服务
                ↓           ↓           ↓
           协议解析    电源控制    扫码设备
```

## 安装和使用

### 1. 环境要求
- Erlang/OTP 23+
- DG-IoT 4.3+
- Cowboy 2.9+
- Python 3.8+ (用于文档解析)

### 2. 编译安装
```bash
# 进入项目根目录
cd /root/gitee/dgiot

# 编译插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_uav).'

# 加载插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_uav).'
```

### 3. 启动服务
```bash
# 启动HTTP服务（端口8088）
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_http:start_http().'

# 访问Web界面
# 打开浏览器访问：http://localhost:8088
```

### 4. 测试验证
```bash
# 运行单元测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_test_service:test().'

# 检查服务状态
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_http:status().'
```

## API接口

### REST API
- `GET /` - Web界面
- `GET /api/uav/status` - 获取系统状态
- `POST /api/uav/test/start` - 开始测试
- `POST /api/uav/test/stop` - 停止测试
- `GET /api/uav/test/results` - 获取测试结果
- `POST /api/uav/power/on` - 电源上电
- `POST /api/uav/power/off` - 电源断电
- `POST /api/uav/power/set-voltage` - 设置电压

### WebSocket接口
- `ws://localhost:8088/ws/uav/data` - 实时数据推送
- 推送内容：电源状态、测试进度、设备数据

## 测试流程

### 标准测试项目
1. **机臂静态测试** - 机臂检测工位
2. **机身静态测试前检查** - 机身总装工位
3. **机臂及螺旋桨安装情况检查** - 机身总装工位
4. **链路功能检查** - 机身总装工位
5. **上电参数检查** - 机身总装工位
6. **夜航灯测试** - 机身总装工位
7. **系统电磁兼容性功能检查** - 机身总装工位
8. **航线加载及载荷功能检查** - 机身总装工位
9. **加速度和姿态角功能测试** - 机械臂测试
10. **机臂桨面水平度测试和动态旋转测试** - 动态测试

### 测试步骤
```
扫码开始 → 准备测试 → 执行测试项 → 数据采集 → 生成报告 → 扫码结束
    ↓          ↓           ↓           ↓           ↓          ↓
 设备识别   硬件初始化   协议通信   实时监控   PDF生成  结果上传
```

## 协议规范

### 981A协议格式
```
同步头: 0xCA 0xA9 (2字节)
MSGID: 协议编号 (1字节)
PacketSize: 协议长度 (2字节)
Data: 协议数据 (变长)
CK_A, CK_B: 校验和 (2字节)
```

### Modbus协议
- **功能码 03**: 读保持寄存器
- **功能码 06**: 写单个寄存器
- **功能码 16#10**: 写多个寄存器

### 寄存器映射
- `0x1000`: 电压设置 (0.01V单位)
- `0x1001`: 电流限制 (0.01A单位)
- `0x1002`: 输出控制 (0=关, 1=开)
- `0x1100`: 实际电压读取
- `0x1101`: 实际电流读取

## 配置说明

### 电源配置
```erlang
% 在配置文件中设置
{uav_power, [
    {ip, "192.168.1.100"},
    {port, 502},
    {unit_id, 1},
    {max_voltage, 5000},  % 50.00V
    {max_current, 2000}   % 20.00A
]}.
```

### 测试参数
```erlang
% 测试项目配置
{uav_tests, [
    {test_timeout, 300000},  % 5分钟超时
    {retry_count, 3},        % 重试次数
    {report_path, "/var/log/uav/reports"}  % 报告存储路径
]}.
```

## 故障排除

### 常见问题

1. **HTTP服务无法启动**
   ```bash
   # 检查端口占用
   netstat -tlnp | grep 8088
   
   # 检查Cowboy依赖
   _build/emqx/rel/emqx/bin/emqx eval 'application:which_applications().'
   ```

2. **电源控制失败**
   ```bash
   # 检查Modbus连接
   telnet 192.168.1.100 502
   
   # 查看电源控制器日志
   _build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_power_controller:get_status(Pid).'
   ```

3. **协议解析错误**
   ```erlang
   % 调试协议解析
   dgiot_uav_protocol:parse_packet(<<16#CA, 16#A9, 1, 10, 1,2,3,4,5,6,7,8,9,10>>).
   ```

### 日志查看
```bash
# 查看插件日志
tail -f logs/uav.log

# 查看HTTP访问日志
tail -f logs/cowboy.log

# 查看错误日志
tail -f logs/error.log
```

## 开发指南

### 添加新测试项目
1. 在 `dgiot_uav_test_service.erl` 中添加测试函数
2. 在Web界面中添加测试项显示
3. 更新测试流程配置

### 扩展协议支持
1. 在 `dgiot_uav_protocol.erl` 中添加协议解析函数
2. 实现 `parse_packet/1` 和 `encode_packet/1` 接口
3. 添加协议测试用例

### 集成新硬件
1. 创建硬件控制器模块
2. 实现标准硬件接口
3. 集成到测试服务中

## 性能指标

### 系统性能
- **并发测试**: 支持多台无人机同时测试
- **响应时间**: API响应 < 100ms
- **数据采集**: 实时数据更新频率 1Hz
- **报告生成**: PDF报告生成 < 10秒

### 资源占用
- **内存使用**: < 50MB
- **CPU占用**: < 5% (空闲状态)
- **网络带宽**: < 1Mbps (正常测试)

## 版本历史

### v1.0.0 (2025-12-23)
- 初始版本发布
- 实现12个标准测试项目
- 支持Modbus电源控制
- 提供Web管理界面
- 集成981A协议解析

### v0.9.0 (2025-12-19)
- 基础框架搭建
- HTTP服务实现
- 测试服务原型
- 协议框架设计

## 贡献指南

1. Fork项目仓库
2. 创建功能分支 (`git checkout -b feature/新功能`)
3. 提交更改 (`git commit -am '添加新功能'`)
4. 推送到分支 (`git push origin feature/新功能`)
5. 创建Pull Request

## 许可证

本项目基于 Apache License 2.0 许可证发布，详情请参阅 [LICENSE](LICENSE) 文件。

## 联系方式

- **项目主页**: https://gitee.com/dgiiot/dgiot
- **问题反馈**: https://gitee.com/dgiiot/dgiot/issues
- **文档地址**: https://dgiot.readthedocs.io/

## 致谢

感谢981A测试系统项目组提供的详细需求文档和技术支持。

---

**提示**: 更多详细文档请查看 `priv/doc/` 目录下的需求分析报告和规格说明书。

## 数据存储

### 存储架构
`dgiot_uav` 插件使用DG-IoT的Parse Server作为数据存储后端，通过 `dgiot_parse` API实现数据的持久化存储。

### 数据表结构

#### 1. UAVTestRecord (测试记录表)
```erlang
#{
    <<"deviceId">> => DeviceId,      % 设备ID
    <<"testType">> => TestType,      % 测试类型
    <<"status">> => Status,          % 测试状态: running/completed/failed
    <<"startTime">> => StartTime,    % 开始时间
    <<"endTime">> => EndTime,        % 结束时间
    <<"params">> => Params,          % 测试参数
    <<"result">> => Result,          % 测试结果
    <<"details">> => Details         % 详细数据
}
```

#### 2. UAVPowerLog (电源日志表)
```erlang
#{
    <<"deviceId">> => DeviceId,      % 设备ID
    <<"voltage">> => Voltage,        % 电压值
    <<"current">> => Current,        % 电流值
    <<"timestamp">> => Timestamp     % 时间戳
}
```

#### 3. UAVProtocolLog (协议日志表)
```erlang
#{
    <<"deviceId">> => DeviceId,      % 设备ID
    <<"protocolType">> => Type,      % 协议类型: modbus/uav981a
    <<"data">> => Data,              % 协议数据
    <<"direction">> => Direction,    % 方向: send/receive
    <<"timestamp">> => Timestamp     % 时间戳
}
```

#### 4. UAVDeviceStatus (设备状态表)
```erlang
#{
    <<"deviceId">> => DeviceId,      % 设备ID
    <<"status">> => Status,          % 状态: idle/testing/error
    <<"details">> => Details,        % 状态详情
    <<"timestamp">> => Timestamp     % 时间戳
}
```

#### 5. UAVTestReport (测试报告表)
```erlang
#{
    <<"deviceId">> => DeviceId,      % 设备ID
    <<"testType">> => TestType,      % 测试类型
    <<"result">> => Result,          % 测试结果: passed/failed
    <<"reportData">> => ReportData,  % 报告数据
    <<"generatedAt">> => GeneratedAt % 生成时间
}
```

### API使用示例

#### 创建测试记录
```erlang
% 开始测试
{ok, RecordId} = dgiot_uav_test_service:start_test(DeviceId, <<"static_test">>, <<"operator1">>).

% 更新测试步骤
ok = dgiot_uav_test_service:update_test_step(RecordId, 1, <<"电源检查">>, #{<<"voltage">> => 24.5}).

% 停止测试
ok = dgiot_uav_test_service:stop_test(RecordId, #{<<"result">> => <<"passed">>, <<"score">> => 95}).
```

#### 存储电源数据
```erlang
% 保存电源数据
ok = dgiot_uav_test_service:save_power_data(DeviceId, 24.5, 2.3).

% 保存协议数据
ok = dgiot_uav_test_service:save_protocol_data(
    DeviceId, 
    <<"modbus">>, 
    <<"010300000002C40B">>, 
    <<"send">>
).
```

#### 查询测试数据
```erlang
% 获取测试状态
{ok, Status} = dgiot_uav_test_service:get_test_status(RecordId).

% 获取测试结果
{ok, Results} = dgiot_uav_test_service:get_test_results(DeviceId, 10).

% 生成测试报告
{ok, ReportId} = dgiot_uav_test_service:generate_test_report(
    DeviceId, 
    <<"static_test">>, 
    <<"passed">>
).
```

### 数据服务模块

#### dgiot_uav_data_service.erl
核心数据存储服务，提供与Parse Server的交互接口：
- `create_test_record/3` - 创建测试记录
- `update_test_result/4` - 更新测试结果
- `get_test_records/2` - 查询测试记录
- `create_power_log/3` - 创建电源日志
- `create_protocol_log/4` - 创建协议日志
- `create_device_status/3` - 创建设备状态
- `create_test_report/4` - 创建测试报告

#### dgiot_uav_test_service.erl
测试逻辑服务，集成数据存储功能：
- `start_test/3` - 开始测试流程
- `stop_test/2` - 停止测试流程
- `update_test_step/4` - 更新测试步骤
- `get_test_status/1` - 获取测试状态
- `get_test_results/2` - 获取测试结果
- `save_power_data/3` - 保存电源数据
- `save_protocol_data/4` - 保存协议数据
- `generate_test_report/3` - 生成测试报告

### 数据一致性保证

#### 1. 事务处理
- 使用Parse Server的原子操作保证数据一致性
- 测试状态与设备状态同步更新
- 错误处理和数据回滚机制

#### 2. 数据验证
- 输入参数类型检查
- 数据范围验证
- 业务逻辑验证

#### 3. 错误处理
- 详细的错误日志记录
- 错误恢复机制
- 数据完整性检查

### 性能优化

#### 1. 批量操作
```erlang
% 批量创建测试记录
BatchRequests = [
    #{<<"method">> => <<"POST">>, <<"path">> => <<"/classes/UAVTestRecord">>, <<"body">> => Record1},
    #{<<"method">> => <<"POST">>, <<"path">> => <<"/classes/UAVTestRecord">>, <<"body">> => Record2}
],
dgiot_parse:batch(BatchRequests).
```

#### 2. 查询优化
- 使用索引优化查询性能
- 分页查询避免大数据量
- 缓存常用查询结果

#### 3. 数据压缩
- 二进制数据压缩存储
- JSON数据优化
- 历史数据归档

### 数据安全

#### 1. 访问控制
- Parse Server的ACL权限控制
- 用户角色权限管理
- 数据访问审计

#### 2. 数据加密
- 敏感数据加密存储
- 传输数据SSL加密
- API访问令牌验证

#### 3. 备份恢复
- 定期数据备份
- 数据恢复机制
- 灾难恢复计划

### 监控和调试

#### 1. 数据监控
```bash
# 查看数据表统计
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_parse:query_object(<<"UAVTestRecord">>, #{<<"count">> => 1}).'

# 查看最近测试记录
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_uav_test_service:get_test_results(<<"device001">>, 5).'
```

#### 2. 性能监控
- 数据操作响应时间
- 存储空间使用情况
- 并发连接数监控

#### 3. 故障诊断
- 数据操作日志
- 错误追踪
- 性能瓶颈分析
