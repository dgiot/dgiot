# Modbus插件完整测试用例设计

## 基于代码分析的关键信息：

### 1. Modbus通道类型
- **通道类型**：`<<"MODBUS">>` (定义在dgiot_modbus_channel.erl)
- **协议类型**：`?PROTOCOL_CHL` (协议采集通道)

### 2. 设备注册方式（3种）
1. **RegisterByIp** - 通过IP地址注册
2. **RegisterByPort** - 通过服务器端口注册（主要方式）
3. **RegisterByRegular** - 常规注册方式

### 3. 创建和启停流程
- **通道创建**：通过`dgiot_channelx:add/4`函数
- **通道启动**：`init/3`函数初始化通道
- **通道停止**：`stop/3`函数停止通道

### 4. 产品、物模型和设备创建
- **产品创建**：通过产品管理API
- **物模型定义**：包含属性、服务、事件
- **设备创建**：基于产品和设备地址

### 5. 数据查询方式
- **实时数据**：通过`last_data`缓存查询
- **时序数据**：通过TDengine数据库查询
- **API接口**：通过HTTP API查询

## 需要添加的测试用例：

### 一、通道管理测试用例
1. **channel_creation** - 通道创建测试
2. **channel_start_stop** - 通道启停测试
3. **channel_config** - 通道配置测试

### 二、注册方式测试用例
4. **register_by_ip** - RegisterByIp注册测试
5. **register_by_port** - RegisterByPort注册测试（已有）
6. **register_by_regular** - RegisterByRegular注册测试

### 三、产品管理测试用例
7. **product_creation** - 产品创建测试
8. **thing_model** - 物模型定义测试
9. **device_creation** - 设备创建测试

### 四、数据查询测试用例
10. **realtime_data** - 实时数据查询测试
11. **historical_data** - 历史数据查询测试
12. **tdengine_query** - TDengine数据查询测试

### 五、完整流程测试用例
13. **end_to_end_flow** - 端到端完整流程测试
14. **error_recovery** - 错误恢复测试
15. **performance_test** - 性能测试

## 总计：15个新的测试用例
