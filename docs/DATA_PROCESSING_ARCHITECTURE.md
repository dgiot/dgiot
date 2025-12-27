# DG-IoT数据处理架构完整指南

## 概述

本文档整合了DG-IoT平台数据处理的核心架构、设计原理和最佳实践，基于前面深入的分析讨论形成完整的文档体系。

## 1. 核心架构设计

### 1.1 七层数据处理架构

```
设备层 → 协议层 → 消息路由层 → 业务层 → 数据层 → 缓存层 → API层
```

#### 各层职责：
1. **设备层**：原始数据采集，不进行任何解析
2. **协议层**：协议解析和封装，注册钩子供业务层调用
3. **消息路由层**：MQTT消息路由，任务队列管理
4. **业务层**：数据解码、属性计算、告警处理（dgiot_task）
5. **数据层**：时序数据存储（TDengine）
6. **缓存层**：实时数据缓存，设备状态缓存
7. **API层**：实时数据查询，历史数据查询

### 1.2 架构设计原则

#### 分层解耦，各安其位，各司其职
- 每层有明确的职责边界
- 层间通过标准接口通信
- 避免跨层直接调用

## 2. 关键技术组件

### 2.1 数据块（Block Data）设计

#### 设计理念
数据块是"一次读取，多次使用"的自然实现，具有以下特性：
- **天生池子**：本身就是最优的缓冲池实现
- **时间一致性**：单次读取自然保证所有数据时间一致
- **结构化高效**：二进制结构是最紧凑高效的组织方式

#### 应用场景
- 单设备多传感器数据采集
- 寄存器批量读取
- 高性能实时数据采集

### 2.2 缓冲池（Buffer Pool）设计

#### 设计理念
缓冲池解决多源数据汇合时的时间戳不一致问题：

#### 核心功能
1. **时间对齐**：将不同时间戳的数据对齐到统一时间窗口
2. **数据完整性**：保证多源数据的完整性和一致性
3. **计算准确性**：确保物理计算和业务分析的准确性

#### 应用场景
- 多设备数据聚合
- 跨系统数据集成
- 复杂计算依赖多源数据

### 2.3 公式计算系统

#### 决策机制
dgiot_task通过解析物模型中的`strategy`字段决定是否需要计算：

```json
{
  "identifier": "actual_temperature",
  "dataForm": {
    "strategy": "计算值",  // 需要计算
    "collection": "%%{raw_temperature} * 0.0625"
  }
}
```

#### 计算流程
```
物模型配置 → dgiot_task解析 → 计算决策 → 执行计算 → 结果存储
```

## 3. 数据处理流程

### 3.1 单设备数据处理流程

```
设备数据 → 协议解析 → 数据块提取 → 公式计算 → 存储 → 缓存 → API查询
```

#### 关键步骤：
1. **协议解析**：通过钩子机制调用协议层解析原始数据
2. **数据块处理**：从二进制数据块中提取各个属性值
3. **公式计算**：根据物模型配置执行计算公式
4. **数据存储**：保存到TDengine和缓存

### 3.2 多设备数据融合流程

```
多设备数据 → 缓冲池时间对齐 → 批量处理 → 聚合计算 → 统一存储 → 聚合查询
```

#### 关键步骤：
1. **时间对齐**：缓冲池将不同时间戳的数据对齐
2. **批量处理**：处理完整的数据集
3. **聚合计算**：执行跨设备的聚合计算
4. **统一存储**：使用统一时间戳存储

## 4. 开发指南

### 4.1 设备通道开发

#### 基本原则
- 只负责数据采集，不进行业务计算
- 遵循七层架构职责划分
- 通过标准接口与业务层通信

#### 代码模板
```erlang
handle_device_data(ChannelId, ProductId, DevAddr, RawData) ->
    %% 1. 转发原始数据到任务通道
    Things = #{
        <<"raw_data">> => RawData,
        <<"data_type">> => <<"modbus_rtu">>,
        <<"product_id">> => ProductId,
        <<"dtu_addr">> => DevAddr
    },
    
    %% 2. 发送到dgiot_task（不决定计算策略）
    dgiot_client:send(TaskChannel, DeviceId, Topic, Things).
```

### 4.2 物模型配置指南

#### 计算值属性配置
```json
{
  "identifier": "angular_x",
  "dataForm": {
    "strategy": "计算值",
    "collection": "block_data[0:2] * 0.1",
    "protocol": "MODBUSRTU"
  },
  "dataSource": {
    "slaveid": "0X01",
    "address": "0X0000",
    "key": "block_data"
  }
}
```

#### 上报值属性配置
```json
{
  "identifier": "block_data",
  "dataForm": {
    "strategy": "上报值",
    "protocol": "MODBUSRTU"
  },
  "dataSource": {
    "slaveid": "0X01",
    "address": "0X0000"
  }
}
```

### 4.3 dgiot_task集成指南

#### 核心函数
```erlang
%% 数据保存入口
save_td(ProductId, DevAddr, Ack, AppData) ->
    dgiot_task_service:save_td(ProductId, DevAddr, Ack, AppData).

%% 智能数据保存
smart_save_td(ProductId, DevAddr, Data, Context) ->
    dgiot_task_service:smart_save_td(ProductId, DevAddr, Data, Context).

%% 获取计算值
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    dgiot_task_service:get_calculated(ProductId, DevAddr, Calculated, Props).
```

## 5. 测试验证体系

### 5.1 测试类型

#### 单元测试
- 测试单个函数的正确性
- 覆盖边界条件和错误场景
- 快速反馈开发问题

#### 集成测试
- 测试模块间的协作
- 验证数据流完整性
- 模拟真实使用场景

#### 端到端测试
- 从设备上报到API查询的完整流程
- 验证系统整体功能
- 性能基准测试

### 5.2 测试设备类型

1. **Modbus RTU设备**：角度传感器（数据块模式）
2. **Modbus TCP设备**：温度传感器（单点读取）
3. **第三方协议设备**：DLINK协议转换
4. **多源数据设备**：缓冲池测试场景

## 6. API数据查询

### 6.1 实时数据查询

```bash
# 查询设备实时数据
curl -X GET "http://127.0.0.1/iotapi/devicecard/{deviceId}" \
  -H "Authorization: Bearer {token}"

# 响应格式
{
  "code": 200,
  "data": {
    "deviceId": "device_123",
    "properties": {
      "angular_x": 25.5,
      "angular_y": 30.2,
      "temperature": 28.3
    },
    "timestamp": 1672531200000
  }
}
```

### 6.2 历史数据查询

```bash
# 查询设备历史数据
curl -X GET "http://127.0.0.1/iotapi/device/{deviceId}/history?start=...&end=..." \
  -H "Authorization: Bearer {token}"
```

### 6.3 聚合数据查询

```bash
# 查询多设备聚合数据
curl -X POST "http://127.0.0.1/iotapi/data/aggregate" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer {token}" \
  -d '{
    "deviceIds": ["device1", "device2", "device3"],
    "property": "temperature",
    "aggregation": "avg",
    "interval": "1h"
  }'
```

## 7. 性能优化指南

### 7.1 数据块优化

#### 优势
- **减少I/O操作**：一次读取多个寄存器
- **提高缓存命中率**：连续内存布局
- **降低CPU占用**：减少协议解析次数

#### 最佳实践
- 将相关寄存器配置为数据块
- 合理设置数据块大小
- 使用偏移量提取子属性

### 7.2 缓冲池优化

#### 优势
- **时间一致性**：保证多源数据时间对齐
- **计算准确性**：提供正确的物理计算基础
- **资源优化**：减少重复计算和存储

#### 最佳实践
- 根据业务需求设置时间窗口
- 合理配置等待超时时间
- 监控缓冲池状态和性能

### 7.3 缓存优化

#### 优势
- **快速响应**：内存访问速度快
- **降低数据库压力**：减少重复查询
- **提高系统稳定性**：缓存层保护后端系统

#### 最佳实践
- 合理设置缓存过期时间
- 监控缓存命中率
- 实现缓存预热机制

## 8. 故障排除指南

### 8.1 常见问题

#### 问题1：数据块属性未解析
**症状**：只解析出基础属性，计算值属性为空
**原因**：物模型配置错误或计算公式错误
**解决方案**：检查物模型配置，验证计算公式语法

#### 问题2：多设备数据时间不一致
**症状**：聚合计算结果不准确
**原因**：设备时钟不同步或网络延迟
**解决方案**：使用缓冲池进行时间对齐

#### 问题3：API查询返回空值
**症状**：API响应正常但数据为空
**原因**：缓存键不匹配或数据未正确存储
**解决方案**：检查缓存键和数据存储逻辑

### 8.2 调试工具

#### 日志查看
```bash
# 查看实时日志
tail -f logs/console.log | grep -E "(ERROR|WARNING|DEBUG.*modbus)"

# 查看特定设备日志
tail -f logs/console.log | grep "device_123"
```

#### 缓存检查
```erlang
%% 检查缓存数据
_build/emqx/rel/emqx/bin/emqx eval '
DeviceId = <<"device_123">>,
case dgiot_data:get({last_data, DeviceId}) of
    not_find -> io:format("last_data缓存空~n");
    Data -> io:format("last_data缓存: ~p~n", [Data])
end.'
```

#### 数据库检查
```erlang
%% 检查TDengine数据
_build/emqx/rel/emqx/bin/emqx eval '
ProductId = <<"product_123">>,
case dgiot_tdengine_adapter:query(ProductId, <<"SELECT * FROM table LIMIT 1">>) of
    {ok, Data} -> io:format("TDengine数据: ~p~n", [Data]);
    {error, Reason} -> io:format("TDengine查询错误: ~p~n", [Reason])
end.'
```

## 9. 更新记录

### 版本历史
- **v1.0 (2025-12-25)**：创建完整的数据处理架构文档
  - 整合七层架构设计
  - 详细说明数据块和缓冲池设计
  - 提供完整的开发指南和测试验证体系

### 维护指南
1. **定期更新**：根据架构演进更新文档
2. **团队培训**：新成员必须阅读本文档
3. **问题反馈**：通过issue系统反馈文档问题

## 10. 相关文档

### 技术专题文档
- [公式计算系统详解](./formula_calculation_system.md)
- [缓冲池设计原理](./buffer_pool_design.md)
- [多源数据融合指南](./multi_source_data_merging.md)

### 开发指南文档
- [设备通道开发指南](./device_channel_development.md)
- [dgiot_task集成指南](./task_module_integration.md)
- [API数据查询指南](./api_data_query.md)

### 测试验证文档
- [单元测试编写指南](./unit_testing_guide.md)
- [集成测试实施指南](./integration_testing_guide.md)
- [端到端测试验证指南](./e2e_testing_guide.md)

---

**提示**：本文档是DG-IoT平台数据处理的核心参考，建议开发团队定期阅读和更新，确保架构理解的一致性。
