---
name: dgiot_sensor_data_workflow
description: DGIOT传感器数据上报到前端展示完整工作流技能，提供七层数据流分析、逐层检查清单、常见问题解决方案，支持快速定位和解决问题
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-26
category: workflow
tags: [dgiot, sensor, data, workflow, troubleshooting, debugging, analysis, monitoring]
trigger_phrases:
  - 传感器数据
  - 数据流调试
  - 前端无数据
  - 数据上报问题
  - 传感器数据流
  - 数据展示问题
  - 实时数据缺失
  - 数据流分析
---

# DGIOT传感器数据工作流技能

## 概述

传感器数据从设备采集到前端展示的完整工作流程技能，提供快速定位和解决问题的标准方法。支持七层数据流分析、逐层检查清单和常见问题解决方案。

## 1. 数据流架构

### 1.1 七层数据流
```
设备 → 协议解析 → 任务队列 → 业务处理 → 数据存储 → 缓存 → 前端展示
```

### 1.2 各层关键组件
| 层级 | 组件 | 关键日志 |
|------|------|----------|
| 协议层 | modbus_rtu.erl | parse_frame, decode_data |
| 消息层 | dgiot_modbusrtu_tcp.erl | Received data, Sending to Task |
| 业务层 | dgiot_task_worker.erl | save_td, dealwith_data |
| 存储层 | TDengine | 保存日志 |
| 缓存层 | dgiot_data | 缓存更新日志 |
| 展示层 | 前端API | API调用日志 |

## 2. 调试工作流

### 2.1 问题诊断流程
```
前端无数据 → API响应 → 缓存数据 → TDengine存储 → 任务队列 → 协议解析 → 设备连接
```

### 2.2 逐层检查清单

#### 前端展示层
- [ ] API响应状态码200？
- [ ] 数据格式正确？
- [ ] 前端解析正常？

#### API层
- [ ] 认证token有效？
- [ ] 缓存查询正确？
- [ ] 错误处理完整？

#### 缓存层
```bash
_build/emqx/rel/emqx/bin/emqx eval '
DeviceId = <<"88a27d8587">>,
case dgiot_data:get({last_data, DeviceId}) of
    not_find -> io:format("last_data缓存空~n");
    Data -> io:format("last_data缓存: ~p~n", [Data])
end.'
```

#### 存储层
```bash
_build/emqx/rel/emqx/bin/emqx eval '
ProductId = <<"feeb43bffb">>,
case dgiot_tdengine_adapter:query(ProductId, <<"SELECT * FROM table LIMIT 1">>) of
    {ok, Data} -> io:format("TDengine数据: ~p~n", [Data]);
    {error, Reason} -> io:format("TDengine查询错误: ~p~n", [Reason])
end.'
```

#### 任务队列层
```bash
tail -f logs/console.log | grep -E "(save_td|dealwith_data|task_save)"
```

#### 协议解析层
```bash
tail -f logs/console.log | grep -E "(parse_frame|decode_data|Received data)"
```

#### 设备连接层
- [ ] 设备在线？
- [ ] 网络正常？
- [ ] 配置正确？

## 3. 常见问题解决方案

### 3.1 实时卡片显示空值
**症状**：API返回数据但value字段为空

**原因**：缓存键不匹配（last_data vs ?DGIOT_DATA_CACHE）

**解决方案**：
```erlang
% 在save_to_tdengine中更新两个缓存
save_to_tdengine(ProductId, DevAddr, Data) ->
    % ... 原有代码 ...
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, NewDevAddr),
    dgiot_data:put({last_data, DeviceId}, Data),
    dgiot_data:insert(?DGIOT_DATA_CACHE, DeviceId, {Data, dgiot_datetime:now_ms()}).
```

### 3.2 数据未到任务队列
**症状**：协议解析正常，任务队列无数据

**解决方案**：
```erlang
case dgiot_client:send(Taskchannel, DeviceId, ChildTopic, EnrichedThings) of
    ok -> ?LOG(info, "发送成功");
    {error, Reason} -> ?LOG(error, "发送失败: ~p", [Reason])
end.
```

### 3.3 计算值属性未解析
**症状**：只解析基础属性，计算值属性为空

**解决方案**：
1. 检查属性配置strategy字段
2. 验证基础属性已正确解析
3. 检查计算公式语法

## 4. 调试工具

### 4.1 数据流检查脚本
```bash
#!/bin/bash
# check_sensor_data_flow.sh

echo "=== 传感器数据流检查脚本 ==="
echo ""

DEVICE_ID="${1:-88a27d8587}"
PRODUCT_ID="${2:-feeb43bffb}"

echo "设备ID: $DEVICE_ID"
echo "产品ID: $PRODUCT_ID"
echo ""

# 1. 检查前端API
echo "1. 检查前端API..."
curl -s -X GET "http://127.0.0.1/iotapi/devicecard/$DEVICE_ID" \
  -H "Authorization: Bearer r:db1f3d43d05c782c8ceebb87724a2ac0" \
  -H "Content-Type: application/json" | jq '.data.value' | head -5

# 2. 检查缓存
echo "2. 检查缓存..."
_build/emqx/rel/emqx/bin/emqx eval "
DeviceId = <<\"$DEVICE_ID\">>,
case dgiot_data:get({last_data, DeviceId}) of
    not_find -> io:format(\"last_data缓存空~n\");
    Data -> io:format(\"last_data缓存: ~p~n\", [Data])
end.
"

# 3. 检查TDengine存储
echo "3. 检查TDengine存储..."
_build/emqx/rel/emqx/bin/emqx eval "
ProductId = <<\"$PRODUCT_ID\">>,
case dgiot_tdengine_adapter:query(ProductId, <<\"SELECT * FROM table LIMIT 1\">>) of
    {ok, Data} -> io:format(\"TDengine数据: ~p~n\", [Data]);
    {error, Reason} -> io:format(\"TDengine查询错误: ~p~n\", [Reason])
end.
"

# 4. 检查任务队列日志
echo "4. 检查任务队列日志..."
grep -E "(save_td|dealwith_data|task_save)" logs/console.log | tail -3

# 5. 检查协议解析日志
echo "5. 检查协议解析日志..."
grep -E "(parse_frame|decode_data|Received data)" logs/console.log | tail -3

echo ""
echo "=== 检查完成 ==="
```

### 4.2 实时数据监控脚本
```bash
#!/bin/bash
# monitor_sensor_data.sh

echo "=== 传感器数据实时监控 ==="
echo "按Ctrl+C停止监控"
echo ""

DEVICE_ID="${1:-88a27d8587}"
INTERVAL="${2:-5}"

while true; do
    echo "时间: $(date '+%Y-%m-%d %H:%M:%S')"
    
    # 检查缓存数据
    _build/emqx/rel/emqx/bin/emqx eval "
        DeviceId = <<\"$DEVICE_ID\">>,
        case dgiot_data:get({last_data, DeviceId}) of
            not_find -> io:format(\"缓存: 空~n\");
            Data -> 
                io:format(\"缓存: 有数据~n\"),
                case maps:get(<<"value">>, Data, undefined) of
                    undefined -> io:format(\"  值字段: 空~n\");
                    Value -> io:format(\"  值字段: ~p~n\", [Value])
                end
        end.
    "
    
    # 检查最新日志
    echo "最新日志:"
    tail -n 2 logs/console.log | grep -E "(ERROR|WARNING|INFO.*$DEVICE_ID)" || echo "  无相关日志"
    
    echo "----------------------------------------"
    sleep $INTERVAL
done
```

## 5. 技能集成

### 5.1 与协议调试技能集成
```
dgiot_protocol_debug 激活
    ↓
[dgiot_sensor_data_workflow] 传感器数据工作流技能
    ↓
分析数据流问题
    ↓
定位问题层级
    ↓
输出解决方案
```

### 5.2 与日志运维技能集成
```
dgiot_log_operations 激活
    ↓
[dgiot_sensor_data_workflow] 传感器数据工作流技能
    ↓
调整日志级别
    ↓
监控关键日志
    ↓
分析数据流问题
```

### 5.3 与在线调试技能集成
```
dgiot_online_debug 激活
    ↓
[dgiot_sensor_data_workflow] 传感器数据工作流技能
    ↓
实时数据监控
    ↓
逐层问题排查
    ↓
输出调试结果
```

## 6. 最佳实践

### 6.1 代码修改原则
- **不轻易改动对外接口**：保持API兼容性
- **先添加调试日志**：确认问题后再修改逻辑
- **保持代码简洁**：删除冗余代码
- **统一日志格式**：便于问题追踪

### 6.2 调试信息规范
```erlang
% 标准调试日志
?LOG(debug, "[模块] 函数 - 操作, 参数: ~p", [Params])

% 关键节点日志
io:format("~s ~p ========== 阶段 ==========~n", [?FILE, ?LINE])
io:format("~s ~p 关键参数: ~p = ~p~n", [?FILE, ?LINE, Key, Value])
```

### 6.3 团队协作
1. **记录问题**：详细记录现象和调试过程
2. **分享方案**：团队内部分享成功解决方案
3. **更新文档**：及时更新调试规范
4. **代码审查**：确保符合规范

## 7. 检查清单

### 7.1 数据流问题排查检查清单
- [ ] 前端API响应正常？
- [ ] 缓存数据存在且正确？
- [ ] TDengine存储数据正常？
- [ ] 任务队列处理正常？
- [ ] 协议解析正确？
- [ ] 设备连接正常？

### 7.2 调试环境检查清单
- [ ] 日志级别设置为debug？
- [ ] 监控脚本准备就绪？
- [ ] 测试数据可用？
- [ ] 环境配置正确？

### 7.3 修复验证检查清单
- [ ] 修复后数据流恢复正常？
- [ ] 所有层级数据一致？
- [ ] 性能影响在可接受范围？
- [ ] 向后兼容性保持？

## 8. 工具命令

### 8.1 编译加载
```bash
# 热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'

# 热加载
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_modbus).'
```

### 8.2 测试命令
```bash
# 测试API
curl -X GET "http://127.0.0.1/iotapi/devicecard/88a27d8587" \
  -H "Authorization: Bearer r:db1f3d43d05c782c8ceebb87724a2ac0"
```

### 8.3 监控命令
```bash
# 查看日志
tail -f logs/console.log | grep -E "(ERROR|WARNING|DEBUG.*modbus)"

# 查看缓存
_build/emqx/rel/emqx/bin/emqx eval '
io:format("缓存统计:~n"),
io:format("last_data缓存: ~p~n", [length(dgiot_data:match({last_data, '_'}))]).
'
```

## 9. 故障排除指南

### 9.1 数据流中断问题
**症状**：某个层级数据缺失

**排查步骤**：
1. 使用数据流检查脚本逐层排查
2. 检查各层级日志
3. 验证配置和连接
4. 分析代码逻辑

### 9.2 数据不一致问题
**症状**：不同层级数据不一致

**排查步骤**：
1. 同时检查缓存和存储数据
2. 验证数据转换逻辑
3. 检查时间戳同步
4. 分析数据更新机制

### 9.3 性能问题
**症状**：数据延迟或处理缓慢

**排查步骤**：
1. 监控各层级处理时间
2. 检查资源使用情况
3. 分析瓶颈所在
4. 优化关键路径

## 10. 总结

通过本技能，可以：
1. **系统化排查问题**：按照七层数据流逐层排查
2. **快速定位问题**：使用检查清单和工具脚本
3. **提供解决方案**：针对常见问题提供标准解决方案
4. **集成开发流程**：与调试、运维等技能无缝集成
5. **提高维护效率**：标准化的问题排查流程

**使用方式**：
```bash
# 当遇到传感器数据问题时
use_skill dgiot_sensor_data_workflow

# 运行数据流检查脚本
.cline/skills/dgiot_sensor_data_workflow/check_sensor_data_flow.sh <设备ID> <产品ID>

# 运行实时监控脚本
.cline/skills/dgiot_sensor_data_workflow/monitor_sensor_data.sh <设备ID> <间隔秒数>