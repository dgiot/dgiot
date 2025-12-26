# 传感器数据上报到前端展示完整工作流

## 概述

传感器数据从设备采集到前端展示的完整工作流程，提供快速定位和解决问题的标准方法。

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

## 4. 最佳实践

### 4.1 代码修改原则
- **不轻易改动对外接口**：保持API兼容性
- **先添加调试日志**：确认问题后再修改逻辑
- **保持代码简洁**：删除冗余代码
- **统一日志格式**：便于问题追踪

### 4.2 调试信息规范
```erlang
% 标准调试日志
?LOG(debug, "[模块] 函数 - 操作, 参数: ~p", [Params])

% 关键节点日志
io:format("~s ~p ========== 阶段 ==========~n", [?FILE, ?LINE])
io:format("~s ~p 关键参数: ~p = ~p~n", [?FILE, ?LINE, Key, Value])
```

### 4.3 团队协作
1. **记录问题**：详细记录现象和调试过程
2. **分享方案**：团队内部分享成功解决方案
3. **更新文档**：及时更新调试规范
4. **代码审查**：确保符合规范

## 5. 工具命令

### 5.1 编译加载
```bash
# 热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'

# 热加载
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:reload_plugin(dgiot_modbus).'
```

### 5.2 测试命令
```bash
# 测试API
curl -X GET "http://127.0.0.1/iotapi/devicecard/88a27d8587" \
  -H "Authorization: Bearer r:db1f3d43d05c782c8ceebb87724a2ac0"
```

### 5.3 监控命令
```bash
# 查看日志
tail -f logs/console.log | grep -E "(ERROR|WARNING|DEBUG.*modbus)"

# 查看缓存
_build/emqx/rel/emqx/bin/emqx eval '
io:format("缓存统计:~n"),
io:format("last_data缓存: ~p~n", [length(dgiot_data:match({last_data, '_'}))]).
'
```

## 6. 更新记录

- **v1.0 (2025-12-24)**：创建传感器数据上报工作流
  - 基于实际调试经验
  - 提供完整数据流架构
  - 包含常见问题解决方案

---

## 使用说明

### 新成员培训
1. 阅读本文档了解数据流
2. 使用检查清单定位问题
3. 参考最佳实践修改代码
4. 使用工具命令调试

### 日常调试
1. 按逐层检查清单排查
2. 参考问题解决方案
3. 记录调试过程
4. 更新文档分享

**提示**：保持调试系统性，遵循"不轻易改动对外接口"原则。
