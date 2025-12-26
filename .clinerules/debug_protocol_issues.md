# Modbus RTU协议调试规范

## 概述

本规范定义了在DG-IoT平台中调试Modbus RTU协议问题的标准流程和方法，确保问题定位的高效性和准确性。

## 调试流程

### 1. 问题复现
- **收集信息**：记录问题发生的时间、设备、产品ID、通道ID
- **数据抓取**：保存原始数据包（十六进制格式）
- **环境确认**：确认设备连接状态、网络配置、产品配置

### 2. 关键日志查看点

#### 2.1 modbus_rtu.erl 关键日志
```erlang
% parse_frame 入口
io:format("~s ~p [DEBUG] parse_frame - Direct sensor mode~n", [?FILE, ?LINE])
io:format("~s ~p   SlaveId: ~p, DtuAddr: ~p, Address: ~p, ProductId: ~p~n", [?FILE, ?LINE, SlaveId, DtuAddr, Address, ProductId])

% decode_data 函数
io:format("~s ~p [DEBUG] decode_data - Enter~n", [?FILE, ?LINE])
io:format("~s ~p   ProductId: ~p, DtuAddr: ~p, Address: ~p~n", [?FILE, ?LINE, ProductId, DtuAddr, Address])
io:format("~s ~p   SlaveId: ~p, FunCode: ~p (0x~2.16.0B)~n", [?FILE, ?LINE, SlaveId, FunCode, FunCode])
io:format("~s ~p   UserZone (hex): ~p~n", [?FILE, ?LINE, dgiot_utils:binary_to_hex(UserZone)])
io:format("~s ~p   modbus_decoder result: ~p~n", [?FILE, ?LINE, Result])
```

#### 2.2 dgiot_modbusrtu_tcp.erl 关键日志
```erlang
% 数据处理入口
io:format("~s ~p ProductId ~p DtuAddr ~p Env ~p Received data: ~p~n", [?FILE, ?LINE, ProductId, DtuAddr, Env, dgiot_utils:binary_to_hex(Buff)])

% 属性解析过程
io:format("~s ~p ========== Calculating derived properties ==========~n", [?FILE, ?LINE])
io:format("~s ~p ProductId: ~p, DevAddr: ~p~n", [?FILE, ?LINE, ProductId, DevAddr])
io:format("~s ~p Input Calculated: ~p~n", [?FILE, ?LINE, Calculated])
io:format("~s ~p Props count: ~p~n", [?FILE, ?LINE, length(Props)])

% 发送到Task通道
io:format("~s ~p ========== Sending to Task Channel ==========~n", [?FILE, ?LINE])
io:format("~s ~p ProductId: ~p, DtuAddr: ~p~n", [?FILE, ?LINE, ProductId, DtuAddr])
io:format("~s ~p Data to send: ~p~n", [?FILE, ?LINE, EnrichedThings])
```

### 3. 常见问题检查清单

#### 3.1 设备地址问题
- [ ] DtuAddr是否为空？如果为空，是否从Env中获取端口信息？
- [ ] 设备地址格式是否正确？是否为 `<<"port_", Port/binary>>` 格式？
- [ ] 设备是否已正确注册？

#### 3.2 数据解析问题
- [ ] 原始数据是否包含有效的Modbus RTU帧？
- [ ] 从机地址（SlaveId）是否正确匹配？
- [ ] 寄存器地址（Address）是否正确匹配？
- [ ] 功能码（FunCode）是否支持？
- [ ] CRC校验是否通过？

#### 3.3 属性配置问题
- [ ] 产品属性配置是否正确加载？
- [ ] 属性数量是否正确？（如：7个属性）
- [ ] 是否为计算值属性（strategy = "计算值"）？
- [ ] 数据源配置中的slaveid和address格式是否正确？

#### 3.4 数据块模式问题
- [ ] 是否为数据块模式（block_data）？
- [ ] 数据块配置是否正确？
- [ ] 数据块是否包含所有子属性的数据？

### 4. 调试命令

#### 4.1 编译命令
```bash
# 热编译modbus插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'

# 热加载modbus插件
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_modbus).'
```

#### 4.2 测试命令
```bash
# 查看产品配置
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_product:lookup_prod(<<"feeb43bffb">>).'

# 测试数据解析
_build/emqx/rel/emqx/bin/emqx eval 'modbus_rtu:parse_frame(<<...>>, #{}, #{<<"dtuproduct">> => <<"feeb43bffb">>, ...}).'
```

#### 4.3 日志查看命令
```bash
# 查看实时日志
tail -f logs/console.log | grep -E "(DEBUG|ERROR|WARNING)"

# 查看特定产品的日志
tail -f logs/console.log | grep "feeb43bffb"
```

### 5. 问题诊断步骤

#### 步骤1：确认数据接收
1. 检查 `dgiot_modbusrtu_tcp.erl` 中的 `Received data` 日志
2. 确认数据是否为有效的Modbus RTU帧
3. 检查设备地址是否正确

#### 步骤2：检查解析匹配
1. 查看 `modbus_rtu.erl` 中的 `parse_frame` 日志
2. 确认SlaveId、Address、ProductId是否匹配
3. 检查功能码是否正确识别

#### 步骤3：验证属性解析
1. 查看 `modbus_decoder result` 日志
2. 确认解析出的属性数量
3. 检查是否为计算值属性

#### 步骤4：检查派生属性计算
1. 查看 `Calculating derived properties` 日志
2. 确认输入数据（Calculated）是否正确
3. 检查属性配置数量

### 6. 数据格式说明

#### 6.1 Modbus RTU帧格式
```
[SlaveId:1][FunCode:1][Data:N][CRC:2]
```

#### 6.2 属性配置格式
```json
{
  "identifier": "angular_x",
  "dataForm": {
    "strategy": "计算值",
    "protocol": "MODBUSRTU"
  },
  "dataSource": {
    "slaveid": "0X01",
    "address": "0X00",
    "key": "block_data"
  }
}
```

#### 6.3 计算值属性依赖
- 计算值属性依赖于基础属性（如 `block_data`）
- 通过 `key` 字段指定依赖的基础属性
- 使用 `dataForm.collection` 中的公式计算

### 7. 故障排除

#### 问题1：只解析出block_data，没有其他属性
**可能原因**：
1. 其他属性为计算值属性，但计算失败
2. 属性配置中的slaveid/address不匹配
3. 数据块不包含所有子属性的数据

**解决方案**：
1. 检查属性配置中的 `strategy` 字段
2. 验证slaveid和address的格式和值
3. 检查数据块大小是否足够

#### 问题2：设备地址为空
**可能原因**：
1. 注册报文未正确解析
2. 端口信息未正确传递

**解决方案**：
1. 检查注册流程
2. 从Env中获取端口信息构建设备地址

#### 问题3：CRC校验失败
**可能原因**：
1. 数据包损坏
2. 帧格式错误

**解决方案**：
1. 重新抓取数据包
2. 检查Modbus RTU帧格式

### 8. 最佳实践

#### 8.1 调试信息添加
- 在关键函数入口添加调试信息
- 显示重要参数的值
- 使用统一的日志格式

#### 8.2 代码修改原则
- 先添加调试信息，不修改逻辑
- 确认问题后再进行修复
- 保持代码简洁，删除冗余代码

#### 8.3 团队协作
- 记录调试过程和发现
- 分享解决方案
- 更新调试规范

### 9. 示例调试场景

#### 场景：角度X/Y/Z属性未解析
**现象**：只解析出 `block_data`，没有 `angular_x`、`angular_y`、`angular_z`

**调试步骤**：
1. 查看 `Props count` 日志，确认属性数量
2. 检查属性配置，确认是否为计算值属性
3. 查看 `Input Calculated` 日志，确认输入数据
4. 检查 `get_calculated` 函数中的处理逻辑

**可能原因**：
- 属性配置中的 `key` 字段不正确
- 计算值属性处理逻辑有误
- 数据块不包含角度数据

### 10. 更新记录

- 2025-12-24：创建Modbus RTU协议调试规范
- 基于实际调试经验总结

---

**提示**：在调试时，按照本规范的步骤进行，可以快速定位和解决问题。保持调试信息的完整性和一致性，有助于团队协作和知识积累。
