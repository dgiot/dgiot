---
name: dgiot_protocol_debug
description: DGIOT协议调试技能，提供协议调试流程、关键日志查看点、常见问题解决方案，支持Modbus RTU等协议调试
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-26
category: debugging
tags: [dgiot, protocol, debug, modbus, rtu, troubleshooting, logs, analysis]
trigger_phrases:
  - 协议调试
  - Modbus调试
  - 报文解析问题
  - 协议解析失败
  - 数据解析错误
  - 调试协议
  - 协议问题排查
  - 报文格式错误
---

# DGIOT协议调试技能

## 概述

本技能提供DGIOT平台协议调试的完整解决方案，特别针对Modbus RTU等协议，提供调试流程、关键日志查看点、常见问题解决方案和调试工具。

## 核心功能

### 1. 协议调试流程

#### 1.1 问题复现流程
```
收集信息 → 数据抓取 → 环境确认 → 日志分析 → 问题定位 → 解决方案
    ↓          ↓          ↓          ↓          ↓          ↓
时间/设备/产品ID  原始数据包  连接状态/网络配置  关键日志查看  原因分析  修复验证
```

#### 1.2 关键日志查看点

**modbus_rtu.erl 关键日志**：
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

**dgiot_modbusrtu_tcp.erl 关键日志**：
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

### 2. 常见问题检查清单

#### 2.1 设备地址问题
- [ ] DtuAddr是否为空？如果为空，是否从Env中获取端口信息？
- [ ] 设备地址格式是否正确？是否为 `<<"port_", Port/binary>>` 格式？
- [ ] 设备是否已正确注册？

#### 2.2 数据解析问题
- [ ] 原始数据是否包含有效的Modbus RTU帧？
- [ ] 从机地址（SlaveId）是否正确匹配？
- [ ] 寄存器地址（Address）是否正确匹配？
- [ ] 功能码（FunCode）是否支持？
- [ ] CRC校验是否通过？

#### 2.3 属性配置问题
- [ ] 产品属性配置是否正确加载？
- [ ] 属性数量是否正确？（如：7个属性）
- [ ] 是否为计算值属性（strategy = "计算值"）？
- [ ] 数据源配置中的slaveid和address格式是否正确？

#### 2.4 数据块模式问题
- [ ] 是否为数据块模式（block_data）？
- [ ] 数据块配置是否正确？
- [ ] 数据块是否包含所有子属性的数据？

### 3. 调试命令

#### 3.1 编译命令
```bash
# 热编译modbus插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_modbus).'

# 热加载modbus插件
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_modbus).'
```

#### 3.2 测试命令
```bash
# 查看产品配置
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_product:lookup_prod(<<"feeb43bffb">>).'

# 测试数据解析
_build/emqx/rel/emqx/bin/emqx eval 'modbus_rtu:parse_frame(<<...>>, #{}, #{<<"dtuproduct">> => <<"feeb43bffb">>, ...}).'
```

#### 3.3 日志查看命令
```bash
# 查看实时日志
tail -f logs/console.log | grep -E "(DEBUG|ERROR|WARNING)"

# 查看特定产品的日志
tail -f logs/console.log | grep "feeb43bffb"
```

### 4. 问题诊断步骤

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

### 5. 数据格式说明

#### 5.1 Modbus RTU帧格式
```
[SlaveId:1][FunCode:1][Data:N][CRC:2]
```

#### 5.2 属性配置格式
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

#### 5.3 计算值属性依赖
- 计算值属性依赖于基础属性（如 `block_data`）
- 通过 `key` 字段指定依赖的基础属性
- 使用 `dataForm.collection` 中的公式计算

### 6. 故障排除

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

### 7. 调试工具

#### 7.1 调试脚本模板
```bash
#!/bin/bash
# debug_modbus_protocol.sh

echo "=== Modbus协议调试脚本 ==="
echo ""

# 1. 检查环境
echo "1. 检查环境..."
_build/emqx/rel/emqx/bin/emqx eval '
    io:format("系统状态检查:~n"),
    io:format("  dgiot_modbus模块: ~p~n", [code:which(dgiot_modbus)]),
    io:format("  modbus_rtu模块: ~p~n", [code:which(modbus_rtu)]).
'

# 2. 查看日志级别
echo "2. 查看日志级别..."
_build/emqx/rel/emqx/bin/emqx eval '
    case logger:get_module_level(dgiot_modbusrtu_tcp) of
        {ok, Level} -> io:format("dgiot_modbusrtu_tcp日志级别: ~p~n", [Level]);
        undefined -> io:format("dgiot_modbusrtu_tcp使用系统默认级别~n")
    end.
'

# 3. 调整日志级别为debug
echo "3. 调整日志级别为debug..."
_build/emqx/rel/emqx/bin/emqx eval '
    logger:set_module_level(dgiot_modbusrtu_tcp, debug),
    logger:set_module_level(modbus_rtu, debug),
    io:format("已调整为debug级别~n").
'

# 4. 开始监控日志
echo "4. 开始监控日志（Ctrl+C停止）..."
tail -f logs/console.log | grep -E "(dgiot_modbusrtu_tcp|modbus_rtu|DEBUG|ERROR|WARNING)"
```

#### 7.2 数据包分析工具
```python
#!/usr/bin/env python3
# analyze_modbus_packet.py

import struct

def analyze_modbus_rtu(packet_hex):
    """分析Modbus RTU数据包"""
    packet = bytes.fromhex(packet_hex)
    
    print("=== Modbus RTU数据包分析 ===")
    print(f"原始数据: {packet_hex}")
    print(f"数据长度: {len(packet)} bytes")
    
    if len(packet) < 4:
        print("❌ 数据包太短，不是有效的Modbus RTU帧")
        return
    
    # 解析从机地址
    slave_id = packet[0]
    print(f"从机地址: {slave_id} (0x{slave_id:02X})")
    
    # 解析功能码
    func_code = packet[1]
    func_names = {
        0x01: "读取线圈状态",
        0x02: "读取输入状态", 
        0x03: "读取保持寄存器",
        0x04: "读取输入寄存器",
        0x05: "写单个线圈",
        0x06: "写单个寄存器",
        0x0F: "写多个线圈",
        0x10: "写多个寄存器"
    }
    func_name = func_names.get(func_code, "未知功能码")
    print(f"功能码: {func_code} (0x{func_code:02X}) - {func_name}")
    
    # 解析数据部分
    if len(packet) > 4:
        data = packet[2:-2]
        print(f"数据部分: {data.hex()}")
        
        # 如果是读取寄存器响应
        if func_code in [0x03, 0x04] and len(data) > 1:
            byte_count = data[0]
            register_data = data[1:]
            print(f"字节数: {byte_count}")
            print(f"寄存器数据: {register_data.hex()}")
            
            # 解析寄存器值
            if len(register_data) >= 2:
                for i in range(0, len(register_data), 2):
                    if i + 2 <= len(register_data):
                        value = struct.unpack('>H', register_data[i:i+2])[0]
                        print(f"  寄存器{i//2}: {value} (0x{value:04X})")
    
    # 解析CRC
    if len(packet) >= 4:
        crc_received = packet[-2:]
        print(f"CRC校验: {crc_received.hex()}")
    
    print("=== 分析完成 ===")

if __name__ == '__main__':
    # 示例：010300000001840B
    packet_hex = input("请输入Modbus RTU数据包（十六进制）: ")
    analyze_modbus_rtu(packet_hex)
```

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

### 9. 技能集成

#### 9.1 与自主开发技能集成
```
dgiot_autonomous_development 激活
    ↓
[dgiot_protocol_debug] 协议调试技能
    ↓
[dgiot_compile_debug] 热编译调试
    ↓
[dgiot_online_debug] 在线调测
    ↓
输出调试方案和修复代码
```

#### 9.2 与Erlang最佳实践集成
```
dgiot_erlang_best_practices 激活
    ↓
[dgiot_protocol_debug] 协议调试
    ↓
[erlang_chinese_utf8] 中文日志
    ↓
[dgiot_compile_debug] 热编译
    ↓
输出符合最佳实践的调试代码
```

### 10. 检查清单

#### 调试前检查
- [ ] 收集了完整的问题信息（时间、设备、产品ID）
- [ ] 保存了原始数据包（十六进制格式）
- [ ] 确认了环境状态（连接、配置）
- [ ] 设置了适当的日志级别（debug）

#### 调试中检查
- [ ] 查看了关键日志查看点
- [ ] 验证了数据包格式
- [ ] 检查了设备地址和配置
- [ ] 分析了属性解析过程

#### 调试后检查
- [ ] 记录了调试过程和发现
- [ ] 验证了修复方案的有效性
- [ ] 更新了相关文档
- [ ] 恢复了日志级别（商用环境）

## 总结

通过本技能，可以：
1. **快速定位协议解析问题**
2. **提供完整的调试流程和工具**
3. **解决常见的协议调试问题**
4. **集成到自主开发流程中**
5. **提高协议调试效率和质量**

**使用方式**：
```bash
# 当遇到协议解析问题时
use_skill dgiot_protocol_debug

# 运行调试脚本
.cline/skills/dgiot_protocol_debug/debug_modbus_protocol.sh