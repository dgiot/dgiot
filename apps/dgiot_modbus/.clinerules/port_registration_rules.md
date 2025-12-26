# Modbus插件端口注册规则

## 概述

本文档定义了DG-IoT Modbus插件中端口注册的相关规则和概念，确保开发人员正确理解和使用端口注册功能。

**核心规则**：
1. **禁止使用客户端连接端口**作为设备标识的一部分，因为客户端端口是随机变化的
2. **允许使用服务器端口（通道端口）**作为设备标识的一部分，因为服务器端口是固定的
3. RegisterByPort注册方式使用服务器端口生成设备地址

## 端口概念区分

### 1. 服务器端口（通道端口）
- **定义**：Modbus通道配置中指定的侦听端口
- **位置**：`dgiot_modbus_channel.erl` 中的通道配置
- **示例**：端口20000
- **特点**：固定的配置端口，不会变化
- **作用**：服务器侦听设备连接的端口，在RegisterByPort中作为设备地址的一部分
- **配置方式**：
  ```erlang
  #{
      <<"port">> => 20000,
      <<"regtype">> => <<"RegisterByPort">>,
      ...
  }
  ```

### 2. 客户端连接端口（禁止使用）
- **定义**：设备连接到服务器时使用的本地端口
- **获取方式**：通过 `inet:peername(Socket)` 获取
- **特点**：随机变化的临时端口
- **禁止原因**：无法用于设备标识，设备重连时端口会变化
- **规则**：禁止在RegisterByPort中使用客户端连接端口

## RegisterByPort注册方式详解

### 1. 注册流程
```
设备连接 → 从State.env获取服务器端口 → 处理注册报文 → 生成设备地址 → 注册设备
```

### 2. 设备地址生成规则
```
设备地址 = 注册报文 + "-" + 服务器端口
```

**示例**：
- 注册报文：`"wrj_dm-zqy"`
- 服务器端口：`20000`
- 生成的设备地址：`"wrj_dm-zqy-20000"`

### 3. 设计目的
- **设备标识**：使用固定端口作为设备标识的一部分
- **端口分组**：同一服务器端口下的设备可以归为一组
- **管理便利**：固定端口便于设备管理和监控

## 代码实现规范

### 1. 中文注释编码规范
**重要规则**：Erlang eval命令不支持中文注释，会导致编码错误

```erlang
% 错误：包含中文注释的Erlang代码（会导致eval失败）
%% 检查设备是否已注册  % ❌ 中文注释会导致编码错误
DeviceAddr = <<"wrj_dm-zqy-20000">>,

% 正确：使用英文注释或无注释
%% Check if device is registered  % ✅ 英文注释安全
DeviceAddr = <<"wrj_dm-zqy-20000">>,
```

**规则要求**：
- 测试脚本中的Erlang代码必须使用英文注释或无注释
- 避免在eval命令中使用包含中文的字符串
- 使用简单的ASCII字符确保编码兼容性

### 2. 服务器端口获取
```erlang
%% 在dgiot_modbus_channel.erl中，服务器端口存储到State.env
init(?TYPE, ChannelId, Args) ->
    #{
        <<"port">> := Port,
        ...
    } = Args,
    
    State = #state{
        env = #{port => Port}  % 将端口信息存储到env中
    }.
```

### 2. RegisterByPort处理
```erlang
%% 在dgiot_modbusrtu_tcp.erl中，从State.env获取服务器端口
handle_info({tcp, Buff}, #tcp{register = false, state = #state{env = Env} = State} = TCPState) ->
    case RegType of
        <<"RegisterByPort">> ->
            case Env of
                #{port := ServerPort} ->
                    % 使用服务器端口
                    handle_port_registration(ChannelId, Buff, Head, Dtutype, ServerPort, TCPState, State);
                _ ->
                    % 服务器端口不存在，回退到正则注册
                    handle_regular_registration(ChannelId, Buff, Head, Dtutype, TCPState, State)
            end.
```

### 3. 设备地址生成
```erlang
%% 生成设备地址：注册报文 + "-" + 服务器端口
process_registration_packet(Buff, Head, Dtutype, Port) ->
    % Port是服务器端口
    DeviceAddr = <<AsciiBuff/binary, "-", (integer_to_binary(Port))/binary>>,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr).
```

## 规则检查清单

### 1. 代码检查
- [ ] RegisterByPort是否使用服务器端口（从State.env获取）？
- [ ] 是否避免了使用 `inet:peername(Socket)` 获取客户端端口？
- [ ] 设备地址生成是否使用服务器端口？
- [ ] 代码注释是否清晰说明了端口类型？
- [ ] 测试脚本中的Erlang代码是否避免使用中文注释？

### 2. 配置检查
- [ ] 通道配置中是否设置了正确的端口？
- [ ] RegisterByPort注册方式是否正确配置？
- [ ] 服务器端口是否在合理范围内（如20000-30000）？

### 3. 测试检查
- [ ] 测试脚本是否验证了RegisterByPort功能？
- [ ] 是否测试了同一端口下多个设备的注册？
- [ ] 是否测试了设备重连时的行为？

## 常见问题解答

### Q1: 为什么禁止使用客户端连接端口？
**A**: 客户端连接端口是随机变化的，设备重连时端口会不同。如果使用客户端端口作为设备标识，设备重连时会被识别为新设备，无法进行有效的设备管理。

### Q2: RegisterByPort有什么优势？
**A**: 
1. **固定标识**：使用服务器端口作为设备标识的一部分，设备标识稳定
2. **端口分组**：同一服务器端口下的设备可以统一管理
3. **配置简单**：只需在通道配置中设置端口即可

### Q3: 设备重连时如何处理？
**A**: 由于使用固定的服务器端口，设备重连时设备地址不变，系统可以正确识别为同一设备。

### Q4: 如何查看设备的服务器端口？
**A**: 可以通过系统日志查看：
```bash
tail -f logs/console.log | grep "RegisterByPort"
```
日志会显示类似信息：
```
RegisterByPort: Processing registration with server port 20000
```

## 最佳实践

### 1. 端口规划
- 为不同的设备类型分配不同的服务器端口范围
- 避免端口冲突，确保每个通道使用唯一的端口
- 记录端口分配表，便于管理和维护

### 2. 代码开发
- 始终使用服务器端口，避免使用客户端端口
- 在代码中添加清晰的注释，说明端口类型
- 遵循设备地址生成规则

### 3. 测试验证
- 测试RegisterByPort注册方式的正确性
- 验证设备地址生成的唯一性
- 测试同一端口下多个设备的并发注册

### 4. 问题排查
- 检查通道配置中的端口设置
- 验证服务器端口是否正确传递到State.env
- 确认设备地址生成是否符合规则

## 相关文件

1. `apps/dgiot_modbus/src/dgiot_modbus_channel.erl` - 通道配置和服务器端口存储
2. `apps/dgiot_modbus/src/dgiot_modbusrtu_tcp.erl` - 端口注册处理逻辑
3. `apps/dgiot_modbus/test/tools/integration/` - 测试脚本和示例

## 更新记录

- **2025-12-26**: 创建端口注册规则文档，明确禁止使用客户端端口，允许使用服务器端口
- **下次评审**: 2026-01-26

---

**重要提示**：严格遵守端口使用规则，确保设备标识的稳定性和可管理性。如有疑问，请参考本文档或咨询插件维护团队。
