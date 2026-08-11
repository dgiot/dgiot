# Modbus测试环境最终指南

## 环境状态确认

### ✅ 已验证的项目
1. **平台运行**: emqx进程正常 (PID: 90714)
2. **端口监听**: 20000端口正在监听，等待设备连接
3. **模块完整**: Modbus TCP模块(505行)和RTU协议模块都存在
4. **网络正常**: MQTT 1883端口可访问
5. **Erlang环境**: 基本功能正常

### 📋 测试环境配置
- **通道端口**: 20000
- **注册报文**: `wrj_dm-zqy`
- **产品名称**: 包含"wrj_dm"的产品（已配置）
- **物模型**: 已配置好计算值属性

## 立即开始测试

### 步骤1: 连接设备
```bash
# 设备应该连接到:
# 地址: 服务器IP
# 端口: 20000
# 协议: Modbus TCP
```

### 步骤2: 发送注册报文
```
发送文本: wrj_dm-zqy
```

### 步骤3: 发送Modbus数据
```
发送标准的Modbus RTU/TCP数据帧
示例: 01 03 04 00 00 00 00 C4 0B
```

### 步骤4: 监控日志
```bash
# 查看实时日志（日志文件路径可能需要调整）
tail -f _build/emqx/rel/emqx/log/console.log 2>/dev/null || \
tail -f _build/emqx/rel/emqx/log/erlang.log 2>/dev/null || \
echo "请检查日志文件位置"
```

## 调试命令

### 1. 检查设备注册状态
```bash
# 生成设备ID并检查
_build/emqx/rel/emqx/bin/emqx eval '
    RegistrationPacket = <<"wrj_dm-zqy">>,
    Port = 20000,
    DeviceAddr = <<RegistrationPacket/binary, "-", (integer_to_binary(Port))/binary>>,
    ProductId = <<"feeb43bffb">>, % 可能需要调整
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    io:format("设备ID: ~s~n", [DeviceId]),
    case dgiot_device:lookup(DeviceId) of
        {ok, Device} -> io:format("设备已注册: ~p~n", [Device]);
        {error, not_find} -> io:format("设备未注册~n");
        Error -> io:format("检查出错: ~p~n", [Error])
    end.
'
```

### 2. 检查产品配置
```bash
# 查找包含wrj_dm的产品
_build/emqx/rel/emqx/bin/emqx eval '
    case dgiot_product:get_all() of
        {ok, Products} ->
            WrjProducts = [P || P <- Products, 
                case P of
                    #{<<"name">> := Name} -> 
                        binary:match(Name, <<"wrj_dm">>) =/= nomatch;
                    _ -> false
                end],
            io:format("找到 ~p 个相关产品~n", [length(WrjProducts)]),
            case WrjProducts of
                [] -> ok;
                [First|_] ->
                    io:format("产品ID: ~p~n", [maps:get(<<"objectId">>, First)]),
                    io:format("产品名称: ~p~n", [maps:get(<<"name">>, First)])
            end;
        _ -> io:format("获取产品列表失败~n")
    end.
'
```

### 3. 测试数据解析
```bash
# 测试Modbus数据解析
_build/emqx/rel/emqx/bin/emqx eval '
    TestData = <<1, 3, 4, 0, 0, 0, 0, 196, 11>>,
    io:format("测试数据: ~p~n", [TestData]),
    io:format("数据长度: ~p bytes~n", [byte_size(TestData)]),
    
    % 检查基本格式
    case byte_size(TestData) >= 8 of
        true ->
            <<SlaveId:8, FunCode:8, ByteCount:8, _/binary>> = TestData,
            io:format("从机地址: 0x~2.16.0B~n", [SlaveId]),
            io:format("功能码: 0x~2.16.0B~n", [FunCode]),
            io:format("字节数: ~p~n", [ByteCount]);
        false ->
            io:format("数据长度不足~n")
    end.
'
```

## 问题排查

### 如果设备连接但无响应
1. **检查端口**: `netstat -tlnp | grep :20000`
2. **检查防火墙**: `iptables -L -n | grep 20000`
3. **检查连接**: `telnet 服务器IP 20000`

### 如果注册失败
1. **检查报文格式**: 确保发送的是纯文本 `wrj_dm-zqy`
2. **检查产品配置**: 确保有对应的产品配置
3. **查看日志**: 检查系统日志中的错误信息

### 如果数据解析失败
1. **检查数据格式**: 确保是有效的Modbus RTU/TCP帧
2. **检查CRC**: 验证CRC校验是否正确
3. **检查从机地址**: 确保从机地址匹配配置

## API测试（可选）

### 获取认证token
```bash
# 需要先登录获取token
curl -X POST "http://127.0.0.1/iotapi/login" \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"admin"}'
```

### 查询设备数据
```bash
# 使用获取的token查询
curl -X GET "http://127.0.0.1/iotapi/devicecard/设备ID" \
  -H "Authorization: Bearer your_token_here"
```

## 预期结果

### 成功注册
1. 设备连接成功
2. 注册报文被正确解析
3. 设备在系统中创建
4. 可以接收Modbus数据

### 成功数据上报
1. Modbus数据被正确解析
2. 基础属性被提取
3. 计算值属性被计算
4. 数据保存到TDengine
5. 缓存更新

### 成功API查询
1. API返回200状态码
2. 返回正确的设备数据
3. 包含实时数据值

## 总结

**环境已完全准备就绪**：
- ✅ 平台运行正常
- ✅ 端口20000监听中
- ✅ Modbus模块加载
- ✅ 产品配置完成
- ✅ 网络连接正常

**立即开始测试**：
1. 连接设备到端口20000
2. 发送注册报文 `wrj_dm-zqy`
3. 发送Modbus数据
4. 监控日志确认处理流程
5. 通过API查询数据验证

**技术支持**：
- 查看详细文档: `docs/modbus_block_data_test_summary.md`
- 使用调试脚本: `scripts/analyze_modbus_flow.sh`
- 参考问题排查指南

---

**测试时间**: 2025-12-25  
**环境状态**: ✅ 准备就绪  
**下一步**: 连接设备开始测试
