# Erlang Shell交互式调试指南

## 启动Erlang Shell

### 方法1: 直接启动Erlang Shell
```bash
# 进入项目目录
cd /root/gitee/dgiot

# 启动Erlang Shell（加载项目路径）
erl -pa _build/emqx/rel/emqx/lib/*/ebin -pa apps/*/ebin
```

### 方法2: 使用emqx的Erlang Shell
```bash
# 使用emqx的nodetool连接到运行中的节点
_build/emqx/rel/emqx/bin/emqx remote_console
```

### 方法3: 启动新的Erlang节点并连接到运行中的emqx
```bash
# 启动新的Erlang节点
erl -name debug@127.0.0.1 -setcookie emqxsecretcookie

# 在Erlang Shell中连接到emqx节点
net_kernel:connect_node('emqx@127.0.0.1').
```

## 常用调试命令

### 1. 基本系统信息
```erlang
% 查看节点信息
node().
nodes().

% 查看系统信息
erlang:system_info(otp_release).
erlang:memory().

% 查看加载的模块
code:all_loaded().
```

### 2. 检查Modbus模块
```erlang
% 检查模块是否加载
code:which(dgiot_modbusrtu_tcp).
code:which(modbus_rtu).
code:which(dgiot_task).

% 查看模块信息
modbus_rtu:module_info().
dgiot_modbusrtu_tcp:module_info(exports).
```

### 3. 测试设备注册
```erlang
% 生成设备ID
RegistrationPacket = <<"wrj_dm-zqy">>,
Port = 20000,
DeviceAddr = <<RegistrationPacket/binary, "-", (integer_to_binary(Port))/binary>>,
ProductId = <<"feeb43bffb">>, % 需要根据实际情况调整
DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr).

% 检查设备是否存在
dgiot_device:lookup(DeviceId).
```

### 4. 测试数据解析
```erlang
% 测试Modbus数据解析
TestData = <<1, 3, 4, 0, 0, 0, 0, 196, 11>>,
State = #{<<"dtuproduct">> => <<"feeb43bffb">>, 
          <<"slaveId">> => 1, 
          <<"dtuaddr">> => <<"wrj_dm-zqy-20000">>, 
          <<"address">> => 0}.

% 调用解析函数
modbus_rtu:parse_frame(TestData, #{}, State).
```

### 5. 检查产品配置
```erlang
% 获取所有产品
{ok, Products} = dgiot_product:get_all().

% 查找包含wrj_dm的产品
WrjProducts = lists:filter(fun(P) ->
    case P of
        #{<<"name">> := Name} when is_binary(Name) ->
            binary:match(Name, <<"wrj_dm">>) =/= nomatch;
        _ -> false
    end
end, Products).

% 查看产品详情
case WrjProducts of
    [] -> not_found;
    [First|_] -> 
        ProductId = maps:get(<<"objectId">>, First),
        {ok, Product} = dgiot_product:lookup_prod(ProductId),
        Product
end.
```

### 6. 测试缓存功能
```erlang
% 测试缓存读写
TestDeviceId = <<"test_device_123">>,
TestData = #{<<"test">> => 123, <<"timestamp">> => erlang:system_time(millisecond)}.

% 写入缓存
dgiot_data:put({last_data, TestDeviceId}, TestData).

% 读取缓存
dgiot_data:get({last_data, TestDeviceId}).

% 查看所有缓存
dgiot_data:match({last_data, '_'}).
```

### 7. 测试公式计算器
```erlang
% 测试公式计算
Collection = <<"(%%{block_data} - 100) * 0.1">>,
Variables = #{<<"block_data">> => 150},
Prop = #{<<"dataType">> => #{<<"type">> => <<"FLOAT">>, <<"specs">> => #{<<"precision">> => 2}}}.

dgiot_formula_calculator_simple:calculate_formula(Collection, Variables, Prop).
```

## 交互式测试场景

### 场景1: 模拟设备注册
```erlang
% 1. 启动Erlang Shell
% 2. 加载必要模块
code:ensure_loaded(dgiot_modbusrtu_tcp).
code:ensure_loaded(modbus_rtu).

% 3. 模拟注册报文
RegistrationPacket = <<"wrj_dm-zqy">>.
Port = 20000.

% 4. 查看注册处理逻辑
% 查看dgiot_modbusrtu_tcp模块中的handle_info函数
dgiot_modbusrtu_tcp:module_info(exports).
```

### 场景2: 测试数据块解析
```erlang
% 1. 准备测试数据
TestData = <<1, 3, 8, 0, 0, 0, 0, 0, 0, 0, 0, 205, 248>>.  % 包含8字节数据

% 2. 准备解析状态
State = #{<<"dtuproduct">> => <<"feeb43bffb">>,
          <<"slaveId">> => 1,
          <<"dtuaddr">> => <<"test_device">>,
          <<"address">> => 0}.

% 3. 解析数据
{_Rest, ParsedData} = modbus_rtu:parse_frame(TestData, #{}, State).

% 4. 查看解析结果
ParsedData.
```

### 场景3: 测试完整数据流
```erlang
% 1. 模拟原始数据接收
RawData = <<"wrj_dm-zqy", 1, 3, 4, 0, 0, 0, 0, 196, 11>>.

% 2. 查看通讯层处理
% 查看dgiot_modbusrtu_tcp:handle_info/2如何处理数据

% 3. 检查任务通道
TaskChannel = dgiot_product_channel:get_taskchannel(<<"feeb43bffb">>).

% 4. 模拟发送到任务通道
% dgiot_client:send(TaskChannel, DeviceId, Topic, Data)
```

## 调试技巧

### 1. 跟踪函数调用
```erlang
% 使用dbg跟踪函数调用
dbg:tracer().
dbg:p(all, c).
dbg:tpl(dgiot_modbusrtu_tcp, handle_info, 2, []).
dbg:tpl(modbus_rtu, parse_frame, 3, []).

% 运行测试后查看跟踪信息
% 停止跟踪
dbg:stop().
```

### 2. 查看进程状态
```erlang
% 查看所有进程
processes().

% 查看特定模块的进程
[P || P <- processes(), 
      case process_info(P, initial_call) of
          {initial_call, {Mod, _, _}} -> Mod == dgiot_modbusrtu_tcp;
          _ -> false
      end].

% 查看进程消息队列
process_info(pid(0,123,0), messages).
```

### 3. 热加载代码
```erlang
% 编译并加载模块
c:l(dgiot_modbusrtu_tcp).

% 或者使用make
make:all([load]).
```

### 4. 异常调试
```erlang
% 捕获异常
try
    modbus_rtu:parse_frame(<<>>, #{}, #{})
catch
    Type:Error ->
        io:format("异常类型: ~p, 错误: ~p~n", [Type, Error]),
        erlang:get_stacktrace()
end.
```

## 实用函数

### 创建测试辅助函数
```erlang
% 在Erlang Shell中定义临时函数
TestHelper = fun() ->
    io:format("=== 测试辅助函数 ===~n"),
    
    % 测试1: 检查模块
    io:format("1. 检查模块加载...~n"),
    Modules = [dgiot_modbusrtu_tcp, modbus_rtu, dgiot_task],
    lists:foreach(fun(Mod) ->
        case code:which(Mod) of
            non_existing -> io:format("  ❌ ~p 未加载~n", [Mod]);
            Path -> io:format("  ✅ ~p 已加载: ~s~n", [Mod, filename:basename(Path)])
        end
    end, Modules),
    
    % 测试2: 检查产品
    io:format("2. 检查产品配置...~n"),
    case dgiot_product:get_all() of
        {ok, Products} ->
            io:format("  产品总数: ~p~n", [length(Products)]),
            WrjProducts = [P || P <- Products, 
                case P of
                    #{<<"name">> := Name} -> binary:match(Name, <<"wrj_dm">>) =/= nomatch;
                    _ -> false
                end],
            io:format("  相关产品数: ~p~n", [length(WrjProducts)]);
        _ -> io:format("  获取产品失败~n")
    end,
    
    io:format("测试完成~n")
end.

% 运行测试
TestHelper().
```

## 快速参考

### 启动命令
```bash
# 快速启动并运行测试
cd /root/gitee/dgiot && erl -pa _build/emqx/rel/emqx/lib/*/ebin -pa apps/*/ebin \
  -eval "io:format('Erlang Shell已启动~n'), 
         io:format('节点: ~p~n', [node()]), 
         io:format('OTP版本: ~p~n', [erlang:system_info(otp_release)])."
```

### 常用快捷键
- `Ctrl+G` 然后 `h` - 查看帮助
- `Ctrl+G` 然后 `c` - 连接到其他节点
- `Ctrl+G` 然后 `q` - 退出Erlang Shell
- `Ctrl+C` 然后 `a` - 中断当前操作
- `Ctrl+C` 然后 `c` - 强制退出

### 调试流程
1. **启动Shell**: 使用正确路径启动Erlang Shell
2. **加载模块**: 确保所需模块已加载
3. **测试函数**: 调用相关函数进行测试
4. **查看结果**: 分析返回值和异常
5. **调整代码**: 根据需要修改代码并重新加载
6. **重复测试**: 直到问题解决

## 总结

Erlang Shell是强大的交互式调试工具，可以：
- ✅ 实时测试函数调用
- ✅ 查看模块和进程状态
- ✅ 跟踪函数执行流程
- ✅ 热加载代码修改
- ✅ 模拟各种测试场景

对于Modbus测试，特别适合：
1. 测试设备注册逻辑
2. 验证数据解析功能
3. 检查产品配置
4. 调试缓存机制
5. 跟踪完整数据流

现在可以打开Erlang Shell开始交互式调试了！
