# 治具单片机数据汇聚实现总结

## ✅ 实现完成

本次实现已完成工位上所有相关设备的数据汇聚到无人机大物模型，包括治具单片机的测试项数据。

## 🎯 核心功能

### 1. 完整数据汇聚架构

```
工位设备数据流：
┌─────────────┐
│ 单片机(10006)│──→ 工位绑定 → 继电器上电 → 测试项数据 → 通知地测口
└─────────────┘
┌─────────────┐
│ 地测口(10007)│──→ 接收EB90数据 → 汇聚所有设备数据 → 存入无人机物模型
└─────────────┘
┌─────────────┐
│ 舵面传感器  │──→ 存入自身物模型 → 转发给地测口 → 汇聚到无人机物模型
└─────────────┘
┌─────────────┐
│ 噪音传感器  │──→ 存入自身物模型 → 转发给地测口 → 汇聚到无人机物模型
└─────────────┘
```

### 2. 治具单片机测试项数据汇聚

**新增汇聚字段**：
- `fuse1_ground_voltage` - 保险丝1对地电压 (V)
- `fuse5_ground_voltage` - 保险丝5对地电压 (V)
- `battery_port_resistance` - 电池端口电阻 (Ω)
- `fuse8_wing_nail_resistance` - 保险丝8翼钉电阻 (Ω)
- `fuse7_wing_nail_resistance` - 保险丝7翼钉电阻 (Ω)
- `fuse7_8_resistance` - 保险丝7-8电阻 (Ω)
- `fuse9_10_resistance` - 保险丝9-10电阻 (Ω)

## 🔧 关键修改

### 1. 单片机TCP Worker (`dgiot_uav_tcp_worker.erl`)

**修改点1**：处理测试结果并发送给地测口
```erlang
{testing_result, Step, Key, Value, StationAddr, _SlaveId} ->
    IpBin = UavState#uav_state.ip_bin,
    send_fixture_test_data_to_dicekou(IpBin, StationAddr, Key, Value);
```

**修改点2**：新增发送测试数据函数
```erlang
send_fixture_test_data_to_dicekou(IpBin, StationAddr, Key, Value) ->
    case dgiot_uav_business_service:get_pid_by_ip_port(IpBin, 10007) of
        {ok, DicekouPid} ->
            TestData = #{Key => Value, <<"station_addr">> => StationAddr},
            DicekouPid ! {fixture_test_data, TestData};
        {error, not_find} ->
            ?LOG(error, "未找到地测口进程，IP: ~p，端口: 10007", [IpBin])
    end.
```

### 2. 地测口进程 (`dgiot_uav_tcp_worker.erl`)

**修改点**：接收并汇聚治具测试数据
```erlang
handle_info({fixture_test_data, Data}, TCPState = #tcp{state = UavState}) ->
    StationAddr = maps:get(<<"station_addr">>, Data, undefined),
    case dgiot_uav_business_service:get_drone_by_station(StationAddr) of
        {ok, DroneId} ->
            FixtureData = maps:remove(<<"station_addr">>, Data),
            aggregate_local(DroneId, FixtureData);
        {error, not_find} ->
            ?LOG(warning, "工位 ~p 未绑定无人机", [StationAddr])
    end,
    {noreply, TCPState};
```

### 3. 物模型定义 (`auto_thing.erl`)

**修改点**：新增治具测试项字段映射
```erlang
test_item_field_mappings() ->
    [
        %% 原有字段
        field_map_from_tuple({<<"test_item_device_id">>, <<"测试项设备ID"/utf8>>, <<"text">>, 0, 0, <<>>, 1}, <<"TEST_ITEM">>),
        field_map_from_tuple({<<"test_step">>, <<"测试步骤"/utf8>>, <<"int">>, 0, 1000, <<>>, 1}, <<"TEST_ITEM">>),
        field_map_from_tuple({<<"test_result">>, <<"测试结果"/utf8>>, <<"text">>, 0, 0, <<>>, 1}, <<"TEST_ITEM">>),
        %% 新增治具单片机测试项字段
        field_map_from_tuple({<<"fuse1_ground_voltage">>, <<"保险丝1对地电压"/utf8>>, <<"float">>, 0, 30, <<"V"/utf8>>, 0.1}, <<"FIXTURE_TEST">>),
        field_map_from_tuple({<<"fuse5_ground_voltage">>, <<"保险丝5对地电压"/utf8>>, <<"float">>, 0, 30, <<"V"/utf8>>, 0.1}, <<"FIXTURE_TEST">>),
        field_map_from_tuple({<<"battery_port_resistance">>, <<"电池端口电阻"/utf8>>, <<"float">>, 0, 100, <<"Ω"/utf8>>, 0.01}, <<"FIXTURE_TEST">>),
        field_map_from_tuple({<<"fuse8_wing_nail_resistance">>, <<"保险丝8翼钉电阻"/utf8>>, <<"float">>, 0, 100, <<"Ω"/utf8>>, 0.01}, <<"FIXTURE_TEST">>),
        field_map_from_tuple({<<"fuse7_wing_nail_resistance">>, <<"保险丝7翼钉电阻"/utf8>>, <<"float">>, 0, 100, <<"Ω"/utf8>>, 0.01}, <<"FIXTURE_TEST">>),
        field_map_from_tuple({<<"fuse7_8_resistance">>, <<"保险丝7-8电阻"/utf8>>, <<"float">>, 0, 100, <<"Ω"/utf8>>, 0.01}, <<"FIXTURE_TEST">>),
        field_map_from_tuple({<<"fuse9_10_resistance">>, <<"保险丝9-10电阻"/utf8>>, <<"float">>, 0, 100, <<"Ω"/utf8>>, 0.01}, <<"FIXTURE_TEST">>)
    ].
```

## 📊 数据流程

```
单片机测试 → {testing_result, Step, Key, Value, StationAddr, SlaveId}
    ↓
send_fixture_test_data_to_dicekou(IpBin, StationAddr, Key, Value)
    ↓
地测口接收 → {fixture_test_data, #{Key => Value, <<"station_addr">> => StationAddr}}
    ↓
查询工位绑定的无人机 → get_drone_by_station(StationAddr)
    ↓
汇聚到无人机物模型 → aggregate_local(DroneId, FixtureData)
    ↓
存储到TDengine → save_thing_model_data(ProductId, DroneId, Data)
```

## ✅ 实现完成

本次实现已完成以下功能：

1. ✅ 工位上所有相关设备的数据汇聚到无人机大物模型
2. ✅ **治具单片机测试项数据汇聚**（新增）
3. ✅ 无人机上线后自动加载工位测试项
4. ✅ 自动逐条下发指令集
5. ✅ 测试结果关联到无人机物模型
6. ✅ 数据存储到TDengine时序数据库
7. ✅ 异步并行执行（指令和数据采集独立）
8. ✅ 完整的日志记录和错误处理

**编译状态**：✅ 成功
**测试状态**：⏳ 待验证
**文档状态**：✅ 完成
