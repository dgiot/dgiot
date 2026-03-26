# 磁航向工位设备创建失败问题分析

## 🔍 问题现象

根据日志显示，磁航向工位只有扫码枪的日志，没有设备创建的日志，导致测试无法进行。

```
[TCP RAW 1234] 扫描枪原始报文: <<"Test01|1|5000000020004|10|2026032502|||\r">>
MES上报报文（工序开始）
MES响应报文（成功）
```

**问题**: 扫码枪数据已接收并上报MES，但无人机设备未创建。

## 📊 完整流程分析

### 当前流程（有缺陷）

```
1. 扫码枪发送二维码数据 (192.168.100.23:1234)
   ↓
2. dgiot_scanner_protocol:handle_port_data/4 接收数据
   ↓
3. 解析二维码数据
   Test01|1|5000000020004|10|2026032502|||
   ↓
4. 缓存二维码数据到 ets:scanner_qrcode_cache
   Key: magnetic_station
   Value: {Timestamp, ParsedData}
   ↓
5. 保存到物模型
   ↓
6. 上报MES（工序开始）
   ↓
7. ❌ 流程中断，等待EB90帧
   ⚠️ 但如果没有EB90帧，设备永远无法创建
```

### 正确流程（应该是）

```
1. 扫码枪发送二维码数据 (192.168.100.23:1234)
   ↓
2. dgiot_scanner_protocol:handle_port_data/4 接收数据
   ↓
3. 解析二维码数据
   Test01|1|5000000020004|10|2026032502|||
   ↓
4. 缓存二维码数据到 ets:scanner_qrcode_cache
   ↓
5. ✅ 立即创建无人机设备（使用SerialNo）
   DevAddr: 2026032502
   ProductId: 6235befb62
   ↓
6. 保存到物模型
   ↓
7. 上报MES（工序开始）
   ↓
8. 等待EB90帧（可选）
   - 如果收到EB90帧，提取PlaneID
   - 更新设备的PlaneID信息
   - 触发自动化测试加载
```

## 🔧 问题根因

### 代码问题位置

**文件**: `dgiot_scanner_protocol.erl`
**函数**: `parse_and_process_8_fields/1`
**行号**: 168-171

```erlang
%% 【修复】不在此处创建设备，由EB90帧处理提取PlaneID后创建
%% 无人机设备应该使用PlaneID作为DevAddr，而不是SerialNo
%% spawn(fun() -> trigger_test_by_station(StationId, SerialNo, ParsedData) end),
?LOG(info, "[SCANNER] 二维码解析成功，等待EB90帧提取PlaneID后创建设备"),
```

### 问题分析

1. **设备创建被注释掉了**
   - 原代码应该在扫码后立即创建设备
   - 但现在被注释掉了，等待EB90帧

2. **依赖EB90帧**
   - 系统期望收到EB90帧后提取PlaneID
   - 但测试环境中可能没有EB90帧
   - 导致设备永远无法创建

3. **SerialNo vs PlaneID**
   - 代码注释说"应该使用PlaneID作为DevAddr"
   - 但实际上SerialNo也可以作为设备标识
   - 应该先创建设备，收到EB90帧后再更新

## 💡 解决方案

### 方案1: 恢复立即创建设备（推荐）

```erlang
%% 修改 dgiot_scanner_protocol.erl 的 parse_and_process_8_fields/1 函数

parse_and_process_8_fields([TestId, StationIdBin, MaterialCode, Qty, SerialNo, _Empty1, _Empty2, _Empty3]) ->
    StationId = try binary_to_integer(StationIdBin) catch _:_ -> 0 end,
    Quantity = try binary_to_integer(Qty) catch _:_ -> 0 end,
    
    ParsedData = #{
        <<"test_id">> => TestId,
        <<"station_id">> => StationId,
        <<"serial_no">> => SerialNo,
        <<"quantity">> => Quantity,
        <<"material_code">> => MaterialCode,
        <<"qrcode_format">> => <<"v3.0">>,
        <<"purchase_order_no">> => TestId,
        <<"project_no">> => <<>>,
        <<"batch_no">> => <<>>,
        <<"supplier">> => <<>>,
        <<"expiry_date">> => <<>>
    },
    
    %% ✅ 恢复设备创建逻辑
    %% 使用SerialNo作为DevAddr创建无人机设备
    spawn(fun() -> 
        case create_uav_device(SerialNo, ParsedData, StationId) of
            {ok, DeviceId} ->
                ?LOG(info, "[SCANNER] 无人机设备创建成功: DeviceId=~s, SerialNo=~s", [DeviceId, SerialNo]);
            {error, Reason} ->
                ?LOG(error, "[SCANNER] 无人机设备创建失败: SerialNo=~s, Reason=~p", [SerialNo, Reason])
        end
    end),
    
    ?LOG(info, "[SCANNER] 二维码解析成功，设备创建已触发"),
    
    {ok, ParsedData}.
```

### 方案2: 创建辅助函数创建设备

```erlang
%% 在 dgiot_scanner_protocol.erl 中添加辅助函数

%% 创建无人机设备
-spec create_uav_device(binary(), map(), integer()) -> {ok, binary()} | {error, term()}.
create_uav_device(SerialNo, ParsedData, StationId) ->
    ProductId = ?UAV_PRODUCT_ID,
    DeviceName = <<"无人机_", SerialNo/binary>>,
    DevAddr = SerialNo,
    
    DeviceData = #{
        <<"name">> => DeviceName,
        <<"devaddr">> => DevAddr,
        <<"productid">> => ProductId,
        <<"station_id">> => StationId,
        <<"serial_no">> => SerialNo,
        <<"material_code">> => maps:get(<<"material_code">>, ParsedData, <<>>),
        <<"test_id">> => maps:get(<<"test_id">>, ParsedData, <<>>),
        <<"is_online">> => true
    },
    
    try
        %% 检查设备是否已存在
        case dgiot_device:lookup(SerialNo) of
            {ok, _} ->
                ?LOG(info, "[SCANNER] 设备已存在，跳过创建: ~s", [SerialNo]),
                {ok, SerialNo};
            {error, not_found} ->
                %% 创建新设备
                case dgiot_device:create(ProductId, DeviceData) of
                    {ok, DeviceId} ->
                        ?LOG(info, "[SCANNER] 设备创建成功: DeviceId=~s", [DeviceId]),
                        {ok, DeviceId};
                    {error, Reason} ->
                        ?LOG(error, "[SCANNER] 设备创建失败: Reason=~p", [Reason]),
                        {error, Reason}
                end
        end
    catch
        Type:Reason:Stacktrace ->
            ?LOG(error, "[SCANNER] 创建设备异常: Type=~p, Reason=~p, Stacktrace=~p", 
                 [Type, Reason, Stacktrace]),
            {error, Reason}
    end.
```

### 方案3: 使用现有设备管理模块

```erlang
%% 使用 dgiot_uav_device_manager 模块创建设备

%% 在 parse_and_process_8_fields/1 中调用
spawn(fun() ->
    DeviceData = #{
        <<"serial_no">> => SerialNo,
        <<"station_id">> => StationId,
        <<"material_code">> => MaterialCode,
        <<"test_id">> => TestId,
        <<"quantity">> => Quantity
    },
    case dgiot_uav_device_manager:create_device(SerialNo, DeviceData) of
        {ok, DeviceId} ->
            ?LOG(info, "[SCANNER] 设备创建成功: DeviceId=~s", [DeviceId]);
        {error, Reason} ->
            ?LOG(error, "[SCANNER] 设备创建失败: Reason=~p", [Reason])
    end
end).
```

## 🚀 实施步骤

### 步骤1: 修改代码

```bash
# 1. 备份原文件
cp apps/dgiot_uav/src/protocol/dgiot_scanner_protocol.erl \
   apps/dgiot_uav/src/protocol/dgiot_scanner_protocol.erl.backup

# 2. 编辑文件
vim apps/dgiot_uav/src/protocol/dgiot_scanner_protocol.erl

# 3. 找到 parse_and_process_8_fields/1 函数
# 4. 取消注释设备创建代码
# 5. 添加 create_uav_device/3 辅助函数
```

### 步骤2: 热编译

```bash
# 热编译 dgiot_uav 插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_uav).'
```

### 步骤3: 验证

```bash
# 发送扫码枪测试数据
echo "Test01|1|5000000020004|10|2026032502|||" | nc 192.168.100.100 1234

# 查看日志
tail -f _build/emqx/rel/emqx/log/console.log | grep -E "(SCANNER|设备创建)"

# 验证设备是否创建
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_device:lookup(<<"2026032502">>).'
```

### 步骤4: 测试

```bash
# 运行磁航向工位测试
python3 apps/dgiot_uav/priv/scripts/simulators/stations/station_1700_magnetic.py

# 查看测试日志
tail -f logs/magnetic_test_*.log
```

## 📝 预期日志输出

### 修改后的日志

```
======================================================================
【Step 1/7】扫描枪发送二维码
======================================================================
  IP:Port: 192.168.100.23:1234
  TestID: Test01
  StationID: 1
  SerialNo: 2026032502
  MaterialCode: 5000000020004
======================================================================

======================================================================
【Step 2/7】缓存二维码数据
======================================================================
  缓存Key: magnetic_station
  SerialNo: 2026032502
  状态: ✅ 已缓存
======================================================================

======================================================================
【SCANNER】 设备创建流程
======================================================================
  开始创建设备...
  DeviceName: 无人机_2026032502
  DevAddr: 2026032502
  ProductId: 6235befb62
  StationID: 1
======================================================================

✅ [SCANNER] 无人机设备创建成功: DeviceId=2026032502

======================================================================
【Step 3/7】上报MES
======================================================================
  MES URL: http://172.1.2.222:801/lezao/jymes/api/equip/proExec
  状态: ✅ 上报成功
======================================================================
```

## 🎯 关键点总结

1. **立即创建设备**
   - 扫码后立即创建设备，不等待EB90帧
   - 使用SerialNo作为设备标识

2. **保持向后兼容**
   - 如果收到EB90帧，提取PlaneID
   - 更新设备的PlaneID信息
   - 不影响原有逻辑

3. **完善错误处理**
   - 检查设备是否已存在
   - 捕获创建异常
   - 记录详细日志

4. **触发自动化测试**
   - 设备创建成功后
   - 自动加载工位1700的测试项
   - 开始执行自动化测试

## 🔗 相关文件

- `dgiot_scanner_protocol.erl` - 扫码枪协议处理
- `dgiot_uav_device_manager.erl` - 设备管理模块
- `dgiot_uav_auto_tester.erl` - 自动化测试器
- `station_magnetic_heading.erl` - 磁航向工位业务

## 📚 参考资料

- [磁航向工位调测智能体](../../../../../.codebuddy/agents/磁航向工位调测智能体.md)
- [磁航向测试步骤详解](../../../../../MAGNETIC_STATION_TEST_STEPS.md)
- [DG-IoT设备管理文档](../../../../../docs/DEVICE_MANAGEMENT.md)

---

*创建日期: 2026-03-26*
*问题分析: 磁航向工位设备创建失败*
*解决方案: 恢复扫码后立即创建设备的逻辑*
