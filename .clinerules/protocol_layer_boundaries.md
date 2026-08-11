# 协议解析层职责边界规则

## 职责分离

**协议解析层**: 解析协议帧、提取字段、转换格式 | ❌不调用存储、不执行业务逻辑、不依赖设备上下文
**业务处理层**: 设备注册、数据汇聚、触发存储 | ✅唯一的存储调用位置
**数据存储层**: 封装存储逻辑、统一接口 | ❌不包含协议解析

## 数据流向

```
原始数据 → 协议解析 → 业务处理 → 数据存储
禁止: 协议层直接调用存储
```

## 函数签名

```erlang
%% ✅ 正确：纯函数
parse_frame(Binary) -> {ok, ParsedData}.

%% ❌ 错误：依赖业务上下文
parse_frame(Binary, ProductId, DevAddr) -> ...
```

## 存储调用规则

❌ **错误**（协议解析层）:
```erlang
handle_parsed_frame(ProductId, DevAddr, Data) ->
    ParsedMap = parse_data(Data),
    uav_thing_model:save_thing_model_data(ProductId, DevAddr, ParsedMap).  ❌
```

✅ **正确**（业务处理层）:
```erlang
handle_result({ok, ParsedMap}, Context) ->
    {ProductId, DevAddr} = get_context(Context),
    uav_thing_model:save_thing_model_data(ProductId, DevAddr, ParsedMap).  ✅
```

## 常见违规模式

### 1. 协议层调用存储
**症状**: `error: [存储] Invalid arguments: DevAddr=undefined`
**检测**: `grep -rn "save_thing_model_data" apps/dgiot_uav/src/protocol/`
**修复**: 删除协议层存储调用，依赖业务层汇聚机制

### 2. 重复存储
**症状**: 同一数据被存储多次
**检测**: 对比协议层和业务层存储调用
**修复**: 选择正确位置（业务层），删除重复调用

### 3. 协议层调用多种存储函数（新增模式）
**症状**: 协议解析层直接调用 `dgiot_task:save_td/4`, `dgiot_device:save/2`, `dgiot_tdengine_adapter:save_sql/2`, `dgiot_data:insert/2` 等存储函数
**检测**: 
```bash
# 搜索协议层中的常见存储调用
grep -rn "dgiot_task:save_td\|dgiot_device:save\|dgiot_tdengine_adapter:save_sql\|dgiot_data:insert" apps/*/src/protocol/
grep -rn "dgiot_task:save_td\|dgiot_device:save\|dgiot_tdengine_adapter:save_sql\|dgiot_data:insert" apps/*/src/communication/
```
**修复**: 
1. 将存储调用提取到业务层
2. 协议层改为纯函数，只返回解析结果
3. 业务层负责获取上下文（ProductId, DevAddr）并调用存储
**影响插件**: Modbus, HJT212, GB26875, Dlink 等协议插件

### 4. 通信层调用存储（通信服务器违规）
**症状**: TCP/UDP通信服务器层直接调用数据存储函数
**检测**: 
```bash
# 搜索通信服务器层中的存储调用
grep -rn "dgiot_task:save_td\|save_td" apps/*/src/communication/*server*
```
**修复**: 
1. 通信层只负责网络连接和数据转发
2. 将数据传递给协议层进行解析
3. 解析结果传递给业务层进行存储
**示例**: `dgiot_modbus_rtu_server.erl` 中的 `dgiot_task:save_td` 调用

## 代码审查清单

协议层:
- [ ] 不调用 `save_*` 函数
- [ ] 不依赖 ProductId/DevAddr
- [ ] 不包含业务逻辑判断

业务层:
- [ ] 正确调用协议层
- [ ] 获取必要上下文
- [ ] 在此处调用存储
- [ ] 有汇聚或过滤逻辑

## 最佳实践

```erlang
%% 协议层：只解析
parse(Frame) -> {ok, ParsedData}.

%% 业务层：处理和存储
process(ParsedData) ->
    Context = get_context(),
    store(ParsedData, Context).

%% 存储层：持久化
save(Data) -> {ok, Result}.
```

## 案例

### 案例1: UAV D2/D3存储问题
**问题**: D2/D3存储报错 `DevAddr=undefined`
**根因**: `uav_protocol.erl`（协议层）调用存储，但DevAddr未定义
**解决**: 删除协议层存储调用，依赖 `dgiot_eb90_protocol:handle_parsed_result/1` 汇聚机制

### 案例2: Modbus协议层多种存储调用
**问题**: Modbus TCP协议层直接调用 `dgiot_data:insert/2`, `dgiot_device:save/2`, `dgiot_tdengine_adapter:save_sql/2`
**根因**: `modbus_tcp.erl`（协议解析层）承担了存储职责
**解决**: 提取存储调用到业务层，协议层改为纯解析函数
**影响**: 破坏七层架构边界，导致耦合度高

### 案例3: HJT212/GB26875统一存储模式
**问题**: HJT212和GB26875协议层统一调用 `dgiot_task:save_td/4`
**根因**: 协议处理层直接处理数据存储，未通过业务层
**解决**: 分离协议解析和存储逻辑，建立回调机制
**模式**: 多个地址类型（系统地址、设备地址、用户ID）统一存储接口

### 案例4: 通信服务器层存储调用
**问题**: Modbus RTU服务器层调用 `dgiot_task:save_td/4`
**根因**: 通信层承担了数据存储职责
**解决**: 通信层只负责网络连接，数据传递给协议层和业务层处理
**层级**: 通信层→协议层→业务层→存储层的完整链条
