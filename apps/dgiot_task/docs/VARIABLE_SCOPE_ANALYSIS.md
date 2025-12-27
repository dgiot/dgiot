# DG-IoT公式变量作用域分析：是否只有解码器内的变量之间计算才可以用？

## 概述

本文档详细分析DG-IoT平台中公式变量的作用域问题，特别是回答"是否只有解码器内的变量之间计算才可以用"这个问题。

## 1. 核心问题澄清

### 问题：只有解码器内的变量之间计算才可以用？

**答案：不是的。DG-IoT支持多层级变量计算，不仅限于解码器内的变量。**

## 2. 变量作用域层级

### 2.1 三个变量作用域层级

#### 层级1：解码器级别变量（Decoder Level Variables）
- **来源**：当前数据包解析结果
- **作用域**：当前数据包解析过程
- **生命周期**：短暂（毫秒级）
- **示例**：`raw_temp`, `block_data[0:2]`, `voltage_a`

#### 层级2：设备级别变量（Device Level Variables）
- **来源**：设备历史数据、状态、配置
- **作用域**：单个设备
- **生命周期**：中长期（小时/天级）
- **示例**：`%%{last_hour_avg}`, `%%{yesterday_total}`, `%%{device_status}`

#### 层级3：设备组级别变量（Device Group Level Variables）
- **来源**：同产品所有设备的统计信息
- **作用域**：同产品所有设备
- **生命周期**：长期（天/月级）
- **示例**：`%%{group_avg_power}`, `%%{max_temperature}`, `%%{device_count}`

## 3. 变量计算能力分析

### 3.1 解码器内变量计算

#### 支持的计算类型
```erlang
%% 示例1：解码器内变量直接计算
"raw_temp * 0.0625"  # 原始温度转实际温度

%% 示例2：解码器内多个变量计算  
"active_power / sqrt(active_power*active_power + reactive_power*reactive_power)"

%% 示例3：数据块提取计算
"block_data[0:2] * 0.1 + block_data[2:4] * 0.2"
```

#### 技术实现
```erlang
%% 在dgiot_task_service.erl中
string2value(Str, Type, Specs) ->
    %% 解析表达式，可以包含解码器内变量
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    
    %% 创建绑定环境（包含解码器变量）
    Bindings = erl_eval:add_binding('raw_temp', 2500, erl_eval:new_bindings()),
    
    %% 计算表达式
    {value, Result, _} = erl_eval:exprs(Exprs, Bindings),
    Result.
```

### 3.2 跨层级变量计算

#### 支持的计算类型
```erlang
%% 示例1：解码器变量 + 设备历史变量
"%%{current_power} - %%{last_hour_avg_power}"  # 当前功率与历史平均比较

%% 示例2：解码器变量 + 设备组变量
"%%{output} / %%{group_max_output} * 100"  # 相对产出百分比

%% 示例3：多层级混合计算
"(%%{current_temp} - %%{last_day_avg}) / %%{group_std_dev} * 10 + 50"
```

#### 技术实现
```erlang
%% 在dgiot_task_service.erl中
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    %% 1. 获取解码器变量（当前数据）
    DecoderVars = Calculated,
    
    %% 2. 获取设备级别变量（历史数据）
    DeviceVars = get_device_variables(ProductId, DevAddr),
    
    %% 3. 获取设备组级别变量（统计信息）
    GroupVars = get_group_variables(ProductId),
    
    %% 4. 合并变量环境（优先级：解码器 > 设备 > 设备组）
    AllVars = maps:merge(GroupVars, maps:merge(DeviceVars, DecoderVars)),
    
    %% 5. 计算公式（可以访问所有层级变量）
    calculate_formula(Formula, AllVars).
```

## 4. 实际应用示例

### 4.1 纯解码器变量计算

#### 场景：实时功率因数计算
```json
{
  "identifier": "power_factor",
  "dataForm": {
    "strategy": "计算值",
    "collection": "active_power / sqrt(active_power*active_power + reactive_power*reactive_power)"
  }
}
```
**说明**：`active_power`和`reactive_power`都是当前数据包解析出的解码器变量。

### 4.2 解码器变量 + 设备变量计算

#### 场景：能耗趋势分析
```json
{
  "identifier": "energy_trend",
  "dataForm": {
    "strategy": "计算值", 
    "collection": "%%{current_energy} - %%{energy_at_same_time_yesterday}"
  }
}
```
**说明**：
- `%%{current_energy}`：解码器变量（当前读数）
- `%%{energy_at_same_time_yesterday}`：设备变量（历史数据）

### 4.3 解码器变量 + 设备组变量计算

#### 场景：设备效率排名
```json
{
  "identifier": "efficiency_rank",
  "dataForm": {
    "strategy": "计算值",
    "collection": "%%{output_power} / %%{group_avg_power} * 100"
  }
}
```
**说明**：
- `%%{output_power}`：解码器变量（当前输出功率）
- `%%{group_avg_power}`：设备组变量（同产品设备平均功率）

### 4.4 多层级混合计算

#### 场景：智能告警阈值
```json
{
  "identifier": "smart_alarm",
  "dataForm": {
    "strategy": "计算值",
    "collection": "(%%{current_temp} - %%{last_hour_avg}) > (%%{group_std_dev} * 3)"
  }
}
```
**说明**：
- `%%{current_temp}`：解码器变量
- `%%{last_hour_avg}`：设备变量
- `%%{group_std_dev}`：设备组变量

## 5. 技术实现细节

### 5.1 变量解析机制

```erlang
%% 变量解析函数
resolve_variables(Formula, Context) ->
    %% Context包含所有层级变量
    #{decoder_vars := DecoderVars,
      device_vars := DeviceVars, 
      group_vars := GroupVars} = Context,
    
    %% 变量替换（支持%%{var}格式）
    replace_variables(Formula, DecoderVars, DeviceVars, GroupVars).

%% 变量替换实现
replace_variables(Formula, DecoderVars, DeviceVars, GroupVars) ->
    %% 1. 替换设备组变量 %%{group_var}
    Formula1 = replace_group_vars(Formula, GroupVars),
    
    %% 2. 替换设备变量 %%{device_var}  
    Formula2 = replace_device_vars(Formula1, DeviceVars),
    
    %% 3. 替换解码器变量（直接变量名，无%%{}）
    Formula3 = replace_decoder_vars(Formula2, DecoderVars),
    
    Formula3.
```

### 5.2 变量优先级机制

```erlang
%% 变量查找优先级
get_variable_value(Name, Context) ->
    #{decoder_vars := DecoderVars,
      device_vars := DeviceVars,
      group_vars := GroupVars} = Context,
    
    %% 优先级：解码器变量 > 设备变量 > 设备组变量
    case maps:get(Name, DecoderVars, undefined) of
        undefined ->
            case maps:get(Name, DeviceVars, undefined) of
                undefined ->
                    maps:get(Name, GroupVars, 0);  % 默认值
                DeviceValue -> DeviceValue
            end;
        DecoderValue -> DecoderValue
    end.
```

## 6. 限制和约束

### 6.1 计算性能约束

#### 实时计算限制
- **解码器变量计算**：无限制，实时计算
- **设备变量计算**：可能涉及历史数据查询，有性能影响
- **设备组变量计算**：可能涉及统计计算，性能影响较大

#### 优化策略
```erlang
%% 缓存设备变量
get_device_variables(ProductId, DevAddr) ->
    CacheKey = {device_vars, ProductId, DevAddr},
    case dgiot_data:get(CacheKey) of
        not_find ->
            Vars = query_device_history(ProductId, DevAddr),
            dgiot_data:insert(CacheKey, Vars, 300),  % 缓存5分钟
            Vars;
        Vars -> Vars
    end.

%% 缓存设备组变量  
get_group_variables(ProductId) ->
    CacheKey = {group_vars, ProductId},
    case dgiot_data:get(CacheKey) of
        not_find ->
            Vars = calculate_group_statistics(ProductId),
            dgiot_data:insert(CacheKey, Vars, 1800),  % 缓存30分钟
            Vars;
        Vars -> Vars
    end.
```

### 6.2 数据一致性约束

#### 时间窗口约束
- **解码器变量**：当前数据包时间戳
- **设备变量**：可能涉及不同时间点的历史数据
- **设备组变量**：统计计算的时间窗口可能不一致

#### 解决方案
```erlang
%% 时间窗口对齐
align_time_windows(CurrentTime, Variables) ->
    %% 对齐所有变量到相同时间窗口
    AlignedVars = maps:map(fun(Name, Value) ->
        align_to_time_window(Name, Value, CurrentTime)
    end, Variables),
    AlignedVars.
```

## 7. 最佳实践

### 7.1 变量使用建议

#### ✅ 推荐做法：
1. **实时计算用解码器变量**：性能最好，无延迟
2. **趋势分析用设备变量**：需要历史数据参与
3. **对比分析用设备组变量**：需要设备间对比
4. **混合计算要谨慎**：注意性能和数据一致性

#### ❌ 避免做法：
1. **频繁查询历史数据**：影响性能
2. **大范围统计计算**：实时计算性能差
3. **不一致的时间窗口**：数据可比性差
4. **过度复杂的混合计算**：难以调试和维护

### 7.2 性能优化建议

1. **缓存历史数据**：减少数据库查询
2. **预计算统计信息**：定时计算设备组变量
3. **异步计算**：耗时的计算异步处理
4. **监控告警**：监控公式计算性能

## 8. 总结

### 8.1 核心结论

**不是只有解码器内的变量之间计算才可以用。DG-IoT支持三个层级的变量计算：**

1. **解码器级别变量**：当前数据包解析结果
2. **设备级别变量**：设备历史数据和状态
3. **设备组级别变量**：同产品设备统计信息

### 8.2 计算能力总结

#### 支持的计算类型：
1. **纯解码器变量计算**：实时，高性能
2. **解码器 + 设备变量计算**：趋势分析，中等性能
3. **解码器 + 设备组变量计算**：对比分析，较低性能
4. **多层级混合计算**：复杂业务逻辑，需要优化

#### 变量访问语法：
- **解码器变量**：直接变量名（如`raw_temp`）
- **设备变量**：`%%{variable_name}`格式
- **设备组变量**：`%%{group_variable_name}`格式

### 8.3 架构优势

1. **灵活性**：支持多层级变量计算
2. **实用性**：满足各种业务场景需求
3. **性能优化**：根据不同场景选择最优计算方式
4. **扩展性**：易于添加新的变量层级

### 8.4 实际意义

1. **对业务开发**：可以定义复杂的业务计算逻辑
2. **对系统性能**：可以根据场景优化计算性能
3. **对数据质量**：可以保证数据的一致性和可比性
4. **对系统维护**：清晰的变量层级和计算逻辑

DG-IoT平台的这种多层级变量计算能力为工业物联网应用提供了强大的数据处理能力，能够满足从简单实时计算到复杂业务分析的各类需求。
