---
name: tdengine_timeseries_storage
description: TDengine时序数据存储最佳实践，总结DGIOT项目中时序数据存储的架构设计、性能优化和最佳实践
version: 1.0.0
author: Cline AI Assistant
created_date: 2026-01-21
category: database
tags: [tdengine, timeseries, storage, performance, optimization, dgiot]
trigger_phrases:
  - "时序数据存储"
  - "TDengine最佳实践"
  - "save_td函数"
  - "时序数据库优化"
  - "物联网数据存储"
  - "数据持久化策略"
  - "高性能时序存储"
---

# TDengine时序数据存储最佳实践

## 概述

本技能总结了DGIOT项目中TDengine时序数据存储的完整架构设计、性能优化策略和最佳实践。基于对`dgiot_task`模块的分析，提供了从数据采集到持久化的完整解决方案。

## 架构设计

### 三层架构设计

DGIOT采用经典的三层架构处理时序数据：

```
┌─────────────────────────────────────────────────────────────┐
│                   表现层 (Presentation Layer)                │
│  dgiot_task.erl - 接口网关，负责协议暴露和请求转发          │
└──────────────────────────────┬──────────────────────────────┘
                                │
┌──────────────────────────────▼──────────────────────────────┐
│                   业务层 (Business Layer)                    │
│  dgiot_task_service.erl - 核心业务逻辑，数据处理和转换      │
└──────────────────────────────┬──────────────────────────────┘
                                │
┌──────────────────────────────▼──────────────────────────────┐
│                   数据访问层 (Data Access Layer)            │
│  dgiot_task_dao.erl - 数据访问，缓存管理和持久化操作       │
└──────────────────────────────┬──────────────────────────────┘
                                │
┌──────────────────────────────▼──────────────────────────────┐
│                   存储层 (Storage Layer)                     │
│  TDengine - 高性能时序数据库，负责数据持久化               │
└─────────────────────────────────────────────────────────────┘
```

### 核心数据流

```
设备数据 → MQTT上报 → dgiot_task:save_td/4 → 业务处理 → TDengine存储
```

## 核心函数分析

### 1. 数据保存函数

#### `save_td/4` - 标准数据保存
```erlang
%% @doc 保存数据到TDengine
save_td(ProductId, DevAddr, Ack, _AppData) ->
    % 1. MQTT消息转发
    Topic = <<"$dg/thing/", ProductId/binary, "/", DevAddr/binary, "/properties/report">>,
    dgiot_mqttc_channel:send(ProductId, DevAddr, Topic, Ack),
    
    % 2. 数据处理流程
    case maps:size(Ack) of
        0 -> #{};
        _ ->
            % 2.1 设备在线状态更新
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            
            % 2.2 数据缓存合并
            Interval = dgiot_product:get_interval(ProductId),
            CacheData = dgiot_task_dao:merge_cache_data(DeviceId, Ack, Interval),
            
            % 2.3 物模型数据处理
            Props = get_props(ProductId),
            Collection = get_collection(ProductId, [], CacheData, Props),
            AllData = get_calculated(ProductId, DevAddr, Collection, Props),
            Storage = get_storage(AllData, Props),
            
            % 2.4 缓存和持久化
            dgiot_task_dao:save_cache_data(DeviceId, CacheData),
            dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage, Interval)
    end.
```

#### `smart_save_td/4` - 智能数据保存
```erlang
%% @doc 智能保存数据
smart_save_td(ProductId, DevAddr, Data, Context) ->
    ?LOG(info, "Smart processing data for ProductId=~p, DevAddr=~p", [ProductId, DevAddr]),
    save_td(ProductId, DevAddr, Data, Context).
```

#### `save_td_no_match/4` - 无匹配模式保存
```erlang
%% @doc 保存数据（无匹配模式）
save_td_no_match(ProductId, DevAddr, Ack, AppData) ->
    % 直接处理，跳过缓存合并
    case length(maps:to_list(Ack)) of
        0 -> #{};
        _ ->
            Props = get_props(ProductId),
            Collection = get_collection(ProductId, [], Ack, Props),
            Calculated = get_calculated(ProductId, DevAddr, Collection, Props),
            Storage = get_storage(Calculated, Props),
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            Interval = maps:get(<<"interval">>, AppData, 3),
            AllData = dgiot_task_dao:merge_cache_data(DeviceId, Storage, Interval),
            dealwith_data(ProductId, DevAddr, DeviceId, AllData, Storage, Interval),
            AllData
    end.
```

### 2. 数据处理函数

#### `get_collection/4` - 数据采集处理
```erlang
%% @doc 获取采集数据
get_collection(ProductId, Dis, Payload, Props) ->
    lists:foldl(fun(Identifier, Acc1) ->
        lists:foldl(fun(X, Acc2) ->
            case Acc2 of
                error -> Acc2;
                _ ->
                    case X of
                        #{<<"dataForm">> := #{<<"strategy">> := Strategy} = DataForm,
                          <<"dataType">> := DataType,
                          <<"identifier">> := Identifier} when Strategy =/= <<"计算值"/utf8>> ->
                            dgiot_task_data:get_userdata(ProductId, Identifier, DataForm, DataType, Payload, Acc2);
                        _ -> Acc2
                    end
            end
        end, Acc1, Props)
    end, Payload, Dis).
```

#### `get_calculated/4` - 计算值处理
```erlang
%% @doc 获取计算值
get_calculated(ProductId, DevAddr, Calculated, Props) ->
    lists:foldl(fun(X, Acc) ->
        case Acc of
            error -> Acc;
            _ ->
                case X of
                    #{<<"isaccumulate">> := true,
                      <<"isstorage">> := true,
                      <<"identifier">> := Identifier,
                      <<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>},
                      <<"dataSource">> := #{<<"key">> := Key} = DataSource} ->
                        %% 统计计算：持续时间、频率等
                        case maps:get(Key, Calculated, not_find) of
                            not_find -> Acc;
                            KeyValue -> get_statistic(ProductId, DevAddr, Key, Identifier, dgiot_utils:to_int(KeyValue), DataSource, Acc)
                        end;
                    #{<<"isstorage">> := true,
                      <<"identifier">> := Identifier,
                      <<"dataForm">> := #{<<"strategy">> := <<"计算值"/utf8>>, <<"collection">> := Collection}} ->
                        %% 公式计算：使用增强的公式计算器
                        case calculate_formula_with_enhanced_calculator(Collection, Calculated, X) of
                            undefined -> 
                                ?LOG(warning, "公式计算失败: Identifier=~p, Collection=~p", [Identifier, Collection]),
                                maps:without([Identifier], Acc);
                            Value -> Acc#{Identifier => Value}
                        end;
                    _ -> Acc
                end
        end
    end, Calculated, Props).
```

## TDengine适配器

### 核心适配器函数

```erlang
%% @doc 保存数据到TDengine
save(Product, Devaddr, Msg) when is_map(Msg) ->
    DeviceId = dgiot_parse_id:get_deviceid(Product, Devaddr),
    dgiot_device:online(DeviceId),
    do_channel(Product,
        fun(Channel) ->
            do_save(Channel, Product, Devaddr, Msg)
        end).

%% @doc 执行保存操作
do_save(Channel, Product, Devaddr, Msg) ->
    dgiot_channelx:do_message(?TYPE, Channel, {data, Product, Devaddr, Msg, #{}}, 30000).
```

### 数据格式转换

```erlang
%% @doc 格式化数据
format(#{
    <<"devaddr">> := DevAddr,
    <<"dtuaddr">> := DtuAddr,
    <<"product">> := Product,
    <<"thing">> := Things
}) ->
    Data = format_thing(Things),
    case maps:size(Data) == 0 of
        true ->
            ignore;
        false ->
            {ok, Data#{
                <<"dtuaddr">> => DtuAddr,
                <<"product">> => Product,
                <<"addr">> => DevAddr
            }}
    end.

%% @doc 格式化物模型数据
format_thing(Things) -> format_thing(Things, #{}).
format_thing([], Acc) -> Acc;
format_thing([#{<<"value">> := Value, <<"identifier">> := Id} | Other], Acc) ->
    format_thing(Other, Acc#{Id => Value}).
```

## 性能优化策略

### 1. 缓存策略

#### 数据缓存合并
```erlang
%% 在dgiot_task_dao.erl中
merge_cache_data(DeviceId, NewData, Interval) ->
    % 1. 获取现有缓存
    case dgiot_data:get({device_cache, DeviceId}) of
        not_find -> NewData;
        OldData ->
            % 2. 合并策略：基于时间间隔
            Now = dgiot_datetime:now_secs(),
            LastUpdate = maps:get(<<"_updatedAt">>, OldData, 0),
            case Now - LastUpdate >= Interval of
                true -> NewData;  % 超过间隔，使用新数据
                false ->          % 在间隔内，合并数据
                    maps:merge(OldData, NewData)
            end
    end.
```

#### 缓存数据保存
```erlang
save_cache_data(DeviceId, Data) ->
    % 添加时间戳
    DataWithTime = Data#{<<"_updatedAt">> => dgiot_datetime:now_secs()},
    dgiot_data:insert({device_cache, DeviceId}, DataWithTime).
```

### 2. 批量写入优化

#### 批量数据聚合
```erlang
%% 建议的批量写入策略
batch_save_td(ProductId, DevAddr, DataList) ->
    % 1. 数据预处理
    ProcessedList = lists:map(fun(Data) ->
        Props = get_props(ProductId),
        Collection = get_collection(ProductId, [], Data, Props),
        get_calculated(ProductId, DevAddr, Collection, Props)
    end, DataList),
    
    % 2. 批量存储
    StorageList = lists:map(fun(Calculated) ->
        Props = get_props(ProductId),
        get_storage(Calculated, Props)
    end, ProcessedList),
    
    % 3. 批量写入TDengine
    dgiot_tdengine_adapter:save_batch(ProductId, DevAddr, StorageList).
```

### 3. 异步处理

#### 异步保存模式
```erlang
%% 异步保存实现
async_save_td(ProductId, DevAddr, Data) ->
    % 使用进程池异步处理
    Pool = dgiot_pool:get_pool(tdengine_pool),
    dgiot_pool:async_call(Pool, ?MODULE, save_td, [ProductId, DevAddr, Data, #{}]).
```

## 最佳实践

### 1. 数据模型设计

#### 超级表设计原则
```sql
-- TDengine超级表设计示例
CREATE STABLE IF NOT EXISTS devices (
    ts TIMESTAMP,
    device_id NCHAR(64),
    product_id NCHAR(32),
    -- 动态标签字段
    tags JSON,
    -- 测量值字段
    temperature FLOAT,
    humidity FLOAT,
    pressure FLOAT,
    -- 元数据字段
    interval INT,
    quality INT
) TAGS (
    region NCHAR(16),
    group NCHAR(32),
    type NCHAR(16)
);
```

#### 子表命名规范
```erlang
%% 子表命名规则
get_table_name(ProductId, DevAddr) ->
    % 格式: productId_devAddr
    <<ProductId/binary, "_", DevAddr/binary>>.
```

### 2. 写入性能优化

#### 批量写入配置
```erlang
%% TDengine通道配置
tdengine_channel_config() ->
    #{
        <<"batch">> => #{
            <<"enable">> => true,
            <<"size">> => 1000,      % 批量大小
            <<"time">> => 1000,      % 批量时间(ms)
            <<"cache">> => 10000     % 缓存大小
        },
        <<"compress">> => #{
            <<"enable">> => true,
            <<"level">> => 2
        }
    }.
```

#### 连接池配置
```erlang
%% 连接池配置
tdengine_pool_config() ->
    #{
        size => 10,           % 连接池大小
        max_overflow => 20,   % 最大溢出连接
        strategy => fifo      % 连接策略
    }.
```

### 3. 查询优化

#### 索引策略
```sql
-- 创建索引
CREATE INDEX idx_device_time ON devices(device_id, ts);

-- 分区策略
CREATE STABLE devices (...) 
TAGS (...) 
PARTITION BY RANGE(ts) (
    PARTITION p2024 VALUES LESS THAN ('2025-01-01'),
    PARTITION p2025 VALUES LESS THAN ('2026-01-01')
);
```

#### 查询缓存
```erlang
%% 查询缓存实现
cached_query(ProductId, DevAddr, StartTime, EndTime) ->
    CacheKey = {tdengine_query, ProductId, DevAddr, StartTime, EndTime},
    case dgiot_data:get(CacheKey) of
        not_find ->
            Result = do_query(ProductId, DevAddr, StartTime, EndTime),
            dgiot_data:insert(CacheKey, Result, 300),  % 缓存5分钟
            Result;
        CachedResult -> CachedResult
    end.
```

### 4. 监控和告警

#### 性能监控指标
```erlang
%% 监控指标定义
tdengine_metrics() ->
    [
        #{
            name => <<"tdengine_write_latency">>,
            type => histogram,
            help => <<"TDengine写入延迟">>,
            labels => [product_id, operation]
        },
        #{
            name => <<"tdengine_write_throughput">>,
            type => counter,
            help => <<"TDengine写入吞吐量">>,
            labels => [product_id]
        },
        #{
            name => <<"tdengine_cache_hit_rate">>,
            type => gauge,
            help => <<"TDengine缓存命中率">>,
            labels => [product_id]
        }
    ].
```

#### 告警规则
```erlang
%% 告警规则配置
tdengine_alerts() ->
    [
        #{
            alert => <<"tdengine_high_latency">>,
            expr => <<"tdengine_write_latency_seconds{quantile=\"0.95\"} > 1">>,
            for => <<"5m">>,
            labels => #{severity => <<"warning">>},
            annotations => #{
                summary => <<"TDengine写入延迟过高">>,
                description => <<"产品 {{ $labels.product_id }} 的TDengine写入延迟超过1秒">>
            }
        },
        #{
            alert => <<"tdengine_low_throughput">>,
            expr => <<"rate(tdengine_write_throughput_total[5m]) < 100">>,
            for => <<"10m">>,
            labels => #{severity => <<"critical">>},
            annotations => #{
                summary => <<"TDengine写入吞吐量过低">>,
                description => <<"产品 {{ $labels.product_id }} 的TDengine写入吞吐量低于100条/秒">>
            }
        }
    ].
```

## 时序数据与物模型的关系

### 1. 物模型定义数据结构

物模型（Thing Model）定义了时序数据的结构和语义：

```erlang
%% 物模型属性示例
#{
    <<"identifier">> => <<"temperature">>,
    <<"name">> => <<"温度"/utf8>>,
    <<"dataType">> => #{
        <<"type">> => <<"float">>,
        <<"specs">> => #{<<"min">> => -40, <<"max">> => 85, <<"unit">> => <<"℃"/utf8>>}
    },
    <<"dataForm">> => #{
        <<"strategy">> => <<"采集值"/utf8>>,
        <<"protocol">> => <<"MODBUSRTU">>,
        <<"address">> => <<"40001">>,
        <<"quantity">> => 1,
        <<"rate">> => 1
    },
    <<"isstorage">> => true,      % 是否存储到TDengine
    <<"isaccumulate">> => false,  % 是否累计统计
    <<"accessMode">> => <<"r">>   % 访问模式：r-只读，rw-读写
}
```

### 2. 物模型处理流程

在`save_td/4`函数中，物模型处理流程如下：

```erlang
save_td(ProductId, DevAddr, Ack, _AppData) ->
    % ... MQTT转发等
    
    % 1. 获取物模型属性
    Props = get_props(ProductId),
    
    % 2. 数据采集处理（基于物模型配置）
    Collection = get_collection(ProductId, [], CacheData, Props),
    
    % 3. 计算值处理（公式计算、统计计算）
    AllData = get_calculated(ProductId, DevAddr, Collection, Props),
    
    % 4. 筛选需要存储的数据
    Storage = get_storage(AllData, Props),
    
    % 5. 存储到TDengine
    dgiot_tdengine_adapter:save(ProductId, DevAddr, Storage)
```

### 3. 物模型与TDengine表结构映射

```
物模型属性 → TDengine表字段
├── identifier → 字段名
├── dataType → 字段类型
├── isstorage → 是否创建字段
└── 其他元数据 → 标签字段
```

## Hook关联机制

### 1. 协议解析Hook

`call_protocol_hook/4`函数通过Hook机制实现协议解析：

```erlang
%% @doc 调用协议钩子
call_protocol_hook(ProductId, DevAddr, Data, Protocol) ->
    ?LOG(info, "Calling protocol hook: ProductId=~p, DevAddr=~p, Protocol=~p", [ProductId, DevAddr, Protocol]),
    case dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, Protocol}, [ProductId, DevAddr, Data]) of
        {ok, [ParsedData | _]} -> {parsed, ParsedData};
        _ -> {error, protocol_not_supported}
    end.
```

### 2. Hook注册机制

协议模块通过Hook注册实现插件化：

```erlang
%% 在协议模块中注册Hook
dgiot_hook:add_hook({?DGIOT_RAW_DATA_PARSER, <<"MODBUSRTU">>}, 
                    fun modbus_protocol:parse_raw_data/3).
```

### 3. Hook执行流程

```
原始数据 → needs_protocol_parsing/1检测 → call_protocol_hook/4调用 → Hook执行 → 解析后数据
```

### 4. Hook与物模型的协同工作

```
原始数据流：
1. 设备上报原始二进制数据
2. needs_protocol_parsing/1检测是否需要协议解析
3. 如果需要，调用call_protocol_hook/4
4. Hook根据协议类型调用对应的解析器
5. 解析后的数据映射到物模型属性
6. 根据物模型配置处理数据
7. 存储到TDengine
```

## 完整的时序数据处理架构

### 1. 架构图

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   设备数据      │───▶│   Hook解析      │───▶│   物模型映射    │
│  (原始二进制)   │    │  (协议适配)     │    │  (数据结构化)   │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  MQTT上报       │    │  数据验证       │    │  计算处理       │
│  (实时传输)     │    │  (类型检查)     │    │  (公式/统计)    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  缓存合并       │───▶│  存储筛选       │───▶│  TDengine存储   │
│  (时间窗口)     │    │  (isstorage)    │    │  (时序数据库)   │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

### 2. 关键关联点

#### 2.1 Hook与物模型的关联
- **协议解析**：Hook根据物模型中的`protocol`字段选择解析器
- **数据映射**：解析后的数据根据物模型`identifier`映射到属性
- **验证规则**：Hook可以使用物模型中的`dataType`进行数据验证

#### 2.2 物模型与TDengine的关联
- **表结构**：物模型定义TDengine表的字段结构
- **存储策略**：`isstorage`字段控制是否存储到TDengine
- **数据类型**：物模型`dataType`映射到TDengine字段类型

#### 2.3 时序数据与业务逻辑的关联
- **计算值**：物模型中的公式计算产生衍生数据
- **统计值**：基于物模型配置的统计计算（时长、频率）
- **告警规则**：基于物模型阈值配置的实时告警

## 最佳实践

### 1. Hook设计最佳实践

#### 可插拔协议支持
```erlang
%% 协议Hook注册模板
register_protocol_hook(Protocol, Module, Function) ->
    dgiot_hook:add_hook({?DGIOT_RAW_DATA_PARSER, Protocol}, 
                        fun Module:Function/3).
```

#### 协议版本管理
```erlang
%% 支持协议版本
call_protocol_hook_with_version(ProductId, DevAddr, Data, Protocol, Version) ->
    HookKey = {?DGIOT_RAW_DATA_PARSER, Protocol, Version},
    case dgiot_hook:run_hook(HookKey, [ProductId, DevAddr, Data]) of
        {ok, [ParsedData | _]} -> {parsed, ParsedData};
        _ -> 
            % 回退到默认版本
            call_protocol_hook(ProductId, DevAddr, Data, Protocol)
    end.
```

### 2. 物模型设计最佳实践

#### 分层物模型设计
```erlang
%% 基础物模型（通用属性）
base_thing_model() ->
    #{
        <<"properties">> => [
            #{
                <<"identifier">> => <<"timestamp">>,
                <<"name">> => <<"时间戳"/utf8>>,
                <<"dataType">> => #{<<"type">> => <<"timestamp">>},
                <<"isstorage">> => true,
                <<"accessMode">> => <<"r">>
            },
            #{
                <<"identifier">> => <<"deviceId">>,
                <<"name">> => <<"设备ID"/utf8>>,
                <<"dataType">> => #{<<"type">> => <<"text">>},
                <<"isstorage">> => true,
                <<"accessMode">> => <<"r">>
            }
        ]
    }.

%% 扩展物模型（产品特定）
extend_thing_model(BaseModel, ProductSpecificProps) ->
    BaseProps = maps:get(<<"properties">>, BaseModel, []),
    BaseModel#{<<"properties">> => BaseProps ++ ProductSpecificProps}.
```

#### 物模型版本控制
```erlang
%% 物模型版本管理
get_thing_model_with_version(ProductId, Version) ->
    Key = {thing_model, ProductId, Version},
    case dgiot_data:get(Key) of
        not_find ->
            % 加载最新版本
            get_latest_thing_model(ProductId);
        Model -> Model
    end.
```

### 3. 时序数据存储最佳实践

#### 数据分区策略
```sql
-- 基于时间的分区
CREATE STABLE devices (...)
PARTITION BY RANGE(ts) (
    PARTITION p_current VALUES LESS THAN (NOW + INTERVAL '1' DAY),
    PARTITION p_history VALUES LESS THAN (MAXVALUE)
);

-- 基于产品的分区
CREATE STABLE devices (...)
TAGS (product_id NCHAR(32))
PARTITION BY HASH(product_id) PARTITIONS 10;
```

#### 数据生命周期管理
```erlang
%% 自动数据清理
clean_old_timeseries_data(ProductId, RetentionDays) ->
    CutoffTime = dgiot_datetime:now_secs() - RetentionDays * 86400,
    Sql = io_lib:format(
        "DELETE FROM ~s WHERE ts < ~p",
        [get_table_name(ProductId), CutoffTime]
    ),
    dgiot_tdengine_adapter:save_sql(ProductId, list_to_binary(Sql)).
```

## 故障排除

### 常见问题及解决方案

#### 问题1: Hook解析失败
**症状**: `call_protocol_hook/4`返回`{error, protocol_not_supported}`
**解决方案**:
1. 检查协议Hook是否已注册：`dgiot_hook:list_hooks(?DGIOT_RAW_DATA_PARSER)`
2. 验证物模型中的`protocol`字段配置
3. 检查协议模块是否已加载

#### 问题2: 物模型映射错误
**症状**: 数据无法正确映射到物模型属性
**解决方案**:
1. 检查物模型`identifier`与Hook输出字段的匹配
2. 验证`dataType`配置是否正确
3. 检查`get_collection/4`函数的数据处理逻辑

#### 问题3: 时序数据存储异常
**症状**: TDengine存储失败或数据丢失
**解决方案**:
1. 检查`isstorage`字段配置
2. 验证TDengine连接和权限
3. 检查表结构是否与物模型匹配

### 调试工具

#### Hook调试脚本
```erlang
%% Hook调试函数
debug_protocol_hook(ProductId, DevAddr, Data, Protocol) ->
    ?LOG(debug, "Debugging protocol hook: ProductId=~p, DevAddr=~p, Protocol=~p", 
         [ProductId, DevAddr, Protocol]),
    
    % 1. 检查Hook注册
    Hooks = dgiot_hook:list_hooks({?DGIOT_RAW_DATA_PARSER, Protocol}),
    ?LOG(debug, "Registered hooks: ~p", [Hooks]),
    
    % 2. 执行Hook
    case dgiot_hook:run_hook({?DGIOT_RAW_DATA_PARSER, Protocol}, 
                            [ProductId, DevAddr, Data]) of
        {ok, [ParsedData | _]} ->
            ?LOG(debug, "Hook parsed data: ~p", [ParsedData]),
            {parsed, ParsedData};
        {ok, []} ->
            ?LOG(error, "Hook returned empty result"),
            {error, empty_result};
        Error ->
            ?LOG(error, "Hook error: ~p", [Error]),
            Error
    end.
```

#### 物模型验证工具
```erlang
%% 物模型验证函数
validate_thing_model(ProductId) ->
    case get_props(ProductId) of
        [] -> 
            ?LOG(error, "No thing model found for product: ~p", [ProductId]),
            {error, thing_model_not_found};
        Props ->
            % 验证必填字段
            Validated = lists:filtermap(fun(Prop) ->
                case maps:get(<<"identifier">>, Prop, undefined) of
                    undefined -> false;
                    Identifier -> 
                        case maps:get(<<"dataType">>, Prop, undefined) of
                            undefined -> 
                                ?LOG(warning, "Missing dataType for identifier: ~p", [Identifier]),
                                false;
                            _ -> {true, Prop}
                        end
                end
            end, Props),
            
            case length(Validated) == length(Props) of
                true -> {ok, Props};
                false -> 
                    ?LOG(error, "Thing model validation failed: ~p valid of ~p total", 
                         [length(Validated), length(Props)]),
                    {error, validation_failed}
            end
    end.
```

## 总结

时序数据、物模型和Hook三者形成了DGIOT物联网平台的核心数据处理架构：

1. **Hook提供协议解析能力**：实现设备数据的标准化解析
2. **物模型定义数据结构**：提供数据的语义和存储规则
3. **时序数据库负责持久化**：提供高性能的数据存储和查询

这种架构的优势：
- **灵活性**：通过Hook支持多种协议
- **标准化**：通过物模型统一数据结构
- **高性能**：通过TDengine提供时序数据优化存储
- **可扩展**：各组件独立，易于扩展和维护

通过深入理解这三者的关联关系，可以更好地设计、优化和调试物联网数据处理系统。
