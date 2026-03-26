# 工位逻辑扩展规则

## 概述

本规则定义了工位逻辑在基类上面再展开的设计原则和最佳实践。

## 核心原则

### 1. 基类设计原则
- **通用功能集中**：所有工位共用的功能放在基类中
- **模板方法定义**：定义标准工作流程，子类可重写特定步骤
- **钩子方法抽象**：定义子类必须实现的抽象方法

### 2. 子类实现原则
- **单一职责**：每个子类只负责一个工位的特定逻辑
- **最小化重写**：只重写需要定制的部分，复用基类功能
- **配置驱动**：工位类型通过配置决定，而不是硬编码

### 3. 工厂模式原则
- **动态创建**：根据配置动态创建相应的工位Worker
- **统一管理**：通过工厂统一管理所有工位实例
- **生命周期管理**：工厂负责工位的创建、销毁和状态管理

## 目录结构规范

```
apps/dgiot_uav/src/station/
├── dgiot_uav_station_base.erl          # 基类
├── dgiot_uav_station_factory.erl       # 工厂类
├── dgiot_uav_station_default_worker.erl # 默认工位实现
├── station1/
│   └── dgiot_uav_station1_worker.erl   # 工位1特定逻辑
├── station2/
│   └── dgiot_uav_station2_worker.erl   # 工位2特定逻辑
├── station3/
│   └── dgiot_uav_station3_worker.erl   # 工位3特定逻辑
└── station4/
    └── dgiot_uav_station4_worker.erl   # 工位4特定逻辑
```

## 代码规范

### 1. 基类代码规范
```erlang
%% 必须定义的行为
-behaviour(gen_server).

%% 必须实现的回调
-callback init_station(Config :: map()) -> {ok, State :: map()} | {error, Reason :: term()}.
-callback handle_station_data(RawData :: binary(), State :: map()) -> 
    {ok, ParsedData :: map(), NewState :: map()} | {error, Reason :: term()}.
-callback execute_station_command(CommandCode :: integer(), Params :: map(), State :: map()) -> 
    {ok, Result :: map(), NewState :: map()} | {error, Reason :: term()}.
```

### 2. 子类代码规范
```erlang
%% 必须实现的行为
-behaviour(dgiot_uav_station_base).

%% 必须导出的回调
-export([
    init_station/1,
    handle_station_data/2,
    execute_station_command/3
]).
```

### 3. 工厂代码规范
```erlang
%% 必须支持的API
-export([
    create_station/1,
    get_station_worker/1,
    list_stations/0,
    stop_station/1
]).
```

## 配置规范

### 1. 工位配置格式
```json
{
    "station_id": 1,
    "station_type": "station1",
    "station_name": "磁航向测试工位",
    "plc_base_address": 1100,
    "check_interval": 1000,
    "specific_config": {
        "magnetic_test_enabled": true,
        "special_registers": [1101, 1102, 1103]
    }
}
```

### 2. 工位类型映射
```erlang
% 工位类型到模块的映射
get_station_module(station1) -> dgiot_uav_station1_worker;
get_station_module(station2) -> dgiot_uav_station2_worker;
get_station_module(station3) -> dgiot_uav_station3_worker;
get_station_module(station4) -> dgiot_uav_station4_worker;
get_station_module(_) -> dgiot_uav_station_default_worker.
```

## 检查清单

### 基类检查清单
- [ ] 定义了必要的行为和回调
- [ ] 实现了通用功能（连接、状态、错误处理）
- [ ] 提供了模板方法
- [ ] 定义了钩子方法

### 子类检查清单
- [ ] 实现了基类定义的行为
- [ ] 只重写了必要的部分
- [ ] 遵循单一职责原则
- [ ] 有完整的错误处理

### 工厂检查清单
- [ ] 支持动态工位创建
- [ ] 实现了工位注册和管理
- [ ] 提供了统一的API接口
- [ ] 有完整的生命周期管理

### 配置检查清单
- [ ] 工位配置包含必要字段
- [ ] 工位类型映射正确
- [ ] 特定配置与工位类型匹配
- [ ] 配置验证通过

## 最佳实践

### 1. 渐进式扩展
- 先实现基类和默认工位
- 逐步添加特定工位逻辑
- 保持向后兼容

### 2. 配置驱动开发
- 通过配置决定工位行为
- 避免硬编码工位类型
- 支持运行时配置更新

### 3. 测试策略
- 基类单元测试
- 子类集成测试
- 工厂功能测试
- 端到端场景测试

## 更新记录

- 2026-01-26：创建工位逻辑扩展规则