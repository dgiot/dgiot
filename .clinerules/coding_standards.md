# DG-IoT编码规范

## 概述

本文件定义了DG-IoT平台的编码规范和最佳实践，确保代码质量和一致性。

## Erlang/OTP编码规范

### 1. 模块结构
```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% 模块说明
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_example).

%% API
-export([start/0, stop/0, parse_packet/1]).

%% 内部函数
-export([]).

%% 包含文件
-include("dgiot.hrl").
-include_lib("emqx/include/emqx.hrl").

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 启动函数
start() ->
    ok.

%% @doc 停止函数
stop() ->
    ok.

%% @doc 解析数据包
%% @spec parse_packet(binary()) -> {ok, map()} | {error, term()}
parse_packet(Packet) when is_binary(Packet) ->
    case Packet of
        <<16#EB, 16#90, Rest/binary>> ->
            {ok, #{magic => 16#EB90, data => Rest}};
        _ ->
            {error, invalid_magic}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private
%% @doc 内部处理函数
process_data(Data) ->
    % 处理逻辑
    Data.
```

### 2. 函数规范

#### 2.1 函数注释
```erlang
%% @doc 函数说明
%% @spec function_name(Type1, Type2) -> ReturnType
%% @param Param1 参数1说明
%% @param Param2 参数2说明
%% @returns 返回值说明
function_name(Param1, Param2) ->
    % 函数体
    ok.
```

#### 2.2 函数命名
- **动词+名词**：`parse_packet/1`, `send_message/2`
- **查询函数**：`get_device_status/1`, `find_by_id/1`
- **设置函数**：`set_config/2`, `update_status/2`
- **检查函数**：`check_connection/0`, `is_valid/1`
- **转换函数**：`to_binary/1`, `from_json/1`

### 3. 数据类型

#### 3.1 二进制处理
```erlang
% 正确的二进制模式匹配
parse_binary(<<Magic:16, Length:16, Data:Length/binary, Rest/binary>>) ->
    {ok, #{magic => Magic, length => Length, data => Data}, Rest}.

% 避免使用bitstring，除非必要
```

#### 3.2 记录和映射
```erlang
% 使用记录定义数据结构
-record(device, {
    id :: binary(),
    name :: binary(),
    status :: online | offline,
    last_seen :: integer()
}).

% 使用映射进行数据处理
parse_to_map(Packet) ->
    #{magic => 16#EB90, data => Packet}.
```

### 4. 错误处理

#### 4.1 返回值约定
```erlang
% 成功返回 {ok, Result}
% 失败返回 {error, Reason}
% 可选返回 {ok, Result, Extra}
% 异步操作返回 {async, Pid}
```

#### 4.2 异常处理
```erlang
% 使用try-catch处理可能抛出异常的操作
safe_operation(Data) ->
    try
        do_risky_operation(Data)
    catch
        error:badarg ->
            {error, invalid_argument};
        error:Reason ->
            {error, Reason}
    end.

% 使用case进行模式匹配错误处理
parse_with_validation(Packet) ->
    case validate_packet(Packet) of
        true ->
            parse_packet(Packet);
        false ->
            {error, invalid_packet}
    end.
```

### 5. 进程和并发

#### 5.1 进程启动
```erlang
% 使用gen_server模式
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% 使用简单进程
spawn_worker() ->
    spawn_link(fun worker_loop/0).
```

#### 5.2 消息传递
```erlang
% 定义消息格式
-record(message, {
    type :: atom(),
    payload :: term(),
    timestamp :: integer()
}).

% 发送消息
send_message(Pid, Type, Payload) ->
    Pid ! #message{type = Type, payload = Payload, timestamp = erlang:system_time()}.
```

## 代码组织

### 1. 目录结构
```
apps/dgiot_plugin/
├── src/                    # 源代码
│   ├── dgiot_plugin.erl   # 主模块
│   ├── dgiot_parser.erl   # 协议解析
│   └── dgiot_handler.erl  # 消息处理
├── include/               # 头文件
│   └── dgiot_plugin.hrl
├── test/                  # 测试代码
│   ├── dgiot_plugin_test.erl
│   └── test_data/        # 测试数据
├── priv/                  # 私有资源
│   ├── config/           # 配置文件
│   └── capture/          # 抓包文件
└── README.md             # 插件文档
```

### 2. 文件命名
- **模块文件**：`dgiot_plugin.erl`
- **测试文件**：`dgiot_plugin_test.erl`
- **头文件**：`dgiot_plugin.hrl`
- **配置文件**：`config.yaml` 或 `config.json`

### 3. 导入和导出

#### 3.1 导出函数
```erlang
% 只导出必要的API函数
-export([start/0, stop/0, parse/1, send/2]).

% 避免导出内部函数
```

#### 3.2 导入模块
```erlang
% 按功能分组导入
-include_lib("kernel/include/logger.hrl").
-include_lib("emqx/include/emqx.hrl").
-include("dgiot.hrl").
```

## 性能优化

### 1. 二进制处理优化
```erlang
% 使用二进制推导式
parse_binary(Data) ->
    <<Magic:16, Length:16, Payload:Length/binary, CRC:16>> = Data,
    #{magic => Magic, length => Length, payload => Payload, crc => CRC}.

% 避免频繁的二进制拼接
```

### 2. 内存管理
```erlang
% 使用ETS进行数据缓存
init_ets() ->
    ets:new(?MODULE, [named_table, public, {keypos, 1}]).

% 及时清理不再使用的数据
```

### 3. 进程管理
```erlang
% 使用监督树管理进程
start_children() ->
    supervisor:start_child(?SUPERVISOR, child_spec()).

% 监控进程状态
monitor_process(Pid) ->
    erlang:monitor(process, Pid).
```

## 日志打印规范

### 1. 日志格式标准

```erlang
% 标准日志格式 - 包含文件名和行号
io:format("~s ~p Event = ~p.~n", [?FILE, ?LINE, Event])

% 带模块上下文的日志
io:format("~s ~p [~p] ~p = ~p.~n", [?FILE, ?LINE, Module, Action, Data])

% 调试日志
io:format("~s ~p DEBUG: ~p = ~p~n", [?FILE, ?LINE, VariableName, VariableValue])

% 错误日志
io:format("~s ~p ERROR: ~p~n", [?FILE, ?LINE, Reason])

% 成功日志
io:format("~s ~p SUCCESS: ~p~n", [?FILE, ?LINE, Result])
```

### 2. 日志级别

- **debug**: 调试信息，用于开发阶段
- **info**: 普通信息，用于记录正常操作
- **warning**: 警告信息，用于记录潜在问题
- **error**: 错误信息，用于记录错误情况
- **critical**: 严重错误，需要立即处理

### 3. 日志最佳实践

1. **包含足够上下文**：每条日志应包含文件名、行号、模块名和操作
2. **避免敏感信息**：不要在日志中记录密码、密钥等敏感信息
3. **结构化数据**：使用易于解析的格式记录结构化数据
4. **适当的日志级别**：根据信息重要性选择合适的日志级别
5. **性能考虑**：避免在高频循环中打印过多日志

### 4. 示例

```erlang
%% @doc 获取API Token
%% @spec get_api_token(Url, Username, Password) -> {ok, Token} | {error, Reason}
get_api_token(Url, Username, Password) ->
    io:format("~s ~p [API] 开始获取Token，用户名: ~p~n", [?FILE, ?LINE, Username]),
    
    try
        % 构建请求
        RequestBody = #{userName => Username, password => Password},
        Headers = [{"Content-Type", "application/json"}],
        
        case dgiot_http_client:request(post, {Url, Headers, "application/json", jsx:encode(RequestBody)}) of
            {ok, #{<<"code">> := 1, <<"response">> := #{<<"token">> := Token}} = Response} ->
                io:format("~s ~p [API] Token获取成功: ~p~n", [?FILE, ?LINE, maps:size(Response)]),
                {ok, Token};
            {ok, #{<<"code">> := Code, <<"message">> := Message}} ->
                io:format("~s ~p [API] Token获取失败，错误码: ~p, 消息: ~p~n", [?FILE, ?LINE, Code, Message]),
                {error, Message};
            {error, Reason} ->
                io:format("~s ~p [API] HTTP请求失败: ~p~n", [?FILE, ?LINE, Reason]),
                {error, Reason}
        end
    catch
        error:Reason ->
            io:format("~s ~p [API] 异常: ~p~n", [?FILE, ?LINE, Reason]),
            {error, Reason}
    end.
```

## 测试规范

### 1. 单元测试结构
```erlang
-module(dgiot_example_test).

-include_lib("eunit/include/eunit.hrl").

%% 测试集
parse_test_() ->
    [
        {"测试有效报文", fun test_valid_packet/0},
        {"测试无效报文", fun test_invalid_packet/0},
        {"测试边界条件", fun test_edge_cases/0}
    ].

test_valid_packet() ->
    Packet = <<16#EB, 16#90, 0, 0, 0, 0>>,
    ?assertEqual({ok, #{magic => 16#EB90}}, dgiot_example:parse_packet(Packet)).

test_invalid_packet() ->
    Packet = <<0, 0, 0, 0, 0, 0>>,
    ?assertEqual({error, invalid_magic}, dgiot_example:parse_packet(Packet)).
```

### 2. 集成测试
```erlang
% 创建端到端测试
end_to_end_test() ->
    {ok, Pid} = dgiot_plugin:start_link(),
    Packet = generate_test_packet(),
    {ok, Result} = dgiot_plugin:parse(Packet),
    ?assert(is_map(Result)),
    ok = dgiot_plugin:stop(Pid).
```

## 文档规范

### 1. 模块文档
```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_example 模块
%%%
%%% 这个模块用于处理示例协议的数据包解析和发送。
%%%
%%% == 功能 ==
%%% 1. 解析示例协议数据包
%%% 2. 验证数据包完整性
%%% 3. 生成响应数据包
%%%
%%% == 协议格式 ==
%%% ```
%%% +--------+--------+--------+--------+
%%% | 魔术字 | 长度   | 数据   | CRC    |
%%% | 2字节  | 2字节  | N字节  | 2字节  |
%%% +--------+--------+--------+--------+
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
```

### 2. 函数文档
```erlang
%% @doc 解析数据包
%% 
%% 这个函数用于解析示例协议的数据包，验证魔术字和CRC校验。
%%
%% == 参数 ==
%% <ul>
%% <li>`Packet` - 二进制数据包，最小长度6字节</li>
%% </ul>
%%
%% == 返回值 ==
%% <ul>
%% <li>`{ok, Map}` - 解析成功，返回解析后的数据映射</li>
%% <li>`{error, Reason}` - 解析失败，返回错误原因</li>
%% </ul>
%%
%% == 示例 ==
%% ```
%% {ok, Result} = dgiot_example:parse_packet(<<16#EB, 16#90, 0, 0, 0, 0>>).
%% ```
parse_packet(Packet) ->
    % 函数实现
```

## 代码审查清单

### 1. 语法检查
- [ ] 没有编译警告
- [ ] 函数规范正确
- [ ] 类型标注完整

### 2. 功能检查
- [ ] 错误处理完整
- [ ] 边界条件处理
- [ ] 性能考虑

### 3. 测试检查
- [ ] 单元测试覆盖
- [ ] 集成测试完整
- [ ] 测试数据充分

### 4. 文档检查
- [ ] 模块文档完整
- [ ] 函数文档清晰
- [ ] 示例代码正确

## 更新记录

- 2025-12-03：创建编码规范文档
- 基于Erlang/OTP最佳实践和DG-IoT项目需求

## 参考链接

- [Erlang编程规则](http://www.erlang.se/doc/programming_rules.shtml)
- [OTP设计原则](http://erlang.org/doc/design_principles/des_princ.html)
- [EUnit用户指南](http://erlang.org/doc/apps/eunit/chapter.html)
