# 新插件开发模板

## 概述

本模板用于指导新插件的开发流程，确保符合DG-IoT平台的开发规范。

## 1. 项目初始化

### 1.1 创建目录结构
```
apps/dgiot_<plugin_name>/
├── src/                    # 源代码目录
│   ├── dgiot_<plugin_name>.erl      # 主模块
│   ├── dgiot_<plugin_name>_parser.erl # 协议解析模块
│   └── dgiot_<plugin_name>_handler.erl # 消息处理模块
├── include/               # 头文件目录
│   └── dgiot_<plugin_name>.hrl
├── test/                  # 测试目录
│   ├── dgiot_<plugin_name>_test.erl
│   └── test_data/        # 测试数据
├── priv/                  # 私有资源目录
│   ├── config/           # 配置文件
│   └── capture/          # 抓包文件（如有）
└── README.md             # 插件文档
```

### 1.2 更新项目配置
在 `apps/dgiot_<plugin_name>/src/dgiot_<plugin_name>.app.src` 中添加插件配置。

## 2. 协议分析

### 2.1 协议文档
- 协议名称：
- 协议版本：
- 通信方式（TCP/UDP/串口）：
- 端口/波特率：
- 数据包格式：

### 2.2 字段定义
| 字段名 | 偏移量 | 长度 | 类型 | 描述 | 示例值 |
|--------|--------|------|------|------|--------|
|        |        |      |      |      |        |

### 2.3 命令字定义
| 命令字 | 名称 | 功能描述 | 请求格式 | 响应格式 |
|--------|------|----------|----------|----------|
|        |      |          |          |          |

## 3. 代码开发

### 3.1 主模块模板
```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_<plugin_name> 模块 - <协议名称>协议插件
%%%
%%% 支持功能：
%%% 1. <功能1>
%%% 2. <功能2>
%%% 3. <功能3>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_<plugin_name>).

%% API
-export([start/0, stop/0, parse_packet/1, encode_packet/1]).

%% 内部函数
-export([]).

-include("dgiot_<plugin_name>.hrl").
-include_lib("dgiot/include/dgiot.hrl").

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 启动插件
start() ->
    io:format("~s ~p Starting ~p plugin...~n", [?FILE, ?LINE, ?MODULE]),
    ok.

%% @doc 停止插件
stop() ->
    io:format("~s ~p Stopping ~p plugin...~n", [?FILE, ?LINE, ?MODULE]),
    ok.

%% @doc 解析数据包
%% @spec parse_packet(binary()) -> {ok, map()} | {error, term()}
parse_packet(Packet) when is_binary(Packet) ->
    io:format("~s ~p Parsing packet: ~p~n", [?FILE, ?LINE, Packet]),
    % TODO: 实现协议解析逻辑
    {ok, #{packet => Packet}}.

%% @doc 编码数据包
%% @spec encode_packet(map()) -> {ok, binary()} | {error, term()}
encode_packet(Data) when is_map(Data) ->
    io:format("~s ~p Encoding data: ~p~n", [?FILE, ?LINE, Data]),
    % TODO: 实现协议编码逻辑
    {ok, <<>>}.

%%%===================================================================
%%% 内部函数
%%%===================================================================
```

### 3.2 头文件模板
```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_<plugin_name> 头文件
%%%
%%% 包含协议相关的宏定义和记录定义
%%%
%%% @end
%%%-------------------------------------------------------------------

%% 魔术字定义
-define(MAGIC_NUMBER, 16#0000).  % TODO: 设置正确的魔术字

%% 命令字定义
-define(CMD_UNKNOWN, 16#00).     % 未知命令

%% 协议版本
-define(PROTOCOL_VERSION, "1.0").

%% 记录定义
-record(<plugin_name>_packet, {
    magic :: integer(),
    length :: integer(),
    command :: integer(),
    data :: binary(),
    crc :: integer()
}).
```

## 4. 测试开发

### 4.1 单元测试模板
```erlang
-module(dgiot_<plugin_name>_test).

-include_lib("eunit/include/eunit.hrl").
-include("dgiot_<plugin_name>.hrl").

%% 测试集
parse_packet_test_() ->
    [
        {"测试有效报文", fun test_valid_packet/0},
        {"测试无效报文", fun test_invalid_packet/0}
    ].

test_valid_packet() ->
    % TODO: 创建测试数据
    Packet = <<>>,
    ?assertMatch({ok, _}, dgiot_<plugin_name>:parse_packet(Packet)).

test_invalid_packet() ->
    Packet = <<0, 0, 0, 0>>,
    ?assertMatch({error, _}, dgiot_<plugin_name>:parse_packet(Packet)).
```

### 4.2 测试数据
在 `test/test_data/` 目录下创建测试数据文件：
- `valid_packets.bin` - 有效报文示例
- `invalid_packets.bin` - 无效报文示例
- `edge_cases.bin` - 边界条件测试数据

## 5. 文档编写

### 5.1 README.md 模板
```markdown
# dgiot_<plugin_name> 插件

## 概述
<插件功能描述>

## 协议支持
- 协议名称：<协议名称>
- 协议版本：<版本号>
- 支持命令：<命令列表>

## 安装和使用

### 编译插件
```bash
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_<plugin_name>).'
```

### 加载插件
```bash
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_<plugin_name>).'
```

### 测试插件
```bash
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_<plugin_name>:test().'
```

## API 文档

### parse_packet/1
解析数据包

**参数：**
- `Packet` - 二进制数据包

**返回值：**
- `{ok, Map}` - 解析成功，返回解析后的数据映射
- `{error, Reason}` - 解析失败，返回错误原因

### encode_packet/1
编码数据包

**参数：**
- `Data` - 数据映射

**返回值：**
- `{ok, Binary}` - 编码成功，返回二进制数据包
- `{error, Reason}` - 编码失败，返回错误原因

## 测试数据
测试数据位于 `test/test_data/` 目录。

## 故障排除
<常见问题及解决方案>

## 更新记录
- <日期>：初始版本
```

## 6. 开发流程检查清单

### 6.1 代码质量
- [ ] 没有编译警告
- [ ] 函数规范注释完整
- [ ] 错误处理完善
- [ ] 日志格式符合规范

### 6.2 测试覆盖
- [ ] 单元测试覆盖核心功能
- [ ] 测试数据充分
- [ ] 边界条件测试
- [ ] 错误场景测试

### 6.3 文档完整
- [ ] README.md 完整
- [ ] API 文档清晰
- [ ] 协议文档详细
- [ ] 故障排除指南

## 7. 热编译和热加载

### 7.1 开发命令
```bash
# 热编译
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_<plugin_name>).'

# 热加载
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_<plugin_name>).'

# 在线测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_<plugin_name>:test().'
```

### 7.2 API调试
```bash
# 新增API后更新Schema
dgiot_parse_utils:update_schemas_json().
```

## 8. 部署和发布

### 8.1 版本管理
- 使用语义化版本号
- 更新 CHANGELOG.md
- 打标签发布

### 8.2 配置管理
- 环境特定的配置文件
- 敏感信息使用环境变量
- 配置验证和默认值

---

**提示：** 将 `<plugin_name>` 替换为实际的插件名称，将 `<协议名称>` 替换为实际的协议名称。
