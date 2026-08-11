# 插件开发工作流

## 概述

本工作流定义了DG-IoT平台插件开发的标准化流程，确保插件开发高效、规范且可维护。

## 1. 开发前准备

### 1.1 需求分析
- [ ] **明确协议类型**：确定插件支持的协议（如无人机、Modbus、BACnet等）
- [ ] **收集协议文档**：获取完整的协议规范文档
- [ ] **分析数据格式**：明确数据包结构、字段定义、命令字等
- [ ] **确定通信方式**：TCP/UDP/串口，端口号/波特率等

### 1.2 环境准备
```bash
# 1. 检查开发环境
make run  # 确保基础环境正常

# 2. 创建插件目录
cd apps && mkdir dgiot_<plugin_name>

# 3. 初始化目录结构
cp -r ../lib-extra/emqx_plugin_template/* dgiot_<plugin_name>/
```

## 2. 项目初始化阶段

### 2.1 创建标准目录结构
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

### 2.2 更新项目配置
```erlang
%% apps/dgiot_<plugin_name>/src/dgiot_<plugin_name>.app.src
{application, dgiot_<plugin_name>, [
    {description, "<插件描述>"},
    {vsn, "1.0.0"},
    {registered, []},
    {mod, {dgiot_<plugin_name>, []}},
    {applications, [kernel, stdlib, dgiot]},
    {env, []},
    {modules, []}
]}.
```

## 3. 协议解析开发阶段

### 3.1 创建头文件定义
```erlang
%% apps/dgiot_<plugin_name>/include/dgiot_<plugin_name>.hrl
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

### 3.2 实现主模块
```erlang
%% apps/dgiot_<plugin_name>/src/dgiot_<plugin_name>.erl
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

## 4. 测试开发阶段

### 4.1 创建测试数据
```bash
# 创建测试数据目录
mkdir -p apps/dgiot_<plugin_name>/test/test_data/{valid,invalid,edge_cases}

# 准备测试数据
# 1. 有效报文数据
# 2. 无效报文数据
# 3. 边界条件数据
```

### 4.2 编写单元测试
```erlang
%% apps/dgiot_<plugin_name>/test/dgiot_<plugin_name>_test.erl
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

## 5. 编译和测试阶段

### 5.1 全量编译（首次）
```bash
# 首次全量编译
make run

# 验证插件是否编译成功
_build/emqx/rel/emqx/bin/emqx eval 'application:which_applications().' | grep dgiot_<plugin_name>
```

### 5.2 热编译（日常开发）
```bash
# 热编译插件
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_<plugin_name>).'

# 热加载插件
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_<plugin_name>).'

# 运行测试
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_<plugin_name>:test().'
```

## 6. 集成测试阶段

### 6.1 创建集成测试脚本
```bash
#!/bin/bash
# apps/dgiot_<plugin_name>/test/integration_test.sh

echo "开始插件集成测试..."
echo "========================================"

# 1. 编译插件
echo "1. 编译插件..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_<plugin_name>).'

# 2. 加载插件
echo "2. 加载插件..."
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_<plugin_name>).'

# 3. 运行单元测试
echo "3. 运行单元测试..."
_build/emqx/rel/emqx/bin/emqx eval 'eunit:test(dgiot_<plugin_name>_test).'

# 4. 功能测试
echo "4. 功能测试..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_<plugin_name>:test_integration().'

echo "========================================"
echo "集成测试完成！"
```

### 6.2 性能测试
```erlang
%% 性能测试模块
-module(dgiot_<plugin_name>_performance_test).

-include_lib("eunit/include/eunit.hrl").

performance_benchmark_test_() ->
    {timeout, 60, fun run_performance_benchmark/0}.

run_performance_benchmark() ->
    % 准备测试数据
    TestData = load_performance_test_data(),
    
    % 测试解析性能
    ParseTimes = test_parse_performance(TestData, 1000),
    io:format("~s ~p Parse performance: ~p ms per packet~n", 
              [?FILE, ?LINE, lists:sum(ParseTimes) / length(ParseTimes)]),
    
    % 验证性能要求
    ?assert(lists:sum(ParseTimes) / length(ParseTimes) < 10).  % 小于10ms
```

## 7. 文档编写阶段

### 7.1 创建README.md
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

### 7.2 API文档更新
```bash
# 新增API后更新Schema
dgiot_parse_utils:update_schemas_json().
```

## 8. 质量保证阶段

### 8.1 代码审查清单
- [ ] **语法检查**：没有编译警告，函数规范正确
- [ ] **功能检查**：错误处理完整，边界条件处理
- [ ] **测试检查**：单元测试覆盖，集成测试完整
- [ ] **文档检查**：模块文档完整，函数文档清晰

### 8.2 性能验证
- [ ] 单包解析时间 < 10ms
- [ ] 内存使用合理
- [ ] 并发处理能力满足需求

## 9. 部署和发布阶段

### 9.1 版本管理
```bash
# 更新版本号
# 1. 更新 .app.src 文件中的 vsn 字段
# 2. 更新 CHANGELOG.md
# 3. 打标签发布
git tag -a v1.0.0 -m "Release dgiot_<plugin_name> v1.0.0"
```

### 9.2 配置管理
- [ ] 环境特定的配置文件
- [ ] 敏感信息使用环境变量
- [ ] 配置验证和默认值

## 10. 维护和更新阶段

### 10.1 日常维护
```bash
# 热编译更新
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_<plugin_name>).'

# 热加载更新
_build/emqx/rel/emqx/bin/emqx eval dgiot_plugin:reload_plugin(dgiot_<plugin_name>).'
```

### 10.2 问题排查
```erlang
% 启用调试日志
dgiot_<plugin_name>:set_log_level(debug).

% 查看插件状态
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:status(dgiot_<plugin_name>).'
```

## 工作流检查清单

### 阶段1：准备阶段
- [ ] 需求分析完成
- [ ] 协议文档收集
- [ ] 环境准备就绪

### 阶段2：开发阶段
- [ ] 目录结构创建
- [ ] 头文件定义完成
- [ ] 主模块实现完成
- [ ] 解析函数实现完成

### 阶段3：测试阶段
- [ ] 测试数据准备
- [ ] 单元测试编写
- [ ] 集成测试通过
- [ ] 性能测试达标

### 阶段4：文档阶段
- [ ] README.md 完成
- [ ] API 文档完整
- [ ] 故障排除指南

### 阶段5：质量保证
- [ ] 代码审查通过
- [ ] 性能验证通过
- [ ] 兼容性测试通过

### 阶段6：发布阶段
- [ ] 版本号更新
- [ ] 配置管理完成
- [ ] 部署验证通过

## 最佳实践

### 开发实践
1. **小步迭代**：每次只实现一个功能，测试通过后再继续
2. **测试驱动**：先写测试用例，再实现功能
3. **文档同步**：代码和文档同步更新
4. **版本控制**：使用Git进行版本管理，提交信息清晰

### 测试实践
1. **自动化测试**：创建自动化测试脚本
2. **真实数据**：使用真实抓包数据作为测试数据
3. **边界测试**：测试各种边界条件
4. **性能监控**：监控内存和CPU使用情况

### 部署实践
1. **灰度发布**：先在小范围测试，再全面部署
2. **回滚计划**：准备回滚方案
3. **监控告警**：设置性能监控和异常告警

## 故障排除指南

### 常见问题
1. **插件加载失败**
   - 检查依赖是否正确
   - 验证模块导出函数

2. **报文解析失败**
   - 检查魔术字是否正确
   - 验证CRC校验算法
   - 检查数据包长度

3. **性能问题**
   - 检查内存使用情况
   - 优化二进制处理
   - 减少不必要的进程创建

### 调试技巧
```erlang
% 打印调试信息
io:format("~s ~p DEBUG: ~p = ~p~n", [?FILE, ?LINE, VariableName, VariableValue]).

% 使用dbg进行调试
dbg:tracer().
dbg:p(all, c).
dbg:tpl(Module, Function, x).
```

---

**提示：** 将 `<plugin_name>` 替换为实际的插件名称，根据具体协议调整工作流细节。
