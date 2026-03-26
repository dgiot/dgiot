%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_fixture_commands - 治具指令管理模块
%%% 集中管理治具的指令列表，提供更新到产品的函数及指令码到值的映射。
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_fixture_commands).

-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    update/0,
    update/1,
    get_commands/0,
    code_to_value/1,
    command_name/1,
    command_desc/1
]).

%% 产品ID常量（治具产品）
-define(FIXTURE_PRODUCT_ID, <<"bd49cc8272">>).

%% ==================== 治具指令定义 ====================
%% 指令列表格式：{Code, Name, Description, SendValue}
%% 其中 SendValue 为 Modbus 写命令时使用的值（16位）
commands() ->
    [
        {1,  <<"控制大继电器上电"/utf8>>, <<"控制大继电器给无人机上电"/utf8>>, 16#FF00},
        {2,  <<"控制大继电器断电"/utf8>>, <<"控制大继电器断电"/utf8>>, 16#0000},
        {3,  <<"启动无人机"/utf8>>, <<"启动无人机电源"/utf8>>, 16#FF00},
        {4,  <<"关闭无人机"/utf8>>, <<"关闭无人机电源"/utf8>>, 16#0000},
        {5,  <<"风速管堵上"/utf8>>, <<"风速管堵上操作"/utf8>>, 16#FF00},
        {6,  <<"风速管打开"/utf8>>, <<"风速管打开操作"/utf8>>, 16#0000},
        {7,  <<"测试引信9,10点电阻"/utf8>>, <<"测试引信9和10点之间的电阻"/utf8>>, 16#0001},
        {8,  <<"测试引信7,8点电阻"/utf8>>, <<"测试引信7和8点之间的电阻"/utf8>>, 16#0002},
        {9,  <<"测试引信7和后翼安装钉电阻"/utf8>>, <<"测试引信7和后翼安装钉之间的电阻"/utf8>>, 16#0004},
        {10, <<"测试引信8和后翼安装钉电阻"/utf8>>, <<"测试引信8和后翼安装钉之间的电阻"/utf8>>, 16#0006},
        {11, <<"测无人机电池端口电阻"/utf8>>, <<"测试无人机电池端口的电阻"/utf8>>, 16#0008},
        {12, <<"测试引信5点与地电压"/utf8>>, <<"测试引信5点与地之间的电压"/utf8>>, 16#000A},
        {13, <<"测试引信1点与地电压"/utf8>>, <<"测试引信1点与地之间的电压"/utf8>>, 16#0008},
        {14, <<"读取工位信息"/utf8>>, <<"读取工位地址和信息"/utf8>>, 16#000D},
        {15, <<"PC与治具通讯检测"/utf8>>, <<"PC通过Modbus-RTU功能码05置位线圈3检测通讯状态(值:16#FF00=置位,16#0000=复位)"/utf8>>, 16#FF00},
        {16, <<"PC控制治具测试"/utf8>>, <<"PC通过Modbus-RTU功能码06写寄存器13控制测试启动(03)/结束(09)"/utf8>>, 16#0003}
    ].

%% ==================== API 函数 ====================

%% @doc 获取所有治具指令（返回 map 列表，用于前端展示或产品存储）
-spec get_commands() -> list(map()).
get_commands() ->
    [ #{<<"code">> => Code, <<"name">> => Name, <<"description">> => Desc} || {Code, Name, Desc, _} <- commands() ].

%% @doc 根据指令码获取发送值（用于后端 Modbus 命令构建）
-spec code_to_value(integer()) -> integer().
code_to_value(Code) ->
    case lists:keyfind(Code, 1, commands()) of
        {Code, _, _, Value} -> Value;
        false -> 16#0000   % 默认值
    end.

%% @doc 根据指令码获取指令名称
-spec command_name(integer()) -> binary().
command_name(Code) ->
    case lists:keyfind(Code, 1, commands()) of
        {Code, Name, _, _} -> Name;
        false -> <<"未知指令"/utf8>>
    end.

%% @doc 根据指令码获取指令描述
-spec command_desc(integer()) -> binary().
command_desc(Code) ->
    case lists:keyfind(Code, 1, commands()) of
        {Code, _, Desc, _} -> Desc;
        false -> <<>>   % 未知指令无描述
    end.

%% @doc 更新治具产品中的指令集（使用默认产品ID）
-spec update() -> ok | {error, term()}.
update() ->
    update(?FIXTURE_PRODUCT_ID).

%% @doc 更新指定产品的指令集（通常为治具产品）
-spec update(binary()) -> ok | {error, term()}.
update(ProductId) ->
    Commands = get_commands(),
    CommandSets = #{<<"modbus">> => Commands},
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, Product} ->
            Content = maps:get(<<"content">>, Product, #{}),
            NewContent = Content#{<<"command_sets">> => CommandSets},
            case dgiot_parse:update_object(<<"Product">>, ProductId, #{<<"content">> => NewContent}) of
                {ok, _} ->
                    ?LOG(info, "治具产品 ~s 指令集更新成功", [ProductId]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "治具产品 ~s 指令集更新失败: ~p", [ProductId, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG(error, "获取治具产品 ~s 失败: ~p", [ProductId, Reason]),
            {error, Reason}
    end.