%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_channel_manager).
-author("dgiot").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([
    get_token/2,
    hot_compile/1,
    hot_reload/1,
    verify_beam_md5/1,
    disable_channel/2,
    enable_channel/2,
    restart_channel/2,
    get_channel_status/1,
    list_channels/0,
    test/0
]).

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 获取Session Token
%% @spec get_token(Username, Password) -> {ok, SessionToken} | {error, Reason}
get_token(Username, Password) ->
    ?LOG(info, "获取Session Token - Username: ~p", [Username]),
    case dgiot_parse_auth:login_by_account(Username, Password) of
        {ok, #{<<"sessionToken">> := SessionToken}} ->
            ?LOG(info, "Session Token获取成功"),
            {ok, SessionToken};
        {error, Reason} ->
            ?LOG(error, "Session Token获取失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 热编译插件
%% @spec hot_compile(PluginName) -> {ok, ModuleList} | {error, Reason}
hot_compile(PluginName) ->
    ?LOG(info, "热编译插件: ~p", [PluginName]),
    case dgiot_plugin:compile(PluginName) of
        {ok, ModuleList} ->
            ?LOG(info, "插件 ~p 热编译成功，模块数: ~p", [PluginName, length(ModuleList)]),
            {ok, ModuleList};
        {error, Reason} ->
            ?LOG(error, "插件 ~p 热编译失败: ~p", [PluginName, Reason]),
            {error, Reason}
    end.

%% @doc 热加载插件
%% @spec hot_reload(PluginName) -> ok | {error, Reason}
hot_reload(PluginName) ->
    ?LOG(info, "热加载插件: ~p", [PluginName]),
    case dgiot_plugin:reload_plugin(PluginName) of
        ok ->
            ?LOG(info, "插件 ~p 热加载成功", [PluginName]),
            ok;
        {error, Reason} ->
            ?LOG(error, "插件 ~p 热加载失败: ~p", [PluginName, Reason]),
            {error, Reason}
    end.

%% @doc 验证beam文件MD5
%% @spec verify_beam_md5(Module) -> {ok, Md5} | {error, Reason}
verify_beam_md5(Module) ->
    ?LOG(debug, "验证beam文件MD5: ~p", [Module]),
    case code:get_object_code(Module) of
        {Module, BeamBinary, _Filename} ->
            Md5Bin = erlang:md5(BeamBinary),
            Md5Hex = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= Md5Bin]),
            ?LOG(debug, "模块 ~p MD5: ~s", [Module, Md5Hex]),
            {ok, list_to_binary(Md5Hex)};
        {error, Reason} ->
            ?LOG(error, "获取模块 ~p beam文件失败: ~p", [Module, Reason]),
            {error, Reason}
    end.

%% @doc 禁用通道
%% @spec disable_channel(ChannelId, SessionToken) -> ok | {error, Reason}
disable_channel(ChannelId, SessionToken) ->
    ?LOG(info, "禁用通道: ~s", [ChannelId]),
    Args = #{
        <<"id">> => ChannelId,
        <<"action">> => <<"disable">>
    },
    case dgiot_bridge:control_channel(Args, SessionToken) of
        {ok, _} ->
            ?LOG(info, "通道 ~s 禁用成功", [ChannelId]),
            ok;
        {error, Reason} ->
            ?LOG(error, "通道 ~s 禁用失败: ~p", [ChannelId, Reason]),
            {error, Reason}
    end.

%% @doc 启用通道
%% @spec enable_channel(ChannelId, SessionToken) -> ok | {error, Reason}
enable_channel(ChannelId, SessionToken) ->
    ?LOG(info, "启用通道: ~s", [ChannelId]),
    Args = #{
        <<"id">> => ChannelId,
        <<"action">> => <<"enable">>
    },
    case dgiot_bridge:control_channel(Args, SessionToken) of
        {ok, _} ->
            ?LOG(info, "通道 ~s 启用成功", [ChannelId]),
            ok;
        {error, Reason} ->
            ?LOG(error, "通道 ~s 启用失败: ~p", [ChannelId, Reason]),
            {error, Reason}
    end.

%% @doc 重启通道（完整流程：disable → enable）
%% @spec restart_channel(ChannelId, SessionToken) -> ok | {error, Reason}
restart_channel(ChannelId, SessionToken) ->
    ?LOG(info, "重启通道: ~s", [ChannelId]),

    %% 步骤1：禁用通道
    case disable_channel(ChannelId, SessionToken) of
        ok ->
            ?LOG(debug, "通道 ~s 已禁用，等待1秒后启用", [ChannelId]),
            timer:sleep(1000),

            %% 步骤2：启用通道
            case enable_channel(ChannelId, SessionToken) of
                ok ->
                    ?LOG(info, "通道 ~s 重启成功", [ChannelId]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "通道 ~s 启用失败: ~p", [ChannelId, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG(error, "通道 ~s 禁用失败: ~p", [ChannelId, Reason]),
            {error, Reason}
    end.

%% @doc 获取通道状态
%% @spec get_channel_status(ChannelId) -> {ok, StatusMap} | {error, Reason}
get_channel_status(ChannelId) ->
    ?LOG(debug, "查询通道状态: ~s", [ChannelId]),
    case dgiot_parsex:get_object(<<"Channel">>, ChannelId) of
        {ok, ChannelMap} ->
            Status = #{
                <<"objectId">> => maps:get(<<"objectId">>, ChannelMap),
                <<"name">> => maps:get(<<"name">>, ChannelMap),
                <<"status">> => maps:get(<<"status">>, ChannelMap),
                <<"isEnable">> => maps:get(<<"isEnable">>, ChannelMap),
                <<"type">> => maps:get(<<"type">>, ChannelMap),
                <<"cType">> => maps:get(<<"cType">>, ChannelMap),
                <<"updatedAt">> => maps:get(<<"updatedAt">>, ChannelMap)
            },
            ?LOG(debug, "通道 ~s 状态: ~p", [ChannelId, Status]),
            {ok, Status};
        {error, Reason} ->
            ?LOG(error, "查询通道 ~s 状态失败: ~p", [ChannelId, Reason]),
            {error, Reason}
    end.

%% @doc 列出所有通道
%% @spec list_channels() -> {ok, ChannelList} | {error, Reason}
list_channels() ->
    ?LOG(debug, "查询所有通道"),
    case dgiot_parsex:query_object(<<"Channel">>, #{<<"limit">> => 1000}) of
        {ok, #{<<"results">> := Channels}} ->
            ?LOG(info, "查询到 ~p 个通道", [length(Channels)]),
            {ok, Channels};
        {error, Reason} ->
            ?LOG(error, "查询所有通道失败: ~p", [Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% 测试函数
%%%===================================================================

%% @doc 测试通道管理功能
%% @spec test() -> ok | {error, Reason}
test() ->
    ?LOG(info, "=== DG-IoT 通道管理测试 ==="),

    %% 步骤1：获取Token
    {ok, Token} = get_token(<<"dgiot_dev">>, <<"dgiot_dev">>),
    ?LOG(info, "✓ 获取Token成功"),

    %% 步骤2：热编译
    {ok, _} = hot_compile(dgiot_tdengine),
    ?LOG(info, "✓ 热编译成功"),

    %% 步骤3：热加载
    ok = hot_reload(dgiot_tdengine),
    ?LOG(info, "✓ 热加载成功"),

    %% 步骤4：验证MD5
    {ok, Md5} = verify_beam_md5(dgiot_tdengine_channel),
    ?LOG(info, "✓ Beam文件MD5: ~s", [Md5]),

    %% 步骤5：重启通道
    ChannelId = <<"08c8fe76bd">>,
    ok = restart_channel(ChannelId, Token),
    ?LOG(info, "✓ 通道重启成功"),

    %% 步骤6：验证状态
    {ok, Status} = get_channel_status(ChannelId),
    ?LOG(info, "✓ 通道状态: ~p", [Status]),

    ?LOG(info, "=== 测试完成 ==="),
    ok.
