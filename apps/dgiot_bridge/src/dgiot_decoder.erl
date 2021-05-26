%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
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

-module(dgiot_decoder).
-include_lib("dgiot/include/logger.hrl").
-export([install/2, get_funcs/1]).
-type state() :: map().


%% -----------------------------------------------------------------
%% 非必需
%% 解码器的功能定义申明
%% -----------------------------------------------------------------
-cmd(#{
    cmd => start,
    params => [],
    desc => <<"启动"/utf8>>
}).

-cmd(#{
    cmd => stop,
    params => [#{
        name => <<"addr">>,
        type => <<"string">>,
        required => true
    },#{
        name => <<"pn">>,
        type => <<"int">>,
        default => 0,
        min => 0,
        max => 100
    }],
    desc => <<"停止"/utf8>>
}).

%% -----------------------------------------------------------------
%% 非必需
%% TCP/UDP连接创建时调用
%% -----------------------------------------------------------------
-callback init(ChannelId :: binary(), State :: state()) ->
    ok | {ok, NewState :: state()} | {stop, Reason :: any()}.
-optional_callbacks([init/2]).


%% -----------------------------------------------------------------
%% 非必需
%% {mqtt, Topic :: binary(), Message :: any()} 表示收到mqtt消息
%% {message, Message :: any()} 表示通道收到的消息, 经过parse_frame解码器后的消息
%% -----------------------------------------------------------------
-callback handle_info(
        {mqtt, Topic :: binary(), Message :: any()} |
        {rule, Message :: any(), State :: state()} |
        {http, Req::any()} |
        {message, Message :: any()}, State :: state()) ->
    ok | {ok, NewState :: state()} | {stop, Reason :: any(), NewState :: state()} | {stop, Reason :: any()}.
-optional_callbacks([handle_info/2]).


%% -----------------------------------------------------------------
%% 非必需
%% Buff 表示收需要解包,
%% Rest 剩余报文，Acc 解开报文集合
%% -----------------------------------------------------------------
-callback parse_frame(Buff :: binary(), State :: state()) ->
    {Rest :: binary(), Acc :: list()} | {Rest :: binary(), Acc :: list(), NewState :: state()}.
-optional_callbacks([parse_frame/2]).


%% -----------------------------------------------------------------
%% 非必需
%% Frame 打包函数
%% ignore 忽略
%% -----------------------------------------------------------------
-callback to_frame(Frame :: map(), State :: state()) ->
   ignore | {reply, Buff :: binary()} | {reply, Buff::binary(), State :: state()}.
-optional_callbacks([to_frame/2]).




%% 安装协议
install(ProductId, Base64Code) ->
    Replace =
        <<
            "-module('", ProductId/binary, "'). ",
            "-include_lib(\"dgiot/include/logger.hrl\"). "
        >>,
    Code = re:replace(base64:decode(Base64Code), <<"-module\\([^\\)]+\\)\\.">>, Replace, [{return, binary}]),
    case catch dgiot_plugin:compile_module(Code) of
        {ok, Mod, Bin, Warnings} ->
            length(Warnings) > 0 andalso
                ?LOG(warning,"Product:~p install protocol Warnings, ~p", [ProductId, Warnings]),
            case code:load_binary(Mod, [], Bin) of
                {module, Mod} ->
                    {ok, Mod};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_funcs(ProductId) ->
    Mod = binary_to_atom(ProductId, utf8),
    [{Name, maps:without([cmd], CMD)} || {cmd, [#{cmd := Name} = CMD]} <- Mod:module_info(attributes)].

