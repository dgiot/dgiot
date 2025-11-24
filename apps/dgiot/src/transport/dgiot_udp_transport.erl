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

%% @doc UDP传输层模块
%% 纯UDP套接字操作，不包含任何业务逻辑
-module(dgiot_udp_transport).
-author("johnliu").
-include("../../include/logger.hrl").

%% API导出
-export([
    open/1, open/2, 
    close/1,
    send/2, send/4,
    setopts/2,
    controlling_process/2,
    getopts/2
]).

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 打开UDP套接字（默认选项）
open(Port) ->
    open(Port, []).

%% @doc 打开UDP套接字（指定选项）
open(Port, Options) ->
    DefaultOptions = [binary, {active, once}, {reuseaddr, true}],
    MergedOptions = merge_options(DefaultOptions, Options),
    gen_udp:open(Port, MergedOptions).

%% @doc 关闭UDP套接字
close(Socket) ->
    gen_udp:close(Socket).

%% @doc 发送数据到指定地址
send(Socket, Addr, Port, Data) ->
    gen_udp:send(Socket, Addr, Port, Data).

%% @doc 发送数据（使用连接套接字）
send(Socket, Data) ->
    gen_udp:send(Socket, Data).

%% @doc 设置套接字选项
setopts(Socket, Options) ->
    inet:setopts(Socket, Options).

%% @doc 更改套接字控制进程
controlling_process(Socket, Pid) ->
    gen_udp:controlling_process(Socket, Pid).

%% @doc 获取套接字选项
getopts(Socket, Options) ->
    inet:getopts(Socket, Options).

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 合并选项列表
merge_options(Default, Custom) ->
    lists:foldl(fun
        (Atom, Acc) when is_atom(Atom) ->
            % 处理原子选项（如 binary, list 等）
            case lists:member(Atom, Acc) of
                true -> Acc;
                false -> [Atom | Acc]
            end;
        ({Key, Value}, Acc) ->
            % 处理键值对选项
            case lists:keymember(Key, 1, Acc) of
                true -> lists:keyreplace(Key, 1, Acc, {Key, Value});
                false -> [{Key, Value} | Acc]
            end;
        (Other, Acc) ->
            % 其他类型的选项，直接添加
            [Other | Acc]
    end, Default, Custom).
