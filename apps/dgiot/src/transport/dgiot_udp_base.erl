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

%% @doc UDP公共基类模块
%% 提供通用的UDP功能函数和错误处理
-module(dgiot_udp_base).
-author("johnliu").
-include("../../include/logger.hrl").

%% API导出
-export([
    parse_addr/1,
    validate_port/1,
    format_error/1,
    get_local_ip/0,
    is_valid_ip/1
]).

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 解析地址字符串
parse_addr(Addr) when is_list(Addr) ->
    case inet:parse_address(Addr) of
        {ok, IP} -> {ok, IP};
        {error, Reason} -> {error, {invalid_address, Addr, Reason}}
    end;
parse_addr(Addr) when is_tuple(Addr) ->
    {ok, Addr};
parse_addr(_) ->
    {error, invalid_address_type}.

%% @doc 验证端口号
validate_port(Port) when is_integer(Port), Port >= 0, Port =< 65535 ->
    true;
validate_port(_) ->
    false.

%% @doc 格式化错误信息
format_error({invalid_address, Addr, Reason}) ->
    io_lib:format("Invalid address ~p: ~p", [Addr, Reason]);
format_error(invalid_address_type) ->
    "Invalid address type, expected string or tuple";
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% @doc 获取本地IP地址
get_local_ip() ->
    case inet:getifaddrs() of
        {ok, IfAddrs} ->
            get_first_non_loopback_ip(IfAddrs);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 检查是否为有效IP地址
is_valid_ip(IP) when is_tuple(IP) ->
    case inet:ntoa(IP) of
        {error, _} -> false;
        _ -> true
    end;
is_valid_ip(IP) when is_list(IP) ->
    case inet:parse_address(IP) of
        {ok, _} -> true;
        _ -> false
    end;
is_valid_ip(_) ->
    false.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 获取第一个非回环IP地址
get_first_non_loopback_ip(IfAddrs) ->
    case lists:foldl(fun({_IfName, Props}, Acc) ->
        case proplists:get_value(addr, Props) of
            {A, _B, _C, _D} = Addr when A =/= 127 ->
                case proplists:get_value(flags, Props) of
                    Flags when is_list(Flags) ->
                        case lists:member(up, Flags) of
                            true -> [Addr | Acc];
                            false -> Acc
                        end;
                    _ -> Acc
                end;
            _ -> Acc
        end
    end, [], IfAddrs) of
        [] -> {error, no_non_loopback_ip};
        [IP | _] -> {ok, IP}
    end.
