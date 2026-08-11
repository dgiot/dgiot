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

-module(dgiot_logger).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").

-export([
    set_loglevel/3,
<<<<<<< HEAD
    test/1,
    log_once/3,
    log_once/4,
    clear_log_once/0,
    clear_log_once/1,
    get_log_once_keys/0]).
=======
    test/1]).
>>>>>>> origin/dgaiot-plugins

test(N) ->
%%    Test = <<"test">>,
    ?MLOG(info, #{<<"test">> => <<"中文"/utf8>>}),
    ?MLOG(info, #{test1 => test1}),
    lists:map(fun(X) ->
        timer:sleep(2),
    ?MLOG(info, #{<<"test">> => X, <<"name">> => <<"中文"/utf8>>, <<"time">> => dgiot_datetime:now_microsecs()}, ['acl_test'])
        end,lists:seq(1,N)).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

%% 获取系统日志等级  emqx_logger:get_primary_log_level().
%% 设置系统日志等级  emqx_logger:set_log_level(debug).

%% 获取app日志等级  emqx_logger:get_primary_log_level().
%% 设置app日志等级  logger:set_application_level(dgiot,debug).

%% 获取module日志等级  logger:get_module_level(dgiot)
%% 设置module日志等级  logger:set_module_level(dgiot_wechat,debug)
set_loglevel(<<"system">>, <<"dgiot">>, Level) ->
<<<<<<< HEAD
    emqx_logger:set_log_level(dgiot_utils:to_atom(Level));
=======
    logger:set_primary_config(level, dgiot_utils:to_atom(Level));
>>>>>>> origin/dgaiot-plugins

set_loglevel(<<"app">>, Name, Level) ->
    logger:set_application_level(dgiot_utils:to_atom(Name), dgiot_utils:to_atom(Level));

set_loglevel(<<"module">>, Name, Level) ->
    logger:set_module_level(dgiot_utils:to_atom(Name), dgiot_utils:to_atom(Level));

set_loglevel(Type, _Name, _Level) ->
    {error, <<Type/binary, " error">>}.
<<<<<<< HEAD

%%--------------------------------------------------------------------
%% Log Once APIs - 每个进程只打印一次的日志
%%--------------------------------------------------------------------

%% @doc 只打印一次的日志（error级别）
%% 使用进程字典存储已打印的key，每个进程独立维护
%% @param Key 日志唯一标识（可以是atom、binary或任何term）
%% @param Format 格式字符串
%% @param Args 格式参数
-spec log_once(term(), io:format(), [term()]) -> ok.
log_once(Key, Format, Args) ->
    log_once(error, Key, Format, Args).

%% @doc 只打印一次的日志（指定级别）
%% @param Level 日志级别（debug/info/warning/error）
%% @param Key 日志唯一标识
%% @param Format 格式字符串
%% @param Args 格式参数
-spec log_once(atom(), term(), io:format(), [term()]) -> ok.
log_once(Level, Key, Format, Args) ->
    %% 使用进程字典存储：{log_once, Key} -> true
    case get({log_once, Key}) of
        true ->
            %% 已打印过，跳过
            ok;
        undefined ->
            %% 首次打印，标记为已打印
            put({log_once, Key}, true),
            %% 调用标准日志宏
            case Level of
                debug   -> ?LOG(debug, Format, Args);
                info    -> ?LOG(info, Format, Args);
                warning -> ?LOG(warning, Format, Args);
                error   -> ?LOG(error, Format, Args);
                _       -> ?LOG(info, Format, Args)
            end
    end.

%% @doc 清除所有"只打印一次"的标记（允许重新打印）
-spec clear_log_once() -> ok.
clear_log_once() ->
    %% 遍历进程字典，删除所有log_once标记
    Keys = [K || K <- get_keys(), is_log_once_key(K)],
    lists:foreach(fun(K) -> erase(K) end, Keys),
    ok.

%% @doc 清除特定key的"只打印一次"标记
-spec clear_log_once(term()) -> ok.
clear_log_once(Key) ->
    erase({log_once, Key}),
    ok.

%% @doc 获取当前进程所有"只打印一次"的key列表
-spec get_log_once_keys() -> [term()].
get_log_once_keys() ->
    [extract_key(K) || K <- get_keys(), is_log_once_key(K)].

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

%% 检查是否为log_once的key
is_log_once_key({log_once, _}) -> true;
is_log_once_key(_) -> false.

%% 提取log_once的key
extract_key({log_once, Key}) -> Key.
=======
>>>>>>> origin/dgaiot-plugins
