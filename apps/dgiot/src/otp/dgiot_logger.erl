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
    test/1]).

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
    emqx_logger:set_log_level(dgiot_utils:to_atom(Level));

set_loglevel(<<"app">>, Name, Level) ->
    logger:set_application_level(dgiot_utils:to_atom(Name), dgiot_utils:to_atom(Level));

set_loglevel(<<"module">>, Name, Level) ->
    logger:set_module_level(dgiot_utils:to_atom(Name), dgiot_utils:to_atom(Level));

set_loglevel(Type, _Name, _Level) ->
    {error, <<Type/binary, " error">>}.
