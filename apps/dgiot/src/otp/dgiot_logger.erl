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
%% Logs
-export([debug/1
    , debug/2
    , debug/3
    , info/1
    , info/2
    , info/3
    , warning/1
    , warning/2
    , warning/3
    , error/1
    , error/2
    , error/3
    , critical/1
    , critical/2
    , critical/3
]).

test(N) ->
%%    Test = <<"test">>,
    ?MLOG(info, #{<<"test">> => test}),
    ?MLOG(info, #{test1 => test1}),
    lists:map(fun(X) ->
        timer:sleep(2),
    ?MLOG(info, #{<<"test">> => X, <<"time">> => dgiot_datetime:now_microsecs()}, ['acl_test'])
        end,lists:seq(1,N)).

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------
-spec(debug(unicode:chardata()) -> ok).
debug(Msg) ->
    emqx_logger:debug(Msg).

-spec(debug(io:format(), [term()]) -> ok).
debug(Format, Args) ->
    emqx_logger:debug(Format, Args).

-spec(debug(logger:metadata(), io:format(), [term()]) -> ok).
debug(Metadata, Format, Args) when is_map(Metadata) ->
    emqx_logger:debug(Format, Args, Metadata).


-spec(info(unicode:chardata()) -> ok).
info(Msg) ->
    emqx_logger:info(Msg).

-spec(info(io:format(), [term()]) -> ok).
info(Format, Args) ->
    emqx_logger:info(Format, Args).

-spec(info(logger:metadata(), io:format(), [term()]) -> ok).
info(Metadata, Format, Args) when is_map(Metadata) ->
    emqx_logger:info(Format, Args, Metadata).


-spec(warning(unicode:chardata()) -> ok).
warning(Msg) ->
    emqx_logger:warning(Msg).

-spec(warning(io:format(), [term()]) -> ok).
warning(Format, Args) ->
    emqx_logger:warning(Format, Args).

-spec(warning(logger:metadata(), io:format(), [term()]) -> ok).
warning(Metadata, Format, Args) when is_map(Metadata) ->
    emqx_logger:warning(Format, Args, Metadata).


-spec(error(unicode:chardata()) -> ok).
error(Msg) ->
    emqx_logger:error(Msg).
-spec(error(io:format(), [term()]) -> ok).
error(Format, Args) ->
    emqx_logger:error(Format, Args).
-spec(error(logger:metadata(), io:format(), [term()]) -> ok).
error(Metadata, Format, Args) when is_map(Metadata) ->
    emqx_logger:error(Format, Args, Metadata).

-spec(critical(unicode:chardata()) -> ok).
critical(Msg) ->
    logger:critical(Msg).

-spec(critical(io:format(), [term()]) -> ok).
critical(Format, Args) ->
    logger:critical(Format, Args).

-spec(critical(logger:metadata(), io:format(), [term()]) -> ok).
critical(Metadata, Format, Args) when is_map(Metadata) ->
    logger:critical(Format, Args, Metadata).

%% 获取系统日志等级  emqx_logger:get_primary_log_level().
%% 设置系统日志等级  emqx_logger:set_log_level(debug).

%% 获取app日志等级  emqx_logger:get_primary_log_level().
%% 设置app日志等级  logger:set_application_level(dgiot,debug).

%% 获取module日志等级  logger:get_module_level(dgiot)
%% 设置module日志等级  logger:set_module_level(dgiot,debug)
set_loglevel(<<"system">>, <<"dgiot">>, Level) ->

    emqx_logger:set_log_level(dgiot_utils:to_atom(Level));

set_loglevel(<<"app">>, Name, Level) ->
    logger:set_application_level(dgiot_utils:to_atom(Name), dgiot_utils:to_atom(Level));

set_loglevel(<<"module">>, Name, Level) ->
    logger:set_module_level(dgiot_utils:to_atom(Name), dgiot_utils:to_atom(Level));

set_loglevel(Type, _Name, _Level) ->
    {error, <<Type/binary, " error">>}.
