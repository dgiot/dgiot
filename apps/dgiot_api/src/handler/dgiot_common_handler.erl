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

-module(dgiot_common_handler).
-author("kenneth").
-include_lib("dgiot/include/logger.hrl").
-record(api, {authorize, base_path, check_request, check_response, consumes, description, method, operationid, path, produces, summary, tags, version}).

%% API
-export([do_request/4]).

%% 产生handler代码
do_request(generate_api, #{<<"mod">> := Mod, <<"type">> := <<"erlang">>} = Args, _Context, _Req) ->
    SWSchema = maps:without([<<"mod">>, <<"type">>], Args),
    Module = list_to_atom(binary_to_list(<<"dgiot_", Mod/binary, "_handler">>)),
    Fun = fun(Source) -> format_val(Mod, Source) end,
    case dgiot_swagger:compile_handler(Module, SWSchema, Fun) of
        {ok, Src} when is_binary(Src) ->
            FileName = "dgiot.zip",
            case generate_erl_packet(FileName, Mod, Src, jsx:encode(SWSchema)) of
                {ok, ZipFile} ->
                    Headers = #{
                        <<"content-type">> => <<"application/zip">>,
                        <<"Content-Disposition">> => list_to_binary("attachment;filename=" ++ FileName)
                    },
                    {200, Headers, ZipFile};
                Err ->
                    Err
            end;
        Err ->
            Err
    end;

%% System 概要: 获取插件 描述:获取插件
do_request(get_plugin, _Args, _Context, _Req) ->
    Filter =
        fun(App, _, _) ->
            re:run(atom_to_binary(App, utf8), <<"dgiot_.*">>) =/= nomatch
        end,
    Plugins = dgiot_plugin:applications(Filter),
    {ok,
        #{
            <<"apps">> => Plugins,
            <<"recordsFiltered">> => length(Plugins),
            <<"recordsTotal">> => length(Plugins)
        }
    };


%% System 概要: 对插件操作 描述:对插件操作, start, stop, reload
%% OperationId:post_plugin_action
%% 请求:POST /iotapi/plugin/:Action
do_request(post_plugin_app, #{<<"Action">> := Action, <<"App">> := App}, _Context, _Req) ->
    case dgiot_license:check_plugin(App) of
        true ->
            case Action of
                <<"stop">> ->
                    case lists:member(App, re:split(application:get_env(dgiot_api, app_forbidden, ""), ",")) of
                        true ->
                            {403, #{<<"error">> => <<"This App is not allow stop!">>}};
                        false ->
                            case application:stop(binary_to_atom(App, utf8)) of
                                {error, {not_started, _}} ->
                                    {400, #{<<"error">> => <<"not started">>}};
                                {error, Reason} ->
                                    {400, #{<<"error">> => dgiot_utils:format("~p", [Reason])}};
                                ok ->
                                    ok
                            end
                    end;
                <<"start">> ->
                    case application:ensure_started(binary_to_atom(App, utf8)) of
                        {error, Reason} ->
                            {400, #{<<"error">> => dgiot_utils:format("~p", [Reason])}};
                        ok ->
                            ok
                    end;
                <<"reload">> when App == <<"all">> ->
                    {ok, dgiot_plugin:reload_modules()};
                <<"reload">> ->
                    {ok, dgiot_plugin:reload_plugin(App)};
                <<"modules">> when App == <<"changed">> ->
                    {ok, dgiot_plugin:get_changed_modules()};
                <<"modules">> ->
                    {ok, dgiot_plugin:get_modules(App)}
            end;
        _ ->
            {200, #{<<"error">> => <<"license error">>}}
    end;


%% System 概要: 编译代码 描述:在线编译代码
%% OperationId:post_compile
%% 请求:POST /iotapi/compile
do_request(post_compile, #{<<"code">> := Base64Code}, _Context, _Req) ->
    Code = base64:decode(Base64Code),
    case dgiot_plugin:compile_module(Code) of
        {ok, Mod, _, Warnings} ->
            {200, #{<<"mod">> => Mod, <<"warnings">> => Warnings}};
        {error, Reason} ->
            {400, #{<<"error">> => Reason}}
    end;

%% System 概要: 集群操作 描述:加入,退出集群
%% OperationId:post_cluster
%% 请求:POST /iotapi/cluster
do_request(post_cluster, #{<<"action">> := Action, <<"node">> := N}, _Context, _Req) ->
    Node = binary_to_list(N),
    Rtn =
        case Action of
            <<"join">> ->
                dgiot_node:join(Node);
            <<"leave">> ->
                dgiot_node:leave(Node)
        end,
    case Rtn of
        ok ->
            {200, #{<<"msg">> => <<"SUCCESS">>}};
        {error, Reason} ->
            {400, #{<<"error">> => dgiot_utils:format("~p", [Reason])}}
    end;


%% Log 概要: 更新日志配置 描述:更新日志配置
do_request(put_log_level, _Args, _Context, _Req) ->
    {error, <<"TO DO">>};

%% Log 概要: 获取日志配置 描述:获取日志配置
do_request(get_log_level, _Args, _Context, _Req) ->
    {error, <<"TO DO">>};

%%  服务器不支持的API接口
do_request(OperationId, Args, _Context, _Req) ->
    ?LOG(error,"do request ~p,~p~n", [OperationId, Args]),
    {error, <<"Not Allowed.">>}.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

generate_erl_packet(ZipFile, Mod, Src, Schema) ->
    SrcPath = binary_to_list(<<"handler/dgiot_", Mod/binary, "_handler.erl">>),
    SchemaPath = binary_to_list(<<"swagger/swagger_", Mod/binary, ".json">>),
    %% 编译handler
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    ok = file:write_file(filename:join([Dir, "priv", SchemaPath]), Schema),
    FileName = filename:join([Dir, "priv", SrcPath]),
    filelib:ensure_dir(FileName),
    ok = file:write_file(FileName, Src),
    case compile:file(FileName, [binary]) of
        error ->
            {error, compile_error};
        {ok, ModuleName, Beam} ->
            BinFile = lists:concat([Dir, "/ebin/", ModuleName, ".beam"]),
            case file:write_file(BinFile, Beam) of
                ok ->
                    dgiot_plugin:reload_module(ModuleName),
                    %% 创建zip
                    case zip:create(ZipFile, [
                        {"dgiot/priv/" ++ SchemaPath, Schema},
                        {"dgiot/src/" ++ SrcPath, Src}
                    ], [memory]) of
                        {ok, {ZipFile, Bin}} ->
                            {ok, Bin};
                        {error, What} ->
                            {error, What}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.




format_val(Mod, Schema) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Tpl = dgiot_httpc:url_join([Dir, "/priv/swagger/erlang_handler.src"]),
    Paths = maps:values(maps:get(<<"paths">>, Schema, #{})),
    Apis = lists:foldl(
        fun(Methods, Acc) ->
            maps:fold(
                fun(Method, {Path, _, State}, Acc1) ->
                    NewMethod = list_to_binary(string:to_upper(binary_to_list(Method))),
                    Index = maps:get(NewMethod, State),
                    {ok, {_, Info}} = dgiot_router:get_state(Index),
                    [maps:to_list(Info#{
                        method => NewMethod,
                        path => list_to_binary(Path)
                    }) | Acc1]
                end, Acc, Methods)
        end, [], Paths),
    {Tpl, [{mod, Mod}, {apis, Apis}], [{api, record_info(fields, api)}]}.

