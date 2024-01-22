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

-module(dgiot_system_handler).
-author("dgiot").
-include_lib("dgiot/include/logger.hrl").
-behavior(dgiot_rest).
-dgiot_rest(all).
-record(api, {authorize, base_path, check_request, check_response, consumes, description, method, operationid, path, produces, summary, tags, version}).

%% API
-export([swagger_system/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/system">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_system.json">>, ?MODULE, [], priv)
swagger_system() ->
    [
        dgiot_http_server:bind(<<"/swagger_system.json">>, ?MODULE, [], priv)
    ].


%%%===================================================================
%%% 请求处理
%%%  如果登录, Context 内有 <<"user">>, version
%%%===================================================================

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: dgiot_req:req()) ->
    {Status :: dgiot_req:http_status(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map(), Req :: dgiot_req:req()}.

handle(OperationID, Args, Context, Req) ->
    Headers = #{},
    case catch do_request(OperationID, Args, Context, Req) of
        {ErrType, Reason} when ErrType == 'EXIT'; ErrType == error ->
            ?LOG(debug, "do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> list_to_binary(io_lib:format("~p", [Reason]))
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            ?LOG(debug, "do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
            ?LOG(debug, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

%% 修改域名证书
do_request(post_domain, Args, _Context, _Req) ->
    dgiot:domain(Args);

%% 产生handler代码
do_request(post_generate_api, #{<<"mod">> := Mod, <<"type">> := <<"erlang">>} = Args, _Context, _Req) ->
    SWSchema = maps:without([<<"mod">>, <<"type">>], Args),
    Module = list_to_atom(binary_to_list(<<"dgiot_", Mod/binary, "_handler">>)),
    Fun = fun(Source) -> format_val(Mod, Source) end,
    case dgiot_swagger:compile_handler(Module, SWSchema, Fun) of
        {ok, Src} when is_binary(Src) ->
            FileName = "dgiot.zip",
            case generate_erl_packet(FileName, Mod, Src, dgiot_json:encode(SWSchema)) of
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

%% System 概要: 获取节点 描述:获取节点
%% OperationId:get_nodes
%% 请求:GET /iotapi/nodes
do_request(get_nodes, _Args, _Context, _Req) ->
    Nodes = dgiot_node:get_nodes(),
    {ok, #{nodes => Nodes}};

%% System 概要: 统计获取 描述:Promethus查询曲线
%% OperationId:get_chart_version
%% 请求:GET /iotapi/chart/:version
do_request(get_chart_version, #{<<"query">> := Query, <<"start">> := Start, <<"end">> := End, <<"step">> := Step} = Args, _Context, _Req) ->
    Version = maps:get(<<"version">>, Args, <<"v1">>),
    case dgiot_stats:query_range(Version, Query, Start, End, Step) of
        {ok, StatusCode, Data} ->
            {StatusCode, Data};
        {error, Reason} ->
            {error, Reason}
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
    case dgiot_plugin:check_plugin(App) of
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

%% Log 概要: 更新日志配置 描述:更新日志级别
do_request(put_log_level, #{<<"type">> := Type, <<"name">> := Name, <<"level">> := Level}, _Context, _Req) ->
    case dgiot_logger:set_loglevel(Type, Name, Level) of
        ok ->
            LoglevelId = dgiot_parse_id:get_loglevelid(Name, Type),
            dgiot_parse:update_object(<<"LogLevel">>, LoglevelId, #{<<"level">> => Level}),
            {200, #{<<"code">> => 200, <<"msg">> => <<"SUCCESS">>}};
        {error, Reason} ->
            {400, #{<<"code">> => 400, <<"error">> => dgiot_utils:format("~p", [Reason])}}
    end;

%% Log 概要: 获取日志配置 描述:获取日志配置
do_request(get_log_level, _Args, _Context, _Req) ->
    {error, <<"TO DO">>};

%% traces 概要: 获取traces 描述:获取traces列表
do_request(get_trace, _Args, _Context, _Req) ->
%%    Data = emqx_tracer:lookup_traces(),
%%%%    NewData =
%%%%        lists:foldl(fun(X, Acc) ->
%%%%            case X of
%%%%                {{topic, Name}, {Level, _}} ->
%%%%                    Acc ++ [#{<<"name">> => dgiot_utils:to_binary(Name), <<"level">> => dgiot_utils:to_binary(Level)}];
%%%%                _ ->
%%%%                    Acc
%%%%            end
%%%%                    end, [], Data),
    {200, #{<<"code">> => 200, <<"data">> => #{}}};

%% traces 概要: traces 描述:启动，停止traces
do_request(post_trace, #{<<"action">> := Action, <<"tracetype">> := Tracetype, <<"handle">> := Handle}, _Context, _Req) ->
    Rtn =
        case Action of
            <<"start">> ->
                ?LOG(info, "Tracetype ~p, Handle ~p", [Tracetype, Handle]),
                dgiot_tracer:add_trace({dgiot_utils:to_atom(Tracetype), Handle});
            <<"stop">> ->
                dgiot_tracer:del_trace({dgiot_utils:to_atom(Tracetype), Handle});
            _Other ->
                {error, _Other}
        end,
    case Rtn of
        true ->
            {200, #{<<"code">> => 200, <<"msg">> => <<"SUCCESS">>}};
        {error, Reason} ->
            {400, #{<<"code">> => 400, <<"error">> => dgiot_utils:format("~p", [Reason])}}
    end;


%%  服务器不支持的API接口
do_request(OperationId, Args, _Context, _Req) ->
    ?LOG(error, "do request ~p,~p~n", [OperationId, Args]),
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
