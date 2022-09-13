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

-module(dgiot_swagger).
-author("johnliu").
-behaviour(gen_server).
-include("dgiot_api.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(SERVER, ?MODULE).
-record(state, {swagger = []}).

%% API
-export([start_link/0, stop/0]).
-export([generate/3, write/3, read/2, tree/0, list/0, parse_schema/3, load_schema/3, compile_handler/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
        {ok, Pid} ->
            dgiot_data:insert({dgiot_swagger, pid}, Pid),
            {ok, Pid};
        R -> R
    end.

stop() ->
    case dgiot_data:get({dgiot_swagger, pid}) of
        not_find ->
            pass;
        Pid ->
            dgiot_data:delete({dgiot_swagger, pid}),
            gen_server:stop(Pid)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.


handle_call(list, _From, #state{swagger = List} = State) ->
    {reply, {ok, List}, State};

handle_call({write, Name, Version, Schema}, _From, #state{swagger = List} = State) ->
    case lists:keyfind(Name, 1, List) of
        false ->
            SchemaPath = get_priv(?MODULE, ?SWAGGER(Name, Version)),
            io:format("~s ~p ~p ~n", [?FILE, ?LINE, SchemaPath]),
            Reply = file:write_file(SchemaPath, jsx:encode(Schema), [write]),
            {reply, Reply, State#state{swagger = [{Name, Version} | List]}};
        {Name, _Version} ->
            {reply, {error, exist}, State}
    end;

handle_call({read, Name, Config}, _From, #state{swagger = List} = State) ->
    case lists:keyfind(Name, 1, List) of
        false ->
            {reply, {error, not_find}, State};
        {Name, CurVersion} ->
            Version = maps:get(<<"version">>, Config, CurVersion),
            Fun =
                fun(Keys) ->
                    lists:foldl(
                        fun(Key, Acc) ->
                            case maps:get(Key, Config, undefined) of
                                undefined -> Acc;
                                Value -> Acc#{Key => Value}
                            end
                        end, #{}, Keys)
                end,
            Map = Fun([<<"host">>, <<"basePath">>]),
            case load_schema(?MODULE, ?SWAGGER(Name, Version), [{labels, binary}, return_maps]) of
                {ok, Schema} ->
                    NewSchema = maps:merge(Schema, Map),
                    FinSchema =
                        case maps:get(<<"tags">>, Config, no) of
                            no ->
                                NewSchema;
                            TagsB ->
                                Tags = re:split(TagsB, <<",">>),
                                get_swagger_by_tags(NewSchema, Tags)
                        end,
                    {reply, {ok, FinSchema}, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% 内部函数
%%%===================================================================

%% 产生swagger
generate(Name, Path, Hand) ->
    generate(Name, Path, #{}, Hand).
generate(Name, Path, AccIn, Hand) ->
    check_mod(Name, Path, AccIn, Hand).

%% 写入swagger文件
write(Name, Version, Schema) ->
    gen_server:call(?SERVER, {write, Name, Version, Schema}).

%% 读取swagger文件
read(Name, Config) ->
    gen_server:call(?SERVER, {read, Name, Config}).

list() ->
    gen_server:call(?SERVER, list).

load_schema(Path, Opts, ext) ->
    case catch file:read_file(Path) of
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error, "read swagger error,~p,~p~n", [Path, Reason]),
            {error, Reason};
        {ok, Bin} ->
            case lists:member(return_maps, Opts) of
                true ->
                    case catch jsx:decode(Bin, Opts) of
                        {'EXIT', Reason} ->
                            ?LOG(error, "decode error,~p,~p~n", [Path, Reason]),
                            {error, Reason};
                        Schemas ->
                            {ok, Schemas}
                    end;
                false ->
                    {ok, Bin}
            end
    end;

load_schema(Mod, FileName, Opts) ->
    Path = get_priv(Mod, FileName),
    case catch file:read_file(Path) of
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error, "read swagger error,~p, ~p ~p~n", [Path, filename:basename(Path), Reason]),
            {error, Reason};
        {ok, Bin} ->
            case lists:member(return_maps, Opts) of
                true ->
                    case catch jsx:decode(Bin, Opts) of
                        {'EXIT', Reason} ->
                            ?LOG(error, "decode error,~p,~p~n", [Path, Reason]),
                            {error, Reason};
                        Schemas ->
                            {ok, Schemas}
                    end;
                false ->
                    {ok, Bin}
            end
    end.


%% 检查模块是否有swagger
check_mod(Name, Path, AccIn, Hand) ->
    S = application:get_env(?APP, swagger_apps, "*"),
    Apps =
        case S == "*" orelse re:run(S, "([^\\,]+)", [global, {capture, all_but_first, binary}]) of
            true -> "*";
            nomatch -> [];
            {match, List} -> lists:concat(List)
        end,
    case load_schema(?MODULE, Path, [{labels, binary}, return_maps]) of
        {ok, BaseSchemas} ->
            Fun =
                fun({App, _Vsn, Mod}, Acc) ->
                    case Apps == "*" orelse lists:member(atom_to_binary(App, utf8), Apps) of
                        false ->
                            Acc;
                        true ->
                            try
                                check_mod_swagger(Name, Mod, Acc, Hand)
                            catch
                                _ErrType:Reason ->
                                    ?LOG(error, "~p ~p", [Mod, Reason]),
                                    Acc
                            end
                    end
                end,
            dgiot_plugin:check_module(Fun, maps:merge(BaseSchemas, AccIn));
        _ -> AccIn
    end.

check_mod_swagger(ServerName, Mod, Schema, Hand) ->
    case code:is_loaded(Mod) == false of
        true ->
            Schema;
        false ->
            IsAll = length([Mod || {dgiot_rest, [all]} <- Mod:module_info(attributes)]) > 0,
            Behaviors = [Behavior || {behavior, [Behavior]} <- Mod:module_info(attributes)],
            case IsAll orelse lists:member(ServerName, Behaviors) of
                false ->
                    Schema;
                true ->
                    F =
                        fun(NewSchema, AccSchema) ->
                            parse_schema(NewSchema, AccSchema,
                                fun(Path, Method, NewMethodInfo, AccSchemas) ->
                                    Hand(Mod, Path, Method, NewMethodInfo, AccSchemas)
                                end)
                        end,
                    Functions = Mod:module_info(exports),
                    lists:foldl(
                        fun
                            ({Fun, 0}, Acc) ->
                                case binary:split(list_to_binary(atom_to_list(Fun)), <<"swagger_">>) of
                                    [<<>>, _Path] ->
                                        case Mod:Fun() of
                                            {error, _Reason} ->
                                                Acc;
                                            NewSchemas when is_list(NewSchemas) ->
                                                lists:foldl(F, Acc, NewSchemas);
                                            NewSchema when is_map(NewSchema) ->
                                                F(NewSchema, Acc)
                                        end;
                                    _ ->
                                        Acc
                                end;
                            (_, Acc) ->
                                Acc
                        end, Schema, Functions)
            end

    end.

parse_schema(NewSchema, AccSchema, Hand) ->
    % add definitions to Acc
    Definitions = maps:get(<<"definitions">>, AccSchema, #{}),
    NewDefinitions = maps:get(<<"definitions">>, NewSchema, #{}),
    Tags = maps:get(<<"tags">>, AccSchema, []),
    NewTags = maps:get(<<"tags">>, NewSchema, []),
    NewAccSchema = AccSchema#{
        <<"definitions">> => maps:merge(Definitions, NewDefinitions),
        <<"tags">> => lists:concat([Tags, NewTags])
    },
    % get paths from NewSchema
    Paths = maps:get(<<"paths">>, NewSchema, #{}),
    Fun =
        fun(Path, Methods, Acc) ->
            maps:fold(
                fun(Method, MethodInfo, Acc1) ->
                    NewPath = get_path(Path, MethodInfo),
                    do_method_fun(NewPath, Method, MethodInfo, Acc1, Hand)
                end, Acc, Methods)
        end,
    maps:fold(Fun, NewAccSchema, Paths).


do_method_fun(Path, Method, MethodInfo, SWSchemas, Hand) ->
    OperationId = dgiot_router:get_operation_id(Path, Method),
    Paths = maps:get(<<"paths">>, SWSchemas, #{}),
    MethodAcc = maps:get(Path, Paths, #{}),
    case maps:get(Method, MethodAcc, no) of
        no ->
            % ?LOG(info,"Path -> ~s, ~s ~s~n", [OperationId, Method, NewPath]),
            BinOpId = list_to_binary(io_lib:format("~p", [OperationId])),
            PreMethodInfo = MethodInfo#{
                <<"operationId">> => OperationId,
                <<"externalDocs">> => #{
                    <<"url">> => <<"https://doc.dgiotcloud.cn/doc/#", BinOpId/binary>>
                }
            },
            NewMethodInfo = Hand(Path, Method, PreMethodInfo, SWSchemas),
            SWSchemas#{
                <<"paths">> => Paths#{
                    Path => MethodAcc#{
                        Method => NewMethodInfo
                    }
                }
            };
        _ ->
            ?LOG(warning, "Path is repeat, ~p~n", [<<Method/binary, " ", Path/binary>>]),
            SWSchemas
    end.


get_path(Path, Map) when is_list(Path) ->
    get_path(list_to_binary(Path), Map);
get_path(Path0, Map) when is_binary(Path0) ->
    Parameters = maps:get(<<"parameters">>, Map, []),
    F =
        fun
            (#{<<"name">> := Name, <<"in">> := <<"path">>}, Acc) ->
                case re:run(Path0, <<"(\\{", Name/binary, "\\})">>, [global, {capture, all_but_first, binary}]) of
                    {match, _PS} -> Acc;
                    nomatch -> [<<"{", Name/binary, "}">> | Acc]
                end;
            (_, Acc) ->
                Acc
        end,
    filename:join([Path0 | lists:foldr(F, [], Parameters)]).


get_swagger_by_tags(Swagger, []) ->
    Swagger;
get_swagger_by_tags(Swagger, Tags) ->
    TagsDef = maps:get(<<"tags">>, Swagger, []),
    Definitions = maps:get(<<"definitions">>, Swagger, []),
    New =
        maps:fold(
            fun(Path, Map, #{<<"paths">> := Paths, <<"tags">> := TagsDef1, <<"definitions">> := Definitions1} = Acc) ->
                NewMap = maps:fold(
                    fun(Method, Info, Acc1) ->
                        case Tags -- maps:get(<<"tags">>, Info, []) == Tags of
                            true ->
                                maps:remove(Method, Acc1);
                            false ->
                                Acc1
                        end
                    end, Map, Map),
                case maps:size(NewMap) == 0 of
                    true ->
                        Acc;
                    false ->
                        #{
                            <<"paths">> => Paths#{Path => NewMap},
                            <<"tags">> => TagsDef1,
                            <<"definitions">> => Definitions1
                        }
                end
            end, #{<<"paths">> => #{}, <<"tags">> => TagsDef, <<"definitions">> => Definitions}, maps:get(<<"paths">>, Swagger, #{})),
    maps:merge(Swagger, New).


get_priv(Mod, <<"/", Path/binary>>) ->
    get_priv(Mod, Path);
get_priv(Mod, Path) ->
    case code:is_loaded(Mod) of
        false ->
            throw({Mod, not_loaded});
        {file, Here} ->
            Dir = filename:dirname(filename:dirname(Here)),
            filename:join([Dir, "priv/swagger/", Path])
    end.

%% 根据swagger动态编译出模块
compile_handler(Mod, Schema, Hand) ->
    case read(?WEBSERVER, #{}) of
        {ok, SWSchema} ->
            Fun =
                fun(Path, Method, MethodInfo, AccSchema) ->
                    Init = #{
                        summary => maps:get(<<"summary">>, MethodInfo, <<>>),
                        description => maps:get(<<"description">>, MethodInfo, <<>>),
                        tags => maps:get(<<"tags">>, MethodInfo, [])
                    },
                    [Route | _] = dgiot_router:parse_path(Mod, Path, Method, MethodInfo, AccSchema, Init),
                    Route
                end,
            NewSchema = parse_schema(Schema, maps:without([<<"paths">>], SWSchema), Fun),
            {TplPath, Vals, Opts} = Hand(NewSchema),
            NewOpts = [{out_dir, false} | Opts],
            case erlydtl:compile({file, TplPath}, dgiot_render, NewOpts) of
                {ok, Render} ->
                    {ok, IoList} = Render:render(Vals),
                    {ok, unicode:characters_to_binary(IoList)};
                error ->
                    ?LOG(error, "erlydtl compile ~p~n", [TplPath]),
                    {error, compile_error}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%{
%%    "label": "基础接口",
%%    "target": "#/DGIOT",
%%    "children": [
%%        {
%%        "target": "#/DGIOT/post_login",
%%        "label": "用户登录"
%%        },
%%        {
%%        "target": "#/DGIOT/post_login",
%%        "label": "退出登录"
%%        }
%%    ]
%%}
tree() ->
    case read(?WEBSERVER, #{}) of
        {ok, SWSchema} ->
            Tags = maps:get(<<"tags">>, SWSchema, []),
            Tree =
                lists:foldl(fun
                                (#{<<"description">> := Description, <<"name">> := Name}, Acc) ->
                                    Acc#{Name => #{<<"label">> => Description,
                                        <<"target">> => <<"#/", Name/binary>>,
                                        <<"children">> => []
                                    }};
                                (_, Acc) ->
                                    Acc
                            end, #{}, Tags),
            Paths = maps:with([<<"paths">>], SWSchema),
            Tree1 =
                lists:foldl(fun(Value, Acc) ->
                    lists:foldl(fun(Value1, Acc1) ->
                        lists:foldl(fun(#{<<"summary">> := Summary, <<"operationId">> := OperationId, <<"tags">> := [PathTag | _]}, Acc2) ->
                                            case maps:find(PathTag, Acc2) of
                                                {ok, #{<<"children">> := Children, <<"target">> := TagTarget} = V} ->
                                                    Path = #{<<"label">> => Summary, <<"target">> => <<TagTarget/binary, "/", OperationId/binary>>},
                                                    Acc2#{PathTag => V#{<<"children">> => Children ++ [Path]}};
                                                error ->
                                                    Acc2
                                            end
                                    end, Acc1, maps:values(Value1))
                                end, Acc, maps:values(Value))
                            end, Tree, maps:values(Paths)),
%%            [<<"Basic">>,],
            NewMap = maps:from_list(lists:sort(maps:to_list(Tree1))),
            {ok, maps:values(NewMap)};
        _ ->
            {error, #{}}
    end.
