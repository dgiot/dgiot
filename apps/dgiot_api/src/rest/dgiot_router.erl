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

-module(dgiot_router).
-author("johnliu").
-include("dgiot_api.hrl").
-include_lib("dgiot/include/logger.hrl").

%% API
-export([path/0, get_paths/2, get_state/1, parse_path/6]).
-export([get_state_by_operation/1, get_operation_id/2, get_swagger_routes/2]).

%% Static Callback
-export([init/2]).
-export([malformed_request/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([last_modified/2]).
-export([generate_etag/2]).
-export([get_file/2]).
-export([search_route/0]).


%% 获取路径
get_paths(Name, DocRoot) ->
    Routes = [
        {"/mod/:Mod/:Fun", ?MODULE, mod},
        {"/swagger/list", ?MODULE, swagger_list},
        {"/install/:Product", ?MODULE, install},
        {"/dgiotproxy/[...]/", dgiot_proxy, proxy},
        {"/[...]/", ?MODULE, {index, DocRoot}},
        {"/[...]", ?MODULE, {dir, DocRoot, []}}
    ],
    ExtRoutes = search_route(),
    lists:concat([ExtRoutes, get_swagger_routes(Name, "swagger-base.json"), ?ROUTER(DocRoot) ++ Routes]).

path() ->
    dgiot_data:match({{'$1', router}, '$2'}).

search_route() ->
    Fun =
        fun({_App, _Vsn, Mod}, Acc) ->
            case code:is_loaded(Mod) of
                false ->
                    Acc;
                _ ->
                    lists:foldl(fun(X, Acc1) ->
                        case X of
                            {route_path, Route} ->
                                Acc1 ++ [{Route, Mod, Mod}];
                            _ ->
                                Acc1
                        end
                                end, Acc, Mod:module_info(attributes))
            end
        end,
    dgiot_plugin:check_module(Fun, []).

get_swagger_routes(Name, BasePath) ->
    Fun =
        fun(Mod, Path, Method, MethodInfo, SWSchema) ->
            Routes = parse_path(Mod, Path, Method, MethodInfo, SWSchema, #{}),
            NewRoutes =
                lists:foldl(
                    fun({RealPath, RestMod, State}, Acc) ->
                        case lists:keyfind(RealPath, 1, Acc) of
                            {RealPath, RestMod, OldState} ->
                                NewOAcc = lists:keydelete(RealPath, 1, Acc),
                                NewState = maps:merge(OldState, State),
                                [{RealPath, RestMod, NewState} | NewOAcc];
                            false ->
                                [{RealPath, RestMod, State} | Acc]
                        end
                    end, get(routes), Routes),
            put(routes, NewRoutes),
            maps:without([<<"basePath">>], MethodInfo)
        end,
    put(routes, []),
    SWSchema = dgiot_swagger:generate(Name, BasePath, Fun),
    Info = maps:get(<<"info">>, SWSchema, #{}),
    Version = maps:get(<<"version">>, Info),
    dgiot_swagger:write(Name, Version, SWSchema),
    SwaggerPath = lists:concat(["/", "swagger", ".json"]),
    [{SwaggerPath, ?MODULE, {swagger, Name}} | erase(routes)].

parse_path(Mod, Path, Method, MethodInfo, SWSchema, Init) ->
    % 将swagger路径表达方式转成cowboy表达方式
    NewPath = re:replace(Path, <<"(\\{([^\\}]+)\\})">>, <<":\\2">>, [global, {return, binary}]),
    NewMethod = list_to_binary(string:to_upper(binary_to_list(Method))),
    BasePath = maps:get(<<"basePath">>, SWSchema),
    Info = maps:get(<<"info">>, SWSchema, #{}),
    OperationId = maps:get(<<"operationId">>, MethodInfo),
    Config = Init#{
        base_path => BasePath,
        version => maps:get(<<"version">>, Info),
        operationid => OperationId,
        authorize => get_security(MethodInfo, SWSchema),
        consumes => get_consumes(MethodInfo, SWSchema),
        produces => get_produces(MethodInfo, SWSchema),
        check_request => get_check_request(MethodInfo, SWSchema),
        check_response => get_check_response(MethodInfo, SWSchema)
    },
    dgiot_data:insert(?DGIOT_SWAGGER, OperationId, get_check_request(MethodInfo, SWSchema)),
    RealPath = dgiot_httpc:url_join([BasePath, NewPath]),
    {ok, Id} = set_state(OperationId, Config#{logic_handler => Mod}),
    State = #{
        logic_handler => Mod,
        NewMethod => Id
    },
    _MockPath = dgiot_httpc:url_join([<<"/mock/">>, NewPath]),
    [{Path1, dgiot_rest, State} || Path1 <- [RealPath]].

%%%===================================================================
%%% 回调函数
%%%===================================================================

init(Req, {index, DocRoot}) ->
    Path = cowboy_req:path(Req),
    case binary:at(Path, byte_size(Path) - 1) of
        $/ ->
            Index = dgiot_httpc:url_join([DocRoot, Path, "index.html"]),
            init(Req, {file, Index, []});
        _ ->
            init(Req, {dir, DocRoot, []})
    end;

%% hand swagger.json
init(Req0, swagger_list) ->
    Req =
        case dgiot_swagger:list() of
            {ok, List} ->
                Swaggers =
                    lists:foldl(
                        fun({Name, Version}, Acc) ->
                            [#{
                                app => Name,
                                path => list_to_binary(lists:concat(["/", Name, ".json"])),
                                version => Version
                            } | Acc]
                        end, [], List),
                dgiot_req:reply(200, ?HEADER#{
                    <<"content-type">> => <<"application/json; charset=utf-8">>
                }, jsx:encode(Swaggers), Req0);
            {error, Reason} ->
                dgiot_req:reply(500, ?HEADER#{
                    <<"content-type">> => <<"application/json; charset=utf-8">>
                }, jsx:encode(#{error => dgiot_utils:to_binary(Reason)}), Req0)
        end,
    {ok, Req, swagger_list};

init(Req0, {swagger, Name} = Opts) ->
    Config = dgiot_req:parse_qs(Req0, [{return, map}]),
    Req =
        case dgiot_swagger:read(Name, Config) of
            {ok, Schema} ->

                dgiot_req:reply(200, ?HEADER#{
                    <<"content-type">> => <<"application/json; charset=utf-8">>
                }, jsx:encode(filter(Schema)), Req0);
            {error, Reason} ->
                dgiot_req:reply(500, ?HEADER#{
                    <<"content-type">> => <<"application/json; charset=utf-8">>
                }, jsx:encode(#{error => dgiot_utils:to_binary(Reason)}), Req0)
        end,
    {ok, Req, Opts};

%% install
init(Req0, install) ->
    Product = dgiot_req:binding(<<"Product">>, Req0),
    #{peer := {IpPeer, _}} = Req0,
    Req =
        case inet:parse_address(dgiot_utils:to_list(dgiot_req:host(Req0))) of
            {ok, IpPeer} ->
                Port = cowboy_req:port(Req0),
                {ok, Name} = dgiot_data:lookup({Port, httpd}),
                case catch (dgiot_install:start(#{product => Product, webserver => Name})) of
                    {Type, Reason} when Type == 'EXIT'; Type == error ->
                        dgiot_req:reply(500, ?HEADER#{
                            <<"content-type">> => <<"application/json; charset=utf-8">>
                        }, jsx:encode(#{error => list_to_binary(io_lib:format("~p", [Reason]))}), Req0);
                    Result ->
                        dgiot_req:reply(200, #{
                            <<"content-type">> => <<"application/json; charset=utf-8">>
                        }, jsx:encode(Result), Req0)
                end;
            _ ->
                dgiot_req:reply(200, #{
                    <<"content-type">> => <<"application/json; charset=utf-8">>
                }, jsx:encode(#{<<"result">> => <<"forbid">>}), Req0)
        end,
    {ok, Req, install};

%% 所有服务器回调总入口
init(Req0, mod) ->
    Mod = dgiot_req:binding(<<"Mod">>, Req0),
    Fun = dgiot_req:binding(<<"Fun">>, Req0),
%%    ?LOG(error,"Mod ~p Fun ~p",[Mod,Fun]),
    Req =
        case catch apply(list_to_atom(binary_to_list(Mod)), list_to_atom(binary_to_list(Fun)), [Req0]) of
            {Err, Reason} when Err == 'EXIT'; Err == error ->
                Msg = jsx:encode(#{error => dgiot_utils:to_binary(Reason)}),
                dgiot_req:reply(500, ?HEADER#{
                    <<"content-type">> => <<"application/json; charset=utf-8">>
                }, Msg, Req0);
            {ok, Req1} ->
                Req1
        end,
    {ok, Req, mod};

%% 允许浏览器跨域请求
init(Req, Opts) ->
    case dgiot_req:method(Req) of
        <<"OPTIONS">> ->
            case ?ACCESS_CONTROL_ALLOW_HEADERS of
                false ->
                    dgiot_req:reply(403, ?HEADER, <<>>, Req);
                Header ->
                    dgiot_req:reply(200, ?HEADER#{
                        <<"access-control-allow-headers">> => Header
                    }, <<>>, Req)
            end;
        _ ->
            cowboy_static:init(Req, Opts)
    end.

malformed_request(Req, State) ->
    cowboy_static:malformed_request(Req, State).

forbidden(Req, State) ->
    cowboy_static:forbidden(Req, State).

content_types_provided(Req, State) ->
    cowboy_static:content_types_provided(Req, State).

resource_exists(Req, State) ->
    cowboy_static:resource_exists(Req, State).

generate_etag(Req, State) ->
    cowboy_static:generate_etag(Req, State).

last_modified(Req, State) ->
    cowboy_static:last_modified(Req, State).

get_file(Req, State) ->
    cowboy_static:get_file(Req, State).


%%%===================================================================
%%% 内部函数
%%%===================================================================

get_check_request(Map, SWSchema) ->
    Parameters = maps:get(<<"parameters">>, Map, #{}),
    lists:foldl(
        fun(#{<<"name">> := Name} = Parameter, Acc) ->
            Parameter1 = maps:without([<<"name">>, <<"description">>, <<"schema">>, <<"default">>], Parameter),
            case maps:get(<<"schema">>, Parameter, no) of
                #{<<"$ref">> := <<"#/definitions/", DefName/binary>>} ->
                    [{DefName, Parameter1#{
                        <<"properties">> => get_definitions(DefName, SWSchema)
                    }} | Acc];
                _ ->
                    [{Name, Parameter1} | Acc]
            end
        end, [], Parameters).


get_check_response(Map, SWSchema) ->
    Responses = maps:get(<<"responses">>, Map, #{}),
    Fun =
        fun(Code, Response, Acc) ->
            HTTPCode = binary_to_integer(Code),
            Acc#{
                HTTPCode => parse_ref_schema(Response, SWSchema)
            }
        end,
    maps:fold(Fun, #{}, Responses).

parse_ref_schema(#{<<"description">> := _} = Schema, SWSchema) ->
    parse_ref_schema(maps:without([<<"description">>], Schema), SWSchema);
parse_ref_schema(#{<<"schema">> := Schema}, SWSchema) ->
    parse_ref_schema(Schema, SWSchema);
parse_ref_schema(#{<<"$ref">> := <<"#/definitions/", Name/binary>>}, SWSchema) ->
    get_definitions(Name, SWSchema);
parse_ref_schema(#{<<"type">> := <<"array">>, <<"items">> := Items} = Rule, SWSchema) ->
    Array = parse_ref_schema(Items, SWSchema),
    Rule#{<<"items">> => Array};
parse_ref_schema(#{<<"type">> := <<"object">>, <<"properties">> := Properties} = Rule, SWSchema) ->
    NewProps = maps:fold(
        fun(Name, Rule1, Acc) ->
            Map = parse_ref_schema(Rule1, SWSchema),
            Acc#{Name => Map}
        end, #{}, Properties),
    maps:without([<<"description">>], maps:merge(Rule, NewProps));
parse_ref_schema(Item, _SWSchema) ->
    Item.

get_definitions(Name, SWSchema) ->
    Definitions = maps:get(<<"definitions">>, SWSchema, #{}),
    Definition = maps:get(Name, Definitions), %% to do
    parse_ref_schema(Definition, SWSchema).


get_operation_id(Path, Method) ->
    OId =
        case re:run(Path, <<"[a-zA-Z0-9]+">>, [global, {capture, all, list}]) of
            {match, M0} ->
                [F | T] = lists:concat(M0),
                M =
                    case lists:member(F, ["get", "put", "delete", "post"]) of
                        true -> T;
                        false -> [F | T]
                    end,
                list_to_binary(string:join(M, "_"));
            _ ->
                re:replace(Path, <<"[^a-zA-Z0-9]">>, <<>>, [global, [{return, binary}]])
        end,
    dgiot_req:to_lower(<<Method/binary, "_", OId/binary>>, [{return, atom}]).

%% cookie认证要放到列表最后
get_security(Map, SWSchema) ->
    SecurityDefinitions = maps:get(<<"securityDefinitions">>, SWSchema, #{}),
    AllTypes = maps:keys(SecurityDefinitions),
    GlobalSecurity = maps:get(<<"security">>, SWSchema, []),
    SecurityList = maps:get(<<"security">>, Map, GlobalSecurity),
    Fun =
        fun(Map1, Acc) ->
            Types = maps:keys(Map1),
            CTypes = Types -- (Types -- AllTypes),
            Security = maps:with(CTypes, SecurityDefinitions),
            maps:fold(fun format_security/3, Acc, Security)
        end,
    lists:foldr(Fun, [], SecurityList).

%% swagger2.0 不支持cookie认证，3.0才认证
format_security(<<"CookieAuth">> = Key, Auth, Acc) ->
    [{Key, Auth#{<<"in">> => <<"cookie">>, <<"type">> => <<"CookieAuth3.0">>}} | Acc];
format_security(Key, Auth, Acc) ->
    [{Key, Auth} | Acc].

get_consumes(Map, SWSchema) ->
    maps:get(<<"consumes">>, Map, maps:get(<<"consumes">>, SWSchema, [])).

get_produces(Map, SWSchema) ->
    maps:get(<<"produces">>, Map, maps:get(<<"produces">>, SWSchema, [])).


set_state(OperationId, State) ->
    NewIdx =
        case dgiot_data:lookup({router, index}) of
            {error, not_find} -> 0;
            {ok, Index} -> Index + 1
        end,
    true = dgiot_data:insert({router, index}, NewIdx),
    true = dgiot_data:insert({NewIdx, router}, {OperationId, State}),
    {ok, NewIdx}.

get_state(Index) ->
    dgiot_data:lookup({Index, router}).

get_state_by_operation(OperationId) ->
    case dgiot_data:match({{'$1', router}, {OperationId, '$2'}}) of
        {ok, [[Index, State]]} ->
            {ok, {Index, State}};
        {error, empty} ->
            {error, not_find}
    end.

%% todo 过滤swaager文件
filter(Schema) ->
%%    io:format("~s ~p ~p ~n", [?FILE, ?LINE, maps:keys(Schema)]),
%%              Tags = maps:get(<<"tags">>,Schema),
    Paths = maps:get(<<"paths">>, Schema),
    NewPath =
        maps:fold(fun
                      (<<"/level/", _/binary>>, _V, Acc) ->
                          Acc;
%%                                  (<<"/classes/", _/binary>>, _V, Acc) ->
%%                                      Acc;
                      (<<"/schema/", _/binary>>, _V, Acc) ->
                          Acc;
                      (K, V, Acc) ->
                          Acc#{K => V}
                  end, #{}, Paths),
    List = lists:keysort(1, maps:to_list(NewPath)),
    Schema#{
        <<"paths">> => maps:from_list(List)
    }.