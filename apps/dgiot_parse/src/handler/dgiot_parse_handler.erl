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

-module(dgiot_parse_handler).
-author("zwx").
-behavior(dgiot_rest).
-include_lib("dgiot/include/logger.hrl").
-dgiot_rest(all).
-define(RE_OPTIONS, [global, {return, binary}]).

%% API
-export([swagger_parse/0, init/2, handle/4]).
-export([login_by_account/2, login_by_token/2, login_by_mail_phone/1, set_cookies/3]).
-export([create_user_for_app/1, get_token/1, create_tree/2, get_classtree/4,create_session/3,add_acl/5]).

%%%===================================================================
%%% swagger Callback
%%%===================================================================

swagger_parse() ->
    S = application:get_env(dgiot_api, swagger_tables, "*"),
    Tables =
        case S == "*" orelse re:run(S, "([^\\,]+)", [global, {capture, all_but_first, binary}]) of
            true -> "*";
            nomatch -> [];
            {match, List} -> lists:concat(List)
        end,
    check_parse(Tables, 0).

check_parse(Tables, I) ->
    case catch dgiot_parse:health() of
        {ok, #{<<"status">> := <<"ok">>}} ->
            case dgiot_parse:get_schemas() of
                {ok, #{<<"results">> := Schemas}} ->
                    Fun =
                        fun(#{<<"className">> := ClassName} = Schema, Acc) ->
                            case Tables == "*" orelse lists:member(ClassName, Tables) of
                                true ->
                                    case catch get_paths(Schema, Acc) of
                                        {'EXIT', Reason} ->
                                            ?LOG(warning,"~p,~p~n", [Schema, Reason]),
                                            Acc;
                                        NewAcc ->
                                            NewAcc
                                    end;
                                false ->
                                    Acc
                            end
                        end,
                    lists:foldl(Fun, default_schemas(), Schemas);
                {error, Reason} ->
                    {error, Reason}
            end;
        {Type, Reason} when Type == error; Type == 'EXIT' ->
            case I > 5 of
                true ->
                    {error, Reason};
                false ->
                    ?LOG(error,"ParseServer is not health,~p!!!", [Reason]),
                    receive after 5000 -> check_parse(Tables, I + 1) end
            end
    end.


init(Req0, Map) ->
    {ok, Body, Req} = dgiot_req:read_body(Req0),
    case catch jsx:decode(Body, [{labels, binary}, return_maps]) of
        #{<<"_JavaScriptKey">> := _JSKey} = RecvMap ->
            Method = maps:get(<<"_method">>, RecvMap, dgiot_req:method(Req)),
            Index = maps:get(Method, Map),
            {ok, {_, Config}} = dgiot_router:get_state(Index),
            dgiot_logger:debug("Parse js call ~p,~p~n", [Config, RecvMap]),
            OperationId = maps:get(operationid, Config, not_allowed),
            Produces = maps:get(produces, Config, []),
            %% 鉴权时，我们使用的是cookies，parse js放在body里面
            Authorize =
                case maps:get(authorize, Config, []) of
                    [] -> [];
                    AuthList ->
                        [{<<"apiKey">>, #{
                            <<"in">> => <<"body">>,
                            <<"name">> => <<"_SessionToken">>
                        }} | AuthList]
                end,
            {dgiot_rest, Req, Config#{
                authorize => Authorize,
                from => js,
                operationid => OperationId,
                check_request => [],  %parse调用都是post，跟rest不一样
                check_response => #{}, %parse调用都是post，跟rest不一样
                produces => Produces,
                consumes => [<<"*">>]
            }};
        _ ->
            {no_call, Req}
    end.


%%%===================================================================
%%% 请求回调
%%%===================================================================

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: dgiot_req:req()) ->
    {Status :: dgiot_req:http_status(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map(), Req :: dgiot_req:req()}.

%% IoTDevice 概要: 获取子角色token 描述:子角色Token查询
%% OperationId: get_token
%% GET /token
handle(get_token, #{<<"name">> := Name} = _Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    case dgiot_parse:query_object(<<"_Role">>, #{
        <<"order">> => <<"updatedAt">>, <<"limit">> => 1,
        <<"where">> => #{<<"name">> => Name}}, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            check_roles(Name);
        {ok, #{<<"results">> := Roles}} when length(Roles) == 0 ->
            {404, #{<<"code">> => 101, <<"error">> => <<"User not found.">>}};
        {error, Error} ->
            {404, #{<<"code">> => 101, <<"error">> => Error}}
    end;

%% IoTDevice 概要: 获取应用token 描述:Token查询
%% OperationId: post_token
%% POST /token
handle(post_token, #{<<"appid">> := AppId, <<"secret">> := Secret}, _Context, _Req) ->
    case login_by_token(AppId, Secret) of
        {ok, UserInfo} ->
            {200, maps:with([<<"access_token">>, <<"expires_in">>, <<"desc">>, <<"name">>], UserInfo)};
        {error, #{<<"code">> := 101} = Err} ->
            {404, Err};
        {error, Err} -> {500, Err}
    end;

handle(post_user, #{<<"username">> := _UserName, <<"password">> := _Password} = Body, #{<<"sessionToken">> := SessionToken}, _Req) ->
    ?LOG(info,"Body ~p", [Body]),
    case create_user(Body, SessionToken) of
        {ok, Data} ->
            {200, Data};
        {error, Error} -> {error, Error}
    end;

handle(delete_user, #{<<"username">> := _UserName} = Body, #{<<"sessionToken">> := SessionToken}, _Req) ->
    ?LOG(info,"Body ~p", [Body]),
    case delete_user(Body, SessionToken) of
        {ok, Data} ->
            {200, Data};
        {error, Error} -> {error, Error}
    end;

handle(put_user, #{<<"username">> := _UserName} = Body, #{<<"sessionToken">> := SessionToken}, _Req) ->
    ?LOG(info,"Body ~p", [Body]),
    case put_user(Body, SessionToken) of
        {ok, Data} ->
            {200, Data};
        {error, Error} -> {error, Error}
    end;

%% parse js的调用处理
handle(OperationID, Args, #{from := js} = Context, Req) ->
    OldHeaders = dgiot_req:headers(Req, [{return, map}]),
    ReqHeader = maps:without([<<"authorization">>, <<"sessionToken">>], OldHeaders),
    {ok, Body, Req1} = dgiot_req:read_body(Req),
    Data = jsx:decode(Body, [{labels, binary}, return_maps]),
    NewBody =
        case maps:get(<<"sessionToken">>, Context, undefined) of
            undefined -> Data;
            SessionToken -> Data#{<<"_SessionToken">> => SessionToken}
        end,
    do_request_before(list_to_binary(atom_to_list(OperationID)), Args, NewBody, ReqHeader, Context, Req1);

handle(OperationID, Args, Context, Req) ->
    OldHeaders = dgiot_req:headers(Req, [{return, map}]),
    ReqHeader = maps:without([<<"authorization">>, <<"sessionToken">>], OldHeaders),
    {ok, Body, Req1} = dgiot_req:read_body(Req),
    Data =
        case Body of
            <<>> -> <<>>;
            _ -> jsx:decode(Body, [{labels, binary}, return_maps])
        end,
    Headers =
        case maps:get(<<"sessionToken">>, Context, undefined) of
            undefined -> ReqHeader;
            Token -> ReqHeader#{<<"X-Parse-Session-Token">> => Token}
        end,
    do_request_before(list_to_binary(atom_to_list(OperationID)), Args, Data, Headers, Context#{from => rest}, Req1).


%%%===================================================================
%%% 内部函数
%%%===================================================================

%% 所有请求的前置处理
do_request_before(<<"get_classes_site_id">>, _Args, _Body, _Headers, _Context, Req) ->
    Id = dgiot_req:binding(<<"Id">>, Req),
    Where =
        case Id of
            <<"default">> -> #{<<"default">> => true};
            _ -> #{<<"productIdentifier">> => Id}
        end,
    Query = #{<<"limit">> => 1, <<"where">> => Where, <<"order">> => <<"-updatedAt">>},
    case dgiot_parse:query_object(<<"Project">>, Query) of
        {ok, #{<<"results">> := []}} ->
            {404, #{<<"code">> => 101, <<"error">> => <<"Object not found.">>}};
        {ok, #{<<"results">> := [Site]}} ->
            Project = maps:with([<<"objectId">>, <<"copyright">>, <<"dashboard">>, <<"logo">>, <<"title">>, <<"background">>], Site),
            {200, Project};
        {error, Reason} ->
            {400, Reason}
    end;

% 根据当前用户，请求出所有菜单
do_request_before(<<"get_classes_navigation">>, Args, _Body, _Headers, Context, _Req) ->
    NewArgs = maps:fold(
        fun(Key, Val, Acc) ->
            case Val of undefined -> Acc;_ -> Acc#{Key => Val} end
        end, #{}, Args),
    case get_navigation(Context, NewArgs) of
        {ok, Items} ->
            Num = 1000,
            Menus = create_tree(maps:values(Items), <<"parent">>),
            Len = length(Menus),
            case Len =< Num of
                true ->
                    {200, #{<<"results">> => Menus}};
                false ->
                    ShowMenus = lists:sublist(Menus, 1, Num),
                    MoreMenus = lists:sublist(Menus, Num + 1, length(Menus) - Num),
                    {200, #{<<"results">> => ShowMenus ++ [#{
                        <<"children">> => MoreMenus,
                        <<"name">> => <<"...">>,
                        <<"icon">> => <<"More">>,
                        <<"parent">> => <<"0">>,
                        <<"url">> => <<"/more">>
                    }]}}
            end;
        {error, Reason} ->
            {500, Reason}
    end;

% 创建应用
do_request_before(<<"post_classes_app">>, _Args, #{<<"name">> := AppName} = Body, _Headers,
        #{<<"sessionToken">> := SessionToken, <<"user">> := #{<<"objectId">> := UserId}}, _Req) ->
    Where = #{
        <<"limit">> => 1, <<"order">> => <<"-updatedAt">>,
        <<"where">> => #{<<"name">> => AppName}
    },
    case dgiot_parse:query_object(<<"App">>, Where) of
        {ok, #{<<"results">> := _Info}} when length(_Info) > 0 ->
%%            ?LOG(info,"_Info ~p",[_Info]),
            {400, #{code => 202, error => <<"App already exists.">>}};
        {ok, #{<<"results">> := []}} ->
            case dgiot_auth:get_session(SessionToken) of
                #{<<"objectId">> := UserId} = UserInfo ->
                    case dgiot_parse:query_object(<<"_Role">>, #{<<"where">> => #{<<"name">> => AppName}}) of
                        {ok, #{<<"results">> := Result1}} when length(Result1) == 0 ->
                            {file, Here} = code:is_loaded(?MODULE),
                            Dir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/install/role/"]),
                            dgiot_install:generate_approle(UserInfo#{
                                <<"dir">> => Dir,
                                <<"appid">> => AppName,
                                <<"name">> => AppName}
                            );
                        _ -> pass
                    end,
                    NewBody = Body#{
                        <<"ACL">> => #{
                            <<"role:", AppName/binary>> => #{<<"read">> => true, <<"write">> => true}
                        },
                        <<"default">> => false,
                        <<"user">> => #{
                            <<"__type">> => <<"Pointer">>,
                            <<"className">> => <<"_User">>,
                            <<"objectId">> => UserId
                        }
                    },
                    {ok, Result} = dgiot_parse:create_object(<<"App">>, NewBody),
                    {200, Result};
                {error, Err} -> {404, Err}
            end;
        {error, Error} -> {error, Error}
    end;


% 创建开发者用户
do_request_before(<<"post_users">>, _Args, #{
    <<"username">> := UserName,
    <<"password">> := PassWord,
    <<"phone">> := Phone} = Body,
        _Headers, _Context, _Req) ->
    ?LOG(info,"Body ~p", [Body]),
    Query = #{
        <<"order">> => <<"-updatedAt,-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{
            <<"$or">> => [#{<<"email">> => UserName}, #{<<"phone">> => Phone}, #{<<"username">> => UserName}]
        }
    },
    case dgiot_parse:query_object(<<"_User">>, Query) of
        {ok, #{<<"results">> := []}} ->
            NewBody = #{
                <<"username">> => UserName,
                <<"password">> => PassWord,
                <<"phone">> => Phone},
            ?LOG(info,"NewBody ~p ", [NewBody]),
            dgiot_parse:create_object(<<"_User">>, NewBody),
            {file, Here} = code:is_loaded(?MODULE),
            Dir = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/install/role/"]),
            Result = dgiot_install:generate_role(maps:to_list(#{
                <<"dir">> => Dir,
                <<"appname">> => UserName,
                <<"appid">> => UserName,
                <<"desc">> => UserName,
                <<"username">> => UserName})
            ),
            {200, Result};
        {ok, #{<<"results">> := [_Info]}} ->
            {error, <<"User exist  ", UserName/binary>>};
        {error, Error} -> {error, Error}
    end;

%% 退出登录
do_request_before(<<"post_logout">>, Args, Body, Headers, Context, Req) ->
    request_parse(<<"post_logout">>, Args, Body, Headers, Context, Req);

do_request_before(<<"post_batch">>, _Args, Batch, Headers, #{base_path := BasePath}, _Req) ->
    Requests =
        lists:foldr(
            fun(#{<<"path">> := Path} = Request, Acc) ->
                Path1 = re:replace(Path, BasePath, <<>>, [{return, binary}]),
                [Request#{<<"path">> => Path1} | Acc]
            end, [], maps:get(<<"requests">>, Batch)),
    case dgiot_parse:batch(Requests, Headers, [{from, rest}]) of
        {ok, Result} -> {200, Result};
        {error, Reason} -> {400, Reason}
    end;

do_request_before(<<"get_schema_", _/binary>>, _Args, _Body, _Headers, #{base_path := BasePath}, Req) ->
    Class = re:replace(dgiot_req:path(Req), <<BasePath/binary, "/schema/">>, <<>>, [{return, binary}]),
    case dgiot_parse:get_schemas(Class) of
        {ok, Result} -> {200, Result};
        {error, Reason} -> {400, Reason}
    end;

do_request_before(<<"put_level_", _/binary>>, Args, _Body, _Headers, #{base_path := BasePath}, Req) ->
    Class = re:replace(dgiot_req:path(Req), <<BasePath/binary, "/level/">>, <<>>, [{return, binary}]),
    case dgiot_parse:set_class_level(Class, Args) of
        {ok, Result} -> {200, Result};
        {error, Reason} -> {400, Reason}
    end;

do_request_before(<<"get_trigger_", _/binary>>, #{<<"triggerName">> := TriggerName}, _Body, _Headers, #{base_path := BasePath}, Req) ->
    Class = re:replace(dgiot_req:path(Req), <<BasePath/binary, "/trigger/">>, <<>>, [{return, binary}]),
    case dgiot_parse:get_trigger(Class, TriggerName) of
        {ok, Result} -> {200, Result};
        {error, #{<<"code">> := 143} = Err} -> {404, Err};
        {error, Reason} -> {400, Reason}
    end;

do_request_before(<<"post_trigger_", _/binary>>, #{<<"triggerName">> := TriggerName, <<"url">> := Url}, _Body, _Headers, #{base_path := BasePath}, Req) ->
    Class = re:replace(dgiot_req:path(Req), <<BasePath/binary, "/trigger/">>, <<>>, [{return, binary}]),
    Re = <<"http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+">>,
    case re:run(Url, Re) of
        {match, _} ->
            case dgiot_parse:add_trigger(Class, TriggerName, Url) of
                {ok, Result} -> {201, Result};
                {error, Reason} -> {400, Reason}
            end;
        nomatch ->
            {400, #{error => <<"URL is illegal!!!">>}}
    end;

do_request_before(<<"put_trigger_", _/binary>>, #{<<"triggerName">> := TriggerName, <<"url">> := Url}, _Body, _Headers, #{base_path := BasePath}, Req) ->
    Class = re:replace(dgiot_req:path(Req), <<BasePath/binary, "/trigger/">>, <<>>, [{return, binary}]),
    Re = <<"http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+">>,
    case re:run(Url, Re) of
        {match, _} ->
            case dgiot_parse:update_trigger(Class, TriggerName, Url) of
                {ok, Result} -> {200, Result};
                {error, Reason} -> {400, Reason}
            end;
        nomatch ->
            {400, #{error => <<"URL is illegal!!!">>}}
    end;

do_request_before(<<"put_trigger_", _/binary>>, #{<<"triggerName">> := TriggerName, <<"__op">> := <<"Delete">>}, _Body, _Headers, #{base_path := BasePath}, Req) ->
    Class = re:replace(dgiot_req:path(Req), <<BasePath/binary, "/trigger/">>, <<>>, [{return, binary}]),
    case dgiot_parse:del_trigger(Class, TriggerName) of
        ok -> {200, #{}};
        {error, Reason} -> {400, Reason}
    end;
do_request_before(<<"post_requestpasswordreset">>, Args, Body, Headers, Context, Req) ->
    request_parse(<<"post_requestpasswordreset">>, Args, Body, Headers, Context, Req);


do_request_before(OperationID, Args, Body, Headers, Context, Req) ->
    request_parse(OperationID, Args, Body, Headers, Context, Req).


%% 所有请求后置处理
do_request_after(<<"get_login">>, 200, ResHeaders, ResBody, Context, Req) ->
    case do_login(jsx:decode(ResBody, [{labels, binary}, return_maps])) of
        {ok, #{<<"sessionToken">> := Token} = UserInfo} ->
            NewReq =
                case maps:get(from, Context, rest) of
                    js -> set_cookies("sessionToken", Token, Req);
                    _ -> Req
                end,
            {200, ResHeaders, UserInfo, NewReq};
        {error, ErrMsg} ->
            {500, ErrMsg}
    end;

do_request_after(_OperationID, StatusCode, ResHeaders, ResBody, _Context, Req) ->
    {StatusCode, ResHeaders, ResBody, Req}.


%% ==========================
%%  parse 请求
%% ==========================

request_parse(OperationID, Args, Body, Headers, #{base_path := BasePath} = Context, Req) ->
    Path = re:replace(dgiot_req:path(Req), BasePath, <<>>, [{return, binary}]),
    QS =
        lists:foldl(
            fun
                ({<<"sessionToken">>, _}, Acc) ->
                    Acc;
                ({N, V}, <<>>) ->
                    <<"?", N/binary, "=", V/binary>>;
                ({N, V}, Acc) ->
                    <<Acc/binary, "&", N/binary, "=", V/binary>>
            end, <<>>, dgiot_req:parse_qs(Req)),
    Url = <<Path/binary, QS/binary>>,
    Method = dgiot_req:method(Req),
    request_parse(OperationID, Url, Method, Args, Body, Headers, Context, Req).

request_parse(OperationID, Url, Method, _Args, Body, Headers, #{from := From} = Context, Req) ->
    case dgiot_parse:request(Method, maps:to_list(Headers), Url, dgiot_parse:get_objectid(OperationID, Body), [{from, From}]) of
        {ok, StatusCode, ResHeaders, ResBody} ->
            NewHeaders =
                lists:foldl(
                    fun
                        ({"etag", _}, Map) ->
                            Map;
                        ({Key, Val}, Map) ->
                            Map#{list_to_binary(Key) => list_to_binary(Val)}
                    end, #{}, ResHeaders),
            do_request_after(OperationID, StatusCode, NewHeaders, ResBody, Context, Req);
        {error, Reason} ->
            ResBody = #{<<"code">> => 1, <<"error">> => dgiot_utils:to_binary(Reason)},
            {500, ResBody}
    end.


get_token(App) ->
    Now = dgiot_datetime:nowstamp(),
    case dgiot_data:get({token, App}) of
        not_find ->
            get_token_(App);
        {_Token, Expire, TTL} when Now > Expire + TTL ->
            get_token_(App);
        {Token, _Expire, _TTL} -> Token
    end.

get_token_(App) ->
    case dgiot_parse:query_object(<<"_Role">>, #{
        <<"limit">> => 1,
        <<"where">> => #{<<"name">> => App}
    }) of
        {ok, #{<<"results">> := [#{<<"objectId">> := AppId,
            <<"tag">> := #{<<"appconfig">> := #{<<"secret">> := Secret}}}]}} ->
            case login_by_token(AppId, Secret) of
                {ok, #{<<"access_token">> := Token, <<"expires_in">> := TTL}} ->
                    dgiot_data:insert({token, App}, {Token, dgiot_datetime:nowstamp(), dgiot_utils:to_int(TTL)}),
                    Token;
                Error -> Error
            end;
        Err ->
            ?LOG(error,"login by token: AppId:~p, Err ~p~n", [App, Err]),
            {error, #{<<"code">> => 101, <<"error">> => <<"AppId Or Secret not found.">>}}
    end.

%% 创建app角色用户
create_user_for_app(App) ->
    AppUser = <<"user_for_", App/binary>>,
    Query = #{
        <<"order">> => <<"-updatedAt,-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{
            <<"$or">> => [#{<<"email">> => AppUser}, #{<<"phone">> => AppUser}, #{<<"username">> => AppUser}]
        }
    },
    case dgiot_parse:query_object(<<"_User">>, Query) of
        {ok, #{<<"results">> := Results}} when length(Results) > 0 ->
            {ok, AppUser};
        {ok, #{<<"results">> := []}} ->
            Data = #{
                <<"username">> => AppUser,
                <<"nick">> => AppUser,
                <<"password">> => new_token(),
                <<"phone">> => AppUser,
                <<"email">> => <<AppUser/binary, "@iotn2n.com">>
            },
            case dgiot_parse:create_object(<<"_User">>, Data) of
                {ok, #{<<"objectId">> := UserId}} ->
                    dgiot_parse:update_object(<<"_User">>, UserId, #{
                        <<"ACL">> => #{
                            UserId => #{<<"read">> => true, <<"write">> => true}
                        }}),
                    {ok, AppUser};
                Error ->
                    ?LOG(info,"Error ~p", [Error]),
                    Error
            end;
        Reason -> Reason
    end.

%% 创建企业内部用户
%%[{"X-Parse-Session-Token", Session}], [{from, rest}]
create_user(#{<<"username">> := UserName, <<"department">> := RoleId} = Body, SessionToken) ->
    case dgiot_parse:get_object(<<"_Role">>, RoleId,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"objectId">> := RoleId}} ->
            Query = #{
                <<"order">> => <<"-updatedAt,-createdAt">>,
                <<"limit">> => 1,
                <<"where">> => #{
                    <<"$or">> => [#{<<"email">> => UserName}, #{<<"phone">> => UserName}, #{<<"username">> => UserName}]
                }
            },
            case dgiot_parse:query_object(<<"_User">>, Query) of
                {ok, #{<<"results">> := Results}} when length(Results) == 0 ->
                    case dgiot_parse:create_object(<<"_User">>, maps:without([<<"department">>], Body)) of
                        {ok, #{<<"objectId">> := UserId}} ->
                            dgiot_parse:update_object(<<"_User">>, UserId, #{
                                <<"ACL">> => #{UserId => #{
                                    <<"read">> => true,
                                    <<"write">> => true
                                }},
                                <<"emailVerified">> => true}),
                            dgiot_parse:update_object(<<"_Role">>, RoleId, #{
                                <<"users">> => #{
                                    <<"__op">> => <<"AddRelation">>,
                                    <<"objects">> => [#{
                                        <<"__type">> => <<"Pointer">>,
                                        <<"className">> => <<"_User">>,
                                        <<"objectId">> => UserId
                                    }]
                                }});
                        Error ->
                            ?LOG(info,"Error ~p", [Error]),
                            Error
                    end;
                {ok, #{<<"results">> := [_Info]}} ->
                    ?LOG(info,"_Info ~p", [_Info]),
                    {error, <<"User exist  ", UserName/binary>>};
                {error, Error} ->
                    ?LOG(info,"Error ~p", [Error]),
                    {error, Error}
            end;
        _ -> {error, <<"token fail">>}
    end.

%% 删除企业内部用户
%%[{"X-Parse-Session-Token", Session}], [{from, rest}]
delete_user(#{<<"username">> := UserName, <<"department">> := RoleId}, SessionToken) ->
    case dgiot_parse:get_object(<<"_Role">>, RoleId,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"objectId">> := RoleId}} ->
            Query = #{
                <<"where">> => #{
                    <<"$relatedTo">> => #{
                        <<"object">> => #{
                            <<"__type">> => <<"Pointer">>,
                            <<"className">> => <<"_Role">>,
                            <<"objectId">> => RoleId},
                        <<"key">> => <<"users">>
                    }
                }
            },
            case dgiot_parse:query_object(<<"_User">>, Query) of
                {ok, #{<<"results">> := Resulst}} when length(Resulst) > 0 ->
                    R = lists:map(fun(#{<<"username">> := Name, <<"objectId">> := ObjectId}) ->
                        case Name of
                            UserName ->
                                dgiot_parse:del_object(<<"_User">>, ObjectId);
                            _ -> pass
                        end
                                  end, Resulst),
                    {ok, #{<<"result">> => R}};
                _ -> {error, <<"not find user">>}
            end;
        _ -> {error, <<"token fail">>}
    end.


%% 修改企业内部用户
%%[{"X-Parse-Session-Token", Session}], [{from, rest}]
put_user(#{<<"username">> := UserName, <<"department">> := RoleId} = Body, SessionToken) ->
    case dgiot_parse:get_object(<<"_Role">>, RoleId,
        [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"objectId">> := RoleId}} ->
            Query = #{
                <<"where">> => #{
                    <<"$relatedTo">> => #{
                        <<"object">> => #{
                            <<"__type">> => <<"Pointer">>,
                            <<"className">> => <<"_Role">>,
                            <<"objectId">> => RoleId},
                        <<"key">> => <<"users">>
                    }
                }
            },
            case dgiot_parse:query_object(<<"_User">>, Query) of
                {ok, #{<<"results">> := Resulst}} when length(Resulst) > 0 ->
                    R = lists:foldl(fun(#{<<"username">> := Name, <<"objectId">> := ObjectId}, Acc) ->
                        case Name of
                            UserName ->
                                {ok, Result} = dgiot_parse:update_object(<<"_User">>, ObjectId,
                                    maps:without([<<"department">>], Body)),
                                Result;
                            _ -> Acc
                        end
                                    end, #{}, Resulst),
                    ?LOG(info,"R ~p", [R]),
                    {ok, #{<<"result">> => R}};
                _ -> {error, <<"not find user">>}
            end;
        _ -> {error, <<"token fail">>}
    end.

%% 用户名和密码登录
login_by_account(UserName, Password) ->
    case dgiot_parse:login(UserName, Password) of
        {ok, UserInfo} ->
            do_login(UserInfo);
        {error, Msg} ->
            {error, Msg}
    end.


%%[{"X-Parse-Session-Token", Session}], [{from, rest}])
login_by_token(AppId, Secret) ->
    case dgiot_parse:get_object(<<"_Role">>, AppId) of
        {ok, #{<<"name">> := Name, <<"tag">> := #{<<"appconfig">> := Config} }} ->

            case maps:find(<<"secret">>, Config) of
                error ->
                    {error, #{<<"code">> => 101, <<"error">> => <<"AppId Or Secret not found.">>}};
                {ok, Secret} ->
                    AppUser = <<"user_for_", Name/binary>>,
                    Query = #{
                        <<"order">> => <<"-updatedAt,-createdAt">>,
                        <<"limit">> => 1,
                        <<"where">> => #{
                            <<"$or">> => [#{<<"email">> => AppUser}, #{<<"phone">> => AppUser}, #{<<"username">> => AppUser}]
                        }
                    },
                    case dgiot_parse:query_object(<<"_User">>, Query) of
                        {ok, #{<<"results">> := [#{<<"objectId">> := UserId} | _]}} ->
                            TTL = maps:get(<<"expires">>, Config, dgiot_auth:ttl()),
                            create_session(UserId, TTL, Name);
                        _ -> {error, #{<<"code">> => 101, <<"error">> => <<"User not found.">>}}
                    end;
                _ ->
                    {error, #{<<"code">> => 101, <<"error">> => <<"AppId Or Secret not found.">>}}
            end;
        Err ->
            ?LOG(error,"login by token: AppId:~p, Secret:~p ~p~n", [AppId, Secret, Err]),
            {error, #{<<"code">> => 101, <<"error">> => <<"AppId Or Secret not found.">>}}
    end.

%% 邮箱/手机验证后的登录
login_by_mail_phone(Account) ->
    Query = #{
        <<"order">> => <<"-updatedAt,-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{
            <<"$or">> => [#{<<"email">> => Account}, #{<<"phone">> => Account}]
        }
    },
    case dgiot_parse:query_object(<<"_User">>, Query) of
        {ok, #{<<"results">> := [Info]}} ->
            ?LOG(info,"Info ~p", [Info]),
            #{<<"objectId">> := UserId, <<"username">> := Desc} = Info,
            Data = #{<<"emailVerified">> => true, <<"phone">> => Account},
            case dgiot_parse:update_object(<<"_User">>, UserId, Data) of
                {ok, #{<<"updatedAt">> := _}} ->
                    create_session(UserId, dgiot_auth:ttl(), Desc);
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, #{<<"results">> := []}} ->
            {error, #{<<"code">> => 101, <<"error">> => <<"User not found by ", Account/binary>>}};
        {error, Reason} ->
            {error, Reason}
    end.


set_cookies(Key, Value, Req) ->
    dgiot_req:set_cookie(Key, Value, Req, #{max_age => dgiot_auth:ttl()}).

check_roles(Name) ->
    Query = #{
        <<"order">> => <<"-updatedAt,-createdAt">>,
        <<"limit">> => 1,
        <<"where">> => #{<<"username">> => <<"user_for_", Name/binary>>}
    },
    case dgiot_parse:query_object(<<"_User">>, Query) of
        {ok, #{<<"results">> := [#{<<"objectId">> := UserId} | _]}} ->
            {ok, #{<<"access_token">> := Access_Token}} =
                create_session(UserId, dgiot_auth:ttl(), Name),
            {200, #{
                <<"desc">> => Name,
                <<"expires_in">> => dgiot_auth:ttl(),
                <<"access_token">> => Access_Token}};
        _ -> {404, #{<<"error">> => <<"User not found.">>}}
    end.

%% 随机产生token
new_token() ->
    GUID = dgiot_utils:guid(),
    dgiot_utils:to_md5(GUID).

create_session(UserId, TTL, Name) ->
    Token = new_token(),
    SessionToken = <<"r:", Token/binary>>,
    case dgiot_parse:create_session(UserId, SessionToken, TTL) of
        {ok, UserInfo} ->
            case do_login(UserInfo, TTL) of
                {ok, NewUserInfo} ->
                    {ok, NewUserInfo#{
                        <<"sessionToken">> => SessionToken,
                        <<"access_token">> => SessionToken,
                        <<"expires_in">> => TTL,
                        <<"desc">> => Name,
                        <<"name">> => Name
                    }};
                {error, ErrMsg} ->
                    {error, ErrMsg}
            end;
        {error, #{<<"code">> := Code, <<"error">> := Err}} ->
            {error, #{<<"code">> => Code, <<"error">> => Err}};
        {error, Reason} ->
            ?LOG(error,"refresh_token ~p, ~p~n", [UserId, Reason]),
            {error, #{<<"code">> => 1, <<"error">> => <<"Internal server error.">>}}
    end.

do_login(UserInfo) ->
    do_login(UserInfo, dgiot_auth:ttl()).

do_login(#{<<"objectId">> := UserId, <<"sessionToken">> := SessionToken} = UserInfo, TTL) ->
    case catch dgiot_parse:get_role(UserId, SessionToken) of
        {ok, #{<<"roles">> := Roles, <<"rules">> := Rules}} ->
            ?LOG(info,"Roles ~p",[Roles]),
            NewRules =
                lists:foldl(
                    fun(Rule, Acc) ->
                        [Rule | lists:delete(Rule, Acc)]
                    end, Rules, [<<"GET_CLASSES_NAVIGATION">>, <<"GET_USERS_ME">>]),
            dgiot_auth:put_session(UserInfo#{<<"roles">> => Roles, <<"rules">> => NewRules}, TTL),
            {ok, maps:without([<<"_account_lockout_expires_at">>, <<"_failed_login_count">>, <<"_email_verify_token">>], UserInfo#{<<"roles">> => maps:values(Roles)})};
        {error, ErrMsg} ->
            {error, ErrMsg};
        {'EXIT', Reason} ->
            {error, #{<<"code">> => 1, <<"error">> => dgiot_utils:format("~p", [Reason])}};
        _Other ->
            ?LOG(info,"_Other ~p",[_Other])
    end.

add_acl(_, _, Records, _NewItems, Acc) when Records == [] ->
    Acc;
add_acl(Class, RoleName, [#{<<"objectId">> := Id, <<"ACL">> := ACLMap} | Records], NewItems, Acc) ->
    Path = <<"/classes/", Class/binary, "/", Id/binary>>,
    case maps:get(RoleName, ACLMap, no) of
        no ->
            case get_value(Id, NewItems) of
                false ->
                    add_acl(Class, RoleName, Records, NewItems, Acc);
                Id ->
                    NewAcl = #{<<"ACL">> => ACLMap#{RoleName => #{<<"read">> => true, <<"write">> => true}}},
                    NewAcc = [#{<<"body">> => NewAcl, <<"method">> => <<"PUT">>, <<"path">> => Path} | Acc],
                    add_acl(Class, RoleName, Records, proplists:delete(Id, NewItems), NewAcc);
                ACL ->
                    NewAcl = #{<<"ACL">> => ACLMap#{RoleName => ACL}},
                    NewAcc = [#{<<"body">> => NewAcl, <<"method">> => <<"PUT">>, <<"path">> => Path} | Acc],
                    add_acl(Class, RoleName, Records, proplists:delete(Id, NewItems), NewAcc)
            end;
        OldACL ->
            case get_value(Id, NewItems) of
                false ->
                    NewAcl = #{<<"ACL">> => maps:without([RoleName], ACLMap)},
                    NewAcc = [#{<<"body">> => NewAcl, <<"method">> => <<"PUT">>, <<"path">> => Path} | Acc],
                    add_acl(Class, RoleName, Records, NewItems, NewAcc);
                O when O == Id; O == OldACL ->
                    add_acl(Class, RoleName, Records, proplists:delete(Id, NewItems), Acc);
                ACL ->
                    NewAcl = #{<<"ACL">> => ACLMap#{RoleName => ACL}},
                    NewAcc = [#{<<"body">> => NewAcl, <<"method">> => <<"PUT">>, <<"path">> => Path} | Acc],
                    add_acl(Class, RoleName, Records, proplists:delete(Id, NewItems), NewAcc)
            end
    end.

get_navigation(#{<<"user">> := #{<<"roles">> := Roles}}, Args) ->
    RoleList = maps:to_list(Roles),
    Requests = [#{
        <<"method">> => <<"GET">>,
        <<"path">> => <<"/classes/Menu">>,
        <<"body">> => Args#{
            <<"where">> => #{
                <<"$relatedTo">> =>#{
                    <<"key">> => <<"menus">>,
                    <<"object">> => #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"_Role">>, <<"objectId">> => RoleId}
                }
            }
        }
    } || {RoleId, _} <- RoleList],
    case dgiot_parse:batch(Requests) of
        {ok, Results} ->
            get_navigation_by_result(Results, RoleList);
        {error, Reason} ->
            {error, Reason}
    end.

get_navigation_by_result(Results, Roles) ->
    get_navigation_by_result(Results, Roles, #{}).
get_navigation_by_result([], [], Acc) ->
    {ok, Acc};
get_navigation_by_result([#{<<"error">> := Err} | _], _, _) ->
    {error, Err};
get_navigation_by_result([#{<<"success">> := #{<<"results">> := Result}} | Menus], [Role | Roles], Acc0) ->
    {RoleId, #{<<"name">> := RoleName, <<"alias">> := Alias}} = Role,
    Fun =
        fun(#{<<"objectId">> := MenuId} = Menu, NewAcc) ->
            Show = #{<<"roleId">> => RoleId, <<"roleName">> => RoleName, <<"alias">> => Alias},
            case maps:get(MenuId, NewAcc, no) of
                no ->
                    NewAcc#{MenuId => Menu#{<<"navShow">> => [Show]}};
                #{<<"navShow">> := RoleIds} ->
                    NewAcc#{MenuId => Menu#{<<"navShow">> => [Show | RoleIds]}}
            end
        end,
    get_navigation_by_result(Menus, Roles, lists:foldl(Fun, Acc0, Result)).

get_classtree(ClassName, Parent, Filter, SessionToken) ->
    case dgiot_parse:query_object(ClassName, Filter, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Classes}} when length(Classes) > 0 ->
            NewClasses =
                lists:foldl(fun(Class, Acc) ->
                    NewClasse = Class#{
                        <<"label">> => maps:get(<<"name">>, Class, <<"label">>),
                        Parent => maps:get(Parent, Class, <<"0">>)},
                    Acc ++ [maps:without([<<"createdAt">>, <<"updatedAt">>, <<"ACL">>], NewClasse)]
                            end, [], Classes),
            ClassTree = dgiot_parse_handler:create_tree(NewClasses, Parent),
            Len = length(ClassTree),
            Num = 1000,
            case Len =< Num of
                true ->
                    {200, #{<<"results">> => ClassTree}};
                false ->
                    ShowTrees = lists:sublist(ClassTree, 1, Num),
                    MoreTrees = lists:sublist(ClassTree, Num + 1, length(ClassTree) - Num),
                    {200, #{<<"results">> => ShowTrees ++ [#{
                        <<"children">> => MoreTrees,
                        <<"name">> => <<"...">>,
                        <<"label">> => <<"...">>,
                        <<"icon">> => <<"More">>,
                        Parent => <<"0">>,
                        <<"url">> => <<"/more">>
                    }]}}
            end;
        _ -> {error, <<"1 not find">>}
    end.


create_tree(Items, Parent) ->
%%    ?LOG(info,"Items ~p", [Items]),
    NewItems = lists:foldl(
        fun(#{<<"objectId">> := Id} = Item, Acc) ->
            ParentId =
                case maps:get(Parent, Item, no) of
                    #{<<"objectId">> := PId} ->
                        binary:replace(PId, <<" ">>, <<>>, [global]);
                    _ ->
                        <<"0">>
                end,
            Node = Item#{Parent => ParentId},
            [{Id, ParentId, Node} | Acc]
        end, [], Items),
    Tree = lists:foldl(
        fun({Id, PId, Node}, Acc) ->
            case lists:keyfind(PId, 1, NewItems) of
                false ->
                    case get_children(Id, NewItems) of
                        [] ->
                            [Node | Acc];
                        Children ->
                            [Node#{<<"children">> => Children} | Acc]
                    end;
                _ ->
                    Acc
            end
        end, [], NewItems),
    lists:sort(fun children_sort/2, Tree).

get_children(CId, Items) ->
    Tree = lists:filtermap(
        fun({Id, PId, Node}) ->
            case PId == CId of
                true ->
                    case get_children(Id, Items) of
                        [] ->
                            {true, Node};
                        Children ->
                            {true, Node#{<<"children">> => Children}}
                    end;
                false ->
                    false
            end
        end, Items),
    lists:sort(fun children_sort/2, Tree).


children_sort(Node1, Node2) ->
    Order1 = maps:get(<<"orderBy">>, Node1, 0),
    Order2 = maps:get(<<"orderBy">>, Node2, 0),
    Order1 < Order2.



get_value(_, []) -> false;
get_value(Key, [{Key, Value}]) -> Value;
get_value(Key, [Key | _]) -> Key;
get_value(Key, [_ | L]) -> get_value(Key, L).

%% ==========================
%%  swagger
%% ==========================

default_schemas() ->
    NewSWSchemas =
        case dgiot_swagger:load_schema(?MODULE, "swagger_parse.json", [{labels, binary}, return_maps]) of
            {ok, SWSchemas} ->
                SWSchemas;
            _ ->
                #{}
        end,
    OldComponents = maps:get(<<"definitions">>, NewSWSchemas, #{}),
    NewSWSchemas#{
        <<"definitions">> => OldComponents
    }.

transform_classe(#{<<"className">> := ClassName, <<"fields">> := Fields}, ComSchemas) ->
    Fun =
        fun(Key, Map, Acc) ->
            Acc#{Key => to_swagger_type(Map#{<<"field">> => Key})}
        end,
    Properties = maps:fold(Fun, #{}, Fields),
    DelCols = [],
    Props = maps:without(DelCols, Properties),
    maps:merge(#{
        ClassName => #{
            <<"type">> => <<"object">>,
            <<"properties">> => Props
        }
    }, ComSchemas).


to_swagger_type(#{<<"type">> := Type}) when Type == <<"ACL">> ->
    #{
        <<"type">> => <<"object">>,
        <<"example">> => #{<<"*">> => #{<<"read">> => true, <<"write">> => false}},
        <<"properties">> => #{
        }
    };
to_swagger_type(#{<<"type">> := Type, <<"targetClass">> := Class}) when Type == <<"Pointer">> ->
    #{
        <<"type">> => <<"object">>,
        <<"description">> => <<"">>,
        <<"properties">> => #{
            <<"objectId">> => #{
                <<"type">> => <<"string">>
            },
            <<"__type">> => #{
                <<"type">> => <<"string">>,
                <<"example">> => Type
            },
            <<"className">> => #{
                <<"type">> => <<"string">>,
                <<"example">> => Class
            }
        }
    };
to_swagger_type(#{<<"type">> := Type, <<"targetClass">> := Class}) when Type == <<"Relation">> ->
    #{
        <<"type">> => <<"object">>,
        <<"description">> => <<"">>,
        <<"properties">> => #{
            <<"__type">> => #{
                <<"type">> => <<"string">>,
                <<"example">> => Type
            },
            <<"className">> => #{
                <<"type">> => <<"string">>,
                <<"example">> => Class
            }
        }
    };
to_swagger_type(#{<<"type">> := Type}) when Type == <<"File">>; Type == <<"Object">> ->
    #{<<"type">> => <<"object">>};
to_swagger_type(#{<<"type">> := Type}) when Type == <<"Number">>;Type == <<"Boolean">>;Type == <<"String">> ->
    #{<<"type">> => list_to_binary(string:to_lower(binary_to_list(Type)))};
to_swagger_type(#{<<"type">> := <<"Array">>}) ->
    #{
        <<"type">> => <<"array">>,
        <<"items">> => #{
        }
    };
to_swagger_type(_) ->
    #{<<"type">> => <<"string">>}.


get_paths(Schema, Acc) ->
    add_paths(Schema, Acc).

add_paths(#{<<"className">> := ClassName} = Schema, Acc) ->
    Definitions = maps:get(<<"definitions">>, Acc, #{}),
    Paths = maps:get(<<"paths">>, Acc, #{}),
    Tags = maps:get(<<"tags">>, Acc, []),
    CSchema = get_path(Tags, ClassName),
    CDefinitions = maps:get(<<"definitions">>, CSchema, #{}),
    CPaths = maps:get(<<"paths">>, CSchema, #{}),
    CTags = maps:get(<<"tags">>, CSchema, []),
    Acc#{
        <<"definitions">> => transform_classe(maps:merge(CDefinitions, Schema), Definitions),
        <<"paths">> => maps:merge(Paths, CPaths),
        <<"tags">> => lists:foldl(
            fun(#{<<"name">> := TagName} = Tag0, Acc1) ->
                NewTags = lists:filtermap(
                    fun(#{<<"name">> := OldTagName}) ->
                        OldTagName =/= TagName
                    end, Acc1),
                [Tag0 | NewTags]
            end, Tags, CTags)
    }.


get_path(Tags, ClassName) ->
    {ok, Bin} = dgiot_swagger:load_schema(?MODULE, "swagger_parse_object.json", []),
    Data = re:replace(Bin, "\\{\\{className\\}\\}", ClassName, ?RE_OPTIONS),
    CTags = lists:filtermap(fun(#{<<"name">> := Name}) -> Name == ClassName end, Tags),
    Desc =
        case CTags of
            [] -> ClassName;
            [#{<<"description">> := Description} | _] -> Description
        end,
    Data1 = re:replace(Data, "\\{\\{tag\\}\\}", Desc, ?RE_OPTIONS),
    jsx:decode(Data1, [{labels, binary}, return_maps]).
