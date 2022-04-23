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
-include("dgiot_parse.hrl").
-dgiot_rest(all).

-define(RE_OPTIONS, [global, {return, binary}]).

%% API
-export([get_OperationID/1, swagger_parse/0, init/2, handle/4]).

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
    dgiot_parse_utils:check_parse(Tables, 0).

init(Req0, Map) ->
    {ok, Body, Req} = dgiot_req:read_body(Req0),
    case catch jsx:decode(Body, [{labels, binary}, return_maps]) of
        #{<<"_JavaScriptKey">> := _JSKey} = RecvMap ->
            Method = maps:get(<<"_method">>, RecvMap, dgiot_req:method(Req)),
            Index = maps:get(Method, Map),
            {ok, {_, Config}} = dgiot_router:get_state(Index),
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
            dgiot_parse_auth:check_roles(Name);
        {ok, #{<<"results">> := Roles}} when length(Roles) == 0 ->
            {404, #{<<"code">> => 101, <<"error">> => <<"User not found.">>}};
        {error, Error} ->
            {404, #{<<"code">> => 101, <<"error">> => Error}}
    end;

%% IoTDevice 概要: 获取应用token 描述:Token查询
%% OperationId: post_token
%% POST /token
handle(post_token, #{<<"appid">> := AppId, <<"secret">> := Secret}, _Context, _Req) ->
    case dgiot_parse_auth:login_by_token(AppId, Secret) of
        {ok, UserInfo} ->
            {200, maps:with([<<"access_token">>, <<"expires_in">>, <<"desc">>, <<"name">>], UserInfo)};
        {error, #{<<"code">> := 101} = Err} ->
            {404, Err};
        {error, Err} -> {500, Err}
    end;

%% IoTDevice 概要: 刷新Token 描述:刷新Token
%% OperationId: get_refresh_session
%% POST /token
handle(get_refresh_session, #{<<"sessionToken">> := SessionToken}, _Context, _Req) ->
    case dgiot_parse_auth:refresh_session(SessionToken) of
        {ok, Ref} ->
            {200, Ref};
        {error, Err} ->
            {500, Err}
    end;

%% IoTDevice 概要: 获取应用token 描述:更新token
%% OperationId: put_token
%% PUT /token
handle(put_token, #{<<"appid">> := AppId, <<"secret">> := Secret}, _Context, _Req) ->
    case dgiot_parse_auth:login_by_token(AppId, Secret) of
        {ok, UserInfo} ->
            {200, maps:with([<<"access_token">>, <<"expires_in">>, <<"desc">>, <<"name">>], UserInfo)};
        {error, #{<<"code">> := 101} = Err} ->
            {404, Err};
        {error, Err} -> {500, Err}
    end;

handle(post_user, #{<<"username">> := _UserName, <<"password">> := _Password} = Body, #{<<"sessionToken">> := SessionToken}, _Req) ->
    case dgiot_parse_auth:create_user(Body, SessionToken) of
        {ok, Data} ->
            dgiot_parse_auth:load_role(),
            {200, Data};
        {error, Error} -> {500, Error}
    end;

handle(delete_user, #{<<"username">> := _UserName} = Body, #{<<"sessionToken">> := SessionToken}, _Req) ->
    case _UserName of
        <<"dgiot_admin">> ->
            {ok, #{<<"code">> => 401, <<"msg">> => <<"dgiot_admin PROHIBITED DELETE">>}};
        _ ->
            case dgiot_parse_auth:delete_user(Body, SessionToken) of
                {ok, Data} ->
                    dgiot_parse_auth:load_role(),
                    {200, Data};
                {error, Error} -> {error, Error}
            end
    end;

handle(put_user, #{<<"username">> := _UserName} = Body, #{<<"sessionToken">> := SessionToken}, _Req) ->
    case dgiot_parse_auth:put_user(Body, SessionToken) of
        {ok, Data} ->
            dgiot_parse_auth:load_role(),
            {200, Data};
        {error, Error} -> {500, Error}
    end;

%% IoTDevice 概要: 禁用账号
%% OperationId: get_disableuser
%% Disuserid 被禁用账号
%% GET /token
%% dgiot_parse:load().
handle(get_disableuser, #{<<"userid">> := Disuserid, <<"action">> := Action} = _Body, #{<<"sessionToken">> := SessionToken} = _Context, _Req) ->
    ?LOG(info, "_Body ~p", [_Body]),
    ?LOG(info, "SessionToken ~p", [SessionToken]),
    case dgiot_auth:get_session(SessionToken) of
        #{<<"objectId">> := UserId} ->
            case dgiot_parse_auth:disableusere(UserId, Disuserid, Action) of
                {ok, Data} ->
                    {200, Data};
                {error, Error} -> {500, #{<<"code">> => 101, <<"error">> => Error}}
            end;
        _ ->
            {500, <<"Not Allowed.">>}
    end;

%% parse js的调用处理
handle(OperationID, Args, #{from := js} = Context, Req) ->
    io:format("~s ~p  do_request_before OperationID = ~p ~n", [?FILE, ?LINE, OperationID]),
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
    case dgiot_parse_utils:get_navigation(Context, NewArgs) of
        {ok, Items} ->
            Num = 1000,
            Menus = dgiot_parse_utils:create_tree(maps:values(Items), <<"parent">>),
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
    ?LOG(info, "Body ~p", [Body]),
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
            ?LOG(info, "NewBody ~p ", [NewBody]),
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
    case dgiot_parse_hook:get_trigger(Class, TriggerName) of
        {ok, Result} -> {200, Result};
        {error, #{<<"code">> := 143} = Err} -> {404, Err};
        {error, Reason} -> {400, Reason}
    end;

do_request_before(<<"post_trigger_", _/binary>>, #{<<"triggerName">> := TriggerName, <<"url">> := Url}, _Body, _Headers, #{base_path := BasePath}, Req) ->
    Class = re:replace(dgiot_req:path(Req), <<BasePath/binary, "/trigger/">>, <<>>, [{return, binary}]),
    Re = <<"http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+">>,
    case re:run(Url, Re) of
        {match, _} ->
            case dgiot_parse_hook:add_trigger(Class, TriggerName, Url) of
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
            case dgiot_parse_hook:update_trigger(Class, TriggerName, Url) of
                {ok, Result} -> {200, Result};
                {error, Reason} -> {400, Reason}
            end;
        nomatch ->
            {400, #{error => <<"URL is illegal!!!">>}}
    end;

do_request_before(<<"put_trigger_", _/binary>>, #{<<"triggerName">> := TriggerName, <<"__op">> := <<"Delete">>}, _Body, _Headers, #{base_path := BasePath}, Req) ->
    Class = re:replace(dgiot_req:path(Req), <<BasePath/binary, "/trigger/">>, <<>>, [{return, binary}]),
    case dgiot_parse_hook:del_trigger(Class, TriggerName) of
        ok -> {200, #{}};
        {error, Reason} -> {400, Reason}
    end;

do_request_before(<<"post_requestpasswordreset">>, Args, Body, Headers, Context, Req) ->
    request_parse(<<"post_requestpasswordreset">>, Args, Body, Headers, Context, Req);

do_request_before(OperationID, Args, Body, Headers, Context, Req) ->
%%    io:format("~s ~p  do_request_before OperationID = ~p ~n", [?FILE, ?LINE, OperationID]),
    request_parse(OperationID, Args, Body, Headers, Context, Req).

%% 所有请求后置处理
do_request_after(<<"get_login">>, 200, ResHeaders, ResBody, Context, Req) ->
    case dgiot_parse_auth:do_login(jsx:decode(ResBody, [{labels, binary}, return_maps])) of
        {ok, #{<<"sessionToken">> := Token} = UserInfo} ->
            NewReq =
                case maps:get(from, Context, rest) of
                    js -> dgiot_parse_auth:set_cookies("sessionToken", Token, Req);
                    _ -> Req
                end,
            {200, ResHeaders, UserInfo, NewReq};
        {error, ErrMsg} ->
            {500, ErrMsg}
    end;

%% delete_classes_product_id
%% delete_classes_device_id
do_request_after(<<"delete_classes_", _OperationID/binary>>, 200, ResHeaders, ResBody, _Context, #{bindings := #{id := ObjectId}} = Req) ->
    dgiot_parse_hook:do_hook({_OperationID, delete}, ['after', <<"{\"objectId\" : \"", ObjectId/binary, "\"}">>, <<"">>]),
    {200, ResHeaders, ResBody, Req};

do_request_after(OperationID, StatusCode, ResHeaders, ResBody, _Context, Req) ->
    [Method, Type | _] = re:split(OperationID,<<"_">>),
    io:format("~s ~p  do_request_before Method = ~p Type ~p ~n", [?FILE, ?LINE, Method, Type]),
    Body =
        case dgiot_hook:run_hook({ Method, Type}, ResBody) of
            {ok, NewBody} ->
                NewBody;
            _ ->
                ResBody
        end,
    {StatusCode, ResHeaders, Body, Req}.


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
    {_Type, NewOperationID} = get_OperationID(OperationID),
    NewUrl = get_url(Url),
    case dgiot_parse:request(Method, maps:to_list(Headers), NewUrl, dgiot_parse_id:get_objectid(NewOperationID, Body), [{from, From}]) of
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


get_OperationID(OperationID) ->
    lists:foldl(fun(NewType, {Type, Acc}) ->
        ApiType = <<"_", NewType/binary, "_">>,
        case re:run(OperationID, ApiType) of
            {match, _} ->
                {NewType, re:replace(OperationID, ApiType, <<"_classes_">>, [global, {return, binary}, unicode])};
            _ ->
                {Type, Acc}
        end
                end, {<<"classes">>, OperationID}, dgiot_data:get(swaggerApi)).

get_url(Url) ->
    re:replace(Url, <<"/amis/">>, <<"/classes/">>, [global, {return, binary}, unicode]).