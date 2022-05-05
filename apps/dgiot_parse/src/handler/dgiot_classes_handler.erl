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

-module(dgiot_classes_handler).
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


do_request_before(OperationID, Args, ReqBody, Headers, Context, Req) ->
    request_parse(OperationID, Args, ReqBody, Headers, Context, Req).


%% delete_classes_product_id
%% delete_classes_device_id
do_request_after(<<"delete_classes_", _OperationID/binary>>, 200, ResHeaders, ResBody, _Context, #{bindings := #{id := ObjectId}} = Req) ->
    dgiot_parse_hook:do_hook({_OperationID, delete}, ['after', <<"{\"objectId\" : \"", ObjectId/binary, "\"}">>, <<"">>]),
    {200, ResHeaders, ResBody, Req};

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

do_request_after(OperationID, StatusCode, ResHeaders, ResBody, _Context, Req) ->
    Map = jsx:decode(ResBody, [{labels, binary}, return_maps]),
    Body = dgiot_parse_hook:api_hook({'after', OperationID, Map, ResBody}),
    {StatusCode, ResHeaders, Body, Req}.

%% ==========================
%%  parse 请求
%% ==========================
request_parse(OperationID, Args, Body, Headers, #{base_path := BasePath, <<"sessionToken">>:= Token} = Context, Req) ->
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
    Method = dgiot_req:method(Req),
    {NewQs, NewType, NewArgs} =  dgiot_parse_hook:api_hook({'before', OperationID, Token, QS, dgiot_req:path(Req), Args}),
    Path = get_path(re:replace(dgiot_req:path(Req), BasePath, <<>>, [{return, binary}]), NewType),
    Url = <<Path/binary, NewQs/binary>>,
    request_parse(OperationID, Url, Method, NewArgs, Body, Headers, Context, Req).

request_parse(OperationID, Url, Method, _Args, Body, Headers, #{from := From} = Context, Req) ->
    {_Type, NewOperationID} = get_OperationID(OperationID),
    case dgiot_parse:request(Method, maps:to_list(Headers), Url, dgiot_parse_id:get_objectid(NewOperationID, Body), [{from, From}]) of
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

get_OperationID(OperationID1) ->
    OperationID = dgiot_utils:to_binary(OperationID1),
    lists:foldl(fun({NewType, _Mod}, {Type, Acc}) ->
        ApiType = <<"_", NewType/binary, "_">>,
        case re:run(OperationID, ApiType) of
            {match, _} ->
                {NewType, re:replace(OperationID, ApiType, <<"_classes_">>, [global, {return, binary}, unicode])};
            _ ->
                {Type, Acc}
        end
                end, {<<"classes">>, OperationID}, dgiot_data:get(swaggerApi)).

get_path(Url, Type) ->
    re:replace(Url, <<"/",Type/binary,"/">>, <<"/classes/">>, [global, {return, binary}, unicode]).
