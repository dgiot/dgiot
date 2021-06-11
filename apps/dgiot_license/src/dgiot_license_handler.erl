%%%-------------------------------------------------------------------
%%% @author dgiot
%%% @copyright (C) 2019, dgiot
%%% @doc
%%% API 处理模块 产生时间: Tue, 21 May 2019 16:19:01 +0800
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_license_handler).
-author("dgiot").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_license/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/system">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_system.json">>, ?MODULE, [], priv)
swagger_license() ->
    [
        dgiot_http_server:bind(<<"/swagger_license.json">>, ?MODULE, [], priv)
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
            ?LOG(info,"do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> dgiot_framework:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
%%            lager:debug("do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
%%            ?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
%%            ?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
%%            ?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
%%            ?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================
%licsetup
do_request(get_lictool, #{<<"appid">> := AppId, <<"appsecret">> := Secret}, _Context, Req) ->
    Host = dgiot_req:host(Req),
    FileName = "dgiot_demon.zip",
    Vars = [{host, Host}, {appid, AppId}, {appsecret, Secret}],
    case license_loader:generate_lictool(FileName, Vars) of
        {ok, ZipFile} ->
            Headers = #{
                <<"content-type">> => <<"application/zip">>,
                <<"Content-Disposition">> => list_to_binary("attachment;filename=" ++ FileName)
            },
            {200, Headers, ZipFile};
        Err ->
            Err
    end;

do_request(get_licsetup, #{<<"license">> := License}, _Context, _Req) ->
    FileName = "dgiot_setup.zip",
    case license_loader:generate_setup(FileName, License) of
        {ok, ZipFile} ->
            Headers = #{
                <<"content-type">> => <<"application/zip">>,
                <<"Content-Disposition">> => list_to_binary("attachment;filename=" ++ FileName)
            },
            {200, Headers, ZipFile};
        Err ->
            Err
    end;

do_request(get_licupdate, #{<<"license">> := License}, _Context, _Req) ->
    FileName = "dgiot_update.zip",
    case license_loader:generate_update(FileName, License) of
        {ok, ZipFile} ->
            Headers = #{
                <<"content-type">> => <<"application/zip">>,
                <<"Content-Disposition">> => list_to_binary("attachment;filename=" ++ FileName)
            },
            {200, Headers, ZipFile};
        Err ->
            Err
    end;

do_request(get_setup_result, #{<<"license">> := License, <<"result">> := Result}, _Context, Req) ->
    case dgiot_parse:query_object(<<"License">>, #{<<"where">> => #{<<"license">> => License}}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := ObjectId} | _]}} ->
            dgiot_parse:update_object(<<"License">>, ObjectId, #{<<"status">> => Result}),
            {200, #{}, #{}, Req};
        Err ->
            Err
    end;

do_request(get_health, _Body, _Context, Req) ->
    {200, #{<<"content-type">> => <<"text/plain">>}, <<"ok">>, Req};

do_request(get_hardinfo, _Body, _Context, Req) ->
    HostName = dgiot_evidence:get_hostname(),
    NatIP = dgiot_evidence:get_natip(),
    WlanIp = dgiot_evidence:get_wlanip(),
    ComputerConfig = dgiot_evidence:get_computerconfig(),
    ComputerKey = dgiot_license:get_hardkey(),
    ComputerAuth = case dgiot_license:check() of
                       true -> <<"已授权"/utf8>>;
                       false -> <<"未授权"/utf8>>
                   end,
    DbInstalled = case dgiot_license:check_parse() of
                      true ->
                          case dgiot_parse:get_schemas(<<"License">>) of
                              {ok, _} -> true;
                              _ -> false
                          end;
                      false -> false
                  end,

    NodeInfo = #{
        <<"hostName">> => <<HostName/binary, ""/utf8>>,
        <<"natIP">> => <<NatIP/binary, ""/utf8>>,
        <<"wlanIp">> => <<WlanIp/binary, ""/utf8>>,
        <<"computerConfig">> => <<ComputerConfig/binary, ""/utf8>>,
        <<"computerKey">> => <<ComputerKey/binary, ""/utf8>>,
        <<"computerAuth">> => <<ComputerAuth/binary, ""/utf8>>,
        <<"serverHealth">> => dgiot_license:check_parse(),
        <<"dbInstalled">> => DbInstalled
    },
    {200, #{}, NodeInfo, Req};

%% OperationId:get_apihub
%% 请求:GET /iotapi/apihub
do_request(get_license, _Body, _Context, Req) ->
    Result = case dgiot_license:check() of
                 false -> false;
                 _ ->
                     dgiot_license:check_parse()
             end,
    {200, #{}, #{<<"result">> => Result}, Req};

do_request(get_iothub, #{<<"license">> := License, <<"addr">> := Addr} = Args, _Context, Req) ->
    ?LOG(info,"License ~p ,Addr ~p", [License, Addr]),
    Key = dgiot_license:get_hardkey(),
    Type = maps:get(<<"type">>, Args, <<"standard">>),
    {Result, Status} =
        case dgiot_license:check_lincense(Type, Key, License) of
            false -> {false, <<"license_failed">>};
            _ ->
                dgiot_license:update_license(Args),
                application:stop(dgiot_parse),
                application:start(dgiot_parse),
                emqx_plugins:load(),
                dgiot_http_server:reload_paths(),
                case dgiot_license:check_parse() of
                    true ->
                        case dgiot_parse:get_schemas(<<"License">>) of
                            {ok, _} -> {true, <<"database_installed">>};
                            _ -> {false, <<"database_uninstalled">>}
                        end;
                    _ -> {false, <<"server_disconnected">>}
                end
        end,
    {200, #{}, #{<<"result">> => Result, <<"status">> => Status}, Req};

do_request(get_iotapp, #{<<"license">> := License} = Args, _Context, Req) ->
    Key = dgiot_license:get_hardkey(),
    Type =
        case maps:get(<<"type">>, Args, <<"standard">>) of
            undefined -> <<"standard">>;
            V -> V
        end,
    Result = case dgiot_license:check_lincense(Type, Key, License) of
                 false -> false;
                 true ->
                     case dgiot_license:check_parse() of
                         true ->
                             case dgiot_parse:get_schemas(<<"License">>) of
                                 {ok, _} -> true;
                                 _ ->
                                     dgiot_mqtt:publish(Key, Key, <<"iot">>),
                                     true
                             end;
                         false -> false
                     end
             end,
    {200, #{}, #{<<"result">> => Result}, Req};

do_request(post_iotapp, #{<<"appname">> := App}, #{<<"sessionToken">> := Token, <<"user">> := #{<<"roles">> := Roles}}, Req) ->
    ?LOG(info,"Token ~p", [Token]),
    Result =
        case lists:any(fun(E) ->
            case E of
                #{<<"name">> := App} -> true;
                _ -> false
            end
                       end, maps:values(Roles)) of
            true ->
                case dgiot_license:check() of
                    true ->
                        Key = dgiot_license:get_hardkey(),
                        dgiot_mqtt:publish(Key, Key, App),
                        true;
                    fasle ->
                        false
                end;
            false -> false
        end,
    {200, #{}, #{<<"result">> => Result}, Req};

%% iot_hub 概要: 获取API Hub 描述:获取APi网关
%% OperationId:get_apihub
%% 请求:GET /iotapi/apihub
do_request(get_apihub, #{<<"appname">> := AppName, <<"token">> := undefined} = _Args, _Context, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    Token = proplists:get_value(<<"sessionToken">>, Cookies),
    UserName = proplists:get_value(<<"username">>, Cookies),
    case dgiot_auth:get_session(Token) of
        #{<<"username">> := UserName} ->
            get_app(AppName, Token);
        _ ->
            {error, <<"token fail">>}
    end;

%% iot_hub 概要: 查询平台api资源 描述:查询平台api资源
%% OperationId:get_apihub
%% 请求:POST /iotapi/apihub
do_request(get_apihub, #{<<"appname">> := AppName, <<"token">> := Token} = _Args, _Context, _Req) ->
    case dgiot_auth:get_session(Token) of
        #{<<"roles">> := Roles} ->
            case lists:any(fun(E) ->
                case E of
                    #{<<"name">> := AppName} -> true;
                    _ -> false
                end
                           end, maps:values(Roles)) of
                true -> get_app(AppName, Token);
                false -> {error, <<"token fail">>}
            end;
        _ ->
            {error, <<"token fail">>}
    end;

%% iot_hub 概要: 查询平台api资源 描述:查询平台api资源
%% OperationId:post_login
%% 请求:POST /iotapi/post_login
do_request(post_login, #{<<"type">> := <<"wechat">>, <<"openid">> := Openid}, _Context, _Req) ->
   dgiot_wechat:get_sns_user(Openid);

do_request(post_login, #{<<"type">> := <<"wechat">>, <<"jscode">> := JSCODE}, _Context, _Req) ->
    dgiot_wechat:get_sns(JSCODE);

do_request(post_login, #{<<"username">> := UserName, <<"password">> := Password}, _Context, _Req) ->
    dgiot_parse_handler:login_by_account(UserName, Password);

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.


get_app(AppName, Token) ->
    Query1 = #{<<"limit">> => 1, <<"keys">> => [<<"name">>, <<"tag">>], <<"where">> => #{<<"name">> => AppName}, <<"order">> => <<"-updatedAt">>},
    APIHub =
        case dgiot_parse:query_object(<<"_Role">>, Query1) of
            {ok, #{<<"results">> := []}} -> #{};
            {ok, #{<<"results">> := [App |_]}} ->
%%                ?LOG(info,"App ~p ",[App]),
                case maps:find(<<"tag">>, App) of
                    {ok, _Tag} ->
                        #{<<"tag">> := #{<<"appconfig">> := Config},
                            <<"objectId">> := Appid,
                            <<"name">> := Name} = App,
                        Config#{
                            <<"appid">> => Name,
                            <<"desc">> => Appid,
                            <<"access_token">> => Token
                        };
                    _ -> #{}
                end;
            _ -> #{}
        end,
%%    ?LOG(info,"APIHub ~p", [APIHub]),
    {200, APIHub}.



