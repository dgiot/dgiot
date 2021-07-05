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
-module(license_loader).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).

-define(DGIOT_METRICS_METER, <<"DGIOT_METRICS_METER">>).
%% API
-export([start_link/0
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    on_client_authenticate/3,
    on_client_disconnected/4,
    random/0,
    get_license_host/0,
    get_deploy/1,
    generate_setup/2,
    generate_update/2,
    generate_lictool/2,
    load_config/2
]).

-record(state, {}).

start() ->
    Env = #{},
    emqx:hook('client.authenticate', fun ?MODULE:on_client_authenticate/3, [Env]),
    emqx:hook('client.disconnected', fun ?MODULE:on_client_disconnected/4, [Env]).

stop() ->
    emqx:unhook('client.authenticate', fun ?MODULE:on_client_authenticate/3),
    emqx:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/4),
    ok.

on_client_authenticate(#{clientid := <<"swlic_", Key/binary>>, peerhost := Peerhost, username := UserName, password := Password}, AuthResult, _Env) ->
    ?LOG(info, "UserName ~p", [UserName]),
    NewAuthResult =
        case dgiot_parse_handler:login_by_token(UserName, Password) of
            {ok, #{<<"objectId">> := UserObjectId, <<"sessionToken">> := Session}} ->
                case dgiot_parse:query_object(<<"License">>, #{<<"where">> => #{<<"key">> => Key}}) of
                    {ok, #{<<"results">> := [#{<<"objectId">> := ObjectId} | _]}} ->
                        dgiot_parse:update_object(<<"License">>, ObjectId, #{
                            <<"key">> => Key,
                            <<"public_ip">> => dgiot_utils:to_binary(inet:ntoa(Peerhost)),
                            <<"is_online">> => true});
                    _ ->
                        {ok, #{<<"name">> := Title}} = dgiot_parse:get_object(<<"_Role">>, UserName),
                        dgiot_parse:create_object(<<"License">>, #{
                            <<"ACL">> =>
                            #{UserObjectId => #{<<"read">> => true, <<"write">> => true}},
                            <<"appname">> => Title,
                            <<"app">> => #{
                                <<"__type">> => <<"Pointer">>,
                                <<"className">> => <<"App">>,
                                <<"objectId">> => UserName
                            },
                            <<"key">> => Key,
                            <<"is_online">> => true,
                            <<"status">> => <<"unauthorized">>,
                            <<"type">> => <<"standard">>,
                            <<"version">> => <<"1.0.0">>,
                            <<"public_ip">> => dgiot_utils:to_binary(inet:ntoa(Peerhost))})
                end,
                Args = #{
                    <<"appid">> => UserName,
                    <<"appsecret">> => Password,
                    <<"key">> => Key,
                    <<"objectid">> => UserObjectId,
                    <<"sessionToken">> => Session
                },
                ?LOG(info, "Args ~p", [Args]),
                supervisor:start_child(dgiot_license_install, [Args]),
                ?LOG(info, "Args11 ~p", [Args]),
                AuthResult#{auth_result => success, anonymous => false};
            _ -> AuthResult#{auth_result => fail, anonymous => false}
        end,
    {stop, NewAuthResult};

on_client_authenticate(_ClientInfo, AuthResult, _Env) ->
    {stop, AuthResult}.

on_client_disconnected(#{clientid := <<"swlic_", Key/binary>>}, _ReasonCode, _, _Env) ->
    case dgiot_parse:query_object(<<"License">>, #{<<"where">> => #{<<"key">> => Key}}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := ObjectId} | _]}} ->
            dgiot_parse:update_object(<<"License">>, ObjectId, #{
                <<"is_online">> => false});
        _ ->
            pass
    end,
    ok;

on_client_disconnected(#{clientid := _ClientId}, _ReasonCode, _, _Env) ->
    ok.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(2000, self(), init),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(init, State) ->
%%    dgiot_livequery:subscribe(<<"License">>, #{}),
%%    Path = <<"http://115.159.59.185:5080/hooks/license_trigger/do">>,
%%    Triggers = [<<"afterSave">>, <<"afterDelete">>],
%%%%    [dgiot_parse:add_trigger(<<"License">>, TriggerName, Path)||TriggerName<-Triggers],
    start(),
    Key = dgiot_license:get_hardkey(),
    dgiot_mqtt:subscribe(Key),
    {noreply, State};

%1、 查询ip地址
%cmd：
%   <<"ifconfig |grep broadcast">>
%ack:
%   <<"        inet 10.0.0.16  netmask 255.255.255.0  broadcast 10.0.0.255">>

%2、查序列号
%cmd:
%  <<"dmidecode |grep 'Serial Number'  | md5sum | cut -d ' ' -f1">>
%ack:
%   <<"6c06075470c1ca8ea3d35e2d6fcc358b">>

%3、查处理器
%cmd:
% <<"dmidecode -t processor | md5sum | cut -d ' ' -f1">>
%ack:
%   <<"a62f7a55dfcc0070b98e235707c5b3e5">>

handle_info({deliver, _Topic, Msg}, State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    case Payload of
        <<"iot">> ->
            dgiot_install:start(#{product => Payload, webserver => #{name => dgiot_rest}});
        APP ->
            dgiot_install:start(#{product => APP, webserver => #{name => dgiot_rest}})
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%随机生成16位Key值
random() ->
    dgiot_license:to_md5(io_lib:format("~p", [erlang:make_ref()])).

get_license_host() ->
    dgiot_utils:get_wlanip().
%%    case dgiot_utils:get_wlanip() of
%%        {ok, Host} -> dgiot_utils:to_binary(Host);
%%        _ -> <<"license.iotn2n.com">>
%%    end.

get_deploy(#{
    <<"type">> := <<"standard">>,
    <<"version">> := <<"1.0.0">>,
    <<"license">> := License,
    <<"software">> := Software,
    <<"appid">> := Appid,
    <<"appsecret">> := Appsecret,
    <<"private_ip">> := Private_ip,
    <<"public_ip">> := PublicIp}) ->
    maps:merge(
        load_config(<<"standard">>, <<"1.0.0">>),
        #{
            <<"standard_private_ip">> => Private_ip,
            <<"standard_public_ip">> => PublicIp,
            <<"standard_dgiot_license">> => License,
            <<"appid">> => Appid,
            <<"appsecret">> => Appsecret,
            <<"pg_pwd">> => random(),
            <<"parse_server_appid">> => random(),
            <<"parse_server_master_key">> => random(),
            <<"parse_server_readonly_master_key">> => random(),
            <<"parse_server_file_key">> => random(),
            <<"parse_server_client_key">> => random(),
            <<"parse_server_js_key">> => random(),
            <<"parse_server_rest_key">> => random(),
            <<"parse_server_donet_key">> => random(),
            <<"parse_server_webhook_key">> => random(),
            <<"parse_server_pwd">> => random(),
            <<"td_passwd">> => random(),
            <<"license_host">> => get_license_host(),
            <<"dgiot_iot_software">> => Software
        }
    );

get_deploy(#{
    <<"type">> := <<"standard">>,
    <<"version">> := <<"1.0.0">>
} = _NodeInfo) ->
    maps:merge(
        load_config(<<"standard">>, <<"1.0.0">>),
        #{
            <<"pg_pwd">> => random(),
            <<"parse_server_port">> => 1337,
            <<"parse_server_appid">> => random(),
            <<"parse_server_master_key">> => random(),
            <<"parse_server_readonly_master_key">> => random(),
            <<"parse_server_file_key">> => random(),
            <<"parse_server_client_key">> => random(),
            <<"parse_server_js_key">> => random(),
            <<"parse_server_rest_key">> => random(),
            <<"parse_server_donet_key">> => random(),
            <<"parse_server_webhook_key">> => random(),
            <<"parse_server_pwd">> => random(),
            <<"td_passwd">> => random(),
            <<"license_host">> => get_license_host()
        }
    ).

generate_lictool(FileName, Vars) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    TplPath = Root ++ "/setup_lictool.sh",
    case erlydtl:compile({file, TplPath}, dgiot_render, [{out_dir, false}]) of
        {ok, Render} ->
            {ok, IoList} = Render:render(Vars),
            BinFile = unicode:characters_to_binary(IoList),
            case zip:create(FileName, [{"setup_lictool.sh", BinFile}], [memory]) of
                {ok, {_ZipFile, Bin}} ->
                    {ok, Bin};
                {error, What} ->
                    {error, What}
            end;
        error ->
            {error, compile_error}
    end.

%% 根据license来判断选择安装脚本模版
generate_setup(FileName, License) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    ?LOG(info, "License ~p FileName ~p ~n", [License, FileName]),
    {ProductType, ProductVersion, Vars} =
        case dgiot_parse:query_object(<<"License">>, #{<<"where">> =>
        #{<<"license">> => License}}) of
            {ok, #{<<"results">> := [#{<<"type">> := Type, <<"version">> := Version, <<"product">> := Product} | _]}} ->
                {Type, Version, lists:map(fun({K, V}) -> {dgiot_utils:to_atom(K), V} end, maps:to_list(Product))};
            _ ->
                {<<"standard">>, <<"1.0.0">>, []}
        end,
    TplPath = Root ++ "setup_" ++ dgiot_utils:to_list(ProductType) ++ "_" ++ dgiot_utils:to_list(ProductVersion) ++ ".sh",
    ?LOG(info, "Vars ~p", [Vars]),
    case erlydtl:compile({file, TplPath}, dgiot_render, [{out_dir, false}]) of
        {ok, Render} ->
            {ok, IoList} = Render:render(Vars),
            BinFile = unicode:characters_to_binary(IoList),
            case zip:create(FileName, [{"setup.sh", BinFile}], [memory]) of
                {ok, {_ZipFile, Bin}} ->
                    {ok, Bin};
                {error, What} ->
                    {error, What}
            end;
        error ->
            {error, compile_error}
    end.

generate_update(FileName, License) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    ?LOG(info, "License ~p FileName ~p ~n", [License, FileName]),
    {ProductType, ProductVersion, Vars} =
        case dgiot_parse:query_object(<<"License">>, #{<<"where">> =>
        #{<<"license">> => License}}) of
            {ok, #{<<"results">> := [#{<<"type">> := Type, <<"version">> := Version, <<"product">> := Product} | _]}} ->
                {Type, Version, lists:map(fun({K, V}) -> {dgiot_utils:to_atom(K), V} end, maps:to_list(Product))};
            _ ->
                {<<"standard">>, <<"1.0.0">>, []}
        end,
    TplPath = Root ++ "update_" ++ dgiot_utils:to_list(ProductType) ++ "_" ++ dgiot_utils:to_list(ProductVersion) ++ ".sh",
    case erlydtl:compile({file, TplPath}, dgiot_render, [{out_dir, false}]) of
        {ok, Render} ->
            {ok, IoList} = Render:render(Vars),
            BinFile = unicode:characters_to_binary(IoList),
            case zip:create(FileName, [{"update.sh", BinFile}], [memory]) of
                {ok, {_ZipFile, Bin}} ->
                    {ok, Bin};
                {error, What} ->
                    {error, What}
            end;
        error ->
            {error, compile_error}
    end.


load_config(ProductType, ProductVersion) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    TplPath = Root ++ "setup_" ++ dgiot_utils:to_list(ProductType) ++ "_" ++ dgiot_utils:to_list(ProductVersion) ++ ".json",
    case file:read_file(TplPath) of
        {ok, Bin} ->
            jsx:decode(Bin, [{labels, binary}, return_maps]);
        {error, _Reason} ->
            #{}
    end.
