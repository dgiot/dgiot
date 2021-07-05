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
-module(license_worker).
-author("johnliu").
-include("dgiot_license.hrl").
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).



-record(task, {appid, appsecret, key, sessionToken, step = <<"core">>, status = <<"boot_up">>}).
%%%===================================================================
%%% API
%%%===================================================================
start_link(#{<<"appid">> := AppId, <<"appsecret">> := AppSecret, <<"key">> := Key} = State) ->
    ?LOG(info, "State ~p", [State]),
    case dgiot_data:lookup(?DGIOT_LIC_WORK, {AppId, AppSecret, Key}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end,
    gen_server:start_link(?MODULE, [State], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"appid">> := AppId, <<"appsecret">> := AppSecret, <<"key">> := Key,
    <<"sessionToken">> := SessionToken}]) ->
    SubTopic = <<"swlic_", Key/binary, "/", AppSecret/binary>>,
    dgiot_mqtt:subscribe(SubTopic),
    erlang:send_after(2000, self(), get_node_info),
%%    dgiot_livequery:subscribe(SessionToken, <<"License">>, #{}),
    dgiot_data:insert(?DGIOT_LIC_WORK, {AppId, AppSecret, Key}, #{}),
    ?LOG(info, "SessionToken  ~p", [SessionToken]),
    {ok, #task{appid = AppId, appsecret = AppSecret, sessionToken = SessionToken, key = Key, step = <<"core">>, status = <<"boot_up">>}};

init(A) ->
    io:format("A ~p ", [A]).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};

handle_info(stop, State) ->
    {stop, normal, State};

%延迟启动
handle_info(get_node_info, #task{appsecret = AppSecret, key = Key} = State) ->
    PubTopic = <<AppSecret/binary, "/", "swlic_", Key/binary>>,
    Core = <<"cat /proc/cpuinfo| grep 'cpu cores'| uniq |awk '{print $4}'">>,
    ?LOG(info, "Core ~p", [Core]),
    dgiot_mqtt:publish(?MODULE, PubTopic, <<Core/binary>>),
    {noreply, State};

handle_info({livequery, #{<<"object">> := Object}}, #task{step = login} = State) ->
    {noreply, do_depploy(Object, State)};

handle_info({livequery, _Other}, State) ->
    {noreply, State};

handle_info({deliver, _Topic, Msg}, #task{step = login} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    case jsx:is_json(Payload) of
        true ->
            ?LOG(info, "R ~p ~n", [jsx:decode(Payload, [{labels, binary}, return_maps])]),
            State;
        _ ->
            [NewPayload | _] = re:split(Payload, <<"\n">>),
            ?LOG(info, "Payload ~p ~n", [NewPayload])
    end,
    {noreply, State};

handle_info({deliver, _Topic, Msg}, #task{appsecret = AppSecret, step = <<"core">>, key = Key} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    ?LOG(info, "Payload ~p", [Payload]),
    save_node_info(<<"core">>, Payload, Key),
    PubTopic = <<AppSecret/binary, "/", "swlic_", Key/binary>>,
    Memory = <<"free -mh |grep 'Mem: '| uniq |awk '{print $2}'">>,
    dgiot_mqtt:publish(?MODULE, PubTopic, <<Memory/binary>>),
    {noreply, State#task{step = <<"memory">>}};

handle_info({deliver, _Topic, Msg}, #task{appsecret = AppSecret, step = <<"memory">>, key = Key} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    save_node_info(<<"memory">>, Payload, Key),
    PubTopic = <<AppSecret/binary, "/", "swlic_", Key/binary>>,
    Disk = <<"lsblk  |grep 'vda ' |awk '{print $4}'|cut -c 1-">>,
    dgiot_mqtt:publish(?MODULE, PubTopic, <<Disk/binary>>),
    {noreply, State#task{step = <<"disk">>}};

handle_info({deliver, _Topic, Msg}, #task{appsecret = AppSecret, step = <<"disk">>, key = Key} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    save_node_info(<<"disk">>, Payload, Key),
    PubTopic = <<AppSecret/binary, "/", "swlic_", Key/binary>>,
    Private_ip = <<"ifconfig eth0 |grep 'broadcast ' |awk '{print $2}'|cut -c 1-">>,
    dgiot_mqtt:publish(?MODULE, PubTopic, <<Private_ip/binary>>),
    {noreply, State#task{step = <<"private_ip">>}};

handle_info({deliver, _Topic, Msg}, #task{appsecret = AppSecret, step = <<"private_ip">>, key = Key} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    save_node_info(<<"private_ip">>, Payload, Key),
    PubTopic = <<AppSecret/binary, "/", "swlic_", Key/binary>>,
    Mac = <<"ifconfig eth0 |grep '(Ethernet)' |awk '{print $2}'|cut -c 1-">>,
    dgiot_mqtt:publish(?MODULE, PubTopic, <<Mac/binary>>),
    {noreply, State#task{step = <<"mac">>}};

handle_info({deliver, _Topic, Msg}, #task{step = <<"mac">>, key = Key} = State) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    save_node_info(<<"mac">>, Payload, Key),
    {noreply, State#task{step = login, status = <<"ready">>}};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #task{appsecret = AppSecret, key = Key, sessionToken = _SessionToken} = _State) ->
    ?LOG(info, "_State ~p ~n", [_State]),
    dgiot_mqtt:unsubscribe(<<"swlic_", Key/binary, "/", AppSecret/binary>>),
%%    dgiot_livequery:stop(SessionToken),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

save_node_info(Step, Payload, Key) ->
    case jsx:is_json(Payload) of
        true ->
            ?LOG(info, "R ~p ~n", [jsx:decode(Payload, [{labels, binary}, return_maps])]);
        _ ->
            [NewPayload | _] = re:split(Payload, <<"\n">>),
            ?LOG(info, "~p~n", [NewPayload]),
            case dgiot_parse:query_object(<<"License">>, #{<<"where">> => #{<<"key">> => Key}}) of
                {ok, #{<<"results">> := [#{<<"objectId">> := ObjectId} | _]}} ->
                    dgiot_parse:update_object(<<"License">>, ObjectId, #{Step => NewPayload});
                _ ->
                    pass
            end
    end.

do_depploy(#{
    <<"objectId">> := ObjectId,
    <<"status">> := <<"start_install">>,
    <<"type">> := Type,
    <<"key">> := Key,
    <<"license">> := License
} = Object, #task{status = <<"ready">>, appsecret = AppSecret, appid = AppId} = State) ->
    Status =
        case dgiot_license:check_lincense(Type, Key, dgiot_utils:to_list(License)) of
            true ->
                Product = license_loader:get_deploy(Object#{<<"appsecret">> => AppSecret, <<"appid">> => AppId}),
                PubTopic = <<AppSecret/binary, "/", "swlic_", Key/binary>>,
                LicenseHost = license_loader:get_license_host(),
                CMD = <<"rm /tmp/dgiot/ -rf && mkdir -p /tmp/dgiot/ && wget http://", LicenseHost/binary, ":5080/iotapi/licsetup?license=", License/binary, " -O /tmp/dgiot/setup.zip && cd /tmp/dgiot && unzip /tmp/dgiot/setup.zip && /bin/sh /tmp/dgiot/setup.sh">>,
                ?LOG(info, "do_depploy CMD ~p ~n", [CMD]),
                dgiot_mqtt:publish(?MODULE, PubTopic, <<CMD/binary>>),
                dgiot_parse:update_object(<<"License">>, ObjectId, #{<<"status">> => <<"installing">>, <<"product">> => Product}),
                <<"installing">>;
            false ->
                dgiot_parse:update_object(<<"License">>, ObjectId, #{<<"status">> => <<"unauthorized">>, <<"product">> => #{}}),
                <<"unauthorized">>
        end,
    State#task{status = Status};

do_depploy(#{
    <<"objectId">> := ObjectId,
    <<"status">> := <<"start_update">>,
    <<"type">> := Type,
    <<"key">> := Key,
    <<"license">> := License
} = Object, #task{status = <<"ready">>, appsecret = AppSecret, appid = AppId} = State) ->
    Status =
        case dgiot_license:check_lincense(Type, Key, dgiot_utils:to_list(License)) of
            true ->
                Product = license_loader:get_deploy(Object#{<<"appsecret">> => AppSecret, <<"appid">> => AppId}),
                PubTopic = <<AppSecret/binary, "/", "swlic_", Key/binary>>,
                LicenseHost = license_loader:get_license_host(),
                CMD = <<"rm /tmp/dgiot/ -rf && mkdir -p /tmp/dgiot/ && wget http://", LicenseHost/binary, ":5080/iotapi/licupdate?license=", License/binary, " -O /tmp/dgiot/update.zip && cd /tmp/dgiot && unzip /tmp/dgiot/update.zip && /bin/sh /tmp/dgiot/update.sh">>,
                ?LOG(info, "do_depploy CMD ~p ~n", [CMD]),
                dgiot_mqtt:publish(?MODULE, PubTopic, <<CMD/binary>>),
                dgiot_parse:update_object(<<"License">>, ObjectId, #{<<"status">> => <<"installing">>, <<"product">> => Product}),
                <<"installing">>;
            false ->
                dgiot_parse:update_object(<<"License">>, ObjectId, #{<<"status">> => <<"unauthorized">>, <<"product">> => #{}}),
                <<"unauthorized">>
        end,
    State#task{status = Status};

do_depploy(_Other, State) ->
    ?LOG(info, "do_depploy State ~p~n", [State]),
    State.
