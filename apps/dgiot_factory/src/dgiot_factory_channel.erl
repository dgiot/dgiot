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

-module(dgiot_factory_channel).
-behavior(dgiot_channelx).
-author("kenneth").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_bridge/include/dgiot_bridge.hrl").
-include("dgiot_factory.hrl").
-define(TYPE, <<"FACTORY">>).
-define(SHEETID(SHEET), <<SHEET/binary, "_id">>).
-define(MAX_BUFF_SIZE, 1024).
-record(state, {id, mod, product, env = #{}}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).
-export([get_id/2, after_handle/4, handle_data/7]).

%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?BACKEND_CHL,
    priority => 2,
    title => #{
        zh => <<"Device缓存通道"/utf8>>
    },
    description => #{
        zh => <<"Device缓存通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/device_profile.png">>,
        title => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        },
        description => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        }
    }
}).


start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, Args) ->
    State = #state{
        id = ChannelId,
        env = Args
    },
    dgiot_parse_hook:subscribe(<<"Device/*">>, put, ChannelId, [<<"content">>]),
    dgiot_parse_hook:subscribe(<<"Device/*">>, delete, ChannelId),
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "Channel ~p", [Event]),
    {ok, State}.


handle_message({sync_parse, _Pid, 'before', put, Token, <<"Device">>, #{<<"content">> := Content, <<"id">> := TaskDeviceId} = _QueryData}, State) ->
    case dgiot_device_cache:lookup(TaskDeviceId) of
        {ok, #{<<"productid">> := TaskProductId}} ->
            case Content of
                #{<<"person">> := #{<<"type">> := PersonType}} ->
                    case process_data(Content, PersonType, Token, TaskDeviceId) of
                        {BatchProductId, BatchDeviceId, BatchAddr, NewData} ->
                            handle_data(TaskProductId, TaskDeviceId, BatchProductId, BatchDeviceId, BatchAddr, PersonType, NewData),
                            {ok, State};
                        _ ->
                            {ok, State}
                    end;
                _ ->
                    {ok, State}
            end;
        _ ->
            {ok, State}
    end;
handle_message(Message, State) ->
    ?LOG(debug, "channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "Channel[~p,~p] stop", [ChannelType, ChannelId]),
    ok.


%% 工人 <<==>> 质检员
%%"0": "免检",  % 工人
%%"1": "合格",  %质检员
%%"2": "不合格",  %质检员
%%"3": "首检",  % 工人
%%"4": "过程检",  % 工人
%%"5": "尾检"   % 工人

%%process_content(#{<<"person_type">> := PersonType, <<"quality_type">> := QualityType} = Payload, ProductId, DeviceId, DevAddr, Id) ->
%%    save_data(ProductId, Id, DevAddr, DeviceId, PersonType, Payload),
%%    dgiot_data:insert(?FACTORY_QUALITY, {ProductId, Id, QualityType}, #{<<"person_type">> => PersonType, <<"quality_type">> => QualityType});
%%
%%process_content(#{<<"person_type">> := PersonType} = Payload, ProductId, DeviceId, DevAddr, Id) ->
%%    save_data(ProductId, Id, DevAddr, DeviceId, PersonType, Payload).

handle_data(TaskProductId, TaskDeviceId, BatchProductId, BatchDeviceId, BatchAddr, PersonType, NewData) ->
    NewPayLoad = run_factory_hook(TaskProductId, TaskDeviceId, BatchDeviceId, PersonType, NewData),
    OldData = get_card_data(BatchProductId, BatchDeviceId),
    dgiot_data:insert(?FACTORY_ORDER, {BatchProductId, BatchDeviceId, PersonType}, NewPayLoad),
    Content = maps:merge(OldData, NewPayLoad),
    dgiot_parse:update_object(<<"Device">>, BatchDeviceId, #{<<"content">> => dgiot_map:unflatten(Content)}),
    dgiot_task:save_td(BatchProductId, BatchAddr, Content, #{}).
get_card_data(BatchProductId, BatchDeviceId) ->
    DevcieTypeList = dgiot_product:get_devicetype(BatchProductId),
    lists:foldl(
        fun(DeviceType, Acc) ->
            case dgiot_data:get(?FACTORY_ORDER, {BatchProductId, BatchDeviceId, DeviceType}) of
                not_find ->
                    Acc;
                Res ->
                    maps:merge(Acc, Res)
            end
        end, #{}, DevcieTypeList).

process_data(Content, PersonType, Token, TaskDeviceId) ->
    FlatMap = dgiot_map:flatten(Content),
    case dgiot_parse:get_object(<<"Device">>, TaskDeviceId) of
        {ok, #{<<"name">> := OrderName, <<"product">> := #{<<"objectId">> := TaskProductId}}} ->
            {ok, {BatchProductId, BatchDeviceId, BatchAddr}} = process_roll_dev(TaskProductId, TaskDeviceId, OrderName, Token, FlatMap),
            NewData = init_data(TaskProductId, TaskDeviceId, BatchDeviceId, FlatMap, PersonType, Token),
            {BatchProductId, BatchDeviceId, BatchAddr, NewData};
        _ ->
            error
    end.

init_data(TaskProductId, TaskDeviceId, BatchDeviceId, FlatMap, PersonType, Token) ->
    NumData = dgiot_factory_utils:turn_num(FlatMap, TaskProductId, PersonType),
    maps:merge(NumData, #{<<"person_sessiontoken">> => Token, <<"person_deviceid">> => TaskDeviceId, ?SHEETID(PersonType) => BatchDeviceId, <<"person_sheetsid">> => BatchDeviceId}).
after_handle(ProductId, DevAddr, Payload, _Type) ->
    Use = turn_user(Payload),
    dgiot_task:save_td(ProductId, DevAddr, Payload#{<<"person_sessiontoken">> => Use}, #{}).

get_id(DevAddr, Type) ->
    Time = dgiot_utils:to_binary(dgiot_datetime:timestamp()),
    Bin = dgiot_utils:to_binary(Type),
    <<ObjID:10/binary, _/binary>> = dgiot_utils:to_md5(<<Bin/binary, DevAddr/binary, Time/binary>>),
    Res = string:to_upper(dgiot_utils:to_list(ObjID)),
    dgiot_utils:to_binary(Res).
turn_user(#{<<"person_sessiontoken">> := SessionToken}) ->
    case dgiot_auth:get_session(SessionToken) of
        {ok, #{<<"user">> := User}} ->
            User;
        _ ->
            SessionToken
    end.

run_factory_hook(TaskProductId, TaskDeviceId, RollDeviceId, PersonType, NewData) ->
    case dgiot_hook:run_hook({factory, TaskProductId, PersonType}, [TaskProductId, TaskDeviceId, RollDeviceId, PersonType, NewData]) of
        {ok, [{ok, Res}]} ->
            Res;
        _ ->
            NewData
    end.


get_sub_product(ProductId) ->
    case dgiot_hook:run_hook({factory, get_sub_product}, ProductId) of
        {ok, [{ok, SubProduct}]} ->
            SubProduct;
        _ ->
            ProductId
    end.

process_roll_dev(TaskProductId, TaskDeviceId, OrderName, SessionToken, FlatMap) ->
    {BatchProductId, BatchDeviceId, BatchAddr} = get_roll_dev_id(TaskProductId, FlatMap),
    case dgiot_device_cache:lookup(BatchDeviceId) of
        {ok, #{<<"acl">> := Acl}} ->
            io:format("~s ~p RollDeviceId = ~p ~n", [?FILE, ?LINE, BatchDeviceId]),
            NewAcl = get_new_acl(SessionToken, Acl),
%%            dgiot_device_cache:put(#{<<"objectId">> => TaskDeviceId, <<"ACL">> => NewAcl}),
            dgiot_parse:update_object(<<"Device">>, TaskDeviceId, #{<<"ACL">> => NewAcl}),
            dgiot_device:save_subdevice(BatchDeviceId, TaskDeviceId, 1),
            {ok, {BatchProductId, BatchDeviceId, BatchAddr}};
        _ ->
            NewAcl = get_new_acl(SessionToken, []),
            Device = #{
                <<"objectId">> => BatchDeviceId,
                <<"devaddr">> => BatchAddr,
                <<"name">> => OrderName,
                <<"ACL">> => NewAcl,
                <<"product">> => #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Product">>,
                    <<"objectId">> => BatchProductId
                }},
%%            dgiot_device_cache:post(Device),
            dgiot_parse:create_object(<<"Device">>, Device),
            dgiot_device:save_subdevice(BatchDeviceId, TaskDeviceId, 1),
            {ok, {BatchProductId, BatchDeviceId, BatchAddr}}

    end.
%%
%%


get_roll_dev_id(ProductId, FlatMap) ->
    BatchAddr = case maps:find(<<"person_devaddr">>, FlatMap) of
                    {ok, Devaddr} ->
                        Devaddr;
                    _ ->
                        dgiot_datetime:format("YYYY-MM-DD HH:NN:SS")
                end,
    BatchProductId = get_sub_product(ProductId),
    BatchDeviceId = dgiot_parse_id:get_deviceid(BatchProductId, BatchAddr),
    {BatchProductId, BatchDeviceId, BatchAddr}.

get_new_acl(SessionToken, Acl) ->
    PersonAcl = case dgiot_auth:get_session(SessionToken) of
                    #{<<"roles">> := Roles} = _User ->
                        [#{<<"name">> := Role} | _] = maps:values(Roles),
                        #{<<"role:", Role/binary>> => #{
                            <<"read">> => true,
                            <<"write">> => true}
                        };
                    Err -> {400, Err}
                end,
    OldAcl = lists:foldl(
        fun(X, Acc) ->
            Acc#{X => #{<<"read">> => true, <<"write">> => true}}
        end, #{}, Acl),
    maps:merge(OldAcl, PersonAcl).


