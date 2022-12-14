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
-define(WORKERCATEGORY, <<"bf6cbee357">>).
-record(state, {id, mod, product, env = #{}}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).
-export([get_id/2, after_handle/4, handle_data/8, get_card_data/2]).
-export([get_sub_product/1, get_new_acl/2, init_worker_device/3]).
-export([get_roll_dev_id/2]).
-export([save2td/3]).
%% 注册通道类型
-channel_type(#{
    cType => ?TYPE,
    type => ?PROTOCOL_CHL,
    priority => 2,
    title => #{
        zh => <<"数字工厂通道"/utf8>>
    },
    description => #{
        zh => <<"数字工厂通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/factory.png">>,
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
init(?TYPE, ChannelId, #{<<"product">> := Products} = Args) ->
    State = #state{
        id = ChannelId,
        env = Args
    },
    lists:map(
        fun
            ({ProductId, _}) ->
                case dgiot_product:get(ProductId) of
                    {ok, #{<<"devType">> := DevType, <<"name">> := Name}} ->
                        TempProductId = get_wokrer_id(Name, DevType),
                        case TempProductId of
                            ProductId ->
                                dgiot_data:insert({ChannelId, worker}, ProductId);
                            _ ->
                                pass
                        end;
                    _ ->
                        pass
                end
        end, Products),
    dgiot_parse_hook:subscribe(<<"Device/*">>, put, ChannelId, [<<"content">>]),
    dgiot_parse_hook:subscribe(<<"Device/*">>, delete, ChannelId),
    dgiot_parse_hook:subscribe(<<"_User/*">>, post, ChannelId),
    dgiot_parse_hook:subscribe(<<"_User/*">>, put, ChannelId),
    dgiot_parse_hook:subscribe(<<"_User/*">>, delete, ChannelId),
    {ok, State, []}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "Channel ~p", [Event]),
    {ok, State}.


handle_message({sync_parse, _Pid, 'after', put, _Token, <<"_User">>, #{<<"objectId">> := UserId} = _QueryData},
    #state{id = ChannelId} = State) ->
    case dgiot_data:get({ChannelId, worker}) of
        not_find ->
            pass;
        ProductId ->
            case dgiot_parse:get_object(<<"_User">>, UserId) of
                {ok, #{<<"username">> := WorkerNum, <<"nick">> := WorkerName}} ->
                    io:format("~s ~p ProductId = ~p ~n",[?FILE,?LINE,ProductId]),
                    init_worker_device(ProductId, WorkerNum, WorkerName);
                _ ->
                    pass
            end
    end,
    {ok, State};
handle_message({sync_parse, _Pid, 'after', post, _Token, <<"_User">>, #{<<"objectId">> := UserId} = _QueryData},
    #state{id = ChannelId} = State) ->
    case dgiot_data:get({ChannelId, worker}) of
        not_find ->
            pass;
        ProductId ->
            case dgiot_parse:get_object(<<"_User">>, UserId) of
                {ok, #{<<"username">> := WorkerNum, <<"nick">> := WorkerName}} ->
                    init_worker_device(ProductId, WorkerNum, WorkerName);
                _ ->
                    pass
            end
    end,
    {ok, State};
handle_message({sync_parse, _Pid, 'before', put, Token, <<"Device">>,
    #{<<"content">> := Content, <<"id">> := TaskDeviceId} = _QueryData},
    #state{id = ChannelId} = State) ->
    io:format("~s ~p TaskDeviceId = ~p ~n", [?FILE, ?LINE, TaskDeviceId]),
    case dgiot_device_cache:lookup(TaskDeviceId) of
        {ok, #{<<"productid">> := TaskProductId}} ->
%%    case dgiot_parse:get_object(<<"Device">>,TaskDeviceId) of
%%        {ok,#{<<"product">> := #{<<"objectId">> := TaskProductId}}} ->
            case Content of
                #{<<"person">> := #{<<"type">> := PersonType}} ->
                    dgiot_metrics:inc(dgiot_factory, <<"input_num">>, 1),
                    case process_data(Content, PersonType, Token, TaskDeviceId) of
                        {BatchProductId, BatchDeviceId, BatchAddr, NewData} ->
                            handle_data(TaskProductId, TaskDeviceId, BatchProductId, BatchDeviceId, BatchAddr, PersonType, NewData, ChannelId),
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


handle_data(_TaskProductId, TaskDeviceId, BatchProductId, BatchDeviceId, BatchAddr, PersonType, NewData, ChannelId) ->
    io:format("~s ~p Type = ~p  ~n", [?FILE, ?LINE,PersonType]),
    io:format("~s ~p BatchProductId = ~p  ~n", [?FILE, ?LINE,BatchProductId]),
    io:format("~s ~p BatchDeviceId = ~p  ~n", [?FILE, ?LINE,BatchDeviceId]),
    NewPayLoad = run_factory_hook(_TaskProductId, TaskDeviceId, BatchProductId, BatchDeviceId, PersonType, NewData, ChannelId),
    dgiot_data:insert(?FACTORY_ORDER, {BatchProductId, BatchDeviceId, PersonType}, NewPayLoad),
    OldData = get_card_data(BatchProductId, BatchDeviceId),
    ALlData = dgiot_map:merge(OldData, NewPayLoad),
    save2parse(BatchProductId, BatchDeviceId, ALlData),
    save2td(BatchProductId, BatchAddr, ALlData).
get_card_data(BatchProductId, BatchDeviceId) ->
    DevcieTypeList = dgiot_product:get_devicetype(BatchProductId) -- [<<"quality">>],
    lists:foldl(
        fun(DeviceType, Acc) ->
            case dgiot_data:get(?FACTORY_ORDER, {BatchProductId, BatchDeviceId, DeviceType}) of
                not_find ->
                    Acc;
                Res ->
                    dgiot_map:merge(Acc, maps:without([<<"quality">>],Res))
            end
        end, #{}, DevcieTypeList).

process_data(FlatMap, PersonType, Token, TaskDeviceId) ->
    case dgiot_parse:get_object(<<"Device">>, TaskDeviceId) of
        {ok, #{<<"name">> := OrderName, <<"product">> := #{<<"objectId">> := TaskProductId}}} ->
            {ok, {BatchProductId, BatchDeviceId, BatchAddr}} = process_roll_dev(TaskProductId, TaskDeviceId, OrderName, Token, FlatMap),
            NewData = init_data(TaskProductId, TaskDeviceId, BatchDeviceId, FlatMap, PersonType, Token),
            {BatchProductId, BatchDeviceId, BatchAddr, NewData};
        _ ->
            error
    end.

init_data(_TaskProductId, TaskDeviceId, BatchDeviceId, FlatMap, _PersonType, Token) ->
%%    maps:merge(FlatMap, #{<<"person_sessiontoken">> => Token, <<"person_deviceid">> => TaskDeviceId, <<"person_sheetsid">> => BatchDeviceId}).
    dgiot_map:merge(FlatMap, #{<<"person">> => #{<<"sessiontoken">> => Token, <<"deviceid">> => TaskDeviceId, <<"sheetsid">> => BatchDeviceId}}).

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

run_factory_hook(TaskProductId, TaskDeviceId, BatchProductId, BatchDeviceId, PersonType, NewData, ChannelId) ->
    case dgiot_hook:run_hook({factory, TaskProductId, PersonType}, [BatchProductId, TaskDeviceId, BatchDeviceId, PersonType, NewData, ChannelId]) of
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
    {BatchProductId, BatchDeviceId, BatchAddr} = case get_roll_dev_id(TaskProductId, FlatMap) of
                                                     {A, B, C} ->
                                                         {A, B, C};
                                                     _ -> {<<"1">>, <<"1">>, <<"1">>}
                                                 end,
    io:format("~s ~p BatchDeviceId = ~p ~n", [?FILE, ?LINE, BatchDeviceId]),
    case dgiot_device_cache:lookup(BatchDeviceId) of
        {ok, #{<<"acl">> := Acl}} ->
            NewAcl = get_new_acl(SessionToken, Acl),
            dgiot_parse:update_object(<<"Device">>, BatchDeviceId, #{<<"ACL">> => NewAcl, <<"isEnable">> => true}),
            dgiot_device:save_subdevice(BatchDeviceId, TaskDeviceId, 1),
            {ok, {BatchProductId, BatchDeviceId, BatchAddr}};
        _ ->
            NewAcl = get_new_acl(SessionToken, []),
            Device = #{
                <<"objectId">> => BatchDeviceId,
                <<"devaddr">> => BatchAddr,
                <<"name">> => OrderName,
                <<"ACL">> => NewAcl,
                <<"basedata">> => #{},
                <<"product">> => #{
                    <<"__type">> => <<"Pointer">>,
                    <<"className">> => <<"Product">>,
                    <<"objectId">> => BatchProductId
                }},
            dgiot_device_cache:post(Device),
            dgiot_parse:create_object(<<"Device">>, Device),
            dgiot_metrics:inc(dgiot_factory, <<"batch_num">>, 1),
            dgiot_device:save_subdevice(BatchDeviceId, TaskDeviceId, 1),
            {ok, {BatchProductId, BatchDeviceId, BatchAddr}}

    end.



get_roll_dev_id(ProductId, FlatMap) ->
    BatchProductId = get_sub_product(ProductId),
    case maps:find(<<"sheetsid">>, maps:get(<<"person">>, FlatMap, #{})) of
        {ok, BatchDeviceId} ->
            io:format("~s ~p BatchDeviceId = ~p ~n", [?FILE, ?LINE, BatchDeviceId]),
            case dgiot_device:lookup(BatchDeviceId) of
                {ok, #{<<"devaddr">> := BatchAddr}} ->
                    {BatchProductId, BatchDeviceId, BatchAddr};
                _ ->
                    error
            end;
        _ ->
            BatchAddr = dgiot_utils:to_binary(dgiot_datetime:nowstamp()),
            BatchDeviceId = list_to_binary(string:to_upper(binary_to_list(dgiot_parse_id:get_deviceid(BatchProductId, BatchAddr)))),
            {BatchProductId, BatchDeviceId, BatchAddr}
    end.

get_new_acl(SessionToken, Acl) ->
    case dgiot_auth:get_session(SessionToken) of
        #{<<"roles">> := Roles} ->
            UserRoleList = maps:fold(
                fun(_, V, Acc) ->
                    case maps:find(<<"name">>, V) of
                        {ok, Role} ->
                            Acc ++ [<<"role:", Role/binary>>];
                        _ ->
                            Acc
                    end
                end, [], Roles),
            NewRoleList = dgiot_utils:unique_2(Acl ++ UserRoleList),
            lists:foldl(
                fun(X, Acc) ->
                    maps:merge(Acc, #{dgiot_utils:to_binary(X) => #{<<"read">> => true, <<"write">> => true}})
                end, #{}, NewRoleList);
        Err -> {400, Err}
    end.


save2parse(BatchProductId, BatchDeviceId, ALlData) ->
    Content = case dgiot_hook:run_hook({factory, BatchProductId, beforeParse}, [ALlData]) of
                  {ok, [{ok, Res}]} ->
                      Res;
                  _ ->
                      io:format("~s ~p BatchDeviceId = ~p ~n", [?FILE, ?LINE, BatchDeviceId]),
                      ALlData
              end,

    dgiot_parse:update_object(<<"Device">>, BatchDeviceId, #{<<"content">> => Content}).

save2td(BatchProductId, BatchAddr, Data) ->
    FlatternData = dgiot_map:flatten(Data),
    NumData = dgiot_product_enum:turn_num(FlatternData, BatchProductId),
    dgiot_task:save_td(BatchProductId, BatchAddr, NumData, #{}).


get_wokrer_id(Name, DevType) ->
    dgiot_parse_id:get_productid(?WORKERCATEGORY, DevType, Name).

init_worker_device(ProductId, WorkerNum, WorkerName) ->
    BinNum = dgiot_utils:to_binary(WorkerNum),
    Devaddr = <<WorkerName/binary,"_",BinNum/binary>>,
    DeviceId = dgiot_parse_id:get_deviceid(ProductId,Devaddr),
    case dgiot_device_cache:lookup(DeviceId) of
        {ok,_} ->
            io:format("~s ~p DeviceId = ~p ~n",[?FILE,?LINE,DeviceId]),
            pass;
        _->
            case dgiot_product:get(ProductId) of
                {ok, Product} ->
                    case Product of
                        #{<<"ACL">> := Acl, <<"name">> := Name, <<"devType">> := DevType, <<"dynamicReg">> := true} ->
%%                            以名字+"_"+工号作为工人设备地址，

                            Device = #{
                                <<"profile">>=>#{<<"worker_flag">> => 1},
                                <<"status">> => <<"ONLINE">>,
                                <<"brand">> => Name,
                                <<"devModel">> => DevType,
                                <<"name">> => Devaddr,
                                <<"devaddr">> => Devaddr,
                                <<"product">> => ProductId,
                                <<"ACL">> => Acl
                            },
                            io:format("~s ~p DeviceId = ~p ~n",[?FILE,?LINE,DeviceId]),
                            dgiot_device:create_device(Device),
                            dgiot_parse:update_object(<<"Device">>,DeviceId , #{<<"ACL">>=> Acl#{<<"*">> =>#{<<"read">> => true}}}),
                            AllData = #{<<"worker_validate">> => true,
                                <<"worker_num">> => WorkerNum,
                                <<"worker_date">> => 0,
                                <<"worker_name">> => WorkerName,
                                <<"product">> => ProductId},
                            NumData = dgiot_product_enum:turn_num(AllData, ProductId),
                            dgiot_data:insert(?WORKER, {ProductId, WorkerNum}, WorkerName),
                            dgiot_task:save_td_no_match(ProductId, Devaddr, NumData, #{});
                        _ ->
                            io:format("~s ~p not_find_product ~n",[?FILE,?LINE]),
                            pass
                    end;
                _ ->
                    error
            end
    end.
