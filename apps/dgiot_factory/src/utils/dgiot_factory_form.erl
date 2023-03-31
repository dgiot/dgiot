%%%-------------------------------------------------------------------
%%% @author wolong
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 3月 2023 11:10
%%%-------------------------------------------------------------------
-module(dgiot_factory_form).
-author("wolong").
-include("dgiot_factory.hrl").
%% API
-export([do_form/8]).

do_form(TaskProductId, TaskDeviceId, BatchProductId, BatchDeviceId, _BatchAddr, PersonType, NewData, ChannelId) ->
    NewPayLoad = run_factory_hook(TaskProductId, TaskDeviceId, BatchProductId, BatchDeviceId, PersonType, NewData, ChannelId),
    dgiot_data:insert(?FACTORY_ORDER, {BatchProductId, BatchDeviceId, PersonType}, NewPayLoad),
    record_device_log(BatchProductId, BatchDeviceId, NewPayLoad),
    OldData = dgiot_factory_utils:get_card_data(BatchProductId, BatchDeviceId),
    ALlData = dgiot_map:merge(OldData, NewPayLoad),
    dgiot_factory_statis:do_statis(TaskProductId, TaskDeviceId, PersonType, NewPayLoad),
    dgiot_parse:update_object(<<"Device">>, BatchDeviceId, #{<<"content">> => ALlData}).


run_factory_hook(TaskProductId, TaskDeviceId, BatchProductId, BatchDeviceId, PersonType, NewData, ChannelId) ->
    case dgiot_hook:run_hook({factory, TaskProductId, PersonType}, [BatchProductId, TaskDeviceId, BatchDeviceId, PersonType, NewData, ChannelId]) of
        {ok, [{ok, Res}]} ->
            Res;
        _ ->
            NewData
    end.

record_device_log(TaskProductId, TaskDeviceId, AllContent) ->
    ACL = case dgiot_device_cache:lookup(TaskDeviceId) of
              {ok, #{<<"acl">> := AclList}} ->
                  lists:foldl(
                      fun(Role, Acc) ->
                          Acc#{Role => #{
                              <<"read">> => true,
                              <<"write">> => false}}
                      end, #{}, AclList);
              _ ->
                  #{<<"*">> => #{<<"read">> => true}}
          end,
    Product = #{
        <<"__type">> => <<"Pointer">>,
        <<"className">> => <<"Product">>,
        <<"objectId">> => TaskProductId},
    Device = #{
        <<"__type">> => <<"Pointer">>,
        <<"className">> => <<"Device">>,
        <<"objectId">> => TaskDeviceId},

    Devaddr = dgiot_utils:to_binary(dgiot_datetime:nowstamp()),
    LogId = dgiot_parse_id:get_devicelogid(TaskDeviceId, Devaddr),
    DeviceLog = #{
        <<"objectId">> => LogId,
        <<"devaddr">> => Devaddr,
        <<"ACL">> => ACL,
        <<"createtime">> => dgiot_utils:to_binary(dgiot_datetime:nowstamp()),
        <<"data">> => AllContent,
        <<"product">> => Product,
        <<"device">> => Device
    },
    dgiot_parse:create_object(<<"Devicelog">>, DeviceLog).
