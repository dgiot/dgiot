%%%-------------------------------------------------------------------
%%% @author wolong
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. 2月 2023 17:12
%%%-------------------------------------------------------------------
-module(dgiot_factory_statis).
-author("wolong").
-include("dgiot_factory.hrl").
%% API
-export([do_statis/4]).
do_statis(TaskProductId, TaskDeviceId, PersonType, Payload) ->
    OldData = get_old_data(TaskProductId, TaskDeviceId),
    DefaultData = get_default_data(PersonType, Payload, OldData),
    NewData = run_statis_hook(TaskProductId, TaskDeviceId, PersonType, Payload, DefaultData),
    dgiot_data:insert(?FACTORYSTATIS, {TaskProductId, TaskDeviceId}, NewData),
    update2parse(TaskDeviceId, NewData),
%%    io:format("~s ~p NewData = ~p. ~n", [?FILE, ?LINE, NewData]),
    dgiot_factory_utils:save2td(TaskDeviceId, NewData).

get_old_data(TaskProductId, TaskDeviceId) ->
    case dgiot_data:lookup(?FACTORYSTATIS, {TaskProductId, TaskDeviceId}) of
        {ok, OldStatis} ->
            OldStatis;
        _ ->
            case dgiot_parse:get_object(<<"Device">>, TaskDeviceId) of
                {ok, #{<<"basedata">> := #{<<"statis">> := Statis, <<"order">> := Order}}} ->
                    #{<<"statis">> => Statis, <<"order">> => Order};
                {ok, #{<<"content">> := #{<<"baseInfo">> := #{<<"Material_List">> := #{<<"Number">> := Number}}}}} ->
                    #{<<"statis">> => #{<<"schedule">> => Number}};
                _ ->
                    #{}
            end

    end.
run_statis_hook(TaskProductId, TaskDeviceId, PersonType, Payload, OldData) ->
    case dgiot_hook:run_hook({factory_statis, TaskProductId, TaskDeviceId, PersonType}, [Payload, OldData]) of
        {ok, [{ok, Res}]} ->
            Res;
        _ ->
            OldData
    end.

update2parse(TaskDeviceId, NewData) ->
    case dgiot_parse:get_object(<<"Device">>, TaskDeviceId) of
        {ok, #{<<"basedata">> := BaseData}} ->
            NewBasedata = dgiot_map:merge(BaseData, NewData),
            dgiot_parse:update_object(<<"Device">>, TaskDeviceId, #{<<"basedata">> => NewBasedata});
        _ ->
            pass
    end.
%%默认操作，对所有工序统计数量和品控检察数
get_default_data(<<"quality">>, #{<<"quality">> := #{<<"type">> := Process, <<"quality">> := <<"合格"/utf8>>}} = Payload, OldData) ->
    CheckNum = maps:get(<<"num">>, maps:get(<<"quality">>, Payload, #{}), 0),
    OldQualified = maps:get(<<"qualitified">>, maps:get(<<"statis">>, OldData, #{}), 0),
    NewQualified = CheckNum + OldQualified,
    OrderInfo = get_order_info(Process, Payload),
    dgiot_map:merge(OldData, #{<<"order">> => OrderInfo, <<"statis">> => #{<<"qualitified">> => NewQualified}});

get_default_data(Process, Payload, OldData) ->
    ProducedNum = maps:get(<<"num">>, maps:get(Process, Payload, #{}), 0),

    OrderInfo = get_order_info(Process, Payload),
    dgiot_map:merge(OldData, #{<<"order">> => OrderInfo, <<"statis">> => #{<<"produced">> => ProducedNum}}).

%%记录生产计划信息，计划号，产成品编码，工序
get_order_info(Process, Payload) ->
    OrderName = maps:get(<<"ordername">>, maps:get(<<"person">>, Payload, #{}), null),
    ProductId = maps:get(<<"fmaterialld">>, maps:get(<<"person">>, Payload, #{}), null),
    #{<<"ordername">> => OrderName, <<"productid">> => ProductId, <<"process">> => Process}.
