%%%-------------------------------------------------------------------
%%% @author jonhl
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 11月 2022 15:06
%%%-------------------------------------------------------------------
-module(dgiot_maintenance).
-author("jonhl").

%% API
-export([get_initdata/2, init_inspection/1, get_inspection/1]).

%% 初始化工单巡检动态数据
init_inspection(#{<<"objectId">> := MaintenanceId, <<"info">> := Info, <<"status">> := 1, <<"product">> := #{<<"objectId">> := ProductId}, <<"device">> := #{<<"objectId">> := DeviceId}} = _QueryData) ->
    InitData = get_initdata(<<"巡检"/utf8>>, ProductId),
    dgiot_parse:update_object(<<"Maintenance">>, MaintenanceId, #{<<"info">> => Info#{<<"dynamicdata">> => InitData}}),
    %%    下发巡检信息
    %%    $dg/device/{productId}/{deviceAddr}/init/response/inspection
    case dgiot_device:lookup(DeviceId) of
        {ok, #{<<"devaddr">> := DevAddr}} ->
            InspectionTopic = <<"$dg/device/", ProductId/binary, "/", DevAddr/binary, "/init/response/inspection">>,
            Data = get_inspection(MaintenanceId),
            dgiot_mqtt:publish(DeviceId, InspectionTopic, dgiot_json:encode(Data));
        _ ->
            pass
    end;

init_inspection(_QueryData) ->
    pass.

get_initdata(Type, ProductId) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := #{<<"properties">> := Props}}} ->
            lists:foldl(fun(#{<<"devicetype">> := Devicetype, <<"name">> := Name,
                <<"identifier">> := Identifier} = X, Acc) ->
                DataType = maps:get(<<"datatype">>, X, #{}),
                Typea = maps:get(<<"type">>, X, <<>>),
                Specs = maps:get(<<"specs">>, DataType, #{}),
                Unit = maps:get(<<"unit">>, Specs, <<"">>),
                Size = size(Type),
                case binary:match(Devicetype, Type) of
                    {0, Size} ->
                        Acc ++ [#{
                            <<"devicetype">> => Devicetype,
                            <<"identifier">> => Identifier, <<"name">> => Name,
                            <<"type">> => Typea, <<"number">> => <<>>,
                            <<"unit">> => Unit}];
                    _O ->
                        Acc
                end
                        end, [], Props);
        _ ->
            []
    end.

get_inspection(MaintenanceId) ->
    case dgiot_parse:get_object(<<"Maintenance">>, MaintenanceId) of
        {ok, #{<<"number">> := Number, <<"status">> := Status, <<"info">> := Info}} ->
            Dynamicdata = maps:get(<<"dynamicdata">>, Info, []),
            Basic = #{
                <<"number">> => Number,
                <<"status">> => Status,
                <<"productname">> => maps:get(<<"productname">>, Info, <<>>),
                <<"devicename">> => maps:get(<<"devicename">>, Info, <<>>),
                <<"executorname">> => maps:get(<<"executorname">>, Info, <<>>),
                <<"startdata">> => maps:get(<<"startdata">>, Info, <<>>),
                <<"completiondata">> => maps:get(<<"completiondata">>, Info, <<>>)
            },
            #{<<"basic">> => Basic, <<"time">> => dgiot_datetime:now_secs(), <<"data">> => Dynamicdata};
        _ ->
            #{}
    end.
