%%%-------------------------------------------------------------------
%%% @author stoneliu
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% solarPower
%%% @end
%%% Created : 07. 五月 2021 12:00
%%%-------------------------------------------------------------------
-module(dgiot_xinchuangwei).
-author("stoneliu").
-export([create_product/1, create_device/4, test/1]).
-include_lib("dgiot/include/logger.hrl").

%% 16#c3
%% Buff = <<16#AA, 16#12, 16#00, 16#03, 16#02, 16#02, 16#, 16#01>>.
%% dgiot_xinchuangwei:test(Buff).
test(Buff) ->
    CheckCrc = dgiot_xinchuangwei_decoder:crc16(Buff),
    Payload = <<Buff/binary, CheckCrc:8>>,
    dgiot_xinchuangwei_decoder:parse_frame(Payload, #{<<"dtuproduct">> => <<"3a8490e534">>, <<"devaddr">> => <<"xinchuangweitest">>}).

create_product(#{
    <<"DTUTYPE">> := DTUTYPE,
    <<"ACL">> := Acl,
    <<"productname">> := Productname}) ->
    Thing = dgiot_license:load_config(?MODULE, "solar_thing"),
    Topo = dgiot_license:load_config(?MODULE, "solar_topo"),
    case dgiot_parse:query_object(<<"Product">>, #{<<"where">> => #{<<"name">> => Productname}}) of
        {ok, #{<<"results">> := [#{<<"objectId">> := ObjectId} | _]}} ->
            {ok, #{<<"objectId">> => ObjectId}};
        _ ->
            R = dgiot_parse:create_object(<<"Product">>, #{<<"name">> => Productname,
                <<"devType">> => <<"dgiot_", DTUTYPE/binary, "_dtu">>,
                <<"desc">> => Productname,
                <<"netType">> => <<"4G">>,
                <<"category">> => <<"IotHub">>,
                <<"config">> => Topo,
                <<"thing">> => Thing,
                <<"nodeType">> => 1,
                <<"ACL">> => Acl,
                <<"productSecret">> => license_loader:random(),
                <<"dynamicReg">> => true}),
            R
    end.

create_device(DeviceId, ProductId, DTUMAC, DTUIP) ->
    case dgiot_parse:get_object(<<"Product">>, ProductId) of
        {ok, #{<<"ACL">> := Acl, <<"devType">> := DevType, <<"name">> := ProductName}} ->
            case dgiot_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"devaddr">> := _GWAddr}} ->
                    dgiot_parse:update_object(<<"Device">>, DeviceId, #{<<"ip">> => DTUIP, <<"status">> => <<"ONLINE">>}),
                    dgiot_task:save_pnque(ProductId, DTUMAC, ProductId, DTUMAC);
                _ ->
                    dgiot_device:create_device(#{
                        <<"devaddr">> => DTUMAC,
                        <<"name">> => <<ProductName/binary, DTUMAC/binary>>,
                        <<"ip">> => DTUIP,
                        <<"isEnable">> => true,
                        <<"product">> => ProductId,
                        <<"ACL">> => Acl,
                        <<"status">> => <<"ONLINE">>,
                        <<"location">> => #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => 120.161324, <<"latitude">> => 30.262441},
                        <<"brand">> => <<ProductName/binary>>,
                        <<"devModel">> => DevType,
                        <<"basedata">> => #{}
                    }),
                    dgiot_task:save_pnque(ProductId, DTUMAC, ProductId, DTUMAC)
            end;
        Error2 -> ?LOG(info, "Error2 ~p ", [Error2])
    end.







