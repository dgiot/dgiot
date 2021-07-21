%%%-------------------------------------------------------------------
%%% @author h7ml
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 7月 2021 10:23
%%%-------------------------------------------------------------------
-module(device2).
-author("h7ml").

%% API
-export([updata_data/0]).
-include_lib("dgiot/include/logger.hrl").

%%% @description 设备数据字段迁移
updata_data() ->
    Info = fun(Page) ->
        case Page of
            [#{<<"basedata">> := Basedata, <<"objectId">> := ObjectId, <<"devaddr">> := Devaddr}] ->
                case Basedata of
                    #{<<"basicdata">> := BasisData} ->
                        PowerOnCtrl = maps:get(<<"PowerOnCtrl">>, BasisData, 0),
                        PubCtrl = maps:get(<<"PubCtrl">>, BasisData, 1),
                        PubFreq = maps:get(<<"PubFreq">>, BasisData, 20),
                        ParaGet = maps:get(<<"ParaGet">>, BasisData, 1),
                        FOTA = maps:get(<<"FOTA">>, BasisData, 0),
                        AgreementRelease = maps:get(<<"AgreementRelease">>, BasisData, 0),
                        PowerOffDelay = maps:get(<<"PowerOffDelay">>, BasisData, 48),
                        ExpirationTime = maps:get(<<"expirationTime">>, Basedata, 0),
                        CtrlSerialNo = maps:get(<<"CtrlSerialNo">>, BasisData, 0),
                        MDSerialNo = maps:get(<<"MDSerialNo">>, BasisData, 0),
                        CtrSoftVersion = maps:get(<<"CtrSoftVersion">>, BasisData, 0),
                        ProtocolVersion = maps:get(<<"ProtocolVersion">>, BasisData, 0),
                        PartAddr = maps:get(<<"partAddr">>, BasisData, 0),
                        MDSoftVersion = maps:get(<<"MDSoftVersion">>, BasisData, 0),
                        SIMSerialNo = maps:get(<<"SIMSerialNo">>, BasisData, 0),
                        dgiot_parse:update_object(<<"Device">>, ObjectId, #{<<"profile">> => #{
                            <<"PowerOnCtrl">> => PowerOnCtrl,
                            <<"PubCtrl">> => PubCtrl,
                            <<"PubFreq">> => PubFreq,
                            <<"ParaGet">> => ParaGet,
                            <<"FOTA">> => FOTA,
                            <<"AgreementRelease">> => AgreementRelease,
                            <<"PowerOffDelay">> => PowerOffDelay
                        }, <<"form">> => #{
                            <<"Devaddr">> => Devaddr,
                            <<"ExpirationTime">> => ExpirationTime,
                            <<"CtrlSerialNo">> => CtrlSerialNo,
                            <<"MDSerialNo">> => MDSerialNo,
                            <<"CtrSoftVersion">> => CtrSoftVersion,
                            <<"ProtocolVersion">> => ProtocolVersion,
                            <<"PartAddr">> => PartAddr,
                            <<"MDSoftVersion">> => MDSoftVersion,
                            <<"SIMSerialNo">> => SIMSerialNo
                        }});
                    _ ->
                        pass
                end;
            R -> io:format("~p ~n", [R])
        end
                end,

    Query = #{<<"where">> => #{}},
    dgiot_parse_loader:start(<<"Device">>, Query, 0, 1, 2000, Info),
    ok.
