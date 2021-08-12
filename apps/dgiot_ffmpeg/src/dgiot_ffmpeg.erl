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
-module(dgiot_ffmpeg).
-author("jonhl").
-include_lib("dgiot/include/logger.hrl").

-export([
    start_live/1,
    start_video/1,
    scan_ipc/1,
    create_ipc_device/1,
    create_scr_device/1,
    get_filter_complex/2,
    get_ipc/1,
    upload/1
]).

start_live(#{
    <<"IPCPWD">> := IPCPWD,
    <<"IPCUSER">> := IPCUSER,
    <<"PARAM">> := PARAM,
    <<"app">> := App,
    <<"interval">> := Interval,
    <<"play">> := PlayUrl,
    <<"push">> := Push,
    <<"pushkey">> := PushKey,
    <<"instruct">> := Instruct,
    <<"ips">> := Ips,
    <<"appdata">> := #{
        <<"reportId">> := ReportId,
        <<"sessionToken">> := SessionToken}}) ->
    lists:foldl(fun({_, Value}, Acc) ->
        lists:foldl(fun({K, _V}, Acc1) ->
%%            ?LOG(info,"K ~p",[K]),
            case binary:split(K, <<$/>>, [global, trim]) of
                [Mac, <<"LIVEVIDEO">>] ->
                    IP = maps:get(Mac, Ips),
                    Acc1 ++ [push_live(Interval, IPCUSER, IPCPWD, IP, PARAM, Mac, Push, PushKey, PlayUrl, ReportId, App, SessionToken)];
                [Mac1, <<"LIVESCREENVIDEO">>] ->
                    Acc1 ++ [push_screenlive(Interval, PARAM, Mac1, Push, PushKey, PlayUrl, ReportId, App, SessionToken)];
                _ -> Acc1
            end
                    end, Acc, maps:to_list(Value))
                end, [], maps:to_list(Instruct));

start_live(Env) ->
    ?LOG(info, "Env ~p", [Env]),
    [].

push_live(Interval, IPCUSER, IPCPWD, IP, PARAM, Mac, Push, PushKey, PlayUrl, ReportId, App, SessionToken) ->
    NewInterval = dgiot_utils:to_list(Interval),
    IPCSRC = " -t " ++ NewInterval
        ++ " -i  \"rtsp://" ++ dgiot_utils:to_list(IPCUSER) ++ ":" ++ dgiot_utils:to_list(IPCPWD) ++ "@" ++
        dgiot_utils:to_list(IP) ++ "\"",
    PushUrl = get_push(Mac, Push, PushKey, dgiot_datetime:now_secs() + dgiot_utils:to_int(Interval) + 600),
    Cmd = "ffmpeg " ++ IPCSRC ++ get_param(PARAM) ++ " " ++ " -f flv " ++ PushUrl,
    ?LOG(info, "Cmd ~p", [Cmd]),
    spawn(fun() -> os:cmd(Cmd) end),
    Data = #{
        <<"url">> => dgiot_utils:to_binary(get_play(Mac, PlayUrl)),
        <<"type">> => <<"liveMonitor">>,
        <<"startAt">> => dgiot_datetime:now_secs(),
        <<"time">> => NewInterval
    },
    dgiot_evidence:post(#{
        <<"id">> => ReportId,
        <<"scene">> => App,
        <<"md5">> => dgiot_license:to_md5(jsx:encode(Data)),
        <<"original">> => #{
            <<"data">> => Data,
            <<"datatype">> => <<"liveMonitor">>,
            <<"sourcetype">> => Mac},
        <<"sessionToken">> => SessionToken}).

push_screenlive(Interval, PARAM, Mac, Push, PushKey, PlayUrl, ReportId, App, SessionToken) ->
    NewInterval = dgiot_utils:to_list(Interval),
    PushUrl = get_push(Mac, Push, PushKey, dgiot_datetime:now_secs() + dgiot_utils:to_int(Interval) + 600),
%%    ?LOG(info,"PushUrl ~p",[PushUrl]),
    Cmd1 = "ffmpeg " ++ " -t " ++ dgiot_utils:to_list(Interval) ++ " -f gdigrab -i desktop " ++ " "
        ++ get_param(PARAM) ++ " " ++ " -f flv " ++ PushUrl,
    ?LOG(info, "Cmd ~p", [Cmd1]),
    spawn(fun() -> os:cmd(Cmd1) end),
    Data = #{
        <<"url">> => dgiot_utils:to_binary(get_play(Mac, PlayUrl)),
        <<"type">> => <<"liveMonitor">>,
        <<"startAt">> => dgiot_datetime:now_secs(),
        <<"time">> => NewInterval
    },
    dgiot_evidence:post(#{
        <<"id">> => ReportId,
        <<"scene">> => App,
        <<"md5">> => dgiot_utils:to_md5(jsx:encode(Data)),
        <<"original">> => #{
            <<"data">> => Data,
            <<"datatype">> => <<"liveMonitor">>,
            <<"sourcetype">> => Mac},
        <<"sessionToken">> => SessionToken}).


start_video(#{
    <<"IPCPWD">> := IPCPWD,
    <<"IPCUSER">> := IPCUSER,
    <<"PARAM">> := PARAM,
    <<"interval">> := Interval,
    <<"instruct">> := Instruct,
    <<"ips">> := Ips
} = Env) ->
    Paths =
        lists:foldl(fun({_, Value}, Acc) ->
            lists:foldl(fun({K, _V}, Acc1) ->
                case binary:split(K, <<$/>>, [global, trim]) of
                    [<<"SCR_", Addr/binary>>, <<"VIDEO">>] ->
                        start_screenrecord(Addr, Interval, PARAM),
                        Acc1 ++ [{get_path(Addr), Addr}];
                    [Mac, <<"VIDEO">>] ->
                        IP = maps:get(Mac, Ips),
                        start_record(Mac, IP, Interval, IPCUSER, IPCPWD, PARAM),
                        Acc1 ++ [{get_path(Mac), Mac}];
                    _ -> Acc1
                end
                        end, Acc, maps:to_list(Value))
                    end, [], maps:to_list(Instruct)),
    erlang:send_after(dgiot_utils:to_int(Interval) * 1000 + 3000, self(), {save_video, Env#{
        <<"begin">> => dgiot_datetime:now_secs(),
        <<"end">> => dgiot_datetime:now_secs() + dgiot_utils:to_int(Interval),
        <<"paths">> => Paths
    }});

start_video(Env) ->
    ?LOG(info, "Env ~p ", [Env]),
    [].

start_record(Mac, IP, Interval, IPCUSER, IPCPWD, PARAM) ->
    NewInterval = dgiot_utils:to_list(Interval),
    IPCSRC = " -t " ++ dgiot_utils:to_list(NewInterval)
        ++ " -i  \"rtsp://" ++ dgiot_utils:to_list(IPCUSER) ++ ":" ++ dgiot_utils:to_list(IPCPWD)
        ++ "@" ++ dgiot_utils:to_list(IP) ++ "\"",
    Cmd = "ffmpeg " ++ IPCSRC ++ " -t " ++ dgiot_utils:to_list(NewInterval) ++ " "
        ++ get_param(PARAM) ++ " -f mp4 " ++ get_path(Mac),
    ?LOG(info, "Cmd ~p", [Cmd]),
    spawn(fun() -> os:cmd(Cmd) end),
    Cmd.


start_screenrecord(Mac, Interval, PARAM) ->
    Cmd = "ffmpeg " ++ " -f gdigrab -i desktop" ++ " -t " ++ dgiot_utils:to_list(Interval)
        ++ " " ++ get_param(PARAM) ++ " -f mp4 " ++ get_path(Mac),
    ?LOG(info, "Cmd ~p", [Cmd]),
    spawn(fun() -> os:cmd(Cmd) end),
    Cmd.

scan_ipc(#{
    <<"IPCMAC">> := <<"ff-ff-ff-ff-ff-ff">>
}) ->
    [];

scan_ipc(#{
    <<"IPCMAC">> := _IPCMAC,
    <<"IPCTYPE">> := _IPCTYPE,
    <<"ACL">> := _Acl}) ->
    ok.
%%    Thing = dgiot_license:load_config(?MODULE, "ffmpeg_thing"),
%%    Topo = dgiot_license:load_config(?MODULE, "ffmpeg_topo"),
%%    windows_capture:update_config(#{
%%        <<"name">> => <<IPCMAC/binary>>,
%%        <<"devType">> => <<"dgiot_ipc">>,
%%        <<"desc">> => <<IPCTYPE/binary, "摄像头"/utf8>>,
%%        <<"netType">> => <<"WIFI">>,
%%        <<"category">> => <<"IotHub">>,
%%        <<"config">> => Topo,
%%        <<"thing">> => Thing,
%%        <<"nodeType">> => 0,
%%        <<"ACL">> => Acl}).


create_scr_device(#{
    <<"ACL">> := Acl,
    <<"gwdevaddr">> := GWAddr,
    <<"gwdevid">> := GwDeviceId
} = Env) ->
    SCREENADDR = <<"SCR_", GWAddr/binary>>,
    {_, #{<<"objectId">> := ProductId}} =
        scan_ipc(Env#{<<"IPCMAC">> => SCREENADDR}),
    {_, #{<<"objectId">> := DeviceId1}} = dgiot_device:create_device(#{
        <<"status">> => <<"ONLINE">>,
        <<"devaddr">> => SCREENADDR,
        <<"name">> => SCREENADDR,
        <<"brand">> => <<"数蛙桌面采集网关"/utf8>>,
        <<"devModel">> => <<"SW_WIN_CAPTURE">>,
        <<"product">> => ProductId,
        <<"ACL">> => Acl,
        <<"route">> => #{GWAddr => SCREENADDR},
        <<"parentId">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"Device">>,
            <<"objectId">> => GwDeviceId
        }
    }),
    create_instruct(Acl, ProductId, DeviceId1, SCREENADDR).

create_ipc_device(#{
    <<"IPCMAC">> := Mac,
    <<"ACL">> := Acl,
    <<"gwdevaddr">> := GWAddr,
    <<"gwdevid">> := GwDeviceId
} = Env) ->

    MacList = binary:split(dgiot_utils:to_binary(Mac), <<$;>>, [global, trim]),
    lists:foldl(fun(NewMac, Acc) ->
        IPCIP = dgiot_utils:get_ipbymac(dgiot_utils:to_binary(NewMac), ping),
        IPCMAC = dgiot_utils:to_binary(re:replace(dgiot_utils:to_list(NewMac), "-", "_",
            [global, {return, list}])),
        case IPCIP of
            <<"">> -> Acc;
            _ ->
                DevAddr = <<"IPC_", IPCMAC/binary>>,
                {_, #{<<"objectId">> := ProductId}} =
                    scan_ipc(Env#{<<"IPCMAC">> => DevAddr}),
                dgiot_product:load(ProductId),
                {_, #{<<"objectId">> := DeviceId}} =
                    dgiot_device:create_device(#{
                        <<"status">> => <<"ONLINE">>,
                        <<"devaddr">> => DevAddr,
                        <<"name">> => DevAddr,
                        <<"ip">> => dgiot_utils:get_natip(),
                        <<"brand">> => <<"数蛙桌面采集网关"/utf8>>,
                        <<"devModel">> => <<"SW_WIN_CAPTURE">>,
                        <<"product">> => ProductId,
                        <<"ACL">> => Acl,
                        <<"route">> => #{GWAddr => DevAddr},
                        <<"parentId">> => #{
                            <<"__type">> => <<"Pointer">>,
                            <<"className">> => <<"Device">>,
                            <<"objectId">> => GwDeviceId
                        }
                    }),
                create_instruct(Acl, ProductId, DeviceId, DevAddr),
                Acc#{DevAddr => IPCIP}
        end
                end, #{}, MacList).

get_ipc(DtuAddr) ->
    lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"ip">> := Ip, <<"devaddr">> := DevAddr,
                <<"product">> := #{<<"devType">> := <<"dgiot_ipc">>}} ->
                Acc#{DevAddr => Ip};
            _ -> Acc
        end
                end, #{}, dgiot_device:get_sub_device(DtuAddr)).

create_instruct(ACL, ProductId, DeviceId, DevAddr) ->
    case dgiot_device:lookup_prod(ProductId) of
        {ok, #{<<"thing">> := Thing}} ->
            #{<<"properties">> := Props} = Thing,
            NewProps =
                lists:foldl(fun(X, Acc) ->
                    case X of
                        #{<<"name">> := <<"点播地址"/utf8>>,
                            <<"dataForm">> := DataForm} ->
                            #{<<"dataForm">> := DataForm} = X,
                            Acc ++ [X#{<<"dataForm">> => DataForm#{<<"address">> => <<DevAddr/binary, "/VIDEO">>}}];
                        _ -> Acc
                    end
                            end, [], Props),
            Pn = <<DevAddr/binary, "/FFMPEG">>,
            Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/", Pn/binary>>,
            dgiot_mqtt:subscribe(Topic),
            dgiot_instruct:create(ProductId, DeviceId, Pn, ACL, <<"all">>, Thing#{
                <<"properties">> => NewProps
            });
        _ -> pass
    end.

get_path(DevAddr) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/video/"]),
    FileName = dgiot_utils:to_list(<<DevAddr/binary, "_">>) ++ dgiot_utils:to_list(dgiot_datetime:now_secs()) ++ ".mp4",
    Root ++ FileName.

get_param(PARAM) ->
    dgiot_utils:to_list(PARAM).

-define(CRLF, "\r\n").
upload(#{
    <<"channelid">> := ChannelId,
    <<"app">> := App,
    <<"paths">> := Paths,
    <<"devaddr">> := DevAddr,
    <<"begin">> := Begin,
    <<"end">> := End,
    <<"appdata">> := Appdata
}) ->
    lists:map(fun({Path, Mac}) ->
        Cmd = "ffprobe -select_streams v -show_streams -v quiet -of csv=\"p=0\" -of json -i \"" ++ Path ++ "\"",
        Bin = unicode:characters_to_binary(os:cmd(Cmd)),
        {Width, Height} =
            case jsx:is_json(Bin) of
                true ->
                    Map = jsx:decode(Bin, [{labels, binary}, return_maps]),
                    case Map of
                        #{<<"streams">> := [#{<<"width">> := Width1, <<"height">> := Height1} | _]} ->
                            {Width1, Height1};
                        _ -> {640, 480}
                    end;
                false -> {640, 480}
            end,
        case dgiot_evidence:upload(Path, App, dgiot_parse_handler:get_token(App)) of
            {ok, #{<<"md5">> := Md5} = Data} ->
                NewData = maps:without([<<"md5">>, <<"retmsg">>, <<"retcode">>, <<"path">>,
                    <<"scene">>, <<"url">>, <<"domain">>], Data),
                Src = maps:get(<<"src">>, NewData),
                #{<<"product">> := Product} = Appdata,
                Ack = #{
                    <<"src">> => Src,
                    <<"width">> => Width,
                    <<"height">> => Height,
                    <<"starttime">> => Begin,
                    <<"endtime">> => End
                },

                dgiot_tdengine_adapter:save(Product, DevAddr, Ack),
                NewTopic = <<"thing/", Product/binary, "/", DevAddr/binary, "/post">>,
                dgiot_bridge:send_log(ChannelId, "to_task: ~p: ~p", [NewTopic, jsx:encode(Ack)]),
%%                dgiot_mqtt:publish(DevAddr, NewTopic, jsx:encode(Ack)),
                case maps:find(<<"reportId">>, Appdata) of
                    {ok, ReportId} ->
                        dgiot_evidence:post(#{
                            <<"id">> => ReportId,
                            <<"scene">> => App,
                            <<"md5">> => Md5,
                            <<"original">> => #{
                                <<"data">> => NewData#{
                                    <<"width">> => Width,
                                    <<"height">> => Height,
                                    <<"mac">> => Mac,
                                    <<"begin">> => Begin,
                                    <<"end">> => End
                                },
                                <<"datatype">> => <<"video">>,
                                <<"sourcetype">> => DevAddr},
                            <<"sessionToken">> => dgiot_parse_handler:get_token(App)});
                    _ -> pass
                end;
            Error -> Error
        end
              end, Paths).

get_play(LiveName, Play) ->
    dgiot_utils:to_list(Play) ++ dgiot_utils:to_list(LiveName) ++ ".flv".

get_push(LiveName, Push, PushKey, Time) ->
    TxTime = string:to_upper(dgiot_utils:to_list(integer_to_binary(Time, 16))),
    TxSecret = dgiot_utils:to_list(PushKey) ++ dgiot_utils:to_list(LiveName) ++ TxTime,
    Md5 = dgiot_utils:to_md5(TxSecret),
    "\"" ++ dgiot_utils:to_list(Push) ++ dgiot_utils:to_list(LiveName) ++ "?txSecret="
        ++ dgiot_utils:to_list(Md5) ++ "&txTime=" ++ TxTime ++ "\"".


get_filter_complex(FILTER, Count) ->
    case FILTER of
        <<"auto">> ->
            case Count of
                2 ->
                    "-filter_complex \"[0:v]pad=iw:ih*2[a];[a][1:v]overlay=0:h\"";
                3 ->
                    "-filter_complex \"[0:v]pad=iw:ih*3[a];[a][1:v]overlay=0:h[b];[b][2:v]overlay=0:2*h\"";
                4 ->
                    "-filter_complex \"[0:v]pad=iw*2:ih*2[a];[a][1:v]overlay=w[b];[b][2:v]overlay=0:h[c];[c][3:v]overlay=w:h\"";
                5 ->
                    "-filter_complex \"[0:v]pad=iw*2:ih*3[a];[a][1:v]overlay=w[b];[b][2:v]overlay=0:h[c];[c][3:v]overlay=w:h[d];[d][4:v]overlay=0:2*h\"";
                6 ->
                    "-filter_complex \"[0:v]pad=iw*2:ih*3[a];[a][1:v]overlay=w[b];[b][2:v]overlay=0:h[c];[c][3:v]overlay=w:h[d];[d][4:v]overlay=0:2*h[e];[e][5:v]overlay=w:2*h\"";
                7 ->
                    "-filter_complex \"[0:v]pad=iw*3:ih*3[a];[a][1:v]overlay=w[b];[b][2:v]overlay=2*w[c];[c][3:v]overlay=0:h[d];[d][4:v]overlay=w:h[e];[e][5:v]overlay=2*w:h[f];[f][6:v]overlay=0:2*h\"";
                8 ->
                    "-filter_complex \"[0:v]pad=iw*3:ih*3[a];[a][1:v]overlay=w[b];[b][2:v]overlay=2*w[c];[c][3:v]overlay=0:h[d];[d][4:v]overlay=w:h[e];[e][5:v]overlay=2*w:h[f];[f][6:v]overlay=0:2*h[g];[g][7:v]overlay=w:2*h\"";
                9 ->
                    "-filter_complex \"[0:v]pad=iw*3:ih*3[a];[a][1:v]overlay=w[b];[b][2:v]overlay=2*w[c];[c][3:v]overlay=0:h[d];[d][4:v]overlay=w:h[e];[e][5:v]overlay=2*w:h[f];[f][6:v]overlay=0:2*h[g];[g][7:v]overlay=w:2*h[h];[h][8:v]overlay=2*w:2*h\"";
                _ -> " "
            end;
        _ -> FILTER
    end.
