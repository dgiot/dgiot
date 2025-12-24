%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

%% @doc dgiot_sophon Protocol
-module(dgiot_sophon).
-include("dgiot_sophon.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    send_push/3,
    send_push/4,
    send_slow/3,
    save_notification/2,
    send_msg/1,
    get_email/1
]).

-define(APP, ?MODULE).
%%  ffmpeg -rtsp_transport tcp -i "rtsp://{{username}}:{{password}}@{{ip}}:554{{channel}}" -c:v copy -c:a copy -f rtsp rtsp://127.0.0.1:8554/quick_stream
%% ffmpeg -rtsp_transport tcp -i {{url}} -c:v copy -c:a copy -f rtsp rtsp://127.0.0.1:8554/quick_stream
%% ffmpeg -rtsp_transport tcp -i rtsp://admin:shuwafly2020@192.168.1.9 -c:v copy -c:a copy -f rtsp rtsp://127.0.0.1:8554/quick_stream
send_push(ChannelId, TaskQue, Cmd, polling) ->
    lists:foldl(fun
                    (#{<<"objectId">> := DeviceId, <<"devaddr">> := Devaddr, <<"profile">> := #{<<"camera_type">> := <<"hikvision">>} = Profile} = Device, Index) ->
                        Args = Profile#{
                            <<"body">> => [
                                #{<<"key">> => <<"cameraIndexCode">>, <<"value">> => Devaddr},
                                #{<<"key">> => <<"streamType">>, <<"value">> => 1},
                                #{<<"key">> => <<"protocol">>, <<"value">> => <<"rtsp">>},
                                #{<<"key">> => <<"transmode">>, <<"value">> => 1},
                                #{<<"key">> => <<"expand">>, <<"value">> => <<"transcode=0">>}
                            ]
                        },
                        case dgiot_hikvision:get_previewURLs(Args) of
                            {ok, #{<<"url">> := Url}} when size(Url) > 0 ->
                                dgiot_rtsp2ws:stop(ChannelId, DeviceId),
                                BinIndex = dgiot_utils:to_binary(Index),
                                PushCmd = dgiot_map:map(#{<<"url">> => Url}, <<Cmd/binary, BinIndex/binary>>),
                                proc_lib:spawn(fun() ->
                                    dgiot_rtsp2ws:exec(PushCmd, ChannelId, DeviceId) end),
                                io:format("~s ~p Pid ~p PushCmd ~p  ~n", [?FILE, ?LINE, self(), PushCmd]),
                                dgiot_device:online(DeviceId),
                                dgiot_data:insert({rtsp_device, <<"polling", BinIndex/binary>>}, Device),
                                Index + 1;
                            _ ->
                                pass
                        end;
                    (#{<<"objectId">> := DeviceId, <<"profile">> := Profile} = Device, Index) ->
                        dgiot_rtsp2ws:stop(ChannelId, DeviceId),
                        Channel = maps:get(<<"channel">>, Profile, <<"">>),
                        BinIndex = dgiot_utils:to_binary(Index),
                        PushCmd = dgiot_map:map(Profile#{<<"device">> => DeviceId, <<"channel">> => Channel}, <<Cmd/binary, BinIndex/binary>>),
                        proc_lib:spawn(fun() ->
                            dgiot_rtsp2ws:exec(PushCmd, ChannelId, DeviceId) end),
                        io:format("~s ~p Pid ~p PushCmd ~p  ~n", [?FILE, ?LINE, self(), PushCmd]),
                        dgiot_device:online(DeviceId),
                        dgiot_data:insert({rtsp_device, <<"polling", BinIndex/binary>>}, Device),
                        Index + 1
                end, 1, TaskQue).

send_push(ChannelId, TaskQue, Cmd) ->
    lists:foldl(fun
                    (#{<<"objectId">> := DeviceId, <<"devaddr">> := Devaddr, <<"profile">> := #{<<"camera_type">> := <<"hikvision">>, <<"streamname">> := Streamname} = Profile} = Device, Index) ->
                        Args = Profile#{
                            <<"body">> => [
                                #{<<"key">> => <<"cameraIndexCode">>, <<"value">> => Devaddr},
                                #{<<"key">> => <<"streamType">>, <<"value">> => 1},
                                #{<<"key">> => <<"protocol">>, <<"value">> => <<"rtsp">>},
                                #{<<"key">> => <<"transmode">>, <<"value">> => 1},
                                #{<<"key">> => <<"expand">>, <<"value">> => <<"transcode=0">>}
                            ]
                        },
                        case dgiot_hikvision:get_previewURLs(Args) of
                            {ok, #{<<"url">> := Url}} when size(Url) > 0 ->
                                dgiot_rtsp2ws:stop(ChannelId, DeviceId),
                                PushCmd = dgiot_map:map(#{<<"url">> => Url}, <<Cmd/binary, Streamname/binary>>),
                                proc_lib:spawn(fun() ->
                                    dgiot_rtsp2ws:exec(PushCmd, ChannelId, DeviceId) end),
                                io:format("~s ~p Pid ~p PushCmd ~p  ~n", [?FILE, ?LINE, self(), PushCmd]),
                                dgiot_device:online(DeviceId),
                                dgiot_data:insert({rtsp_device, Streamname}, Device),
                                Index + 1;
                            _ ->
                                pass
                        end;
                    (#{<<"objectId">> := DeviceId, <<"profile">> := Profile} = Device, Index) ->
                        dgiot_rtsp2ws:stop(ChannelId, DeviceId),
                        Channel = maps:get(<<"channel">>, Profile, <<"">>),
                        BinIndex = dgiot_utils:to_binary(Index),
                        PushCmd = dgiot_map:map(Profile#{<<"device">> => DeviceId, <<"channel">> => Channel}, <<Cmd/binary, BinIndex/binary>>),
                        proc_lib:spawn(fun() ->
                            dgiot_rtsp2ws:exec(PushCmd, ChannelId, DeviceId) end),
                        io:format("~s ~p Pid ~p PushCmd ~p  ~n", [?FILE, ?LINE, self(), PushCmd]),
                        dgiot_device:online(DeviceId),
                        dgiot_data:insert({rtsp_device, <<"quick_stream", BinIndex/binary>>}, Device),
                        Index + 1
                end, 1, TaskQue).

send_slow(ChannelId, DeviceId, Cmd) ->
    case dgiot_parse:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"profile">> := Profile} = Device} ->
            dgiot_rtsp2ws:stop(ChannelId, DeviceId),
            Channel = maps:get(<<"channel">>, Profile, <<"">>),
            PushCmd = dgiot_map:map(Profile#{<<"device">> => DeviceId, <<"channel">> => Channel}, <<Cmd/binary, "_slow">>),
            proc_lib:spawn(fun() -> dgiot_rtsp2ws:exec(PushCmd, ChannelId, DeviceId) end),
            io:format("~s ~p Pid ~p PushCmd ~p  ~n", [?FILE, ?LINE, self(), PushCmd]),
            dgiot_device:online(DeviceId),
            dgiot_data:insert({rtsp_device, <<"quick_stream_slow">>}, Device);
        _ ->
            pass
    end.

save_notification(#{<<"objectId">> := DeviceId, <<"product">> := #{<<"objectId">> := ProductId}, <<"ACL">> := Acl}, Content) ->
    Data = #{
        <<"ACL">> => Acl,
        <<"content">> => Content,
        <<"public">> => false,
        <<"status">> => 0,
        <<"process">> => <<"">>,
        <<"type">> => <<"ai_recognition_push">>,
        <<"device">> => #{<<"__type">> => <<"Pointer">>, <<"className">> => <<"Device">>, <<"objectId">> => DeviceId}
    },
    dgiot_parse:create_object(<<"Notification">>, Data),
    ViewId = dgiot_parse_id:get_viewid(ProductId, <<"notification">>, <<"Product">>, <<"send_msg">>),
    Content#{<<"_deviceid">> => DeviceId, <<"_productid">> := ProductId, <<"_viewid">> := ViewId, <<"dgiot_alarmvalue">> => maps:get(<<"img_url">>, Content, <<>>), <<"description">> => maps:get(<<"warning_info">>, Content, <<>>)};

save_notification(_, _) ->
    pass.


send_msg(#{<<"_deviceid">> := DeviceId, <<"warning_info">> := Warning_info}) ->
    Emails = dgiot_notification:get_Emails(DeviceId, <<>>, <<>>),
    Data = #{
        <<"to">> => Emails,
        <<"todes">> => <<"aaa">>,
        <<"subject">> => Warning_info,
        <<"data">> => get_email(<<"email.html">>)
    },
    dgiot_notification:send_email(Data).

get_email(FileName) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Path = dgiot_httpc:url_join([Dir, "/priv/html/", dgiot_utils:to_list(FileName)]),
    case catch file:read_file(Path) of
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error, "read  Path,~p error,~p ~n", [Path, Reason]),
%%            针对获取不到的文件做处理
            <<"">>;
        {ok, Bin} ->
            Bin
    end.



