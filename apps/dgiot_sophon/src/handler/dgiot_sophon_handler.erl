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
-module(dgiot_sophon_handler).
-author("johnliu").
-behavior(dgiot_rest).
-include_lib("dgiot/include/logger.hrl").
-dgiot_rest(all).


%% API
-export([swagger_sophon/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/sophon>>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_sophon.json">>, ?MODULE, [], priv)
swagger_sophon() ->
    [
        dgiot_http_server:bind(<<"/swagger_sophon.json">>, ?MODULE, [], priv)
    ].


%%%===================================================================
%%% 请求处理
%%%  如果登录, Context 内有 <<"user">>, version
%%%===================================================================

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: dgiot_req:req()) ->
    {Status :: dgiot_req:http_status(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map(), Req :: dgiot_req:req()}.

handle(OperationID, Args, Context, Req) ->
    Headers = #{},
    case catch do_request(OperationID, Args, Context, Req) of
        {ErrType, Reason} when ErrType == 'EXIT'; ErrType == error ->
%%            ?LOG(info, "do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> dgiot_utils:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
%%            ?LOG(info, "do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
%%            ?LOG(info, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
%%            ?LOG(info, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
%%            ?LOG(info, "do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

%% System 概要: demo测试接口 描述:demo测试接口
%% OperationId:post_demotest
%% 请求:POST /iotapi/post_demotest
%%#{<<"camera_code">> => <<"C810394">>,
%%<<"camera_name">> => <<"50">>,
%%<<"created_time">> => <<"2024-03-26 11:26:58">>,
%%<<"img_url">> =>
%%<<"http://192.168.1.3:8088/image/20240326/A00007/SAAI20240326112700c78735b64f.jpg">>,
%%<<"position">> => <<"50">>,
%%<<"task_code">> => <<"T081954">>,
%%<<"task_name">> => <<232,144,164,231,159,179>>,
%%<<"video_url">> => <<>>,
%%<<"warning_code">> =>
%%<<"SAAI20240326112700c78735b64f">>,
%%<<"warning_info">> =>
%%<<231,129,171,231,132,176,230,163,128,230,181,
%%139,229,145,138,232,173,166>>,
%%<<"warning_type">> => <<"A00007">>}.

%% AI检测火灾推送
do_request(post_sophon, #{<<"camera_name">> := Camera_name} = Args, _Context, _Req) ->
%%    io:format("~s ~p Args = ~p.~n", [?FILE, ?LINE, Args]),
    case dgiot_data:get({rtsp_device, Camera_name}) of
        #{<<"objectId">> := _} = Device ->
            Content = dgiot_sophon:save_notification(Device, Args),
            dgiot_umeng:send_dashboard(Content),
            dgiot_sophon:send_msg(Content);
        _ ->
            pass
    end,
    {ok, #{<<"status">> => 0, <<"msg">> => <<"success">>}};

do_request(post_push_linaro, #{<<"deviceId">> := DeviceId} = Args, _Context, _Req) ->
%%    io:format("~s ~p Args = ~p.~n", [?FILE, ?LINE, Args]),
    spawn(fun() ->
        send_push(Args)
          end),
    dgiot_parsex:update_object(<<"Device">>, DeviceId, #{<<"profile">> => Args}),
    {ok, #{<<"status">> => 0, <<"msg">> => <<"success">>}};

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    io:format("~s ~p OperationId = ~p.~n", [?FILE, ?LINE, _OperationId]),
    io:format("~s ~p Args = ~p.~n", [?FILE, ?LINE, _Args]),
    {error, <<"Not Allowed.">>}.

send_push(#{<<"control">> := <<"start">>, <<"deviceId">> := DeviceId, <<"streamname">> := Streamname} = Profile) ->
    case dgiot_parsex:get_object(<<"Device">>, DeviceId) of
        {ok, #{<<"devaddr">> := Devaddr} = Device} ->
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
                    io:format("~s ~p Url [~p] size => ~p ~n", [?FILE, ?LINE, Url, size(Url)]),
                    dgiot_rtsp2ws:stop(DeviceId, Streamname),
                    PushCmd = <<"ffmpeg -rtsp_transport tcp -i  ", Url/binary, " -c:v copy -c:a copy -f rtsp rtsp://127.0.0.1:8554/", Streamname/binary>>,
                    proc_lib:spawn(fun() ->
%%                dgiot_rtsp2ws:exec(<<"nohup ", PushCmd/binary, " > /dev/null 2>&1 &">>, DeviceId, Streamname) end),
                        dgiot_rtsp2ws:exec(PushCmd, DeviceId, Streamname) end),
                    io:format("~s ~p Pid ~p PushCmd ~p  ~n", [?FILE, ?LINE, self(), PushCmd]),
                    dgiot_data:insert({rtsp_device, Streamname}, Device);
                _ ->
                    pass
            end;
        _ ->
            []
    end;

send_push(#{<<"deviceId">> := DeviceId, <<"streamname">> := Streamname} = _Args) ->
    dgiot_rtsp2ws:stop(DeviceId, Streamname),
    ok.
