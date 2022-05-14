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

-module(dgiot_product_file).
-author("kenneth").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(CRLF, "\r\n").

-export([get_file/4, encode/1, decode/3]).


get_file(ProductId, DevAddr, FileUrl, Ext) ->
    Name = dgiot_datetime:now_microsecs(),
    FileName = dgiot_utils:to_list(Name) ++ "." ++ dgiot_utils:to_list(Ext),
    BinFileName = dgiot_utils:to_binary(FileName),
    case ibrowse:send_req(dgiot_utils:to_list(FileUrl), [], get) of
        {ok, "200", _Header1, Stream} ->
            DeviceId = dgiot_parse_id:get_deviceid(ProductId, DevAddr),
            AppName = dgiot_device:get_appname(DeviceId),
            SessionToken = dgiot_parse_auth:get_token(AppName),
            inets:start(),
            Url = dgiot_device:get_url(AppName),

            NewUrl = <<Url/binary, "/upload">>,
            Boundary = <<"-----------------------acebdf135724681">>,
            Header = <<"--", Boundary/binary, ?CRLF, "Content-Disposition: form-data;name=\"">>,

            Data1 = <<"output\"", ?CRLF, ?CRLF, "json", ?CRLF>>,
            ParamBody1 = <<Header/binary, Data1/binary>>,

            Data2 = <<"scene\"", ?CRLF, ?CRLF, AppName/binary, ?CRLF>>,
            ParamBody2 = <<Header/binary, Data2/binary>>,

            Data3 = <<"path\"", ?CRLF, ?CRLF, "dgiot_file/", DeviceId/binary, ?CRLF>>,
            ParamBody3 = <<Header/binary, Data3/binary>>,

            Data4 = <<"auth_token\"", ?CRLF, ?CRLF, SessionToken/binary, ?CRLF>>,
            ParamBody4 = <<Header/binary, Data4/binary>>,

            Data5 = <<"filename\"", ?CRLF, ?CRLF, BinFileName/binary, ?CRLF>>,
            ParamBody5 = <<Header/binary, Data5/binary>>,

            Tail = <<"--", Boundary/binary, "--", ?CRLF, ?CRLF>>,

            Binstream = dgiot_utils:to_binary(Stream),

            FileBody = <<Header/binary, "file\"; filename=\"", BinFileName/binary, "\"", ?CRLF,
                "Content-Type: application/octet-stream", ?CRLF, ?CRLF, Binstream/binary, ?CRLF, Tail/binary>>,

            ParamBody = <<ParamBody1/binary, ParamBody2/binary, ParamBody3/binary, ParamBody4/binary, ParamBody5/binary>>,

            Body = <<ParamBody/binary, FileBody/binary>>,
            Size = byte_size(Body),
            ContentType = <<"multipart/form-data; boundary=", Boundary/binary>>,
            case httpc:request(post, {dgiot_utils:to_list(NewUrl), [{"Content-Length", integer_to_list(Size)}], binary_to_list(ContentType), Body}, [], []) of
                {ok, {{"HTTP/1.1", 200, "OK"}, _, Json}} ->
                    case jsx:decode(dgiot_utils:to_binary(Json), [{labels, binary}, return_maps]) of
                        #{<<"md5">> := _Md5} ->
                            {ok, Name};
                        Error1 ->
                            Error1
                    end;
                Error ->
                    Error
            end;
        Error2 ->
            Error2
    end.

encode(Frame) when is_map(Frame) ->
    case maps:get(<<"data">>, Frame, no) of
        no ->
            dgiot_protocol:encode(Frame);
        Data ->
            HxData = dgiot_utils:binary_to_hex(Data),
            case re:run(HxData, <<"0000180080E1">>) of
                nomatch ->
                    dgiot_protocol:encode(Frame);
                _ ->
                    {ignore, Frame}
            end
    end;
encode(Data) when is_binary(Data) ->
    {ok, Data}.

decode([], _, _State) ->
    {error, not_decode};
decode([MsgType | Other], Bin, State) ->
    case dgiot_protocol:decode(MsgType, Bin, State) of
        {ok, Rest, Messages} ->
            {ok, Rest, Messages};
        {error, _} ->
            %?LOG(error,"decode:~p, not this protocol:~p", [Bin, MsgType]),
            decode(Other, Bin, State)
    end.
