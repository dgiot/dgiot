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
-module(dgiot_bacnet_decoder).
-author("johnliu").
-include_lib("dgiot_bacnet.hrl").
-include_lib("dgiot/include/logger.hrl").
-protocol([?BACNET]).

%% API
-export([parse_frame/2, to_frame/1, test/0]).

%% 注册协议参数
-params(#{
}).

%% 注册协议类型
-protocol_type(#{
    cType => ?BACNET,
    type => <<"bacnet">>,
    colum => 10,
    title => #{
        zh => <<"楼宇自控协议"/utf8>>
    },
    description => #{
        zh => <<"楼宇自控协议"/utf8>>
    }
}).

test() ->
    io:format("NewResult ~p~n", [<<"NewResult">>]).

parse_frame(Buff, Opts) ->
    parse_frame(Buff, #{}, Opts).

parse_frame(<<>>, Acc, _Opts) ->
    {ok, Acc};

parse_frame(<<16#21, 16#31, PacketLen:16, Unknown:32, DeviceType:16, Serial:16, Time:32, Checksum:16/binary, Data/binary>> = Buff,
        Acc, Opts) when PacketLen > 32 ->
    Len = size(Buff),
    {Acc1, Rest1} =
        case Len of
            PacketLen ->
                NewAcc = Acc#{
                    <<"msgtype">> => ?BACNET,
                    <<"unknow">> => Unknown,
                    <<"devicetype">> => DeviceType,
                    <<"serial">> => Serial,
                    <<"time">> => Time,
                    <<"checksum">> => dgiot_utils:binary_to_hex(Checksum),
                    <<"data">> => Data
                },
                {NewAcc, <<>>};
            false ->
                {Acc, <<>>}
        end,
    parse_frame(Rest1, Acc1, Opts);

parse_frame(<<_:8, Data/binary>> = _Rest, Acc, Opts) ->
    parse_frame(Data, Acc, Opts).


to_frame(Frame) ->
    to_frame_last(Frame).

to_frame_last(#{
    <<"msgtype">> := ?BACNET
} = _Frame) ->
    <<>>;

to_frame_last(_Frame) ->
    io:format("~s ~p Frame ~p ~n", [?FILE, ?LINE, _Frame]),
    <<>>.
