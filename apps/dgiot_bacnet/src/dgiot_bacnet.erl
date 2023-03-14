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
-module(dgiot_bacnet).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").

-export([
    bacnetcallback/1,
    whois/2
]).

bacnetcallback(Data) ->
%%    io:format("~s ~p Data = ~p.~n", [?FILE, ?LINE, Data]),
    lists:foldl(fun(X, Acc) ->
        case X of
            #{<<"device">> := Device, <<"properties">> := Properties} ->
                HostAddress = maps:get(<<"hostAddress">>, Device, <<"">>),
                Address = maps:get(<<"address">>, Device, HostAddress),
                InstanceNumber = maps:get(<<"instanceNumber">>, Device, <<"">>),
                Name = maps:get(<<"name">>, Device, <<Address/binary, "_", InstanceNumber/binary>>),
                dgiot_device:create_device(#{
                    <<"status">> => <<"ONLINE">>,
                    <<"name">> => Name,
                    <<"devaddr">> => <<Address/binary, "_", InstanceNumber/binary>>,
                    <<"ip">> => HostAddress,
                    <<"brand">> => <<"Bacnet">>,
                    <<"devModel">> => <<"DGIOT_GROUP">>,
                    <<"product">> => <<"ProductId">>,
                    <<"basedata">> => X,
                    <<"address">> => Address,
                    <<"ACL">> => #{<<"role:admin">> => #{<<"read">> => true, <<"write">> => true}}}),
%%                _Thing =
                    lists:foldl(fun(Prop, Acc1) ->
                        Id = maps:get(<<"id">>, Prop, HostAddress),
                        Value = maps:get(<<"value">>, Prop, HostAddress),
                        Acc1#{Id => Value}
                                end, #{}, Properties);
            _ ->
                Acc
        end
                end, [], Data).


whois(ChannelId, ClientId) ->
    % 构建BACnet Who-Is消息
    WhoIs = [
        <<16#01>>, % BACnet协议版本
        <<16#00, 16#06>>, % 目标网络/长度
        <<16#FF>>, % 目标MAC地址
        <<16#00, 16#00>>, % 目标对象类型和实例号
        <<16#10>> % 最大APDU长度
    ],

    % 将Who-Is消息发送到BACnet广播地址
    dgiot_udp_broadcast:send(ChannelId, ClientId, dgiot_utils:to_binary(WhoIs)).
