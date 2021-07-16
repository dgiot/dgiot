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

-module(dgiot_group).
-include("dgiot_group.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([
    start_group/1,
    start_schedule/1,
    test_start_schedule/0,
    test_zeta/1,
    test_spawn/0
]).

%% 启动虚拟集中器
start_group(#{<<"vcaddr">> := VcAddr, <<"meters">> := Meters}) ->
    Pns = dgiot_utils:binary_bits_zip(start_meters(VcAddr, Meters)),
    dgiot_data:insert(?dgiot_GROUP, dgiot_utils:hex_to_binary(VcAddr), Pns).

start_meters(VcAddr, Meters) ->
    lists:foldl(
        fun
            (#{<<"pn">> := Pn, <<"channel">> := Channel, <<"addr">> := DevAddr}, Pns) ->
                Meter = #meter{
                    addr = dgiot_utils:hex_to_binary(DevAddr),
                    channel = maps:fold(
                        fun
                            (<<"chs">>, _Val, Acc) ->
                                Acc;
                            (Key, Val, Acc) ->
                                Acc#{binary_to_integer(Key) => Val}
                        end, #{}, Channel)
                },
                dgiot_data:insert(?dgiot_GROUP_METER, {dgiot_utils:hex_to_binary(VcAddr), Pn}, Meter),
                dgiot_utils:set_binary_bits_n(Pns, Pn, 1);
            (Meter, Pns) ->
                ?LOG(error,"arg is error, ~p", [Meter]),
                Pns
        end, <<0:2048>>, Meters).

start_schedule(#{<<"mid">> := _Mid,
    <<"tid">> := _Tid,
    <<"di">> := _Di,
    <<"mod">> := _Mod,
    <<"fun">> := _Fun,
    <<"que">> := _Que,
    <<"quelen">> := _QueLen,
    <<"tags">> := _Tags,
    <<"delay">> := _Delay,
    <<"retry">> := _Rerty,
    <<"times">> := _Times,
    <<"begin">> := _Begin} = Args) ->
    supervisor:start_child(group_task, [Args]).

test_zeta(#{<<"que">> := _Que, <<"offset">> := _Offset, <<"tags">> := _Tags}) ->
    dgiot_data:get_consumer(<<"task/zeta/send">>,1).
%%     ?LOG(info,"Que ~p ,offset ~p ,Tags ~p ",[Que,Offset,Tags]).

test_start_schedule() ->
    %tags
    %start <<"FF 00 00 00">>
    %end   <<"FF C8 FF FF">>
    Threshold = list_to_integer("C8FF", 16), % mid from <<"00 00">>  to <<"C8 FF">> 对应现实中的货车编号
    Tags = 16#FF, %% 对应货车里面的Tag编号，符合上面的编码规则
    Di = <<"tags">>, %标识这个Di是给Tag心跳专用
    Tid = 16#FF0E, %zeta专用 tid
    Mod = group_loader,
    Fun = test_zeta,
    dgiot_data:set_consumer(<<"task/zeta/route">>,250), %% 发车线路消费组
    QueLen = 100, %% 每个线路里面的AP数
    Delay =  15, %发车间隔 单位毫秒，15分钟可以发6w辆车
    Retry = 15 * 60 * 1000, %tags心跳间隔 AP 实际发包时，可以加一点随机数，把心跳间隔散开一点
    Times = 3600,  %单车上tags发包间隔 单位毫秒
    Begin = 150, % 下发任务后开始时间间隔 单位毫秒
    dgiot_data:set_consumer(<<"task/zeta/send">>,15000000), %% 发车线路消费组
    lists:foreach(
        fun(I) ->
            Args =  #{ <<"tid">> => Tid,
                <<"mid">> => I,
                <<"di">> => Di,
                <<"mod">> => Mod,
                <<"fun">> => Fun,
                <<"que">> => dgiot_data:get_consumer(<<"task/zeta/route">>,1),
                <<"quelen">> => QueLen,
                <<"tags">> => Tags,
                <<"delay">> => Delay,
                <<"retry">> => Retry,
                <<"times">> => Times,
                <<"begin">> => Begin
            },
            group_loader:start_schedule(Args)
        end, lists:seq(1, Threshold)).

test_spawn() ->
    Table = <<"dddd">>,
    Pid = erlang:spawn(fun()->
        io:format("~p pid ~p ~n",[Table,self()])
                       end),
    receive
        _ ->
            io:format("11 ~p Pid ~p ~n",[Table, Pid]),
            erlang:garbage_collect(Pid),
            true = erlang:exit(Pid, brutal_kill)
    after
        5000 ->
            io:format("22 ~p  Pid ~p ~n",[Table, Pid]),
            erlang:garbage_collect(Pid),
            true = erlang:exit(Pid, brutal_kill)
    end.
