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
-module(dgiot_tcpc_channel).
-behavior(dgiot_channelx).
-include("dgiot_bridge.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-define(TYPE, <<"TCPC">>).
-record(state, {id, env}).
%% API
-export([
    start/2
]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).

%% tcp client callback
-define(MAX_BUFF_SIZE, 10 * 1024).
-export([init/1, handle_info/2, terminate/2]).

-channel(?TYPE).
-channel_type(#{
    cType => ?TYPE,
    type => ?BRIDGE_CHL,
    title => #{
        zh => <<"TCPC资源通道"/utf8>>
    },
    description => #{
        zh => <<"TCPC资源通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"ip">> => #{
        order => 1,
        type => string,
        required => true,
        default => <<"127.0.0.1"/utf8>>,
        title => #{
            zh => <<"服务器地址"/utf8>>
        },
        description => #{
            zh => <<"服务器地址"/utf8>>
        }
    },
    <<"port">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 8080,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"端口"/utf8>>
        }
    },
    <<"ico">> => #{
        order => 102,
        type => string,
        required => false,
        default => <<"/dgiot_file/shuwa_tech/zh/product/dgiot/channel/TcpIcon.jpeg">>,
        title => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        },
        description => #{
            en => <<"channel ICO">>,
            zh => <<"通道ICO"/utf8>>
        }
    }
}).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).


%% 通道初始化
init(?TYPE, ChannelId,
        #{<<"ip">> := Ip, <<"port">> := Port} = Args) ->
    State = #state{id = ChannelId, env = Args},
    NewArgs = #{<<"ip">> => Ip, <<"port">> => Port, <<"mod">> => ?MODULE, <<"child">> => #{}},
    {ok, State, dgiot_client:register(ChannelId, tcp_client_sup, NewArgs)}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info, "channel ~p", [Event]),
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning, "channel stop ~p,~p", [ChannelType, ChannelId]),
    ok.


%% tcp client  callback
init(TCPState) ->
    {ok, TCPState}.

handle_info(connection_ready, #tcp{state = #{productid := ProductId} = _State} = TCPState) ->
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 1 + 1) * 1000,
    dgiot_tcp_client:send(TCPState, <<"login">>),
    case do_cmd(ProductId, connection_ready, <<>>, TCPState) of
        default ->
            erlang:send_after(Time, self(), login);
        _ ->
            pass
    end,
    {noreply, TCPState};

handle_info(#{<<"cmd">> := Cmd, <<"data">> := Data, <<"productId">> := ProductId}, TCPState) ->
    case do_cmd(ProductId, Cmd, Data, TCPState) of
        default ->
            {noreply, TCPState};
        Result ->
            Result
    end;

handle_info(tcp_closed, #tcp{state = #{productid := ProductId} = _State} = TCPState) ->
    case do_cmd(ProductId, tcp_closed, <<>>, TCPState) of
        default ->
            {noreply, TCPState};
        Result ->
            Result
    end;

handle_info({tcp, Buff}, #tcp{buff = Old, state = #{productid := ProductId} = _State} = TCPState) ->
    Data = <<Old/binary, Buff/binary>>,
    case do_cmd(ProductId, tcp, Data, TCPState) of
        default ->
            {noreply, TCPState};
        {noreply, Bin, State} ->
            {noreply, TCPState#tcp{buff = Bin, state = State}};
        {stop, Reason, State} ->
            {stop, Reason, TCPState#tcp{state = State}};
        Result ->
            Result
    end;

handle_info({deliver, _Topic, Msg}, #tcp{state = State} = TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    ?LOG(info, "Client recv from mqtt  Payload ~p ~n ~p~n", [Payload, State]),
%%    Message =
%%        case jsx:is_json(Payload) of
%%            true ->
%%                jsx:decode(Payload, [{labels, binary}, return_maps]);
%%            false ->
%%                binary_to_term(Payload)
%%        end,
    {noreply, TCPState};


handle_info(login, #tcp{state = #{productid := ProductId, devaddr := DevAddr, hb := Hb} = _State} = TCPState) ->
    Topic = <<"mock/", ProductId/binary, "/", DevAddr/binary>>,
    dgiot_mqtt:subscribe(Topic),
    erlang:send_after(Hb * 1000, self(), heartbeat),
    ?LOG(info, "~p ", [<<"login">>]),
    dgiot_tcp_client:send(TCPState, <<"login">>),
    {noreply, TCPState};

handle_info(heartbeat, #tcp{state = #{devaddr := _DevAddr, hb := Hb} = _State} = TCPState) ->
    erlang:send_after(Hb * 1000, self(), heartbeat),
%%    ?LOG(info,"~p ",[<<"heartbeat">>]),
    dgiot_tcp_client:send(TCPState, <<"heartbeat">>),
    {noreply, TCPState};

handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    ok.

do_cmd(ProductId, Cmd, Data, #tcp{state = #{id := ChannelId} = State} = TCPState) ->
    case dgiot_hook:run_hook({tcp, ProductId}, [Cmd, Data, State]) of
        {ok, NewState} ->
            {noreply, TCPState#tcp{state = NewState}};
        {reply, ProductId, Payload, NewState} ->
            case dgiot_tcp_server:send(TCPState#tcp{state = NewState}, Payload) of
                ok ->
                    ok;
                {error, Reason} ->
                    dgiot_bridge:send_log(ChannelId, ProductId, "Send Fail, ~p, CMD:~p", [Cmd, Reason])
            end,
            {noreply, TCPState#tcp{state = NewState}};
        _ ->
            default
    end.