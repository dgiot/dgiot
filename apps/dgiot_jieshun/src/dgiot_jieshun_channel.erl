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

-module(dgiot_jieshun_channel).
-behavior(dgiot_channelx).
-author("johnliu").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include("dgiot_jieshun.hrl").
-define(TYPE, <<"ZETA">>).
-define(MAX_BUFF_SIZE, 1024).
%% API
-export([start/2]).
-include_lib("dgiot/include/logger.hrl").


%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).
%% TCP callback
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2, code_change/3]).


%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 1,
    title => #{
        zh => <<"捷顺采集通道"/utf8>>
    },
    description => #{
        zh => <<"捷顺采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 8486,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"侦听端口"/utf8>>
        }
    }
}).

start(ChannelId, ChannelArgs) ->
    dgiot_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, #{
    <<"port">> := Port,
    <<"product">> := Products

} = Args) ->
    Env = get_newenv(Args),
    [{ProdcutId, App} | _] = get_app(Products),
    State = #state{
        channelid = ChannelId,
        env = Env#{
            <<"channelid">> => ChannelId,
            <<"app">> => App,
            <<"product">> => ProdcutId
        }
    },
    case application:get_application(dgiot_modbus) of
        undefined -> application:start(dgiot_modbus);
        _ -> pass
    end,
    {ok, State, dgiot_tcp_server:child_spec(?MODULE, dgiot_utils:to_int(Port), State)};

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}, #{}}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, Event, State) ->
    ?LOG(info,"Channel ~p", [Event]),
    {ok, State}.

handle_message(Message, State) ->
    ?LOG(info,"Channel ~p", [Message]),
    {ok, State}.

stop(ChannelType, ChannelId, _State) ->
    ?LOG(warning,"Channel[~p,~p] stop", [ChannelType, ChannelId]),
    ok.

%% =======================
%% {ok, State} | {stop, Reason}
init(#tcp{state = #state{channelid = ChannelId} = State} = TCPState) ->
    case dgiot_bridge:get_products(ChannelId) of
        {ok, ?TYPE, ProductIds} ->
            {ok, TCPState#tcp{state = State#state{product = ProductIds}}};
        {error, not_find} ->
            {stop, not_find_channel}
    end.

% tcp的集中器设备会收到下面三个消息
handle_info({tcp, Buff}, #tcp{state = #state{product = _Products} = State} = TCPState) ->
    ?LOG(info,"Buff ~p", [dgiot_utils:binary_to_hex(Buff)]),
    case dgiot_jieshun_decoder:parse_frame(Buff, TCPState) of
        {ok, <<>>, Messages} ->
%%            ?LOG(info,"Messages ~p", [Messages]),
            lists:map(fun(X) ->
                MyState = #mystate{version = maps:get(<<"cs-version">>, X, <<"1.9">>)},
                case dgiot_jieshun_decoder:to_frame(X#{<<"dir">> := ?DIR_DOWN}, MyState) of
                    {ok, Payload} ->
%%                        ?LOG(info,"Payload ~p", [dgiot_utils:binary_to_hex(Payload)]),
                        dgiot_tcp_server:send(TCPState, Payload);
                    {error, Reason} ->
                        {error, Reason}
                end
                      end, Messages),
            {noreply, TCPState};
        {ok, _Rest, _Messages1} ->
            {noreply, TCPState};
        {error, Reason} ->
            {stop, Reason, TCPState#tcp{state = State}}
    end;

handle_info({deliver, _Topic, Msg}, #tcp{state = State} = TCPState) ->
    Payload = dgiot_mqtt:get_payload(Msg),
    ?LOG(debug,"Server recv from mqtt  Payload ~p ~n ~p~n", [Payload, State]),
    _Message =
        case jsx:is_json(Payload) of
            true ->
                jsx:decode(Payload, [{labels, binary}, return_maps]);
            false ->
                binary_to_term(Payload)
        end,
%%    NewState = do_message(Message, State, TCPState),
    {noreply, TCPState#tcp{state = TCPState}};

handle_info({jiaoshi, _ApID}, #tcp{state = _State} = TCPState) ->
%%    NewState = do_message({jiaoshi, ApID}, State, TCPState),
    {noreply, TCPState#tcp{state = TCPState}};

handle_info(tcp_closed, TCPState) ->
    ?LOG(debug,"tcpserver:~p, ~p~n", [tcp_closed, TCPState]),
    {noreply, TCPState};


%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.

terminate(_Reason, _TCPState) ->
    ok.

code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.

get_newenv(Args) ->
    maps:without([
        <<"behaviour">>,
        <<"MaxOverFlow">>,
        <<"Size">>,
        <<"applicationtText">>,
        <<"product">>], Args).

get_app(Products) ->
    lists:map(fun({ProdcutId, #{<<"ACL">> := Acl}}) ->
        Predicate = fun(E) ->
            case E of
                <<"role:", _/binary>> -> true;
                _ -> false
            end
                    end,
        [<<"role:", App/binary>> | _] = lists:filter(Predicate, maps:keys(Acl)),
        {ProdcutId, App}
              end, Products).
