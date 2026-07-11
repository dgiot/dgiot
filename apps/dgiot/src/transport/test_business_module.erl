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

%% @doc 测试业务逻辑模块
%% 用于UDP服务器和客户端测试
-module(test_business_module).
-author("johnliu").
-include("dgiot_socket.hrl").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

%% API导出
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3
]).

%% 状态记录定义
-record(state, {
    received_messages = [],
    sent_messages = [],
    multicast_messages = [],
    client_info = #{},
    start_time = os:system_time(millisecond)
}).

%%%===================================================================
%%% 回调函数
%%%===================================================================

%% @doc 初始化业务模块（服务器模式）
init(#udp{} = State) ->
    io:format("~s ~p Event = Initializing test business module for UDP server.~n", [?FILE, ?LINE]),
    {ok, State#udp{state = #state{}}};

%% @doc 初始化业务模块（客户端模式）
init(#dclient{} = Dclient) ->
    io:format("~s ~p Event = Initializing test business module for UDP client.~n", [?FILE, ?LINE]),
    {ok, Dclient#dclient{child = #state{}}};

%% @doc 处理元组参数（兼容性）
init({udp} = State) ->
    io:format("~s ~p Event = Initializing test business module with tuple state: ~p.~n", [?FILE, ?LINE, State]),
    {ok, #udp{state = #state{}}}.

%% @doc 处理调用请求
handle_call(get_stats, _From, #dclient{child = State} = Dclient) ->
    Stats = #{
        received_count => length(State#state.received_messages),
        sent_count => length(State#state.sent_messages),
        multicast_count => length(State#state.multicast_messages),
        uptime_ms => os:system_time(millisecond) - State#state.start_time
    },
    {reply, {ok, Stats}, Dclient};

handle_call(get_stats, _From, #udp{state = State} = UdpState) ->
    Stats = #{
        received_count => length(State#state.received_messages),
        sent_count => length(State#state.sent_messages),
        multicast_count => length(State#state.multicast_messages),
        uptime_ms => os:system_time(millisecond) - State#state.start_time
    },
    {reply, {ok, Stats}, UdpState};

handle_call(Request, From, #dclient{} = Dclient) ->
    io:format("~s ~p Event = Unhandled call: ~p from ~p.~n", [?FILE, ?LINE, Request, From]),
    {reply, {error, unhandled_call}, Dclient};

handle_call(Request, From, #udp{} = State) ->
    io:format("~s ~p Event = Unhandled call: ~p from ~p.~n", [?FILE, ?LINE, Request, From]),
    {reply, {error, unhandled_call}, State}.

%% @doc 处理异步请求
handle_cast(Msg, #dclient{} = Dclient) ->
    io:format("~s ~p Event = Unhandled cast: ~p.~n", [?FILE, ?LINE, Msg]),
    {noreply, Dclient};

handle_cast(Msg, #udp{} = State) ->
    io:format("~s ~p Event = Unhandled cast: ~p.~n", [?FILE, ?LINE, Msg]),
    {noreply, State}.

%% @doc 处理连接就绪事件
handle_info(connection_ready, #dclient{child = State} = Dclient) ->
    io:format("~s ~p Event = UDP client connection ready.~n", [?FILE, ?LINE]),
    {noreply, Dclient#dclient{child = State#state{client_info = #{connected => true, timestamp => os:system_time(millisecond)}}}};

%% @doc 处理连接就绪事件（服务器模式）
handle_info(connection_ready, #udp{state = State} = UdpState) ->
    io:format("~s ~p Event = UDP server connection ready.~n", [?FILE, ?LINE]),
    {noreply, UdpState#udp{state = State#state{client_info = #{connected => true, timestamp => os:system_time(millisecond)}}}};

%% @doc 处理接收到的UDP数据
handle_info({udp, Ip, Port, Data, IsMulticast}, #dclient{child = State} = Dclient) ->
    io:format("~s ~p Event = Client received UDP data from ~p:~p, multicast: ~p, length: ~p bytes.~n", 
             [?FILE, ?LINE, Ip, Port, IsMulticast, byte_size(Data)]),
    
    % 记录接收到的消息
    MessageRecord = #{
        ip => Ip,
        port => Port,
        data => Data,
        multicast => IsMulticast,
        timestamp => os:system_time(millisecond),
        length => byte_size(Data)
    },
    
    NewState = case IsMulticast of
        true ->
            State#state{
                received_messages = [MessageRecord | State#state.received_messages],
                multicast_messages = [MessageRecord | State#state.multicast_messages]
            };
        false ->
            State#state{
                received_messages = [MessageRecord | State#state.received_messages]
            }
    end,
    
    % 打印消息内容（限制长度以避免控制台输出过多）
    DisplayData = case byte_size(Data) > 100 of
        true -> <<(binary:part(Data, 0, 100))/binary, <<"...">>/binary>>;
        false -> Data
    end,
    io:format("~s ~p Event = Message content: ~p.~n", [?FILE, ?LINE, DisplayData]),
    
    {noreply, Dclient#dclient{child = NewState}};

%% @doc 处理接收到的UDP数据（服务器模式）
handle_info({udp, Ip, Port, Data, IsMulticast}, State) when is_record(State, state) ->
    io:format("~s ~p Event = Server received UDP data from ~p:~p, multicast: ~p, length: ~p bytes.~n", 
             [?FILE, ?LINE, Ip, Port, IsMulticast, byte_size(Data)]),
    
    % 记录接收到的消息
    MessageRecord = #{
        ip => Ip,
        port => Port,
        data => Data,
        multicast => IsMulticast,
        timestamp => os:system_time(millisecond),
        length => byte_size(Data)
    },
    
    % 服务器模式下，State 是 #state{} 记录
    NewState = case IsMulticast of
        true ->
            State#state{
                received_messages = [MessageRecord | State#state.received_messages],
                multicast_messages = [MessageRecord | State#state.multicast_messages]
            };
        false ->
            State#state{
                received_messages = [MessageRecord | State#state.received_messages]
            }
    end,
    
    % 打印消息内容（限制长度以避免控制台输出过多）
    DisplayData = case byte_size(Data) > 100 of
        true -> <<(binary:part(Data, 0, 100))/binary, <<"...">>/binary>>;
        false -> Data
    end,
    io:format("~s ~p Event = Message content: ~p.~n", [?FILE, ?LINE, DisplayData]),
    
    {noreply, NewState};

%% @doc 处理连接关闭事件
handle_info(udp_closed, #dclient{child = State} = Dclient) ->
    io:format("~s ~p Event = UDP client connection closed.~n", [?FILE, ?LINE]),
    NewState = State#state{client_info = #{connected => false, timestamp => os:system_time(millisecond)}},
    {noreply, Dclient#dclient{child = NewState}};

%% @doc 处理连接关闭事件（服务器模式）
handle_info(udp_closed, #udp{state = State} = UdpState) ->
    io:format("~s ~p Event = UDP server connection closed.~n", [?FILE, ?LINE]),
    NewState = State#state{client_info = #{connected => false, timestamp => os:system_time(millisecond)}},
    {noreply, UdpState#udp{state = NewState}};

%% @doc 处理其他信息
handle_info(Info, #dclient{} = Dclient) ->
    io:format("~s ~p Event = Client received unhandled info: ~p.~n", [?FILE, ?LINE, Info]),
    {noreply, Dclient};

handle_info(Info, #udp{} = State) ->
    io:format("~s ~p Event = Server received unhandled info: ~p.~n", [?FILE, ?LINE, Info]),
    {noreply, State}.

%% @doc 终止清理
terminate(Reason, #state{}) ->
    io:format("~s ~p Event = Test business module terminating: ~p.~n", [?FILE, ?LINE, Reason]),
    ok;

terminate(Reason, _) ->
    io:format("~s ~p Event = Test business module terminating: ~p.~n", [?FILE, ?LINE, Reason]),
    ok.

%% @doc 代码变更
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
