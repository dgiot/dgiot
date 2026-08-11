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

%% @doc UDP会话层模块
%% 统一的gen_server实现，管理连接状态和消息处理
-module(dgiot_udp_session).
-author("johnliu").
-behaviour(gen_server).
-include("logger.hrl").

%% API导出
-export([
    start_link/1,
    send/2, send/3, send/4,
    close/1,
    get_status/1,
    join_multicast_group/2,
    leave_multicast_group/2,
    send_multicast/4
]).

%% gen_server回调
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% 记录状态
-record(state, {
    mode,           % server | client
    socket,         % UDP套接字
    mod,            % 回调模块（服务器模式）
    options = [],   % 选项
    state,          % 用户状态
    remote_addr,    % 远程地址（客户端模式）
    remote_port,    % 远程端口（客户端模式）
    multicast_groups = [] % 已加入的多播组列表
}).

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 启动会话进程
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @doc 发送数据（使用已连接的套接字）
send(ClientPid, Data) ->
    gen_server:call(ClientPid, {send, Data}).

%% @doc 发送数据到指定地址（兼容旧接口）
send(ClientPid, Addr, Data) ->
    gen_server:call(ClientPid, {send, Addr, Data}).

%% @doc 发送数据到指定地址和端口
send(ClientPid, Addr, Port, Data) ->
    gen_server:call(ClientPid, {send, Addr, Port, Data}).

%% @doc 关闭会话
close(ClientPid) ->
    gen_server:call(ClientPid, close).

%% @doc 获取会话状态
get_status(ClientPid) ->
    gen_server:call(ClientPid, get_status).

%% @doc 加入多播组
join_multicast_group(ClientPid, MulticastGroup) ->
    gen_server:call(ClientPid, {join_multicast_group, MulticastGroup}).

%% @doc 离开多播组
leave_multicast_group(ClientPid, MulticastGroup) ->
    gen_server:call(ClientPid, {leave_multicast_group, MulticastGroup}).

%% @doc 发送多播消息
send_multicast(ClientPid, MulticastGroup, Port, Message) ->
    gen_server:call(ClientPid, {send_multicast, MulticastGroup, Port, Message}).

%%%===================================================================
%%% gen_server回调
%%%===================================================================

%% @doc 初始化会话
init(Args) ->
    io:format("~s ~p [UDP_SESSION] Args = ~p.~n", [?FILE, ?LINE, Args]),
    
    % 处理参数类型，支持map、proplists和混合类型
    {Mode, Mod, Options, UserState} = case Args of
        _ when is_map(Args) ->
            {
                maps:get(mode, Args, server),
                maps:get(mod, Args, undefined),
                maps:get(options, Args, []),
                maps:get(state, Args, #{})
            };
        _ when is_list(Args) ->
            % 检查是否是混合类型（列表包含map）
            case Args of
                % 特殊情况：列表第一个元素是元组，后面是map
                [{mode, _} | Map] when is_map(Map) ->
                    % 将元组列表和map合并
                    MergedMap = lists:foldl(fun
                        ({Key, Value}, Acc) when is_atom(Key) -> 
                            Acc#{Key => Value};
                        (MapPart, Acc) when is_map(MapPart) -> 
                            maps:merge(Acc, MapPart);
                        (_Other, Acc) ->
                            Acc
                    end, #{}, [{mode, server}, Map]),
                    {
                        maps:get(mode, MergedMap, server),
                        maps:get(mod, MergedMap, undefined),
                        maps:get(options, MergedMap, []),
                        maps:get(state, MergedMap, #{})
                    };
                _ ->
                    % 检查是否是混合类型（列表包含map）
                    HasMap = lists:any(fun
                        (Map) when is_map(Map) -> true;
                        (_) -> false
                    end, Args),
                    
                    case HasMap of
                        true ->
                            % 混合类型：将列表中的proplists和map合并
                            MergedMap = lists:foldl(fun
                                (Map, Acc) when is_map(Map) -> 
                                    maps:merge(Acc, Map);
                                ({Key, Value}, Acc) when is_atom(Key) -> 
                                    Acc#{Key => Value};
                                ({Key, Value}, Acc) when is_binary(Key) -> 
                                    % 将二进制键转换为原子
                                    AtomKey = binary_to_atom(Key, utf8),
                                    Acc#{AtomKey => Value};
                                (_Other, Acc) ->
                                    Acc
                            end, #{}, Args),
                            {
                                maps:get(mode, MergedMap, server),
                                maps:get(mod, MergedMap, undefined),
                                maps:get(options, MergedMap, []),
                                maps:get(state, MergedMap, #{})
                            };
                        false ->
                            % 纯proplists
                            {
                                proplists:get_value(mode, Args, server),
                                proplists:get_value(mod, Args, undefined),
                                proplists:get_value(options, Args, []),
                                proplists:get_value(state, Args, #{})
                            }
                    end
            end
    end,
    
    io:format("~s ~p [UDP_SESSION] Parsed: Mode=~p, Mod=~p, Options=~p, UserState=~p~n", 
              [?FILE, ?LINE, Mode, Mod, Options, UserState]),
    
    % 检查是否有多播组配置
    MulticastGroups = proplists:get_value(multicast_groups, Options, []),
    io:format("~s ~p [UDP_SESSION] Multicast groups from options: ~p~n", 
              [?FILE, ?LINE, MulticastGroups]),
    
    case dgiot_udp_protocol:create_socket(Options) of
        {ok, Socket} ->
            io:format("~s ~p [UDP_SESSION] Socket created successfully: ~p~n", 
                     [?FILE, ?LINE, Socket]),
            
            % 加入多播组（如果有配置）
            JoinedGroups = join_multicast_groups(Socket, MulticastGroups),
            
            State = #state{
                mode = Mode,
                socket = Socket,
                mod = Mod,
                options = Options,
                state = UserState,
                multicast_groups = JoinedGroups
            },
            
            % 如果是服务器模式，调用回调模块的init函数
            case Mode of
                server when Mod =/= undefined ->
                    case Mod:init(UserState) of
                        {ok, NewUserState} ->
                            io:format("~s ~p [UDP_SESSION] Callback module init success~n", 
                                     [?FILE, ?LINE]),
                            {ok, State#state{state = NewUserState}};
                        {stop, Reason} ->
                            io:format("~s ~p [UDP_SESSION] Callback module init failed: ~p~n", 
                                     [?FILE, ?LINE, Reason]),
                            {stop, Reason}
                    end;
                _ ->
                    io:format("~s ~p [UDP_SESSION] Session initialized successfully~n", 
                             [?FILE, ?LINE]),
                    {ok, State}
            end;
        {error, Reason} ->
            io:format("~s ~p [UDP_SESSION] Socket creation failed: ~p~n", 
                     [?FILE, ?LINE, Reason]),
            {stop, Reason}
    end.

%% @doc 处理调用
handle_call({send, Data}, _From, #state{mode = client, socket = Socket} = State) ->
    Result = dgiot_udp_transport:send(Socket, Data),
    {reply, Result, State};

handle_call({send, Addr, Data}, _From, #state{socket = Socket, remote_port = RemotePort} = State) ->
    % 使用记录的远程端口发送数据
    case RemotePort of
        undefined ->
            {reply, {error, no_remote_port}, State};
        Port ->
            Result = dgiot_udp_transport:send(Socket, Addr, Port, Data),
            {reply, Result, State}
    end;

handle_call({send, Addr, Port, Data}, _From, #state{socket = Socket} = State) ->
    Result = dgiot_udp_transport:send(Socket, Addr, Port, Data),
    {reply, Result, State};

handle_call(close, _From, State) ->
    {stop, normal, ok, State};

handle_call(get_status, _From, #state{mode = Mode, socket = Socket} = State) ->
    Status = #{
        mode => Mode,
        socket => Socket,
        options => State#state.options,
        state => State#state.state,
        multicast_groups => State#state.multicast_groups
    },
    {reply, Status, State};

handle_call({join_multicast_group, MulticastGroup}, _From, #state{socket = Socket, multicast_groups = Groups} = State) ->
    case dgiot_udp_multicast:join_multicast_group(Socket, MulticastGroup) of
        ok ->
            NewGroups = case lists:member(MulticastGroup, Groups) of
                true -> Groups;
                false -> [MulticastGroup | Groups]
            end,
            {reply, ok, State#state{multicast_groups = NewGroups}};
        Error ->
            {reply, Error, State}
    end;

handle_call({leave_multicast_group, MulticastGroup}, _From, #state{socket = Socket, multicast_groups = Groups} = State) ->
    case dgiot_udp_multicast:leave_multicast_group(Socket, MulticastGroup) of
        ok ->
            NewGroups = lists:delete(MulticastGroup, Groups),
            {reply, ok, State#state{multicast_groups = NewGroups}};
        Error ->
            {reply, Error, State}
    end;

handle_call({send_multicast, MulticastGroup, Port, Message}, _From, #state{socket = Socket} = State) ->
    Result = dgiot_udp_multicast:send_multicast(Socket, MulticastGroup, Port, Message),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc 处理Cast消息
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc 处理Info消息（UDP数据接收）
handle_info({udp, Socket, Addr, Port, Data}, #state{mode = server, mod = Mod, state = UserState} = State) ->
    % 重新激活套接字
    dgiot_udp_transport:setopts(Socket, [{active, once}]),
    
    % 调用回调模块处理数据
    case Mod of
        undefined ->
            ?LOG(warning, "Received UDP data but no callback module defined: ~p", [Data]);
        _ ->
            case Mod:handle_info({udp, Addr, Port, Data}, UserState) of
                {ok, NewUserState} ->
                    {noreply, State#state{state = NewUserState}};
                {stop, Reason, NewUserState} ->
                    {stop, Reason, State#state{state = NewUserState}};
                _ ->
                    {noreply, State}
            end
    end;

handle_info({udp, Socket, Addr, Port, Data}, #state{mode = client} = State) ->
    % 重新激活套接字
    dgiot_udp_transport:setopts(Socket, [{active, once}]),
    
    % 客户端模式，记录远程地址
    NewState = State#state{
        remote_addr = Addr,
        remote_port = Port
    },
    
    % 这里可以添加客户端的数据处理逻辑
    ?LOG(debug, "Client received UDP data from ~p:~p: ~p", [Addr, Port, Data]),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc 终止处理
terminate(_Reason, #state{socket = Socket}) ->
    case Socket of
        undefined -> ok;
        _ -> dgiot_udp_transport:close(Socket)
    end,
    ok.

%% @doc 代码变更
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @private 加入多播组列表
join_multicast_groups(Socket, MulticastGroups) ->
    io:format("~s ~p [UDP_SESSION] Joining multicast groups: ~p~n", 
              [?FILE, ?LINE, MulticastGroups]),
    
    JoinedGroups = lists:filtermap(fun(Group) ->
        case dgiot_udp_multicast:join_multicast_group(Socket, Group) of
            ok ->
                io:format("~s ~p [UDP_SESSION] ✓ Successfully joined multicast group: ~p~n", 
                         [?FILE, ?LINE, Group]),
                {true, Group};
            Error ->
                io:format("~s ~p [UDP_SESSION] ✗ Failed to join multicast group ~p: ~p~n", 
                         [?FILE, ?LINE, Group, Error]),
                false
        end
    end, MulticastGroups),
    
    io:format("~s ~p [UDP_SESSION] Total joined groups: ~p~n", 
              [?FILE, ?LINE, length(JoinedGroups)]),
    JoinedGroups.
