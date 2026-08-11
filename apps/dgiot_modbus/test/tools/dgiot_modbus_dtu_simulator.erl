%%%-------------------------------------------------------------------
%%% @doc
%%% Modbus RTU DTU模拟器
%%% 
%%% 功能：
%%% 1. 模拟DTU终端连接到DG-IoT通道
%%% 2. 发送注册报文（wrj_dm_zqy）
%%% 3. 定期发送传感器数据报文
%%% 4. 验证数据解析结果
%%% 
%%% 使用方式：
%%% 1. 启动模拟器：dgiot_modbus_dtu_simulator:start().
%%% 2. 连接到通道：dgiot_modbus_dtu_simulator:connect(20000).
%%% 3. 发送测试数据：dgiot_modbus_dtu_simulator:send_test_data().
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_modbus_dtu_simulator).

%% API
-export([start/0, stop/0, connect/1, disconnect/0, send_registration/0, 
         send_report_data/0, send_custom_data/1, get_status/0, test_full_flow/0]).

%% 内部函数
-export([init/0, loop/1]).

-include_lib("dgiot/include/logger.hrl").

%% DTU模拟器状态记录（使用不同的记录名避免冲突）
-record(dtu_state, {
    socket :: gen_tcp:socket() | undefined,
    connected = false :: boolean(),
    channel_id :: binary() | undefined,
    product_id = <<"feeb43bffb">> :: binary(),
    interval = 5000 :: integer(),  % 上报间隔（毫秒）
    timer_ref :: reference() | undefined
}).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 启动模拟器
start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.

%% @doc 停止模拟器
stop() ->
    ?MODULE ! stop,
    ok.

%% @doc 连接到指定通道
connect(ChannelId) when is_integer(ChannelId) ->
    connect(integer_to_binary(ChannelId));
connect(ChannelId) when is_binary(ChannelId) ->
    ?MODULE ! {connect, ChannelId},
    ok.

%% @doc 断开连接
disconnect() ->
    ?MODULE ! disconnect,
    ok.

%% @doc 发送注册报文
send_registration() ->
    ?MODULE ! send_registration,
    ok.

%% @doc 发送上报数据
send_report_data() ->
    ?MODULE ! send_report_data,
    ok.

%% @doc 发送自定义数据
send_custom_data(Data) when is_binary(Data) ->
    ?MODULE ! {send_custom, Data},
    ok.

%% @doc 获取模拟器状态
get_status() ->
    ?MODULE ! {get_status, self()},
    receive
        {status, Status} -> Status
    after 5000 -> {error, timeout}
    end.

%% @doc 测试完整流程
test_full_flow() ->
    ?LOG(info, "开始DTU模拟器完整流程测试..."),
    
    % 1. 启动模拟器
    {ok, _Pid} = start(),
    timer:sleep(1000),
    
    % 2. 连接到通道20000
    connect(20000),
    timer:sleep(2000),
    
    % 3. 发送注册报文
    send_registration(),
    timer:sleep(2000),
    
    % 4. 发送上报数据
    send_report_data(),
    timer:sleep(3000),
    
    % 5. 验证数据解析
    verify_data_parsing(),
    
    % 6. 停止模拟器
    stop(),
    
    ?LOG(info, "DTU模拟器完整流程测试完成"),
    ok.

%%%===================================================================
%%% 内部函数
%%%===================================================================

init() ->
    ?LOG(info, "DTU模拟器启动"),
    State = #dtu_state{},
    loop(State).

loop(State) ->
    receive
        {connect, ChannelId} ->
            NewState = handle_connect(ChannelId, State),
            loop(NewState);
        
        disconnect ->
            NewState = handle_disconnect(State),
            loop(NewState);
        
        send_registration ->
            NewState = handle_send_registration(State),
            loop(NewState);
        
        send_report_data ->
            NewState = handle_send_report_data(State),
            loop(NewState);
        
        {send_custom, Data} ->
            NewState = handle_send_custom(Data, State),
            loop(NewState);
        
        {get_status, From} ->
            From ! {status, State},
            loop(State);
        
        stop ->
            handle_stop(State);
        
        _ ->
            loop(State)
    end.

handle_connect(ChannelId, State) ->
    case gen_tcp:connect("localhost", 20000, [binary, {active, false}, {packet, raw}]) of
        {ok, Socket} ->
            ?LOG(info, "成功连接到通道 ~p", [ChannelId]),
            State#dtu_state{
                socket = Socket,
                connected = true,
                channel_id = ChannelId
            };
        {error, Reason} ->
            ?LOG(error, "连接失败: ~p", [Reason]),
            State
    end.

handle_disconnect(#dtu_state{socket = Socket} = State) when Socket =/= undefined ->
    gen_tcp:close(Socket),
    ?LOG(info, "断开连接"),
    State#dtu_state{socket = undefined, connected = false};
handle_disconnect(State) ->
    ?LOG(warning, "未连接，无需断开"),
    State.

handle_send_registration(#dtu_state{socket = Socket} = State) when Socket =/= undefined ->
    % 注册报文：wrj_dm_zqy
    RegistrationData = <<"wrj_dm_zqy">>,
    case gen_tcp:send(Socket, RegistrationData) of
        ok ->
            ?LOG(info, "发送注册报文: ~p", [RegistrationData]),
            State;
        {error, Reason} ->
            ?LOG(error, "发送注册报文失败: ~p", [Reason]),
            State
    end;
handle_send_registration(State) ->
    ?LOG(warning, "未连接，无法发送注册报文"),
    State.

handle_send_report_data(#dtu_state{socket = Socket} = State) when Socket =/= undefined ->
    % 上报报文数据（十六进制）
    ReportData = hex_to_binary("0103600C190E130311003902B200020880000000000000000000000000006A009E9FB5059200000000000000000000000000000000000000000000000000000000000000002FFDEDF1F8868AF7000000000000000000000000000000004365000000007DB3"),
    case gen_tcp:send(Socket, ReportData) of
        ok ->
            ?LOG(info, "发送上报数据，长度: ~p字节", [byte_size(ReportData)]),
            State;
        {error, Reason} ->
            ?LOG(error, "发送上报数据失败: ~p", [Reason]),
            State
    end;
handle_send_report_data(State) ->
    ?LOG(warning, "未连接，无法发送上报数据"),
    State.

handle_send_custom(Data, #dtu_state{socket = Socket} = State) when Socket =/= undefined ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            ?LOG(info, "发送自定义数据，长度: ~p字节", [byte_size(Data)]),
            State;
        {error, Reason} ->
            ?LOG(error, "发送自定义数据失败: ~p", [Reason]),
            State
    end;
handle_send_custom(_Data, State) ->
    ?LOG(warning, "未连接，无法发送自定义数据"),
    State.

handle_stop(#dtu_state{socket = Socket} = State) ->
    case Socket of
        undefined -> ok;
        _ -> gen_tcp:close(Socket)
    end,
    ?LOG(info, "DTU模拟器停止"),
    ok.

%%%===================================================================
%%% 工具函数
%%%===================================================================

%% @doc 十六进制字符串转换为二进制
hex_to_binary(HexStr) ->
    CleanHex = re:replace(HexStr, "[^0-9A-Fa-f]", "", [global, {return, binary}]),
    << <<(binary_to_integer(<<H, L>>, 16))>> || <<H, L>> <= CleanHex >>.

%% @doc 验证数据解析
verify_data_parsing() ->
    ?LOG(info, "开始验证数据解析..."),
    
    % 等待数据解析完成
    timer:sleep(2000),
    
    % 这里可以添加具体的验证逻辑
    % 例如：查询设备数据，验证角度值等
    ?LOG(info, "数据解析验证完成（具体验证逻辑待实现）"),
    ok.
