%%%-------------------------------------------------------------------
%%% @doc 舵面传感器控制模块
%%%-------------------------------------------------------------------
-module(dgiot_uav_surface_controller).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot_uav/include/dgiot_uav.hrl").
-include_lib("dgiot_uav/include/dgiot_uav_config.hrl").

%% API
-export([
    unlock/2,
    set_angle_reference/2,
    save_config/2,
    set_baudrate/3,
    read_angles/2,
    read_registers/4,
    write_register/4,
    handle_port_data/4,
    handle_read_surface/1,
    test/0
]).

%% 舵面配置
-define(SLAVE_ID, 16#50).

%% 寄存器地址
-define(REG_SAVE,   16#00).
-define(REG_CALSW,  16#01).
-define(REG_BAUD,   16#04).
-define(REG_KEY,    16#69).
-define(REG_ROLL,   16#3D).
-define(REG_PITCH,  16#3E).
-define(REG_YAW,    16#3F).

-define(CAL_MODE_ANGLE_REF, 16#08).
-define(UNLOCK_KEY, 16#8588).

-define(BAUD_9600,   16#02).
-define(BAUD_115200, 16#06).

%% 读舵面参数
-define(READ_START_ADDR, 16#34).
-define(READ_REG_COUNT, 48).

%% 超时时间
-define(TIMEOUT_MS, 5000).

%% 注册报文列表
-define(SURFACE_DEVICES, [
    <<"wrj_dm_zqy">>, <<"wrj_dm_yqy">>,
    <<"wrj_dm_zcw">>, <<"wrj_dm_ycw">>,
    <<"wrj_dm_zhj">>
]).

%%====================================================================
%% 公开API
%%====================================================================
unlock(Pid, SlaveId) -> send_write(Pid, SlaveId, ?REG_KEY, ?UNLOCK_KEY).
set_angle_reference(Pid, SlaveId) -> send_write(Pid, SlaveId, ?REG_CALSW, ?CAL_MODE_ANGLE_REF).
save_config(Pid, SlaveId) -> send_write(Pid, SlaveId, ?REG_SAVE, 16#0000).
set_baudrate(Pid, SlaveId, Baud) -> send_write(Pid, SlaveId, ?REG_BAUD, Baud).

read_angles(Pid, SlaveId) ->
    case read_registers(Pid, SlaveId, ?REG_ROLL, 3) of
        {ok, [R, P, Y]} -> {ok, #{roll => to_angle(R), pitch => to_angle(P), yaw => to_angle(Y)}};
        Error -> Error
    end.

read_registers(Pid, SlaveId, StartAddr, Count) ->
    send_command(Pid, build_read_request(SlaveId, StartAddr, Count), 
                 fun(Resp) -> parse_read_response(Resp, Count) end).

write_register(Pid, SlaveId, RegAddr, Value) ->
    send_write(Pid, SlaveId, RegAddr, Value).

%%====================================================================
%% 端口数据处理
%%====================================================================
-spec handle_port_data(binary(), #tcp{}, #uav_state{}, inet:socket()) -> 
    {#tcp{}, #uav_state{}, noreply}.
handle_port_data(NewBuf, TCPState, UavState, Socket) ->
    case is_registration_binary(NewBuf) of
        {true, DeviceId, Rest} ->
            handle_registration(DeviceId, Rest, TCPState, UavState, Socket);
        false ->
            handle_modbus_data(NewBuf, TCPState, UavState, Socket)
    end.

handle_registration(DeviceId, Rest, TCPState, UavState, Socket) ->
    NewDevAddr = build_devaddr(UavState, DeviceId),
    NewUavState = UavState#uav_state{retry_count = 0, devaddr = NewDevAddr},
    NewTCPState = TCPState#tcp{clientid = UavState#uav_state.device_id},
    
    ?LOG(info, "[SURFACE] 注册成功 - Device:~s, DevAddr:~s", [DeviceId, NewDevAddr]),
    
    inet:setopts(Socket, [{active, once}]),
    {NewTCPState#tcp{buff = ensure_binary(Rest)}, NewUavState, noreply}.

handle_modbus_data(NewBuf, TCPState, UavState, Socket) when byte_size(NewBuf) >= 5 ->
    <<_SlaveId:8, FuncCode:8, _/binary>> = NewBuf,
    
    case FuncCode =:= 16#03 of
        true ->
            process_modbus_response(NewBuf, TCPState, UavState, Socket);
        false ->
            {TCPState#tcp{buff = <<>>}, UavState, noreply}
    end;
handle_modbus_data(NewBuf, TCPState, UavState, Socket) ->
    inet:setopts(Socket, [{active, once}]),
    {TCPState#tcp{buff = NewBuf}, UavState, noreply}.

process_modbus_response(Data, TCPState, UavState, Socket) ->
    case UavState#uav_state.devaddr of
        <<>> -> 
            ?LOG(warning, "[SURFACE] devaddr未设置，跳过处理");
        DevAddr ->
            uav_surface_service:handle_surface_data(
                UavState#uav_state.product_id, DevAddr, Data)
    end,
    NewUavState = UavState#uav_state{retry_count = 0},
    inet:setopts(Socket, [{active, once}]),
    {TCPState#tcp{buff = <<>>}, NewUavState, noreply}.

%%====================================================================
%% 定时读舵面
%%====================================================================
-spec handle_read_surface(#tcp{}) -> ok.
handle_read_surface(TCPState) ->
    case is_surface_device(TCPState#tcp.clientid) of
        true -> send_surface_read_command(TCPState);
        false -> ok
    end.

send_surface_read_command(TCPState) ->
    Cmd = build_read_request(?SLAVE_ID, ?READ_START_ADDR, ?READ_REG_COUNT),
    CmdWithCrc = add_crc(Cmd),
    dgiot_tcp_server:send(TCPState, CmdWithCrc),
    ?LOG(debug, "[SURFACE] 发送读命令 - Hex:~s", [dgiot_utils:binary_to_hex(CmdWithCrc)]).

is_surface_device(DeviceId) ->
    lists:member(DeviceId, ?SURFACE_DEVICES).

%%====================================================================
%% 命令构建
%%====================================================================
build_read_request(SlaveId, StartAddr, Count) ->
    <<SlaveId:8, 16#03:8, StartAddr:16, Count:16>>.

build_write_request(SlaveId, RegAddr, Value) ->
    <<SlaveId:8, 16#06:8, RegAddr:16, Value:16>>.

add_crc(Data) -> <<Data/binary, (crc16(Data)):16/little>>.

send_write(Pid, SlaveId, RegAddr, Value) ->
    send_command(Pid, build_write_request(SlaveId, RegAddr, Value),
                 fun(Resp) -> parse_write_response(Resp, SlaveId, RegAddr, Value) end).

send_command(Pid, Command, Parser) ->
    CmdWithCrc = add_crc(Command),
    case dgiot_tcp_client:send(Pid, CmdWithCrc) of
        ok ->
            receive
                {tcp, Response} -> Parser(Response)
            after ?TIMEOUT_MS -> {error, timeout}
            end;
        Error -> Error
    end.

%%====================================================================
%% 响应解析
%%====================================================================
parse_read_response(<<_SlaveId:8, 16#03:8, ByteCount:8, Data:ByteCount/binary, _Crc:16/little>>, ExpectedCount) ->
    case ByteCount =:= ExpectedCount * 2 of
        true -> {ok, parse_registers(Data, [])};
        false -> {error, invalid_length}
    end;
parse_read_response(_, _) -> {error, invalid_response}.

parse_write_response(<<SlaveId:8, 16#06:8, RegAddr:16, Value:16, _Crc:16/little>>, SlaveId, RegAddr, Value) ->
    {ok, #{address => RegAddr, value => Value}};
parse_write_response(<<SlaveId:8, 16#86:8, Exception:8, _Crc:16/little>>, SlaveId, _, _) ->
    {error, {modbus_exception, Exception}};
parse_write_response(_, _, _, _) -> {error, invalid_response}.

parse_registers(<<>>, Acc) -> lists:reverse(Acc);
parse_registers(<<Val:16, Rest/binary>>, Acc) -> parse_registers(Rest, [Val | Acc]).

%%====================================================================
%% 单位转换
%%====================================================================
to_angle(Raw) ->
    Signed = if Raw > 32767 -> Raw - 65536; true -> Raw end,
    Signed / 32768 * 180.

%%====================================================================
%% CRC16计算
%%====================================================================
crc16(Data) -> crc16_loop(Data, 16#FFFF).

crc16_loop(<<>>, Crc) -> Crc;
crc16_loop(<<Byte:8, Rest/binary>>, Crc) ->
    crc16_loop(Rest, crc16_byte(Crc bxor Byte, 8)).

crc16_byte(Crc, 0) -> Crc;
crc16_byte(Crc, N) ->
    Next = case (Crc band 1) =:= 1 of
        true -> (Crc bsr 1) bxor 16#A001;
        false -> Crc bsr 1
    end,
    crc16_byte(Next, N - 1).

%%====================================================================
%% 辅助函数
%%====================================================================
is_registration_binary(Bin) ->
    lists:foldl(fun(DeviceId, false) ->
        Prefix = <<DeviceId/binary, "\n">>,
        case Bin of
            <<Prefix, Rest/binary>> -> {true, DeviceId, Rest};
            _ -> false
        end
    end, false, ?SURFACE_DEVICES).

build_devaddr(UavState, DeviceId) ->
    IpBin = UavState#uav_state.ip_bin,
    Port = UavState#uav_state.port,
    <<IpBin/binary, "_", (integer_to_binary(Port))/binary, "_", DeviceId/binary>>.

ensure_binary(undefined) -> <<>>;
ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(_) -> <<>>.

%%====================================================================
%% 测试函数
%%====================================================================
test() ->
    ?LOG(info, "[SURFACE] 测试开始", []),
    test_check_device(),
    test_read_command(),
    test_timer(),
    {ok, done}.

test_check_device() ->
    LoginId = get(login_id),
    case is_surface_device(LoginId) of
        true -> ?LOG(info, "[TEST] ✓ 当前是舵面设备: ~p", [LoginId]);
        false -> ?LOG(warning, "[TEST] ✗ 当前不是舵面设备: ~p", [LoginId])
    end.

test_read_command() ->
    ?LOG(info, "[TEST] 读命令: SlaveId=0x~2.16.0B, Addr=0x~4.16.0B, Count=~p",
         [?SLAVE_ID, ?READ_START_ADDR, ?READ_REG_COUNT]).

test_timer() ->
    case get(surface_timer) of
        undefined -> ?LOG(warning, "[TEST] ✗ 定时器未设置");
        T -> ?LOG(info, "[TEST] ✓ 定时器已设置: ~p", [T])
    end.
