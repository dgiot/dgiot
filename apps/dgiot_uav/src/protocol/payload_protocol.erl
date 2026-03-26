%%%-------------------------------------------------------------------
%%% @doc
%%% payload_protocol.erl - 载荷协议处理器模块
%%%
%%% 本模块负责处理飞控到载荷（FC-to-Payload）和载荷到飞控（Payload-to-FC）的通信协议，
%%% 包括数据终端（Data Terminal）帧的处理。作为载荷协议的总入口，协调各子模块。
%%%
%%% 协议对应：Payload.docx 中的飞控与载荷通信协议。
%%%
%%% 主要功能：
%%% - start/0, stop/0: 启动/停止协议处理器进程
%%% - process_frame/1: 处理协议帧（自动分类）
%%% - process_frame/2: 处理指定类型的协议帧
%%% - send_command/3: 发送命令到载荷
%%% - send_command/4: 发送多个命令（可重复）
%%% - get_status/0: 获取协议处理器状态
%%% - get_statistics/0: 获取统计信息
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(payload_protocol).

%% API
-export([
    start/0,
    stop/0,
    process_frame/1,
    process_frame/2,
    send_command/3,
    send_command/4,
    get_status/0,
    get_statistics/0,
    set_config/1,
    get_config/1,
    test/0
]).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot_uav/include/dgiot_uav.hrl").

%% 命令码定义（与 fc_to_payload 保持一致）
-define(CMD_PAYLOAD_WORK, 16#01).
-define(CMD_PAYLOAD_SLEEP, 16#02).
-define(CMD_VISIBLE_LIGHT, 16#04).
-define(CMD_INFRARED, 16#05).

%% 错误码
-define(ERR_INVALID_COMMAND, invalid_command).

%% 数据类型定义
-define(DATA_TYPE_COMPOSITE, 16#1D).
-define(DATA_TYPE_VISIBLE_IMG, 16#14).
-define(DATA_TYPE_INFRARED_IMG, 16#15).

%% 分辨率定义
-define(RES_ACCEL, 100).
-define(RES_ANGLE_RATE_HIGH, 200).

%% 协议处理器状态记录
-record(state, {
    mode = normal,                 % 运行模式：normal, debug, test
    fc_sequence = 0,               % FC帧序列号
    pl_sequence = 0,               % Payload帧序列号
    dt_sequence = 0,               % 数据终端帧序列号
    last_command,                  % 最后接收到的命令
    last_status,                   % 最后接收到的状态
    frame_count = 0,               % 总帧计数
    error_count = 0,               % 错误计数
    start_time                     % 启动时间
}).

%%%===================================================================
%%% API函数
%%%===================================================================

%% @doc 启动协议处理器
-spec start() -> {ok, pid()}.
start() ->
    Pid = spawn(fun() -> init() end),
    register(payload_protocol, Pid),
    {ok, Pid}.

%% @doc 停止协议处理器
-spec stop() -> ok.
stop() ->
    payload_protocol ! stop,
    ok.

%% @doc 处理协议帧（自动分类）
-spec process_frame(binary()) -> {ok, term()} | {error, term()}.
process_frame(Data) ->
    payload_protocol ! {process, self(), Data},
    receive
        {result, Result} -> Result
    after 1000 -> {error, timeout}
    end.

%% @doc 处理指定类型的协议帧
-spec process_frame(atom(), binary()) -> {ok, term()} | {error, term()}.
process_frame(fc_to_payload, Data) ->
    fc_to_payload:decode(Data);
process_frame(payload_to_fc, Data) ->
    payload_to_fc:decode(Data);
process_frame(data_terminal, Data) ->
    data_terminal:decode_frame(Data).

%% @doc 发送命令到载荷
-spec send_command(integer(), map(), map()) -> {ok, [binary()]} | {error, term()}.
send_command(Command, Params, _Options) ->
    case fc_to_payload:validate_command(Command) of
        true ->
            Frames = fc_to_payload:send_command(Command, maps:get(aircraft_params, Params, <<0:256>>)),
            payload_protocol ! {send_frames, Frames},
            {ok, Frames};
        false ->
            {error, ?ERR_INVALID_COMMAND}
    end.

%% @doc 发送多个命令到载荷
-spec send_command([integer()], map(), map(), integer()) -> {ok, [binary()]}.
send_command(Commands, Params, _Options, Repeat) when is_list(Commands) ->
    AircraftParams = maps:get(aircraft_params, Params, <<0:256>>),
    Frames = lists:flatmap(
        fun(Command) ->
            fc_to_payload:send_command(Command, AircraftParams)
        end,
        lists:duplicate(Repeat, Commands)
    ),
    payload_protocol ! {send_frames, Frames},
    {ok, Frames}.

%% @doc 获取协议处理器状态
-spec get_status() -> map().
get_status() ->
    payload_protocol ! {get_status, self()},
    receive
        {status, Status} -> Status
    after 1000 -> #{error => timeout}
    end.

%% @doc 获取协议统计信息
-spec get_statistics() -> map().
get_statistics() ->
    payload_protocol ! {get_stats, self()},
    receive
        {stats, Stats} -> Stats
    after 1000 -> #{error => timeout}
    end.

%% @doc 设置配置参数
-spec set_config(map()) -> ok.
set_config(Config) ->
    payload_protocol ! {set_config, Config},
    ok.

%% @doc 获取配置参数
-spec get_config(atom()) -> term().
get_config(Key) ->
    payload_protocol ! {get_config, self(), Key},
    receive
        {config, Value} -> Value
    after 1000 -> undefined
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

init() ->
    State = #state{
        start_time = erlang:system_time(millisecond)
    },
    loop(State).

loop(State) ->
    receive
        {process, From, Data} ->
            Result = handle_frame(Data, State),
            From ! {result, Result},
            NewState = update_statistics(Result, State),
            loop(NewState);

        {send_frames, Frames} ->
            send_frames_to_interface(Frames),
            NewState = State#state{
                fc_sequence = State#state.fc_sequence + length(Frames)
            },
            loop(NewState);

        {get_status, From} ->
            Status = #{
                mode => State#state.mode,
                fc_sequence => State#state.fc_sequence,
                pl_sequence => State#state.pl_sequence,
                dt_sequence => State#state.dt_sequence,
                frame_count => State#state.frame_count,
                error_count => State#state.error_count,
                uptime => erlang:system_time(millisecond) - State#state.start_time
            },
            From ! {status, Status},
            loop(State);

        {get_stats, From} ->
            Uptime = erlang:system_time(millisecond) - State#state.start_time,
            Stats = #{
                total_frames => State#state.frame_count,
                error_rate => case State#state.frame_count of
                    0 -> 0.0;
                    _ -> State#state.error_count / State#state.frame_count
                end,
                frame_rate => case Uptime of
                    0 -> 0.0;
                    _ -> State#state.frame_count / (Uptime / 1000)
                end,
                uptime_seconds => Uptime / 1000
            },
            From ! {stats, Stats},
            loop(State);

        {set_config, Config} ->
            NewState = apply_config(Config, State),
            loop(NewState);

        {get_config, From, Key} ->
            Value = get_config_value(Key, State),
            From ! {config, Value},
            loop(State);

        stop ->
            ?LOG(info, "Protocol processor stopping", []),
            ok;

        Unknown ->
            ?LOG(warning, "Unknown message: ~p", [Unknown]),
            loop(State)
    end.

handle_frame(Data, State) ->
    case classify_frame(Data) of
        fc_to_payload ->
            handle_fc_frame(Data, State);
        payload_to_fc ->
            handle_payload_frame(Data, State);
        data_terminal ->
            handle_dt_frame(Data, State);
        unknown ->
            {error, unknown_frame_type}
    end.

classify_frame(<<16#EB, 16#90, _/binary>>) -> fc_to_payload;
classify_frame(<<16#AA, 16#55, _/binary>>) -> payload_to_fc;
classify_frame(_) -> unknown.

handle_fc_frame(Data, State) ->
    case fc_to_payload:decode(Data) of
        {ok, Frame} ->
            _NewState = State#state{
                last_command = Frame#fc_to_payload.command_code,
                pl_sequence = State#state.pl_sequence + 1
            },
            execute_command(Frame),
            {ok, {fc_frame, Frame}};
        {error, Reason} ->
            {error, Reason}
    end.

handle_payload_frame(Data, State) ->
    case payload_to_fc:decode(Data) of
        {ok, Frame, Version} ->
            case payload_to_fc:validate_data_range(Frame) of
                ok ->
                    _NewState = State#state{
                        last_status = {Frame, Version},
                        fc_sequence = State#state.fc_sequence + 1
                    },
                    process_status(Frame, Version),
                    {ok, {payload_frame, Frame, Version}};
                {error, Field, Value, Range} ->
                    {error, {data_range_error, Field, Value, Range}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

handle_dt_frame(Data, State) ->
    case data_terminal:decode_frame(Data) of
        {ok, Frame} ->
            _NewState = State#state{
                dt_sequence = State#state.dt_sequence + 1
            },
            process_data_terminal(Frame),
            {ok, {dt_frame, Frame}};
        {error, Reason} ->
            {error, Reason}
    end.

execute_command(#fc_to_payload{command_code = Command} = Frame) ->
    case Command of
        ?CMD_PAYLOAD_WORK -> ?LOG(info, "Executing: Payload WORK", []);
        ?CMD_PAYLOAD_SLEEP -> ?LOG(info, "Executing: Payload SLEEP", []);
        ?CMD_VISIBLE_LIGHT -> ?LOG(info, "Executing: Switch to VISIBLE light", []);
        ?CMD_INFRARED -> ?LOG(info, "Executing: Switch to INFRARED", []);
        _ -> ?LOG(info, "Executing command: ~p", [Command])
    end,
    log_command(Command, Frame).

process_status(Frame, Version) ->
    case Version of
        basic ->
            Status0Info = payload_to_fc:parse_status0(Frame#payload_to_fc_basic.status0),
            Status1Info = payload_to_fc:parse_status1(Frame#payload_to_fc_basic.status1),
            log_status(Status0Info, Status1Info);
        extended ->
            Status0Info = payload_to_fc:parse_status0(Frame#payload_to_fc_extended.status0),
            Status1Info = payload_to_fc:parse_status1(Frame#payload_to_fc_extended.status1),
            log_status(Status0Info, Status1Info),
            AccelX = payload_to_fc:convert_accel(Frame#payload_to_fc_extended.accel_x, ?RES_ACCEL),
            GyroX = payload_to_fc:convert_rate(Frame#payload_to_fc_extended.gyro_x, ?RES_ANGLE_RATE_HIGH),
            ImuTemp = payload_to_fc:convert_imu_temp(Frame#payload_to_fc_extended.imu_temp),
            log_mems_data(AccelX, GyroX, ImuTemp)
    end.

process_data_terminal(#data_terminal_frame{data_type = DataType} = _Frame) ->
    case DataType band 16#0F of
        ?DATA_TYPE_COMPOSITE -> ?LOG(info, "Processing composite data", []);
        ?DATA_TYPE_VISIBLE_IMG -> ?LOG(info, "Processing visible light image", []);
        ?DATA_TYPE_INFRARED_IMG -> ?LOG(info, "Processing infrared image", []);
        _ -> ?LOG(info, "Processing unknown data type: ~p", [DataType])
    end.

update_statistics({ok, _}, State) ->
    State#state{frame_count = State#state.frame_count + 1};
update_statistics({error, _}, State) ->
    State#state{
        frame_count = State#state.frame_count + 1,
        error_count = State#state.error_count + 1
    }.

apply_config(Config, State) ->
    case maps:get(mode, Config, undefined) of
        undefined -> State;
        Mode -> State#state{mode = Mode}
    end.

get_config_value(mode, State) -> State#state.mode;
get_config_value(_, _) -> undefined.

send_frames_to_interface(Frames) ->
    lists:foreach(
        fun(Frame) ->
            ?LOG(info, "Sending frame: ~p bytes", [byte_size(Frame)])
        end,
        Frames
    ).

log_command(Command, _Frame) ->
    Timestamp = erlang:system_time(millisecond),
    CommandName = fc_to_payload:get_command_name(Command),
    ?LOG(info, "~p: Command ~s (0x~2.16.0B) executed",
         [Timestamp, CommandName, Command]).

log_status(Status0Info, Status1Info) ->
    ?LOG(info, "Status0: ~p, Status1: ~p", [Status0Info, Status1Info]).

log_mems_data(AccelX, GyroX, ImuTemp) ->
    ?LOG(info, "MEMS Data - AccelX: ~.2f m/s², GyroX: ~.3f °/s, Temp: ~.1f °C",
         [AccelX, GyroX, ImuTemp]).

%%%===================================================================
%%% 测试
%%%===================================================================

test() ->
    {ok, _} = start(),
    AircraftParams = data_terminal:default_aircraft_params_binary(), % fc_to_payload 模块中没有此函数，使用 data_terminal 模块中的函数
    {ok, Frames} = send_command(?CMD_PAYLOAD_WORK, #{aircraft_params => AircraftParams}, #{}),
    ?LOG(info, "Sent ~p frames", [length(Frames)]),
    Status = get_status(),
    ?LOG(info, "Protocol status: ~p", [Status]),
    Stats = get_statistics(),
    ?LOG(info, "Protocol statistics: ~p", [Stats]),
    stop(),
    ok.