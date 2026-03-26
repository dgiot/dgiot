%%--------------------------------------------------------------------
%% @doc 噪音传感器协议处理模块
%%--------------------------------------------------------------------
-module(dgiot_noise_protocol).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_socket.hrl").
-include_lib("dgiot_uav/include/dgiot_uav.hrl").

-export([handle_tcp_data/1, handle_port_data/4]).

%% 注册报文
-define(REG_NOISE1, <<"noise_sensor1\n">>).
-define(REG_NOISE2, <<"noise_sensor2\n">>).

%% 浮点数读取需要的最小字节数
-define(MIN_FLOAT_BYTES, 4).

%% 功能码
-define(FC_READ_HOLDING, 16#03).

%%====================================================================
%% TCP数据处理
%%====================================================================
handle_tcp_data(Data) when byte_size(Data) >= 4 ->
    %% 正确的二进制模式匹配：未指定大小的字段必须在末尾
    case Data of
        <<Value:32/float, Rest/binary>> ->
            process_noise_data(Value),
            {ok, Rest};
        _ ->
            {error, invalid_format}
    end;
handle_tcp_data(_) ->
    {error, incomplete}.

process_noise_data(Value) ->
    DevAddr = get(devaddr),
    ProductId = get(product_id),
    Timestamp = erlang:system_time(millisecond),
    Data = #{<<"noise">> => Value},

    %% 聚合数据
    case whereis(dgiot_uav_aggregator) of
        undefined ->
            dgiot_uav_aggregator:start_link();
        _ -> ok
    end,
    dgiot_uav_aggregator:aggregate(DevAddr, ProductId, Data, Timestamp),

    %% 转发到无人机
    forward_to_drone(DevAddr, Data),

    ?LOG(debug, "[NOISE] 噪声值:~f", [Value]).

forward_to_drone(DevAddr, Data) ->
    case extract_ip(DevAddr) of
        {ok, IpBin} ->
            dgiot_uav_business_service:send_aggregate_to_drone(IpBin, Data);
        {error, _} -> ok
    end.

extract_ip(DevAddr) ->
    case binary:split(DevAddr, <<"_">>) of
        [IpBin, _PortBin] -> {ok, IpBin};
        _ -> {error, invalid}
    end.

%%====================================================================
%% 端口数据处理
%%====================================================================
-spec handle_port_data(binary(), #tcp{}, #uav_state{}, inet:socket()) ->
    {#tcp{}, #uav_state{}, noreply}.
handle_port_data(NewBuf, TCPState, UavState, Socket) ->
    case NewBuf of
        ?REG_NOISE1 ->
            handle_registration(TCPState, UavState, Socket, NewBuf);
        ?REG_NOISE2 ->
            handle_registration(TCPState, UavState, Socket, NewBuf);
        _ ->
            handle_sensor_data(NewBuf, TCPState, UavState, Socket)
    end.

handle_registration(TCPState, UavState, Socket, Rest) ->
    ?LOG(info, "[NOISE] 噪音传感器注册"),
    NewTCPState = TCPState#tcp{
        clientid = UavState#uav_state.device_id,
        buff = extract_rest(Rest)
    },
    inet:setopts(Socket, [{active, once}]),
    {NewTCPState, UavState, noreply}.

handle_sensor_data(NewBuf, TCPState, UavState, Socket) ->
    case byte_size(NewBuf) of
        Size when Size >= 8 ->
            %% 检查是否为Modbus响应
            case NewBuf of
                <<_SlaveId:8, FuncCode:8, _RemainingData/binary>> when FuncCode =:= ?FC_READ_HOLDING ->
                    case handle_tcp_data(NewBuf) of
                        {ok, _} -> ok;
                        _ -> ok
                    end;
                _ ->
                    ?LOG(debug, "[NOISE] 非Modbus数据，长度:~p", [byte_size(NewBuf)])
            end,
            inet:setopts(Socket, [{active, once}]),
            {TCPState#tcp{buff = <<>>}, UavState, noreply};
        _ ->
            %% 数据不完整，等待更多数据
            inet:setopts(Socket, [{active, once}]),
            {TCPState#tcp{buff = NewBuf}, UavState, noreply}
    end.

%%====================================================================
%% 辅助函数
%%====================================================================
extract_rest(Bin) when is_binary(Bin) ->
    %% 提取注册报文后的剩余数据
    case Bin of
        ?REG_NOISE1 ->
            % 完全匹配注册报文，返回空二进制
            <<>>;
        ?REG_NOISE2 ->
            % 完全匹配注册报文，返回空二进制
            <<>>;
        _ ->
            % 非注册报文，保持原样
            Bin
    end.
