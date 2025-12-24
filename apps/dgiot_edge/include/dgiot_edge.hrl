-define(dgiot_edge_TCP_DTU, dgiot_edge_tcp_dtu_ets).

-record(termios, {
    iflag = 0,
    oflag = 0,
    cflag = 0,
    lflag = 0,
    line = 0,
    cc = <<>>,
    ispeed = serctl:constant(b9600),
    ospeed = serctl:constant(b9600)
}).

-record(state, {
    % Original termios attributes
    oattr,
    serialport,
    port,
    % PID of controlling process
    pid,
    % serial dev file descriptor
    fd,
    % device name
    dev,
    speed,
    % interval (Unit: millisecond)
    interval = 50 :: integer(), %% b2400
    %% Timestamp (Unit: millisecond) 收包时间
    timestamp = 0 :: integer(),
    %% Message from
    data = <<>> :: binary(),
    %% 累计收包数
    package_recv_count = 0 :: integer(),
    %% 累计收包字节数
    package_recv_bytes = 0 :: integer(),
    %% 累计发包数
    package_send_count = 0 :: integer(),
    %% 累计发包字节数
    package_send_bytes = 0 :: integer(),
    ref = undefined,
    env
}).

-record(task, {freq = 0 :: integer(), deviceid = <<>>, serialport = <<>>, data = <<>>, count = 0 :: integer(), bytes = 0 :: integer()}).
