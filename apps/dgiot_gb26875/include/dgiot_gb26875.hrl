-define(DGIOT_GB26875_TCP_DTU, dgiot_gb26875_tcp_dtu).
-record(state, {
    id,
    devaddr = <<>>,
    heartcount = 0,
    regtype = <<>>,
    head = "xxxxxx0eee",
    len = 0,
    app = <<>>,
    product = <<>>,
    deviceId = <<>>,
    scale = 10,
    temperature = 0,
    env = <<>>,
    dtutype = <<>>
}).

-define(GB26875, <<"gb26875">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 命令
-define(COMMAND_RESERVED, 0).            % 预留
-define(COMMAND_CONTROL, 1).            % 控制命令     时间同步
-define(COMMAND_SEND_DATA, 2).          % 发送数据     发送火灾报警和建筑消防设施运行状态等信息
-define(COMMAND_CONFIRM, 3).            % 确认        对控制命令和发送信息的确认回答
-define(COMMAND_REQUEST, 4).            % 请求        查询火灾报警和建筑消防设施运行状态等信息
-define(COMMAND_RESPONSE, 5).           % 应答        返回查询的信息
-define(COMMAND_DENY, 6).               % 否认        对控制命令和发送信息的否认回答

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 类型标志定义表 8.1.1
-define(TYPE_RESERVED(Name), lists:member(Name, [0 |lists:seq(26, 29)| lists:seq(45, 49)| lists:seq(54, 59)| lists:seq(63, 68)| lists:seq(70, 73)| lists:seq(75, 77)])).   % 预留
-define(TYPE_UP_SYSTEM_STATE, 1).        %  8．2．1．1 4个字节, 上传建筑消防设施系统状态
-define(TYPE_UP_RUNNING_STATUS, 2).          % 上传建筑消防设施部件运行状态
-define(TYPE_UP_ANALOG_QUANTITY, 3).            % 上传建筑消防设施部件模拟量值
-define(TYPE_UP_OPERATING_INFORMATION, 4).            % 上传建筑消防设施操作信息
-define(TYPE_UP_SOFTWARE_VERSION, 5).           % 上传建筑消防设施软件版本
-define(TYPE_UP_SYSTEM_CONFIGURATION, 6).               % 上传建筑消防设施系统配置情况
-define(TYPE_UP_PARTS_CONFIGURATION, 7).               % 上传建筑消防设施部件配置情况
-define(TYPE_UP_SYSTEM_TIME, 8).               % 上传建筑消防设施系统时间
