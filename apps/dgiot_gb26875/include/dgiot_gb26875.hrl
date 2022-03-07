-define(DGIOT_GB26875_TCP_DTU, dgiot_gb26875_tcp_dtu).
-record(state, {
    id,
    devaddr = <<>>,
    len = 0,
    app = <<>>,
    product = <<>>,
    deviceId = <<>>,
    env = <<>>,
    dtutype = <<>>
}).

-define(GB26875, <<"gb26875">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 命令
-define(COMMAND_RESERVED(Name), lists:member(Name, [0 |lists:seq(7, 127)]).            % 预留
-define(COMMAND_CONTROL, 1).            % 控制命令     时间同步
-define(COMMAND_SEND_DATA, 2).          % 发送数据     发送火灾报警和建筑消防设施运行状态等信息
-define(COMMAND_CONFIRM, 3).            % 确认        对控制命令和发送信息的确认回答
-define(COMMAND_REQUEST, 4).            % 请求        查询火灾报警和建筑消防设施运行状态等信息
-define(COMMAND_RESPONSE, 5).           % 应答        返回查询的信息
-define(COMMAND_DENY, 6).               % 否认        对控制命令和发送信息的否认回答

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 类型标志定义表 8.1.1
%% 类型标志为l字节二进制数，取值范围0～255，类型标志见表3。
-define(CLASS_UP_SYSTEM_STATE, 1).        %  8．2．1．1 4个字节, 上传建筑消防设施系统状态
-define(CLASS_UP_RUNNING_STATUS, 2).          % 上传建筑消防设施部件运行状态
-define(CLASS_UP_ANALOG_QUANTITY, 3).            % 上传建筑消防设施部件模拟量值
-define(CLASS_UP_OPERATING_INFORMATION, 4).            % 上传建筑消防设施操作信息
-define(CLASS_UP_SOFTWARE_VERSION, 5).           % 上传建筑消防设施软件版本
-define(CLASS_UP_SYSTEM_CONFIGURATION, 6).               % 上传建筑消防设施系统配置情况
-define(CLASS_UP_PARTS_CONFIGURATION, 7).               % 上传建筑消防设施部件配置情况
-define(CLASS_UP_SYSTEM_TIME, 8).               % 上传建筑消防设施系统时间
-define(CLASS_UP_RESERVED(Name), lists:member(Name, [22, 23, 27 | lists:seq(9, 20)] | lists:seq(29, 60)] ).
-define(CLASS_UP_USERDEV_RUNINFO, 21).               % 上传用户信息传输装置运行状态
-define(CLASS_UP_USERDEV_OPINFO, 24).               % 上传用户信息传输装置操作信息
-define(CLASS_UP_USERDEV_SOFTVER, 25).               % 上传用户信息传输装置软件版本
-define(CLASS_UP_USERDEV_CONFIG, 26).               % 上传用户信息传输装置配置情况
-define(CLASS_UP_USERDEV_SYSTIME, 28).               % 上传用户信息传输装置系统时间

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 建筑消防设施系统状态 8．2．1．1
%% 系统类型定义表
-define(SYS_TYPE_COMMOM , 0).        %  通用
-define(SYS_TYPE_FIRE_ALARM, 0).     %  火灾报警系统