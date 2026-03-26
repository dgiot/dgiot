%%%-------------------------------------------------------------------
%%% @doc
%%% uav_protocol_utils 模块 - 无人机协议实用函数库
%%% 包含无人机指令集的中文名称映射（根据用户提供的表格）
%%% @end
%%%-------------------------------------------------------------------
-module(uav_protocol_utils).

-export([
    parse_platform_type/1,
    switch_command_str/1,
    waypoint_str/1,
    payload_sub_command_str/1,
    payload_continuous_command_str/1,
    rudder_channel_str/1,
    cmd_code_to_bytes/1,
    remote_command_name/1
]).

%% 平台类型解析
parse_platform_type(0) -> <<"未知平台"/utf8>>;
parse_platform_type(1) -> <<"固定翼"/utf8>>;
parse_platform_type(2) -> <<"旋翼机"/utf8>>;
parse_platform_type(3) -> <<"垂直起降"/utf8>>;
parse_platform_type(_) -> <<"其他平台"/utf8>>.

%% 开关命令字符串（根据用户提供的指令集）
switch_command_str(16#A2) -> <<"筒内模式"/utf8>>;
switch_command_str(16#E6) -> <<"起飞模式"/utf8>>;
switch_command_str(16#3D) -> <<"定高模式"/utf8>>;
switch_command_str(16#E9) -> <<"导航模式"/utf8>>;
switch_command_str(16#51) -> <<"返航模式"/utf8>>;
switch_command_str(16#B8) -> <<"攻击模式"/utf8>>;
switch_command_str(16#B9) -> <<"复飞模式"/utf8>>;
switch_command_str(16#3F) -> <<"关闭电机"/utf8>>;
switch_command_str(16#39) -> <<"高度+"/utf8>>;
switch_command_str(16#3B) -> <<"高度-"/utf8>>;
switch_command_str(16#61) -> <<"配平油门+"/utf8>>;
switch_command_str(16#63) -> <<"配平油门-"/utf8>>;
switch_command_str(16#C2) -> <<"配平副翼+"/utf8>>;
switch_command_str(16#C3) -> <<"配平副翼-"/utf8>>;
switch_command_str(16#C4) -> <<"配平升降+"/utf8>>;
switch_command_str(16#C5) -> <<"配平升降-"/utf8>>;
switch_command_str(16#C6) -> <<"左偏"/utf8>>;
switch_command_str(16#C7) -> <<"右偏"/utf8>>;
switch_command_str(16#65) -> <<"空速上调"/utf8>>;
switch_command_str(16#67) -> <<"空速下调"/utf8>>;
switch_command_str(16#AE) -> <<"航向校准开"/utf8>>;
switch_command_str(16#AF) -> <<"航向校准关"/utf8>>;
switch_command_str(16#A1) -> <<"空速故障"/utf8>>;
switch_command_str(16#6A) -> <<"镇定+"/utf8>>;
switch_command_str(16#6B) -> <<"镇定-"/utf8>>;
switch_command_str(16#01) -> <<"顺序飞行"/utf8>>;
switch_command_str(16#02) -> <<"倒序飞行"/utf8>>;
switch_command_str(16#FC) -> <<"航线查询"/utf8>>;
switch_command_str(16#F5) -> <<"原点查询"/utf8>>;
switch_command_str(16#FD) -> <<"ET查询"/utf8>>;
switch_command_str(16#C1) -> <<"软件版本查询"/utf8>>;
switch_command_str(16#CD) -> <<"攻击查询"/utf8>>;
switch_command_str(16#D7) -> <<"电池激活"/utf8>>;
switch_command_str(16#A7) -> <<"导引头上电"/utf8>>;
switch_command_str(16#A8) -> <<"导引头断电"/utf8>>;
switch_command_str(16#CA) -> <<"引信上电"/utf8>>;
switch_command_str(16#CB) -> <<"引信断电"/utf8>>;
switch_command_str(16#B0) -> <<"发射点火"/utf8>>;
switch_command_str(16#B1) -> <<"引信自毁"/utf8>>;
switch_command_str(16#B2) -> <<"引信电容充电"/utf8>>;
switch_command_str(16#B4) -> <<"引信电容放电"/utf8>>;
switch_command_str(16#F3) -> <<"舵面使能"/utf8>>;
switch_command_str(16#FB) -> <<"舵面中位"/utf8>>;
switch_command_str(16#31) -> <<"左右舵上偏5"/utf8>>;
switch_command_str(16#33) -> <<"左右舵下偏5"/utf8>>;
switch_command_str(16#32) -> <<"左右舵上偏10"/utf8>>;
switch_command_str(16#34) -> <<"左右舵下偏10"/utf8>>;
switch_command_str(16#36) -> <<"左右舵上偏15"/utf8>>;
switch_command_str(16#35) -> <<"左右舵下偏15"/utf8>>;
switch_command_str(16#45) -> <<"动力试车使能"/utf8>>;
switch_command_str(16#47) -> <<"电机起转"/utf8>>;
switch_command_str(16#4A) -> <<"平飞油门"/utf8>>;
switch_command_str(16#49) -> <<"爬升油门"/utf8>>;
switch_command_str(16#4C) -> <<"动力试车关闭"/utf8>>;
switch_command_str(_) -> <<"未知命令"/utf8>>.

%% 航点字符串
waypoint_str(0) -> <<"起飞点"/utf8>>;
waypoint_str(1) -> <<"航点1"/utf8>>;
waypoint_str(2) -> <<"航点2"/utf8>>;
waypoint_str(3) -> <<"航点3"/utf8>>;
waypoint_str(4) -> <<"航点4"/utf8>>;
waypoint_str(5) -> <<"航点5"/utf8>>;
waypoint_str(6) -> <<"航点6"/utf8>>;
waypoint_str(7) -> <<"航点7"/utf8>>;
waypoint_str(8) -> <<"航点8"/utf8>>;
waypoint_str(9) -> <<"航点9"/utf8>>;
waypoint_str(10) -> <<"航点10"/utf8>>;
waypoint_str(11) -> <<"航点11"/utf8>>;
waypoint_str(12) -> <<"航点12"/utf8>>;
waypoint_str(13) -> <<"航点13"/utf8>>;
waypoint_str(14) -> <<"航点14"/utf8>>;
waypoint_str(15) -> <<"航点15"/utf8>>;
waypoint_str(16) -> <<"航点16"/utf8>>;
waypoint_str(17) -> <<"航点17"/utf8>>;
waypoint_str(18) -> <<"航点18"/utf8>>;
waypoint_str(19) -> <<"航点19"/utf8>>;
waypoint_str(20) -> <<"航点20"/utf8>>;
waypoint_str(_) -> <<"未知航点"/utf8>>.

%% 载荷子命令字符串
payload_sub_command_str(16#18) -> <<"手动跟踪"/utf8>>;
payload_sub_command_str(16#19) -> <<"数引"/utf8>>;
payload_sub_command_str(16#B1) -> <<"自动跟踪"/utf8>>;
payload_sub_command_str(16#C9) -> <<"扫描"/utf8>>;
payload_sub_command_str(16#CA) -> <<"变倍放大"/utf8>>;
payload_sub_command_str(16#56) -> <<"变倍缩小"/utf8>>;
payload_sub_command_str(16#A7) -> <<"导引头上电"/utf8>>;
payload_sub_command_str(16#A8) -> <<"导引头断电"/utf8>>;
payload_sub_command_str(_) -> <<"未知载荷命令"/utf8>>.

%% 载荷连续命令字符串
payload_continuous_command_str(16#18) -> <<"手动跟踪"/utf8>>;
payload_continuous_command_str(16#19) -> <<"数引"/utf8>>;
payload_continuous_command_str(16#B1) -> <<"自动跟踪"/utf8>>;
payload_continuous_command_str(16#C9) -> <<"扫描"/utf8>>;
payload_continuous_command_str(_) -> <<"未知连续命令"/utf8>>.

%% 舵机通道字符串
rudder_channel_str(1) -> <<"左前舵"/utf8>>;
rudder_channel_str(2) -> <<"右前舵"/utf8>>;
rudder_channel_str(3) -> <<"左垂尾"/utf8>>;
rudder_channel_str(4) -> <<"右垂尾"/utf8>>;
rudder_channel_str(_) -> <<"未知通道"/utf8>>.

%% @doc 将命令码转换为字节表示
%% 根据错误信息，需要返回 {ok, {CmdH, CmdL}}
-spec cmd_code_to_bytes(CmdCode :: integer()) -> {ok, {integer(), integer()}} | {error, term()}.
cmd_code_to_bytes(CmdCode) when is_integer(CmdCode) ->
    %% 将16位命令码拆分为高字节和低字节
    CmdH = (CmdCode bsr 8) band 16#FF,
    CmdL = CmdCode band 16#FF,
    {ok, {CmdH, CmdL}};
cmd_code_to_bytes(_) ->
    {error, invalid_cmd_code}.

%% @doc 获取遥控指令中文名称
-spec remote_command_name(Code :: integer()) -> binary().
remote_command_name(Code) ->
    %% 委托给 switch_command_str/1 函数
    switch_command_str(Code).