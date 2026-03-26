%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_stub_functions - 无人机系统存根函数模块
%%%
%%% 为编译警告中的未定义函数提供存根实现，确保系统可以正常编译和运行。
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_stub_functions).

%% API导出
-export([load_product/1]).
-export([get_writeData/2]).
-export([get_command_example/1]).
-export([send_command/6]).
-export([get_station_commands/1]).
-export([build_coil_command/3]).
-export([execute_test_item/3]).
-export([load_test_items_by_station/1]).
-export([get_station_by_fixture_addr/1]).
-export([delete/1]).
-export([test_alarm_action/2]).
-export([setsockopt/5]).
-export([ipv6_probe/0]).
-export([merge_cache_data/3]).
-export([save_cache_data/2]).
-export([default_aircraft_params_binary/0]).
-export([bind_qrcode_to_drone/3]).
-export([convert_link_to_thing_model/1]).
-export([save_td_data/2]).

-include_lib("dgiot/include/logger.hrl").

%%%===================================================================
%%% 存根函数实现
%%%===================================================================

%% @doc 加载产品信息存根
-spec load_product(ProductId :: binary()) -> {ok, map()} | {error, term()}.
load_product(ProductId) ->
    ?LOG(info, "存根: dgiot_product:load_product(~p)", [ProductId]),
    {ok, #{<<"product_id">> => ProductId, <<"product_name">> => <<"测试产品">>}}.

%% @doc 获取写数据存根
-spec get_writeData(binary(), term()) -> {ok, binary()} | {error, term()}.
get_writeData(DeviceId, Data) ->
    ?LOG(info, "存根: dgiot_edge:get_writeData(~p, ~p)", [DeviceId, Data]),
    {ok, <<"write_data_stub">>}.

%% @doc 获取命令示例存根
-spec get_command_example(binary()) -> {ok, binary()} | {error, term()}.
get_command_example(CommandType) ->
    ?LOG(info, "存根: command_examples:get_command_example(~p)", [CommandType]),
    {ok, <<"command_example_stub">>}.

%% @doc 发送命令存根
-spec send_command(binary(), integer(), binary(), integer(), integer(), term()) -> 
    {ok, binary()} | {error, term()}.
send_command(DeviceId, StationId, CommandType, Addr, Value, Extra) ->
    ?LOG(info, "存根: dgiot_uav_command_unified:send_command(~p, ~p, ~p, ~p, ~p, ~p)", 
         [DeviceId, StationId, CommandType, Addr, Value, Extra]),
    {ok, <<"command_sent_stub">>}.

%% @doc 获取工位命令存根
-spec get_station_commands(StationId :: integer()) -> {ok, list()} | {error, term()}.
get_station_commands(StationId) ->
    ?LOG(info, "存根: dgiot_uav_business_service:get_station_commands(~p)", [StationId]),
    {ok, []}.

%% @doc 构建线圈命令存根
-spec build_coil_command(binary(), integer(), integer()) -> {ok, binary()} | {error, term()}.
build_coil_command(DeviceId, Addr, Value) ->
    ?LOG(info, "存根: dgiot_fixture_controller:build_coil_command(~p, ~p, ~p)", [DeviceId, Addr, Value]),
    {ok, <<"coil_command_stub">>}.

%% @doc 执行测试项存根
-spec execute_test_item(binary(), integer(), term()) -> {ok, map()} | {error, term()}.
execute_test_item(DeviceId, TestItemId, Params) ->
    ?LOG(info, "存根: dgiot_uav_test_item_service:execute_test_item(~p, ~p, ~p)", 
         [DeviceId, TestItemId, Params]),
    {ok, #{<<"result">> => <<"test_completed">>}}.

%% @doc 加载工位测试项存根
-spec load_test_items_by_station(StationId :: integer()) -> {ok, list()} | {error, term()}.
load_test_items_by_station(StationId) ->
    ?LOG(info, "存根: dgiot_uav_test_item_service:load_test_items_by_station(~p)", [StationId]),
    {ok, []}.

%% @doc 通过治具地址获取工位存根
-spec get_station_by_fixture_addr(FixtureAddr :: integer()) -> {ok, integer()} | {error, term()}.
get_station_by_fixture_addr(FixtureAddr) ->
    ?LOG(info, "存根: dgiot_uav_plc_channel:get_station_by_fixture_addr(~p)", [FixtureAddr]),
    %% 简化映射：治具地址模4+1
    StationId = (FixtureAddr rem 4) + 1,
    {ok, StationId}.

%% @doc 删除通道存根
-spec delete(ChannelId :: binary()) -> ok | {error, term()}.
delete(ChannelId) ->
    ?LOG(info, "存根: dgiot_channelx:delete(~p)", [ChannelId]),
    ok.

%% @doc 测试告警动作存根
-spec test_alarm_action(AlarmId :: binary(), Action :: binary()) -> {ok, map()} | {error, term()}.
test_alarm_action(AlarmId, Action) ->
    ?LOG(info, "存根: dgiot_uav_alarm_manager:test_alarm_action(~p, ~p)", [AlarmId, Action]),
    {ok, #{<<"result">> => <<"alarm_action_completed">>}}.

%% @doc 设置socket选项存根
-spec setsockopt(term(), term(), term(), term(), term()) -> ok | {error, term()}.
setsockopt(Socket, Level, Opt, Value, Timeout) ->
    ?LOG(info, "存根: prim_inet:setsockopt(~p, ~p, ~p, ~p, ~p)", [Socket, Level, Opt, Value, Timeout]),
    ok.

%% @doc IPv6探测存根
-spec ipv6_probe() -> boolean().
ipv6_probe() ->
    ?LOG(info, "存根: gen_tcp:ipv6_probe()"),
    false.

%% @doc 合并缓存数据存根
-spec merge_cache_data(DeviceId :: binary(), Data :: map(), Opts :: map()) -> 
    {ok, map()} | {error, term()}.
merge_cache_data(DeviceId, Data, Opts) ->
    ?LOG(info, "存根: dgiot_task:merge_cache_data(~p, ~p, ~p)", [DeviceId, Data, Opts]),
    {ok, Data}.

%% @doc 保存缓存数据存根
-spec save_cache_data(DeviceId :: binary(), Data :: map()) -> ok | {error, term()}.
save_cache_data(DeviceId, Data) ->
    ?LOG(info, "存根: dgiot_task:save_cache_data(~p, ~p)", [DeviceId, Data]),
    ok.

%% @doc 获取默认飞机参数二进制存根
-spec default_aircraft_params_binary() -> binary().
default_aircraft_params_binary() ->
    ?LOG(info, "存根: fc_to_payload:default_aircraft_params_binary()"),
    <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>.

%% @doc 绑定二维码到无人机存根
-spec bind_qrcode_to_drone(Qrcode :: binary(), DroneId :: binary(), StationId :: integer()) -> 
    {ok, map()} | {error, term()}.
bind_qrcode_to_drone(Qrcode, DroneId, StationId) ->
    ?LOG(info, "存根: dgiot_scanner_protocol:bind_qrcode_to_drone(~p, ~p, ~p)", 
         [Qrcode, DroneId, StationId]),
    {ok, #{<<"qrcode">> => Qrcode, <<"drone_id">> => DroneId, <<"station_id">> => StationId}}.

%% @doc 转换链路到物模型存根
-spec convert_link_to_thing_model(LinkData :: map()) -> {ok, map()} | {error, term()}.
convert_link_to_thing_model(LinkData) ->
    ?LOG(info, "存根: uav_thing_model:convert_link_to_thing_model(~p)", [LinkData]),
    ThingModel = #{
        <<"properties">> => [],
        <<"services">> => [],
        <<"events">> => []
    },
    {ok, ThingModel}.

%% @doc 保存TDengine数据存根
-spec save_td_data(DeviceId :: binary(), Data :: map()) -> ok | {error, term()}.
save_td_data(DeviceId, Data) ->
    ?LOG(info, "存根: dgiot_tdengine:save(~p, ~p)", [DeviceId, Data]),
    ok.