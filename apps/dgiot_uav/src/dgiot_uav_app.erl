%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_app 模块 - 无人机测试应用启动模块
%%% 
%%% 负责启动无人机测试系统的监控树和初始化工作
%%%-------------------------------------------------------------------
-module(dgiot_uav_app).
-author("johnliu").

-behaviour(application).
-emqx_plugin(?MODULE).

-export([start/2, stop/1,
         %% 设备查询便捷函数
         find_device/1,
         find_device/2,
         get_device/1,
         list_devices/0,
         list_devices/1,
         list_products/0,
         find_product/1,
         get_device_status/1,
         get_device_ip/1,
         update_device_status/2,
         %% 工位设备查询
         get_station_device/1,
         list_station_devices/1,
         list_online_devices/0,
         list_offline_devices/0,
         count_devices/0,
         %% 中文便捷函数
         station_name/1,
         find_device_by_station_num/1,
         %% 修复函数
         fix_device_names/0,
         rebuild_test_devices/0,
         create_test_items/0]).

-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot.hrl").
-include("dgiot_uav.hrl").

%% 应用启动
start(_StartType, _StartArgs) ->
    ?LOG(info, "启动无人机测试应用"),

    %% 初始化测试项派发器ETS表
    %% TODO: dgiot_uav_test_dispatcher:start() - 模块不存在，暂时注释
    %% dgiot_uav_test_dispatcher:start(),

    %% 初始化工位管理器ETS表（必须在通道启动前初始化）
    ?LOG(info, "初始化工位管理器ETS表"),
    dgiot_uav_station_manager:init_ets(),

    %% 初始化业务ETS表（包括uav_station_plc等）
    ?LOG(info, "初始化业务ETS表"),
    dgiot_uav_business_service:init_ets(),

    %% 启动监控树
    case dgiot_uav_sup:start_link() of
        {ok, Pid} ->
            ?LOG(info, "无人机测试应用启动成功"),
            {ok, Pid};
        Error ->
            ?LOG(error, "无人机测试应用启动失败: ~p", [Error]),
            Error
    end.

%% 应用停止
stop(_State) ->
    ?LOG(info, "停止无人机测试应用"),
    ok.

%%--------------------------------------------------------------------
%% 设备查询便捷函数
%%--------------------------------------------------------------------

%% @doc 根据设备名称查询设备
find_device(Name) when is_binary(Name) ->
    find_device(<<"Device">>, Name);
find_device(Name) when is_list(Name) ->
    find_device(<<Name>>).

find_device(Class, Name) when is_binary(Name) ->
    case dgiot_parse:query_object(Class, #{<<"where">> => #{<<"name">> => Name}, <<"limit">> => 1}) of
        {ok, #{<<"results">> := []}} ->
            {error, not_found};
        {ok, #{<<"results">> := [Device | _]}} ->
            {ok, Device};
        Error ->
            Error
    end.

%% @doc 查询所有设备
list_devices() ->
    list_devices(10).

list_devices(Limit) ->
    dgiot_parse:query_object(<<"Device">>, #{<<"limit">> => Limit}).

%% @doc 查询所有产品
list_products() ->
    dgiot_parse:query_object(<<"Product">>, #{<<"limit">> => 100}).

%% @doc 根据产品名称查询产品
find_product(Name) when is_binary(Name) ->
    case dgiot_parse:query_object(<<"Product">>, #{<<"where">> => #{<<"name">> => Name}, <<"limit">> => 1}) of
        {ok, #{<<"results">> := []}} ->
            {error, not_found};
        {ok, #{<<"results">> := [Product | _]}} ->
            {ok, Product};
        Error ->
            Error
    end.

%% @doc 根据ObjectId获取设备详情
get_device(ObjectId) when is_binary(ObjectId) ->
    dgiot_parse:get_object(<<"Device">>, ObjectId);
get_device(ObjectId) when is_list(ObjectId) ->
    get_device(<<ObjectId>>).

%% @doc 获取设备状态
get_device_status(ObjectId) ->
    case get_device(ObjectId) of
        {ok, Device} ->
            Status = maps:get(<<"status">>, Device, <<"UNKNOWN">>),
            {ok, Status};
        Error ->
            Error
    end.

%% @doc 获取设备IP地址
get_device_ip(ObjectId) ->
    case get_device(ObjectId) of
        {ok, Device} ->
            Ip = maps:get(<<"ip">>, Device, <<>>),
            {ok, Ip};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% 工位设备查询
%%--------------------------------------------------------------------

%% @doc 根据工位号查询设备（如1=总测1, 2=总测2）
get_station_device(StationNum) when is_integer(StationNum), StationNum > 0, StationNum =< 7 ->
    StationName = <<"总测", (list_to_binary(integer_to_list(StationNum)))/binary>>,
    find_device_by_station(StationName);
get_station_device(StationNum) when is_integer(StationNum) ->
    {error, invalid_station_number}.

%% 根据设备名称关键字模糊查询
find_device_by_station(StationName) ->
    case list_devices(50) of
        {ok, #{<<"results">> := Devices}} ->
            Filtered = [D || D <- Devices,
                case maps:get(<<"name">>, D, undefined) of
                    undefined -> false;
                    Name -> binary:match(Name, StationName) =/= nomatch
                end
            ],
            case Filtered of
                [] -> {error, not_found};
                _ -> {ok, Filtered}
            end;
        Error ->
            Error
    end.

%% @doc 根据工位号列出该工位的所有设备
list_station_devices(StationNum) ->
    get_station_device(StationNum).

%% @doc 查询所有在线设备
list_online_devices() ->
    case dgiot_parse:query_object(<<"Device">>,
        #{<<"where">> => #{<<"status">> => <<"ONLINE">>},
         <<"limit">> => 100}) of
        {ok, Result} ->
            {ok, Result};
        Error ->
            Error
    end.

%% @doc 查询所有离线设备
list_offline_devices() ->
    case dgiot_parse:query_object(<<"Device">>,
        #{<<"where">> => #{<<"status">> => <<"OFFLINE">>},
         <<"limit">> => 100}) of
        {ok, Result} ->
            {ok, Result};
        Error ->
            Error
    end.

%% @doc 统计设备数量
count_devices() ->
    case dgiot_parse:query_object(<<"Device">>, #{<<"limit">> => 1000}) of
        {ok, #{<<"results">> := Devices}} ->
            Total = length(Devices),
            Online = length([D || D <- Devices, maps:get(<<"status">>, D, <<>>) =:= <<"ONLINE">>]),
            Offline = Total - Online,
            {ok, #{total => Total, online => Online, offline => Offline}};
        Error ->
            Error
    end.

%% @doc 更新设备状态
update_device_status(ObjectId, Status) when is_binary(ObjectId), is_binary(Status) ->
    dgiot_parse:update_object(<<"Device">>, ObjectId, #{<<"status">> => Status});
update_device_status(ObjectId, Status) when is_list(ObjectId) ->
    update_device_status(<<ObjectId>>, Status).

%%--------------------------------------------------------------------
%% 中文便捷函数（解决emqx eval中文参数问题）
%%--------------------------------------------------------------------

%% @doc 生成工位名称（二进制），参数1-7对应"总测1"-"总测7"
station_name(1) -> <<230,128,187,230,181,139,49>>;      % 总测1
station_name(2) -> <<230,128,187,230,181,139,50>>;      % 总测2
station_name(3) -> <<230,128,187,230,181,139,51>>;      % 总测3
station_name(4) -> <<230,128,187,230,181,139,52>>;      % 总测4
station_name(5) -> <<230,128,187,230,181,139,53>>;      % 总测5
station_name(6) -> <<230,128,187,230,181,139,54>>;      % 总测6
station_name(7) -> <<230,128,187,230,181,139,55>>;      % 总测7
station_name(N) when is_integer(N), N > 0 -> station_name(1).

%% @doc 根据工位号（1-7）查询设备（模糊匹配devaddr）
find_device_by_station_num(N) when is_integer(N), N >= 1, N =< 7 ->
    Prefix = station_name(N),
    case list_devices(50) of
        {ok, #{<<"results">> := Devices}} ->
            Filtered = [D || D <- Devices,
                case maps:get(<<"devaddr">>, D, undefined) of
                    undefined -> false;
                    DevAddr -> binary:match(DevAddr, Prefix) =/= nomatch
                end
            ],
            case Filtered of
                [] -> {error, not_found};
                _ -> {ok, Filtered}
            end;
        Error ->
            Error
    end;
find_device_by_station_num(_) ->
    {error, invalid_station_number}.

%%--------------------------------------------------------------------
%% 修复函数
%%--------------------------------------------------------------------

%% @doc 修复测试项设备名称：去掉"试项"或"测试项"后缀
fix_device_names() ->
    io:format("开始修复设备名称...~n"),
    case dgiot_parse:query_object(<<"Device">>, #{
        <<"where">> => #{
            <<"product">> => #{
                <<"__type">> => <<"Pointer">>, 
                <<"className">> => <<"Product">>, 
                <<"objectId">> => <<"343cf21f82">>
            }
        },
        <<"limit">> => 100,
        <<"keys">> => <<"name,devaddr,objectId">>
    }) of
        {ok, #{<<"results">> := Results}} ->
            io:format("查询到 ~p 个设备~n", [length(Results)]),
            % "试项"的UTF8编码: 232,175,149,233,161,185 (6字节)
            % "测试项"的UTF8编码: 230,181,139,232,175,149,233,161,185 (9字节)
            Fix = fun(D) ->
                ObjectId = maps:get(<<"objectId">>, D, <<>>),
                Name = maps:get(<<"name">>, D, <<>>),
                DevAddr = maps:get(<<"devaddr">>, D, <<>>),
                NameLen = byte_size(Name),
                case NameLen >= 6 of
                    true ->
                        Suffix = binary:part(Name, NameLen - 6, 6),
                        NewName = case Suffix of
                            %% 匹配"试项"(6字节)
                            <<232,175,149,233,161,185>> -> 
                                binary:part(Name, 0, NameLen - 6);
                            _ ->
                                case NameLen >= 9 of
                                    true ->
                                        Suffix9 = binary:part(Name, NameLen - 9, 9),
                                        %% 匹配"测试项"(9字节)
                                        case Suffix9 of
                                            <<230,181,139,232,175,149,233,161,185>> ->
                                                binary:part(Name, 0, NameLen - 9);
                                            _ -> undefined
                                        end;
                                    false -> undefined
                                end
                        end,
                        case NewName of
                            undefined -> ok;
                            _ -> 
                                io:format("修复 ~s: ~ts -> ~ts (devaddr=~ts)~n", [ObjectId, Name, NewName, DevAddr]),
                                dgiot_parse:update_object(<<"Device">>, ObjectId, #{<<"name">> => NewName})
                        end;
                    false ->
                        ok
                end
            end,
            lists:foreach(Fix, Results),
            io:format("修复完成~n"),
            ok;
        {error, Reason} ->
            io:format("查询失败: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 重建测试项设备：删除旧设备，用新的devaddr（去掉工位号）重建
rebuild_test_devices() ->
    io:format("=== 开始重建测试项设备 ===~n"),
    case dgiot_parse:query_object(<<"Device">>, #{
        <<"where">> => #{
            <<"product">> => #{
                <<"__type">> => <<"Pointer">>, 
                <<"className">> => <<"Product">>, 
                <<"objectId">> => <<"343cf21f82">>
            }
        },
        <<"limit">> => 200,
        <<"keys">> => <<"name,devaddr,objectId">>
    }) of
        {ok, #{<<"results">> := Results}} ->
            io:format("查询到 ~p 个设备~n", [length(Results)]),
            Process = fun(D) ->
                ObjectId = maps:get(<<"objectId">>, D, <<>>),
                Name = maps:get(<<"name">>, D, <<>>),
                DevAddr = maps:get(<<"devaddr">>, D, <<>>),
                % 使用binary:split来检测是否包含下划线
                case binary:split(DevAddr, <<"_">>) of
                    [_, NewDevAddr] when NewDevAddr =/= <<>> -> 
                        % 检查前半部分是否是工位号（纯数字或D开头）
                        case binary:first(DevAddr) of
                            $D -> skip;  % D开头的如D1100，跳过
                            _ -> 
                                io:format("处理 ~ts: ~ts -> ~ts~n", [ObjectId, DevAddr, NewDevAddr]),
                                % 删除旧设备
                                ProductId = <<"343cf21f82">>,
                                DelResult = dgiot_parse:del_object(<<"Device">>, ObjectId),
                                case DelResult of
                                    ok ->
                                        io:format("  删除成功~n");
                                    {ok, _} ->
                                        io:format("  删除成功~n");
                                    {error, DelReason} ->
                                        io:format("  删除失败: ~p, 继续重建~n", [DelReason])
                                end,
                                % 直接使用 dgiot_parse 创建设备对象
                                DeviceData = #{
                                    <<"name">> => Name,
                                    <<"devaddr">> => NewDevAddr,
                                    <<"product">> => #{
                                        <<"__type">> => <<"Pointer">>,
                                        <<"className">> => <<"Product">>,
                                        <<"objectId">> => ProductId
                                    },
                                    <<"status">> => <<"OFFLINE">>,
                                    <<"isEnable">> => true
                                },
                                case dgiot_parse:create_object(<<"Device">>, DeviceData) of
                                    {ok, #{<<"objectId">> := NewObjectId}} ->
                                        io:format("  重建成功: ~ts~n", [NewObjectId]),
                                        dgiot_task:save_pnque(ProductId, NewDevAddr, ProductId, NewDevAddr);
                                    {error, CreateReason} ->
                                        io:format("  重建失败: ~p~n", [CreateReason])
                                end
                        end;
                    _ -> skip
                end
            end,
            lists:foreach(Process, Results),
            io:format("=== 重建完成 ===~n"),
            ok;
        {error, Reason} ->
            io:format("查询失败: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc 从测试卡创建测试项设备
create_test_items() ->
    io:format("=== 创建测试项设备 ===~n"),
    TestItems = [
        % 磁航向工位
        {<<"磁航向_磁航向校准"/utf8>>, <<"磁航向校准"/utf8>>},
        {<<"磁航向_磁航向测试"/utf8>>, <<"磁航向测试"/utf8>>},
        % 总测工位
        {<<"总测_上电"/utf8>>, <<"上电"/utf8>>},
        {<<"总测_导电膜与引信通信调试"/utf8>>, <<"导电膜与引信通信调试"/utf8>>},
        {<<"总测_飞控版本号检查"/utf8>>, <<"飞控版本号检查"/utf8>>},
        {<<"总测_导航状态检查"/utf8>>, <<"导航状态检查"/utf8>>},
        {<<"总测_帧频检查"/utf8>>, <<"帧频检查"/utf8>>},
        {<<"总测_发射筒通讯测试"/utf8>>, <<"发射筒通讯测试"/utf8>>},
        {<<"总测_主循环时间检查"/utf8>>, <<"主循环时间检查"/utf8>>},
        {<<"总测_电压显示"/utf8>>, <<"电压显示"/utf8>>},
        {<<"总测_一次电池通讯检查"/utf8>>, <<"一次电池通讯检查"/utf8>>},
        {<<"总测_卫星导航检查"/utf8>>, <<"卫星导航检查"/utf8>>},
        {<<"总测_原点装订功能调试"/utf8>>, <<"原点装订功能调试"/utf8>>},
        {<<"总测_航线装订功能"/utf8>>, <<"航线装订功能"/utf8>>},
        {<<"总测_加速度校准"/utf8>>, <<"加速度校准"/utf8>>},
        {<<"总测_姿态测试"/utf8>>, <<"姿态测试"/utf8>>},
        {<<"总测_气压高度检查"/utf8>>, <<"气压高度检查"/utf8>>},
        {<<"总测_空速标定"/utf8>>, <<"空速标定"/utf8>>},
        {<<"总测_空速调试"/utf8>>, <<"空速调试"/utf8>>},
        {<<"总测_左前翼校准"/utf8>>, <<"左前翼校准"/utf8>>},
        {<<"总测_右前翼校准"/utf8>>, <<"右前翼校准"/utf8>>},
        {<<"总测_左垂尾校准"/utf8>>, <<"左垂尾校准"/utf8>>},
        {<<"总测_右垂尾校准"/utf8>>, <<"右垂尾校准"/utf8>>},
        {<<"总测_舵面极性调试"/utf8>>, <<"舵面极性调试"/utf8>>},
        {<<"总测_铁电故障调试"/utf8>>, <<"铁电故障调试"/utf8>>},
        {<<"总测_动力测试"/utf8>>, <<"动力测试"/utf8>>},
        {<<"总测_引信5V供电调试"/utf8>>, <<"引信5V供电调试"/utf8>>},
        {<<"总测_引信24V供电调试"/utf8>>, <<"引信24V供电调试"/utf8>>},
        {<<"总测_弹翼开关与引信通信调试"/utf8>>, <<"弹翼开关与引信通信调试"/utf8>>},
        {<<"总测_电子变倍功能调试"/utf8>>, <<"电子变倍功能调试"/utf8>>},
        {<<"总测_锁定测试"/utf8>>, <<"锁定测试"/utf8>>},
        {<<"总测_扫描与刹车测试"/utf8>>, <<"扫描与刹车测试"/utf8>>},
        {<<"总测_重复性检查"/utf8>>, <<"重复性检查"/utf8>>},
        {<<"总测_黑白热切换测试"/utf8>>, <<"黑白热切换测试"/utf8>>},
        {<<"总测_H264码率、图像清晰度测试"/utf8>>, <<"H264码率、图像清晰度测试"/utf8>>},
        {<<"总测_数据链检查"/utf8>>, <<"数据链检查"/utf8>>},
        % 拷机工位
        {<<"拷机_拷机准备"/utf8>>, <<"拷机准备"/utf8>>},
        {<<"拷机_导航状态检查"/utf8>>, <<"导航状态检查"/utf8>>},
        {<<"拷机_卫星导航检查"/utf8>>, <<"卫星导航检查"/utf8>>},
        {<<"拷机_空速标定"/utf8>>, <<"空速标定"/utf8>>},
        {<<"拷机_空速调试"/utf8>>, <<"空速调试"/utf8>>},
        {<<"拷机_拷机结束"/utf8>>, <<"拷机结束"/utf8>>},
        % 桁架工位
        {<<"桁架_导引头上电"/utf8>>, <<"导引头上电"/utf8>>},
        {<<"桁架_可见光与红外切换测试"/utf8>>, <<"可见光与红外切换测试"/utf8>>},
        {<<"桁架_拷机结束"/utf8>>, <<"拷机结束"/utf8>>}
    ],
    ProductId = <<"343cf21f82">>,
    F = fun({Name, DevAddr}) ->
        DeviceData = #{
            <<"name">> => Name,
            <<"devaddr">> => DevAddr,
            <<"product">> => #{
                <<"__type">> => <<"Pointer">>,
                <<"className">> => <<"Product">>,
                <<"objectId">> => ProductId
            },
            <<"status">> => <<"OFFLINE">>,
            <<"isEnable">> => true
        },
        case dgiot_parse:create_object(<<"Device">>, DeviceData) of
            {ok, #{<<"objectId">> := ObjectId}} ->
                io:format("OK: ~ts -> ~ts~n", [Name, ObjectId]),
                dgiot_task:save_pnque(ProductId, DevAddr, ProductId, DevAddr);
            {error, Reason} ->
                io:format("ERROR: ~ts -> ~p~n", [Name, Reason])
        end
    end,
    lists:foreach(F, TestItems),
    io:format("=== 创建完成 ===~n"),
    ok.