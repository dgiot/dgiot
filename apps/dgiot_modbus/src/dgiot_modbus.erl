
-module(dgiot_modbus).
-include_lib("dgiot/include/logger.hrl").

%% @doc Modbus设备管理模块
%% 负责Modbus设备的启动、连接管理、设备注册和创建
%% 支持设备查询、客户端连接建立、父子设备关系处理

%% API
-export([
        start_client/2, 
        register_client/5, 
        create_device/5,
        connect_client/3,
        is_valid_device/1,
        start_client_connection/5,
        log_invalid_device/1
    ]).

%% @doc 启动Modbus客户端连接
%% 查询指定产品下的所有设备，并建立客户端连接
%% ChannelId: 通道ID
%% ProductId: 产品ID
%% 返回: 无
start_client(ChannelId, ProductId) ->
    case dgiot_parsex:query_object(<<"Device">>, #{<<"keys">> => [<<"ip">>, <<"detail">>, <<"parentId">>], <<"where">> => #{<<"product">> => ProductId}}) of
        {ok,#{<<"results">> := Devices}} ->

        connect_client(ChannelId, ProductId, Devices);
    _ ->
        pass
    end.

%% @doc 连接设备客户端
%% 递归处理设备列表，为每个设备启动客户端连接
%% 支持父子设备关系处理
%% ChannelId: 通道ID
%% ProductId: 产品ID
%% Devices: 设备列表
%% 返回: 无
connect_client(_ChannelId, _ProductId, []) ->
    pass;

connect_client(ChannelId, ProductId, [Device | Devices]) ->
    case is_valid_device(Device) of
        {true, DeviceId, Ip, Port, ParentId} ->
            %% 启动客户端连接
            start_client_connection(ChannelId, DeviceId, Ip, Port, ParentId),
            connect_client(ChannelId, ProductId, Devices);
        false ->
            %% 记录无效设备信息并继续处理
            log_invalid_device(Device),
            connect_client(ChannelId, ProductId, Devices)
    end.

%% @doc 验证设备数据是否完整
%% 检查设备是否包含必要的字段：objectId, ip, detail.port
%% Device: 设备数据映射
%% 返回: {true, DeviceId, Ip, Port, ParentId} | false
is_valid_device(#{<<"objectId">> := DeviceId, <<"ip">> := Ip, <<"detail">> := #{<<"port">> := Port}} = Device) ->
    ParentId = maps:get(<<"parentId">>, Device, <<>>),
    {true, DeviceId, Ip, Port, ParentId};
is_valid_device(_Device) ->
    false.

%% @doc 启动客户端连接
%% 根据设备是否有父设备来启动不同类型的连接
%% ChannelId: 通道ID
%% DeviceId: 设备ID
%% Ip: 设备IP地址
%% Port: 设备端口
%% ParentId: 父设备ID（可为空）
%% 返回: 无
start_client_connection(ChannelId, DeviceId, Ip, Port, ParentId) ->
    case ParentId of
        <<>> ->
            %% 独立设备，启动普通客户端连接
            dgiot_client:start(ChannelId, DeviceId, #{<<"ip">> => Ip, <<"port">> => Port});
        #{<<"objectId">> := ParentIdValue} ->
            %% 子设备，启动带有父设备信息的客户端连接
            dgiot_client:start(ChannelId, DeviceId, #{
                <<"ip">> => Ip, 
                <<"port">> => Port, 
                <<"child">> => #{<<"parentId">> => ParentIdValue}
            });
        _ ->
            %% 处理无效的ParentId格式
            ?LOG(warning, "Invalid parentId format for device ~p: ~p", [DeviceId, ParentId]),
            dgiot_client:start(ChannelId, DeviceId, #{<<"ip">> => Ip, <<"port">> => Port})
    end.

%% @doc 记录无效设备信息
%% 当设备数据不完整时记录警告日志
%% Device: 无效的设备数据
%% 返回: 无
log_invalid_device(Device) ->
    case Device of
        #{<<"objectId">> := DeviceId} ->
            ?LOG(warning, "Device ~p has incomplete data, skipping connection: ~p", [DeviceId, maps:without([<<"detail">>], Device)]);
        _ ->
            ?LOG(warning, "Invalid device data without objectId: ~p", [Device])
    end.

%% @doc 注册Modbus客户端
%% 处理设备登录流程，包括设备ID生成、日志记录、MQTT订阅和设备创建
%% ChannelId: 通道ID
%% ProductId: 产品ID
%% DtuAddr: 设备地址
%% DtuIp: 设备IP地址
%% Dtutype: 设备类型
%% 返回: create_device/5 的返回值
register_client(ChannelId, ProductId, DtuAddr, DtuIp, Dtutype) ->
    %% 根据产品ID和设备地址生成设备唯一ID
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DtuAddr),
    io:format("~s ~p  DtuAddr:~p  ProductId ~p DeviceId ~p  ~n", [?FILE, ?LINE,  DtuAddr, ProductId, DeviceId]),
    %% 发送设备登录日志
    dgiot_bridge:send_log(ChannelId, ProductId, DtuAddr, "~s ~p DTU login DtuAddr:~p", [?FILE, ?LINE, DtuAddr]),
    %% 订阅设备配置主题
    Topic = <<"$dg/device/", ProductId/binary, "/", DtuAddr/binary, "/profile">>,
    dgiot_mqtt:subscribe(Topic),
    %% 订阅设备调试主题
    Topic1 = <<"$dg/device/", ProductId/binary, "/", DtuAddr/binary, "/debug">>,
    dgiot_mqtt:subscribe(Topic1),
    %% 保存设备在线状态日志
    dgiot_device:save_log(ProductId, DtuAddr, DtuAddr, <<"online">>),
    %% 创建设备记录
    create_device(DeviceId, ProductId, DtuAddr, DtuIp, Dtutype).

create_device(DeviceId, ProductId, DtuAddr, DtuIp, Dtutype) ->
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl, <<"devType">> := DevType}} ->
            dgiot_device:create_device(#{
                <<"devaddr">> => DtuAddr,
                <<"name">> => <<Dtutype/binary, "_", DtuAddr/binary>>,
                <<"ip">> => DtuIp,
                <<"isEnable">> => true,
                <<"product">> => ProductId,
                <<"ACL">> => Acl,
                <<"status">> => <<"ONLINE">>,
                <<"brand">> => Dtutype,
                <<"devModel">> => DevType
            }),
            dgiot_task:save_pnque(ProductId, DtuAddr, ProductId, DtuAddr),
            Productname =
                case dgiot_parse:get_object(<<"Product">>, ProductId) of
                    {ok, #{<<"name">> := Productname1}} ->
                        Productname1;
                    _ ->
                        <<"">>
                end,
            ?MLOG(info, #{<<"clientid">> => DeviceId, <<"devaddr">> => DtuAddr, <<"productid">> => ProductId, <<"productname">> => Productname, <<"devicename">> => <<Dtutype/binary, DtuAddr/binary>>, <<"status">> => <<"在线"/utf8>>}, ['device_statuslog']),
            {DeviceId, DtuAddr};
        _Error2 ->
%%            ?LOG(info, "Error2 ~p ", [Error2]),
            {<<>>, <<>>}
    end.
