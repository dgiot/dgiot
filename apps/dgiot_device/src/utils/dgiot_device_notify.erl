-module(dgiot_device_notify).
-author("Your Name").
-include("dgiot_device.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([notification/6]).

%% @doc 发送设备状态通知
%% DeviceId: 设备ID
%% Status: 设备状态 (true/false)
%% Longitude: 经度
%% Latitude: 纬度
%% IsEnable: 是否启用
%% Now: 当前时间戳
notification(DeviceId, Status, Longitude, Latitude, IsEnable, Now) ->
    Topic = <<"$dg/user/devicestate/", DeviceId/binary, "/report">>,
    case dgiot_mqtt:has_routes(Topic) of
        true ->
            publish_device_state(DeviceId, Topic, Status, Longitude, Latitude, IsEnable, Now);
        false ->
            pass
    end.

%% @doc 发布设备状态到MQTT主题
publish_device_state(DeviceId, Topic, Status, Longitude, Latitude, IsEnable, Now) ->
    NewStatus = status_to_binary(Status),
    Address = get_device_address(DeviceId, Longitude, Latitude),
    PubData = build_publish_data(DeviceId, NewStatus, Longitude, Latitude, IsEnable, Now, Address),
    dgiot_mqtt:publish(DeviceId, Topic, dgiot_json:encode(PubData)).

%% @doc 将状态转换为二进制格式
status_to_binary(true) -> <<"ONLINE">>;
status_to_binary(_) -> <<"OFFLINE">>.

%% @doc 获取设备地址信息
get_device_address(DeviceId, Longitude, Latitude) ->
    case dgiot_data:get(?DGIOT_LOCATION_ADDRESS, DeviceId) of
        Addr when byte_size(Addr) > 0 -> Addr;
        _ -> dgiot_device_cache:get_address(DeviceId, Longitude, Latitude)
    end.

%% @doc 构建发布数据
build_publish_data(DeviceId, Status, Longitude, Latitude, IsEnable, Now, Address) ->
    BaseData = #{
        DeviceId => #{
            <<"status">> => Status,
            <<"isEnable">> => IsEnable,
            <<"lastOnlineTime">> => Now,
            <<"location">> => #{
                <<"longitude">> => Longitude,
                <<"latitude">> => Latitude
            }
        }
    },
    case byte_size(Address) of
        0 -> BaseData;
        _ -> BaseData#{<<"address">> => Address}
    end.
