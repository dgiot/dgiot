%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_device_manager - 无人机设备生命周期管理
%%% 增强版：增加设备在线状态更新和详细日志（全部使用 error 级别）
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_device_manager).

-include_lib("dgiot/include/logger.hrl").

%% 导入扫描枪协议模块，用于获取缓存的二维码数据
-import(dgiot_scanner_protocol, [get_cached_qrcode/1]).

-export([create_device/5, ensure_device/3, get_device_by_addr/1, update_device_name/2, update_device_content/2, update_test_report/4, update_station_info/2]).

%% 定义设备在线状态字段
-define(STATUS_ONLINE, <<"ONLINE">>).
-define(STATUS_OFFLINE, <<"OFFLINE">>).

%%%===================================================================
%%% API 函数
%%%===================================================================

%% @doc 创建设备（返回设备ID）
-spec create_device(LoginId :: binary(), ProductId :: binary(), DevAddr :: binary(),
                    Ip :: binary(), ChineseName :: binary()) -> {ok, binary()} | {error, term()}.
create_device(_LoginId, ProductId, DevAddr, Ip, ChineseName) ->
    ?LOG(error, "[设备管理器] 开始创建设备: DevAddr=~s, ProductId=~p, IP=~s, Name=~ts",
         [DevAddr, ProductId, Ip, ChineseName]),
    case dgiot_product:lookup_prod(ProductId) of
        {ok, #{<<"ACL">> := Acl, <<"devType">> := DevType, <<"name">> := _ProductName}} ->
            Params = #{
                <<"devaddr">> => DevAddr,
                <<"name">> => ChineseName,
                <<"ip">> => Ip,
                <<"isEnable">> => true,
                <<"product">> => ProductId,
                <<"ACL">> => Acl,
                <<"status">> => ?STATUS_ONLINE,
                <<"brand">> => ChineseName,
                <<"devModel">> => DevType,
                <<"last_updated">> => dgiot_datetime:now_secs()
            },
            case dgiot_device:create_device(Params) of
                {ok, #{<<"objectId">> := ObjectId}} ->
                    dgiot_task:save_pnque(ProductId, DevAddr, ProductId, DevAddr),
                    ?LOG(error, "[设备管理器] ✅ 设备创建成功: DevAddr=~s, ObjectId=~s", [DevAddr, ObjectId]),
                    %% TODO: 暂时注释掉，待dgiot_uav_aggregator模块实现ensure_tdengine_subtable/2函数
                    %% dgiot_uav_aggregator:ensure_tdengine_subtable(ProductId, DevAddr),
                    {ok, ObjectId};
                {error, #{<<"code">> := 137}} -> % 设备已存在
                    ?LOG(error, "[设备管理器] 设备已存在，尝试获取现有设备ID: DevAddr=~s", [DevAddr]),
                    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"devaddr">> => DevAddr}}) of
                        {ok, #{<<"results">> := [#{<<"objectId">> := OId} | _]}} ->
                            ?LOG(error, "[设备管理器] ✅ 获取到现有设备ID: ~s", [OId]),
                            {ok, OId};
                        _ ->
                            ?LOG(error, "[设备管理器] ❌ 设备已存在但查询失败: DevAddr=~s", [DevAddr]),
                            {error, already_exists}
                    end;
                {error, Reason} ->
                    ?LOG(error, "[设备管理器] 设备创建失败: DevAddr=~s, Reason=~p", [DevAddr, Reason]),
                    {error, Reason}
            end;
        _Error ->
            ?LOG(error, "[设备管理器] 产品不存在: ProductId=~p", [ProductId]),
            {error, product_not_found}
    end.

%% @doc 确保设备存在（根据飞机ID），并更新在线状态
-spec ensure_device(DroneId :: binary(), ProductId :: binary(), Ip :: binary()) ->
    {ok, DeviceObjectId :: binary()} | {error, term()}.
ensure_device(DroneId, ProductId, Ip) when is_binary(DroneId) ->
    ?LOG(info, "[设备管理器] ensure_device 被调用: DroneID=~s, ProductId=~p, IP=~s",
         [DroneId, ProductId, Ip]),
    case dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"devaddr">> => DroneId}}) of
        {ok, #{<<"results">> := [Device | _]}} ->
            ObjectId = maps:get(<<"objectId">>, Device),
            ?LOG(info, "[设备管理器] 设备已存在: DevAddr=~s, ObjectId=~s", [DroneId, ObjectId]),
            
            %% 检查磁航向工位的二维码缓存，更新设备名称并存储二维码数据到content
            case get_cached_qrcode(1) of
                {ok, ParsedData} ->
                    case maps:get(<<"serial_no">>, ParsedData, <<>>) of
                        <<>> -> 
                            ?LOG(info, "[设备管理器] 磁航向工位的二维码没有序列号，保持现有名称");
                        SerialNo -> 
                            CurrentName = maps:get(<<"name">>, Device, <<>>),
                            if CurrentName =/= SerialNo ->
                                ?LOG(info, "[设备管理器] 更新现有设备名称: ~s -> ~s", [CurrentName, SerialNo]),
                                update_device_name(DroneId, SerialNo);
                               true -> 
                                ?LOG(info, "[设备管理器] 设备名称已是最新: ~s", [SerialNo])
                            end
                    end,
                    %% 将完整的二维码数据存储到设备的content字段
                    ?LOG(info, "[设备管理器] 存储二维码数据到设备content字段: 字段数=~p", [maps:size(ParsedData)]),
                    update_device_content(DroneId, ParsedData);
                {error, not_find} ->
                    ?LOG(info, "[设备管理器] 磁航向工位没有缓存二维码");
                {error, expired} ->
                    ?LOG(info, "[设备管理器] 磁航向工位的二维码缓存已过期")
            end,
            
            %% 更新设备在线状态
            update_device_online(ObjectId),
            {ok, ObjectId};
        {ok, #{<<"results">> := []}} ->
            ?LOG(info, "[设备管理器] 设备不存在，准备创建: DroneID=~s", [DroneId]),
            %% 获取设备名称：总是检查磁航向工位（工位1）的二维码缓存
            ChineseName = case get_cached_qrcode(1) of
                {ok, ParsedData} ->
                    %% 使用二维码中的序列号作为设备名称
                    case maps:get(<<"serial_no">>, ParsedData, <<>>) of
                        <<>> -> 
                            ?LOG(info, "[设备管理器] 磁航向工位的二维码没有序列号，使用默认名称"),
                            get_drone_name(DroneId);
                        SerialNo -> 
                            ?LOG(info, "[设备管理器] ✅ 使用磁航向工位的二维码序列号: ~s", [SerialNo]),
                            SerialNo
                    end;
                {error, not_find} ->
                    ?LOG(info, "[设备管理器] 磁航向工位没有缓存二维码，使用默认名称"),
                    get_drone_name(DroneId);
                {error, expired} ->
                    ?LOG(info, "[设备管理器] 磁航向工位的二维码缓存已过期，使用默认名称"),
                    get_drone_name(DroneId)
            end,
            case create_device(DroneId, ProductId, DroneId, Ip, ChineseName) of
                {ok, ObjectId} ->
                    update_device_online(ObjectId),
                    %% 如果二维码数据存在，存储到content字段
                    case get_cached_qrcode(1) of
                        {ok, QrData} ->
                            ?LOG(info, "[设备管理器] 新建设备存储二维码数据到content字段"),
                            update_device_content(DroneId, QrData);
                        _ -> ok
                    end,
                    {ok, ObjectId};
                {error, Reason} = Error ->
                    ?LOG(error, "[设备管理器] 设备创建失败: DroneID=~s, Reason=~p", [DroneId, Reason]),
                    Error
            end;
        {error, Reason} ->
            ?LOG(error, "[设备管理器] 查询设备失败: DroneID=~s, Reason=~p", [DroneId, Reason]),
            {error, Reason}
    end.

%% @doc 根据设备地址获取设备信息
% -spec get_device_by_addr(DevAddr :: binary()) ->
%     {ok, #{<<"results">> := list()}} | {error, term()}.
get_device_by_addr(DevAddr) ->
    dgiot_parse:query_object(<<"Device">>, #{<<"where">> => #{<<"devaddr">> => DevAddr}}).

%% @doc 更新设备名称
-spec update_device_name(DevAddr :: binary(), NewName :: binary()) -> ok | {error, term()}.
update_device_name(DevAddr, NewName) ->
    ?LOG(info, "[设备管理器] 更新设备名称: DevAddr=~s, NewName=~s", [DevAddr, NewName]),
    case get_device_by_addr(DevAddr) of
        {ok, #{<<"results">> := [#{<<"objectId">> := OId} | _]}} ->
            case dgiot_parse:update_object(<<"Device">>, OId, #{<<"name">> => NewName}) of
                {ok, _Result} ->
                    ?LOG(info, "[设备管理器] 设备名称更新成功: DevAddr=~s", [DevAddr]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "[设备管理器] 设备名称更新失败: DevAddr=~s, Reason=~p", [DevAddr, Reason]),
                    {error, Reason}
            end;
        _ ->
            ?LOG(error, "[设备管理器] 设备不存在: DevAddr=~s", [DevAddr]),
            {error, not_found}
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 生成无人机显示名称
-spec get_drone_name(DroneId :: binary()) -> binary().
get_drone_name(DroneId) ->
    try binary_to_integer(DroneId) of
        IntId ->
            case IntId of
                16#5D11 -> <<"拷机1"/utf8>>;
                16#5CC1 -> <<"拷机1"/utf8>>;
                16#5CD1 -> <<"桁架"/utf8>>;
                _ -> unicode:characters_to_binary(io_lib:format("无人机 (0x~4.16.0B)", [IntId]))
            end
    catch _:_ ->
        <<"无人机 (", DroneId/binary, ")"/utf8>>
    end.

%% @doc 更新设备在线状态（设置 status = ONLINE 并刷新 last_updated）
-spec update_device_online(ObjectId :: binary()) -> ok | {error, term()}.
update_device_online(ObjectId) ->
    UpdateData = #{
        <<"status">> => ?STATUS_ONLINE,
        <<"last_updated">> => dgiot_datetime:now_secs()
    },
    case dgiot_parse:update_object(<<"Device">>, ObjectId, UpdateData) of
        {ok, _Result} ->
            ?LOG(info, "[设备管理器] 设备在线状态更新成功: ObjectId=~s", [ObjectId]),
            ok;
        {error, Reason} ->
            ?LOG(error, "[设备管理器] 设备在线状态更新失败: ObjectId=~s, Reason=~p", [ObjectId, Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% 内部辅助函数
%%%===================================================================



%% @doc 更新设备content字段
-spec update_device_content(DevAddr :: binary(), ContentData :: map()) -> ok | {error, term()}.
update_device_content(DevAddr, ContentData) ->
    ?LOG(info, "[设备管理器] 更新设备content字段: DevAddr=~s, 数据大小=~p", [DevAddr, maps:size(ContentData)]),
    case get_device_by_addr(DevAddr) of
        {ok, #{<<"results">> := [#{<<"objectId">> := OId} | _]}} ->
            %% 构建完整的content结构
            FullContent = build_device_content(ContentData),
            case dgiot_parse:update_object(<<"Device">>, OId, #{<<"content">> => FullContent}) of
                {ok, _Result} ->
                    ?LOG(info, "[设备管理器] 设备content字段更新成功: DevAddr=~s", [DevAddr]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "[设备管理器] 设备content字段更新失败: DevAddr=~s, Reason=~p", [DevAddr, Reason]),
                    {error, Reason}
            end;
        _ ->
            ?LOG(error, "[设备管理器] 设备不存在: DevAddr=~s", [DevAddr]),
            {error, not_found}
    end.

%% @doc 构建设备content字段的完整结构
-spec build_device_content(RawData :: map()) -> map().
build_device_content(RawData) ->
    %% content字段结构设计：
    %% {
    %%   "qrcode_info": { // 二维码信息
    %%     "serial_no": "5004055",
    %%     "purchase_order_no": "...",
    %%     "project_no": "...",
    %%     "material_code": "...",
    %%     "batch_no": "...",
    %%     "supplier": "...",
    %%     "expiry_date": "...",
    %%     "quantity": 1,
    %%     "scan_time": "2026-03-16T08:40:30Z"
    %%   },
    %%   "test_reports": { // 测试报告
    %%     "overall_status": "not_started", // 整体状态: not_started, running, completed, failed
    %%     "overall_result": "untested", // 整体结果: passed, failed, untested
    %%     "steps": { // 测试步骤
    %%       "1": {"name": "备检并获取编码", "status": "not_started", "result": "untested", "start_time": null, "end_time": null},
    %%       "2": {"name": "机身静态测试前检查", "status": "not_started", "result": "untested", "start_time": null, "end_time": null},
    %%       "3": {"name": "机身及螺旋桨安装情况检查", "status": "not_started", "result": "untested", "start_time": null, "end_time": null},
    %%       "4": {"name": "电压测量检查", "status": "not_started", "result": "untested", "start_time": null, "end_time": null},
    %%       "5": {"name": "链路功能检查", "status": "not_started", "result": "untested", "start_time": null, "end_time": null},
    %%       "6": {"name": "上电参数检查", "status": "not_started", "result": "untested", "start_time": null, "end_time": null},
    %%       "7": {"name": "夜航灯测试", "status": "not_started", "result": "untested", "start_time": null, "end_time": null},
    %%       "8": {"name": "气压高度检测", "status": "not_started", "result": "untested", "start_time": null, "end_time": null},
    %%       "9": {"name": "系统电磁兼容性功能检查", "status": "not_started", "result": "untested", "start_time": null, "end_time": null},
    %%       "10": {"name": "航线加载及载荷功能检查", "status": "not_started", "result": "untested", "start_time": null, "end_time": null}
    %%     }
    %%   },
    %%   "station_info": { // 工位信息
    %%     "current_station": null, // 当前所在工位ID
    %%     "station_history": [] // 工位历史记录
    %%   },
    %%   "metadata": { // 元数据
    %%     "last_updated": "2026-03-16T08:40:30Z",
    %%     "content_version": "1.0"
    %%   }
    %% }
    
    %% 提取二维码信息
    QrcodeInfo = #{
        <<"serial_no">> => maps:get(<<"serial_no">>, RawData, <<>>),
        <<"purchase_order_no">> => maps:get(<<"purchase_order_no">>, RawData, <<>>),
        <<"project_no">> => maps:get(<<"project_no">>, RawData, <<>>),
        <<"material_code">> => maps:get(<<"material_code">>, RawData, <<>>),
        <<"batch_no">> => maps:get(<<"batch_no">>, RawData, <<>>),
        <<"supplier">> => maps:get(<<"supplier">>, RawData, <<>>),
        <<"expiry_date">> => maps:get(<<"expiry_date">>, RawData, <<>>),
        <<"quantity">> => maps:get(<<"quantity">>, RawData, 1),
        <<"qrcode_format">> => maps:get(<<"qrcode_format">>, RawData, <<"unknown">>),
        <<"scan_time">> => dgiot_datetime:now_secs()
    },
    
    %% 构建测试报告结构（初始化状态）
    Steps = #{
        <<"1">> => #{<<"name">> => <<"备检并获取编码"/utf8>>, <<"status">> => <<"not_started">>, <<"result">> => <<"untested">>},
        <<"2">> => #{<<"name">> => <<"机身静态测试前检查"/utf8>>, <<"status">> => <<"not_started">>, <<"result">> => <<"untested">>},
        <<"3">> => #{<<"name">> => <<"机身及螺旋桨安装情况检查"/utf8>>, <<"status">> => <<"not_started">>, <<"result">> => <<"untested">>},
        <<"4">> => #{<<"name">> => <<"电压测量检查"/utf8>>, <<"status">> => <<"not_started">>, <<"result">> => <<"untested">>},
        <<"5">> => #{<<"name">> => <<"链路功能检查"/utf8>>, <<"status">> => <<"not_started">>, <<"result">> => <<"untested">>},
        <<"6">> => #{<<"name">> => <<"上电参数检查"/utf8>>, <<"status">> => <<"not_started">>, <<"result">> => <<"untested">>},
        <<"7">> => #{<<"name">> => <<"夜航灯测试"/utf8>>, <<"status">> => <<"not_started">>, <<"result">> => <<"untested">>},
        <<"8">> => #{<<"name">> => <<"气压高度检测"/utf8>>, <<"status">> => <<"not_started">>, <<"result">> => <<"untested">>},
        <<"9">> => #{<<"name">> => <<"系统电磁兼容性功能检查"/utf8>>, <<"status">> => <<"not_started">>, <<"result">> => <<"untested">>},
        <<"10">> => #{<<"name">> => <<"航线加载及载荷功能检查"/utf8>>, <<"status">> => <<"not_started">>, <<"result">> => <<"untested">>}
    },
    
    TestReports = #{
        <<"overall_status">> => <<"not_started">>,
        <<"overall_result">> => <<"untested">>,
        <<"steps">> => Steps
    },
    
    %% 返回完整的content结构
    #{
        <<"qrcode_info">> => QrcodeInfo,
        <<"test_reports">> => TestReports,
        <<"station_info">> => #{<<"current_station">> => null, <<"station_history">> => []},
        <<"metadata">> => #{
            <<"last_updated">> => dgiot_datetime:now_secs(),
            <<"content_version">> => <<"1.0">>
        }
    }.

%% @doc 更新测试报告步骤
-spec update_test_report(DevAddr :: binary(), StepNumber :: integer(), Status :: binary(), Result :: binary()) -> ok | {error, term()}.
update_test_report(DevAddr, StepNumber, Status, Result) ->
    ?LOG(info, "[设备管理器] 更新测试报告: DevAddr=~s, Step=~p, Status=~s, Result=~s", [DevAddr, StepNumber, Status, Result]),
    case get_device_by_addr(DevAddr) of
        {ok, #{<<"results">> := [#{<<"objectId">> := OId, <<"content">> := Content} | _]}} ->
            StepKey = dgiot_utils:to_binary(StepNumber),
            
            %% 获取现有的步骤数据，保留名称字段
            ExistingSteps = maps:get(<<"steps">>, maps:get(<<"test_reports">>, Content, #{}), #{}),
            ExistingStep = maps:get(StepKey, ExistingSteps, #{}),
            ExistingName = maps:get(<<"name">>, ExistingStep, get_default_step_name(StepNumber)),
            
            %% 构建步骤更新数据，保留原有名称
            StepUpdate = #{<<"name">> => ExistingName, <<"status">> => Status, <<"result">> => Result},
            
            %% 更新测试报告中的步骤
            UpdatedSteps = maps:put(StepKey, StepUpdate, ExistingSteps),
            
            %% 更新整体测试报告
            UpdatedTestReports = maps:put(<<"steps">>, UpdatedSteps, maps:get(<<"test_reports">>, Content, #{})),
            
            %% 更新content字段
            UpdatedContent = maps:put(<<"test_reports">>, UpdatedTestReports, Content),
            
            %% 更新元数据
            FinalContent = maps:put(<<"metadata">>, #{
                <<"last_updated">> => dgiot_datetime:now_secs(),
                <<"content_version">> => <<"1.0">>
            }, UpdatedContent),
            
            case dgiot_parse:update_object(<<"Device">>, OId, #{<<"content">> => FinalContent}) of
                {ok, _Result} ->
                    ?LOG(info, "[设备管理器] 测试报告更新成功: DevAddr=~s, Step=~p", [DevAddr, StepNumber]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "[设备管理器] 测试报告更新失败: DevAddr=~s, Reason=~p", [DevAddr, Reason]),
                    {error, Reason}
            end;
        _ ->
            ?LOG(error, "[设备管理器] 设备不存在: DevAddr=~s", [DevAddr]),
            {error, not_found}
    end.

%% @doc 获取默认的测试步骤名称
-spec get_default_step_name(StepNumber :: integer()) -> binary().
get_default_step_name(1) -> <<"备检并获取编码"/utf8>>;
get_default_step_name(2) -> <<"机身静态测试前检查"/utf8>>;
get_default_step_name(3) -> <<"机身及螺旋桨安装情况检查"/utf8>>;
get_default_step_name(4) -> <<"电压测量检查"/utf8>>;
get_default_step_name(5) -> <<"链路功能检查"/utf8>>;
get_default_step_name(6) -> <<"上电参数检查"/utf8>>;
get_default_step_name(7) -> <<"夜航灯测试"/utf8>>;
get_default_step_name(8) -> <<"气压高度检测"/utf8>>;
get_default_step_name(9) -> <<"系统电磁兼容性功能检查"/utf8>>;
get_default_step_name(10) -> <<"航线加载及载荷功能检查"/utf8>>;
get_default_step_name(_) -> <<"未知测试步骤"/utf8>>.

%% @doc 更新工位信息
-spec update_station_info(DevAddr :: binary(), StationId :: integer()) -> ok | {error, term()}.
update_station_info(DevAddr, StationId) ->
    ?LOG(info, "[设备管理器] 更新工位信息: DevAddr=~s, StationId=~p", [DevAddr, StationId]),
    case get_device_by_addr(DevAddr) of
        {ok, #{<<"results">> := [#{<<"objectId">> := OId, <<"content">> := Content} | _]}} ->
            %% 获取当前工位信息
            StationInfo = maps:get(<<"station_info">>, Content, #{<<"current_station">> => null, <<"station_history">> => []}),
            CurrentStation = maps:get(<<"current_station">>, StationInfo, null),
            
            %% 更新工位历史
            StationHistory = maps:get(<<"station_history">>, StationInfo, []),
            UpdatedHistory = case CurrentStation of
                null -> StationHistory;
                _ -> [CurrentStation | StationHistory]
            end,
            
            %% 更新工位信息
            UpdatedStationInfo = #{
                <<"current_station">> => StationId,
                <<"station_history">> => UpdatedHistory
            },
            
            %% 更新content字段
            UpdatedContent = maps:put(<<"station_info">>, UpdatedStationInfo, Content),
            
            %% 更新元数据
            FinalContent = maps:put(<<"metadata">>, #{
                <<"last_updated">> => dgiot_datetime:now_secs(),
                <<"content_version">> => <<"1.0">>
            }, UpdatedContent),
            
            case dgiot_parse:update_object(<<"Device">>, OId, #{<<"content">> => FinalContent}) of
                {ok, _Result} ->
                    ?LOG(info, "[设备管理器] 工位信息更新成功: DevAddr=~s, StationId=~p", [DevAddr, StationId]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "[设备管理器] 工位信息更新失败: DevAddr=~s, Reason=~p", [DevAddr, Reason]),
                    {error, Reason}
            end;
        _ ->
            ?LOG(error, "[设备管理器] 设备不存在: DevAddr=~s", [DevAddr]),
            {error, not_found}
    end.

