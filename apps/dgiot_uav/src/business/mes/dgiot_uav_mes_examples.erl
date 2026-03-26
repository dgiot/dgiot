%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_mes_examples - 无人机MES示例生成与测试
%%%
%%% 提供从JSON文件读取测试项、生成MES示例、测试上报功能等。
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_mes_examples).

%% API
-export([
    get_json/1,
    get_test_items/0,
    get_mes_examples/0,
    generate_mes_data_from_test_items/1,
    test_mes_reporting/0,
    test_device_status_report/0,
    test_test_start_report/0,
    test_test_completion_report/0,
    test_json_reading/0,
    test_mes_detailed/0
]).

-include_lib("dgiot/include/logger.hrl").

%% 无人机所在大工序类型（与API模块保持一致）
-define(DRONE_TYPE_FINAL_TEST, 1).
-define(DRONE_TYPE_BURN_IN, 2).
-define(DRONE_TYPE_INSPECTION, 3).
-define(DRONE_TYPE_MAGNETIC, 4).

%% 产线状态类型编码
-define(LINE_STA_START, 1).
-define(LINE_STA_PRODUCTION, 2).

%%%===================================================================
%%% JSON文件读取
%%%===================================================================

-spec get_json(binary()) -> map() | binary().
get_json(Type) ->
    FileName = <<Type/binary, ".json">>,
    dgiot_utils:get_JsonFile(?MODULE, FileName).

-spec get_test_items() -> list().
get_test_items() ->
    case get_json(<<"test_items_full">>) of
        TestItems when is_list(TestItems) -> TestItems;
        _ ->
            ?LOG(error, "无法读取测试项数据"),
            []
    end.

-spec get_mes_examples() -> map().
get_mes_examples() ->
    case get_json(<<"mes">>) of
        #{<<"item">> := _Items} = MesData -> MesData;
        _ ->
            ?LOG(error, "无法读取MES示例数据"),
            #{}
    end.

%%%===================================================================
%%% 示例生成
%%%===================================================================

-spec generate_mes_data_from_test_items(list()) -> list().
generate_mes_data_from_test_items(TestItems) ->
    lists:map(fun(TestItem) ->
        #{
            device_address => maps:get(<<"device_address">>, TestItem, <<"未知地址">>),
            device_name => maps:get(<<"device_name">>, TestItem, <<"未知设备">>),
            station_name => maps:get(<<"station_name">>, TestItem, <<"未知工位">>),
            test_steps_count => length(maps:get(<<"test_steps">>, TestItem, [])),
            mes_report_examples => generate_mes_examples_for_test_item(TestItem)
        }
    end, TestItems).

generate_mes_examples_for_test_item(TestItem) ->
    DeviceAddress = maps:get(<<"device_address">>, TestItem, <<"未知地址"/utf8>>),
    StationName = maps:get(<<"station_name">>, TestItem, <<"未知工位"/utf8>>),
    case StationName of
        <<"磁航向"/utf8>> -> generate_magnetic_mes_examples(DeviceAddress);
        <<"总测1"/utf8>> -> generate_final_test_mes_examples(DeviceAddress);
        <<"拷机1"/utf8>> -> generate_burn_in_mes_examples(DeviceAddress);
        <<"拷机2"/utf8>> -> generate_burn_in_mes_examples(DeviceAddress);
        <<"桁架"/utf8>> -> generate_gantry_mes_examples(DeviceAddress);
        _ -> generate_generic_mes_examples(DeviceAddress)
    end.

generate_magnetic_mes_examples(DeviceAddress) ->
    [
        #{
            type => <<"开工"/utf8>>,
            line_sta => ?LINE_STA_START,
            description => <<"磁航向工位开工"/utf8>>,
            example_data => example_data(<<"A:磁航向"/utf8>>, <<"UAV_MAG_001">>, ?DRONE_TYPE_MAGNETIC, DeviceAddress, <<"A-MAG-01">>)
        },
        #{
            type => <<"生产"/utf8>>,
            line_sta => ?LINE_STA_PRODUCTION,
            description => <<"磁航向校准测试中"/utf8>>,
            example_data => example_data(<<"A:磁航向"/utf8>>, <<"UAV_MAG_001">>, ?DRONE_TYPE_MAGNETIC, DeviceAddress, <<"A-MAG-01">>)
        }
    ].

generate_final_test_mes_examples(DeviceAddress) ->
    [
        #{
            type => <<"开工"/utf8>>,
            line_sta => ?LINE_STA_START,
            description => <<"总测工位开工"/utf8>>,
            example_data => example_data(<<"A:总测01">>, <<"UAV_FT_001">>, ?DRONE_TYPE_FINAL_TEST, DeviceAddress, <<"A-FT-01">>)
        },
        #{
            type => <<"生产"/utf8>>,
            line_sta => ?LINE_STA_PRODUCTION,
            description => <<"总测进行中"/utf8>>,
            example_data => example_data(<<"A:总测01">>, <<"UAV_FT_001">>, ?DRONE_TYPE_FINAL_TEST, DeviceAddress, <<"A-FT-01">>)
        }
    ].

generate_burn_in_mes_examples(DeviceAddress) ->
    [
        #{
            type => <<"开工"/utf8>>,
            line_sta => ?LINE_STA_START,
            description => <<"拷机工位开工"/utf8>>,
            example_data => example_data(<<"A:拷机01">>, <<"UAV_BI_001">>, ?DRONE_TYPE_BURN_IN, DeviceAddress, <<"A-BI-01">>)
        },
        #{
            type => <<"生产"/utf8>>,
            line_sta => ?LINE_STA_PRODUCTION,
            description => <<"拷机测试中"/utf8>>,
            example_data => example_data(<<"A:拷机01">>, <<"UAV_BI_001">>, ?DRONE_TYPE_BURN_IN, DeviceAddress, <<"A-BI-01">>)
        }
    ].

generate_gantry_mes_examples(_DeviceAddress) ->
    [
        #{
            type => <<"生产"/utf8>>,
            line_sta => ?LINE_STA_PRODUCTION,
            description => <<"桁架导引头测试中"/utf8>>,
            example_data => #{
                <<"func_id">> => <<"ALM_MES">>,
                <<"line_no">> => <<"A:桁架">>,
                <<"line_sta">> => ?LINE_STA_PRODUCTION,
                <<"drone_no">> => <<"UAV_GT_001">>,
                <<"date_time">> => dgiot_uav_mes_utils:get_current_timestamp(),
                <<"data_record">> => #{
                    <<"drone_no">> => <<"UAV_GT_001">>,
                    <<"drone_type">> => ?DRONE_TYPE_INSPECTION,
                    <<"line_proc_no">> => <<"A-GT-01">>,
                    <<"eqp_action_list">> => <<"导引头测试执行中"/utf8>>
                }
            }
        }
    ].

generate_generic_mes_examples(DeviceAddress) ->
    [
        #{
            type => <<"开工"/utf8>>,
            line_sta => ?LINE_STA_START,
            description => <<"工位开工"/utf8>>,
            example_data => #{
                <<"func_id">> => <<"ALM_MES">>,
                <<"line_no">> => <<"A:通用工位">>,
                <<"line_sta">> => ?LINE_STA_START,
                <<"drone_no">> => <<"UAV_GEN_001">>,
                <<"date_time">> => dgiot_uav_mes_utils:get_current_timestamp(),
                <<"data_record">> => #{
                    <<"trans_id">> => dgiot_uav_mes_utils:generate_mes_trans_id(),
                    <<"station_info">> => <<"通用测试工位"/utf8>>,
                    <<"device_address">> => DeviceAddress
                }
            }
        }
    ].

example_data(LineNo, DroneNo, DroneType, DeviceAddress, LineProcNo) ->
    #{
        <<"func_id">> => <<"ALM_MES">>,
        <<"line_no">> => LineNo,
        <<"line_sta">> => ?LINE_STA_START,
        <<"drone_no">> => DroneNo,
        <<"date_time">> => dgiot_uav_mes_utils:get_current_timestamp(),
        <<"data_record">> => #{
            <<"trans_id">> => dgiot_uav_mes_utils:generate_mes_trans_id(),
            <<"station_info">> => <<"测试工位"/utf8>>,
            <<"device_address">> => DeviceAddress,
            <<"drone_no">> => DroneNo,
            <<"drone_type">> => DroneType,
            <<"line_proc_no">> => LineProcNo,
            <<"eqp_action_list">> => <<"测试执行中"/utf8>>
        }
    }.

%%%===================================================================
%%% 测试函数
%%%===================================================================

-spec test_mes_reporting() -> ok.
test_mes_reporting() ->
    ?LOG(info, "开始测试MES上报功能"),
    test_device_status_report(),
    test_test_start_report(),
    test_test_completion_report(),
    ?LOG(info, "MES上报功能测试完成"),
    ok.

-spec test_device_status_report() -> ok.
test_device_status_report() ->
    ?LOG(info, "测试设备状态上报"),
    TestData = #{
        <<"trans_id">> => dgiot_uav_mes_utils:generate_mes_trans_id(),
        <<"station_info">> => <<"总测+拷机工位"/utf8>>
    },
    case dgiot_uav_mes_api:report_device_status(<<"ALM_MES">>, <<"A:总测01">>, <<"UAV001">>, TestData) of
        {ok, _} -> ?LOG(info, "设备状态上报测试通过");
        {error, Reason} -> ?LOG(error, "设备状态上报测试失败: ~p", [Reason])
    end,
    ok.

-spec test_test_start_report() -> ok.
test_test_start_report() ->
    ?LOG(info, "测试测试开始上报"),
    TestData = #{
        <<"trans_id">> => dgiot_uav_mes_utils:generate_mes_trans_id(),
        <<"drone_type">> => ?DRONE_TYPE_FINAL_TEST,
        <<"line_proc_no">> => <<"A-JC-03">>,
        <<"eqp_action_list">> => <<"机械臂上升"/utf8>>
    },
    case dgiot_uav_mes_api:report_test_start(<<"ALM_MES">>, <<"A:总测01">>, <<"UAV002">>, TestData) of
        {ok, _} -> ?LOG(info, "测试开始上报测试通过");
        {error, Reason} -> ?LOG(error, "测试开始上报测试失败: ~p", [Reason])
    end,
    ok.

-spec test_test_completion_report() -> ok.
test_test_completion_report() ->
    ?LOG(info, "测试测试完成上报"),
    TestResults = #{
        <<"parameters">> => [
            #{
                <<"name">> => <<"电压测量"/utf8>>,
                <<"upper">> => 26000,
                <<"lower">> => 22000,
                <<"standard">> => 24000,
                <<"value">> => 24500,
                <<"result">> => <<"合格"/utf8>>
            }
        ],
        <<"rpt_link">> => <<"/dgiot_file/test_report_001.docx">>
    },
    case dgiot_uav_mes_api:report_test_completion(<<"ALM_MES">>, <<"A:总测01">>, <<"UAV003">>, ?DRONE_TYPE_FINAL_TEST, TestResults) of
        {ok, _} -> ?LOG(info, "测试完成上报测试通过");
        {error, Reason} -> ?LOG(error, "测试完成上报测试失败: ~p", [Reason])
    end,
    ok.

-spec test_json_reading() -> ok.
test_json_reading() ->
    ?LOG(info, "开始测试JSON读取功能"),
    case get_mes_examples() of
        #{<<"item">> := Items} when is_list(Items) ->
            ?LOG(info, "成功读取MES示例数据，共 ~p 个示例", [length(Items)]);
        _ ->
            ?LOG(error, "读取MES示例数据失败")
    end,
    TestItems = get_test_items(),
    ?LOG(info, "成功读取测试项数据，共 ~p 个测试项", [length(TestItems)]),
    MesData = generate_mes_data_from_test_items(TestItems),
    ?LOG(info, "成功生成 ~p 个MES上报示例", [length(MesData)]),
    lists:foreach(fun(#{device_address := Addr, station_name := Station, test_steps_count := Count}) ->
        ?LOG(info, "测试项: ~p, 工位: ~p, 测试步骤: ~p", [Addr, Station, Count])
    end, lists:sublist(MesData, 3)),
    ?LOG(info, "JSON读取功能测试完成"),
    ok.

-spec test_mes_detailed() -> ok.
test_mes_detailed() ->
    io:format("=== 详细MES测试 ===~n~n"),
    io:format("1. 测试设备状态上报...~n"),
    TestData1 = #{
        <<"trans_id">> => <<"20260129175100123456">>,
        <<"station_info">> => <<"总测+拷机工位"/utf8>>
    },
    Result1 = dgiot_uav_mes_api:report_device_status(<<"ALM_MES">>, <<"A:总测01">>, <<"UAV001">>, TestData1),
    io:format("结果: ~p~n~n", [Result1]),

    io:format("2. 测试测试完成上报...~n"),
    TestResults = #{
        <<"parameters">> => [
            #{
                <<"name">> => <<"电压测量"/utf8>>,
                <<"upper">> => 26000,
                <<"lower">> => 22000,
                <<"standard">> => 24000,
                <<"value">> => 24500,
                <<"result">> => <<"合格"/utf8>>
            }
        ],
        <<"rpt_link">> => <<"/dgiot_file/test_report_001.docx">>
    },
    Result2 = dgiot_uav_mes_api:report_test_completion(<<"ALM_MES">>, <<"A:总测01">>, <<"UAV003">>, ?DRONE_TYPE_FINAL_TEST, TestResults),
    io:format("结果: ~p~n~n", [Result2]),

    io:format("3. 测试JSON读取功能...~n"),
    Result3 = test_json_reading(),
    io:format("结果: ~p~n~n", [Result3]),

    io:format("=== 测试完成 ===~n"),
    ok.