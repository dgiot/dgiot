%%%-------------------------------------------------------------------
%%% @doc DGIOT对象管理技巧演示
%%% 展示如何使用最佳实践进行对象管理
%%% @end
%%%-------------------------------------------------------------------
-module(object_management_demo).
-author("DGIOT Team").
-export([
    demo_all/0,
    demo_user_management/0,
    demo_device_management/0,
    demo_test_data_management/0,
    demo_batch_operations/0,
    demo_error_handling/0
]).

-include_lib("dgiot/include/dgiot.hrl").
-include_lib("dgiot/include/logger.hrl").

%% @doc 运行所有演示
demo_all() ->
    ?LOG(info, "开始DGIOT对象管理技巧演示..."),
    
    Results = [
        demo_user_management(),
        demo_device_management(),
        demo_test_data_management(),
        demo_batch_operations(),
        demo_error_handling()
    ],
    
    ?LOG(info, "所有演示完成"),
    {ok, Results}.

%% @doc 演示用户管理技巧
demo_user_management() ->
    ?LOG(info, "=== 演示1: 用户管理技巧 ==="),
    
    %% 技巧1: 完整的用户创建流程
    UserName = <<"test_user_", (dgiot_utils:random())/binary>>,
    Password = <<"password123">>,
    Email = <<UserName/binary, "@example.com">>,
    
    case create_user_with_validation(UserName, Password, Email) of
        {ok, UserId} ->
            ?LOG(info, "✓ 用户创建成功: ~s", [UserId]),
            
            %% 技巧2: 创建用户会话
            case create_user_session(UserId) of
                {ok, SessionToken} ->
                    ?LOG(info, "✓ 用户会话创建成功: ~s", [SessionToken]),
                    
                    %% 技巧3: 创建用户角色
                    RoleName = <<"test_role_", (dgiot_utils:random())/binary>>,
                    case create_user_role(RoleName, UserId) of
                        {ok, RoleId} ->
                            ?LOG(info, "✓ 用户角色创建成功: ~s", [RoleId]),
                            
                            %% 技巧4: 设置用户ACL
                            case set_user_acl(UserId, RoleId) of
                                ok ->
                                    ?LOG(info, "✓ 用户ACL设置成功"),
                                    {ok, #{user_id => UserId, session_token => SessionToken, role_id => RoleId}};
                                {error, Reason} ->
                                    ?LOG(error, "✗ 用户ACL设置失败: ~p", [Reason]),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            ?LOG(error, "✗ 用户角色创建失败: ~p", [Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    ?LOG(error, "✗ 用户会话创建失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG(error, "✗ 用户创建失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 演示设备管理技巧
demo_device_management() ->
    ?LOG(info, "=== 演示2: 设备管理技巧 ==="),
    
    %% 技巧1: 创建产品
    ProductName = <<"test_product_", (dgiot_utils:random())/binary>>,
    case create_product(ProductName) of
        {ok, ProductId} ->
            ?LOG(info, "✓ 产品创建成功: ~s", [ProductId]),
            
            %% 技巧2: 创建设备（使用标准ID生成）
            DeviceAddr = <<"TEST_DEVICE_", (dgiot_utils:random())/binary>>,
            case create_device_with_id(ProductId, DeviceAddr) of
                {ok, DeviceId} ->
                    ?LOG(info, "✓ 设备创建成功: ~s", [DeviceId]),
                    
                    %% 技巧3: 创建设备影子数据
                    case create_device_shadow(DeviceId) of
                        ok ->
                            ?LOG(info, "✓ 设备影子数据创建成功"),
                            
                            %% 技巧4: 创建设备配置
                            case create_device_config(DeviceId) of
                                {ok, ConfigId} ->
                                    ?LOG(info, "✓ 设备配置创建成功: ~s", [ConfigId]),
                                    {ok, #{product_id => ProductId, device_id => DeviceId, config_id => ConfigId}};
                                {error, Reason} ->
                                    ?LOG(error, "✗ 设备配置创建失败: ~p", [Reason]),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            ?LOG(error, "✗ 设备影子数据创建失败: ~p", [Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    ?LOG(error, "✗ 设备创建失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG(error, "✗ 产品创建失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 演示测试数据管理技巧
demo_test_data_management() ->
    ?LOG(info, "=== 演示3: 测试数据管理技巧 ==="),
    
    %% 创建测试设备
    ProductId = <<"test_product_demo">>,
    DeviceAddr = <<"TEST_DEVICE_DEMO">>,
    
    case create_device_with_id(ProductId, DeviceAddr) of
        {ok, DeviceId} ->
            ?LOG(info, "测试设备创建成功: ~s", [DeviceId]),
            
            %% 技巧1: 创建测试任务
            Operator = <<"demo_operator">>,
            TestConfig = #{
                <<"test_type">> => <<"functional">>,
                <<"duration">> => 3600,
                <<"parameters">> => #{<<"voltage">> => 220, <<"current">> => 10}
            },
            
            case create_test_task(DeviceId, Operator, TestConfig) of
                {ok, TaskId} ->
                    ?LOG(info, "✓ 测试任务创建成功: ~s", [TaskId]),
                    
                    %% 技巧2: 创建测试步骤（批量）
                    Steps = [
                        #{<<"step_id">> => 1, <<"name">> => <<"初始化测试">>, <<"description">> => <<"设备初始化">>},
                        #{<<"step_id">> => 2, <<"name">> => <<"功能测试">>, <<"description">> => <<"基本功能验证">>},
                        #{<<"step_id">> => 3, <<"name">> => <<"性能测试">>, <<"description">> => <<"性能指标测试">>},
                        #{<<"step_id">> => 4, <<"name">> => <<"稳定性测试">>, <<"description">> => <<"长时间运行测试">>}
                    ],
                    
                    case create_test_steps(TaskId, Steps) of
                        {ok, StepIds} ->
                            ?LOG(info, "✓ 测试步骤创建成功: ~p 个步骤", [length(StepIds)]),
                            
                            %% 技巧3: 创建测试指标（关联数据）
                            StepId = lists:nth(2, StepIds),  %% 使用第二个步骤
                            Indications = [
                                #{<<"name">> => <<"响应时间">>, <<"unit">> => <<"ms">>, <<"min">> => 0, <<"max">> => 100},
                                #{<<"name">> => <<"成功率">>, <<"unit">> => <<"%">>, <<"min">> => 95, <<"max">> => 100},
                                #{<<"name">> => <<"吞吐量">>, <<"unit">> => <<"req/s">>, <<"min">> => 1000, <<"max">> => 5000}
                            ],
                            
                            case create_test_indications(StepId, Indications) of
                                {ok, IndicationIds} ->
                                    ?LOG(info, "✓ 测试指标创建成功: ~p 个指标", [length(IndicationIds)]),
                                    
                                    %% 技巧4: 创建测试结果
                                    Results = [
                                        #{<<"indication_id">> => lists:nth(1, IndicationIds), <<"value">> => 50, <<"qualified">> => true},
                                        #{<<"indication_id">> => lists:nth(2, IndicationIds), <<"value">> => 98.5, <<"qualified">> => true},
                                        #{<<"indication_id">> => lists:nth(3, IndicationIds), <<"value">> => 3500, <<"qualified">> => true}
                                    ],
                                    
                                    case create_test_results(TaskId, StepId, Results) of
                                        {ok, ResultIds} ->
                                            ?LOG(info, "✓ 测试结果创建成功: ~p 个结果", [length(ResultIds)]),
                                            
                                            %% 技巧5: 生成测试报告
                                            case generate_test_report(TaskId) of
                                                {ok, ReportId} ->
                                                    ?LOG(info, "✓ 测试报告生成成功: ~s", [ReportId]),
                                                    {ok, #{
                                                        task_id => TaskId,
                                                        step_ids => StepIds,
                                                        indication_ids => IndicationIds,
                                                        result_ids => ResultIds,
                                                        report_id => ReportId
                                                    }};
                                                {error, Reason} ->
                                                    ?LOG(error, "✗ 测试报告生成失败: ~p", [Reason]),
                                                    {error, Reason}
                                            end;
                                        {error, Reason} ->
                                            ?LOG(error, "✗ 测试结果创建失败: ~p", [Reason]),
                                            {error, Reason}
                                    end;
                                {error, Reason} ->
                                    ?LOG(error, "✗ 测试指标创建失败: ~p", [Reason]),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            ?LOG(error, "✗ 测试步骤创建失败: ~p", [Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    ?LOG(error, "✗ 测试任务创建失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG(error, "✗ 测试设备创建失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 演示批量操作技巧
demo_batch_operations() ->
    ?LOG(info, "=== 演示4: 批量操作技巧 ==="),
    
    %% 技巧1: 批量创建字典数据
    DictEntries = [
        #{<<"type">> => <<"system">>, <<"key">> => <<"app_name">>, <<"value">> => <<"DGIOT Demo">>},
        #{<<"type">> => <<"system">>, <<"key">> => <<"app_version">>, <<"value">> => <<"1.0.0">>},
        #{<<"type">> => <<"config">>, <<"key">> => <<"max_connections">>, <<"value">> => 1000},
        #{<<"type">> => <<"config">>, <<"key">> => <<"timeout">>, <<"value">> => 30},
        #{<<"type">> => <<"ui">>, <<"key">> => <<"theme">>, <<"value">> => <<"dark">>},
        #{<<"type">> => <<"ui">>, <<"key">> => <<"language">>, <<"value">> => <<"zh-CN">>}
    ],
    
    case batch_create_dicts(DictEntries) of
        {ok, DictIds} ->
            ?LOG(info, "✓ 批量字典创建成功: ~p 个条目", [length(DictIds)]),
            
            %% 技巧2: 批量更新操作
            Updates = [
                #{<<"objectId">> => DictId, <<"value">> => <<"updated_value">>}
                || DictId <- DictIds
            ],
            
            case batch_update_dicts(Updates) of
                {ok, UpdateResults} ->
                    ?LOG(info, "✓ 批量更新成功: ~p 个更新", [length(UpdateResults)]),
                    
                    %% 技巧3: 批量查询验证
                    case batch_query_dicts(DictIds) of
                        {ok, QueryResults} ->
                            ?LOG(info, "✓ 批量查询成功: ~p 个结果", [length(QueryResults)]),
                            {ok, #{dict_ids => DictIds, update_results => UpdateResults, query_results => QueryResults}};
                        {error, Reason} ->
                            ?LOG(error, "✗ 批量查询失败: ~p", [Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    ?LOG(error, "✗ 批量更新失败: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG(error, "✗ 批量字典创建失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 演示错误处理技巧
demo_error_handling() ->
    ?LOG(info, "=== 演示5: 错误处理技巧 ==="),
    
    %% 技巧1: 重复对象创建错误处理
    UserName = <<"duplicate_test_user">>,
    Password = <<"password123">>,
    Email = <<UserName/binary, "@example.com">>,
    
    %% 第一次创建应该成功
    case create_user_simple(UserName, Password, Email) of
        {ok, UserId1} ->
            ?LOG(info, "✓ 第一次用户创建成功: ~s", [UserId1]),
            
            %% 第二次创建应该失败（重复）
            case create_user_simple(UserName, Password, Email) of
                {error, #{<<"code">> := 137}} ->
                    ?LOG(info, "✓ 重复创建错误处理成功: 检测到重复对象错误 (code: 137)"),
                    
                    %% 技巧2: 重试机制演示
                    case create_with_retry(<<"TestClass">>, #{<<"unique_field">> => dgiot_utils:random()}, 3) of
                        {ok, ObjectId} ->
                            ?LOG(info, "✓ 重试机制演示成功: 创建对象 ~s", [ObjectId]),
                            
                            %% 技巧3: 错误分类处理
                            TestCases = [
                                {<<"invalid_class">>, #{<<"field">> => <<"value">>}, 119},  %% 权限错误
                                {<<"_User">>, #{}, 107},  %% 无效参数错误
                                {<<"NonExistentClass">>, #{<<"objectId">> => <<"nonexistent">>}, 101}  %% 对象未找到
                            ],
                            
                            ErrorResults = lists:map(
                                fun({Class, Data, ExpectedCode}) ->
                                    case dgiot_parse:create_object(Class, Data) of
                                        {error, #{<<"code">> := Code}} when Code == ExpectedCode ->
                                            {ok, {Class, Code}};
                                        {error, Reason} ->
                                            {error, {Class, Reason}};
                                        Result ->
                                            {unexpected, Result}
                                    end
                                end,
                                TestCases
                            ),
                            
                            ?LOG(info, "✓ 错误分类处理完成: ~p", [ErrorResults]),
                            {ok, #{user_id => UserId1, retry_object => ObjectId, error_results => ErrorResults}};
                        {error, Reason} ->
                            ?LOG(error, "✗ 重试机制失败: ~p", [Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    ?LOG(error, "✗ 重复创建错误处理失败: ~p", [Reason]),
                    {error, Reason};
                Result ->
                    ?LOG(error, "✗ 预期错误未发生: ~p", [Result]),
                    {error, unexpected_result}
            end;
        {error, Reason} ->
            ?LOG(error, "✗ 第一次用户创建失败: ~p", [Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% 内部实现函数
%%%===================================================================

%% @doc 创建用户（带验证）
create_user_with_validation(UserName, Password, Email) ->
    %% 技巧: 数据验证
    case validate_user_data(UserName, Password, Email) of
        {ok, ValidatedData} ->
            %% 技巧: 检查用户是否已存在
            case check_user_exists(UserName, Email) of
                {ok, not_exists} ->
                    %% 技巧: 完整用户数据结构
                    UserData = ValidatedData#{
                        <<"nick">> => UserName,
                        <<"phone">> => <<>>,
                        <<"profile">> => #{<<"demo">> => true},
                        <<"createdat">> => dgiot_datetime:now_secs(),
                        <<"updatedat">> => dgiot_datetime:now_secs()
                    },
                    
                    %% 技巧: 错误处理
                    handle_create_result(dgiot_parse:create_object(<<"_User">>, UserData));
                {ok, exists} ->
                    {error, user_already_exists};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建用户（简单版）
create_user_simple(UserName, Password, Email) ->
    UserData = #{
        <<"username">> => UserName,
        <<"password">> => Password,
        <<"email">> => Email,
        <<"createdat">> => dgiot_datetime:now_secs()
    },
    dgiot_parse:create_object(<<"_User">>, UserData).

%% @doc 创建用户会话
create_user_session(UserId) ->
    SessionToken = <<"demo_token_", (dgiot_utils:random())/binary>>,
    TTL = 3600,
    
    SessionData = #{
        <<"objectId">> => dgiot_parse_id:get_sessionId(SessionToken),
        <<"sessionToken">> => SessionToken,
        <<"user">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"_User">>,
            <<"objectId">> => UserId
        },
        <<"expiresAt">> => #{
            <<"__type">> => <<"Date">>,
            <<"iso">> => dgiot_datetime:format(dgiot_datetime:nowstamp() + TTL, <<"YY-MM-DDTHH:NN:SS.000Z">>)
        }
    },
    
    case dgiot_parse:create_object(<<"_Session">>, SessionData) of
        {ok, #{<<"objectId">> := _SessionId}} ->
            {ok, SessionToken};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建用户角色
create_user_role(RoleName, UserId) ->
    RoleData = #{
        <<"name">> => RoleName,
        <<"users">> => #{
            <<"__op">> => <<"AddRelation">>,
            <<"objects">> => [#{
                <<"__type">> => <<"Pointer">>,
                <<"className">> => <<"_User">>,
                <<"objectId">> => UserId
            }]
        },
        <<"createdat">> => dgiot_datetime:now_secs()
    },
    
    case dgiot_parse:create_object(<<"_Role">>, RoleData) of
        {ok, #{<<"objectId">> := RoleId}} ->
            {ok, RoleId};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 设置用户ACL
set_user_acl(UserId, RoleId) ->
    ACL = #{
        UserId => #{<<"read">> => true, <<"write">> => true},
        RoleId => #{<<"read">> => true, <<"write">> => false}
    },
    
    case dgiot_parse:update_object(<<"_User">>, UserId, #{<<"ACL">> => ACL}) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 创建产品
create_product(ProductName) ->
    ProductData = #{
        <<"name">> => ProductName,
        <<"devType">> => <<"demo_device">>,
        <<"dynamicReg">> => true,
        <<"createdat">> => dgiot_datetime:now_secs()
    },
    
    case dgiot_parse:create_object(<<"Product">>, ProductData) of
        {ok, #{<<"objectId">> := ProductId}} ->
            {ok, ProductId};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建设备（使用标准ID生成）
create_device_with_id(ProductId, DeviceAddr) ->
    DeviceId = dgiot_parse_id:get_deviceid(ProductId, DeviceAddr),
    
    DeviceData = #{
        <<"objectId">> => DeviceId,
        <<"devaddr">> => DeviceAddr,
        <<"product">> => #{
            <<"__type">> => <<"Pointer">>,
            <<"className">> => <<"Product">>,
            <<"objectId">> => ProductId
        },
        <<"status">> => <<"offline">>,
        <<"name">> => <<"Demo Device">>,
        <<"createdat">> => dgiot_datetime:now_secs()
    },
    
    case dgiot_parse:create_object(<<"Device">>, DeviceData) of
        {ok, #{<<"objectId">> := CreatedId}} ->
            {ok, CreatedId};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建设备影子数据
create_device_shadow(DeviceId) ->
    ShadowData = #{
        <<"device_id">> => DeviceId,
        <<"status">> => <<"initialized">>,
        <<"last_heartbeat">> => dgiot_datetime:now_secs(),
        <<"createdat">> => dgiot_datetime:now_secs()
    },
    
    case dgiot_parse:create_object(<<"DeviceShadow">>, ShadowData) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 创建设备配置
create_device_config(DeviceId) ->
    ConfigId = <<"config_", DeviceId/binary>>,
    
    ConfigData = #{
        <<"objectId">> => ConfigId,
        <<"device_id">> => DeviceId,
        <<"config">> => #{
            <<"polling_interval">> => 30,
            <<"timeout">> => 10,
            <<"retry_count">> => 3
        },
        <<"createdat">> => dgiot_datetime:now_secs()
    },
    
    case dgiot_parse:create_object(<<"DeviceConfig">>, ConfigData) of
        {ok, #{<<"objectId">> := CreatedId}} ->
            {ok, CreatedId};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建测试任务
create_test_task(DeviceId, Operator, Config) ->
    TaskId = dgiot_utils:random(),
    
    TaskData = #{
        <<"objectId">> => TaskId,
        <<"device_id">> => DeviceId,
        <<"operator">> => Operator,
        <<"config">> => Config,
        <<"status">> => <<"not_started">>,
        <<"createdat">> => dgiot_datetime:now_secs()
    },
    
    case dgiot_parse:create_object(<<"UAVTestTask">>, TaskData) of
        {ok, #{<<"objectId">> := CreatedId}} ->
            {ok, CreatedId};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 创建测试步骤
create_test_steps(TaskId, Steps) ->
    StepIds = lists:map(
        fun(StepConfig) ->
            StepId = dgiot_utils:random(),
            StepData = maps:merge(StepConfig, #{
                <<"objectId">> => StepId,
                <<"task_id">> => TaskId,
                <<"status">> => <<"pending">>,
                <<"createdat">> => dgiot_datetime:now_secs()
            }),
            
            case dgiot_parse:create_object(<<"UAVTestStep">>, StepData) of
                {ok, #{<<"objectId">> := CreatedId}} ->
                    CreatedId;
                {error, Reason} ->
                    throw({error, Reason})
            end
        end,
        Steps
    ),
    
    {ok, StepIds}.

%% @doc 创建测试指标
create_test_indications(StepId, Indications) ->
    IndicationIds = lists:map(
        fun(IndicationConfig) ->
            IndicationId = dgiot_utils:random(),
            IndicationData = maps:merge(IndicationConfig, #{
                <<"objectId">> => IndicationId,
                <<"step_id">> => StepId,
                <<"qualified">> => false,
                <<"createdat">> => dgiot_datetime:now_secs()
            }),
            
            case dgiot_parse:create_object(<<"UAVTestIndication">>, IndicationData) of
                {ok, #{<<"objectId">> := CreatedId}} ->
                    CreatedId;
                {error, Reason} ->
                    throw({error, Reason})
            end
        end,
        Indications
    ),
    
    {ok, IndicationIds}.

%% @doc 创建测试结果
create_test_results(TaskId, StepId, Results) ->
    ResultIds = lists:map(
        fun(ResultConfig) ->
            ResultId = dgiot_utils:random(),
            ResultData = maps:merge(ResultConfig, #{
                <<"objectId">> => ResultId,
                <<"task_id">> => TaskId,
                <<"step_id">> => StepId,
                <<"createdat">> => dgiot_datetime:now_secs()
            }),
            
            case dgiot_parse:create_object(<<"UAVTestResult">>, ResultData) of
                {ok, #{<<"objectId">> := CreatedId}} ->
                    CreatedId;
                {error, Reason} ->
                    throw({error, Reason})
            end
        end,
        Results
    ),
    
    {ok, ResultIds}.

%% @doc 生成测试报告
generate_test_report(TaskId) ->
    ReportId = dgiot_utils:random(),
    
    ReportData = #{
        <<"objectId">> => ReportId,
        <<"task_id">> => TaskId,
        <<"report_data">> => #{
            <<"summary">> => <<"Demo test report">>,
            <<"generated_at">> => dgiot_datetime:now_secs()
        },
        <<"createdat">> => dgiot_datetime:now_secs()
    },
    
    case dgiot_parse:create_object(<<"UAVTestReport">>, ReportData) of
        {ok, #{<<"objectId">> := CreatedId}} ->
            {ok, CreatedId};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 批量创建字典
batch_create_dicts(DictEntries) ->
    Requests = [
        #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"/classes/Dict">>,
            <<"body">> => maps:merge(Entry, #{<<"createdat">> => dgiot_datetime:now_secs()})
        }
        || Entry <- DictEntries
    ],
    
    case dgiot_parse:batch(Requests) of
        {ok, Results} ->
            DictIds = lists:map(
                fun(#{<<"success">> := #{<<"objectId">> := ObjectId}}) ->
                    ObjectId
                end,
                Results
            ),
            {ok, DictIds};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 批量更新字典
batch_update_dicts(Updates) ->
    Requests = [
        #{
            <<"method">> => <<"PUT">>,
            <<"path">> => <<"/classes/Dict/", ObjectId/binary>>,
            <<"body">> => maps:merge(Update, #{<<"updatedat">> => dgiot_datetime:now_secs()})
        }
        || #{<<"objectId">> := ObjectId} = Update <- Updates
    ],
    
    case dgiot_parse:batch(Requests) of
        {ok, Results} ->
            {ok, Results};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 批量查询字典
batch_query_dicts(DictIds) ->
    Requests = [
        #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"/classes/Dict/", DictId/binary>>
        }
        || DictId <- DictIds
    ],
    
    case dgiot_parse:batch(Requests) of
        {ok, Results} ->
            {ok, Results};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 重试创建
create_with_retry(Class, Data, MaxRetries) ->
    create_with_retry(Class, Data, MaxRetries, 0).

create_with_retry(_Class, _Data, MaxRetries, Attempt) when Attempt >= MaxRetries ->
    {error, max_retries_exceeded};
create_with_retry(Class, Data, MaxRetries, Attempt) ->
    case dgiot_parse:create_object(Class, Data) of
        {ok, Result} -> {ok, Result};
        {error, _} ->
            timer:sleep(100 * Attempt),
            create_with_retry(Class, Data, MaxRetries, Attempt + 1)
    end.

%% @doc 验证用户数据
validate_user_data(UserName, Password, Email) ->
    RequiredFields = [
        {<<"username">>, UserName},
        {<<"password">>, Password},
        {<<"email">>, Email}
    ],
    
    lists:foreach(
        fun({Field, Value}) ->
            case is_binary(Value) andalso byte_size(Value) > 0 of
                true -> ok;
                false -> throw({error, {invalid_field, Field}})
            end
        end,
        RequiredFields
    ),
    
    {ok, #{
        <<"username">> => UserName,
        <<"password">> => Password,
        <<"email">> => Email
    }}.

%% @doc 检查用户是否存在
check_user_exists(UserName, Email) ->
    Query = #{
        <<"where">> => #{
            <<"$or">> => [
                #{<<"username">> => UserName},
                #{<<"email">> => Email}
            ]
        },
        <<"limit">> => 1
    },
    
    case dgiot_parse:query_object(<<"_User">>, Query) of
        {ok, #{<<"results">> := []}} ->
            {ok, not_exists};
        {ok, #{<<"results">> := [_]}} ->
            {ok, exists};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 处理创建结果
handle_create_result(Result) ->
    case Result of
        {ok, #{<<"objectId">> := ObjectId}} ->
            {ok, ObjectId};
        {error, Reason} ->
            {error, Reason}
    end.
