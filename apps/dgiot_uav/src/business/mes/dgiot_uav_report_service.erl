%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_uav_report_service 模块 - 无人机测试报告服务
%%%
%%% 设计报告模板、实现PDF生成服务、报告存储和查询
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_uav_report_service).

%% API
-export([
    % 报告生成
    generate_test_report/1,
    generate_summary_report/1,
    generate_detailed_report/1,
    
    % Word报告生成
    generate_word_report/1,
    generate_word_report/2,
    save_word_report/2,
    get_word_report_url/1,
    
    % PDF生成
    create_pdf_report/1,
    save_pdf_report/2,
    get_pdf_report/1,
    
    % 报告存储和查询
    store_report/1,
    get_report/1,
    list_reports/2,
    delete_report/1,
    
    % 报告模板
    ensure_report_template/0,
    ensure_report_directory/0,
    
    % 测试
    test/0
]).

%% 内部函数
-export([]).

-include_lib("dgiot/include/logger.hrl").

%%%===================================================================

%%% 报告生成
%%%===================================================================

%% @doc 生成测试报告
generate_test_report(TestData) when is_map(TestData) ->
    ?LOG(info, "生成测试报告: test=~p", [maps:get(<<"testId">>, TestData, <<"unknown">>)]),
    
    % 提取测试信息
    TestId = maps:get(<<"testId">>, TestData, <<"unknown">>),
    DeviceId = maps:get(<<"deviceId">>, TestData, <<"unknown">>),
    TestType = maps:get(<<"testType">>, TestData, <<"basic">>),
    
    % 生成报告模板
    Report = #{
        <<"reportId">> => <<"report_", TestId/binary>>,
        <<"testId">> => TestId,
        <<"deviceId">> => DeviceId,
        <<"testType">> => TestType,
        <<"generatedAt">> => dgiot_datetime:now_secs(),
        <<"reportType">> => <<"test_report">>,
        <<"content">> => create_report_content(TestData)
    },
    
    {ok, Report}.

%% @doc 生成汇总报告
generate_summary_report(DeviceId) when is_binary(DeviceId) ->
    ?LOG(info, "生成汇总报告: device=~p", [DeviceId]),
    
    % 模拟汇总数据
    Summary = #{
        <<"reportId">> => <<"summary_", DeviceId/binary, "_", (dgiot_utils:random())/binary>>,
        <<"deviceId">> => DeviceId,
        <<"generatedAt">> => dgiot_datetime:now_secs(),
        <<"reportType">> => <<"summary_report">>,
        <<"content">> => #{
            <<"totalTests">> => 15,
            <<"passedTests">> => 12,
            <<"failedTests">> => 3,
            <<"successRate">> => 80.0,
            <<"lastTestTime">> => dgiot_datetime:now_secs() - 3600,
            <<"averageScore">> => 85.5,
            <<"recommendations">> => [
                <<"定期检查电池状态">>,
                <<"更新固件到最新版本">>,
                <<"加强通信信号稳定性">>
            ]
        }
    },
    
    {ok, Summary}.

%% @doc 生成详细报告
generate_detailed_report(ReportId) when is_binary(ReportId) ->
    ?LOG(info, "生成详细报告: report=~p", [ReportId]),
    
    % 模拟详细报告
    DetailedReport = #{
        <<"reportId">> => ReportId,
        <<"generatedAt">> => dgiot_datetime:now_secs(),
        <<"reportType">> => <<"detailed_report">>,
        <<"content">> => #{
            <<"executiveSummary">> => #{
                <<"overallResult">> => <<"PASSED">>,
                <<"score">> => 92,
                <<"recommendation">> => <<"设备状态良好，可投入使用">>
            },
            <<"testDetails">> => [
                #{
                    <<"category">> => <<"静态测试">>,
                    <<"tests">> => [
                        #{<<"name">> => <<"外观检查">>, <<"result">> => <<"PASSED">>, <<"score">> => 100},
                        #{<<"name">> => <<"结构检查">>, <<"result">> => <<"PASSED">>, <<"score">> => 95},
                        #{<<"name">> => <<"紧固件检查">>, <<"result">> => <<"PASSED">>, <<"score">> => 90}
                    ]
                },
                #{
                    <<"category">> => <<"动态测试">>,
                    <<"tests">> => [
                        #{<<"name">> => <<"电源序列测试">>, <<"result">> => <<"PASSED">>, <<"score">> => 88},
                        #{<<"name">> => <<"电机控制测试">>, <<"result">> => <<"PASSED">>, <<"score">> => 92},
                        #{<<"name">> => <<"通信链路测试">>, <<"result">> => <<"PASSED">>, <<"score">> => 85}
                    ]
                }
            ],
            <<"charts">> => #{
                <<"voltageChart">> => <<"base64_encoded_chart_data">>,
                <<"currentChart">> => <<"base64_encoded_chart_data">>,
                <<"temperatureChart">> => <<"base64_encoded_chart_data">>
            },
            <<"conclusion">> => <<"设备通过所有测试项目，符合使用标准">>
        }
    },
    
    {ok, DetailedReport}.

%%%===================================================================

%%% Word报告生成
%%%===================================================================

-define(REPORT_BASE_DIR, "/data/dgiot/nginx/html/reports").
-define(REPORT_WORD_DIR, "/word").
-define(REPORT_PDF_DIR, "/pdf").

-define(REPORT_BASE_URL_DEV, "http://127.0.0.1/reports").
-define(REPORT_BASE_URL_MES, "http://172.16.100.100/reports").

%% @doc 获取报告URL（根据环境自动选择）
get_report_base_url() ->
    Env = os:getenv("DGIOT_ENV"),
    BaseUrl = case Env of
        "mes" -> ?REPORT_BASE_URL_MES;
        "production" -> ?REPORT_BASE_URL_MES;
        _ -> ?REPORT_BASE_URL_DEV
    end,
    iolist_to_binary(BaseUrl).

%% @doc 生成Word和PDF报告
generate_word_report(TestData) when is_map(TestData) ->
    generate_word_report(TestData, #{}).

generate_word_report(TestData, Options) when is_map(TestData), is_map(Options) ->
    ?LOG(info, "生成Word和PDF测试报告", []),
    
    % 确保模板存在
    ok = ensure_report_template(),
    
    % 提取测试数据
    DeviceId = maps:get(<<"deviceId">>, TestData, <<"unknown">>),
    TestId = maps:get(<<"testId">>, TestData, <<>>),
    StationName = maps:get(<<"stationName">>, TestData, <<"总测1">>),
    TestResult = maps:get(<<"testResult">>, TestData, <<"通过">>),
    TestItems = maps:get(<<"testItems">>, TestData, []),
    Summary = maps:get(<<"summary">>, TestData, #{}),
    
    % 构建Word变量上下文
    Context = build_word_context(DeviceId, TestId, StationName, TestResult, TestItems, Summary),
    
    % 生成时间戳和文件名
    Timestamp = erlang:system_time(millisecond),
    {DateStr0, TimeStr0} = format_timestamp(),
    DateStr = iolist_to_binary(DateStr0),
    TimeStr = iolist_to_binary(TimeStr0),
    
    % 按飞机ID创建目录结构
    BaseDirBin = list_to_binary(?REPORT_BASE_DIR),
    WordDir = <<BaseDirBin/binary, "/", DeviceId/binary, (list_to_binary(?REPORT_WORD_DIR))/binary>>,
    PdfDir = <<BaseDirBin/binary, "/", DeviceId/binary, (list_to_binary(?REPORT_PDF_DIR))/binary>>,
    
    % 创建目录
    ok = create_report_dirs(WordDir, PdfDir),
    
    % 生成Word文件名
    WordFileName = <<DeviceId/binary, "_", DateStr/binary, "_", TimeStr/binary, ".docx">>,
    WordFilePath = <<WordDir/binary, "/", WordFileName/binary>>,
    
    % 调用Python脚本生成Word报告
    WordResult = generate_word_by_python(WordFileName, Context, WordDir),
    
    case WordResult of
        ok ->
            % 生成PDF
            PdfFileName = <<DeviceId/binary, "_", DateStr/binary, "_", TimeStr/binary, ".pdf">>,
            PdfFilePath = <<PdfDir/binary, "/", PdfFileName/binary>>,
            
            % 转换Word为PDF
            PdfResult = convert_word_to_pdf(WordFilePath, PdfFilePath, PdfDir),
            
            % 构建URL
            BaseUrl = get_report_base_url(),
            WordUrl = <<BaseUrl/binary, "/", DeviceId/binary, (list_to_binary(?REPORT_WORD_DIR))/binary, "/", WordFileName/binary>>,
            PdfUrl = case PdfResult of
                ok -> <<BaseUrl/binary, "/", DeviceId/binary, (list_to_binary(?REPORT_PDF_DIR))/binary, "/", PdfFileName/binary>>;
                _ -> <<>>
            end,
            
            ?LOG(info, "报告生成成功, Word: ~s, PDF: ~s", [WordUrl, PdfUrl]),
            {ok, #{
                <<"deviceId">> => DeviceId,
                <<"wordFileName">> => WordFileName,
                <<"wordFilePath">> => WordFilePath,
                <<"wordUrl">> => WordUrl,
                <<"pdfFileName">> => PdfFileName,
                <<"pdfFilePath">> => PdfFilePath,
                <<"pdfUrl">> => PdfUrl,
                <<"generatedAt">> => Timestamp
            }};
        {error, Reason} ->
            ?LOG(error, "报告生成失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 构建Word模板上下文
build_word_context(DeviceId, _TestId, StationName, TestResult, TestItems, _Summary) ->
    % 计算通过率
    TotalItems = length(TestItems),
    PassedItems = length([I || I <- TestItems, maps:get(<<"result">>, I, <<"failed">>) =:= <<"passed">>]),
    FailedItems = TotalItems - PassedItems,
    PassRate = case TotalItems of
        0 -> 0;
        _ -> round(PassedItems / TotalItems * 100)
    end,
    
    % 格式化日期时间（使用ASCII格式避免编码问题）
    {DateStr0, TimeStr0} = format_timestamp(),
    DateStr = iolist_to_binary(DateStr0),
    TimeStr = iolist_to_binary(TimeStr0),
    DateTimeBin = <<DateStr/binary, " ", TimeStr/binary>>,
    
    #{
        <<"device_id">> => binary_to_list(DeviceId),
        <<"device_name">> => binary_to_list(DeviceId),
        <<"product_name">> => <<"CJ-ZX-UAV-01">>,
        <<"测试日期"/utf8>> => DateStr,
        <<"测试时间"/utf8>> => TimeStr,
        <<"测试日期时间"/utf8>> => DateTimeBin,
        <<"测试人员"/utf8>> => <<"MES系统"/utf8>>,
        <<"station_name">> => binary_to_list(StationName),
        <<"test_result">> => binary_to_list(TestResult),
        <<"total_items">> => erlang:integer_to_list(TotalItems),
        <<"passed_items">> => erlang:integer_to_list(PassedItems),
        <<"failed_items">> => erlang:integer_to_list(FailedItems),
        <<"pass_rate">> => erlang:integer_to_list(PassRate)
    }.

%% @doc 通过Python脚本生成Word报告
generate_word_by_python(FileName, Context, Dir) ->
    ScriptPath = get_python_script_path(),
    TemplatePath = get_template_path(),
    OutputPath = <<Dir/binary, "/", FileName/binary>>,
    
    % 将Context转换为JSON
    JsonStr = map_to_json(Context),
    
    % 创建临时JSON文件（使用UTF-8编码）
    TempJsonFile = <<"/tmp/uav_report_context_", FileName/binary, ".json">>,
    ok = file:write_file(TempJsonFile, JsonStr, [{encoding, utf8}]),
    
    % 构建命令（转换为字符串）
    Cmd = io_lib:format(
        "cd ~s && python3 word_template_replacer_v2.py -t ~s -o ~s -j ~s",
        [ScriptPath, binary_to_list(TemplatePath), binary_to_list(OutputPath), binary_to_list(TempJsonFile)]
    ),
    
    ?LOG(info, "执行Word生成命令: ~s", [Cmd]),
    
    % 执行命令
    Result = os:cmd(Cmd),
    
    % 清理临时文件
    file:delete(TempJsonFile),
    
    case file:read_file(OutputPath) of
        {ok, _} ->
            ?LOG(info, "Word报告生成成功: ~s", [OutputPath]),
            ok;
        _ ->
            ?LOG(error, "Word报告生成失败,输出: ~s", [Result]),
            {error, Result}
    end.

%% @doc 格式化时间戳为日期和时间字符串
format_timestamp() ->
    Now = erlang:localtime(),
    {{Y, M, D}, {H, Min, S}} = Now,
    DateStr = io_lib:format("~4..0w~2..0w~2..0w", [Y, M, D]),
    TimeStr = io_lib:format("~2..0w~2..0w~2..0w", [H, Min, S]),
    {DateStr, TimeStr}.

%% @doc 创建报告目录结构
create_report_dirs(WordDir, PdfDir) ->
    % 创建Word目录
    case filelib:is_dir(WordDir) of
        true -> ok;
        false ->
            Cmd1 = io_lib:format("mkdir -p ~s", [WordDir]),
            ?LOG(info, "创建Word目录: ~s", [os:cmd(Cmd1)])
    end,
    % 创建PDF目录
    case filelib:is_dir(PdfDir) of
        true -> ok;
        false ->
            Cmd2 = io_lib:format("mkdir -p ~s", [PdfDir]),
            ?LOG(info, "创建PDF目录: ~s", [os:cmd(Cmd2)])
    end,
    ok.

%% @doc 将Word转换为PDF
convert_word_to_pdf(WordPath, PdfFileName, PdfDir) ->
    PdfPath = <<PdfDir/binary, "/", PdfFileName/binary>>,
    
    % 尝试使用LibreOffice转换
    Cmd = io_lib:format(
        "libreoffice --headless --convert-to pdf --outdir ~s ~s 2>/dev/null || echo 'LIBREOFFICE_NOT_FOUND'",
        [PdfDir, WordPath]
    ),
    
    ?LOG(info, "执行PDF转换命令: ~s", [Cmd]),
    
    Result = os:cmd(Cmd),
    
    % 检查PDF是否生成
    case file:read_file(PdfPath) of
        {ok, _} ->
            ?LOG(info, "PDF生成成功: ~s", [PdfPath]),
            ok;
        _ ->
            % LibreOffice不可用，尝试使用unoconv或其他工具
            Cmd2 = io_lib:format(
                "unoconv -f pdf -o ~s ~s 2>/dev/null || echo 'UNOCONV_NOT_FOUND'",
                [PdfPath, WordPath]
            ),
            Result2 = os:cmd(Cmd2),
            case file:read_file(PdfPath) of
                {ok, _} ->
                    ?LOG(info, "PDF生成成功(unoconv): ~s", [PdfPath]),
                    ok;
                _ ->
                    % 尝试使用python-docx2pdf
                    Cmd3 = io_lib:format(
                        "python3 -m docx2pdf ~s ~s 2>/dev/null || echo 'DOCX2PDF_NOT_FOUND'",
                        [WordPath, PdfPath]
                    ),
                    Result3 = os:cmd(Cmd3),
                    case file:read_file(PdfPath) of
                        {ok, _} ->
                            ?LOG(info, "PDF生成成功(python): ~s", [PdfPath]),
                            ok;
                        _ ->
                            ?LOG(warning, "PDF转换失败,LibreOffice/unoconv/python均不可用: ~s ~s ~s", [Result, Result2, Result3]),
                            {error, <<"PDF生成工具不可用">>}
                    end
            end
    end.

%% @doc 地图转JSON字符串
map_to_json(Map) ->
    Items = maps:to_list(Map),
    JsonPairs = lists:map(
        fun({K, V}) ->
            Key = case is_binary(K) of 
                true -> binary_to_list(K); 
                false -> K 
            end,
            Val = case is_binary(V) of 
                true -> binary_to_list(V); 
                false -> V 
            end,
            io_lib:format("\"~s\": \"~s\"", [Key, Val])
        end,
        Items
    ),
    JsonStr = "{" ++ string:join(JsonPairs, ",") ++ "}",
    unicode:characters_to_binary(JsonStr, utf8, utf8).

%% @doc 获取Python脚本路径
get_python_script_path() ->
    case os:getenv("DGIOT_UAV_SCRIPTS") of
        false -> "/root/gitee/dgiot/apps/dgiot_uav/priv/scripts";
        Path -> Path
    end.

%% @doc 获取模板路径
get_template_path() ->
    ScriptPath = get_python_script_path(),
    iolist_to_binary([ScriptPath, "/test_report_template.docx"]).

%% @doc 确保报告模板存在
ensure_report_template() ->
    TemplatePath = get_template_path(),
    case filelib:is_regular(TemplatePath) of
        true -> ok;
        false ->
            ?LOG(warning, "报告模板不存在,尝试创建: ~s", [TemplatePath]),
            ScriptPath = get_python_script_path(),
            Cmd = io_lib:format("cd ~s && python3 word_template_replacer_v2.py --create-template", [ScriptPath]),
            Result = os:cmd(Cmd),
            ?LOG(info, "模板创建结果: ~s", [Result]),
            ok
    end.

%% @doc 保存Word报告
save_word_report(ReportData, Options) ->
    generate_word_report(ReportData, Options).

%% @doc 获取Word报告URL
get_word_report_url(FileName) when is_binary(FileName) ->
    <<(get_report_base_url())/binary, "/", FileName/binary>>;
get_word_report_url(FileName) when is_list(FileName) ->
    get_word_report_url(erlang:list_to_binary(FileName)).

%%%===================================================================

%%% PDF生成
%%%===================================================================

%% @doc 创建PDF报告
create_pdf_report(ReportData) when is_map(ReportData) ->
    ?LOG(info, "创建PDF报告: report=~p", [maps:get(<<"reportId">>, ReportData, <<"unknown">>)]),
    
    ReportId = maps:get(<<"reportId">>, ReportData, <<"unknown">>),
    
    % 模拟PDF生成
    PdfContent = #{
        <<"pdfId">> => <<"pdf_", ReportId/binary>>,
        <<"reportId">> => ReportId,
        <<"generatedAt">> => dgiot_datetime:now_secs(),
        <<"fileName">> => <<"UAV_Test_Report_", ReportId/binary, ".pdf">>,
        <<"fileSize">> => 102400,  % 100KB
        <<"content">> => <<"PDF binary content would be here">>,
        <<"metadata">> => #{
            <<"pages">> => 12,
            <<"author">> => <<"DG-IoT UAV System">>,
            <<"title">> => <<"无人机测试报告">>,
            <<"subject">> => <<"设备测试与评估">>
        }
    },
    
    {ok, PdfContent}.

%% @doc 保存PDF报告
save_pdf_report(PdfData, StoragePath) when is_map(PdfData), is_binary(StoragePath) ->
    PdfId = maps:get(<<"pdfId">>, PdfData, <<"unknown">>),
    ?LOG(info, "保存PDF报告: pdf=~p, path=~p", [PdfId, StoragePath]),
    
    % 模拟保存操作
    SavedPdf = PdfData#{
        <<"storagePath">> => StoragePath,
        <<"savedAt">> => dgiot_datetime:now_secs(),
        <<"status">> => <<"saved">>
    },
    
    {ok, SavedPdf}.

%% @doc 获取PDF报告
get_pdf_report(PdfId) when is_binary(PdfId) ->
    ?LOG(info, "获取PDF报告: pdf=~p", [PdfId]),
    
    % 模拟PDF数据
    PdfData = #{
        <<"pdfId">> => PdfId,
        <<"reportId">> => <<"report_", (binary:part(PdfId, 4, byte_size(PdfId)-4))/binary>>,
        <<"fileName">> => <<"UAV_Test_Report_", PdfId/binary, ".pdf">>,
        <<"fileSize">> => 102400,
        <<"generatedAt">> => dgiot_datetime:now_secs() - 3600,
        <<"downloadUrl">> => <<"/api/v1/reports/pdf/", PdfId/binary>>,
        <<"metadata">> => #{
            <<"pages">> => 12,
            <<"author">> => <<"DG-IoT UAV System">>
        }
    },
    
    {ok, PdfData}.

%%%===================================================================

%%% 报告存储和查询
%%%===================================================================

%% @doc 存储报告
store_report(Report) when is_map(Report) ->
    ReportId = maps:get(<<"reportId">>, Report, <<"unknown">>),
    ?LOG(info, "存储报告: report=~p", [ReportId]),
    
    % 模拟存储操作
    StoredReport = Report#{
        <<"storedAt">> => dgiot_datetime:now_secs(),
        <<"storageId">> => <<"storage_", ReportId/binary>>,
        <<"status">> => <<"stored">>
    },
    
    {ok, StoredReport}.

%% @doc 获取报告
get_report(ReportId) when is_binary(ReportId) ->
    ?LOG(info, "获取报告: report=~p", [ReportId]),
    
    % 模拟报告数据
    Report = #{
        <<"reportId">> => ReportId,
        <<"testId">> => <<"test_", (binary:part(ReportId, 7, byte_size(ReportId)-7))/binary>>,
        <<"deviceId">> => <<"uav001">>,
        <<"testType">> => <<"full">>,
        <<"generatedAt">> => dgiot_datetime:now_secs() - 7200,
        <<"reportType">> => <<"test_report">>,
        <<"content">> => #{
            <<"overallResult">> => <<"PASSED">>,
            <<"score">> => 92,
            <<"details">> => [
                #{<<"category">> => <<"静态测试">>, <<"result">> => <<"PASSED">>},
                #{<<"category">> => <<"动态测试">>, <<"result">> => <<"PASSED">>}
            ]
        }
    },
    
    {ok, Report}.

%% @doc 列出报告
list_reports(DeviceId, Limit) when is_binary(DeviceId), is_integer(Limit) ->
    ?LOG(info, "列出报告: device=~p, limit=~p", [DeviceId, Limit]),
    
    % 模拟报告列表
    Reports = [
        #{
            <<"reportId">> => <<"report_001">>,
            <<"testId">> => <<"test_001">>,
            <<"deviceId">> => DeviceId,
            <<"testType">> => <<"basic">>,
            <<"generatedAt">> => dgiot_datetime:now_secs() - 86400,
            <<"overallResult">> => <<"PASSED">>,
            <<"score">> => 85
        },
        #{
            <<"reportId">> => <<"report_002">>,
            <<"testId">> => <<"test_002">>,
            <<"deviceId">> => DeviceId,
            <<"testType">> => <<"full">>,
            <<"generatedAt">> => dgiot_datetime:now_secs() - 43200,
            <<"overallResult">> => <<"PASSED">>,
            <<"score">> => 92
        },
        #{
            <<"reportId">> => <<"report_003">>,
            <<"testId">> => <<"test_003">>,
            <<"deviceId">> => DeviceId,
            <<"testType">> => <<"advanced">>,
            <<"generatedAt">> => dgiot_datetime:now_secs() - 21600,
            <<"overallResult">> => <<"FAILED">>,
            <<"score">> => 65
        }
    ],
    
    % 限制返回数量
    LimitedReports = lists:sublist(Reports, min(Limit, length(Reports))),
    
    Result = #{
        <<"deviceId">> => DeviceId,
        <<"totalReports">> => length(Reports),
        <<"returnedReports">> => length(LimitedReports),
        <<"reports">> => LimitedReports
    },
    
    {ok, Result}.

%% @doc 删除报告
delete_report(ReportId) when is_binary(ReportId) ->
    ?LOG(info, "删除报告: report=~p", [ReportId]),
    
    % 模拟删除操作
    Result = #{
        <<"reportId">> => ReportId,
        <<"deleted">> => true,
        <<"deletedAt">> => dgiot_datetime:now_secs()
    },
    
    {ok, Result}.

%%%===================================================================

%%% 测试函数
%%%===================================================================

test() ->
    ?LOG(info, "测试Word报告生成功能", []),
    
    % 检查目录
    ok = ensure_report_directory(),
    
    % 创建模板
    ok = ensure_report_template(),
    
    % 生成测试报告数据
    TestData = #{
        <<"deviceId">> => <<"UAV-TEST-001">>,
        <<"testId">> => <<"TEST-001">>,
        <<"stationName">> => <<"总测1">>,
        <<"testResult">> => <<"通过">>,
        <<"testItems">> => [
            #{<<"name">> => <<"外观检查">>, <<"result">> => <<"passed">>},
            #{<<"name">> => <<"电压测量">>, <<"result">> => <<"passed">>},
            #{<<"name">> => <<"通信测试">>, <<"result">> => <<"passed">>}
        ],
        <<"summary">> => #{
            <<"total">> => 3,
            <<"passed">> => 3,
            <<"failed">> => 0
        }
    },
    
    % 生成Word报告
    case generate_word_report(TestData) of
        {ok, ReportInfo} ->
            ?LOG(info, "测试报告生成成功: ~p", [ReportInfo]),
            {ok, ReportInfo};
        {error, Reason} ->
            ?LOG(error, "测试报告生成失败: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc 确保报告基础目录存在
ensure_report_directory() ->
    Dir = ?REPORT_BASE_DIR,
    case filelib:is_dir(Dir) of
        true -> 
            ?LOG(info, "报告基础目录已存在: ~s", [Dir]),
            ok;
        false ->
            ?LOG(warning, "报告基础目录不存在,尝试创建: ~s", [Dir]),
            Cmd = io_lib:format("mkdir -p ~s", [Dir]),
            Result = os:cmd(Cmd),
            ?LOG(info, "目录创建结果: ~s", [Result]),
            ok
    end.

%%%===================================================================

%%% 内部函数
%%%===================================================================

%% 创建报告内容
create_report_content(TestData) ->
    TestId = maps:get(<<"testId">>, TestData, <<"unknown">>),
    DeviceId = maps:get(<<"deviceId">>, TestData, <<"unknown">>),
    
    #{
        <<"header">> => #{
            <<"title">> => <<"无人机测试报告">>,
            <<"testId">> => TestId,
            <<"deviceId">> => DeviceId,
            <<"date">> => dgiot_datetime:format(dgiot_datetime:now_secs(), <<"YYYY-MM-DD HH:mm:ss">>)
        },
        <<"summary">> => #{
            <<"overallResult">> => <<"PASSED">>,
            <<"totalTests">> => 12,
            <<"passedTests">> => 12,
            <<"failedTests">> => 0,
            <<"successRate">> => 100.0
        },
        <<"testResults">> => [
            #{<<"test">> => <<"外观检查">>, <<"result">> => <<"PASSED">>, <<"details">> => <<"无异常">>},
            #{<<"test">> => <<"结构检查">>, <<"result">> => <<"PASSED">>, <<"details">> => <<"结构完整">>},
            #{<<"test">> => <<"电压测量">>, <<"result">> => <<"PASSED">>, <<"details">> => <<"24.0V (正常)">>},
            #{<<"test">> => <<"电流测量">>, <<"result">> => <<"PASSED">>, <<"details">> => <<"1.2A (正常)">>},
            #{<<"test">> => <<"通信测试">>, <<"result">> => <<"PASSED">>, <<"details">> => <<"连接稳定">>}
        ],
        <<"conclusion">> => #{
            <<"verdict">> => <<"设备通过所有测试项目">>,
            <<"recommendation">> => <<"可以投入使用">>,
            <<"nextTestDate">> => dgiot_datetime:now_secs() + 30 * 86400  % 30天后
        }
    }.
