%%%-------------------------------------------------------------------
%%% @doc
%%% dgiot_browser_tools 模块 - 浏览器工具基础底座集成
%%%
%%% 提供网页端与本地协作的工具函数，类似dgiot_plugin:compile的标准化接口
%%% 支持文件上传、代码编译、调试、测试等功能
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dgiot_browser_tools).
-author("dgiot").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot.hrl").

%% API 导出
-export([
    compile/1,
    compile/2,
    debug/2,
    debug/3,
    test/2,
    test/3,
    apply_changes/1,
    sync_context/1,
    get_status/0,
    
    % 文件上传相关API
    upload_file/2,
    upload_file/3,
    process_uploaded_file/1,
    get_uploaded_file/1,
    list_uploaded_files/0,
    delete_uploaded_file/1,
    
    % PoW挑战相关API
    create_pow_challenge/0,
    verify_pow_response/2,
    
    % WASM文件管理
    get_wasm_file/1,
    list_wasm_files/0
]).

%% 模块加载时自动初始化
-on_load(on_load/0).

%%%===================================================================
%%% 类型定义
%%%===================================================================

-type file_upload_result() :: #{
    file_id     => binary(),
    file_name   => binary(),
    file_size   => integer(),
    upload_time => integer(),
    status      => binary()
}.

-type pow_challenge() :: #{
    challenge_id => binary(),
    algorithm    => binary(),
    difficulty   => integer(),
    timestamp    => integer(),
    expires_at   => integer(),
    nonce        => binary()
}.

%%%===================================================================
%%% 编译调试 API
%%%===================================================================

-spec compile(map()) -> {ok, map()} | {error, term()}.
compile(#{<<"file_path">> := FilePath, <<"code_content">> := CodeContent}) ->
    compile(FilePath, CodeContent).

-spec compile(binary(), binary()) -> {ok, map()} | {error, term()}.
compile(FilePath, CodeContent) ->
    ?LOG(info, "Browser Tools编译: ~p", [FilePath]),
    TempFile = save_to_temp_file(FilePath, CodeContent),
    App = extract_app_from_path(FilePath),
    Result = dgiot_plugin:compile(App),
    {ok, #{
        <<"result">>    => Result,
        <<"temp_file">> => TempFile,
        <<"app">>       => App,
        <<"timestamp">> => dgiot_datetime:now_secs()
    }}.

-spec debug(binary(), binary()) -> {ok, map()} | {error, term()}.
debug(FilePath, CodeContent) ->
    debug(FilePath, CodeContent, #{}).

-spec debug(binary(), binary(), map()) -> {ok, map()} | {error, term()}.
debug(FilePath, CodeContent, Options) ->
    ?LOG(info, "Browser Tools调试: ~p", [FilePath]),
    {ok, Applied} = apply_changes(#{<<"file_path">> => FilePath, <<"code_content">> => CodeContent}),
    DebugResult = execute_debug_command(FilePath, Options),
    {ok, #{
        <<"applied">>      => Applied,
        <<"debug_result">> => DebugResult,
        <<"options">>      => Options,
        <<"timestamp">>    => dgiot_datetime:now_secs()
    }}.

-spec test(binary(), binary()) -> {ok, map()} | {error, term()}.
test(FilePath, CodeContent) ->
    test(FilePath, CodeContent, #{}).

-spec test(binary(), binary(), map()) -> {ok, map()} | {error, term()}.
test(FilePath, CodeContent, Options) ->
    ?LOG(info, "Browser Tools测试: ~p", [FilePath]),
    {ok, Applied} = apply_changes(#{<<"file_path">> => FilePath, <<"code_content">> => CodeContent}),
    TestResult = execute_test_command(FilePath, Options),
    {ok, #{
        <<"applied">>     => Applied,
        <<"test_result">> => TestResult,
        <<"options">>     => Options,
        <<"timestamp">>   => dgiot_datetime:now_secs()
    }}.

-spec apply_changes(map()) -> {ok, map()} | {error, term()}.
apply_changes(#{<<"file_path">> := FilePath, <<"code_content">> := CodeContent}) ->
    ?LOG(info, "应用代码修改: ~p", [FilePath]),
    BackupFile = backup_original_file(FilePath),
    ok = file:write_file(FilePath, CodeContent),
    {ok, #{
        <<"file_path">>   => FilePath,
        <<"backup_file">> => BackupFile,
        <<"status">>      => <<"applied">>,
        <<"timestamp">>   => dgiot_datetime:now_secs()
    }}.

-spec sync_context(binary()) -> {ok, map()} | {error, term()}.
sync_context(ContextData) ->
    ?LOG(info, "同步上下文: ~p bytes", [byte_size(ContextData)]),
    CompressedContext = compress_context(ContextData),
    Key = save_to_context_cache(CompressedContext),
    {ok, #{
        <<"original_size">>   => byte_size(ContextData),
        <<"compressed_size">> => byte_size(CompressedContext),
        <<"compression_ratio">> => byte_size(CompressedContext) / byte_size(ContextData),
        <<"cache_key">>        => Key,
        <<"timestamp">>        => dgiot_datetime:now_secs()
    }}.

-spec get_status() -> map().
get_status() ->
    #{
        <<"tool_name">>      => <<"dgiot_browser_tools">>,
        <<"version">>        => <<"1.0.0">>,
        <<"functions">>      => [
            <<"compile/1">>, <<"compile/2">>,
            <<"debug/2">>,   <<"debug/3">>,
            <<"test/2">>,    <<"test/3">>,
            <<"apply_changes/1">>,
            <<"sync_context/1">>,
            <<"get_status/0">>,
            <<"upload_file/2">>, <<"upload_file/3">>,
            <<"process_uploaded_file/1">>,
            <<"get_uploaded_file/1">>,
            <<"list_uploaded_files/0">>,
            <<"delete_uploaded_file/1">>,
            <<"create_pow_challenge/0">>,
            <<"verify_pow_response/2">>,
            <<"get_wasm_file/1">>,
            <<"list_wasm_files/0">>
        ],
        <<"integrated_with">> => [
            <<"dgiot_plugin">>,
            <<"dgiot_web_collaboration">>
        ],
        <<"timestamp">>       => dgiot_datetime:now_secs()
    }.

%%%===================================================================
%%% 文件上传 API
%%%===================================================================

-spec upload_file(binary(), binary()) -> {ok, file_upload_result()} | {error, term()}.
upload_file(FileName, FileContent) ->
    upload_file(FileName, FileContent, #{}).

-spec upload_file(binary(), binary(), map()) -> {ok, file_upload_result()} | {error, term()}.
upload_file(FileName, FileContent, Options) ->
    ?LOG(info, "上传文件: ~p, 大小: ~p bytes", [FileName, byte_size(FileContent)]),
    FileId = generate_file_id(FileName),
    UploadDir = get_upload_dir(),
    FilePath = filename:join([UploadDir, FileId]),
    case file:write_file(FilePath, FileContent) of
        ok ->
            Metadata = #{
                file_id     => FileId,
                file_name   => FileName,
                file_size   => byte_size(FileContent),
                upload_time => dgiot_datetime:now_secs(),
                file_path   => list_to_binary(FilePath),
                options     => Options,
                status      => <<"uploaded">>
            },
            save_file_metadata(FileId, Metadata),
            {ok, Metadata};
        {error, Reason} ->
            ?LOG(error, "文件保存失败: ~p", [Reason]),
            {error, Reason}
    end.

-spec process_uploaded_file(binary()) -> {ok, map()} | {error, term()}.
process_uploaded_file(FileId) ->
    ?LOG(info, "处理上传的文件: ~p", [FileId]),
    case get_file_metadata(FileId) of
        {ok, Metadata} ->
            FilePath = binary_to_list(maps:get(file_path, Metadata)),
            {ok, FileContent} = file:read_file(FilePath),
            FileName = maps:get(file_name, Metadata),
            ProcessedResult = process_file_by_type(FileName, FileContent),
            UpdatedMetadata = Metadata#{
                status         => <<"processed">>,
                processed_time => dgiot_datetime:now_secs(),
                process_result => ProcessedResult
            },
            update_file_metadata(FileId, UpdatedMetadata),
            {ok, UpdatedMetadata};
        {error, not_found} ->
            {error, <<"file_not_found">>}
    end.

-spec get_uploaded_file(binary()) -> {ok, binary()} | {error, term()}.
get_uploaded_file(FileId) ->
    case get_file_metadata(FileId) of
        {ok, Metadata} ->
            FilePath = binary_to_list(maps:get(file_path, Metadata)),
            file:read_file(FilePath);
        {error, not_found} ->
            {error, <<"file_not_found">>}
    end.

-spec list_uploaded_files() -> {ok, [file_upload_result()]}.
list_uploaded_files() ->
    Files = get_all_file_metadata(),
    {ok, Files}.

-spec delete_uploaded_file(binary()) -> ok | {error, term()}.
delete_uploaded_file(FileId) ->
    case get_file_metadata(FileId) of
        {ok, Metadata} ->
            FilePath = binary_to_list(maps:get(file_path, Metadata)),
            file:delete(FilePath),
            delete_file_metadata(FileId),
            ok;
        {error, not_found} ->
            {error, <<"file_not_found">>}
    end.

%%%===================================================================
%%% PoW 挑战 API
%%%===================================================================

-spec create_pow_challenge() -> {ok, pow_challenge()}.
create_pow_challenge() ->
    ChallengeId = generate_challenge_id(),
    Timestamp = dgiot_datetime:now_secs(),
    Challenge = #{
        challenge_id => ChallengeId,
        algorithm    => <<"sha256">>,
        difficulty   => 4,
        timestamp    => Timestamp,
        expires_at   => Timestamp + 300,
        nonce        => generate_random_nonce()
    },
    save_pow_challenge(ChallengeId, Challenge),
    {ok, Challenge}.

-spec verify_pow_response(binary(), binary()) -> {ok, boolean()} | {error, binary()}.
verify_pow_response(ChallengeId, Response) ->
    case get_pow_challenge(ChallengeId) of
        {ok, Challenge} ->
            Now = dgiot_datetime:now_secs(),
            ExpiresAt = maps:get(expires_at, Challenge),
            if
                Now > ExpiresAt ->
                    {error, <<"challenge_expired">>};
                true ->
                    Nonce = maps:get(nonce, Challenge),
                    Difficulty = maps:get(difficulty, Challenge),
                    IsValid = verify_pow(Nonce, Response, Difficulty),
                    delete_pow_challenge(ChallengeId),
                    {ok, IsValid}
            end;
        {error, not_found} ->
            {error, <<"challenge_not_found">>}
    end.

%%%===================================================================
%%% WASM 文件管理 API
%%%===================================================================

-spec get_wasm_file(binary()) -> {ok, binary()} | {error, term()}.
get_wasm_file(FileName) ->
    WasmDir = get_wasm_dir(),
    FilePath = filename:join([WasmDir, FileName]),
    case file:read_file(FilePath) of
        {ok, Content} ->
            {ok, Content};
        {error, enoent} ->
            download_wasm_file(FileName);
        {error, Reason} ->
            {error, Reason}
    end.

-spec list_wasm_files() -> {ok, [binary()]} | {error, term()}.
list_wasm_files() ->
    WasmDir = get_wasm_dir(),
    case file:list_dir(WasmDir) of
        {ok, Files} ->
            WasmFiles = [list_to_binary(F) || F <- Files, filename:extension(F) =:= ".wasm"],
            {ok, WasmFiles};
        {error, enoent} ->
            {ok, []};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% 内部辅助函数
%%%===================================================================

save_to_temp_file(FilePath, CodeContent) ->
    TempDir = "/tmp/dgiot_browser_tools/",
    filelib:ensure_dir(TempDir),
    TempFile = filename:join([TempDir, filename:basename(FilePath)]),
    ok = file:write_file(TempFile, CodeContent),
    TempFile.

extract_app_from_path(FilePath) ->
    PathParts = filename:split(FilePath),
    case lists:member("apps", PathParts) of
        true ->
            Index = index_of("apps", PathParts),
            AppPart = lists:nth(Index + 1, PathParts),
            list_to_atom(AppPart);
        false ->
            unknown
    end.

index_of(Value, List) -> index_of(Value, List, 1).
index_of(_, [], _) -> 0;
index_of(Value, [Value|_], Index) -> Index;
index_of(Value, [_|Rest], Index) -> index_of(Value, Rest, Index + 1).

execute_debug_command(FilePath, Options) ->
    App = extract_app_from_path(FilePath),
    Command = maps:get(<<"command">>, Options, <<"debug">>),
    case Command of
        <<"debug">> ->
            {ok, #{<<"type">> => <<"erl_shell">>, <<"app">> => App}};
        <<"log">> ->
            {ok, #{<<"type">> => <<"log_view">>, <<"app">> => App}};
        _ ->
            {error, <<"unknown_debug_command">>}
    end.

execute_test_command(FilePath, Options) ->
    App = extract_app_from_path(FilePath),
    TestType = maps:get(<<"test_type">>, Options, <<"unit">>),
    case TestType of
        <<"unit">> ->
            {ok, #{<<"type">> => <<"unit_test">>, <<"app">> => App}};
        <<"integration">> ->
            {ok, #{<<"type">> => <<"integration_test">>, <<"app">> => App}};
        <<"performance">> ->
            {ok, #{<<"type">> => <<"performance_test">>, <<"app">> => App}};
        _ ->
            {error, <<"unknown_test_type">>}
    end.

backup_original_file(FilePath) ->
    BackupDir = "/tmp/dgiot_browser_tools/backups/",
    filelib:ensure_dir(BackupDir),
    BackupFile = filename:join([BackupDir, filename:basename(FilePath) ++ ".backup"]),
    case file:copy(FilePath, BackupFile) of
        {ok, _} -> BackupFile;
        {error, Reason} ->
            ?LOG(error, "备份文件失败: ~p", [Reason]),
            undefined
    end.

compress_context(ContextData) ->
    MaxSize = 128 * 1024,  % 128k
    if
        byte_size(ContextData) =< MaxSize -> ContextData;
        true -> binary:part(ContextData, 0, MaxSize)
    end.

save_to_context_cache(CompressedContext) ->
    Timestamp = dgiot_datetime:now_secs(),
    Key = <<"context_cache_", (integer_to_binary(Timestamp))/binary>>,
    ets:insert(browser_tools_context_cache, {Key, CompressedContext, Timestamp}),
    Key.

generate_file_id(FileName) ->
    Timestamp = integer_to_binary(dgiot_datetime:now_secs()),
    Random = integer_to_binary(rand:uniform(1000000)),
    <<"file_", Timestamp/binary, "_", Random/binary, "_", FileName/binary>>.

get_upload_dir() ->
    UploadDir = "/tmp/dgiot_browser_tools/uploads/",
    filelib:ensure_dir(UploadDir),
    UploadDir.

save_file_metadata(FileId, Metadata) ->
    ets:insert(browser_tools_file_metadata, {FileId, Metadata}).

get_file_metadata(FileId) ->
    case ets:lookup(browser_tools_file_metadata, FileId) of
        [{FileId, Metadata}] -> {ok, Metadata};
        [] -> {error, not_found}
    end.

get_all_file_metadata() ->
    ets:tab2list(browser_tools_file_metadata).

update_file_metadata(FileId, Metadata) ->
    ets:insert(browser_tools_file_metadata, {FileId, Metadata}).

delete_file_metadata(FileId) ->
    ets:delete(browser_tools_file_metadata, FileId).

process_file_by_type(FileName, FileContent) ->
    case filename:extension(FileName) of
        <<".erl">> -> #{<<"type">> => <<"erlang_source">>, <<"size">> => byte_size(FileContent)};
        <<".hrl">> -> #{<<"type">> => <<"erlang_header">>, <<"size">> => byte_size(FileContent)};
        <<".json">> -> #{<<"type">> => <<"json_data">>, <<"size">> => byte_size(FileContent)};
        <<".md">> -> #{<<"type">> => <<"markdown">>, <<"size">> => byte_size(FileContent)};
        <<".py">> -> #{<<"type">> => <<"python_script">>, <<"size">> => byte_size(FileContent)};
        <<".sh">> -> #{<<"type">> => <<"shell_script">>, <<"size">> => byte_size(FileContent)};
        _ -> #{<<"type">> => <<"unknown">>, <<"size">> => byte_size(FileContent)}
    end.

generate_challenge_id() ->
    Timestamp = integer_to_binary(dgiot_datetime:now_secs()),
    Random = integer_to_binary(rand:uniform(1000000)),
    <<"challenge_", Timestamp/binary, "_", Random/binary>>.

generate_random_nonce() ->
    <<Nonce:128>> = crypto:strong_rand_bytes(16),
    integer_to_binary(Nonce).

save_pow_challenge(ChallengeId, Challenge) ->
    ets:insert(browser_tools_pow_challenges, {ChallengeId, Challenge}).

get_pow_challenge(ChallengeId) ->
    case ets:lookup(browser_tools_pow_challenges, ChallengeId) of
        [{ChallengeId, Challenge}] -> {ok, Challenge};
        [] -> {error, not_found}
    end.

delete_pow_challenge(ChallengeId) ->
    ets:delete(browser_tools_pow_challenges, ChallengeId).

verify_pow(Nonce, Response, Difficulty) ->
    Hash = crypto:hash(sha256, <<Nonce/binary, Response/binary>>),
    HashBinary = binary:encode_hex(Hash),
    Prefix = binary:part(HashBinary, 0, Difficulty),
    case binary:match(Prefix, <<"0">>) of
        nomatch -> false;
        _ -> true
    end.

get_wasm_dir() ->
    WasmDir = "/tmp/dgiot_browser_tools/wasm/",
    filelib:ensure_dir(WasmDir),
    WasmDir.

download_wasm_file(FileName) ->
    ?LOG(info, "模拟下载WASM文件: ~p", [FileName]),
    MockWasm = <<0,97,115,109,1,0,0,0>>,
    WasmDir = get_wasm_dir(),
    FilePath = filename:join([WasmDir, FileName]),
    case file:write_file(FilePath, MockWasm) of
        ok -> {ok, MockWasm};
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% ETS 表初始化与生命周期
%%%===================================================================

init_ets() ->
    ets:new(browser_tools_file_metadata, [named_table, public, {keypos, 1}, {write_concurrency, true}]),
    ets:new(browser_tools_pow_challenges, [named_table, public, {keypos, 1}, {write_concurrency, true}]),
    ets:new(browser_tools_context_cache, [named_table, public, {keypos, 1}, {write_concurrency, true}]),
    ?LOG(info, "Browser Tools ETS表初始化完成"),
    ok.

start() ->
    ?LOG(info, "启动Browser Tools模块"),
    init_ets(),
    ok.

% stop() ->
%     ?LOG(info, "停止Browser Tools模块"),
%     ets:delete(browser_tools_file_metadata),
%     ets:delete(browser_tools_pow_challenges),
%     ets:delete(browser_tools_context_cache),
%     ok.

on_load() ->
    start().