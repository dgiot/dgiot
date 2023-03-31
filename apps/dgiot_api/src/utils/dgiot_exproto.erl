%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_exproto).
-include_lib("dgiot/include/logger.hrl").
-compile({no_auto_import, [register/1]}).

-define(SERVER, ?MODULE).
-define(DGIOT_EXPROTO_PYTHON, dgiot_exproto_python).

%% APIs: Connection level
-export([
      do_python/5
    , get_exproto/3
    , post_exproto/4
    , put_exproto/5
    , delete_exproto/3
    , get_release_exproto/5
    , get_type/0
    , do_exproto/4
]).


%%--------------------------------------------------------------------
%% Helper
%%--------------------------------------------------------------------
do_exproto(OperationId, #{<<"exprotomode">> := <<"async">>} = Args, #{<<"sessionToken">> := SessionToken}, #{headers :=  #{<<"host">> := Host}}) ->
    case dgiot_data:get({ext,dgiot_utils:to_binary(OperationId)}) of
        not_find ->
            pass;
        {Mod,python} ->
            BinMod = dgiot_utils:to_binary(Mod),
            BinFun = dgiot_utils:to_binary(OperationId),
            Payload = #{
                <<"mod">> => BinMod,
                <<"fun">> => BinFun,
                <<"args">> => Args,
                <<"session">> => SessionToken,
                <<"host">> => Host
            },
            Topic = <<SessionToken/binary, "/", BinFun/binary ,"_",  "python_",BinMod/binary>>,
            dgiot_mqtt:publish(self(), <<"dgiot_exproto_api">>, jsx:encode(Payload)),
            {ok, #{<<"results">> => #{<<"topic">> => Topic}}};
        _ ->
            {error, <<"Not Allowed.">>}
    end;

do_exproto(OperationId, Args, #{<<"sessionToken">> := SessionToken}, #{headers :=  #{<<"host">> := Host}}) ->
    case dgiot_data:get({ext,dgiot_utils:to_binary(OperationId)}) of
        not_find ->
            pass;
        {Mod,python} ->
            Payload = dgiot_exproto:do_python(Mod, OperationId, Args, SessionToken, Host),
            Data = cow_base64url:decode(Payload),
            Result =
                case jsx:is_json(Data) of
                    true ->
                        jsx:decode(Data);
                    _ -> Data
                end,
            {ok, #{<<"results">> => Result}};
        _ ->
            {error, <<"Not Allowed.">>}
    end.

do_python(Mod, OperationId, Args, SessionToken, Host) ->
    {file, Here} = code:is_loaded(Mod),
    Dir = filename:dirname(filename:dirname(Here)),
    Path = dgiot_httpc:url_join([Dir, "/priv/exproto/release"]),
    ?LOG(info,"Path ~p", [Path]),
    NewPid =
        case dgiot_data:get(?DGIOT_EXPROTO_PYTHON, {python_api, SessionToken}) of
            not_find ->
                {ok, Pid} = python:start([{python, get_type()}, {python_path, Path},{call_timeout,60 * 1000 * 30}]),
                dgiot_data:insert(?DGIOT_EXPROTO_PYTHON, {python_api, SessionToken}, {Pid, dgiot_datetime:nowstamp()}),
                Pid;
            {OldPid, _TS} -> OldPid
        end,
    Env = maps:without([<<"rules">>, <<"menu">>], dgiot_auth:get_session(SessionToken)),
    Roles = maps:values(maps:get(<<"roles">>, Env)),
    python:call(NewPid, list_to_atom(dgiot_utils:to_list(OperationId)), do_exproto,
        [base64:encode(jsx:encode(Args)), SessionToken, base64:encode(jsx:encode(Env#{<<"roles">> => Roles, <<"host">> => Host}))]
    ).

get_exproto(Type, Mod, <<"all">>) ->
    get_exproto(Type, Mod, <<"debug">>) ++ get_exproto(Type, Mod, <<"release">>);

get_exproto(Type, Mod, Version) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    CurrDir = Dir ++ "/priv/" ++ dgiot_utils:to_list(Version) ++ "/",
    ?LOG(info,"CurrDir ~p", [CurrDir]),
    Extension =
        case Type of
            <<"python">> -> <<".py">>;
            <<"java">> -> <<".java">>;
            _ -> <<".py">>
        end,
    F =
        fun(F, {I, Acc}) ->
            ?LOG(info,"F ~p ", [F]),
            {ok, Data} = file:read_file(F),
            Name = dgiot_utils:to_binary(re:replace(filename:basename(F), Extension, <<>>, [{return, binary}])),
            IsFile =
                case Mod of
                    <<"all">> ->
                        true;
                    Name ->
                        true;
                    _ ->
                        false
                end,
            case IsFile of
                false ->
                    {I, Acc};
                true ->
                    SwaggerFile = CurrDir ++ "swagger_" ++ dgiot_utils:to_list(Type) ++ "_" ++ dgiot_utils:to_list(Name) ++ ".json",
                    case file:read_file(SwaggerFile) of
                        {ok, SwaggerData} ->
                            SwaggerJson = jsx:decode(SwaggerData, [{labels, binary}, return_maps]),
                            {I + 1, [#{
                                <<"type">> => Type,
                                <<"mod">> => Name,
                                <<"code">> => base64:encode(Data),
                                <<"swagger">> => SwaggerJson,
                                <<"version">> => Version
                            } | Acc]};
                        _ ->
                            {I, Acc}
                    end
            end
        end,
    {_Count, Files} = filelib:fold_files(CurrDir, dgiot_utils:to_list(Extension), true, F, {0, []}),
    Files.

post_exproto(Type, Mod, Code, Swagger) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    CurrDir = Dir ++ "/priv/debug/",
    Extension =
        case Type of
            <<"python">> -> <<".py">>;
            <<"java">> -> <<".java">>;
            _ -> <<".py">>
        end,
    F =
        fun(F, Acc) ->
            {ok, _Data} = file:read_file(F),
            Name = dgiot_utils:to_binary(re:replace(filename:basename(F), Extension, <<>>, [{return, binary}])),
            case Mod of
                Name -> Acc * 0;
                _ -> Acc * 1
            end
        end,
    IsFindMod = filelib:fold_files(CurrDir, dgiot_utils:to_list(Extension), true, F, 1),
    case IsFindMod of
        0 ->
            <<"mod exist">>;
        1 ->
            CodeFile = CurrDir ++ dgiot_utils:to_list(Mod) ++ dgiot_utils:to_list(Extension),
            Data = base64:decode(Code),
            ok = file:write_file(CodeFile, dgiot_utils:to_binary(Data)),
            SwaggerFile = CurrDir ++ "swagger_" ++ dgiot_utils:to_list(Type) ++ "_" ++ dgiot_utils:to_list(Mod) ++ ".json",
            ok = file:write_file(SwaggerFile, jsx:encode(Swagger)),
            <<"post succeed">>
    end.

put_exproto(Type, Mod, Code, Swagger, SessionToken) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    CurrDir = Dir ++ "/priv/debug/",
    Extension =
        case Type of
            <<"python">> -> <<".py">>;
            <<"java">> -> <<".java">>;
            _ -> <<".py">>
        end,
    F =
        fun(F, Acc) ->
            {ok, _Data} = file:read_file(F),
            Name = dgiot_utils:to_binary(re:replace(filename:basename(F), Extension, <<>>, [{return, binary}])),
            case Mod of
                Name -> Acc + 1;
                _ -> Acc + 0
            end
        end,
    IsFindMod = filelib:fold_files(CurrDir, dgiot_utils:to_list(Extension), true, F, 0),
    case IsFindMod of
        0 -> <<"mod not exist">>;
        _ ->
            CodeFile = CurrDir ++ dgiot_utils:to_list(Mod) ++ dgiot_utils:to_list(Extension),
            Data = base64:decode(Code),
            ok = file:write_file(CodeFile, Data),
            SwaggerFile = CurrDir ++ "swagger_" ++ dgiot_utils:to_list(Type) ++ "_" ++ dgiot_utils:to_list(Mod) ++ ".json",
            ok = file:write_file(SwaggerFile, jsx:encode(Swagger)),
            dgiot_utils:to_binary(test_python(Mod, SessionToken))
    end.

delete_exproto(Type, Mod, <<"all">>) ->
    delete_exproto(Type, Mod, <<"debug">>),
    delete_exproto(Type, Mod, <<"release">>);

delete_exproto(Type, Mod, Version) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    CurrDir = Dir ++ "/priv/" ++ dgiot_utils:to_list(Version) ++ "/",
    Extension =
        case Type of
            <<"python">> -> <<".py">>;
            <<"java">> -> <<".java">>;
            _ -> <<".py">>
        end,
    F =
        fun(F, Acc) ->
            Name = dgiot_utils:to_binary(re:replace(filename:basename(F), Extension, <<>>, [{return, binary}])),
            case Mod of
                Name ->
                    Acc + 1;
                _ ->
                    Acc + 0
            end
        end,
    IsFindMod = filelib:fold_files(CurrDir, ".*", true, F, 0),
    R = case IsFindMod of
            0 ->
                <<"mod not exist">>;
            _ ->
                CodeFile = CurrDir ++ dgiot_utils:to_list(Mod) ++ dgiot_utils:to_list(Extension),
                ok = file:delete(CodeFile),
                SwaggerFile = CurrDir ++ "swagger_" ++ dgiot_utils:to_list(Type) ++ "_" ++ dgiot_utils:to_list(Mod) ++ ".json",
                ok = file:delete(SwaggerFile),
                <<"delete succeed">>
        end,
    case Version of
        <<"release">> ->
            Key = dgiot_utils:get_macs(),
            dgiot_mqtt:publish(self(), <<"restart/webserver/", Key/binary>>, <<"restart">>);
        _ -> pass
    end,
    R.

get_release_exproto(Type, Mod, Code, Swagger, SessionToken) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    CurrDir = Dir ++ "/priv/release/",
    Extension =
        case Type of
            <<"python">> -> <<".py">>;
            <<"java">> -> <<".java">>;
            _ -> <<".py">>
        end,
    CodeFile = CurrDir ++ dgiot_utils:to_list(Mod) ++ dgiot_utils:to_list(Extension),
    Data = base64:decode(Code),
    ok = file:write_file(CodeFile, Data),
    SwaggerFile = CurrDir ++ "swagger_" ++ dgiot_utils:to_list(Type) ++ "_" ++ dgiot_utils:to_list(Mod) ++ ".json",
    ok = file:write_file(SwaggerFile, jsx:encode(Swagger)),
    Key = dgiot_utils:get_macs(),
    dgiot_mqtt:publish(self(), <<"restart/webserver/", Key/binary>>, <<"restart">>),
    dgiot_utils:to_binary(test_python(Mod, SessionToken)).

%%-record(python_options, {
%%    python = default :: string() | default,
%%    cd :: Path :: string() | undefined,
%%    use_stdio = use_stdio :: use_stdio | nouse_stdio,
%%    compressed = 0 :: 0..9,
%%    packet = 4 :: 1 | 2 | 4,
%%    env = [] :: [{EnvName :: string(), EnvValue :: string()}],
%%    python_path = [] :: [Path :: string()],
%%    port_options = [binary, hide, exit_status]
%%    :: [Option :: atom() | {Name :: atom(), Value :: term()}],
%%    start_timeout = ?DEFAULT_START_TIMEOUT :: pos_integer() | infinity,
%%    call_timeout = ?DEFAULT_CALL_TIMEOUT :: pos_integer() | infinity,
%%    buffer_size = ?DEFAULT_BUFFER_SIZE :: pos_integer()
%%}).
test_python(Mod, SessionToken) ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Path = dgiot_httpc:url_join([Dir, "/priv/debug"]),
    NewPid =
        case dgiot_data:get({python_api, SessionToken}) of
            not_find ->
                {ok, Pid} =
                    python:start([{python, get_type()}, {python_path, Path},{call_timeout,60 * 1000 * 30}]),
                Pid;
            OldPid -> OldPid
        end,
    python:call(NewPid, list_to_atom(dgiot_utils:to_list(Mod)), do_exproto, []).

get_type() ->
    Type = application:get_env(dgiot_exproto, type, <<"python">>),
    dgiot_utils:to_list(Type).

