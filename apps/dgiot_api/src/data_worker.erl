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
-module(data_worker).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).
-record(task, {sessiontoken = <<>>, env = #{}}).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3, stop/1]).

-export([
    export_parse/1
    , export_td/1
    , export_files/1
    , import_parse/1
    , import_td/0
    , import_files/0
]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(#{<<"sessionToken">> := SessionToken} = State) ->
    case dgiot_data:lookup({data, SessionToken}) of
        {ok, Pid} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    ok;
                false ->
                    gen_server:start_link(?MODULE, [State], [])
            end;
        _Reason ->
            gen_server:start_link(?MODULE, [State], [])
    end;

start_link(_State) ->
    ok.

stop(#{<<"sessionToken">> := SessionToken}) ->
    case dgiot_data:lookup({data, SessionToken}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([#{<<"sessionToken">> := SessionToken} = State]) ->
    io:format("~s ~p State = ~p.~n", [?FILE, ?LINE, State]),
    dgiot_data:insert({data, SessionToken}, self()),
    erlang:send_after(1000, self(), station),
    stop(State),
    {ok, #task{sessiontoken = SessionToken, env = State}};

init(A) ->
    ?LOG(info, "A ~p ", [A]).

handle_call(stop, _From, State) ->
    erlang:garbage_collect(self()),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%handle_info({export_data, Body, SchemasFile}, #task{id = ChannelId} = State) ->
%%    io:format("~s ~p start SchemasFile = ~p.~n", [?FILE, ?LINE, SchemasFile]),
%%    NewData = dgiot_tdengie_dump:export(ChannelId, Body),
%%    case zip:create(<<"tdengine">>, NewData, [memory]) of
%%        {ok, {_, ZipFile}} ->
%%            file:write_file(SchemasFile, ZipFile);
%%        %% io:format("~s ~p end = ~p.~n", [?FILE, ?LINE, R]);
%%        _ ->
%%            pass
%%    end,
%%    {ok, State};

%% 导出
handle_info(dashboard, #task{sessiontoken = SessionToken, env = #{<<"type">> := <<"export">>}} = State) ->
    export_parse(SessionToken),
    export_td(SessionToken),
    export_files(SessionToken),
    {stop, normal, State};

%% 导入
handle_info(dashboard, #task{env = #{<<"type">> := <<"import">>, <<"file">> := _File}} = State) ->
    import_parse(<<"">>),
    import_td(),
    import_files(),
    {stop, normal, State};

handle_info({'EXIT', _From, Reason}, State) ->
    erlang:garbage_collect(self()),
    {stop, Reason, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


export_parse(SessionToken) ->
    RoleNames =
        case dgiot_auth:get_session(SessionToken) of
            #{<<"roles">> := Roles} = _User ->
                dgiot_role:get_rolenames(Roles);
            _ ->
                #{}
        end,
    lists:foldl(fun(RoleName, _Acc) ->
        {file, Here} = code:is_loaded(data_worker),
        Path = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/station/dgiot_pg_export.sh "]),
        Cmd = Path ++ dgiot_utils:to_list(RoleName),
        os:cmd(Cmd)
                end, #{}, RoleNames).

export_td(SessionToken) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} ->
            {error, Error};
        {ok, ChannelId} ->
            MS = dgiot_utils:to_binary(dgiot_datetime:now_ms()),
            Path = list_to_binary(dgiot_http_server:get_env(dgiot_api, docroot)),
            file:make_dir(<<Path/binary, "/download">>),
            file:make_dir(<<Path/binary, "/download/tdengine">>),
            FileName = "/download/tdengine/" ++ dgiot_utils:to_list(MS) ++ ".zip",
            SchemasFile = dgiot_utils:to_list(Path) ++ FileName,
            NewData = dgiot_tdengie_dump:export(ChannelId, #{}),
            case zip:create(<<"tdengine">>, NewData, [memory]) of
                {ok, {_, ZipFile}} ->
                    file:write_file(SchemasFile, ZipFile);
                %% io:format("~s ~p end = ~p.~n", [?FILE, ?LINE, R]);
                _ ->
                    pass
            end,
            {ok, #{<<"path">> => dgiot_utils:to_binary(FileName)}}
    end.

export_files(RoleName) ->
    {file, Here} = code:is_loaded(data_worker),
    Path = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/station/dgiot_files_export.sh "]),
    Cmd = Path ++ dgiot_utils:to_list(RoleName),
    os:cmd(Cmd).

import_parse(RoleName) ->
    {file, Here} = code:is_loaded(data_worker),
    Path = dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/station/dgiot_pg_import.sh "]),
    Cmd = Path ++ dgiot_utils:to_list(RoleName),
    os:cmd(Cmd).

import_td() ->
    ok.

import_files() ->
    ok.
