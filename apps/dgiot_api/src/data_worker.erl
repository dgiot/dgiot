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
    , import_td/1
    , import_files/0
    , restart_channel/1
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
%%    io:format("~s ~p State = ~p.~n", [?FILE, ?LINE, State]),
    dgiot_data:insert({data, SessionToken}, self()),
    erlang:send_after(1000, self(), station),
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

%% 导出
handle_info(station, #task{sessiontoken = SessionToken, env = #{<<"type">> := <<"export">>}} = State) ->
    io:format("~s ~p SessionToken = ~p.~n", [?FILE, ?LINE, SessionToken]),
    export_parse(SessionToken),
    export_td(SessionToken),
    export_files(SessionToken),
    {stop, normal, State};

%% 导入
handle_info(station, #task{sessiontoken = SessionToken, env = #{<<"type">> := <<"import">>, <<"file">> := #{<<"fullpath">> := Fullpath} = _File}} = State) ->
    io:format("~s ~p State = ~p.~n", [?FILE, ?LINE, State]),
    import_parse(Fullpath),
    dgiot_device_cache:parse_cache_Device(0),
    restart_channel(SessionToken),
    import_td(SessionToken),
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


%%  data_worker:export_parse(<<"r:21debcab56050159c174a61195e4f8d6">>).
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
        Path = dgiot_utils:to_binary(dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/station/dgiot_pg_dump_export.sh "])),
        Cmd = <<"sh ", Path/binary, RoleName/binary>>,
        os:cmd(dgiot_utils:to_atom(Cmd))
                end, #{}, RoleNames).

%% data_worker:export_td(<<"r:21debcab56050159c174a61195e4f8d6">>).
export_td(SessionToken) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} ->
            {error, Error};
        {ok, ChannelId} ->
            file:make_dir(<<"/home/station/tdengine">>),
            FileName = "/home/station/tdengine/tables.zip",
            NewData = dgiot_tdengine_dump:export(ChannelId, #{<<"sessionToken">> => SessionToken}),
            case zip:create(<<"tdengine">>, NewData, [memory]) of
                {ok, {_, ZipFile}} ->
                    file:write_file(FileName, ZipFile);
                _ ->
                    pass
            end,
            {ok, #{<<"path">> => dgiot_utils:to_binary(FileName)}}
    end.

%% data_worker:export_files(<<"r:6dff46c8028917292acc8679f3e790f5">>).
export_files(SessionToken) ->
    Query = #{
        <<"keys">> => [<<"path">>, <<"name">>]
    },
    case dgiot_parse:query_object(<<"Files">>, Query, [{"X-Parse-Session-Token", SessionToken}], [{from, rest}]) of
        {ok, #{<<"results">> := Data}} ->
            lists:foldl(fun(#{<<"path">> := Path, <<"name">> := Name}, _Acc) ->
                {file, Here} = code:is_loaded(data_worker),
                CPath = dgiot_utils:to_binary(dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/station/dgiot_files_export.sh "])),
                Cmd = <<"sh ", CPath/binary, Path/binary, " ", Name/binary>>,
                os:cmd(dgiot_utils:to_atom(Cmd))
                        end, [], Data);
        _ ->
            pass
    end.

%% data_worker:import_parse(<<"/data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/www/upload/202311613544.gz">>).
import_parse(Fullpath) ->
    {file, Here} = code:is_loaded(data_worker),
    Basename = filename:basename(Fullpath),
    Path = dgiot_utils:to_binary(dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/station/dgiot_pg_dump_import.sh "])),
    Cmd = <<"sh ", Path/binary, Basename/binary>>,
    io:format("~s ~p Cmd = ~p.~n", [?FILE, ?LINE, Cmd]),
    os:cmd(dgiot_utils:to_atom(Cmd)).

%% data_worker:import_td(<<"r:a4f169ffbbb37cdca429570396573ce3">>).
import_td(SessionToken) ->
    case dgiot_product_tdengine:get_channel(SessionToken) of
        {error, Error} ->
            {error, Error};
        {ok, ChannelId} ->
            case zip:unzip("/data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/www/upload/station/tdengine/tables.zip", [memory]) of
                {ok, Result} ->
                    dgiot_tdengine_dump:import(ChannelId, Result);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% data_worker:import_files().
import_files() ->
    {file, Here} = code:is_loaded(data_worker),
    Path = dgiot_utils:to_binary(dgiot_httpc:url_join([filename:dirname(filename:dirname(Here)), "/priv/station/dgiot_files_import.sh "])),
    Cmd = <<"sh ", Path/binary>>,
    io:format("~s ~p Cmd = ~p.~n", [?FILE, ?LINE, Cmd]),
    os:cmd(dgiot_utils:to_atom(Cmd)).

%% data_worker:restart_channel(<<"r:a4f169ffbbb37cdca429570396573ce3">>)
restart_channel(SessionToken) ->
    case dgiot_parse:query_object(<<"Channel">>, #{<<"where">> => #{<<"isEnable">> => true}}) of
        {ok, #{<<"results">> := Results}} ->
            lists:foldl(fun(#{<<"objectId">> := ChannelId}, _Acc) ->
                dgiot_bridge:control_channel(ChannelId, <<"disable">>, SessionToken),
                timer:sleep(500),
                dgiot_bridge:control_channel(ChannelId, <<"enable">>, SessionToken),
                timer:sleep(500)
                        end, [], Results);
        _ ->
            pass
    end.
