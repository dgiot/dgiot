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

-module(dgiot_tdengine_mqtt).
-author("johnliu").
-include("dgiot_tdengine.hrl").
-include_lib("dgiot/include/logger.hrl").

-behaviour(gen_server).

%% API
-export([
    start/0,
    start_link/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    on_client_connected/3,
%%    on_client_connected/4,
    on_client_disconnected/4,
    sql/2,
    debug/2,
    start_td_client/2
]).

-record(state, {}).

start() ->
    ?LOG(info, "start"),
    Env = #{},
    emqx:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
    emqx:hook('client.disconnected', fun ?MODULE:on_client_disconnected/4, [Env]).

stop() ->
    emqx:unhook('client.connected', fun ?MODULE:on_client_connected/3),
    emqx:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/4),
    ok.


on_client_connected(#{clientid := <<"swtd_", Key/binary>>, username := UserName}, _ConnInfo, _Env) ->
    case dgiot_data:lookup(?DGIOT_TD_CH_MODEL, UserName) of
        {ok, Password} ->
            case binary:match(Key, [Password]) of
                nomatch -> stop;
                _ ->
                    Args = #{
                        <<"channelid">> => UserName,
                        <<"productid">> => Password,
                        <<"key">> => Key
                    },
                    supervisor:start_child(lsx_td, [Args]),
                    ok
            end;
        _ -> stop
    end;

on_client_connected(_ClientInfo = #{clientid := _ClientId}, _ConnInfo, _Env) ->
    ok.

on_client_disconnected(#{username := ChannelId, clientid := <<"swtd_", Key/binary>>} = Client, _ReasonCode, _, _Env) ->
    ?LOG(info, "Client ~p ", [Client]),
    case dgiot_data:lookup(?DGIOT_TD_CH_WORK, {ChannelId, Key}) of
        {ok, Pid} when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> gen_server:call(Pid, disconnect, 1000);
                false -> pass
            end;
        _Reason ->
            ?LOG(info, "_Reason ~p ", [_Reason]),
            ok
    end,
    stop;

on_client_disconnected(#{clientid := _ClientId}, _ReasonCode, _, _Env) ->
    ok.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(2000, self(), init),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(init, State) ->
    start(),
    {noreply, State};


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

sql(ChannelId, Sql) ->
    try
        lists:map(fun({{X, _}, Pid}) ->
            case is_process_alive(Pid) andalso (X == ChannelId) of
                true -> gen_server:call(Pid, {sql, Sql}, 1000 * 15);
                false -> pass
            end
                  end, ets:tab2list(?DGIOT_TD_CH_WORK))
    catch
        error:Reason ->
            ?LOG(warning, "database high-load:~p", [Reason]);
        _:Ret ->
            ?LOG(warning, "unknown error :~p", [Ret])
    end.

debug(ChannelId, Sql) ->
    try
        lists:map(fun({{X, _}, Pid}) ->
            case is_process_alive(Pid) andalso (X == ChannelId) of
                true -> gen_server:call(Pid, {debug, Sql}, 1000 * 15);
                false -> pass
            end
                  end, ets:tab2list(?DGIOT_TD_CH_WORK))
    catch
        error:Reason ->
            ?LOG(warning, "database high-load:~p", [Reason]);
        _:Ret ->
            ?LOG(warning, "unknown error :~p", [Ret])
    end.

%% =======================
start_td_client(ChannelId, App) ->
%%    dgiot_data:init(?dgiot_TD_CH_MODEL),
%%    dgiot_data:insert(?dgiot_TD_CH_MODEL, ChannelId, App),
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    Root = dgiot_httpc:url_join([Dir, "/priv/"]),
    SourcePath = Root ++ "start_td_channel.sh",
    ServerName = <<ChannelId/binary, App/binary>>,
    Vars = [
        {dgiot_td_channel, ServerName},
        {host, <<"127.0.0.1">>},
        {channelid, ChannelId},
        {app, App}],
    DestPath = Root ++ dgiot_utils:to_list(ServerName) ++ ".sh",
    case erlydtl:compile({file, SourcePath}, dgiot_render, [{out_dir, false}]) of
        {ok, Render} ->
            {ok, IoList} = Render:render(Vars),
            {ok, File} = file:open(DestPath, [write, raw]),
            BinFile = unicode:characters_to_binary(IoList),
            file:write(File, BinFile),
            file:close(File),
            os:cmd("/bin/sh " ++ DestPath);
        error ->
            {error, compile_error}
    end.
