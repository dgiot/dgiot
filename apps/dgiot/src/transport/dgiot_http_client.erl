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
-module(dgiot_http_client).
-author("johnliu").
-include_lib("dgiot/include/logger.hrl").
-include_lib("dgiot/include/dgiot_client.hrl").

-define(CHNANEL(Name), dgiot_utils:to_atom(Name)).
-define(JSON_DECODE(Data), jsx:decode(Data, [{labels, binary}, return_maps])).
-define(HTTPOption(Option), [{timeout, 60000}, {connect_timeout, 60000}] ++ Option).
-define(REQUESTOption(Option), [{body_format, binary} | Option]).
-define(HEAD_CFG, [{"content-length", del}, {"referer", del}, {"user-agent", "dgiot"}]).
-record(connect_state, {mod}).

%% gen_server callbacks
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% API
-export([
    request/2,
    method/1,
    method/2
]).

-export([
    send/2,
    send/4,
    get/2,
    get/3,
    post/3,
    post/4,
    set_uri/2
]).


start_link(Args) ->
    dgiot_client:start_link(?MODULE, Args).

init([#{<<"channel">> := ChannelId, <<"client">> := ClientId, <<"mod">> := Mod} = Args]) ->
%%  io:format("~s ~p  Args ~p ~n", [?FILE, ?LINE, Args]),
    set_uri(dgiot_utils:to_binary(ChannelId), maps:with([<<"proctol">>, <<"host">>, <<"ip">>, <<"port">>], Args)),
    UserData = #connect_state{mod = Mod},
    ChildState = maps:get(<<"child">>, Args, #{}),
    StartTime = dgiot_client:get_time(maps:get(<<"starttime">>, Args,  dgiot_datetime:now_secs())),
    EndTime = dgiot_client:get_time(maps:get(<<"endtime">>, Args,  dgiot_datetime:now_secs() + 1000000000)),
    Freq = maps:get(<<"freq">>, Args, 30),
    Count = dgiot_client:get_count(StartTime, EndTime, Freq),
    Clock = #dclock{freq = Freq, nexttime = StartTime, count = Count, round = 0},
    Dclient = #dclient{channel = ChannelId, client = ClientId, status = ?DCLIENT_INTIALIZED, clock = Clock, userdata = UserData, child = ChildState},
    dgiot_client:add(ChannelId, ClientId),
    case Mod:init(Dclient) of
        {ok, NewDclient} ->
            {ok, NewDclient, hibernate};
        {stop, Reason} ->
            {stop, Reason}
    end.

handle_call(Request, From, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_call(Request, From, Dclient) of
        {reply, Reply, NewDclient} ->
            {reply, Reply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, Reason, NewDclient}
    end.

handle_cast(Msg, #dclient{channel = ChannelId, client = ClientId,
    userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_cast(Msg, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, Reason, NewDclient} ->
            dgiot_client:stop(ChannelId, ClientId),
            {reply, Reason, NewDclient}
    end.

%% 往http server 发送报文
handle_info({send, Fun, Args}, #dclient{userdata = #connect_state{mod = Mod}} = Dclient) ->
%%  io:format("~s ~p send to from ~p:~p : ~p ~n", [?FILE, ?LINE,  _Ip, _Port, dgiot_utils:to_hex(PayLoad)]),
    case send(Fun, Args) of
        {ok, Result1} ->
            Mod:handle_info({Fun, ok, Result1}, Dclient);
        {error, Reason} ->
            Mod:handle_info({Fun, error, Reason}, Dclient)
    end,
    {noreply, Dclient, hibernate};

handle_info({send, Registry, Metrics, Fun, Args}, #dclient{userdata = #connect_state{mod = Mod}} = Dclient) ->
    case send(Registry, Metrics, Fun, Args) of
        {ok, Result1} ->
            Mod:handle_info({Metrics, ok, Result1}, Dclient);
        {error, Reason} ->
            Mod:handle_info({Metrics, error, Reason}, Dclient)
    end,
    {noreply, Dclient, hibernate};

handle_info(Info, #dclient{channel = ChannelId, client = ClientId, userdata = #connect_state{mod = Mod}} = Dclient) ->
    case Mod:handle_info(Info, Dclient) of
        {noreply, NewDclient} ->
            {noreply, NewDclient, hibernate};
        {stop, _Reason, NewDclient} ->
            timer:sleep(10),
            dgiot_client:stop(ChannelId, ClientId),
            {noreply, NewDclient, hibernate}
    end.

terminate(Reason, #dclient{userdata = #connect_state{mod = Mod}} = Dclient) ->
    Mod:terminate(Reason, Dclient).

code_change(OldVsn, #dclient{userdata = #connect_state{mod = Mod}} = Dclient, Extra) ->
    Mod:code_change(OldVsn, Dclient, Extra).

%%%===================================================================
%%% Internal functions
%%%===================================================================
set_uri(ChannelId, Args) when is_binary(ChannelId) andalso byte_size(ChannelId) > 20 ->
    <<_:11/binary, Tid/binary>> = dgiot_utils:to_binary(ChannelId),
    set_uri(dgiot_utils:to_atom(Tid), Args);

set_uri(ChannelId, #{<<"ip">> := Host} = Args) ->
    NewArgs = maps:without([<<"ip">>], Args),
    set_uri(ChannelId, NewArgs#{<<"host">> => Host});
set_uri(ChannelId, #{<<"host">> := Host, <<"port">> := Port} = Args) ->
    Proctol = maps:get(<<"proctol">>, Args, <<"http">>),
    persistent_term:put({ChannelId, http_uri}, #{<<"proctol">> => Proctol, <<"host">> => Host, <<"port">> => Port}).

get_url(ChannelId, Path) ->
    #{<<"proctol">> := Proctol, <<"host">> := Host, <<"port">> := Port} = persistent_term:get({ChannelId, http_uri}),
    lists:concat([dgiot_utils:to_list(Proctol), "://", dgiot_utils:to_list(Host), ":", Port, "/", dgiot_utils:to_list(Path)]).

send(Fun, Args) when Fun == post orelse Fun == get ->
    send(?MODULE, ?MODULE, Fun, Args).

send(Registry, Metrics, Fun, Args) ->
    {Time, Result} = timer:tc(?MODULE, Fun, Args),
%%    io:format("~s ~p  Time ~p Result ~p ~n", [?FILE, ?LINE, Time, Result]),
    MSecs = Time / 1000,
    dgiot_metrics:inc(Registry, <<"http_count">>, 1),
    dgiot_metrics:inc(Registry, <<"http_", Metrics/binary, "_time">>, MSecs, average),
    case Result of
        {ok, Result1} ->
%%            io:format("~s ~p  ~p Args ~p Result1 ~p ~n", [?FILE, ?LINE, Metrics, Args, Result1]),
            dgiot_metrics:inc(Registry, <<"http_", Metrics/binary, "_succ">>, 1),
            dgiot_metrics:inc(Registry, <<"http_succ">>, 1),
            {ok, Result1};
        {error, Reason} ->
%%            io:format("~s ~p  ~p Args ~p Reason ~p ~n", [?FILE, ?LINE, Metrics, Args, Reason]),
            dgiot_metrics:inc(Registry, <<"http_", Metrics/binary, "_fail">>, 1),
            {error, Reason}
    end.

get(ChannelId, Path) ->
    get(ChannelId, Path, []).
get(ChannelId, Path, Header) ->
    Url = get_url(ChannelId, Path),
    request(get, {Url, Header}).

post(ChannelId, Path, Data) ->
    post(ChannelId, Path, [], Data).
post(ChannelId, Path, Header, Data) ->
    Url = get_url(ChannelId, Path),
    request(post, {Url, Header, "application/xml;charset=UTF-8", Data}).

method(Method) ->
    list_to_atom(string:to_lower(dgiot_utils:to_list(Method))).
method(Method, atom) ->
    list_to_atom(string:to_lower(dgiot_utils:to_list(Method)));
method(Method, binary) ->
    list_to_binary(string:to_lower(dgiot_utils:to_list(Method))).

do_decode(Headers, Body) ->
    case lists:keyfind("content-encoding", 1, Headers) of
        {_, "gzip"} ->
            zlib:gunzip(Body);
        _ ->
            Body
    end.

request(Method, Request) ->
    case httpc:request(Method, Request, ?HTTPOption([{autoredirect, true}]), ?REQUESTOption([])) of
        {ok, {{_, 200, _}, Headers, Body}} ->
            Deconde = do_decode(Headers, Body),
            case jsx:is_json(dgiot_utils:to_binary(Deconde)) of
                true ->
                    {ok, ?JSON_DECODE(dgiot_utils:to_binary(Deconde))};
                _ ->
                    {ok, Deconde}
            end;
        {ok, {{_, HTTPCode, _}, _, Body}} ->
            {error, {HTTPCode, Body}};
        {error, {failed_connect, _}} ->
            {error, econnrefused};
        {error, Reason} ->
            {error, Reason}
    end.
