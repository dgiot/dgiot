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
-module(dgiot_httpc_worker).
-author("johnliu").
-behaviour(gen_server).
-include_lib("dgiot/include/logger.hrl").
%% API
-export([start_link/2, get/3, post/4, post/3, get/2]).

%% gen_server callbacks
-export([
    start/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tid, id, page = 1, token, refreshtoken, sleep = 12}).

-export([
    getToken/1,
    tokenRefresh/1,
    getData/2
]
).

%%%===================================================================
%%% API
%%%===================================================================
start(Tid) ->
    Http = get_config(Tid, <<"xxx_http">>, #{}),
    Count = maps:get(<<"count">>, Http, 0),
    Sup = list_to_atom(lists:concat([Tid, http])),
    [{ok, _} = supervisor:start_child(Sup, [Tid, Id]) || Id <- lists:seq(1, Count)].

start_link(Tid, Id) ->
    case dgiot_data:lookup({Tid, Id, httpc}) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            ok
    end,
    Server = list_to_atom(lists:concat([Tid, httpc, Id])),
    gen_server:start_link({local, Server}, ?MODULE, [Tid, Id], []).

init([Tid, Id]) ->
    dgiot_data:insert({Tid, Id, httpc}, self()),
    rand:seed(exs1024),
    Time = erlang:round(rand:uniform() * 5 + 1) * 1000,
    erlang:send_after(Time, self(), start),
    Sleep = get_config(Tid, <<"xxx_http">>, <<"time">>, 30),
    {ok, #state{tid = Tid, id = Id, sleep = Sleep}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(start, #state{tid = Tid, sleep = Sleep} = State) ->
    erlang:send_after(Sleep * 1000, self(), start),
    erlang:send_after(1000, self(), getToken),
    {noreply, State#state{tid = Tid}};

handle_info(getToken, #state{tid = Tid, sleep = _Sleep} = State) ->
    case getToken(Tid) of
        {ok, #{<<"accessToken">> := AccessToken, <<"refreshToken">> := RefreshToken, <<"expire">> := _Expire}} ->
            erlang:send_after(5 * 1000, self(), getData),
            dgiot_data:insert({Tid, self(), token}, AccessToken),
            {noreply, State#state{token = AccessToken, refreshtoken = RefreshToken}};
        {error, Reason} ->
            ?LOG(debug, "getToken ~p", [Reason]),
            {noreply, State}
    end;

handle_info(getData, #state{tid = Tid, token = Token, sleep = _Sleep} = State) when is_binary(Token), byte_size(Token) > 0 ->
    case getData(Tid, Token) of
        {ok, List} ->
            ?LOG(debug, "getData ~p", [length(List)]);
        {error, Reason} ->
            ?LOG(debug, "getData ~p", [Reason])
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{tid = Tid, id = Id} = _State) ->
    dgiot_data:delete({Tid, Id, httpc}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


post(Name, Path, Data) ->
    post(Name, Path, [], Data).
post(Name, Path, Header, Data) ->
    Url = get_url(Name, Path),
    request(post, {Url, Header, "application/xml;charset=UTF-8", Data}).

get(Name, Path) ->
    #{
        <<"apiKey">> := Key,
        <<"apiSecret">> := Secret
    } = get_config(Name, <<"xxx_auth">>),
    T = dgiot_utils:to_list(dgiot_datetime:now_ms()),
    Header = [
        {"apiKey", dgiot_utils:to_list(Key)},
        {"sign", get_sign(Name, Secret, Key, T)},
        {"t", T},
        {"Content-Type", "application/json;charset=UTF-8"},
        {"lang", "zh"}
    ],
    get(Name, Path, Header).

get(Name, Path, Header) ->
    Url = get_url(Name, Path),
    request(get, {Url, Header}).

get_url(Name, Path) ->
    #{
        <<"host">> := Host,
        <<"port">> := Port
    } = get_config(Name, <<"xxx_http">>),
    lists:concat(["http://", binary_to_list(Host), ":", Port, "/", Path]).

get_sign(Name, Secret, Key, T) ->
    AccessToken = dgiot_data:get({Name, self(), token}),
    Data = dgiot_utils:to_binary(dgiot_utils:to_list(Key) ++ dgiot_utils:to_list(AccessToken) ++ T),
    string:uppercase(binary_to_list(hmac:encode(sha256, Secret, Data))).

get_sign(Secret, Key, T) ->
    Data = dgiot_utils:to_binary(dgiot_utils:to_list(Key) ++ T),
    string:uppercase(binary_to_list(hmac:encode(sha256, Secret, Data))).

request(Method, Request) ->
    case httpc:request(Method, Request, [{autoredirect, true}, {timeout, 10000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Body};
        {ok, {{_, HTTPCode, _}, _, Body}} ->
            {error, {HTTPCode, Body}};
        {error, {failed_connect, _}} ->
            {error, econnrefused};
        {error, Reason} ->
            {error, Reason}
    end.

%%{ok,#{<<"code">> => <<"1001">>,
%%<<"msg">> =>
%%<<230,151,160,230,149,136,231,154,132,97,112,105,75,101,
%%121,230,136,150,97,112,105,83,101,99,114,101,...>>,
%%<<"success">> => false}}
format_json(Body) ->
    case catch jsx:decode(Body, [{labels, binary}, return_maps]) of
        {'EXIT', Reason} ->
            {error, Reason};
        #{<<"success">> := Status, <<"code">> := Code, <<"msg">> := Msg} when Status == false ->
            {error, #{<<"code">> => Code, <<"msg">> => Msg}};
        #{<<"result">>:= Result} ->
            {ok, Result}
    end.

get_start_time(EndTime, Sleep) ->
%%    {{Y, M, D}, _} = dgiot_datetime:to_localtime(EndTime),
     _StartTime = EndTime - Sleep * 1000 * 60.
%%    StartTime1 = dgiot_datetime:to_unixtime({{Y, M, D}, {0, 0, 0}}),
%%    max(StartTime, StartTime1).


tc(Name, Mod, Fun, Args) ->
    {Time, Result} = timer:tc(Mod, Fun, Args),
    MSecs = Time / 1000,
    dgiot_metrics:inc(dgiot_httpc, <<"http_count">>, 1),
    dgiot_metrics:inc(dgiot_httpc, <<"http_", Name/binary, "_time">>, MSecs, average),
    case Result of
        {ok, Result1} ->
            ?LOG(debug, "~p Args ~p Result1 ~p", [Name, Args, format_json(Result1)]),
            dgiot_metrics:inc(dgiot_httpc, <<"http_", Name/binary, "_succ">>, 1),
            dgiot_metrics:inc(dgiot_httpc, <<"http_succ">>, 1),
            {ok, Result1};
        {error, Reason} ->
            ?LOG(debug,"~p Args:~p, Reason:~p", [Name, Args, Reason]),
            dgiot_metrics:inc(dgiot_httpc, <<"http_", Name/binary, "_fail">>, 1),
            {error, Reason}
    end.


tokenRefresh(Name) ->
    Path = lists:concat(["v1.0/token"]),
    #{
        <<"apiKey">> := Key,
        <<"apiSecret">> := Secret
    } = get_config(Name, <<"xxx_auth">>),
    T = dgiot_utils:to_list(dgiot_datetime:now_ms()),
    Header = [
        {"apiKey", dgiot_utils:to_list(Key)},
        {"sign", get_sign(Secret, Key, T)},
        {"t", T},
        {"Content-Type", "application/json;charset=UTF-8"},
        {"lang", "zh"}
    ],
    get(Name, Path, Header).

getToken(Name) ->
    case tc(<<"getWanAccessToken">>, ?MODULE, tokenRefresh, [Name]) of
        {ok, Body} ->
            case format_json(Body) of
                {ok, #{<<"accessToken">> := _AccessToken, <<"refreshToken">> := _RefreshToken, <<"expire">> := _Expire} = Result} ->
                    ?LOG(debug, "Result ~p", [Result]),
                    {ok, Result};
                {error, Reason} ->
                    ?LOG(debug, "Reason ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%getData
getData(Name, Uid) ->
    EndTime = dgiot_datetime:now_ms(),
    StartTime = get_start_time(dgiot_datetime:nowstamp(), 1000),
    Path = lists:concat(["v1.0/test/data/", dgiot_utils:to_list(Uid), "?pageNo=", 1, "&pageSize=", 1,
        "&startTime=", StartTime, "&endTime=", EndTime, "&dataType=", 0]),
    case tc(<<"getDate">>, ?MODULE, get, [Name, Path]) of
        {ok, Body} ->
            case format_json(Body) of
                {ok, Result} ->
                    ?LOG(debug, "Data ~p", [Result]),
                    {ok, Result};
                {error, Reason} ->
                    ?LOG(debug, "Reason ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_config(Name) ->
    dgiot_data:lookup({Name, config}).

get_config(Name, Key) ->
    {ok, Config} = get_config(Name),
    maps:get(Key, Config).

get_config(Name, Key, Default) ->
    {ok, Config} = get_config(Name),
    maps:get(Key, Config, Default).

get_config(Name, Session, Key, Default) ->
    maps:get(Key, get_config(Name, Session), Default).