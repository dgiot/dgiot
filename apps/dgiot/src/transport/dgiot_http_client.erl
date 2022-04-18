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
-define(CHNANEL(Name), dgiot_utils:to_atom(Name)).
-define(JSON_DECODE(Data), jsx:decode(Data, [{labels, binary}, return_maps])).
-define(HTTPOption(Option), [{timeout, 60000}, {connect_timeout, 60000}] ++ Option).
-define(REQUESTOption(Option), [{body_format, binary} | Option]).
-define(HEAD_CFG, [{"content-length", del}, {"referer", del}, {"user-agent", "dgiot"}]).

%% API
-export([
    request/2,
    method/1,
    method/2
]).

-export([
    get/2,
    get/3,
    post/3,
    post/4,
    tc/4,
    set_url/2,
    get_sign/3,
    get_sign/4,
    getToken/1,
    format_json/1
]).

get(Name, Path) ->
    #{
        <<"apiKey">> := Key,
        <<"apiSecret">> := Secret
    } = persistent_term:get({Name, http_appkey}, #{
        <<"apiKey">> => <<"Key">>,
        <<"apiSecret">> => <<"Secret">>
    }),
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

post(Name, Path, Data) ->
    post(Name, Path, [], Data).
post(Name, Path, Header, Data) ->
    Url = get_url(Name, Path),
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
    case httpc:request(Method, Request, ?HTTPOption([{autoredirect, true}]), []) of
        {ok, {{_, 200, _}, Headers, Body}} ->
            Deconde = do_decode(Headers, Body),
            case jsx:is_json(dgiot_utils:to_binary(Deconde)) of
                true ->
                    {ok, jsx:decode(dgiot_utils:to_binary(Deconde), [{labels, binary}, return_maps])};
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


set_url(Name, Args) ->
    persistent_term:put({Name, http_url}, Args).

get_url(Name, Path) ->
    #{
        <<"proctol">> := Proctol,
        <<"host">> := Host,
        <<"port">> := Port
    } = persistent_term:get({Name, http_url}, #{
        <<"proctol">> => <<"http">>,
        <<"host">> => "127.0.0.1",
        <<"port">> => 5080
    }),
    lists:concat([dgiot_utils:to_list(Proctol), "//", dgiot_utils:to_list(Host), ":", Port, "/", dgiot_utils:to_list(Path)]).

tc(Name, Mod, Fun, Args) ->
    {Time, Result} = timer:tc(Mod, Fun, Args),
    MSecs = Time / 1000,
    dgiot_metrics:inc(dgiot_http_client, <<"http_count">>, 1),
    dgiot_metrics:inc(dgiot_http_client, <<"http_", Name/binary, "_time">>, MSecs, average),
    case Result of
        {ok, Result1} ->
            ?LOG(debug, "~p Args ~p Result1 ~p", [Name, Args, format_json(Result1)]),
            dgiot_metrics:inc(dgiot_http_client, <<"http_", Name/binary, "_succ">>, 1),
            dgiot_metrics:inc(dgiot_http_client, <<"http_succ">>, 1),
            {ok, Result1};
        {error, Reason} ->
            ?LOG(debug, "~p Args:~p, Reason:~p", [Name, Args, Reason]),
            dgiot_metrics:inc(dgiot_http_client, <<"http_", Name/binary, "_fail">>, 1),
            {error, Reason}
    end.

format_json(Body) ->
    case catch dgiot_json:decode(Body, [{labels, binary}, return_maps]) of
        {'EXIT', Reason} ->
            {error, Reason};
        #{<<"success">> := Status, <<"code">> := Code, <<"msg">> := Msg} when Status == false ->
            {error, #{<<"code">> => Code, <<"msg">> => Msg}};
        #{<<"result">> := Result} ->
            {ok, Result}
    end.


get_sign(Name, Secret, Key, T) ->
    AccessToken = dgiot_data:get({Name, self(), token}),
    Data = dgiot_utils:to_binary(dgiot_utils:to_list(Key) ++ dgiot_utils:to_list(AccessToken) ++ T),
    string:uppercase(binary_to_list(hmac:encode(sha256, Secret, Data))).

get_sign(Secret, Key, T) ->
    Data = dgiot_utils:to_binary(dgiot_utils:to_list(Key) ++ T),
    string:uppercase(binary_to_list(hmac:encode(sha256, Secret, Data))).

getToken(Name) ->
    case tc(?FUNCTION_NAME, ?MODULE, tokenRefresh, [Name]) of
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
