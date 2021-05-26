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

-module(dgiot_req).
-include("dgiot_api.hrl").
-author("johnliu").

%% API
-export([get_value/3, get_qs/2, get_qs/3, qs/1, parse_qs/1, parse_qs/2, header/2, header/3, headers/1, headers/2, path/1, host/1, port/1, binding/2, method/1, read_body/1, read_body/2, set_cookie/4]).
-export([to_lower/1, to_lower/2]).
-export([reply/2,reply/3,reply/4]).


-type req() :: map().
-type http_status() :: cowboy:http_status().
-export_type([req/0, http_status/0]).



get_value(<<"query">>, Name, Req) ->
    {get_qs(Name, Req), Req};

get_value(<<"header">>, Name, Req) ->
    {dgiot_req:header(Name, Req), Req};

get_value(<<"path">>, Name, Req) ->
    {dgiot_req:binding(Name, Req), Req};

get_value(<<"cookie">>, Name, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    {proplists:get_value(Name, Cookies), Req};


%% 取body时，返回新的Req
get_value(<<"body">>, Name, Req0) ->
    {ok, Body, Req} = read_body(Req0),
    Params = jsx:decode(Body, [{labels, binary}, return_maps]),
    {maps:get(Name, Params, undefined), Req}.

method(Req) ->
    cowboy_req:method(Req).


-spec get_qs(binary(), req()) -> binary() | undefined.
get_qs(Name, Req) ->
    QS = parse_qs(Req),
    proplists:get_value(Name, QS).

get_qs(Name, Req, Default) ->
    QS = parse_qs(Req),
    proplists:get_value(Name, QS, Default).

parse_qs(Req) ->
    cowboy_req:parse_qs(Req).

parse_qs(Req, Opts) ->
    QS = parse_qs(Req),
    case proplists:get_value(return, Opts) of
        map -> maps:from_list(QS);
        _ -> QS
    end.

qs(Req) ->
    cowboy_req:qs(Req).


-spec header(binary(), req()) -> binary() | undefined.

header(Name, Req, Default) ->
    case header(Name, Req) of
        undefined -> Default;
        Value -> Value
    end.

header(Name0, Req) ->
    Name = to_lower(Name0),
    cowboy_req:header(Name, Req).

headers(Req) ->
    cowboy_req:headers(Req).
headers(Req, Opt) ->
    case proplists:get_value(return, Opt) of
        list ->
            maps:to_list(headers(Req));
        map ->
            headers(Req)
    end.


path(Req) ->
    cowboy_req:path(Req).

host(Req) ->
    cowboy_req:host(Req).

port(Req) ->
    cowboy_req:port(Req).

-spec binding(atom(), req()) -> any() | undefined.
binding(Name0, Req0) ->
    Name = binary_to_atom(Name0, utf8),
    cowboy_req:binding(Name, Req0).

-spec read_body(Req :: req()) ->
    {ok, binary(), req()} | {more, binary(), req()}.
read_body(Req0) ->
    case maps:get(has_read_body, Req0, false) of
        false ->
            Fun =
                fun({Status, Body1, Req}) ->
                    Body0 = maps:get(body, Req, <<>>),
                    Body = <<Body0/binary, Body1/binary>>,
                    case Status of
                        more ->
                            read_body(Req#{body => Body});
                        ok ->
                            case cowboy_req:header(<<"content-type">>, Req) of
                               Type when Type == <<"application/x-www-form-urlencoded">>; Type == <<"application/x-www-urlencoded">> ->
                                    Body2 = jsx:encode(cow_qs:parse_qs(Body)),
                                    {ok, Body2, Req#{body =>Body2 }};
                                _ ->
                                    {ok, Body, Req#{body => Body}}
                            end
                    end
                end,
            read_body(Req0, Fun);
        true ->
            {ok, maps:get(body, Req0, <<>>), Req0}
    end.

-spec read_body(Req :: req(), Fun::function()) ->
    {ok, binary(), req()} | {more, binary(), req()}.
read_body(Req0, Fun) ->
    Result = cowboy_req:read_body(Req0),
    Fun(Result).

set_cookie(Name, Value, Req, Opts) ->
    %?LOG(info,"Set-cookie ~p:~p~n", [Name, Value]),
    cowboy_req:set_resp_cookie(dgiot_utils:to_list(Name), dgiot_utils:to_list(Value), Req, Opts).


reply(Status, Req) ->
    cowboy_req:reply(Status, Req).
reply(Status, Headers, Body, Req) ->
    cowboy_req:reply(Status, Headers, Body, Req).
reply(Status, Headers, Req) ->
    cowboy_req:reply(Status, Headers, Req).


to_lower(V, Opts) ->
    case proplists:get_value(return, Opts) of
        binary -> dgiot_utils:to_binary(to_lower(V));
        list -> dgiot_utils:to_list(to_lower(V));
        atom -> dgiot_utils:to_atom(to_lower(V))
    end.

to_lower(V) when is_list(V) ->
    string:to_lower(V);
to_lower(V) when is_binary(V) ->
    list_to_binary(string:to_lower(binary_to_list(V))).

