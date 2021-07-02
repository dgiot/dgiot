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

-module(dgiot_proxy).
-author("johnliu").
-include("dgiot_api.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([init/2]).
-export([pre_hook/1, post_hook/2, get_wlanip/0]).

%%%-------------------------------------------------------------------
%%% COWBOY
%%%-------------------------------------------------------------------
%%% The cowboy behaviour & entrypoint of the module.
%%根据dgiotproxy字典查出该条path的代理配置
get_opts(Path) ->
    [_, _, Flag | _] = binary:split(Path, <<$/>>, [global, trim]),
    Size = erlang:byte_size(<<"/dgiotproxy/", Flag/binary, "/">>),
    NewPath =
        case erlang:byte_size(Path) of
            NewSize when NewSize =< Size ->
                "";
            _ ->
                string:sub_string(dgiot_utils:to_list(Path), Size + 1)
        end,
    {ok, Type} = application:get_env(dgiot_api, dicttype),
    DictId = dgiot_parse:get_dictid(Flag, dgiot_utils:to_binary(Type)),
    Data =
        case dgiot_data:get(DictId) of
            not_find ->
                case dgiot_parse:get_object(<<"Dict">>, DictId) of
                    {ok, #{<<"data">> := Data1}} ->
                        dgiot_data:insert(DictId, Data1),
                        Data1;
                    _ -> #{}
                end;
            Data1 -> Data1
        end,
    Host = maps:get(<<"host">>, Data, <<"www.iotn2n.com">>),
    Wlanip = get_wlanip(),
    NewHost = re:replace(Host, <<"127.0.0.1">>, Wlanip, [global, {return, binary}]),
    Protocol = dgiot_utils:to_list(maps:get(<<"protocol">>, Data, <<"http">>)),
    Forward_Header = maps:get(<<"x-forwarded-for">>, Data, true),
    Hook = maps:get(<<"hook">>, Data, ?MODULE),
    [
        {host, dgiot_utils:to_list(NewHost)},
        {protocol, Protocol},
        {?FORWARD_HEADER, Forward_Header},
        {disable_proxy_headers, true},
        {use_forwarded_for, true},
        {modify_path, dgiot_utils:to_list(NewPath)},
        {hook, dgiot_utils:to_atom(Hook)}
    ].

init(Req0, proxy) ->
    Path = cowboy_req:path(Req0),
    State = get_opts(Path),
    Method = method(Req0),
    {Req1, Request} = request(Req0, State),
    HTTPOptions = opts_http_opts(State),
    Options = opts_misc_opts(State),
    %% TODO: Use request/5 if state has a profile parameter
    Mod = opts_hook_opts(State),
    PreRequest = Mod:pre_hook(Request),
    case httpc:request(Method, PreRequest, HTTPOptions, Options) of
        % We got a response from the remote server!
        {ok, Resp = {{_RespVersion, RespStatus, RespReason}, _RespHeaders, RespBody}} ->
            ?LOG(info, "Proxy response: ~p ~s", [RespStatus, RespReason]),
            OkReq1 = cowboy_req:reply(RespStatus, response_headers(Resp, State), RespBody, Req1),
            PostOkReq1 = Mod:post_hook(OkReq1, State),
            {ok, PostOkReq1, State};
        % Proxy error (not error on remote server, actual e.g. network error)
        Error ->
            ?LOG(error, "Proxy error: ~p", [Error]),
            ErrReq1 = cowboy_req:reply(502, #{"content-type" => "text/plain"}, dump(Error), Req1),
            PostErrReq1 = Mod:post_hook(ErrReq1, State),
            {ok, PostErrReq1, State}
    end.

%%%-------------------------------------------------------------------
%%% RESPONSE
%%%-------------------------------------------------------------------
%%% Functions for sending the server response back to the client.

%% Builds the response headers from the remote servers response.
response_headers({{RespVersion, RespStatus, RespReason}, RespHeaders, _RespBody}, Opts) ->
    List = case opts_disable_proxy_headers(Opts) of
               true ->
                   RespHeaders;
               false ->
                   [
                       {"x-proxy-http-version", RespVersion},
                       {"x-proxy-status", to_string(RespStatus)},
                       {"x-proxy-reason", to_string(RespReason)}
                       | RespHeaders
                   ]
           end,
    maps:from_list(List).

%%%-------------------------------------------------------------------
%%% REQUEST
%%%-------------------------------------------------------------------
%%% Functions for making the request to the back end server.

%% Creates the request.
request(Req, Opts) ->
    RequestURL = request_url(Req, Opts),
    RequestHeaders = request_headers(Req, Opts),
    case cowboy_req:has_body(Req) of
        true ->
            ContentType = to_string(cowboy_req:header(<<"content-type">>, Req, "")),
            {BodyReq, Body} = request_body(Req, Opts),
            {BodyReq, {RequestURL, RequestHeaders, ContentType, Body}};
        false ->
            {Req, {RequestURL, RequestHeaders}}
    end.

%% Creates the request URL.
request_url(Req, Opts) ->
    NewPath = to_string(opts_modify_path(Opts)),
    case maps:get(qs, Req, <<"">>) of
        <<"">> ->
            to_string([opts_protocol(Opts), "://", opts_host(Opts), "/", NewPath]);
        Qs ->
            to_string([opts_protocol(Opts), "://", opts_host(Opts), "/", NewPath, "?", Qs])
    end.


%% Creates the request headers.
request_headers(Req, Opts) ->
    % Client headers
    Headers = [
        {string:to_lower(to_string(Key)), to_string(Value)}
        || {Key, Value} <- maps:to_list(cowboy_req:headers(Req))
    ],
    % Replace the host?
    HostHeaders = lists:keystore("host", 1, Headers, {"host", opts_host(Opts)}),
    OriginHeaders = lists:keystore("origin", 1, HostHeaders, {"origin", opts_origin(Opts)}),
    % Add the peer IP to x-forwarded-for?
    case opts_use_forwarded_for(Opts) of
        true ->
            {PeerAddress, _PeerPort} = cowboy_req:peer(Req),
            PeerAddressString = inet:ntoa(PeerAddress),
            case lists:keyfind(?FORWARD_HEADER, 1, OriginHeaders) of
                false ->
                    [{?FORWARD_HEADER, PeerAddressString} | OriginHeaders];
                {?FORWARD_HEADER, XForwardedFor} ->
                    lists:keyreplace(?FORWARD_HEADER, 1, OriginHeaders,
                        {?FORWARD_HEADER, to_string([PeerAddressString, ", ", XForwardedFor])})
            end;
        false -> OriginHeaders
    end.

%% Reads the request body.
request_body(Req, Opts) ->
    request_body(Req, Opts, <<>>).
request_body(Req, Opts, Acc) ->
    case cowboy_req:read_body(Req, opts_body_opts(Opts)) of
        {ok, Data, NewReq} ->
            {NewReq, <<Acc/binary, Data/binary>>};
        {more, Data, NewReq} ->
            request_body(NewReq, Opts, <<Acc/binary, Data/binary>>)
    end.

%%%-------------------------------------------------------------------
%%% OPTIONS
%%%-------------------------------------------------------------------
%%% Functions for reading options. Use this as a reference for all available options.
%%% See the top of this source file for documentation on the available options.

opts_protocol(Opts) -> proplists:get_value(protocol, Opts, "http").
opts_host(Opts) -> proplists:get_value(host, Opts).
opts_origin(Opts) -> to_string([opts_protocol(Opts), "://", opts_host(Opts)]).
opts_use_forwarded_for(Opts) -> proplists:get_bool(use_forwarded_for, Opts).
opts_disable_proxy_headers(Opts) -> proplists:get_bool(disable_proxy_headers, Opts).
opts_modify_path(Opts) -> proplists:get_value(modify_path, Opts, fun identity/1).
opts_body_opts(Opts) -> proplists:get_value(body_opts, Opts, #{}).
opts_http_opts(Opts) -> proplists:get_value(http_opts, Opts, []).
opts_misc_opts(Opts) -> proplists:get_value(misc_opts, Opts, []).
opts_hook_opts(Opts) -> proplists:get_value(hook, Opts, []).

%%%-------------------------------------------------------------------
%%% HELPER
%%%-------------------------------------------------------------------
%%% Utility functions.

%% The identity of a value.
identity(Value) -> Value.

%% Translate method names between formats.
method(<<"HEAD">>) -> head;
method(<<"GET">>) -> get;
method(<<"PUT">>) -> put;
method(<<"POST">>) -> post;
method(<<"TRACE">>) -> trace;
method(<<"OPTIONS">>) -> options;
method(<<"DELETE">>) -> delete;
method(<<"PATCH">>) -> patch;
method(Req) when is_map(Req) -> method(cowboy_req:method(Req));
method(M) -> error({unsupported_method, M}).

%% Any string to a list to characters.
to_string(Int) when is_integer(Int) -> integer_to_list(Int);
to_string(Binary) when is_binary(Binary) -> binary_to_list(Binary);
to_string(List) -> binary_to_list(iolist_to_binary(List)).

%% Dumps any term into a string representation.
dump(Term) ->
    to_string(io_lib:format("~p", [Term])).

pre_hook(Req) ->
    Req.

post_hook(Resp, _State) ->
    Resp.

get_wlanip() ->
    case dgiot_data:get(wlanip) of
        not_find ->
            inets:start(),
            Ip1 =
                case httpc:request(get, {"http://whatismyip.akamai.com/", []}, [], []) of
                    {ok, {_, _, IP}} -> dgiot_utils:to_binary(IP);
                    _ -> <<"127.0.0.1">>
                end,
            dgiot_data:insert(wlanip, Ip1);
        Ip ->
            Ip
    end.
