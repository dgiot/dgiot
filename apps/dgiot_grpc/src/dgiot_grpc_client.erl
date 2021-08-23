%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

%% @doc dgiot_grpc Protocol
-module(dgiot_grpc_client).
-include("dgiot_grpc.hrl").
-include_lib("dgiot/include/logger.hrl").
-define(PB_CLIENT_MOD, emqx_exhook_v_1_hook_provider_client).

-export([ start_client_sup/2
]).

%% APIs
-export([
    do_call/3
    , do_init/1
    , do_deinit/1]).

%%--------------------------------------------------------------------
%% APIs

%% for creating a Child spec to hang on another Application supervisor.
start_client_sup(ChannleId, Opts0) ->
%%   Opts0 =  [{scheme,http},{host,"127.0.0.1"},{port,9000}]
    Name = dgiot_utils:to_list(ChannleId),
    {URL, ClientOpts} = channel_opts(Opts0),
    _ = application:ensure_all_started(gproc),
    case uri_string:parse(URL) of
        #{scheme := Scheme, host := Host, port := Port} ->
            Server = {Scheme, Host, Port},
            #{id       => Name,
                start    => {grpc_client_sup, start_link, [Name, Server, ClientOpts]},
                restart  => transient,
                shutdown => infinity,
                type     => supervisor,
                modules  => [grpc_client_sup]};
        {error, _Reason, _} -> []
    end.

%% @private
channel_opts( #{
    <<"scheme">> := Scheme,
    <<"port">> := Port,
    <<"host">> := Host}) ->
    NewScheme =  dgiot_utils:to_atom(Scheme),
    SvrAddr = format_http_uri(NewScheme, Host, Port),
    ClientOpts = case NewScheme of
                     https ->
%%                         SslOpts = lists:keydelete(ssl, 1, proplists:get_value(ssl_options, Opts, [])),
%%                         #{gun_opts =>
%%                         #{transport => ssl,
%%                             transport_opts => SslOpts}};
                         #{};
                     _ -> #{}
                 end,
    {SvrAddr, ClientOpts}.

format_http_uri(Scheme, Host0, Port) ->
    Host = case is_tuple(Host0) of
               true -> inet:ntoa(Host0);
               _ -> dgiot_utils:to_list(Host0)
           end,
    lists:flatten(io_lib:format("~s://~s:~w", [Scheme, Host, Port])).

do_deinit(Name) ->
    _ = do_call(Name, 'on_provider_unloaded', #{}),
    ok.

do_init(ChannName) ->
    Req = #{broker => maps:from_list(emqx_sys:info())},
    case do_call(ChannName, 'on_provider_loaded', Req) of
        {ok, _InitialResp} ->
           ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec do_call(string(), atom(), map()) -> {ok, map()} | {error, term()}.
do_call(ChannName, Fun, Req) ->
    Options = #{channel => ChannName},
    ?LOG(debug, "Call ~0p:~0p(~0p, ~0p)", [?PB_CLIENT_MOD, Fun, Req, Options]),
    case catch apply(?PB_CLIENT_MOD, Fun, [Req, Options]) of
        {ok, Resp, _Metadata} ->
            ?LOG(debug, "Response {ok, ~0p, ~0p}", [Resp, _Metadata]),
            {ok, Resp};
        {error, {Code, Msg}, _Metadata} ->
            ?LOG(error, "CALL ~0p:~0p(~0p, ~0p) response errcode: ~0p, errmsg: ~0p",
                [?PB_CLIENT_MOD, Fun, Req, Options, Code, Msg]),
            {error, {Code, Msg}};
        {error, Reason} ->
            ?LOG(error, "CALL ~0p:~0p(~0p, ~0p) error: ~0p",
                [?PB_CLIENT_MOD, Fun, Req, Options, Reason]),
            {error, Reason};
        {'EXIT', {Reason, Stk}} ->
            ?LOG(error, "CALL ~0p:~0p(~0p, ~0p) throw an exception: ~0p, stacktrace: ~0p",
                [?PB_CLIENT_MOD, Fun, Req, Options, Reason, Stk]),
            {error, Reason}
    end.
