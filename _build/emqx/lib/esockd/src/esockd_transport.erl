%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(esockd_transport).

-include("esockd.hrl").
-include_lib("ssl/src/ssl_api.hrl").

-export([type/1, is_ssl/1]).
-export([listen/2]).
-export([ready/3, wait/1]).
-export([send/2, async_send/2, recv/2, recv/3, async_recv/2, async_recv/3]).
-export([controlling_process/2]).
-export([close/1, fast_close/1]).
-export([getopts/2, setopts/2, getstat/2]).
-export([sockname/1, peername/1, shutdown/2]).
-export([peercert/1, peer_cert_subject/1, peer_cert_common_name/1]).
-export([ssl_upgrade_fun/1]).
-export([proxy_upgrade_fun/1]).
-export([ensure_ok_or_exit/2]).
-export([gc/1]).

-export_type([socket/0]).

-type(ssl_socket() :: #ssl_socket{}).
-type(proxy_socket() :: #proxy_socket{}).
-type(socket() :: inet:socket() | ssl_socket() | proxy_socket() | #sslsocket{}).

-spec(type(socket()) -> tcp | ssl | proxy).
type(Sock) when is_port(Sock) ->
    tcp;
type(#ssl_socket{})  ->
    ssl;
type(#proxy_socket{}) ->
    proxy.

-spec(is_ssl(socket()) -> boolean()).
is_ssl(Sock) when is_port(Sock) ->
    false;
is_ssl(#ssl_socket{})  ->
    true;
is_ssl(#proxy_socket{socket = Sock}) ->
    is_ssl(Sock).

-spec(ready(pid(), socket(), [esockd:sock_fun()]) -> any()).
ready(Pid, Sock, UpgradeFuns) ->
    Pid ! {sock_ready, Sock, UpgradeFuns}.

-spec(wait(socket()) -> {ok, socket()} | {error, term()}).
wait(Sock) ->
    receive
        {sock_ready, Sock, UpgradeFuns} ->
            upgrade(Sock, UpgradeFuns)
    end.

-spec(upgrade(socket(), [esockd:sock_fun()]) -> {ok, socket()} | {error, term()}).
upgrade(Sock, []) ->
    {ok, Sock};
upgrade(Sock, [Upgrade | More]) ->
    case Upgrade(Sock) of
        {ok, NewSock} -> upgrade(NewSock, More);
        Error         -> fast_close(Sock), Error
    end.

-spec(listen(inet:port_number(), [gen_tcp:listen_option()])
      -> {ok, inet:socket()} | {error, system_limit | inet:posix()}).
listen(Port, Opts) ->
    gen_tcp:listen(Port, Opts).

-spec(controlling_process(socket(), pid()) -> ok | {error, Reason} when
      Reason :: closed | not_owner | badarg | inet:posix()).
controlling_process(Sock, NewOwner) when is_port(Sock) ->
    gen_tcp:controlling_process(Sock, NewOwner);
controlling_process(#ssl_socket{ssl = SslSock}, NewOwner) ->
    ssl:controlling_process(SslSock, NewOwner);
controlling_process(#proxy_socket{socket = Sock}, NewOwner) ->
    controlling_process(Sock, NewOwner);
%% Before upgrading, the DTLS socket is #sslsocket{}
%% type, instead of a port().
%%
%% See: ssl:transport_accept/1
controlling_process(SslSock = #sslsocket{}, NewOwner) ->
    ssl:controlling_process(SslSock, NewOwner).

-spec(close(socket()) -> ok | {error, term()}).
close(Sock) when is_port(Sock) ->
    gen_tcp:close(Sock);
close(#ssl_socket{ssl = SslSock}) ->
    ssl:close(SslSock);
close(#proxy_socket{socket = Sock}) ->
    close(Sock).

-spec(fast_close(socket()) -> ok).
fast_close(Sock) when is_port(Sock) ->
    catch port_close(Sock), ok;
fast_close(#ssl_socket{tcp = Sock, ssl = SslSock}) ->
    _ = fast_close_sslsock(SslSock),
    catch port_close(Sock), ok;
fast_close(SslSock = #sslsocket{}) ->
    fast_close_sslsock(SslSock);
fast_close(#proxy_socket{socket = Sock}) ->
    fast_close(Sock).

%% @private
fast_close_sslsock(SslSock) ->
    {Pid, MRef} = spawn_monitor(fun() -> ssl:close(SslSock) end),
    TRef = erlang:send_after(?SSL_CLOSE_TIMEOUT, self(), {Pid, ssl_close_timeout}),
    receive
        {Pid, ssl_close_timeout} ->
            erlang:demonitor(MRef, [flush]),
            exit(Pid, kill);
        {'DOWN', MRef, process, Pid, _Reason} ->
            erlang:cancel_timer(TRef)
    end.

-spec(send(socket(), iodata()) -> ok | {error, Reason} when
      Reason :: closed | timeout | inet:posix()).
send(Sock, Data) when is_port(Sock) ->
    gen_tcp:send(Sock, Data);
send(#ssl_socket{ssl = SslSock}, Data) ->
    ssl:send(SslSock, Data);
send(#proxy_socket{socket = Sock}, Data) ->
    send(Sock, Data).

%% @doc Port command to write data.
-spec(async_send(socket(), iodata()) -> ok | {error, Reason} when
      Reason :: close | timeout | inet:posix()).
async_send(Sock, Data) when is_port(Sock) ->
    try erlang:port_command(Sock, Data, []) of
        true -> ok;
        false -> %% nosuspend option and port busy
            {error, busy}
    catch
        error:_Error ->
            {error, einval}
    end;
async_send(Sock = #ssl_socket{ssl = SslSock}, Data) ->
    case ssl:send(SslSock, Data) of
        ok -> self() ! {inet_reply, Sock, ok}, ok;
        Error -> Error
    end;
async_send(#proxy_socket{socket = Sock}, Data) ->
    async_send(Sock, Data).

-spec(recv(socket(), non_neg_integer())
      -> {ok, iodata()} | {error, closed | inet:posix()}).
recv(Sock, Length) when is_port(Sock) ->
    gen_tcp:recv(Sock, Length);
recv(#ssl_socket{ssl = SslSock}, Length) ->
    ssl:recv(SslSock, Length);
recv(#proxy_socket{socket = Sock}, Length) ->
    recv(Sock, Length).

-spec(recv(socket(), non_neg_integer(), timeout())
      -> {ok, iodata()} | {error, closed | inet:posix()}).
recv(Sock, Length, Timeout) when is_port(Sock) ->
    gen_tcp:recv(Sock, Length, Timeout);
recv(#ssl_socket{ssl = SslSock}, Length, Timeout)  ->
    ssl:recv(SslSock, Length, Timeout);
recv(#proxy_socket{socket = Sock}, Length, Timeout) ->
    recv(Sock, Length, Timeout).

%% @doc Async receive data.
-spec(async_recv(socket(), non_neg_integer()) -> {ok, reference()}).
async_recv(Sock, Length) ->
    async_recv(Sock, Length, infinity).

-spec(async_recv(socket(), non_neg_integer(), timeout()) -> {ok, reference()}).
async_recv(Sock = #ssl_socket{ssl = SslSock}, Length, Timeout) ->
    Self = self(),
    Ref = make_ref(),
    spawn(fun() ->
              Self ! {inet_async, Sock, Ref, ssl:recv(SslSock, Length, Timeout)}
          end),
    {ok, Ref};
async_recv(Sock, Length, infinity) when is_port(Sock) ->
    prim_inet:async_recv(Sock, Length, -1);
async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    prim_inet:async_recv(Sock, Length, Timeout);
async_recv(#proxy_socket{socket = Sock}, Length, Timeout) ->
    async_recv(Sock, Length, Timeout).

%% @doc Get socket options.
-spec(getopts(socket(), [inet:socket_getopt()])
      -> {ok, [inet:socket_setopt()]} | {error, inet:posix()}).
getopts(Sock, OptionNames) when is_port(Sock) ->
    inet:getopts(Sock, OptionNames);
getopts(#ssl_socket{ssl = SslSock}, OptionNames) ->
    ssl:getopts(SslSock, OptionNames);
getopts(SslSock = #sslsocket{}, OptionNames) ->
    ssl:getopts(SslSock, OptionNames);
getopts(#proxy_socket{socket = Sock}, OptionNames) ->
    getopts(Sock, OptionNames).

%% @doc Set socket options
-spec(setopts(socket(), [inet:socket_setopt()]) -> ok | {error, inet:posix()}).
setopts(Sock, Opts) when is_port(Sock) ->
    inet:setopts(Sock, Opts);
setopts(#ssl_socket{ssl = SslSock}, Opts) ->
    ssl:setopts(SslSock, Opts);
setopts(SslSock = #sslsocket{}, Opts) ->
    ssl:setopts(SslSock, Opts);
setopts(#proxy_socket{socket = Socket}, Opts) ->
    setopts(Socket, Opts).

%% @doc Get socket stats
-spec(getstat(socket(), [inet:stat_option()])
      -> {ok, [{inet:stat_option(), integer()}]} | {error, inet:posix()}).
getstat(Sock, Stats) when is_port(Sock) ->
    inet:getstat(Sock, Stats);
getstat(#ssl_socket{tcp = Sock}, Stats) ->
    inet:getstat(Sock, Stats);
getstat(SslSock = #sslsocket{}, Stats) ->
    ssl:getstat(SslSock, Stats);
getstat(#proxy_socket{socket = Sock}, Stats) ->
    getstat(Sock, Stats).

%% @doc Sockname
-spec(sockname(socket()) -> {ok, {inet:ip_address(), inet:port_number()}} |
                          {error, inet:posix()}).
sockname(Sock) when is_port(Sock) ->
    inet:sockname(Sock);
sockname(#ssl_socket{ssl = SslSock}) ->
    ssl:sockname(SslSock);
sockname(SslSock = #sslsocket{}) ->
    ssl:sockname(SslSock);
sockname(#proxy_socket{dst_addr = DstAddr, dst_port = DstPort}) ->
    {ok, {DstAddr, DstPort}}.

%% @doc Peername
-spec(peername(socket()) -> {ok, {inet:ip_address(), inet:port_number()}} |
                          {error, inet:posix()}).
peername(Sock) when is_port(Sock) ->
    inet:peername(Sock);
peername(#ssl_socket{ssl = SslSock}) ->
    ssl:peername(SslSock);
peername(#proxy_socket{src_addr = SrcAddr, src_port = SrcPort}) ->
    {ok, {SrcAddr, SrcPort}};
%% Before upgrading, the DTLS socket is #sslsocket{}
%% type, instead of a port().
%%
%% See: ssl:transport_accept/1
peername(SslSock = #sslsocket{}) ->
    ssl:peername(SslSock).

%% @doc Socket peercert
-spec(peercert(socket()) -> nossl | binary() | list(pp2_additional_ssl_field()) |
                          {error, term()}).
peercert(Sock) when is_port(Sock) ->
    nossl;
peercert(#ssl_socket{ssl = SslSock}) ->
    case ssl:peercert(SslSock) of
        {ok, Cert} -> Cert;
        %% One-way SSL
        {error, no_peercert} ->
            undefined;
        Error -> Error
    end;
peercert(SslSock = #sslsocket{}) ->
    case ssl:peercert(SslSock) of
        {ok, Cert} -> Cert;
        {error, no_peercert} -> undefined;
        Error -> Error
    end;
peercert(#proxy_socket{pp2_additional_info = AdditionalInfo}) ->
    proplists:get_value(pp2_ssl, AdditionalInfo, []).

%% @doc Peercert subject
-spec(peer_cert_subject(socket()) -> undefined | binary()).
peer_cert_subject(Sock) when is_port(Sock) ->
    undefined;
peer_cert_subject(#ssl_socket{ssl = SslSock}) ->
    case ssl:peercert(SslSock) of
        {ok, Cert} ->
            esockd_ssl:peer_cert_subject(Cert);
        _Error -> undefined
    end;
peer_cert_subject(Sock = #proxy_socket{}) ->
    %% Common Name? Haproxy PP2 will not pass subject.
    peer_cert_common_name(Sock).

%% @doc Peercert common name
-spec(peer_cert_common_name(socket()) -> undefined | binary()).
peer_cert_common_name(Sock) when is_port(Sock) ->
    undefined;
peer_cert_common_name(#ssl_socket{ssl = SslSock}) ->
    case ssl:peercert(SslSock) of
        {ok, Cert} ->
            esockd_ssl:peer_cert_common_name(Cert);
        _Error -> undefined
    end;
peer_cert_common_name(#proxy_socket{pp2_additional_info = AdditionalInfo}) ->
    proplists:get_value(pp2_ssl_cn,
                        proplists:get_value(pp2_ssl, AdditionalInfo, [])).

%% @doc Shutdown socket
-spec(shutdown(socket(), How) -> ok | {error, inet:posix()} when
    How :: read | write | read_write).
shutdown(Sock, How) when is_port(Sock) ->
    gen_tcp:shutdown(Sock, How);
shutdown(#ssl_socket{ssl = SslSock}, How) ->
    ssl:shutdown(SslSock, How);
shutdown(#proxy_socket{socket = Sock}, How) ->
    shutdown(Sock, How).

%% @doc TCP/DTLS socket -> #ssl_socket{}
-spec(ssl_upgrade_fun([ssl:ssl_option()]) -> esockd:sock_fun()).
ssl_upgrade_fun(SslOpts) ->
    {Timeout, SslOpts1} = take_handshake_timeout(SslOpts),
    fun(Sock) when is_port(Sock);             % for tcp
                   is_record(Sock, sslsocket) % for dtls
                   ->
        Args = case is_port(Sock) of
                   true -> [Sock, SslOpts1, Timeout];
                   false -> [Sock, Timeout]
               end,
        try erlang:apply(ssl, handshake, Args) of
            {ok, SslSock} ->
                {ok, #ssl_socket{tcp = Sock, ssl = SslSock}};
            {ok, SslSock, _Ext} -> %% OTP 21.0
                {ok, #ssl_socket{tcp = Sock, ssl = SslSock}};
            {error, Reason} when Reason =:= closed; Reason =:= timeout ->
                {error, Reason};
            {error, Reason} ->
                {error, {ssl_error, Reason}}
        catch
            _Error:Reason ->
                {error, {ssl_failure, Reason}}
        end
    end.

take_handshake_timeout(SslOpts) ->
    case lists:keytake(handshake_timeout, 1, SslOpts) of
        {value, {handshake_timeout, Timeout}, SslOpts1} ->
            {Timeout, SslOpts1};
        false ->
            {?SSL_HANDSHAKE_TIMEOUT, SslOpts}
    end.

%% @doc TCP | SSL -> ProxySocket
proxy_upgrade_fun(Opts) ->
    Timeout = proxy_protocol_timeout(Opts),
    fun(Sock) ->
        case esockd_proxy_protocol:recv(?MODULE, Sock, Timeout) of
            {ok, ProxySock} -> {ok, ProxySock};
            {error, Reason} -> {error, Reason}
        end
    end.

proxy_protocol_timeout(Opts) ->
    proplists:get_value(proxy_protocol_timeout, Opts, ?PROXY_RECV_TIMEOUT).

-spec(ensure_ok_or_exit(atom(), list(term())) -> term()).
ensure_ok_or_exit(Fun, Args = [Sock|_]) when is_atom(Fun), is_list(Args) ->
    case erlang:apply(?MODULE, Fun, Args) of
        {error, Reason} when Reason =:= enotconn; Reason =:= closed ->
            fast_close(Sock),
            exit(normal);
        {error, Reason} ->
            fast_close(Sock),
            exit({shutdown, Reason});
         Result -> Result
    end.

gc(Sock) when is_port(Sock) ->
    ok;
%% Defined in ssl/src/ssl_api.hrl:
%% -record(sslsocket, {fd = nil, pid = nil}).
gc(#ssl_socket{ssl = {sslsocket, _, Pid}}) when is_pid(Pid) ->
    erlang:garbage_collect(Pid);
gc(#proxy_socket{socket = Sock}) ->
    gc(Sock);
gc(_Sock) -> ok.

