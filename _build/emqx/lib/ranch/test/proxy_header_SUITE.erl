%% Copyright (c) 2018, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(proxy_header_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).
-import(ct_helper, [name/0]).

%% ct.

all() ->
	ct_helper:all(?MODULE).

%% Tests.

recv_v1_proxy_header_tcp(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = name(),
	ProxyInfo = #{
		version => 1,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v1_proxy_header_tcp_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = name(),
	ProxyInfo = #{
		version => 1,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

recv_v2_proxy_header_tcp(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = name(),
	ProxyInfo = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v2_proxy_header_tcp_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = name(),
	ProxyInfo = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

recv_v2_local_header_tcp(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = name(),
	ProxyInfo = #{
		version => 2,
		command => local
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v2_local_header_tcp_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = name(),
	ProxyInfo = #{
		version => 2,
		command => local
	},
	do_proxy_header_tcp(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

do_proxy_header_tcp(Name, ProxyInfo, Data1, Data2) ->
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		proxy_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [ranch_proxy_header:header(ProxyInfo), Data1]),
	receive
		{proxy_protocol, ProxyInfo} ->
			ok
	after 2000 ->
		error(timeout)
	end,
	ok = gen_tcp:send(Socket, Data2),
	Len1 = byte_size(Data1),
	Len2 = byte_size(Data2),
	{ok, <<Data1:Len1/binary, Data2/binary>>} = gen_tcp:recv(Socket, Len1 + Len2, 1000),
	ok = ranch:stop_listener(Name),
	ok.

recv_v1_proxy_header_ssl(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = name(),
	ProxyInfo = #{
		version => 1,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v1_proxy_header_ssl_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = name(),
	ProxyInfo = #{
		version => 1,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

recv_v2_proxy_header_ssl(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = name(),
	ProxyInfo = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v2_proxy_header_ssl_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = name(),
	ProxyInfo = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 444,
		dest_address => {192, 168, 0, 1},
		dest_port => 443
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

recv_v2_local_header_ssl(_) ->
	doc("Confirm we can read the proxy header at the start of the connection."),
	Name = name(),
	ProxyInfo = #{
		version => 2,
		command => local
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<>>, <<"TCP Ranch is working!">>).

recv_v2_local_header_ssl_extra_data(_) ->
	doc("Confirm we can read the proxy header at the start of the connection "
		"and that the extra data in the first packet can be read afterwards."),
	Name = name(),
	ProxyInfo = #{
		version => 2,
		command => local
	},
	do_proxy_header_ssl(Name, ProxyInfo, <<"HELLO">>, <<"TCP Ranch is working!">>).

do_proxy_header_ssl(Name, ProxyInfo, Data1, Data2) ->
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts,
		proxy_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket0} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket0, [ranch_proxy_header:header(ProxyInfo)]),
	{ok, Socket} = ssl:connect(Socket0, [], 1000),
	ok = ssl:send(Socket, Data1),
	receive
		{proxy_protocol, ProxyInfo} ->
			ok
	after 2000 ->
		error(timeout)
	end,
	ok = ssl:send(Socket, Data2),
	Len1 = byte_size(Data1),
	Len2 = byte_size(Data2),
	{ok, <<Data1:Len1/binary, Data2/binary>>} = ssl:recv(Socket, Len1 + Len2, 1000),
	ok = ranch:stop_listener(Name),
	ok.
