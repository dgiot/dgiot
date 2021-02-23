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

-module(ranch_proxy_header).

-export([parse/1]).
-export([header/1]).
-export([header/2]).

-type proxy_info() :: #{
	%% Mandatory part.
	version := 1 | 2,
	command := local | proxy,
	transport_family => undefined | ipv4 | ipv6 | unix,
	transport_protocol => undefined | stream | dgram,
	%% Addresses.
	src_address => inet:ip_address() | binary(),
	src_port => inet:port_number(),
	dest_address => inet:ip_address() | binary(),
	dest_port => inet:port_number(),
	%% Extra TLV-encoded data.
	alpn => binary(), %% US-ASCII.
	authority => binary(), %% UTF-8.
	ssl => #{
		client := [ssl | cert_conn | cert_sess],
		verified := boolean(),
		version => binary(), %% US-ASCII.
		cipher => binary(), %% US-ASCII.
		sig_alg => binary(), %% US-ASCII.
		key_alg => binary(), %% US-ASCII.
		cn => binary() %% UTF-8.
	},
	netns => binary(), %% US-ASCII.
	%% Unknown TLVs can't be parsed so the raw data is given.
	raw_tlvs => [{0..255, binary()}]
}.
-export_type([proxy_info/0]).

-type build_opts() :: #{
	checksum => crc32c,
	padding => pos_integer() %% >= 3
}.

%% Parsing.

-spec parse(Data) -> {ok, proxy_info(), Data} | {error, atom()} when Data::binary().
parse(<<"\r\n\r\n\0\r\nQUIT\n", Rest/bits>>) ->
	parse_v2(Rest);
parse(<<"PROXY ", Rest/bits>>) ->
	parse_v1(Rest);
parse(_) ->
	{error, 'The PROXY protocol header signature was not recognized. (PP 2.1, PP 2.2)'}.

-ifdef(TEST).
parse_unrecognized_header_test() ->
	{error, _} = parse(<<"GET / HTTP/1.1\r\n">>),
	ok.
-endif.

%% Human-readable header format (Version 1).
parse_v1(<<"TCP4 ", Rest/bits>>) ->
	parse_v1(Rest, ipv4);
parse_v1(<<"TCP6 ", Rest/bits>>) ->
	parse_v1(Rest, ipv6);
parse_v1(<<"UNKNOWN\r\n", Rest/bits>>) ->
	{ok, #{
		version => 1,
		command => proxy,
		transport_family => undefined,
		transport_protocol => undefined
	}, Rest};
parse_v1(<<"UNKNOWN ", Rest0/bits>>) ->
	case binary:split(Rest0, <<"\r\n">>) of
		[_, Rest] ->
			{ok, #{
				version => 1,
				command => proxy,
				transport_family => undefined,
				transport_protocol => undefined
			}, Rest};
		[_] ->
			{error, 'Malformed or incomplete PROXY protocol header line. (PP 2.1)'}
	end;
parse_v1(_) ->
	{error, 'The INET protocol and family string was not recognized. (PP 2.1)'}.

parse_v1(Rest0, Family) ->
	try
		{ok, SrcAddr, Rest1} = parse_ip(Rest0, Family),
		{ok, DestAddr, Rest2} = parse_ip(Rest1, Family),
		{ok, SrcPort, Rest3} = parse_port(Rest2, $\s),
		{ok, DestPort, Rest4} = parse_port(Rest3, $\r),
		<<"\n", Rest/bits>> = Rest4,
		{ok, #{
			version => 1,
			command => proxy,
			transport_family => Family,
			transport_protocol => stream,
			src_address => SrcAddr,
			src_port => SrcPort,
			dest_address => DestAddr,
			dest_port => DestPort
		}, Rest}
	catch
		throw:parse_ipv4_error ->
			{error, 'Failed to parse an IPv4 address in the PROXY protocol header line. (PP 2.1)'};
		throw:parse_ipv6_error ->
			{error, 'Failed to parse an IPv6 address in the PROXY protocol header line. (PP 2.1)'};
		throw:parse_port_error ->
			{error, 'Failed to parse a port number in the PROXY protocol header line. (PP 2.1)'};
		_:_ ->
			{error, 'Malformed or incomplete PROXY protocol header line. (PP 2.1)'}
	end.

parse_ip(<<Addr:7/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:8/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:9/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:10/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:11/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:12/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:13/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:14/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(<<Addr:15/binary, $\s, Rest/binary>>, ipv4) -> parse_ipv4(Addr, Rest);
parse_ip(Data, ipv6) ->
	[Addr, Rest] = binary:split(Data, <<$\s>>),
	parse_ipv6(Addr, Rest).

parse_ipv4(Addr0, Rest) ->
	case inet:parse_ipv4strict_address(binary_to_list(Addr0)) of
		{ok, Addr} -> {ok, Addr, Rest};
		{error, einval} -> throw(parse_ipv4_error)
	end.

parse_ipv6(Addr0, Rest) ->
	case inet:parse_ipv6strict_address(binary_to_list(Addr0)) of
		{ok, Addr} -> {ok, Addr, Rest};
		{error, einval} -> throw(parse_ipv6_error)
	end.

parse_port(<<Port:1/binary, C, Rest/bits>>, C) -> parse_port(Port, Rest);
parse_port(<<Port:2/binary, C, Rest/bits>>, C) -> parse_port(Port, Rest);
parse_port(<<Port:3/binary, C, Rest/bits>>, C) -> parse_port(Port, Rest);
parse_port(<<Port:4/binary, C, Rest/bits>>, C) -> parse_port(Port, Rest);
parse_port(<<Port:5/binary, C, Rest/bits>>, C) -> parse_port(Port, Rest);

parse_port(Port0, Rest) ->
	try binary_to_integer(Port0) of
		Port when Port > 0, Port =< 65535 ->
			{ok, Port, Rest};
		_ ->
			throw(parse_port_error)
	catch _:_ ->
		throw(parse_port_error)
	end.

-ifdef(TEST).
parse_v1_test() ->
	%% Examples taken from the PROXY protocol header specification.
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := ipv4,
		transport_protocol := stream,
		src_address := {255, 255, 255, 255},
		src_port := 65535,
		dest_address := {255, 255, 255, 255},
		dest_port := 65535
	}, <<>>} = parse(<<"PROXY TCP4 255.255.255.255 255.255.255.255 65535 65535\r\n">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := ipv6,
		transport_protocol := stream,
		src_address := {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535},
		src_port := 65535,
		dest_address := {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535},
		dest_port := 65535
	}, <<>>} = parse(<<"PROXY TCP6 "
		"ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff "
		"ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 65535 65535\r\n">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := undefined,
		transport_protocol := undefined
	}, <<>>} = parse(<<"PROXY UNKNOWN\r\n">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := undefined,
		transport_protocol := undefined
	}, <<>>} = parse(<<"PROXY UNKNOWN "
		"ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff "
		"ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff 65535 65535\r\n">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := ipv4,
		transport_protocol := stream,
		src_address := {192, 168, 0, 1},
		src_port := 56324,
		dest_address := {192, 168, 0, 11},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r\nHost: 192.168.0.11\r\n\r\n">>} = parse(<<
		"PROXY TCP4 192.168.0.1 192.168.0.11 56324 443\r\n"
		"GET / HTTP/1.1\r\n"
		"Host: 192.168.0.11\r\n"
		"\r\n">>),
	%% Test cases taken from tomciopp/proxy_protocol.
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := ipv4,
		transport_protocol := stream,
		src_address := {192, 168, 0, 1},
		src_port := 56324,
		dest_address := {192, 168, 0, 11},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r">>} = parse(<<
		"PROXY TCP4 192.168.0.1 192.168.0.11 56324 443\r\nGET / HTTP/1.1\r">>),
	{error, _} = parse(<<"PROXY TCP4 192.1638.0.1 192.168.0.11 56324 443\r\nGET / HTTP/1.1\r">>),
	{error, _} = parse(<<"PROXY TCP4 192.168.0.1 192.168.0.11 1111111 443\r\nGET / HTTP/1.1\r">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := ipv6,
		transport_protocol := stream,
		src_address := {8193, 3512, 0, 66, 0, 35374, 880, 29492},
		src_port := 4124,
		dest_address := {8193, 3512, 0, 66, 0, 35374, 880, 29493},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r">>} = parse(<<"PROXY TCP6 "
		"2001:0db8:0000:0042:0000:8a2e:0370:7334 "
		"2001:0db8:0000:0042:0000:8a2e:0370:7335 4124 443\r\nGET / HTTP/1.1\r">>),
	{error, _} = parse(<<"PROXY TCP6 "
		"2001:0db8:0000:0042:0000:8a2e:0370:7334 "
		"2001:0db8:00;0:0042:0000:8a2e:0370:7335 4124 443\r\nGET / HTTP/1.1\r">>),
	{error, _} = parse(<<"PROXY TCP6 "
		"2001:0db8:0000:0042:0000:8a2e:0370:7334 "
		"2001:0db8:0000:0042:0000:8a2e:0370:7335 4124 foo\r\nGET / HTTP/1.1\r">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := undefined,
		transport_protocol := undefined
	}, <<"GET / HTTP/1.1\r">>} = parse(<<"PROXY UNKNOWN 4124 443\r\nGET / HTTP/1.1\r">>),
	{ok, #{
		version := 1,
		command := proxy,
		transport_family := undefined,
		transport_protocol := undefined
	}, <<"GET / HTTP/1.1\r">>} = parse(<<"PROXY UNKNOWN\r\nGET / HTTP/1.1\r">>),
	ok.
-endif.

%% Binary header format (version 2).

%% LOCAL.
parse_v2(<<2:4, 0:4, _:8, Len:16, Rest0/bits>>) ->
	case Rest0 of
		<<_:Len/binary, Rest/bits>> ->
			{ok, #{
				version => 2,
				command => local
			}, Rest};
		_ ->
			{error, 'Missing data in the PROXY protocol binary header. (PP 2.2)'}
	end;
%% PROXY.
parse_v2(<<2:4, 1:4, Family:4, Protocol:4, Len:16, Rest/bits>>)
		when Family =< 3, Protocol =< 2 ->
	case Rest of
		<<Header:Len/binary, _/bits>> ->
			parse_v2(Rest, Len, parse_family(Family), parse_protocol(Protocol),
				<<Family:4, Protocol:4, Len:16, Header:Len/binary>>);
		_ ->
			{error, 'Missing data in the PROXY protocol binary header. (PP 2.2)'}
	end;
%% Errors.
parse_v2(<<Version:4, _/bits>>) when Version =/= 2 ->
	{error, 'Invalid version in the PROXY protocol binary header. (PP 2.2)'};
parse_v2(<<_:4, Command:4, _/bits>>) when Command > 1 ->
	{error, 'Invalid command in the PROXY protocol binary header. (PP 2.2)'};
parse_v2(<<_:8, Family:4, _/bits>>) when Family > 3 ->
	{error, 'Invalid address family in the PROXY protocol binary header. (PP 2.2)'};
parse_v2(<<_:12, Protocol:4, _/bits>>) when Protocol > 2 ->
	{error, 'Invalid transport protocol in the PROXY protocol binary header. (PP 2.2)'}.

parse_family(0) -> undefined;
parse_family(1) -> ipv4;
parse_family(2) -> ipv6;
parse_family(3) -> unix.

parse_protocol(0) -> undefined;
parse_protocol(1) -> stream;
parse_protocol(2) -> dgram.

parse_v2(Data, Len, Family, Protocol, _)
		when Family =:= undefined; Protocol =:= undefined ->
	<<_:Len/binary, Rest/bits>> = Data,
	{ok, #{
		version => 2,
		command => proxy,
		%% In case only one value was undefined, we set both explicitly.
		%% It doesn't make sense to have only one known value.
		transport_family => undefined,
		transport_protocol => undefined
	}, Rest};
parse_v2(<<
		S1, S2, S3, S4,
		D1, D2, D3, D4,
		SrcPort:16, DestPort:16, Rest/bits>>, Len, Family=ipv4, Protocol, Header)
		when Len >= 12 ->
	parse_tlv(Rest, Len - 12, #{
		version => 2,
		command => proxy,
		transport_family => Family,
		transport_protocol => Protocol,
		src_address => {S1, S2, S3, S4},
		src_port => SrcPort,
		dest_address => {D1, D2, D3, D4},
		dest_port => DestPort
	}, Header);
parse_v2(<<
		S1:16, S2:16, S3:16, S4:16, S5:16, S6:16, S7:16, S8:16,
		D1:16, D2:16, D3:16, D4:16, D5:16, D6:16, D7:16, D8:16,
		SrcPort:16, DestPort:16, Rest/bits>>, Len, Family=ipv6, Protocol, Header)
		when Len >= 36 ->
	parse_tlv(Rest, Len - 36, #{
		version => 2,
		command => proxy,
		transport_family => Family,
		transport_protocol => Protocol,
		src_address => {S1, S2, S3, S4, S5, S6, S7, S8},
		src_port => SrcPort,
		dest_address => {D1, D2, D3, D4, D5, D6, D7, D8},
		dest_port => DestPort
	}, Header);
parse_v2(<<SrcAddr0:108/binary, DestAddr0:108/binary, Rest/bits>>,
		Len, Family=unix, Protocol, Header)
		when Len >= 216 ->
	try
		[SrcAddr, _] = binary:split(SrcAddr0, <<0>>),
		true = byte_size(SrcAddr) > 0,
		[DestAddr, _] = binary:split(DestAddr0, <<0>>),
		true = byte_size(DestAddr) > 0,
		parse_tlv(Rest, Len - 216, #{
			version => 2,
			command => proxy,
			transport_family => Family,
			transport_protocol => Protocol,
			src_address => SrcAddr,
			dest_address => DestAddr
		}, Header)
	catch _:_ ->
		{error, 'Invalid UNIX address in PROXY protocol binary header. (PP 2.2)'}
	end;
parse_v2(_, _, _, _, _) ->
	{error, 'Invalid length in the PROXY protocol binary header. (PP 2.2)'}.

-ifdef(TEST).
parse_v2_test() ->
	%% Test cases taken from tomciopp/proxy_protocol.
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := ipv4,
		transport_protocol := stream,
		src_address := {127, 0, 0, 1},
		src_port := 444,
		dest_address := {192, 168, 0, 1},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10, %% Signature.
		33, %% Version and command.
		17, %% Family and protocol.
		0, 12, %% Length.
		127, 0, 0, 1, %% Source address.
		192, 168, 0, 1, %% Destination address.
		1, 188, %% Source port.
		1, 187, %% Destination port.
		"GET / HTTP/1.1\r\n">>),
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := ipv4,
		transport_protocol := dgram,
		src_address := {127, 0, 0, 1},
		src_port := 444,
		dest_address := {192, 168, 0, 1},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10, %% Signature.
		33, %% Version and command.
		18, %% Family and protocol.
		0, 12, %% Length.
		127, 0, 0, 1, %% Source address.
		192, 168, 0, 1, %% Destination address.
		1, 188, %% Source port.
		1, 187, %% Destination port.
		"GET / HTTP/1.1\r\n">>),
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := ipv6,
		transport_protocol := stream,
		src_address := {5532, 4240, 1, 0, 0, 0, 0, 0},
		src_port := 444,
		dest_address := {8193, 3512, 1, 0, 0, 0, 0, 0},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10, %% Signature.
		33, %% Version and command.
		33, %% Family and protocol.
		0, 36, %% Length.
		21, 156, 16, 144, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, %% Source address.
		32, 1, 13, 184, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, %% Destination address.
		1, 188, %% Source port.
		1, 187, %% Destination port.
		"GET / HTTP/1.1\r\n">>),
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := ipv6,
		transport_protocol := dgram,
		src_address := {5532, 4240, 1, 0, 0, 0, 0, 0},
		src_port := 444,
		dest_address := {8193, 3512, 1, 0, 0, 0, 0, 0},
		dest_port := 443
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10, %% Signature.
		33, %% Version and command.
		34, %% Family and protocol.
		0, 36, %% Length.
		21, 156, 16, 144, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, %% Source address.
		32, 1, 13, 184, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, %% Destination address.
		1, 188, %% Source port.
		1, 187, %% Destination port.
		"GET / HTTP/1.1\r\n">>),
	Path = <<"/var/pgsql_sock">>,
	Len = byte_size(Path),
	Padding = 8 * (108 - Len),
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := unix,
		transport_protocol := stream,
		src_address := Path,
		dest_address := Path
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10,
		33,
		49,
		0, 216,
		Path/binary, 0:Padding,
		Path/binary, 0:Padding,
		"GET / HTTP/1.1\r\n">>),
	{ok, #{
		version := 2,
		command := proxy,
		transport_family := unix,
		transport_protocol := dgram,
		src_address := Path,
		dest_address := Path
	}, <<"GET / HTTP/1.1\r\n">>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10,
		33,
		50,
		0, 216,
		Path/binary, 0:Padding,
		Path/binary, 0:Padding,
		"GET / HTTP/1.1\r\n">>),
	ok.

parse_v2_regression_test() ->
	%% Real packet received from AWS. We confirm that the CRC32C
	%% check succeeds only (in other words that ok is returned).
	{ok, _, <<>>} = parse(<<
		13, 10, 13, 10, 0, 13, 10, 81, 85, 73, 84, 10, 33, 17, 0, 84,
		172, 31, 7, 113, 172, 31, 10, 31, 200, 242, 0, 80, 3, 0, 4,
		232, 214, 137, 45, 234, 0, 23, 1, 118, 112, 99, 101, 45, 48,
		56, 100, 50, 98, 102, 49, 53, 102, 97, 99, 53, 48, 48, 49, 99,
		57, 4, 0, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>),
	ok.
-endif.

parse_tlv(Rest, 0, Info, _) ->
	{ok, Info, Rest};
%% PP2_TYPE_ALPN.
parse_tlv(<<16#1, TLVLen:16, ALPN:TLVLen/binary, Rest/bits>>, Len, Info, Header) ->
	parse_tlv(Rest, Len - TLVLen - 3, Info#{alpn => ALPN}, Header);
%% PP2_TYPE_AUTHORITY.
parse_tlv(<<16#2, TLVLen:16, Authority:TLVLen/binary, Rest/bits>>, Len, Info, Header) ->
	parse_tlv(Rest, Len - TLVLen - 3, Info#{authority => Authority}, Header);
%% PP2_TYPE_CRC32C.
parse_tlv(<<16#3, TLVLen:16, CRC32C:32, Rest/bits>>, Len0, Info, Header) when TLVLen =:= 4 ->
	Len = Len0 - TLVLen - 3,
	BeforeLen = byte_size(Header) - Len - TLVLen,
	<<Before:BeforeLen/binary, _:32, After:Len/binary>> = Header,
	%% The initial CRC is ranch_crc32c:crc32c(<<"\r\n\r\n\0\r\nQUIT\n", 2:4, 1:4>>).
	case ranch_crc32c:crc32c(2900412422, [Before, <<0:32>>, After]) of
		CRC32C ->
			parse_tlv(Rest, Len, Info, Header);
		_ ->
			{error, 'Failed CRC32C verification in PROXY protocol binary header. (PP 2.2)'}
	end;
%% PP2_TYPE_NOOP.
parse_tlv(<<16#4, TLVLen:16, _:TLVLen/binary, Rest/bits>>, Len, Info, Header) ->
	parse_tlv(Rest, Len - TLVLen - 3, Info, Header);
%% PP2_TYPE_SSL.
parse_tlv(<<16#20, TLVLen:16, Client, Verify:32, Rest0/bits>>, Len, Info, Header) ->
	SubsLen = TLVLen - 5,
	case Rest0 of
		<<Subs:SubsLen/binary, Rest/bits>> ->
			SSL0 = #{
				client => parse_client(<<Client>>),
				verified => Verify =:= 0
			},
			case parse_ssl_tlv(Subs, SubsLen, SSL0) of
				{ok, SSL, <<>>} ->
					parse_tlv(Rest, Len - TLVLen - 3, Info#{ssl => SSL}, Header);
				Error={error, _} ->
					Error
			end;
		_ ->
			{error, 'Invalid TLV length in the PROXY protocol binary header. (PP 2.2)'}
	end;
%% PP2_TYPE_NETNS.
parse_tlv(<<16#30, TLVLen:16, NetNS:TLVLen/binary, Rest/bits>>, Len, Info, Header) ->
	parse_tlv(Rest, Len - TLVLen - 3, Info#{netns => NetNS}, Header);
%% Unknown TLV.
parse_tlv(<<TLVType, TLVLen:16, TLVValue:TLVLen/binary, Rest/bits>>, Len, Info, Header) ->
	RawTLVs = maps:get(raw_tlvs, Info, []),
	parse_tlv(Rest, Len - TLVLen - 3, Info#{raw_tlvs => [{TLVType, TLVValue}|RawTLVs]}, Header);
%% Invalid TLV length.
parse_tlv(_, _, _, _) ->
	{error, 'Invalid TLV length in the PROXY protocol binary header. (PP 2.2)'}.

parse_client(<<_:5, ClientCertSess:1, ClientCertConn:1, ClientSSL:1>>) ->
	Client0 = case ClientCertSess of
		0 -> [];
		1 -> [cert_sess]
	end,
	Client1 = case ClientCertConn of
		0 -> Client0;
		1 -> [cert_conn|Client0]
	end,
	case ClientSSL of
		0 -> Client1;
		1 -> [ssl|Client1]
	end.

parse_ssl_tlv(Rest, 0, Info) ->
	{ok, Info, Rest};
%% Valid TLVs.
parse_ssl_tlv(<<TLVType, TLVLen:16, TLVValue:TLVLen/binary, Rest/bits>>, Len, Info) ->
	case ssl_subtype(TLVType) of
		undefined ->
			{error, 'Invalid TLV subtype for PP2_TYPE_SSL in PROXY protocol binary header. (PP 2.2)'};
		Type ->
			parse_ssl_tlv(Rest, Len - TLVLen - 3, Info#{Type => TLVValue})
	end;
%% Invalid TLV length.
parse_ssl_tlv(_, _, _) ->
	{error, 'Invalid TLV length in the PROXY protocol binary header. (PP 2.2)'}.

ssl_subtype(16#21) -> version;
ssl_subtype(16#22) -> cn;
ssl_subtype(16#23) -> cipher;
ssl_subtype(16#24) -> sig_alg;
ssl_subtype(16#25) -> key_alg;
ssl_subtype(_) -> undefined.

%% Building.

-spec header(proxy_info()) -> iodata().
header(ProxyInfo) ->
	header(ProxyInfo, #{}).

-spec header(proxy_info(), build_opts()) -> iodata().
header(#{version := 2, command := local}, _) ->
	<<"\r\n\r\n\0\r\nQUIT\n", 2:4, 0:28>>;
header(#{version := 2, command := proxy,
		transport_family := Family,
		transport_protocol := Protocol}, _)
		when Family =:= undefined; Protocol =:= undefined ->
	<<"\r\n\r\n\0\r\nQUIT\n", 2:4, 1:4, 0:24>>;
header(ProxyInfo=#{version := 2, command := proxy,
		transport_family := Family,
		transport_protocol := Protocol}, Opts) ->
	Addresses = addresses(ProxyInfo),
	TLVs = tlvs(ProxyInfo, Opts),
	ExtraLen = case Opts of
		#{checksum := crc32c} -> 7;
		_ -> 0
	end,
	Len = iolist_size(Addresses) + iolist_size(TLVs) + ExtraLen,
	Header = [
		<<"\r\n\r\n\0\r\nQUIT\n", 2:4, 1:4>>,
		<<(family(Family)):4, (protocol(Protocol)):4>>,
		<<Len:16>>,
		Addresses,
		TLVs
	],
	case Opts of
		#{checksum := crc32c} ->
			CRC32C = ranch_crc32c:crc32c([Header, <<16#3, 4:16, 0:32>>]),
			[Header, <<16#3, 4:16, CRC32C:32>>];
		_ ->
			Header
	end;
header(#{version := 1, command := proxy,
		transport_family := undefined,
		transport_protocol := undefined}, _) ->
	<<"PROXY UNKNOWN\r\n">>;
header(#{version := 1, command := proxy,
		transport_family := Family0,
		transport_protocol := stream,
		src_address := SrcAddress, src_port := SrcPort,
		dest_address := DestAddress, dest_port := DestPort}, _)
		when SrcPort > 0, SrcPort =< 65535, DestPort > 0, DestPort =< 65535 ->
	[
		<<"PROXY ">>,
		case Family0 of
			ipv4 when tuple_size(SrcAddress) =:= 4, tuple_size(DestAddress) =:= 4 ->
				[<<"TCP4 ">>, inet:ntoa(SrcAddress), $\s, inet:ntoa(DestAddress)];
			ipv6 when tuple_size(SrcAddress) =:= 8, tuple_size(DestAddress) =:= 8 ->
				[<<"TCP6 ">>, inet:ntoa(SrcAddress), $\s, inet:ntoa(DestAddress)]
		end,
		$\s,
		integer_to_binary(SrcPort),
		$\s,
		integer_to_binary(DestPort),
		$\r, $\n
	].

family(ipv4) -> 1;
family(ipv6) -> 2;
family(unix) -> 3.

protocol(stream) -> 1;
protocol(dgram) -> 2.

addresses(#{transport_family := ipv4,
		src_address := {S1, S2, S3, S4}, src_port := SrcPort,
		dest_address := {D1, D2, D3, D4}, dest_port := DestPort})
		when SrcPort > 0, SrcPort =< 65535, DestPort > 0, DestPort =< 65535 ->
	<<S1, S2, S3, S4, D1, D2, D3, D4, SrcPort:16, DestPort:16>>;
addresses(#{transport_family := ipv6,
		src_address := {S1, S2, S3, S4, S5, S6, S7, S8}, src_port := SrcPort,
		dest_address := {D1, D2, D3, D4, D5, D6, D7, D8}, dest_port := DestPort})
		when SrcPort > 0, SrcPort =< 65535, DestPort > 0, DestPort =< 65535 ->
	<<
		S1:16, S2:16, S3:16, S4:16, S5:16, S6:16, S7:16, S8:16,
		D1:16, D2:16, D3:16, D4:16, D5:16, D6:16, D7:16, D8:16,
		SrcPort:16, DestPort:16
	>>;
addresses(#{transport_family := unix,
		src_address := SrcAddress, dest_address := DestAddress})
		when byte_size(SrcAddress) =< 108, byte_size(DestAddress) =< 108 ->
	SrcPadding = 8 * (108 - byte_size(SrcAddress)),
	DestPadding = 8 * (108 - byte_size(DestAddress)),
	<<
		SrcAddress/binary, 0:SrcPadding,
		DestAddress/binary, 0:DestPadding
	>>.

tlvs(ProxyInfo, Opts) ->
	[
		binary_tlv(ProxyInfo, alpn, 16#1),
		binary_tlv(ProxyInfo, authority, 16#2),
		ssl_tlv(ProxyInfo),
		binary_tlv(ProxyInfo, netns, 16#30),
		raw_tlvs(ProxyInfo),
		noop_tlv(Opts)
	].

binary_tlv(Info, Key, Type) ->
	case Info of
		#{Key := Bin} ->
			Len = byte_size(Bin),
			<<Type, Len:16, Bin/binary>>;
		_ ->
			<<>>
	end.

noop_tlv(#{padding := Len0}) when Len0 >= 3 ->
	Len = Len0 - 3,
	<<16#4, Len:16, 0:Len/unit:8>>;
noop_tlv(_) ->
	<<>>.

ssl_tlv(#{ssl := Info=#{client := Client0, verified := Verify0}}) ->
	Client = client(Client0, 0),
	Verify = if
		Verify0 -> 0;
		not Verify0 -> 1
	end,
	TLVs = [
		binary_tlv(Info, version, 16#21),
		binary_tlv(Info, cn, 16#22),
		binary_tlv(Info, cipher, 16#23),
		binary_tlv(Info, sig_alg, 16#24),
		binary_tlv(Info, key_alg, 16#25)
	],
	Len = iolist_size(TLVs) + 5,
	[<<16#20, Len:16, Client, Verify:32>>, TLVs];
ssl_tlv(_) ->
	<<>>.

client([], Client) -> Client;
client([ssl|Tail], Client) -> client(Tail, Client bor 16#1);
client([cert_conn|Tail], Client) -> client(Tail, Client bor 16#2);
client([cert_sess|Tail], Client) -> client(Tail, Client bor 16#4).

raw_tlvs(Info) ->
	[begin
		Len = byte_size(Bin),
		<<Type, Len:16, Bin/binary>>
	end || {Type, Bin} <- maps:get(raw_tlvs, Info, [])].

-ifdef(TEST).
v1_test() ->
	Test1 = #{
		version => 1,
		command => proxy,
		transport_family => undefined,
		transport_protocol => undefined
	},
	{ok, Test1, <<>>} = parse(iolist_to_binary(header(Test1))),
	Test2 = #{
		version => 1,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 1234,
		dest_address => {10, 11, 12, 13},
		dest_port => 23456
	},
	{ok, Test2, <<>>} = parse(iolist_to_binary(header(Test2))),
	Test3 = #{
		version => 1,
		command => proxy,
		transport_family => ipv6,
		transport_protocol => stream,
		src_address => {1, 2, 3, 4, 5, 6, 7, 8},
		src_port => 1234,
		dest_address => {65535, 55555, 2222, 333, 1, 9999, 777, 8},
		dest_port => 23456
	},
	{ok, Test3, <<>>} = parse(iolist_to_binary(header(Test3))),
	ok.

v2_test() ->
	Test0 = #{
		version => 2,
		command => local
	},
	{ok, Test0, <<>>} = parse(iolist_to_binary(header(Test0))),
	Test1 = #{
		version => 2,
		command => proxy,
		transport_family => undefined,
		transport_protocol => undefined
	},
	{ok, Test1, <<>>} = parse(iolist_to_binary(header(Test1))),
	Test2 = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 1234,
		dest_address => {10, 11, 12, 13},
		dest_port => 23456
	},
	{ok, Test2, <<>>} = parse(iolist_to_binary(header(Test2))),
	Test3 = #{
		version => 2,
		command => proxy,
		transport_family => ipv6,
		transport_protocol => stream,
		src_address => {1, 2, 3, 4, 5, 6, 7, 8},
		src_port => 1234,
		dest_address => {65535, 55555, 2222, 333, 1, 9999, 777, 8},
		dest_port => 23456
	},
	{ok, Test3, <<>>} = parse(iolist_to_binary(header(Test3))),
	Test4 = #{
		version => 2,
		command => proxy,
		transport_family => unix,
		transport_protocol => dgram,
		src_address => <<"/run/source.sock">>,
		dest_address => <<"/run/destination.sock">>
	},
	{ok, Test4, <<>>} = parse(iolist_to_binary(header(Test4))),
	ok.

v2_tlvs_test() ->
	Common = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 1234,
		dest_address => {10, 11, 12, 13},
		dest_port => 23456
	},
	Test1 = Common#{alpn => <<"h2">>},
	{ok, Test1, <<>>} = parse(iolist_to_binary(header(Test1))),
	Test2 = Common#{authority => <<"internal.example.org">>},
	{ok, Test2, <<>>} = parse(iolist_to_binary(header(Test2))),
	Test3 = Common#{netns => <<"/var/run/netns/example">>},
	{ok, Test3, <<>>} = parse(iolist_to_binary(header(Test3))),
	Test4 = Common#{ssl => #{
		client => [ssl, cert_conn, cert_sess],
		verified => true,
		version => <<"TLSv1.3">>, %% Note that I'm not sure this example value is correct.
		cipher => <<"ECDHE-RSA-AES128-GCM-SHA256">>,
		sig_alg => <<"SHA256">>,
		key_alg => <<"RSA2048">>,
		cn => <<"example.com">>
	}},
	{ok, Test4, <<>>} = parse(iolist_to_binary(header(Test4))),
	%% Note that the raw_tlvs order is not relevant and therefore
	%% the parser does not reverse the list it builds.
	Test5In = Common#{raw_tlvs => RawTLVs=[
		%% The only custom TLV I am aware of is defined at:
		%% https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html#proxy-protocol
		{16#ea, <<16#1, "instance-id">>},
		%% This TLV is entirely fictional.
		{16#ff, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 0>>}
	]},
	Test5Out = Test5In#{raw_tlvs => lists:reverse(RawTLVs)},
	{ok, Test5Out, <<>>} = parse(iolist_to_binary(header(Test5In))),
	ok.

v2_checksum_test() ->
	Test = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 1234,
		dest_address => {10, 11, 12, 13},
		dest_port => 23456
	},
	{ok, Test, <<>>} = parse(iolist_to_binary(header(Test, #{checksum => crc32c}))),
	ok.

v2_padding_test() ->
	Test = #{
		version => 2,
		command => proxy,
		transport_family => ipv4,
		transport_protocol => stream,
		src_address => {127, 0, 0, 1},
		src_port => 1234,
		dest_address => {10, 11, 12, 13},
		dest_port => 23456
	},
	{ok, Test, <<>>} = parse(iolist_to_binary(header(Test, #{padding => 123}))),
	ok.
-endif.
