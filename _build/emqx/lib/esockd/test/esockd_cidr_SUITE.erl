%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(esockd_cidr_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> esockd_ct:all(?MODULE).

%%--------------------------------------------------------------------
%% CIDR Test Cases
%%--------------------------------------------------------------------

t_parse_ipv4(_) ->
	?assertEqual({{192,168,0,0}, {192,168,0,0}, 32}, esockd_cidr:parse("192.168.0.0")),
	?assertEqual({{1,2,3,4}, {1,2,3,4}, 32}, esockd_cidr:parse("1.2.3.4")),
	?assertEqual({{0,0,0,0}, {255,255,255,255}, 0}, esockd_cidr:parse("192.168.0.0/0", true)),
	?assertEqual({{192,0,0,0}, {192,255,255,255}, 8}, esockd_cidr:parse("192.168.0.0/8", true)),
	?assertEqual({{192,168,0,0}, {192,169,255,255}, 15}, esockd_cidr:parse("192.168.0.0/15", true)),
	?assertEqual({{192,168,0,0}, {192,168,255,255}, 16}, esockd_cidr:parse("192.168.0.0/16")),
	?assertEqual({{192,168,0,0}, {192,168,127,255}, 17}, esockd_cidr:parse("192.168.0.0/17")),
	?assertEqual({{192,168,0,0}, {192,168,63,255}, 18}, esockd_cidr:parse("192.168.0.0/18")),
	?assertEqual({{192,168,0,0}, {192,168,31,255}, 19}, esockd_cidr:parse("192.168.0.0/19")),
	?assertEqual({{192,168,0,0}, {192,168,15,255}, 20}, esockd_cidr:parse("192.168.0.0/20")),
	?assertEqual({{192,168,0,0}, {192,168,7,255}, 21}, esockd_cidr:parse("192.168.0.0/21")),
	?assertEqual({{192,168,0,0}, {192,168,3,255}, 22}, esockd_cidr:parse("192.168.0.0/22")),
	?assertEqual({{192,168,0,0}, {192,168,1,255}, 23}, esockd_cidr:parse("192.168.0.0/23")),
	?assertEqual({{192,168,0,0}, {192,168,0,255}, 24}, esockd_cidr:parse("192.168.0.0/24")),
	?assertEqual({{192,168,0,0}, {192,168,0,1}, 31}, esockd_cidr:parse("192.168.0.0/31")),
	?assertEqual({{192,168,0,0}, {192,168,0,0}, 32}, esockd_cidr:parse("192.168.0.0/32")).

t_parse_ipv6(_) ->
	?assertEqual({{0, 0, 0, 0, 0, 0, 0, 0}, {65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535}, 0},
                esockd_cidr:parse("2001:abcd::/0", true)),
	?assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0}, {8193, 43981, 65535, 65535, 65535, 65535, 65535, 65535}, 32},
                esockd_cidr:parse("2001:abcd::/32")),
	?assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0}, {8193, 43981, 32767, 65535, 65535, 65535, 65535, 65535}, 33},
                esockd_cidr:parse("2001:abcd::/33")),
	?assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0}, {8193, 43981, 16383, 65535, 65535, 65535, 65535, 65535}, 34},
                 esockd_cidr:parse("2001:abcd::/34")),
	?assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0}, {8193, 43981, 8191, 65535, 65535, 65535, 65535, 65535}, 35},
                esockd_cidr:parse("2001:abcd::/35")),
	?assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0}, {8193, 43981, 4095, 65535, 65535, 65535, 65535, 65535}, 36},
                esockd_cidr:parse("2001:abcd::/36")),
	?assertEqual({{8193, 43981, 0, 0, 0, 0, 0, 0}, {8193, 43981, 0, 0, 0, 0, 0, 0}, 128},
                esockd_cidr:parse("2001:abcd::/128")).

t_ipv4_address_count(_) ->
	?assertEqual(4294967296, esockd_cidr:count(esockd_cidr:parse("192.168.0.0/0", true))),
	?assertEqual(65536, esockd_cidr:count(esockd_cidr:parse("192.168.0.0/16", true))),
	?assertEqual(32768, esockd_cidr:count(esockd_cidr:parse("192.168.0.0/17", true))),
	?assertEqual(256, esockd_cidr:count(esockd_cidr:parse("192.168.0.0/24", true))),
	?assertEqual(1, esockd_cidr:count(esockd_cidr:parse("192.168.0.0/32", true))).

t_ipv6_address_count(_) ->
    ?assert(esockd_cidr:count(esockd_cidr:parse("2001::abcd/0", true)) == math:pow(2, 128)),
	?assert(esockd_cidr:count(esockd_cidr:parse("2001::abcd/64", true)) == math:pow(2, 64)),
	?assert(esockd_cidr:count(esockd_cidr:parse("2001::abcd/128")) == 1).

t_to_string(_) ->
    ?assertEqual("192.168.0.0/16", esockd_cidr:to_string({{192,168,0,0}, {192,168,255,255}, 16})),
	?assertEqual("2001:abcd::/32", esockd_cidr:to_string({{8193, 43981, 0, 0, 0, 0, 0, 0},
                                                          {8193, 43981, 65535, 65535, 65535, 65535, 65535, 65535}, 32})).

t_ipv4_match(_) ->
    CIDR = esockd_cidr:parse("192.168.0.0/16"),
	?assert(esockd_cidr:match({192,168,0,0}, CIDR)),
    ?assert(esockd_cidr:match({192,168,0,1}, CIDR)),
    ?assert(esockd_cidr:match({192,168,1,0}, CIDR)),
    ?assert(esockd_cidr:match({192,168,0,255}, CIDR)),
    ?assert(esockd_cidr:match({192,168,255,0}, CIDR)),
    ?assert(esockd_cidr:match({192,168,255,255}, CIDR)),
    ?assertNot(esockd_cidr:match({192,168,255,256}, CIDR)),
    ?assertNot(esockd_cidr:match({192,169,0,0}, CIDR)),
    ?assertNot(esockd_cidr:match({192,167,255,255}, CIDR)).

t_ipv6_match(_) ->
	CIDR = {{8193, 43981, 0, 0, 0, 0, 0, 0},
            {8193, 43981, 8191, 65535, 65535, 65535, 65535, 65535}, 35},
    ?assert(esockd_cidr:match({8193, 43981, 0, 0, 0, 0, 0, 0}, CIDR)),
    ?assert(esockd_cidr:match({8193, 43981, 0, 0, 0, 0, 0, 1}, CIDR)),
    ?assert(esockd_cidr:match({8193, 43981, 8191, 65535, 65535, 65535, 65535, 65534}, CIDR)),
    ?assert(esockd_cidr:match({8193, 43981, 8191, 65535, 65535, 65535, 65535, 65535}, CIDR)),
    ?assertNot(esockd_cidr:match({8193, 43981, 8192, 65535, 65535, 65535, 65535, 65535}, CIDR)),
    ?assertNot(esockd_cidr:match({65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535}, CIDR)).

t_is_ipv4(_) ->
    ?assert(esockd_cidr:is_ipv4({127,0,0,1})),
    ?assertNot(esockd_cidr:is_ipv4({8193, 43981, 0, 0, 0, 0, 0, 0})).

t_is_ipv6(_) ->
    ?assertNot(esockd_cidr:is_ipv6({127,0,0,1})),
    ?assert(esockd_cidr:is_ipv6({8193, 43981, 0, 0, 0, 0, 0, 0})).

