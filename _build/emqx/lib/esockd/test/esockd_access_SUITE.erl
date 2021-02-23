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

-module(esockd_access_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() -> esockd_ct:all(?MODULE).

%%--------------------------------------------------------------------
%% Access
%%--------------------------------------------------------------------

t_match(_) ->
    Rules = [esockd_access:compile({deny,  "192.168.1.1"}),
             esockd_access:compile({allow, "192.168.1.0/24"}),
             esockd_access:compile({deny,  all})],
    ?assertEqual({matched, deny}, esockd_access:match({192,168,1,1}, Rules)),
    ?assertEqual({matched, allow}, esockd_access:match({192,168,1,4}, Rules)),
    ?assertEqual({matched, allow}, esockd_access:match({192,168,1,60}, Rules)),
    ?assertEqual({matched, deny}, esockd_access:match({10,10,10,10}, Rules)).

t_nomatch(_) ->
    Rules = [esockd_access:compile({deny,  "192.168.1.1"}),
             esockd_access:compile({allow, "192.168.1.0/24"})
            ],
    ?assertEqual(nomatch, esockd_access:match({10,10,10,10}, Rules)).

t_match_localhost(_) ->
    Rules = [esockd_access:compile({allow, "127.0.0.1"}),
             esockd_access:compile({deny, all})],
    ?assertEqual({matched, allow}, esockd_access:match({127,0,0,1}, Rules)),
    ?assertEqual({matched, deny}, esockd_access:match({192,168,0,1}, Rules)).

t_match_allow(_) ->
    Rules = [esockd_access:compile({deny, "10.10.0.0/16"}),
             esockd_access:compile({allow, all})],
    ?assertEqual({matched, deny}, esockd_access:match({10,10,0,10}, Rules)),
    ?assertEqual({matched, allow}, esockd_access:match({127,0,0,1}, Rules)),
    ?assertEqual({matched, allow}, esockd_access:match({192,168,0,1}, Rules)).

t_match_ipv6(_) ->
    Rules = [esockd_access:compile({deny, "2001:abcd::/64"}),
             esockd_access:compile({allow, all})],
    {ok, Addr1} = inet:parse_address("2001:abcd::10"),
    {ok, Addr2} = inet:parse_address("2001::10"),
    ?assertEqual({matched, deny}, esockd_access:match(Addr1, Rules)),
    ?assertEqual({matched, allow}, esockd_access:match(Addr2, Rules)).

