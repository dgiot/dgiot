%%--------------------------------------------------------------------
%% Copyright (c) 2021 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(dgiot_network).

-export([
    get_hostname/0
    , get_ip/1
    , get_port/1
    , get_natip/0
    , get_wlanip/0
    , get_computerconfig/0
    , get_arp/0
    , get_macs/0
    , get_ipbymac/1
    , get_macbyip/1
    , get_ifaddrs/0
    , get_ifaddr/1
    , get_ifip/1
    , ping_all/1
    , ping_all/0
    , arping/3
    , arping/2
    , get_ipbymac/2
    , get_ipv4/1
    , get_ipv6/1
    , resolve/1
    , get_url_path/1
    , get_ports/0
    , check_port/1
]).

-import(dgiot_utils, [to_int/1, to_list/1, to_binary/1]).

get_hostname() ->
    {ok, Hostname} = inet:gethostname(),
    unicode:characters_to_binary(Hostname).

get_natip() ->
    IpList = lists:foldl(
        fun
            ({_, [{A, B, C, D}]}, Acc) ->
                Acc
                ++ [to_list(A) ++ "."]
                    ++ [to_list(B) ++ "."]
                    ++ [to_list(C) ++ "."]
                    ++ [to_list(D) ++ " "];
            (_, Acc) ->
                Acc
        end, [], get_ifaddrs()),
    to_binary(IpList).

get_wlanip() ->
    inets:start(),
    case httpc:request(get, {"http://whatismyip.akamai.com/", []}, [], []) of
        {ok, {_, _, IP}} -> to_binary(IP);
        _ -> <<"">>
    end.

get_computerconfig() ->
    case os:type() of
        {win32, _} ->
            <<"Active code page: 65001\r\nNumberOfCores  \r\r\n", CPU:2/binary, _/binary>> =
                unicode:characters_to_binary(os:cmd("chcp 65001 && wmic cpu get NumberOfCores")),
            <<"Active code page: 65001\r\nCapacity    \r\r\n", MemBin/binary>> =
                unicode:characters_to_binary(os:cmd("chcp 65001 && wmic memorychip  get Capacity")),
            List = re:split(MemBin, " "),
            Mem = lists:foldl(fun(X, Acc) ->
                M = dgiot_utils:trim_string(to_list(X)),
                case to_binary(M) of
                    <<"\n", _/binary>> -> Acc;
                    <<"\r", _/binary>> -> Acc;
                    _ -> Acc + to_int(M) div (1024 * 1024 * 1024)
                end
                              end, 0, List),
            BinMem = to_binary(Mem),
            <<CPU/binary, "C/", BinMem/binary, " G">>;
        _ ->
            <<BinCPU/binary>> =
                unicode:characters_to_binary(string:strip(os:cmd("cat /proc/cpuinfo | grep \"cpu cores\" | uniq | wc -l"), right, $\n)),
            <<BinMem/binary>> =
                unicode:characters_to_binary(string:strip(os:cmd("grep MemTotal /proc/meminfo | awk '{print $2 / 1024 / 1024}'"), right, $\n)),
            <<BinCPU/binary, "C/", BinMem/binary, " G">>
    end.

get_arp() ->
    Result =
        case os:type() of
            {unix, linux} ->
                re:run(os:cmd("arp -a"),
                    <<"([\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}).*?([\\S]{2}:[\\S]{2}:[\\S]{2}:[\\S]{2}:[\\S]{2}:[\\S]{2})">>,
                    [global, {capture, all_but_first, binary}]);
            _ ->
                re:run(os:cmd("chcp 65001 & arp -a"),
                    <<"([\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}).*?([\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2})">>,
                    [global, {capture, all_but_first, binary}])
        end,
    case Result of
        {match, Ips} ->
            lists:foldl(
                fun([Ip, Mac], Acc) ->
                    Acc#{Ip => Mac}
                end, #{}, Ips);
        _ ->
            #{}
    end.

get_ipbymac(Mac, ping) ->
    case get_ipbymac(Mac) of
        <<"">> ->
            ping_all(),
            get_ipbymac(Mac);
        Ip -> Ip
    end;
get_ipbymac(Mac, Network) ->
    case get_ipbymac(Mac) of
        Ip when size(Ip) > 6 ->
            Ip;
        _ ->
            ping_all(Network),
            get_ipbymac(Mac)
    end.

get_ipbymac(Mac) ->
    IpMacs =
        case os:type() of
            {unix, linux} ->
                re:run(os:cmd("arp -a"),
                    <<"([\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}).*?([\\S]{2}:[\\S]{2}:[\\S]{2}:[\\S]{2}:[\\S]{2}:[\\S]{2})">>,
                    [global, {capture, all_but_first, binary}]);
            _ ->
                re:run(os:cmd("chcp 65001 & arp -a"),
                    <<"([\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}).*?([\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2})">>,
                    [global, {capture, all_but_first, binary}])
        end,
    NewMac = case os:type() of
                 {unix, linux} ->
                     dgiot_utils:to_binary(re:replace(dgiot_utils:to_list(Mac), "-", ":", [global, {return, list}]));
                 _ ->
                     Mac
             end,
    case IpMacs of
        {match, Iflist} ->
            IpList =
                lists:foldl(fun(X, Acc) ->
                    case X of
                        [Ip, NewMac] ->
                            lists:umerge([Acc, [<<Ip/binary, " ">>]]);
                        _ ->
                            Acc
                    end
                            end, [], Iflist),
            case IpList of
                [] -> <<"">>;
                _ ->
                    to_binary(dgiot_utils:trim_string(IpList))
            end;
        _ -> <<"">>
    end.

get_macbyip(Ip) ->
    Ips =
        case os:type() of
            {unix, linux} ->
                re:run(dgiot_utils:to_binary(os:cmd("arp -a")),
                    <<"([\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}).*?([\\S]{2}:[\\S]{2}:[\\S]{2}:[\\S]{2}:[\\S]{2}:[\\S]{2})">>,
                    [global, {capture, all_but_first, binary}]);
            _ ->
                re:run(dgiot_utils:to_binary(os:cmd("chcp 65001 & arp -a")),
                    <<"([\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}).*?([\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2})">>,
                    [global, {capture, all_but_first, binary}])
        end,
    case Ips of
        {match, Iflist} ->
            IpList = lists:foldl(
                fun(X, Acc) ->
                    case X of
                        [Ip, Mac] ->
                            lists:umerge([Acc, [list_to_binary(string:to_upper(re:replace(Mac, "-", ":", [global, {return, list}])))]]);
                        _ -> Acc
                    end
                end, [], Iflist),
            case IpList of
                [] -> <<"">>;
                _ ->
                    to_binary(dgiot_utils:trim_string(IpList))
            end;
        _ ->
            <<"">>
    end.

get_ip({A, B, C, D}) ->
    Ip = to_list(A) ++ "." ++
        to_list(B) ++ "." ++
        to_list(C) ++ "." ++
        to_list(D),
    to_binary(Ip);

get_ip({{A, B, C, D}, _Port}) ->
    Ip = to_list(A) ++ "." ++
        to_list(B) ++ "." ++
        to_list(C) ++ "." ++
        to_list(D),
    to_binary(Ip);

get_ip(Socket) ->
    case esockd_transport:peername(Socket) of
        {ok, {{A, B, C, D}, _Port}} ->
            Ip = to_list(A) ++ "." ++
                to_list(B) ++ "." ++
                to_list(C) ++ "." ++
                to_list(D),
            to_binary(Ip);
        _ ->
            <<"">>

    end.

get_port(Socket) ->
    case esockd_transport:peername(Socket) of
        {ok, {{_A, _B, _C, _D}, Port}} ->
            Port;
        _ ->
            0
    end.

%%re:run(os:cmd("chcp 65001 & arp -a"),
%%<<"([\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}).*?([\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2})">>,
%%[global, {capture, all_but_first, binary}])
ping_all() ->
    ping(get_ifaddrs()).
ping_all(Network) ->
    Ips = re:split(dgiot_utils:to_list(Network), ";", [{return, list}]),
    ping(Ips).

ping([]) ->
    pass;
ping([{IfAddr, [{A, B, C, _D}]} | Ips]) ->
    do_ping({A, B, C, _D}, IfAddr),
    ping(Ips);
ping([Ip | Ips]) ->
    case inet:parse_address(dgiot_utils:to_list(Ip)) of
        {ok, {A, B, C, _D}} -> do_ping({A, B, C, _D});
        _ -> pass
    end,
    ping(Ips).

do_ping({A, B, C, _D}) ->
    case os:type() of
        {unix, linux} ->
            [ping_("ping -c 2 -w 2 " ++ to_list(A) ++ "." ++ to_list(B) ++ "." ++ to_list(C) ++ "." ++ dgiot_utils:to_list(N)) || N <- lists:seq(1, 254)];
        _ ->
            [ping_("ping -n 2 -w 2 " ++ to_list(A) ++ "." ++ to_list(B) ++ "." ++ to_list(C) ++ "." ++ dgiot_utils:to_list(N)) || N <- lists:seq(1, 254)]
    end.

do_ping({A, B, C, _D}, Ifaddr) ->
    case os:type() of
        {unix, linux} ->
            arping({A, B, C, _D}, Ifaddr);
        _ ->
            [ping_("ping -n 2 -w 2 " ++ to_list(A) ++ "." ++ to_list(B) ++ "." ++ to_list(C) ++ "." ++ dgiot_utils:to_list(N)) || N <- lists:seq(1, 254)]
    end.

arping({A, B, C, _D}, Ifaddr) ->
    [ping_("arping -I " ++ dgiot_utils:to_list(Ifaddr) ++ " -f -w 2 " ++ to_list(A) ++ "." ++ to_list(B) ++ "." ++ to_list(C) ++ "." ++ dgiot_utils:to_list(N)) || N <- lists:seq(1, 254)].

arping({_A, _B, _C, 255}, _Ifaddr, Mac) ->
    {Mac, 255};
arping({A, B, C, N}, Ifaddr, Mac) ->
    Result = ping_("arping -I " ++ dgiot_utils:to_list(Ifaddr) ++ " -f -w 2 " ++ to_list(A) ++ "." ++ to_list(B) ++ "." ++ to_list(C) ++ "." ++ dgiot_utils:to_list(N)),
    case re:run(Result, Mac) of
        {match, _} ->
            {Mac, N};
        _ ->
            arping({A, B, C, N + 1}, Ifaddr, Mac)
    end.

ping_(Command) ->
    timer:sleep(100),
    open_port({spawn, "sh -c 'exec " ++ dgiot_utils:to_list(Command) ++ "'"}, []),
    receive
        {_Port, {data, Data}} ->
            dgiot_mqtt:publish(self(), <<"dgiot/ping/logs">>, dgiot_utils:to_binary(Data));
        {_Port, {exit_status, Status}} ->
            Status
    end.

get_ifaddrs() ->
    case inet:getifaddrs() of
        {ok, Iflist} ->
            get_ifaddrs(Iflist, []);
        _ -> []
    end.

get_ifaddrs([], Acc) ->
    Acc;
get_ifaddrs([{IfAddr, V} | Iflist], Acc0) ->
    Acc = case lists:keyfind([up, running], 2, V) of
              false -> Acc0;
              _ ->
                  Acc0 ++ [{IfAddr, get_ipv4(V)}]
          end,
    NewAcc = case lists:keyfind([up, broadcast, running, multicast], 2, V) of
                 false -> Acc;
                 _ -> Acc ++ [{IfAddr, get_ipv4(V)}]
             end,
    get_ifaddrs(Iflist, NewAcc).

get_ifaddr(Inetsname) when is_list(Inetsname) ->
    case inet:getifaddrs() of
        {ok, Iflist} ->
            Inets = proplists:get_value(Inetsname, Iflist, []),
            Hwaddr = proplists:get_value(hwaddr, Inets, []),
            dgiot_utils:binary_to_hex(iolist_to_binary(Hwaddr));
        _ ->
            <<>>
    end;

get_ifaddr(Inetsname) ->
    get_ifaddr(dgiot_utils:to_list(Inetsname)).

get_ifip(Inetsname) when is_list(Inetsname) ->
    case inet:getifaddrs() of
        {ok, Iflist} ->
            Inets = proplists:get_value(Inetsname, Iflist, []),
            case proplists:get_value(addr, Inets, not_find) of
                not_find ->
                    <<>>;
                Ip ->
                    get_ip(Ip)
            end;
        _ ->
            <<>>
    end;

get_ifip(Inetsname) ->
    get_ifip(dgiot_utils:to_list(Inetsname)).

get_ipv4(Hostent) ->
    lists:foldl(fun({K, V}, Acc) ->
        case K of
            addr ->
                case inet:parse_ipv4_address(inet:ntoa(V)) of
                    {error, einval} -> Acc;
                    _ -> Acc ++ [V]
                end;
            _ -> Acc
        end
                end, [], Hostent).

get_ipv6(Hostent) ->
    lists:foldl(fun({K, V}, Acc) ->
        case K of
            addr -> case inet:parse_ipv6_address(inet:ntoa(V)) of
                        {error, einval} -> Acc;
                        _ -> Acc ++ [V]
                    end;
            _ -> Acc
        end
                end, [], Hostent).

resolve(Host) ->
    case inet:parse_address(dgiot_utils:to_list(Host)) of  %%判定是否为ip地址
        {ok, {IP1, IP2, IP3, IP4}} ->
            combin_ip(IP1, IP2, IP3, IP4);
        _ ->
            case inet:getaddr(dgiot_utils:to_list(Host), inet) of  %%DNS解析，通过域名解析对应一个IP值
                {ok, {IP1, IP2, IP3, IP4}} ->
                    combin_ip(IP1, IP2, IP3, IP4);
                {error, _Reason} -> Host
            end
    end.

combin_ip(IP1, IP2, IP3, IP4) ->
    dgiot_utils:to_list(IP1) ++ "." ++ dgiot_utils:to_list(IP2) ++ "." ++ dgiot_utils:to_list(IP3) ++ "." ++ dgiot_utils:to_list(IP4).

get_url_path(Url) when is_list(Url) ->
    get_url_path(to_binary(Url));

get_url_path(<<"http://", Rest/binary>>) ->
    url_path(to_list(Rest));

get_url_path(<<"https://", Rest/binary>>) ->
    url_path(to_list(Rest)).

url_path(Url) ->
%%    "192.168.0.183:5094/wordServer/20211112142832/1.jpg",
    {match, [{Start, _Len}]} = re:run(Url, <<"\/">>),
    to_binary(string:substr(Url, Start + 1, length(Url))).

get_ports() ->
    lists:foldl(fun(X, Acc) ->
        case inet:port(X) of
            {ok, Port} ->
                Acc ++ [Port];
            _ ->
                Acc
        end
                end, [], erlang:ports()).

check_port(Port) ->
    lists:any(fun(X) ->
        case inet:port(X) of
            {ok, Port} ->
                true;
            _ ->
                false
        end
              end, erlang:ports()).

get_macs() ->
    case inet:getifaddrs() of
        {ok, Iflist} ->
            Macs = lists:foldl(fun({_K, V}, [A, B, C, D, E, F] = Acc) ->
                case proplists:get_value(hwaddr, V) of
                    [A1, B1, C1, D1, E1, F1] ->
                        [A + A1, B + B1, C + C1, D + D1, E + E1, F + F1];
                    _ ->
                        Acc
                end
                               end, [0, 0, 0, 0, 0, 0], Iflist),
            dgiot_utils:to_md5(dgiot_utils:to_binary(lists:concat(Macs)));
        _ ->
            dgiot_utils:to_md5(dgiot_utils:random())
    end.
