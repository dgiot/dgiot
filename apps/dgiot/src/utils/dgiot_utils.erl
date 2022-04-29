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

-module(dgiot_utils).
-author("johnliu").
-include("dgiot.hrl").
-include_lib("dgiot/include/logger.hrl").
-export([
    send_fsm/3
    , send_msg/2
    , wait_request/2
    , get_env/1
    , binary_to_hex/1
    , hex_to_binary/1
    , to_utf8/2
    , to_md5/1
    , to_hex/1
    , to_binary/1
    , to_atom/1
    , to_int/1
    , to_list/1
    , to_bool/1
    , to_float/1
    , to_float/2
    , to_map/1
    , list_to_map/1
    , tokens/2
    , to_term/1
    , is_like/2
    , is_alive/1
    , append/2
    , append/3
    , get_val/2
    , get_val/3
    , check_value_is_null/1
    , check_val/3
    , open_dets/1
    , open_dets/2
    , bits_to_binary/1
    , binary_to_bits/1
    , get_binary_bits_n/2
    , set_binary_bits_n/3
    , binary_bits_sum/1
    , binary_bits_zero_list/1
    , binary_bits_nozero_list/1
    , binary_bits_zip/1
    , zip_bin/1
    , is_in_binary/2
    , get_parity/1
    , crc16/1
    , add_33h/1
    , sub_33h/1
    , sub_value/1
    , add_value/1
    , hash/1
    , hash/2
    , hash/3
    , hash/4
    , hash_2/1
    , hash_2/2
    , hash_2/3
    , hash_2/4
    , squotes_wrapped/1
    , round/2
    , split_list/5
    , read/3
    , read_from_csv/2
    , read_csv/3
    , rate/2
    , merge_maps/1
    , make_error_response/3
    , new_pns/1
    , new_pns/3
    , format/2
    , guid/0
    , new_counter/2
    , update_counter/1
    , parse_objectid/0
    , shuffle/1
    , split_zetag/1
    , unique_1/1
    , unique_2/1
    , string2value/1
    , get_file/3
    , get_JsonFile/2
    , post_file/2
    , random/0
    , get_hostname/0
    , get_ip/1
    , get_port/1
    , get_natip/0
    , get_wlanip/0
    , get_computerconfig/0
    , get_ipbymac/1
    , get_ifaddrs/0
    , ping_all/0
    , get_ipbymac/2
    , get_ipv4/1
    , get_ipv6/1
    , trim_string/1
    , get_url_path/1
    , get_ports/0
    , check_port/1
    , gzip/1
    , reverse/1
]).

-define(TIMEZONE, + 8).

send_fsm(Key, Msg, TimeOut) ->
    erlang:send_after(TimeOut, self(), {fsm, Key, Msg}).

send_msg(PidOrName, Msg) ->
    case is_alive(PidOrName) of
        false ->
            {error, not_alive};
        Pid ->
            case Msg of
                {call, Message} ->
                    gen_server:call(Pid, Message, 5000);
                {cast, Message} ->
                    gen_server:cast(Pid, Message);
                _ ->
                    Pid ! Msg
            end
    end.

binary_to_hex(Id) ->
    <<<<Y>> || <<X:4>> <= Id, Y <- integer_to_list(X, 16)>>.

hex_to_binary(Id) ->
    NewId = re:replace(Id, " ", "", [global, {return, binary}, unicode]),
    <<<<Z>> || <<X:8, Y:8>> <= NewId, Z <- [binary_to_integer(<<X, Y>>, 16)]>>.


to_md5(V) when is_binary(V); is_list(V) ->
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [D]) || D <- binary_to_list(erlang:md5(V))]));
to_md5(V) ->
    to_md5(to_binary(V)).

to_hex(V) ->
    binary_to_hex(to_binary(V)).

to_binary(V) when is_atom(V) -> to_binary(atom_to_list(V));
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_pid(V) -> to_binary(pid_to_list(V));
to_binary(V) when is_map(V) -> jsx:encode(V);
to_binary(V) -> to_binary(io_lib:format("~p", [V])).

to_atom(V) when is_binary(V) -> to_atom(binary_to_list(V));
to_atom(V) when is_list(V) -> list_to_atom(V);
to_atom(V) when is_atom(V) -> V;
to_atom(V) -> to_atom(io_lib:format("~p", [V])).

to_int([V]) -> to_int(V);
to_int(V) when V == null; V == <<"Undefined">>; V == undefined; V == <<>>; V == "" -> 0;
to_int(V) when is_float(V) -> round(V);
to_int(V) when is_integer(V) -> V;
to_int(V) when is_list(V) -> list_to_integer(V);
to_int(V) when is_binary(V) -> binary_to_integer(V);
to_int(true) -> 1;
to_int(false) -> 0;
to_int(_V) -> throw({error, <<"ValueError">>}).

to_list(V) when is_atom(V) -> atom_to_list(V);
to_list(V) when is_binary(V) -> binary_to_list(V);
to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_list(V) -> V;
to_list(V) -> io_lib:format("~p", [V]).


to_bool(<<"false">>) -> false;
to_bool("false") -> false;
to_bool(V) when is_integer(V) and V =< 0 -> false;
to_bool(<<"0">>) -> false;
to_bool(_V) -> true.

to_float(V, Degree) ->
    NewV =
        case binary:split(to_binary(V), <<$e>>, [global, trim]) of
            [V1, _] ->
                V1;
            [_] ->
                V
        end,
    New = io_lib:format(lists:concat(["~.", Degree, "f"]), [to_float(NewV)]),
    to_float(New).

to_float(V) when is_float(V) -> V;
to_float(V) when V == ""; V == <<>>; V == null; V == undefined -> 0.0;
to_float(V) when is_integer(V) -> V / 1;
to_float(V) when is_list(V) -> to_float(to_binary(V));
to_float(V) when is_binary(V) ->
    case catch binary_to_float(V) of
        {'EXIT', _} ->
            to_float(to_int(V));
        N ->
            N
    end.

to_utf8(Binary, Type) ->
    utf8(Binary, <<>>, <<>>, Type).
utf8(<<>>, Block, Result, Type) ->
    Code = iconverl:get_utf8(Block, Type),
    <<Result/binary, Code/binary>>;
utf8(<<I:8, Rest/binary>>, Block, Result, Type) when I < 128 andalso I > 0 ->
    Code = iconverl:get_utf8(Block, Type),
    Ascii = <<I:8>>,
    utf8(Rest, <<>>, <<Result/binary, Code/binary, Ascii/binary>>, Type);
utf8(<<I:8, Rest/binary>>, Block, Result, Type) ->
    utf8(Rest, <<Block/binary, I:8>>, Result, Type).

to_map(List) when is_list(List) ->
    list_to_map(List);

to_map(Data) when is_binary(Data) ->
    case jsx:is_json(Data) of
        true ->
            jsx:decode(Data, [{labels, binary}, return_maps]);
        _ ->
            Data
    end.

list_to_map(List) -> list_to_map(List, #{}).
list_to_map([], Map) -> Map;
list_to_map([{}], Map) -> Map;
list_to_map([{Key, Value} | Other], Map) ->
    case is_list(Value) of
        true ->
            list_to_map(Other, Map#{to_binary(Key) => list_to_map(Value, #{})});
        false ->
            list_to_map(Other, Map#{to_binary(Key) => Value})
    end;
list_to_map(Arr, _Map) ->
    Arr.


tokens(S, []) ->
    [S];
tokens(S, [P | Other]) ->
    case string:tokens(S, P) of
        [S] ->
            tokens(S, Other);
        Res ->
            Res
    end.

% 将字符串转成erlang格式
to_term(Bin) when is_binary(Bin) ->
    to_term(binary_to_list(Bin));
to_term(Str) when is_list(Str) ->
    New = case lists:nth(length(Str), Str) == $. of
              true -> Str;
              false -> Str ++ "."
          end,
    case erl_scan:string(New) of
        {ok, Scan, _} ->
            case erl_parse:parse_exprs(Scan) of
                {ok, P} ->
                    case erl_eval:exprs(P, []) of
                        {value, Value, []} -> {ok, Value};
                        Reason -> {error, Reason}
                    end;
                Reason ->
                    {error, Reason}
            end;
        Reason ->
            {error, Reason}
    end.

is_like(_, []) -> false;
is_like(Keywords, [Value | Other]) when Value == null; Value == undefined; Value == <<>> ->
    is_like(Keywords, Other);
is_like(Keywords, [Value | Other]) ->
    Re = <<".*?", Keywords/binary, ".*?">>,
    case re:run(Value, Re) of
        {match, _} -> true;
        _ -> is_like(Keywords, Other)
    end.

is_alive(Pid) when is_pid(Pid) ->
    is_process_alive(Pid) andalso Pid;
is_alive(Name) when is_binary(Name) orelse is_list(Name) ->
    is_alive(to_atom(Name));
is_alive(Name) when is_atom(Name) ->
    Pid = case whereis(Name) of
              undefined -> global:whereis_name(Name);
              Pid1 -> Pid1
          end,
    Pid =/= undefined andalso Pid.

append(New, Map) when is_map(New) andalso is_map(Map) ->
    maps:merge(Map, New);
append(New, Opts) when is_list(New) andalso is_list(Opts) ->
    Opts ++ New.

append(Key, Value, Map) when is_map(Map) ->
    Map#{Key => Value};
append(Key, Value, Opts) when is_list(Opts) ->
    [{Key, Value} | proplists:delete(Key, Opts)];

append(Key, Value, {Key1, Opts}) ->
    {Key1, append(Key, Value, Opts)};
append(Key, Value, Other) ->
    #{Key => Value, <<"data">> => Other}.


get_val(Key, MapOpt, Default) ->
    case get_val(Key, MapOpt) of
        null -> Default;
        undefined -> Default;
        Value -> Value
    end.

get_val(Key, Map) when is_map(Map) ->
    maps:get(Key, Map, undefined);
get_val(Key, Opts) when is_list(Opts) ->
    proplists:get_value(Key, Opts);
get_val(_, _) -> undefined.


check_value_is_null(Value) when Value == ""; Value == undefined; Value == <<>> ->
    null;
check_value_is_null(Value) ->
    Value.

%% 检查值，没有则抛错
check_val(Key, Params, Err) ->
    Value = get_val(Key, Params, undefined),
    case check_value_is_null(Value) of
        null -> throw({error, Err});
        Value -> Value
    end.


open_dets(Name) ->
    open_dets(Name, []).
open_dets(Name, Opts) ->
    Path = lists:concat(["data/", Name, ".dets"]),
    case filelib:ensure_dir(Path) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            case dets:open_file(Name, [{file, Path} | Opts]) of
                {ok, Name} ->
                    {ok, Path};
                {error, Why} ->
                    {error, Why}
            end
    end.

%%--------------------------------------------------------------------
%% Btis Query and Op
%%--------------------------------------------------------------------
bits_to_binary(BitList) when is_list(BitList) ->
    <<<<X:1>> || X <- BitList>>.
binary_to_bits(Binary) when is_binary(Binary) ->
    [X || <<X:1>> <= Binary].
get_binary_bits_n(Binary, N) when is_binary(Binary) ->
    Pos = N - 1,
    <<_:Pos, Value:1, _/bits>> = Binary,
    Value.
set_binary_bits_n(Binary, N, Value) when is_binary(Binary) ->
    Pos = N - 1,
    <<Head:Pos, _:1, Tail/bits>> = Binary,
    <<Head:Pos, Value:1, Tail/bits>>.
binary_bits_sum(Binary) when is_binary(Binary) ->
    lists:sum([X || <<X:1>> <= Binary]).
binary_bits_zero_list(Binary) when is_binary(Binary) ->
    List = [X || <<X:1>> <= Binary],
    {ZeroList, _} = lists:foldl(
        fun(X, {Acc, Pos}) ->
            case X of
                1 -> {Acc, Pos + 1};
                0 -> {lists:merge(Acc, [Pos + 1]), Pos + 1}
            end
        end, {[], 0}, List),
    ZeroList.

binary_bits_nozero_list(Binary) when is_binary(Binary) ->
    List = [X || <<X:1>> <= Binary],
    {NoZeroList, _} = lists:foldl(
        fun(X, {Acc, Pos}) ->
            case X of
                0 -> {Acc, Pos + 1};
                _ -> {lists:merge(Acc, [Pos + 1]), Pos + 1}
            end
        end, {[], 0}, List),
    NoZeroList.

binary_bits_zip(Binary) when is_binary(Binary) ->
    ReverBin = list_to_binary(lists:reverse(binary_to_list(Binary))),
    list_to_binary(lists:reverse(binary_to_list(zip_bin(ReverBin)))).

zip_bin(<<0:8, Binary/binary>>) ->
    zip_bin(Binary);
zip_bin(Binary) ->
    Binary.

is_in_binary(<<>>, _Binary) ->
    true;
is_in_binary(Partten, Binary) ->
    case binary:match(Binary, Partten) of
        nomatch -> false;
        _ -> true
    end.


reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) ->
    reverse(Rest, <<H/binary, Acc/binary>>).

get_parity(Data) when is_binary(Data) ->
    get_parity(binary_to_list(Data));
get_parity(Data) when is_list(Data) ->
    lists:foldl(
        fun(X, Sum) ->
            ((X rem 256) + Sum) rem 256
        end, 0, Data).

%%    CRC-16/MODBUS 算法：
%%    在CRC计算时只用8个数据位，起始位及停止位，如有奇偶校验位也包括奇偶校验位，都不参与CRC计算。
%%    CRC计算方法是：
%%    1、 加载一值为0XFFFF的16位寄存器，此寄存器为CRC寄存器。
%%    2、 把第一个8位二进制数据（即通讯信息帧的第一个字节）与16位的CRC寄存器的相异或，异或的结果仍存放于该CRC寄存器中。
%%    3、 把CRC寄存器的内容右移一位，用0填补最高位，并检测移出位是0还是1。
%%    4、 如果移出位为零，则重复第三步（再次右移一位）；如果移出位为1，CRC寄存器与0XA001进行异或。
%%    5、 重复步骤3和4，直到右移8次，这样整个8位数据全部进行了处理。
%%    6、 重复步骤2和5，进行通讯信息帧下一个字节的处理。
%%    7、 将该通讯信息帧所有字节按上述步骤计算完成后，得到的16位CRC寄存器的高、低字节进行交换
%%    8、 最后得到的CRC寄存器内容即为：CRC校验码。
crc16(Buff) -> crc16(Buff, 16#FFFF).
crc16(<<>>, Crc) ->
    <<A:8, B:8>> = <<Crc:16>>,
    <<B:8, A:8>>;
crc16(<<B:8, Other/binary>>, Crc) ->
    NewCrc =
        lists:foldl(
            fun(_, Acc) ->
                Odd = Acc band 16#0001,
                New = Acc bsr 1,
                case Odd of
                    1 ->
                        New bxor 16#A001;
                    0 ->
                        New
                end
            end, Crc bxor B, lists:seq(1, 8)),
    crc16(Other, NewCrc).

add_33h(Data) when is_binary(Data) ->
    add_33h(binary_to_list(Data));

add_33h(Data) when is_list(Data) ->
    lists:map(
        fun(Y) ->
            add_value(Y)
        end, Data).

sub_33h(Data) when is_binary(Data) ->
    sub_33h(binary_to_list(Data));

sub_33h(Data) when is_list(Data) ->
    lists:map(
        fun(Y) ->
            sub_value(Y)
        end, Data).

sub_value(Value) when Value < 16#33 ->
    16#FF + Value - 16#33 + 1;
sub_value(Value) ->
    Value - 16#33.

add_value(Value) when Value < 16#CC ->
    Value + 16#33;
add_value(Value) ->
    Value + 16#33 - 16#FF - 1.

-spec hash(any()) -> list().
hash(Value) ->
    to_list(to_hex(crypto:hash(md5, to_list(Value)))).
-spec hash(any(), any()) -> list().
hash(Value1, Value2) ->
    hash(lists:concat([to_list(Value1), to_list(Value2)])).
-spec hash(any(), any(), any()) -> list().
hash(Value1, Value2, Value3) ->
    hash(lists:concat([to_list(Value1), to_list(Value2), to_list(Value3)])).
-spec hash(any(), any(), any(), any()) -> list().
hash(Value1, Value2, Value3, Value4) ->
    hash(lists:concat([to_list(Value1), to_list(Value2), to_list(Value3), to_list(Value4)])).

hash_2(Value) ->
    to_list(Value).
hash_2(Value1, Value2) ->
    lists:concat([to_list(Value1), to_list(Value2)]).
hash_2(Value1, Value2, Value3) ->
    lists:concat([to_list(Value1), to_list(Value2), to_list(Value3)]).
hash_2(Value1, Value2, Value3, Value4) ->
    lists:concat([to_list(Value1), to_list(Value2), to_list(Value3), to_list(Value4)]).

%% single quotes wrapped binary() | list()
-spec squotes_wrapped(binary() | list()) -> list().
squotes_wrapped(Value) ->
    lists:concat(["'", to_list(Value), "'"]).

-spec round(float(), integer()) -> float().
round(Num, Len) ->
    NewLen = min(308, Len),
    N = math:pow(10, NewLen),
    round(Num * N) / N.

read(Path, Fun, Acc) ->
    case file:open(Path, [read]) of
        {ok, IoDevice} ->
            R = read_line(IoDevice, Fun, Acc),
            file:close(IoDevice),
            R;
        {error, Reason} ->
            {error, Reason}
    end.
read_line(IoDevice, Fun, Acc) ->
    case file:read_line(IoDevice) of
        {ok, Row} ->
            read_line(IoDevice, Fun, Acc ++ [Fun(Row)]);
        eof ->
            Acc;
        {error, _Reason} ->
            Acc
    end.

split_list(_Start, _End, _Flag, [], Result) ->
    Result;
split_list(Start, End, Flag, [Row | Acc], Result) ->
    case re:run(Row, Start, [{capture, first, list}]) of
        {match, [_]} ->
            split_list(Start, End, true, Acc, Result);
        _ ->
            case re:run(Row, End, [{capture, first, list}]) of
                {match, [_]} ->
                    Result;
                _ ->
                    case Flag of
                        true ->
                            split_list(Start, End, Flag, Acc, Result ++ [Row]);
                        _ ->
                            split_list(Start, End, Flag, Acc, Result)
                    end
            end
    end.

read_from_csv(Path, Fun) ->
    case file:open(Path, [read]) of
        {ok, IoDevice} ->
            R = read_csv(IoDevice, Fun, ","),
            file:close(IoDevice),
            R;
        {error, Reason} ->
            {error, Reason}
    end.

read_csv(IoDevice, Fun, Delimiter) ->
    case file:read_line(IoDevice) of
        {ok, Row} ->
            Cols = [list_to_binary(Col) || Col <- string:tokens(lists:sublist(Row, 1, length(Row) - 1), Delimiter)],
            Fun(Cols),
            read_csv(IoDevice, Fun, Delimiter);
        eof ->
            {ok, read_complete};
        {error, Reason} ->
            ?LOG(error, "~p", [Reason])
    end.

rate(_Success, 0) ->
    0.0;
rate(Success, All) ->
    min(100.0, round((to_int(Success) / to_int(All) * 100), 2)).


merge_maps(MapList) ->
    lists:foldl(fun(Map, Acc) -> maps:merge(Acc, Map) end, #{}, MapList).

make_error_response(HttpStatusCode, ErrorCode, ErrorMsg) when 400 =< HttpStatusCode andalso HttpStatusCode =< 418 ->
    erlang:throw({HttpStatusCode, ErrorCode, ErrorMsg}).

new_pns(Pns) ->
    NewPns = gb_sets:from_list(lists:map(fun(Pn) -> dgiot_utils:to_int(Pn) end, Pns)),
    new_pns(NewPns, [], 1).

new_pns(_Pns, List, 2049) ->
    dgiot_utils:bits_to_binary(lists:reverse(List));

new_pns(Pns, List, N) ->
    NewList =
        case gb_sets:is_member(N, Pns) of
            true ->
                [1 | List];
            false ->
                [0 | List]
        end,
    new_pns(Pns, NewList, N + 1).

get_env(Key) ->
    case get_env(Key, not_find) of
        not_find -> throw({error, not_find});
        Value -> Value
    end.

get_env(Key, Default) ->
    get_env(?MODULE, Key, Default).

get_env(App, Key, Default) ->
    application:get_env(App, Key, Default).

format(Format, Args) ->
    re:replace(lists:flatten(io_lib:format(Format, Args)), "\"|\n|\s+", " ", [global, {return, binary}]).

guid() ->
    <<<<Y>> || <<X:4>> <= emqx_guid:gen(), Y <- integer_to_list(X, 16)>>.

new_counter(Name, Max) ->
    dgiot_data:insert({{Name, counter}, max}, Max),
    dgiot_data:insert({Name, counter}, 0).

update_counter(Name) ->
    {ok, Max} = dgiot_data:lookup({{Name, counter}, max}),
    dgiot_data:update_counter({Name, counter}, {2, 1, Max, 1}).


parse_objectid() ->
    Alphas = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890",
    [lists:nth(round(rand:uniform() * 61 + 1), Alphas) || _ <- lists:seq(1, 10)].


shuffle(List) ->
    lists:sort(fun(_, _) -> rand:uniform() < rand:uniform() end, List).

split_zetag(Tag) when size(Tag) =:= 8 ->
    {Rt, _} = erlang:split_binary(Tag, 4),
    Rt;

split_zetag(_) ->
    throw({error, <<"bad zetag">>}).

%% @doc 通过遍历去重
-spec unique_1(List) -> Return when
    List :: list(),
    Return :: list().
unique_1(List) ->
    unique_1(List, []).

unique_1([], ResultList) -> ResultList;
unique_1([H | L], ResultList) ->
    case lists:member(H, ResultList) of
        true -> unique_1(L, ResultList);
        false -> unique_1(L, [H | ResultList])
    end.

%% @doc 利用set结构去重
-spec unique_2(List) -> Return when
    List :: list(),
    Return :: list().
unique_2(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).

string2value(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.


wait_request(Time, _) when Time =< 0 ->
    false;
wait_request(Time, Fun) ->
    Sec = min(Time, 1000),
    receive
    after Sec ->
        case Fun() of
            false ->
                wait_request(Time - Sec, Fun);
            Result ->
                Result
        end
    end.

%%
%% @description: 读取json文件并返回
%%
get_JsonFile(Mod, FileName) ->
    {file, Here} = code:is_loaded(Mod),
    Dir = filename:dirname(filename:dirname(Here)),
    Path = dgiot_httpc:url_join([Dir, "/priv/json/", dgiot_utils:to_list(FileName)]),
    case catch file:read_file(Path) of
        {Err, Reason} when Err == 'EXIT'; Err == error ->
            ?LOG(error, "read  Path,~p error,~p ~n", [Path, Reason]),
            {error, Reason};
        {ok, Bin} ->
            Bin
    end.

get_file(Root, Files, App) ->
    FileName = App ++ "_" ++ dgiot_utils:to_list(dgiot_datetime:now_secs()) ++ ".zip",
    file:make_dir(Root ++ App),
    ZipRoot = Root ++ "/" ++ App ++ "_" ++ dgiot_utils:to_list(dgiot_datetime:now_secs()),
    file:make_dir(ZipRoot),
    ZipFile = Root ++ "/" ++ App ++ "/" ++ FileName,
    lists:map(fun({FileApp, FileName1}) ->
        FileAppList = unicode:characters_to_list(FileApp),
        FileNameList = unicode:characters_to_list(FileName1),
        ZipApp = ZipRoot ++ "/" ++ FileAppList,
        file:make_dir(ZipApp),
        SrcFile = Root ++ "/" ++ FileAppList ++ "/" ++ FileNameList,
        DestFile = ZipApp ++ "/" ++ FileNameList,
        file:copy(SrcFile, DestFile)
              end, Files),
    Cmd = case os:type() of
              {win32, _} -> "chcp 65001 && dgiot_zip zippath " ++ ZipFile ++ " " ++ ZipRoot;
              _ -> "zip  " ++ ZipFile ++ " " ++ ZipRoot
          end,
    ?LOG(info, "Cmd ~p", [Cmd]),
    spawn(fun() ->
        os:cmd(Cmd)
          end),
    App ++ "/" ++ FileName.

post_file(Root, FileName) ->
    ZipFile = Root ++ "/" ++ FileName,
    ?LOG(info, "ZipFile ~p", [ZipFile]),
    case filelib:is_file(ZipFile) of
        true ->
            Cmd = case os:type() of
                      {win32, _} -> "chcp 65001 && dgiot_zip unzip " ++ ZipFile ++ " " ++ Root;
                      _ -> "unzip -O CP936 " ++ ZipFile ++ " -d " ++ Root
                  end,
            ?LOG(info, "Cmd ~p", [Cmd]),
            {ok, os:cmd(Cmd)};
        false ->
            ?LOG(info, "~p not exist", [ZipFile]),
            {error, ZipFile ++ "not exirt"}
    end.


%随机生成16位Key值
random() ->
    to_md5(io_lib:format("~p", [erlang:make_ref()])).

get_hostname() ->
    {ok, Hostname} = inet:gethostname(),
    unicode:characters_to_binary(Hostname).

get_natip() ->
    IpList = lists:foldl(fun({A, B, C, D}, Acc) ->
        Acc
        ++ [to_list(A) ++ "."]
            ++ [to_list(B) ++ "."]
            ++ [to_list(C) ++ "."]
            ++ [to_list(D) ++ " "]
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
                M = trim_string(to_list(X)),
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

get_ipbymac(Mac, ping) ->
    case get_ipbymac(Mac) of
        <<"">> ->
            ping_all(),
            get_ipbymac(Mac);
        Ip -> Ip
    end.

get_ipbymac(Mac) ->
    case re:run(os:cmd("chcp 65001 & arp -a"),
        <<"([\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}).*?([\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2})">>,
        [global, {capture, all_but_first, binary}]) of
        {match, Iflist} ->
            IpList = lists:foldl(fun(X, Acc) ->
                case X of
                    [Ip, Mac] -> lists:umerge([Acc, [<<Ip/binary, " ">>]]);
                    _ -> Acc
                end
                                 end, [], Iflist),
            case IpList of
                [] -> <<"">>;
                _ ->
                    to_binary(trim_string(IpList))
            end;
        _ -> <<"">>
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
    {ok, {{A, B, C, D}, _Port}} = esockd_transport:peername(Socket),
    Ip = to_list(A) ++ "." ++
        to_list(B) ++ "." ++
        to_list(C) ++ "." ++
        to_list(D),
    to_binary(Ip).

get_port(Socket) ->
    {ok, {{_A, _B, _C, _D}, Port}} = esockd_transport:peername(Socket),
    Port.

%%re:run(os:cmd("chcp 65001 & arp -a"),
%%<<"([\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}\\.[\\d]{1,3}).*?([\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2}-[\\S]{2})">>,
%%[global, {capture, all_but_first, binary}])

ping_all() ->
    lists:map(fun({A, B, C, _D}) ->
        Cmd = "for /l %i in (1,1,255) do ping -n 1 -w 50 "
            ++ to_list(A) ++ "."
            ++ to_list(B) ++ "."
            ++ to_list(C) ++ "."
            ++ "%i",
        os:cmd(Cmd)
              end,
        get_ifaddrs()).

get_ifaddrs() ->
    case inet:getifaddrs() of
        {ok, Iflist} ->
            lists:foldl(fun({_K, V}, Acc) ->
                NewAcc =
                    case lists:keyfind([up, running], 2, V) of
                        false -> Acc;
                        _ ->
                            Acc ++ get_ipv4(V)
                    end,
                case lists:keyfind([up, broadcast, running, multicast], 2, V) of
                    false -> NewAcc;
                    _ -> NewAcc ++ get_ipv4(V)
                end
                        end, [], Iflist);
        _ -> []
    end.

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

trim_string(Str) when is_binary(Str) ->
    trim_string(Str, binary);
trim_string(Str) when is_list(Str) ->
    trim_string(Str, list).

trim_string(Str, Ret) ->
    re:replace(Str, "^[\s\x{3000}]+|[\s\x{3000}]+$", "", [global, {return, Ret}, unicode]).


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


%% @private
%% Reproducible gzip by not setting mtime and OS
%%
%% From https://tools.ietf.org/html/rfc1952
%%
%% +---+---+---+---+---+---+---+---+---+---+
%% |ID1|ID2|CM |FLG|     MTIME     |XFL|OS | (more-->)
%% +---+---+---+---+---+---+---+---+---+---+
%%
%% +=======================+
%% |...compressed blocks...| (more-->)
%% +=======================+
%%
%% +---+---+---+---+---+---+---+---+
%% |     CRC32     |     ISIZE     |
%% +---+---+---+---+---+---+---+---+
gzip(Uncompressed) ->
    Compressed = gzip_no_header(Uncompressed),
    Header = <<31, 139, 8, 0, 0, 0, 0, 0, 0, 0>>,
    Crc = erlang:crc32(Uncompressed),
    Size = byte_size(Uncompressed),
    Trailer = <<Crc:32/little, Size:32/little>>,
    iolist_to_binary([Header, Compressed, Trailer]).

%% @private
gzip_no_header(Uncompressed) ->
    Zstream = zlib:open(),

    try
        zlib:deflateInit(Zstream, default, deflated, -15, 8, default),
        Compressed = zlib:deflate(Zstream, Uncompressed, finish),
        zlib:deflateEnd(Zstream),
        iolist_to_binary(Compressed)
    after
        zlib:close(Zstream)
    end.


