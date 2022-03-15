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

-module(dgiot_httpc).
-include_lib("dgiot/include/logger.hrl").
-author("johnliu").
-export([url_join/1, qs/1, urldecode/1, urlencode/1]).
-export([upload/2, download/2, fileUpload/3]).

-define(CRLF, "\r\n").
upload(Url, Path) ->
    inets:start(),
    case file:read_file(Path) of
        {ok, Stream} ->
            FileName = dgiot_utils:to_binary(filename:basename(Path)),
            Boundary = <<"-----------------------acebdf135724681">>,
            Header = <<"--", Boundary/binary, "\r\n", "Content-Disposition: form-data;name=\"">>,
            Data5 = <<"filename\"", "\r\n", "\r\n", FileName/binary, "\r\n">>,
            ParamBody5 = <<Header/binary, Data5/binary>>,
            Tail = <<"--", Boundary/binary, "--", "\r\n", "\r\n">>,
            FileBody = <<Header/binary, "file\"; filename=\"", FileName/binary, "\"", "\r\n",
                "Content-Type: application/octet-stream", "\r\n", "\r\n", Stream/binary, "\r\n", Tail/binary>>,
            ParamBody = <<ParamBody5/binary>>,
            Body = <<ParamBody/binary, FileBody/binary>>,
            Size = byte_size(Body),
            ContentType = <<"multipart/form-data; boundary=", Boundary/binary>>,
            case httpc:request(post, {dgiot_utils:to_list(Url), [{"Content-Length", integer_to_list(Size)}], binary_to_list(ContentType), Body}, [], []) of
                {ok, {{"HTTP/1.1", 200, "OK"}, _, Json}} ->
                    case jsx:decode(dgiot_utils:to_binary(Json), [{labels, binary}, return_maps]) of
                        #{<<"md5">> := _Md5} = Data ->
                            {ok, Data};
                        Error1 -> Error1
                    end;
                Error -> Error
            end;
        {error, Reason} ->
            ?LOG(info, "Reason ~p ", [Reason]),
            {error, Reason}
    end.

fileUpload(Url, Path, Producttempid) ->
    case file:read_file(Path) of
        {ok, Stream} ->
            FileName = dgiot_utils:to_binary(filename:basename(Path)),
            Boundary = <<"----WebKitFormBoundaryCEh4tVfSNDV7cY9B">>,
            Header = <<"--", Boundary/binary, "\r\n", "Content-Disposition: form-data;name=\"file\"; filename=\"", FileName/binary, "\"", "\r\n">>,

            FileBody = <<"Content-Type: application/vnd.openxmlformats-officedocument.wordprocessingml.document", "\r\n", "\r\n",
                Stream/binary, "\r\n">>,

            FilenameBody = <<"--", Boundary/binary, "\r\n", "Content-Disposition: form-data; name=\"filename\"", "\r\n", "\r\n", Producttempid/binary, "\r\n">>,

            PathBody = <<"--", Boundary/binary, "\r\n", "Content-Disposition: form-data; name=\"path\"", "\r\n", "\r\n", "dgiot_file/product/topo/", Producttempid/binary, "\r\n">>,

            Tail = <<"--", Boundary/binary, "--">>,

            Body = <<Header/binary, FileBody/binary, FilenameBody/binary, PathBody/binary, Tail/binary>>,

            Size = byte_size(Body),
            ContentType = <<"multipart/form-data; boundary=", Boundary/binary>>,

            case httpc:request(post, {dgiot_utils:to_list(Url), [{"Content-Length", integer_to_list(Size)}], binary_to_list(ContentType), Body}, [], []) of
                {ok, {{"HTTP/1.1", 200, "OK"}, _, Json}} ->
                    case jsx:decode(dgiot_utils:to_binary(Json), [{labels, binary}, return_maps]) of
                        #{<<"msg">> := <<"SUCCESS">>, <<"code">> := 0} = Data ->
                            {ok, Data};
                        Error1 ->
                            Error1
                    end;
                Error ->
                    Error
            end;
        {error, Reason} ->
            ?LOG(info, "Reason ~p ", [Reason]),
            {error, Reason}
    end.

download(Url, Path) ->
    httpc:request(get, {Url, []}, [], [{body_format, binary}, {stream, Path}]).


url_join(List) ->
    url_join(List, "").
url_join([], URL) -> URL;
url_join([Path | Other], URL) when is_binary(Path) ->
    url_join([binary_to_list(Path) | Other], URL);
url_join([Path | Other], "") -> url_join(Other, Path);
url_join([Path | Other], URL) when is_list(Path) ->
    Url1 =
        case lists:reverse(URL) of
            "/" ++ _ -> URL;
            _ -> lists:concat([URL, "/"])
        end,
    Url2 =
        case Path of
            "/" ++ Rest -> lists:concat([Url1, Rest]);
            Rest -> lists:concat([Url1, Rest])
        end,
    url_join(Other, Url2).



qs([]) ->
    <<>>;
qs(L) ->
    qs(L, <<>>).

qs([], Acc) ->
    <<$&, Qs/bits>> = Acc,
    Qs;
qs([{Name, true} | Tail], Acc) ->
    Acc2 = urlencode(Name, <<Acc/bits, $&>>),
    qs(Tail, Acc2);
qs([{Name, Value} | Tail], Acc) ->
    Acc2 = urlencode(Name, <<Acc/bits, $&>>),
    Acc3 = urlencode(Value, <<Acc2/bits, $=>>),
    qs(Tail, Acc3).


-spec urldecode(B) -> B when B :: binary().
urldecode(B) ->
    urldecode(B, <<>>).

urldecode(<<$%, H, L, Rest/bits>>, Acc) ->
    C = (unhex(H) bsl 4 bor unhex(L)),
    urldecode(Rest, <<Acc/bits, C>>);
urldecode(<<$+, Rest/bits>>, Acc) ->
    urldecode(Rest, <<Acc/bits, " ">>);
urldecode(<<C, Rest/bits>>, Acc) when C =/= $% ->
    urldecode(Rest, <<Acc/bits, C>>);
urldecode(<<>>, Acc) ->
    Acc.

unhex($0) -> 0;
unhex($1) -> 1;
unhex($2) -> 2;
unhex($3) -> 3;
unhex($4) -> 4;
unhex($5) -> 5;
unhex($6) -> 6;
unhex($7) -> 7;
unhex($8) -> 8;
unhex($9) -> 9;
unhex($A) -> 10;
unhex($B) -> 11;
unhex($C) -> 12;
unhex($D) -> 13;
unhex($E) -> 14;
unhex($F) -> 15;
unhex($a) -> 10;
unhex($b) -> 11;
unhex($c) -> 12;
unhex($d) -> 13;
unhex($e) -> 14;
unhex($f) -> 15.



-spec urlencode(B) -> B when B :: binary().
urlencode(B) ->
    urlencode(B, <<>>).

urlencode(<<$\s, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $+>>);
urlencode(<<$-, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $->>);
urlencode(<<$., Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $.>>);
urlencode(<<$0, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $0>>);
urlencode(<<$1, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $1>>);
urlencode(<<$2, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $2>>);
urlencode(<<$3, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $3>>);
urlencode(<<$4, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $4>>);
urlencode(<<$5, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $5>>);
urlencode(<<$6, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $6>>);
urlencode(<<$7, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $7>>);
urlencode(<<$8, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $8>>);
urlencode(<<$9, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $9>>);
urlencode(<<$A, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $A>>);
urlencode(<<$B, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $B>>);
urlencode(<<$C, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $C>>);
urlencode(<<$D, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $D>>);
urlencode(<<$E, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $E>>);
urlencode(<<$F, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $F>>);
urlencode(<<$G, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $G>>);
urlencode(<<$H, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $H>>);
urlencode(<<$I, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $I>>);
urlencode(<<$J, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $J>>);
urlencode(<<$K, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $K>>);
urlencode(<<$L, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $L>>);
urlencode(<<$M, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $M>>);
urlencode(<<$N, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $N>>);
urlencode(<<$O, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $O>>);
urlencode(<<$P, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $P>>);
urlencode(<<$Q, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $Q>>);
urlencode(<<$R, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $R>>);
urlencode(<<$S, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $S>>);
urlencode(<<$T, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $T>>);
urlencode(<<$U, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $U>>);
urlencode(<<$V, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $V>>);
urlencode(<<$W, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $W>>);
urlencode(<<$X, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $X>>);
urlencode(<<$Y, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $Y>>);
urlencode(<<$Z, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $Z>>);
urlencode(<<$_, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $_>>);
urlencode(<<$a, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $a>>);
urlencode(<<$b, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $b>>);
urlencode(<<$c, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $c>>);
urlencode(<<$d, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $d>>);
urlencode(<<$e, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $e>>);
urlencode(<<$f, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $f>>);
urlencode(<<$g, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $g>>);
urlencode(<<$h, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $h>>);
urlencode(<<$i, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $i>>);
urlencode(<<$j, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $j>>);
urlencode(<<$k, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $k>>);
urlencode(<<$l, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $l>>);
urlencode(<<$m, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $m>>);
urlencode(<<$n, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $n>>);
urlencode(<<$o, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $o>>);
urlencode(<<$p, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $p>>);
urlencode(<<$q, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $q>>);
urlencode(<<$r, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $r>>);
urlencode(<<$s, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $s>>);
urlencode(<<$t, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $t>>);
urlencode(<<$u, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $u>>);
urlencode(<<$v, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $v>>);
urlencode(<<$w, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $w>>);
urlencode(<<$x, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $x>>);
urlencode(<<$y, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $y>>);
urlencode(<<$z, Rest/bits>>, Acc) -> urlencode(Rest, <<Acc/bits, $z>>);
urlencode(<<C, Rest/bits>>, Acc) ->
    H = hex(C bsr 4),
    L = hex(C band 16#0f),
    urlencode(Rest, <<Acc/bits, $%, H, L>>);
urlencode(<<>>, Acc) ->
    Acc.

hex(0) -> $0;
hex(1) -> $1;
hex(2) -> $2;
hex(3) -> $3;
hex(4) -> $4;
hex(5) -> $5;
hex(6) -> $6;
hex(7) -> $7;
hex(8) -> $8;
hex(9) -> $9;
hex(10) -> $A;
hex(11) -> $B;
hex(12) -> $C;
hex(13) -> $D;
hex(14) -> $E;
hex(15) -> $F.


