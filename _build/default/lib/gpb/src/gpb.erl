%%% Copyright (C) 2010-2013  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
%%% MA  02110-1301  USA

%% @doc This module contains a data-driven encoder/decoder plus some auxiliary
%% helper functions.
%%
%% For information on how to generate code for a set or `.proto' files,
%% see {@link gpb_compile}.
%%
%% The reason for keeping this (slower) data-driven encoder/decoder is
%% mostly to be able to cross-check with the generated encoder/decoder.
%% and also to some extent for easier inter-op testing.
%%
%% The encoder/decoder in this module works on records or tuples,
%% but the functions {@link msg_to_map/3} and {@link msg_from_map/4}
%% can convert to and from maps.
%%
%% A note about format versions of definitions: Functions in this module
%% that take definitions, such as {@link encode_msg/2} and
%% {@link decode_msg/3} expect the definitions to be on the latest format
%% version.  If you use functions in `gpb_compile' to parse proto
%% files to definitions, for example {@link gpb_compile:file/2}, then
%% make sure to include the option
%% `{proto_defs_version, gpb_defs:latest_defs_version()}'.
%% This is in contrast to the advice in the
%% [doc/dev-guide/proto-defs-versions.md] to tool writers to use a
%% hard-wired version number, but this module is part of gpb and thus
%% developed in tandem with any changes to the definitions, whereas
%% the definitions returned from `gpb_compile' can occasionally default
%% to an earlier-than-latest version for compatibility reasons.
%% @end

-module(gpb).
%-compile(export_all).
-export([decode_msg/3]).
-export([encode_msg/2]).
-export([merge_msgs/3]).
-export([verify_msg/2, check_scalar/2]).
-export([map_item_pseudo_fields/2]).
-export([map_item_pseudo_fields_for_decoding/2]).
-export([is_allowed_as_key_type/1]).
-export([is_type_packable/1]).
-export([is_msg_proto3/2, proto3_type_default/2]).
-export([proto2_type_default/2]).
-export([encode_varint/1, decode_varint/1, decode_varint/2]).
-export([encode_wiretype/1, decode_wiretype/1]).
-export([decode_packet/3]).
-export([version_as_string/0, version_as_list/0]).
-export([field_records_to_proplists/1, proplists_to_field_records/1]).
-export([field_record_to_proplist/1,   proplist_to_field_record/1]).
-export([defs_records_to_proplists/1,  proplists_to_defs_records/1]).
-export([rpc_records_to_proplists/1, rpc_record_to_proplist/1, proplists_to_rpc_records/1]).

-ifndef(NO_HAVE_MAPS).
-export([msg_to_map/3]).
-export([msg_from_map/4]).
-endif. % -ifndef(NO_HAVE_MAPS).

-include_lib("eunit/include/eunit.hrl").
-include("../include/gpb.hrl").
-include("../include/gpb_version.hrl").

-type field() :: #?gpb_field{} | #gpb_oneof{}.

-type proplist_defs() :: [proplist_def()].
-type proplist_def() :: {{msg,Name::atom()}, [proplist_field()]} |
                        term().
-type proplist_field() :: [proplist_field_item()] | [proplist_oneof_item()].
-type proplist_field_item() :: {name, atom()} |
                               {fnum, integer()} |
                               {rnum, pos_integer()} |
                               {type, gpb_field_type()} |
                               {occurrence, required | optional | repeated} |
                               {opts, [term()]}.
-type proplist_oneof_item() :: {name, atom()} |
                               {rnum, pos_integer()} |
                               {field, [proplist_field_item()]}.
-type proplist_rpc() :: [proplist_rpc_item()].
-type proplist_rpc_item() :: {name, atom()} |
                             {input, [field()]} |
                             {output, [field()]} |
                             {input_stream, boolean()} |
                             {output_stream, boolean()} |
                             {opts, [term()]}.


%% +infinity, -infinity, not a number:
%% +Inf: sign: 0    exponent: all ones, fraction: all zeros
%% -Inf: sign: 1    exponent: all ones, fraction: all zeros
%% NaN:  sign: 0|1, exponent: all ones, fraction: anything but all zero bits
%% (so one must match for +-inf first and nan lastly)
%%
%% float:  1 bit sign, 8 bits exponent,  23 bits fraction
%% double: 1 bit sign, 11 bits exponent, 52 bits fraction
%%
%% Also, byte order is little endian so all octets are reversed (but not the
%% bits within the octet), so we get the following bit patterns:
%%
%% low frac bits:16, low exp bit:1,  high frac bits:7, sign:1, high exp bits:7
%% low frac bits:48, low exp bits:4, high frac bits:4, sign:1, high exp bits:7
%%
%%                                 frac  exp     frac  sign  exp
-define(PLUS_INF_32le_BITPATTERN,  0:16, 1:1,     0:7,  0:1, 2#1111111:7).
-define(MINUS_INF_32le_BITPATTERN, 0:16, 1:1,     0:7,  1:1, 2#1111111:7).
-define(NAN_32le_BITPATTERN_match, _:16, 1:1,     _:7,  _:1, 2#1111111:7).
-define(NAN_32le_BITPATTERN_make,  0:16, 1:1,    64:7,  0:1, 2#1111111:7).

-define(PLUS_INF_64le_BITPATTERN,  0:48, 2#1111:4,0:4,  0:1,2#1111111:7).
-define(MINUS_INF_64le_BITPATTERN, 0:48, 2#1111:4,0:4,  1:1,2#1111111:7).
-define(NAN_64le_BITPATTERN_match, _:48, 2#1111:4,_:4,  _:1,2#1111111:7).
-define(NAN_64le_BITPATTERN_make,  0:48, 2#1111:4,8:4,  0:1,2#1111111:7).

%% TODO
%%
%% * Add a new_default_msg that sets default values according to
%%   type (and optionalness) as documented on the google web:
%%   strings="", booleans=false, integers=0, enums=<first value> and so on.
%%   Maybe only for required fields?
%%
%%   Message fields can also have default values specified in the .proto file.
%%
%%   Records with default values could fit nicely here.
%%
%% * Verify type-mismatches spec<-->actual-wire-contents? (optionally?)
%%
%% * Crash or silent truncation on values out of range when encoding?
%%   Example: (1 bsl 33) for an uint32? The bit-syntax silently truncates,
%%   but this has been under debate on the erlang mailing list as it
%%   was unexpected. Related: principle of least astonishment.


%% @doc Return the version as a string.
%% Valid version format is:
%% ```
%%      <n>.<m>            % e.g. 2.1, 2.1.1, etc (any number of dots and ints)
%%      <n>.<m>-<o>-<text> % e.g. 2.1-53-gb996fbe means: a commit after 2.1
%% '''
%% The format is what `git describe --always --tags' produces,
%% given that all tags are always on the format `<n>.<m>'.
-spec version_as_string() -> string().
version_as_string() ->
    S = ?gpb_version,
    case vsn_format() of
        enforce_conforming -> assert_version_format(S);
        any -> ok
    end,
    S.

%% @doc Return the version on a list format, so that it can be compared.
%% The version_as_list is better if you want to be able to compare
%% versions, for instance to see if one version is larger/later than
%% another.
%%
%% For the case of non-tagged versions, this scheme often works, but
%% is a bit of a kluge, since in erlang, numbers are the smallest of
%% types, and a string such as "gb996fbe" cannot (and generally should
%% not) be converted to a number. So for non-tagged versions, one
%% should only check whether they are or are not equal, not whether
%% one is larger/later or smaller/earlier than another.
%%
%% This will return for example:
%% ```
%%    "2.1"             -> [2,1]
%%    "2.1-53-gb996fbe" -> [2,1,0,0,53,"gb996fbe"]
%%    "2.1.1"           -> [2,1,1]
%%    "2.2"             -> [2,2]
%% '''
%% (Lists are better than tuples when doing version comparisons.  For tuples
%% this holds: `{2,2} < {2,1,1}', since tuples are first compared by
%% size then element by element for tuples of the same size. For
%% lists, it holds instead that: `[2,1,1] < [2,2]'.)
-spec version_as_list() -> [integer() | string()].
version_as_list() ->
    case vsn_format() of
        enforce_conforming ->
            version_as_list(version_as_string());
        any ->
            S = version_as_string(),
            case analyse_vsn_format(S) of
                git  -> version_as_string();
                text -> [S]
            end
    end.

vsn_format() ->
    case os:getenv("GPB_ALLOW_NON_CONFORMING_VSN_FORMAT") of
        false -> enforce_conforming;
        _     -> any
    end.

version_as_list(S) ->
    case analyse_vsn_format(S) of
        git -> v2l(S, "");
        text -> [-1,-1, -1,-1,-1, S] % outside of ordinary versions
    end.

assert_version_format(S) ->
    case analyse_vsn_format(S) of
        git  -> ok;
        text -> erlang:error({invalid_version_format,S})
    end.

-define(is_digit(C), $0 =< C, C =< $9).

analyse_vsn_format(S) ->
    case catch analyse_vsn_1(S) of
        git -> git;
        _X  -> text
    end.

analyse_vsn_1([C|T]) when ?is_digit(C) -> analyse_vsn_2(T). % must begin with 0-9

analyse_vsn_2([C|T]) when ?is_digit(C) -> analyse_vsn_2(T);
analyse_vsn_2("."++T)                  -> analyse_vsn_3(T);
analyse_vsn_2("-"++T)                  -> analyse_vsn_4(T);
analyse_vsn_2("")                      -> git.

analyse_vsn_3([C|T]) when ?is_digit(C) -> analyse_vsn_2(T). % 0-9 must follow .

analyse_vsn_4([C|T]) when ?is_digit(C) -> analyse_vsn_5(T). % 0-9 must follow -

analyse_vsn_5([C|T]) when ?is_digit(C) -> analyse_vsn_5(T);
analyse_vsn_5("-"++[_|_])              -> git. % at least one char after -

v2l([C|T], Acc) when ?is_digit(C) -> v2l(T, [C|Acc]);
v2l("."++T, Acc)                  -> [v_acc_to_int(Acc) | v2l(T, "")];
v2l("", Acc)                      -> [v_acc_to_int(Acc)];
v2l("-"++T, Acc)                  -> [v_acc_to_int(Acc), 0, 0 | v2l2(T, "")].

v2l2([C|Tl], Acc) when ?is_digit(C) -> v2l2(Tl, [C|Acc]);
v2l2("-"++T, Acc)                   -> [v_acc_to_int(Acc), T].

v_acc_to_int(Acc) ->
    list_to_integer(lists:reverse(Acc)).

%% @doc Decode a binary to a message on record format.
%%
%% If a field would occur more than once in the message,
%% it is subject to merging, as per how protobuf is expected to work.
%% See {@link merge_msgs/3} for more details.
-spec decode_msg(binary(), atom(), gpb_defs:defs()) -> tuple().
decode_msg(Bin, MsgName, MsgDefs) ->
    MsgKey = {msg,MsgName},
    Msg    = new_initial_msg(MsgKey, MsgDefs),
    MsgDef = keyfetch(MsgKey, MsgDefs),
    decode_field(Bin, MsgDef, MsgDefs, Msg).

new_initial_msg({msg,MsgName}=Key, MsgDefs) ->
    new_init_2(MsgName, Key, MsgDefs);
new_initial_msg({group,Name}=Key, MsgDefs) ->
    new_init_2(Name, Key, MsgDefs).

new_init_2(MsgName, Key, MsgDefs) ->
    MsgDef = keyfetch(Key, MsgDefs),
    lists:foldl(fun(#?gpb_field{rnum=RNum, occurrence=repeated}, Record) ->
                        setelement(RNum, Record, []);
                   (#?gpb_field{rnum=RNum, type={msg,_Name}=FMsgKey,
                                occurrence=Occurrence},
                    Record) ->
                        if Occurrence == required ->
                                SubMsg = new_initial_msg(FMsgKey, MsgDefs),
                                setelement(RNum, Record, SubMsg);
                           true ->
                                Record
                        end;
                   (#?gpb_field{type=Type, occurrence=defaulty, rnum=RNum},
                    Record) ->
                        Default = proto3_type_default(Type, MsgDefs),
                        setelement(RNum, Record, Default);
                   (#?gpb_field{}, Record) ->
                        Record;
                   (#gpb_oneof{}, Record) ->
                        Record
                end,
                erlang:make_tuple(length(MsgDef)+1, undefined, [{1,MsgName}]),
                MsgDef).

decode_field(Bin, MsgDef, MsgDefs, Msg) when byte_size(Bin) > 0 ->
    {Key, Rest} = decode_varint(Bin, 32),
    FieldNum = Key bsr 3,
    WireType = Key band 7,
    case find_field(FieldNum, MsgDef) of
        false ->
            {Msg2, Rest2} = skip_field_or_collect_unknown(
                              Rest, FieldNum, WireType, Msg, MsgDef),
            decode_field(Rest2, MsgDef, MsgDefs, Msg2);
        {#?gpb_field{type=FieldType, rnum=RNum}=FieldDef, IsOneof} ->
            case fielddef_matches_wiretype_get_info(WireType, FieldDef) of
                {yes,packed} ->
                    AccSeq = element(RNum, Msg),
                    {NewSeq, Rest2} = decode_packed(FieldType, Rest, MsgDefs,
                                                   AccSeq),
                    NewMsg = setelement(RNum, Msg, NewSeq),
                    decode_field(Rest2, MsgDef, MsgDefs, NewMsg);
                {yes,normal} ->
                    {NewValue, Rest2} = decode_type(FieldType, Rest, MsgDefs),
                    NewMsg = add_field(NewValue, FieldDef, IsOneof, MsgDefs,
                                       Msg),
                    decode_field(Rest2, MsgDef, MsgDefs, NewMsg);
                {yes,group} ->
                    {NewValue, Rest2} = decode_group(FieldDef, Rest, MsgDefs),
                    NewMsg = add_field(NewValue, FieldDef, IsOneof, MsgDefs,
                                       Msg),
                    decode_field(Rest2, MsgDef, MsgDefs, NewMsg);
                no ->
                    {Msg2, Rest2} = skip_field_or_collect_unknown(
                                      Rest, FieldNum, WireType, Msg, MsgDef),
                    decode_field(Rest2, MsgDef, MsgDefs, Msg2)
            end
    end;
decode_field(<<>>, MsgDef, _MsgDefs, Record0) ->
    %% Reverse any repeated fields, but only on the top-level, not recursively.
    RepeatedRNums = [N || #?gpb_field{rnum=N, occurrence=repeated} <- MsgDef],
    lists:foldl(fun(RNum, Record) ->
                        OldValue = element(RNum, Record),
                        ReversedField = lists:reverse(OldValue),
                        setelement(RNum, Record, ReversedField)
                end,
                Record0,
                RepeatedRNums).

find_field(N, [#?gpb_field{fnum=N}=F | _]) ->
    {F, false};
find_field(N, [#?gpb_field{} | Rest]) ->
    find_field(N, Rest);
find_field(N, [#gpb_oneof{fields=Fs} | Rest]) ->
    case lists:keyfind(N, #?gpb_field.fnum, Fs) of
        #?gpb_field{}=F -> {F, true};
        false           -> find_field(N, Rest)
    end;
find_field(_, []) ->
    false.

fielddef_matches_wiretype_get_info(WireType, #?gpb_field{type={group,_}}) ->
    if WireType == 3 -> {yes, group};
       true          -> no
    end;
fielddef_matches_wiretype_get_info(WireType, #?gpb_field{occurrence=repeated,
                                                         type=Type}) ->
    WireTypeForPacked = encode_wiretype(bytes),
    case is_type_packable(Type) of
        true when WireType == WireTypeForPacked ->
            {yes, packed};
        true ->
            wiretype_matches_normal_type(WireType, Type);
        false ->
            wiretype_matches_normal_type(WireType, Type)
    end;
fielddef_matches_wiretype_get_info(WireType, #?gpb_field{type=Type}) ->
    wiretype_matches_normal_type(WireType, Type).

wiretype_matches_normal_type(WireType, Type) ->
    case encode_wiretype(Type) of
        WireType -> {yes, normal};
        _        -> no
    end.

%% @doc Decode an integer 0..5 to a wire type
-spec decode_wiretype(non_neg_integer()) -> varint | bits32 | bits64 |
                                            group_start | group_end |
                                            length_delimited.
decode_wiretype(0) -> varint;
decode_wiretype(1) -> bits64;
decode_wiretype(2) -> length_delimited;
decode_wiretype(3) -> group_start;
decode_wiretype(4) -> group_end;
decode_wiretype(5) -> bits32.

skip_field_or_collect_unknown(Bin, FieldNum, WireType, Msg, MsgDef) ->
    case [F || F <- MsgDef, is_field_for_unknowns(F)] of
        [] ->
            {_Unknown, Rest} = skip_field(Bin, FieldNum, WireType),
            {Msg, Rest};
        [#?gpb_field{rnum=RNum}] ->
            {Unknown, Rest} = skip_field(Bin, FieldNum, WireType),
            Msg2 = append_to_element(RNum, Unknown, Msg),
            {Msg2, Rest}
    end.

skip_field(Bin, FieldNum, WireType) ->
    case decode_wiretype(WireType) of
        varint ->
            {N, Rest} = decode_varint(Bin, 64),
            {{varint, FieldNum, N}, Rest};
        bits64 ->
            <<N:64/little, Rest/binary>> = Bin,
            {{fixed64, FieldNum, N}, Rest};
        length_delimited ->
            {Len, Rest} = decode_varint(Bin, 64),
            <<Data:Len/binary, Rest2/binary>> = Rest,
            {{length_delimited, FieldNum, Data}, Rest2};
        group_start ->
            %% read_group will return the group as a binary,
            %% but internal format for unknown groups is [unknown_field()]
            {GroupData, Rest} = read_group(Bin, FieldNum),
            GroupFields = skipped_group_fields(GroupData, []),
            {{group, FieldNum, GroupFields}, Rest};
        bits32 ->
            <<N:32/little, Rest/binary>> = Bin,
            {{fixed32, FieldNum, N}, Rest}
    end.

skipped_group_fields(Bin, Acc) when Bin =/= <<>> ->
    {Key, Rest} = decode_varint(Bin, 32),
    FieldNum = Key bsr 3,
    WireType = Key band 7,
    {Skipped, Rest2} = skip_field(Rest, FieldNum, WireType),
    skipped_group_fields(Rest2, [Skipped | Acc]);
skipped_group_fields(<<>>, Acc) ->
    lists:reverse(Acc).

decode_packed(FieldType, Bin, MsgDefs, Seq0) ->
    {Len, Rest} = decode_varint(Bin, 64),
    <<Bytes:Len/binary, Rest2/binary>> = Rest,
    {decode_packed_aux(Bytes, FieldType, MsgDefs, Seq0), Rest2}.

decode_packed_aux(Bytes, FieldType, MsgDefs, Acc) when byte_size(Bytes) > 0 ->
    {NewValue, Rest} = decode_type(FieldType, Bytes, MsgDefs),
    decode_packed_aux(Rest, FieldType, MsgDefs, [NewValue | Acc]);
decode_packed_aux(<<>>, _FieldType, _MsgDefs, Acc) ->
    Acc.

decode_type(FieldType, Bin, MsgDefs) ->
    case FieldType of
        sint32 ->
            {NV, T} = decode_varint(Bin, 32),
            {decode_zigzag(NV), T};
        sint64 ->
            {NV, T} = decode_varint(Bin, 64),
            {decode_zigzag(NV), T};
        int32 ->
            {NV, T} = decode_varint(Bin, 32),
            %% Contrary to the 64 bit encoding done for int32 (and enum),
            %% decode the value as 32 bits, so we decode negatives
            %% given both as 32 bits and as 64 bits wire encodings
            %% to the same integer.
            <<N:32/signed>> = <<NV:32>>,
            {N, T};
        int64 ->
            {NV, T} = decode_varint(Bin, 64),
            <<N:64/signed>> = <<NV:64>>,
            {N, T};
        uint32 ->
            {_N, _Rest} = decode_varint(Bin, 32);
        uint64 ->
            {_N, _Rest} = decode_varint(Bin, 64);
        bool ->
            {N, Rest} = decode_varint(Bin, 64),
            {N =/= 0, Rest};
        {enum, _EnumName}=Key ->
            {N, Rest} = decode_type(int32, Bin, MsgDefs),
            {value, {Key, EnumValues}} = lists:keysearch(Key, 1, MsgDefs),
            case lists:keyfind(N, 2, EnumValues) of
                {EnumName, N, _} -> {EnumName, Rest}; % proto_defs_version 3
                {EnumName, N}    -> {EnumName, Rest}; % proto_defs_version 2
                false            -> {N, Rest}
            end;
        fixed64 ->
            <<N:64/little, Rest/binary>> = Bin,
            {N, Rest};
        sfixed64 ->
            <<N:64/little-signed, Rest/binary>> = Bin,
            {N, Rest};
        double ->
            case Bin of
                <<?PLUS_INF_64le_BITPATTERN, Rest/binary>> ->
                    {infinity, Rest};
                <<?MINUS_INF_64le_BITPATTERN, Rest/binary>> ->
                    {'-infinity', Rest};
                <<?NAN_64le_BITPATTERN_match, Rest/binary>> ->
                    {nan, Rest};
                <<N:64/little-float, Rest/binary>> ->
                    {N, Rest}
            end;
        string ->
            {Len, Rest} = decode_varint(Bin, 64),
            <<Utf8Str:Len/binary, Rest2/binary>> = Rest,
            {unicode:characters_to_list(Utf8Str, unicode), Rest2};
        bytes ->
            {Len, Rest} = decode_varint(Bin, 64),
            <<Bytes:Len/binary, Rest2/binary>> = Rest,
            {Bytes, Rest2};
        {msg,MsgName} ->
            {Len, Rest} = decode_varint(Bin, 64),
            <<MsgBytes:Len/binary, Rest2/binary>> = Rest,
            {decode_msg(MsgBytes, MsgName, MsgDefs), Rest2};
        fixed32 ->
            <<N:32/little, Rest/binary>> = Bin,
            {N, Rest};
        sfixed32 ->
            <<N:32/little-signed, Rest/binary>> = Bin,
            {N, Rest};
        float ->
            case Bin of
                <<?PLUS_INF_32le_BITPATTERN, Rest/binary>> ->
                    {infinity, Rest};
                <<?MINUS_INF_32le_BITPATTERN, Rest/binary>> ->
                    {'-infinity', Rest};
                <<?NAN_32le_BITPATTERN_match, Rest/binary>> ->
                    {nan, Rest};
                <<N:32/little-float, Rest/binary>> ->
                    {N, Rest}
            end;
        {map,KeyType,ValueType} ->
            MsgName = map_item_tmp_name(),
            MsgDefs2 = map_msg_defs_for_decoding(KeyType, ValueType, MsgDefs),
            {{MsgName,Key,Value}, Rest2} =
                decode_type({msg, MsgName}, Bin, MsgDefs2),
            case {ValueType, Value} of
                {{msg,_},undefined} ->
                    error({gpb_error, {missing_value_for_mapfield,FieldType}});
                _ ->
                    ok
            end,
            {{Key,Value}, Rest2}
        end.

decode_group(#?gpb_field{type={group,Name}, fnum=FieldNum}, Bin, MsgDefs) ->
    {GroupBin, Rest} = read_group(Bin, FieldNum),
    Key    = {group,Name},
    Msg    = new_initial_msg(Key, MsgDefs),
    MsgDef = keyfetch(Key, MsgDefs),
    {decode_field(GroupBin, MsgDef, MsgDefs, Msg), Rest}.

read_group(Bin, FieldNum) ->
    {NumBytes, EndTagLen} = read_gr_b(Bin, 0, 0, 0, 0, FieldNum),
    <<GroupBin:NumBytes/binary, _:EndTagLen/binary, Rest/binary>> = Bin,
    {GroupBin, Rest}.

%% Like skipping over fields, but record the total length,
%% Each field is <(FieldNum bsl 3) bor FieldType> ++ <FieldValue>
%% Record the length because varints may be non-optimally encoded.
%%
%% Groups can be nested, but assume the same FieldNum cannot be nested
%% because group field numbers are shared with the rest of the fields numbers.
%% Thus we can search just for an group-end with the same field number.
%%
%% (The only time the same group field number could occur would
%% be in a nested sub message, but then it would be inside a
%% length-delimited entry, which we skip-read by length.)
read_gr_b(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen, FieldNum)
  when N < (32-7) ->
    read_gr_b(Tl, N+7, X bsl N + Acc, NumBytes, TagLen+1, FieldNum);
read_gr_b(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen, FieldNum) ->
    Key = X bsl N + Acc,
    TagLen1 = TagLen + 1,
    case {Key bsr 3, decode_wiretype(Key band 7)} of
        {FieldNum, group_end} ->
            {NumBytes, TagLen1};
        {_, varint} ->
            read_gr_vi(Tl, 0, NumBytes + TagLen1, FieldNum);
        {_, bits64} ->
            <<_:64, Tl2/binary>> = Tl,
            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 8, 0, FieldNum);
        {_, length_delimited} ->
            read_gr_ld(Tl, 0, 0, NumBytes + TagLen1, FieldNum);
        {_, group_start} ->
            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);
        {_, group_end} ->
            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);
        {_, bits32} ->
            <<_:32, Tl2/binary>> = Tl,
            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 4, 0, FieldNum)
    end.

read_gr_vi(<<1:1, _:7, Tl/binary>>, N, NumBytes, FieldNum)
  when N < (64-7) ->
    read_gr_vi(Tl, N+7, NumBytes+1, FieldNum);
read_gr_vi(<<0:1, _:7, Tl/binary>>, _, NumBytes, FieldNum) ->
    read_gr_b(Tl, 0, 0, NumBytes+1, 0, FieldNum).

read_gr_ld(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum)
  when N < (64-7) ->
    read_gr_ld(Tl, N+7, X bsl N + Acc, NumBytes+1, FieldNum);
read_gr_ld(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum) ->
    Len = X bsl N + Acc,
    NumBytes1 = NumBytes + 1,
    <<_:Len/binary, Tl2/binary>> = Tl,
    read_gr_b(Tl2, 0, 0, NumBytes1 + Len, 0, FieldNum).


add_field(Value, FieldDef, false=_IsOneof, MsgDefs, Record) ->
    %% FIXME: what about bytes?? "For numeric types and strings, if
    %% the same value appears multiple times, the parser accepts the
    %% last value it sees." But what about bytes?
    %% http://code.google.com/apis/protocolbuffers/docs/encoding.html
    %% For now, we assume it works like strings.
    case FieldDef of
        #?gpb_field{rnum = RNum, occurrence = required, type = {msg,_Name}}->
            merge_field_msg(RNum, Value, Record, MsgDefs);
        #?gpb_field{rnum = RNum, occurrence = optional, type = {msg,_Name}}->
            merge_field_msg(RNum, Value, Record, MsgDefs);
        #?gpb_field{rnum = RNum, occurrence = required, type = {group,_Name}}->
            merge_field_group(RNum, Value, Record, MsgDefs);
        #?gpb_field{rnum = RNum, occurrence = optional, type = {group,_Name}}->
            merge_field_group(RNum, Value, Record, MsgDefs);
        #?gpb_field{rnum = RNum, occurrence = required}->
            setelement(RNum, Record, Value);
        #?gpb_field{rnum = RNum, occurrence = optional}->
            setelement(RNum, Record, Value);
        #?gpb_field{rnum = RNum, occurrence = defaulty}->
            setelement(RNum, Record, Value);
        #?gpb_field{rnum = RNum, occurrence = repeated, type={map,_,_}} ->
            append_to_map(RNum, Value, Record);
        #?gpb_field{rnum = RNum, occurrence = repeated} ->
            append_to_element(RNum, Value, Record)
    end;
add_field(Value, FieldDef, true=_IsOneof, MsgDefs, Record) ->
    #?gpb_field{rnum=RNum, name=Name} = FieldDef,
    case FieldDef of
        #?gpb_field{type={msg,_SubMsgType}} ->
            case element(RNum, Record) of
                {Name, PrevMsg} ->
                    MergedMsg = {Name, merge_msgs(PrevMsg, Value, MsgDefs)},
                    setelement(RNum, Record, MergedMsg);
                _ ->
                    setelement(RNum, Record, {Name, Value})
            end;
        _ ->
            setelement(RNum, Record, {Name, Value})
    end.

merge_field_msg(RNum, NewMsg, Record, MsgDefs) ->
    case element(RNum, Record) of
        undefined ->
            setelement(RNum, Record, NewMsg);
        PrevMsg ->
            MergedMsg = merge_msgs(PrevMsg, NewMsg, MsgDefs),
            setelement(RNum, Record, MergedMsg)
    end.

merge_field_group(RNum, NewGroup, Record, MsgDefs) ->
    case element(RNum, Record) of
        undefined ->
            setelement(RNum, Record, NewGroup);
        PrevMsg ->
            MergedGroup = merge_groups(PrevMsg, NewGroup, MsgDefs),
            setelement(RNum, Record, MergedGroup)
    end.

append_to_element(RNum, NewElem, Record) ->
    PrevElems = element(RNum, Record),
    setelement(RNum, Record, [NewElem | PrevElems]).

append_to_map(RNum, {Key, _Value}=NewItem, Record) ->
    PrevElems = element(RNum, Record),
    NewElems = lists:keystore(Key, 1, PrevElems, NewItem),
    setelement(RNum, Record, NewElems).

%% @doc Merge messages on record format.
%%
%% Merging of messages M1 and M2 are defined in protobuf to work like this:
%% <dl>
%%   <dt>integers, floats, booleans, strings,  bytes and enums</dt>
%%   <dd>If the field is set in M2, it overwrites
%%       any field in M1.</dd>
%%
%%   <dt>Repeated fields</dt>
%%   <dd>Elements of the field in M2 are appended to that of M1</dd>
%%
%%   <dt>Sub messages</dt>
%%   <dd>Fields that are sub messages are merged recursively.</dd>
%%
%%   <dt>Map fields</dt>
%%   <dd>A map is treated as a repeated field in the sense that
%%       map elements of M2 are added to M1. If a key of exists
%%       in both M1 and M2, the map element of M2 overwrites
%%       that of M1.</dd>
%%
%%   <dt>Oneof fields</dt>
%%   <dd>A oneof field of M2 generally overwrites a oneof field of M1.
%%       If the oneof field is a sub message, they are merged
%%       recursively.</dd>
%% </dl>
-spec merge_msgs(tuple(), tuple(), gpb_defs:defs()) -> tuple().
merge_msgs(PrevMsg, NewMsg, MsgDefs)
  when element(1,PrevMsg) == element(1,NewMsg) ->
    Key = {msg,element(1,PrevMsg)},
    merge_m_g_aux(PrevMsg, NewMsg, Key, MsgDefs).

merge_groups(PrevGroup, NewGroup, MsgDefs)
  when element(1,PrevGroup) == element(1,NewGroup) ->
    Key = {group,element(1,PrevGroup)},
    merge_m_g_aux(PrevGroup, NewGroup, Key, MsgDefs).

merge_m_g_aux(PrevMsg, NewMsg, Key, MsgDefs) ->
    MsgDef = keyfetch(Key, MsgDefs),
    lists:foldl(
      fun(#?gpb_field{rnum=RNum, occurrence=repeated, type=Type}, AccRecord) ->
              case Type of
                  {map,_,_} ->
                      NewMap  = element(RNum, NewMsg),
                      lists:foldl(
                        fun(NewItem, R) -> append_to_map(RNum, NewItem, R) end,
                        AccRecord,
                        NewMap);
                  _ ->
                      PrevSeq = element(RNum, AccRecord),
                      NewSeq  = element(RNum, NewMsg),
                      setelement(RNum, AccRecord, PrevSeq ++ NewSeq)
              end;
         (#?gpb_field{rnum=RNum, type={msg,_FieldMsgName}}, AccRecord) ->
              case {element(RNum, AccRecord), element(RNum, NewMsg)} of
                  {undefined, undefined} ->
                      AccRecord;
                  {undefined, NewSubMsg} ->
                      setelement(RNum, AccRecord, NewSubMsg);
                  {_PrevSubMsg, undefined} ->
                      AccRecord;
                  {PrevSubMsg, NewSubMsg} ->
                      MergedSubMsg = merge_msgs(PrevSubMsg, NewSubMsg, MsgDefs),
                      setelement(RNum, AccRecord, MergedSubMsg)
              end;
         (#?gpb_field{rnum=RNum, type={group,_FieldGroupName}}, AccRecord) ->
              case {element(RNum, AccRecord), element(RNum, NewMsg)} of
                  {undefined, undefined} ->
                      AccRecord;
                  {undefined, NewSubGroup} ->
                      setelement(RNum, AccRecord, NewSubGroup);
                  {_PrevSubGroup, undefined} ->
                      AccRecord;
                  {PrevSubGroup, NewSubGroup} ->
                      MergedSubGroup = merge_groups(PrevSubGroup, NewSubGroup,
                                                    MsgDefs),
                      setelement(RNum, AccRecord, MergedSubGroup)
              end;
         (#?gpb_field{rnum=RNum}, AccRecord) ->
              case element(RNum, NewMsg) of
                  undefined -> AccRecord;
                  NewValue  -> setelement(RNum, AccRecord, NewValue)
              end;
         (#gpb_oneof{rnum=RNum, fields=OFields}, AccRecord) ->
              %% The language guide for oneof says that
              %%
              %%   "If the parser encounters multiple members of the
              %%   same oneof on the wire, only the last member seen
              %%   is used in the parsed message."
              %%
              %% In practice, this seems to mean they are merged,
              %% at least according to experiments with generated c++ code.
              %%
              case {element(RNum, AccRecord), element(RNum, NewMsg)} of
                  {undefined, undefined} ->
                      AccRecord;
                  {undefined, NewElem} ->
                      setelement(RNum, AccRecord, NewElem);
                  {_PrevElem, undefined} ->
                      AccRecord;
                  {{OFName, PrevValue}, {OFName, NewValue}=NewElem} ->
                      case lists:keyfind(OFName, #?gpb_field.name, OFields) of
                          #?gpb_field{type={msg,_}} ->
                              NewSub = merge_msgs(PrevValue, NewValue, MsgDefs),
                              setelement(RNum, AccRecord, {OFName,NewSub});
                          #?gpb_field{} ->
                              setelement(RNum, AccRecord, NewElem)
                      end;
                  {_PrevElem, NewElem} ->
                      %% oneof fields
                      setelement(RNum, AccRecord, NewElem)
              end
      end,
      PrevMsg,
      MsgDef).


%% @doc Encode a message on record format to a binary.
-spec encode_msg(tuple(), gpb_defs:defs()) -> binary().
encode_msg(Msg, MsgDefs) ->
    MsgName = element(1, Msg),
    MsgDef = keyfetch({msg, MsgName}, MsgDefs),
    encode_2(MsgDef, Msg, MsgDefs, <<>>).

encode_group(Msg, MsgDefs) ->
    GroupName = element(1, Msg),
    MsgDef = keyfetch({group, GroupName}, MsgDefs),
    encode_2(MsgDef, Msg, MsgDefs, <<>>).

encode_2([#?gpb_field{}=Field | Rest], Msg, MsgDefs, Acc) ->
    EncodedField =
        case classify_field_for_encoding(Field) of
            repeated_packed ->
                encode_packed(Field, Msg, MsgDefs);
            normal_field ->
                encode_field(Field, Msg, MsgDefs);
            unknowns ->
                encode_unknowns(Field, Msg, MsgDefs)
        end,
    encode_2(Rest, Msg, MsgDefs, <<Acc/binary, EncodedField/binary>>);
encode_2([#gpb_oneof{fields=Fields, rnum=RNum} | Rest], Msg, MsgDefs, Acc) ->
    case element(RNum, Msg) of
        {Name, Value} ->
            Field = lists:keyfind(Name, #?gpb_field.name, Fields),
            NewAcc = encode_2([Field#?gpb_field{occurrence=required}],
                              setelement(RNum, Msg, Value), MsgDefs,
                              Acc),
            encode_2(Rest, Msg, MsgDefs, NewAcc);
        undefined ->
            encode_2(Rest, Msg, MsgDefs, Acc)
    end;
encode_2([], _Msg, _MsgDefs, Acc) ->
    Acc.

classify_field_for_encoding(#?gpb_field{occurrence=Occurrence}=Field) ->
    case is_field_for_unknowns(Field) of
        true ->
            unknowns;
        false ->
            case {Occurrence, is_packed(Field)} of
                {repeated, true} ->
                    repeated_packed;
                _ ->
                    normal_field
            end
    end.

encode_packed(#?gpb_field{rnum=RNum, fnum=FNum, type=Type}, Msg, MsgDefs) ->
    case element(RNum, Msg) of
        []    ->
            <<>>;
        Elems ->
            PackedFields = encode_packed_2(Elems, Type, MsgDefs, <<>>),
            <<(encode_fnum_type(FNum, bytes))/binary,
              (encode_varint(byte_size(PackedFields)))/binary,
              PackedFields/binary>>
    end.

encode_packed_2([Elem | Rest], Type, MsgDefs, Acc) ->
    NewAcc = <<Acc/binary, (encode_value(Elem, Type, MsgDefs))/binary>>,
    encode_packed_2(Rest, Type, MsgDefs, NewAcc);
encode_packed_2([], _Type, _MsgDefs, Acc) ->
    Acc.

encode_field(#?gpb_field{rnum=RNum, fnum=FNum, type=Type, occurrence=required},
             Msg, MsgDefs) ->
    Value = element(RNum, Msg),
    encode_field_value(Value, FNum, Type, MsgDefs);
encode_field(#?gpb_field{rnum=RNum, fnum=FNum, type=Type, occurrence=optional},
             Msg, MsgDefs) ->
    case element(RNum, Msg) of
        undefined ->
            <<>>;
        Value ->
            encode_field_value(Value, FNum, Type, MsgDefs)
    end;
encode_field(#?gpb_field{rnum=RNum, fnum=FNum, type=Type, occurrence=defaulty},
             Msg, MsgDefs) ->
    case element(RNum, Msg) of
        undefined ->
            <<>>;
        Value ->
            case is_proto3_type_default(Type, MsgDefs, Value) of
                true ->
                    <<>>;
                false ->
                    encode_field_value(Value, FNum, Type, MsgDefs)
            end
    end;
encode_field(#?gpb_field{rnum=RNum, fnum=FNum, type=Type, occurrence=repeated},
             Msg, MsgDefs) ->
    encode_repeated(element(RNum, Msg), FNum, Type, MsgDefs, <<>>).

encode_repeated([Elem | Rest], FNum, Type, MsgDefs, Acc) ->
    EncodedValue = encode_field_value(Elem, FNum, Type, MsgDefs),
    NewAcc = <<Acc/binary, EncodedValue/binary>>,
    encode_repeated(Rest, FNum, Type, MsgDefs, NewAcc);
encode_repeated([], _FNum, _Type, _MsgDefs, Acc) ->
    Acc.

encode_field_value(Value, FNum, {group,_}=Type, MsgDefs) ->
    <<(encode_fnum_type(FNum, group_start))/binary,
      (encode_value(Value, Type, MsgDefs))/binary,
      (encode_fnum_type(FNum, group_end))/binary>>;
encode_field_value(Value, FNum, Type, MsgDefs) ->
    <<(encode_fnum_type(FNum, Type))/binary,
      (encode_value(Value, Type, MsgDefs))/binary>>.

encode_fnum_type(FNum, Type) ->
    encode_varint((FNum bsl 3) bor encode_wiretype(Type)).

encode_value(Value, Type, MsgDefs) ->
    case Type of
        sint32 ->
            encode_varint(encode_zigzag(Value));
        sint64 ->
            encode_varint(encode_zigzag(Value));
        int32 ->
            if Value >= 0 ->
                    encode_varint(Value);
               true ->
                    %% Encode as a 64 bit value, for interop compatibility.
                    %% Some implementations don't decode 32 bits properly,
                    %% and Google's protobuf (C++) encodes as 64 bits
                    <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
                    encode_varint(N)
            end;
        int64 ->
            if Value >= 0 ->
                    encode_varint(Value);
               true ->
                    <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
                    encode_varint(N)
            end;
        uint32 ->
            encode_varint(Value);
        uint64 ->
            encode_varint(Value);
        bool ->
            if Value       -> encode_varint(1);
               not Value   -> encode_varint(0);
               Value =:= 1 -> encode_varint(1);
               Value =:= 0 -> encode_varint(0)
            end;
        {enum, _EnumName}=Key ->
            N = if is_atom(Value) ->
                        {Key, EnumValues} = lists:keyfind(Key, 1, MsgDefs),
                        case lists:keyfind(Value, 1, EnumValues) of
                            {Value, EN, _} -> EN; % proto_defs_version 3
                            {Value, EN}    -> EN  % proto_defs_version 2
                        end;
                   is_integer(Value) ->
                        Value
                end,
            encode_value(N, int32, MsgDefs);
        fixed64 ->
            <<Value:64/little>>;
        sfixed64 ->
            <<Value:64/signed-little>>;
        double ->
            case Value of
                nan         -> <<?NAN_64le_BITPATTERN_make>>;
                infinity    -> <<?PLUS_INF_64le_BITPATTERN>>;
                '-infinity' -> <<?MINUS_INF_64le_BITPATTERN>>;
                _           -> <<Value:64/float-little>>
            end;
        string ->
            Utf8 = unicode:characters_to_binary(Value),
            <<(encode_varint(byte_size(Utf8)))/binary, Utf8/binary>>;
        bytes ->
            if is_binary(Value) ->
                   <<(encode_varint(byte_size(Value)))/binary, Value/binary>>;
               is_list(Value) ->
                   ValueBin = iolist_to_binary(Value),
                   <<(encode_varint(byte_size(ValueBin)))/binary, ValueBin/binary>>
            end;
        {msg,_MsgName} ->
            SubMsg = encode_msg(Value, MsgDefs),
            <<(encode_varint(byte_size(SubMsg)))/binary, SubMsg/binary>>;
        {group,_MsgName} ->
            encode_group(Value, MsgDefs);
        fixed32 ->
            <<Value:32/little>>;
        sfixed32 ->
            <<Value:32/signed-little>>;
        float ->
            case Value of
                nan         -> <<?NAN_32le_BITPATTERN_make>>;
                infinity    -> <<?PLUS_INF_32le_BITPATTERN>>;
                '-infinity' -> <<?MINUS_INF_32le_BITPATTERN>>;
                _           -> <<Value:32/float-little>>
            end;
        {map,KeyType,ValueType} ->
            {Key,Value1} = Value,
            MsgName = map_item_tmp_name(),
            MsgDefs1 = [map_item_tmp_def(KeyType, ValueType) | MsgDefs],
            encode_value({MsgName,Key,Value1}, {msg,MsgName}, MsgDefs1)
    end.

encode_unknowns(#?gpb_field{rnum=RNum}, Msg, _MsgDefs) ->
    encode_unknown_fields(element(RNum, Msg)).

encode_unknown_fields(Fields) ->
    lists:foldl(
      fun(Unknown, Acc) ->
              case Unknown of
                  {varint, FieldNum, N} ->
                      Key = encode_fnum_type(FieldNum, int32),
                      NBin = encode_varint(N),
                      <<Acc/binary, Key/binary, NBin/binary>>;
                  {fixed64, FieldNum, N} ->
                      Key = encode_fnum_type(FieldNum, fixed64),
                      <<Acc/binary, Key/binary, N:64/little>>;
                  {length_delimited, FieldNum, Data} ->
                      Key = encode_fnum_type(FieldNum, string),
                      Len = encode_varint(byte_size(Data)),
                      <<Acc/binary, Key/binary, Len/binary, Data/binary>>;
                  {group, FieldNum, GroupFields} ->
                      GS = encode_fnum_type(FieldNum, group_start),
                      GroupData = encode_unknown_fields(GroupFields),
                      GE = encode_fnum_type(FieldNum, group_end),
                      <<Acc/binary, GS/binary, GroupData/binary, GE/binary>>;
                  {fixed32, FieldNum, N} ->
                      Key = encode_fnum_type(FieldNum, fixed32),
                      <<Acc/binary, Key/binary, N:32/little>>
              end
      end,
      <<>>,
      Fields).

%% @doc Encode a wire type for a protobuf field type.
-spec encode_wiretype(gpb_field_type()) -> non_neg_integer().
encode_wiretype(sint32)            -> 0;
encode_wiretype(sint64)            -> 0;
encode_wiretype(int32)             -> 0;
encode_wiretype(int64)             -> 0;
encode_wiretype(uint32)            -> 0;
encode_wiretype(uint64)            -> 0;
encode_wiretype(bool)              -> 0;
encode_wiretype({enum, _EnumName}) -> 0;
encode_wiretype(fixed64)           -> 1;
encode_wiretype(sfixed64)          -> 1;
encode_wiretype(double)            -> 1;
encode_wiretype(string)            -> 2;
encode_wiretype(bytes)             -> 2;
encode_wiretype({msg,_MsgName})    -> 2;
encode_wiretype(group_start)       -> 3;
encode_wiretype(group_end)         -> 4;
encode_wiretype(fixed32)           -> 5;
encode_wiretype(sfixed32)          -> 5;
encode_wiretype(float)             -> 5;
encode_wiretype({map,_KT,_VT}) -> encode_wiretype({msg,map_item_tmp_name()}).

%% @equiv decode_varint(Bin, 64)
-spec decode_varint(binary()) -> {non_neg_integer(), Rest::binary()}.
decode_varint(Bin) -> decode_varint(Bin, 64).

%% @doc Decode an unsigned varint.  The decoded integer will have be at most
%% `MaxNumBits' bits long. Any higher bits will be masked away. If the binary
%% contains an overly long encoded integer, this function will fail.
-spec decode_varint(binary(), pos_integer()) -> {non_neg_integer(), binary()}.
decode_varint(Bin, MaxNumBits) -> de_vi(Bin, 0, 0, MaxNumBits).

de_vi(<<1:1, X:7, Rest/binary>>, N, Acc, MaxNumBits) when N < (64-7) ->
    de_vi(Rest, N+7, X bsl N + Acc, MaxNumBits);
de_vi(<<0:1, X:7, Rest/binary>>, N, Acc, MaxNumBits) ->
    Mask = (1 bsl MaxNumBits) - 1,
    {(X bsl N + Acc) band Mask, Rest}.

%% @doc Decode a varint-length-delimited packet.
%% This function works like `erlang:decode_packet/3', but the length
%% is a varint encoded value.
%%
%% The `Type' is either `uint32' or `uint64', indicating the maximum
%% number of bits considered as length of the packet.
%%
%% Available options (just as for `erlang:decode_packet/3'):
%% <dl>
%%   <dt>`{packet_size, MaxNumBytes :: integer() >= 0}'</dt>
%%   <dd>This defines a maximum packet size. 0, which is the default,
%%       means no upper limit. If the encoded length indicates a packet
%%       of size larger than `packet_size', then `{error, invalid}' will
%%       be returned instead.</dd>
%% </dl>
%%
%% This function will return:
%% <dl>
%%   <dt>`{ok, Packet :: binary(), Rest :: binary()}'</dt>
%%   <dd>One packet was extracted in `Packet', trailing data is in `Rest'</dd>
%%   <dt>`{more, Length}' when<br/>
%%       `Length = TotalSize :: integer() >= 1 | undefined'</dt>
%%   <dd>This signifies that more data is needed to extracted one packet.
%%       If sufficient data was present to know the size of the packet,
%%       an integer is returned indicating the <em>total</em> number of
%%       bytes needed to extract one packet.
%%       </dd>
%%   <dt>`{error, Reason :: term()}'</dt>
%%   <dd>If the length is malformed, for instance overly many bytes
%%       are used to encode the length, then `{error,invalid}'
%%       will be returned. The idea is to protect against a denial of
%%       service in case an adversary would send a valid (possibly small)
%%       length encoded using a huge number of bytes.</dd>
%% </dl>
%%
%% Example: Here is an example of how to use this function to receive
%% varint length delimited packets from a TCP/IP socket in `active' and
%% `binary' mode.
%% ```
%%    receive_tcp_data(Socket, Pending) when is_binary(Pending) ->
%%        receive
%%           {tcp, Socket, IncomingBin} ->
%%               Data = <<Pending/binary, IncomingBin/binary>>,
%%               %% We may now have either a partial packet,
%%               %% or one packet and possibly some more,
%%               %% or several packets and possibly some more.
%%               %% Handle them in a loop:
%%               NewPending = handle_packets(Data),
%%               receive_tcp_data(Socket, NewPending)
%%        end.
%%
%%    handle_packets(Data) ->
%%        %% In this example, we do not expect any decoding errors,
%%        %% for instance a malformed varint encoded length,
%%        %% so for simplicity fail fast on any error.
%%        case gpb:decode_packet(uint32, Data, []) of
%%            {ok, Packet, Rest} ->
%%                handle_one_packet(Packet), % do some work here
%%                handle_packets(Rest);
%%            {more, _Length} ->
%%                Data
%%        end.
%% '''
-spec decode_packet(Type, binary(), Opts) -> Result when
      Type   :: uint32 | uint64,
      Opts   :: [Opt],
      Opt    :: {packet_size, non_neg_integer()}, % max size, 0 means no limit
      Result :: {ok, Packet, Rest} |
                {more, Length} |
                {error, Reason::term()},
      Packet :: binary(),
      Rest   :: binary(),
      Length :: pos_integer() | undefined.
decode_packet(Type, Bin, Options) ->
    Bits = case Type of
                     uint32 -> 32;
                     uint64 -> 64
                 end,
    de_pkt(Bin, 0, 0, 0, Bits, Options).

de_pkt(<<1:1, X:7, Rest/binary>>, N, Acc, C, Bits, Opts) when N < (64-7) ->
    de_pkt(Rest, N+7, X bsl N + Acc, C + 1, Bits, Opts);
de_pkt(<<0:1, X:7, Rest/binary>>, N, Acc, Consumed, Bits, Opts) ->
    Mask = (1 bsl Bits) - 1,
    PktLen = (X bsl N + Acc) band Mask,
    MaxLen = proplists:get_value(packet_size, Opts, 0),
    if PktLen > MaxLen, MaxLen =/= 0 ->
            {error, invalid};
       byte_size(Rest) >= PktLen ->
            <<Pkt:PktLen/binary, Rest2/bitstring>> = Rest,
            {ok, Pkt, Rest2};
       true ->
            Total = Consumed + 1 + PktLen,
            {more, Total}
    end;
de_pkt(<<1:1, _/bitstring>>, N, _Acc, _C, _Bits, _Opts) when N >= (64-7) ->
    {error, invalid};
de_pkt(<<>>, _N, _Acc, _Consumed, _Bits, _Opts) ->
    {more, undefined}.

%% @doc Encode an unsigned varint to a binary.
-spec encode_varint(integer()) -> binary().
encode_varint(N) -> en_vi(N).

en_vi(N) when N =< 127 -> <<N>>;
en_vi(N) when N >= 128 -> <<1:1, (N band 127):7, (en_vi(N bsr 7))/binary>>.


decode_zigzag(N) when N band 1 =:= 0 -> N bsr 1;        %% N is even
decode_zigzag(N) when N band 1 =:= 1 -> -((N+1) bsr 1). %% N is odd

encode_zigzag(N) when N >= 0 -> N * 2;
encode_zigzag(N) when N <  0 -> N * -2 - 1.

%% @doc Verify type and range of fields in a message on record format.
-spec verify_msg(tuple() | term(), gpb_defs:defs()) -> ok.
verify_msg(Msg, MsgDefs) when is_tuple(Msg), tuple_size(Msg) >= 1 ->
    MsgName = element(1, Msg),
    case lists:keysearch({msg,MsgName}, 1, MsgDefs) of
        {value, _} ->
            verify_msg2(Msg, MsgName, MsgDefs, [top_level]);
        false ->
            mk_type_error(not_a_known_message, MsgName, [top_level])
    end;
verify_msg(Msg, _MsgDefs) ->
    mk_type_error(expected_a_message, Msg, []).

%% Verify that Msg is actually a message named MsgName as defined in MsgDefs
verify_msg2(Msg, MsgName, MsgDefs, Path) when is_tuple(Msg),
                                              element(1, Msg) == MsgName ->
    MsgKey = {msg, MsgName},
    {value, {MsgKey, Fields}} = lists:keysearch(MsgKey, 1, MsgDefs),
    if tuple_size(Msg) == length(Fields) + 1 ->
            Path2 = if Path == [top_level] -> [MsgName];
                       true                -> Path
                    end,
            verify_fields(Msg, Fields, Path2, MsgDefs);
       true ->
            mk_type_error({bad_record,MsgName}, Msg, Path)
    end;
verify_msg2(V, MsgName, _MsgDefs, Path) ->
    mk_type_error({bad_msg, MsgName}, V, Path).

verify_group(Msg, GName, MsgDefs, Path) when is_tuple(Msg),
                                             element(1, Msg) == GName ->
    Key = {group, GName},
    {value, {Key, Fields}} = lists:keysearch(Key, 1, MsgDefs),
    if tuple_size(Msg) == length(Fields) + 1 ->
            Path2 = if Path == [top_level] -> [GName];
                       true                -> Path
                    end,
            verify_fields(Msg, Fields, Path2, MsgDefs);
       true ->
            mk_type_error({bad_record,GName}, Msg, Path)
    end;
verify_group(V, GroupName, _MsgDefs, Path) ->
    mk_type_error({bad_group, GroupName}, V, Path).

verify_fields(Msg, Fields, Path, MsgDefs) when tuple_size(Msg)
                                               == length(Fields) + 1 ->
    lists:foreach(
      fun(#?gpb_field{name=Name, type=Type, rnum=RNum,
                      occurrence=Occurrence}=Field) ->
              case is_field_for_unknowns(Field) of
                  false ->
                      Value = element(RNum, Msg),
                      verify_value(Value, Type, Occurrence, Path++[Name],
                                   MsgDefs);
                  true ->
                      ok
              end;
         (#gpb_oneof{name=Name, rnum=RNum, fields=OFields}) ->
              case element(RNum, Msg) of
                  {FName, Value} ->
                      case lists:keyfind(FName, #?gpb_field.name, OFields) of
                          #?gpb_field{type=Type} ->
                              verify_value(Value, Type, optional, Path++[Name],
                                           MsgDefs);
                          false ->
                              mk_type_error(bad_oneof_indicator, FName, Path)
                      end;
                  undefined ->
                      ok;
                  Other ->
                      mk_type_error(bad_oneof_value, Other, Path)
              end
      end,
      Fields);
verify_fields(Msg, _Fields, Path, _MsgDefs) ->
    mk_type_error(bad_record, Msg, Path).

verify_value(Value, Type, Occurrence, Path, MsgDefs) ->
    case Occurrence of
        required -> verify_value_2(Value, Type, Path, MsgDefs);
        repeated -> verify_list(Value, Type, Path, MsgDefs);
        optional -> verify_optional(Value, Type, Path, MsgDefs);
        defaulty -> verify_optional(Value, Type, Path, MsgDefs)
    end.

%% @doc Verify type and range of a boolean, integral, floating point,
%% string, bytes or value.
-spec check_scalar(any(), gpb_scalar()) -> ok | {error, Reason::term()}.
check_scalar(Value, Type) when is_atom(Type) ->
    try
        verify_value_2(Value, Type, [], [])
    catch
        error:{gpb_type_error, {Reason, _Info}} ->
            {error, {Reason, Value}}
    end.

verify_value_2(V, int32, Path, _MsgDefs)    -> verify_int(V, {i,32}, Path);
verify_value_2(V, int64, Path, _MsgDefs)    -> verify_int(V, {i,64}, Path);
verify_value_2(V, uint32, Path, _MsgDefs)   -> verify_int(V, {u,32}, Path);
verify_value_2(V, uint64, Path, _MsgDefs)   -> verify_int(V, {u,64}, Path);
verify_value_2(V, sint32, Path, _MsgDefs)   -> verify_int(V, {i,32}, Path);
verify_value_2(V, sint64, Path, _MsgDefs)   -> verify_int(V, {i,64}, Path);
verify_value_2(V, fixed32, Path, _MsgDefs)  -> verify_int(V, {u,32}, Path);
verify_value_2(V, fixed64, Path, _MsgDefs)  -> verify_int(V, {u,64}, Path);
verify_value_2(V, sfixed32, Path, _MsgDefs) -> verify_int(V, {i,32}, Path);
verify_value_2(V, sfixed64, Path, _MsgDefs) -> verify_int(V, {i,64}, Path);
verify_value_2(V, bool, Path, _MsgDefs)     -> verify_bool(V, Path);
verify_value_2(V, float, Path, _MsgDefs)    -> verify_float(V, Path);
verify_value_2(V, double, Path, _MsgDefs)   -> verify_float(V, Path);
verify_value_2(V, string, Path, _MsgDefs)   -> verify_string(V, Path);
verify_value_2(V, bytes, Path, _MsgDefs)    -> verify_bytes(V, Path);
verify_value_2(V, {enum,E}, Path, MsgDefs)  -> verify_enum(V, E, MsgDefs, Path);
verify_value_2(V, {msg,M}, Path, MsgDefs)   -> verify_msg2(V, M, MsgDefs, Path);
verify_value_2(V, {group,G}, Path, MsgDefs) -> verify_group(V, G, MsgDefs,Path);
verify_value_2(V, {map,_,_}=M, Path, MsgDefs) -> verify_map(V, M, MsgDefs,Path).

verify_int(V, {i,32}, _) when -(1 bsl 31) =< V, V =< (1 bsl 31 - 1) -> ok;
verify_int(V, {i,64}, _) when -(1 bsl 63) =< V, V =< (1 bsl 63 - 1) -> ok;
verify_int(V, {u,32}, _) when 0 =< V, V =< (1 bsl 32 - 1)           -> ok;
verify_int(V, {u,64}, _) when 0 =< V, V =< (1 bsl 64 - 1)           -> ok;
verify_int(V, {S,Bits}, Path) ->
    Signedness = case S of
                     i -> signed;
                     u -> unsigned
                 end,
    if is_integer(V) ->
            mk_type_error({value_out_of_range, Signedness, Bits}, V, Path);
       true ->
            mk_type_error({bad_integer_value, Signedness, Bits}, V, Path)
    end.

verify_bool(true,  _) -> ok;
verify_bool(false, _) -> ok;
verify_bool(1,  _) -> ok;
verify_bool(0, _) -> ok;
verify_bool(V, Path) ->
    mk_type_error(bad_boolean_value, V, Path).

verify_float(V, _) when is_float(V) -> ok;
verify_float(V, _) when is_integer(V) -> ok;
verify_float(nan, _) -> ok;
verify_float(infinity, _) -> ok;
verify_float('-infinity', _) -> ok;
verify_float(V, Path) ->
    mk_type_error(bad_floating_point_value, V, Path).

verify_string(V, Path) when is_list(V); is_binary(V) ->
    try
        unicode:characters_to_binary(V),
        ok
    catch error:badarg ->
            mk_type_error(bad_unicode_string, V, Path)
    end;
verify_string(V, Path) ->
    mk_type_error(bad_unicode_string, V, Path).

verify_bytes(V, _) when is_binary(V) ->
    ok;
verify_bytes(V, Path) ->
    mk_type_error(bad_binary_value, V, Path).

verify_enum(V, EnumName, MsgDefs, Path) ->
    EnumKey = {enum, EnumName},
    if is_atom(V) ->
            {EnumKey, Enumerations} = lists:keyfind(EnumKey, 1, MsgDefs),
            case lists:keymember(V, 1, Enumerations) of
                true  -> ok;
                false -> mk_type_error(bad_enum_value, V, Path)
            end;
       is_integer(V) ->
            %% must be 32 bit signed int, I think
            verify_int(V, {i,32}, Path)
    end.

verify_map({Key,Value}, {map, KeyType, ValueType}, MsgDefs, Path) ->
    MsgName = map_item_tmp_name(),
    MsgDefs1 = [map_item_tmp_def(KeyType, ValueType) | MsgDefs],
    MapAsMsg = {MsgName, Key, Value},
    verify_msg2(MapAsMsg, MsgName, MsgDefs1, [mapitem | Path]);
verify_map(V, _, _, Path) ->
    mk_type_error(bad_map_item_value, V, Path).

verify_list(Elems, Type, Path, MsgDefs) when is_list(Elems) ->
    lists:foreach(fun(Elem) -> verify_value_2(Elem, Type, Path, MsgDefs) end,
                  Elems);
verify_list(Elems, Type, Path, _MsgDefs) ->
    mk_type_error({bad_repeated,Type}, Elems, Path).

verify_optional(undefined, _Type, _Path, _MsgDefs) ->
    ok;
verify_optional(Value, Type, Path, MsgDefs) ->
    verify_value_2(Value, Type, Path, MsgDefs).

mk_type_error(Error, ValueSeen, Path) ->
    Path2 = if Path == [] ->
                    top_level;
               true ->
                    gpb_lib:dot_join([atom_to_list(E) || E <- Path])
            end,
    erlang:error({gpb_type_error, {Error, [{value, ValueSeen},{path, Path2}]}}).

%% --

%% Conversion functions between various forms of #?gpb_field{} and a proplist
%% with keys being the #?gpb_field{} record's field names.

%% @doc In definitions, convert msg field records to proplists.
-spec defs_records_to_proplists(gpb_defs:defs()) -> proplist_defs().
defs_records_to_proplists(Defs) ->
    [case Def of
         {{msg,Msg}, Fields} ->
             {{msg,Msg}, field_records_to_proplists(Fields)};
         Other ->
             Other
     end
     || Def <- Defs].

%% @doc In definitions, convert msg fields as proplists to field records.
-spec proplists_to_defs_records(proplist_defs()) -> gpb_defs:defs().
proplists_to_defs_records(Defs) ->
    [case Def of
         {{msg,Msg}, PropList} ->
             {{msg,Msg}, proplists_to_field_records(PropList)};
         Other ->
             Other
     end
     || Def <- Defs].

%% @doc Convert msg field definitions on field record format to proplists.
-spec field_records_to_proplists([field()]) -> [proplist_field()].
field_records_to_proplists(Fields) when is_list(Fields) ->
    [case F of
         #?gpb_field{} -> field_record_to_proplist(F);
         #gpb_oneof{}  -> oneof_record_to_proplist(F)
     end
     || F <- Fields].

%% @doc Convert one field definition on field record format to a proplist.
-spec field_record_to_proplist(#?gpb_field{}) -> [proplist_field_item()].
field_record_to_proplist(#?gpb_field{}=F) ->
    Names = record_info(fields, ?gpb_field),
    lists:zip(Names, tl(tuple_to_list(F))).

oneof_record_to_proplist(#gpb_oneof{}=F) ->
    Names = record_info(fields, gpb_oneof),
    [if FName == fields -> {FName, field_records_to_proplists(FValue)};
        FName /= fields -> {FName, FValue}
     end
     || {FName, FValue} <- lists:zip(Names, tl(tuple_to_list(F)))].

%% @doc Convert proplists for msg fields to msg field records.
-spec proplists_to_field_records([proplist_field()]) -> [field()].
proplists_to_field_records(PLs) ->
    [case {is_field_pl(PL), is_oneof_pl(PL)} of
         {true, false} -> proplist_to_field_record(PL);
         {false, true} -> proplist_to_oneof_record(PL)
     end
     || PL <- PLs].

is_field_pl(PL) -> are_all_fields_present(record_info(fields, ?gpb_field), PL).

is_oneof_pl(PL) -> are_all_fields_present(record_info(fields, gpb_oneof), PL).

are_all_fields_present(FNames, PL) ->
    lists:all(fun(FName) -> lists:keymember(FName, 1, PL) end,
              FNames).

%% @doc Convert a proplist for an msg field to an msg field record.
-spec proplist_to_field_record([proplist_field_item()]) -> #?gpb_field{}.
proplist_to_field_record(PL) when is_list(PL) ->
    Names = record_info(fields, ?gpb_field),
    RFields = [proplists:get_value(Name, PL) || Name <- Names],
    list_to_tuple([?gpb_field | RFields]).

proplist_to_oneof_record(PL) when is_list(PL) ->
    Names = record_info(fields, gpb_oneof),
    RFields = [proplists:get_value(Name, PL) || Name <- Names],
    list_to_tuple(
      [gpb_oneof | [if N == fields -> proplists_to_field_records(V);
                       N /= fields -> V
                    end
                    || {N, V} <- lists:zip(Names, RFields)]]).

%% @doc Convert rpc records to proplists.
-spec rpc_records_to_proplists([#?gpb_rpc{}]) -> [proplist_rpc()].
rpc_records_to_proplists(Rpcs) when is_list(Rpcs) ->
    [rpc_record_to_proplist(R) || R <- Rpcs].

%% @doc Convert an rpc record to a proplist.
-spec rpc_record_to_proplist(#?gpb_rpc{}) -> proplist_rpc().
rpc_record_to_proplist(#?gpb_rpc{}=R) ->
    Names = record_info(fields, ?gpb_rpc),
    lists:zip(Names, tl(tuple_to_list(R))).

%% @doc Convert proplists to rpc records.
-spec proplists_to_rpc_records([proplist_rpc()]) -> [#?gpb_rpc{}].
proplists_to_rpc_records(PLs) ->
    [proplist_to_rpc_record(PL) || PL <- PLs].

proplist_to_rpc_record(PL) when is_list(PL) ->
    Names = record_info(fields, ?gpb_rpc),
    RFields = [proplists:get_value(Name, PL) || Name <- Names],
    list_to_tuple([?gpb_rpc | RFields]).

map_item_tmp_def(KeyType, ValueType) ->
    MsgName = map_item_tmp_name(),
    {{msg, MsgName}, map_item_pseudo_fields(KeyType, ValueType)}.

map_item_tmp_def_for_decoding(KeyType, ValueType) ->
    MsgName = map_item_tmp_name(),
    {{msg, MsgName}, map_item_pseudo_fields_for_decoding(KeyType, ValueType)}.

map_item_tmp_name() ->
    '$mapitem'.

%% @doc Make a list of fields for a pseudo message, given a key type and
%% a value type for a `map<_,_>' type field, for use in encoding,
%% verification.
%%
%% At encoding a field value is to be included even if the proto syntax
%% is `"proto3"' and the value to encode is the type's default value.
-spec map_item_pseudo_fields(gpb_map_key(), gpb_map_value()) -> [field()].
map_item_pseudo_fields(KeyType, ValueType) ->
    [#?gpb_field{name=key, fnum=1, rnum=2,
                 occurrence=required, type=KeyType},
     #?gpb_field{name=value, fnum=2, rnum=3,
                 occurrence=required, type=ValueType}].

%% @doc Make a list of fields for a pseudo message, given a key type and
%% a value type for a `map<_,_>' type field, for use in decoding.
%%
%% At decoding if the field would not be present in the binary to
%% decode, it is expected to have be the type's default value, even when
%% the proto syntax is `"proto2"'.
-spec map_item_pseudo_fields_for_decoding(gpb_map_key(), gpb_map_value()) ->
          [field()].
map_item_pseudo_fields_for_decoding(KeyType, ValueType) ->
    [#?gpb_field{name=key, fnum=1, rnum=2, occurrence=defaulty,
                 type=KeyType},
     #?gpb_field{name=value, fnum=2, rnum=3, occurrence=defaulty,
                 type=ValueType}].

%% @doc Test whether a field type is allowed as key type in a `msg<_,_>'
%% type field.
-spec is_allowed_as_key_type(gpb_field_type()) -> boolean().
is_allowed_as_key_type({enum,_}) -> false;
is_allowed_as_key_type({msg,_}) -> false;
is_allowed_as_key_type(double) -> false;
is_allowed_as_key_type(float) -> false;
is_allowed_as_key_type(bytes) -> false;
is_allowed_as_key_type(_) -> true.

%% @doc Test whether a field is allowed to have the option `[packed]' or not.
-spec is_type_packable(gpb_field_type()) -> boolean().
is_type_packable(int32)     -> true;
is_type_packable(int64)     -> true;
is_type_packable(uint32)    -> true;
is_type_packable(uint64)    -> true;
is_type_packable(sint32)    -> true;
is_type_packable(sint64)    -> true;
is_type_packable(fixed32)   -> true;
is_type_packable(fixed64)   -> true;
is_type_packable(sfixed32)  -> true;
is_type_packable(sfixed64)  -> true;
is_type_packable(bool)      -> true;
is_type_packable(float)     -> true;
is_type_packable(double)    -> true;
is_type_packable(string)    -> false;
is_type_packable(bytes)     -> false;
is_type_packable({enum,_})  -> true;
is_type_packable({msg,_})   -> false;
is_type_packable({group,_}) -> false;
is_type_packable({map,_,_}) -> false.

%% --

is_packed(#?gpb_field{opts=Opts}) ->
    lists:member(packed, Opts).

%% @doc Test whether a message was defined with `"proto3"' syntax.
%%
%% If a file with `"proto3"' syntax import another file with `"proto2"'
%% syntax, or vice versa, then some messages will be of `"proto2"' while
%% others will be `"proto3"'.
-spec is_msg_proto3(atom(), gpb_defs:defs()) -> boolean().
is_msg_proto3(Name, MsgDefs) ->
    case lists:keyfind(proto3_msgs, 1, MsgDefs) of
        {proto3_msgs, Names} ->
            lists:member(Name, Names);
        false ->
            false
    end.

is_proto3_type_default(string, _MsgDefs, Value) ->
    unicode:characters_to_binary(Value) =:= <<>>;
is_proto3_type_default(bytes, _MsgDefs, Value) ->
    iolist_size(Value) == 0;
is_proto3_type_default({enum, _}, _MsgDefs, 0) ->
    true;
is_proto3_type_default(Type, MsgDefs, Value) ->
    proto3_type_default(Type, MsgDefs) =:= Value.

map_msg_defs_for_decoding(KeyType, ValueType, MsgDefs) ->
    [map_item_tmp_def_for_decoding(KeyType, ValueType) | MsgDefs].

%% @doc Return the default for a type of a message field in a `"proto2"' file.
-spec proto2_type_default(gpb_field_type(), gpb_defs:defs()) -> term().
proto2_type_default(Type, MsgDefs) ->
    %% Type-specific defaults for proto2 are the same as for proto3,
    %% with one slight exception, for enums. In both proto2 and
    %% proto3, the first defined enumerator is the type-specific
    %% default, but in proto3, the first enumerator must additionally
    %% have value = 0, ie the (would-be) wire-value for proto3 is 0.
    %%
    %% We go with the proto3 type defaults, hope it does not lead to
    %% some odd corner case.
    proto3_type_default(Type, MsgDefs).

%% @doc Return the default for a type of a message field in a `"proto3"' file.
-spec proto3_type_default(gpb_field_type(), gpb_defs:defs()) -> term().
proto3_type_default(Type, MsgDefs) ->
    case Type of
        sint32   -> 0;
        sint64   -> 0;
        int32    -> 0;
        int64    -> 0;
        uint32   -> 0;
        uint64   -> 0;
        bool     -> false;
        fixed64  -> 0;
        sfixed64 -> 0;
        double   -> 0.0;
        string   -> "";
        bytes    -> <<>>;
        {msg,_}  -> undefined;
        fixed32  -> 0;
        sfixed32 -> 0;
        float    -> 0.0;
        {map,_KT,_VT} -> [];
        {enum, _EnumName}=Key ->
            {Key, EnumDef} = lists:keyfind(Key, 1, MsgDefs),
            PDVsn = proplists:get_value(proto_defs_version, MsgDefs, 1),
            if PDVsn >= 3 ->
                    [{Sym0, _V0, _} | _] = EnumDef,
                    Sym0;
               PDVsn =< 2 ->
                    %% Skip any {option, _, _} elems:
                    [{Sym0, _V0} | _] = [Elem || {_Sym, _V}=Elem <- EnumDef],
                    Sym0
            end
    end.

-ifndef(NO_HAVE_MAPS).
-type map_opts()          :: map_opts(none()).
-type map_opts(OtherOpts) :: [map_opt(OtherOpts)].
-type map_opt(Other) :: {maps_unset_optional, omitted | present_undefined} |
                        {maps_oneof, flat | tuples} |
                        Other.

%% @doc Convert a message, as returned by eg {@link decode_msg/3}
%%      on tuple format to a map.
%%
%%      The `MapOpts' look similar to the options understood by
%%      gpb_compile, but is only a limited subset of them and they
%%      concern only different map representations and not the
%%      record representation.  While it can support some of the map
%%      options understood by gpb_compile it does not support
%%      mapfields_as_maps for the records representation, as this is not
%%      supported by the rest of the code in this module.
-spec msg_to_map(tuple(), gpb_defs:defs(), map_opts()) -> map().
msg_to_map(Msg, Defs, MapOpts) when is_atom(element(1, Msg)) ->
    MsgName = element(1, Msg),
    Fields = keyfetch({msg, MsgName}, Defs),
    Values = tl(tuple_to_list(Msg)),
    fields_to_map(Fields, Values, maps:new(), Defs, MapOpts).

fields_to_map([F | FRest], [V | VRest], Acc, Defs, Opts) ->
    case F of
        #?gpb_field{name=FName, occurrence=repeated, type={map,KType,VType}} ->
            KVs = [begin
                       ItemK2 = v_to_map(ItemK, KType, Defs, Opts),
                       ItemV2 = v_to_map(ItemV, VType, Defs, Opts),
                       {ItemK2, ItemV2}
                   end
                   || {ItemK, ItemV} <- V],
            KVs2 = case get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) of
                       maps      -> maps:from_list(KVs);
                       '2tuples' -> KVs
                   end,
            Acc2 = maps:put(FName, KVs2, Acc),
            fields_to_map(FRest, VRest, Acc2, Defs, Opts);
        #?gpb_field{name=FName, occurrence=repeated, type=Type} ->
            Elems2 = [v_to_map(Elem, Type, Defs, Opts) || Elem <- V],
            Acc2 = maps:put(FName, Elems2, Acc),
            fields_to_map(FRest, VRest, Acc2, Defs, Opts);
        #?gpb_field{name=FName, occurrence=Optional, type=Type}
          when Optional =:= optional;
               Optional =:= defaulty ->
            if V =:= undefined ->
                    case get_unset_by_opts(Opts) of
                        omitted ->
                            fields_to_map(FRest, VRest, Acc, Defs, Opts);
                        present_undefined ->
                            Acc2 = maps:put(FName, undefined, Acc),
                            fields_to_map(FRest, VRest, Acc2, Defs, Opts)
                    end;
               true ->
                    V2 = v_to_map(V, Type, Defs, Opts),
                    Acc2 = maps:put(FName, V2, Acc),
                    fields_to_map(FRest, VRest, Acc2, Defs, Opts)
            end;
        #?gpb_field{name=FName, occurrence=required, type=Type} ->
            V2 = v_to_map(V, Type, Defs, Opts),
            Acc2 = maps:put(FName, V2, Acc),
            fields_to_map(FRest, VRest, Acc2, Defs, Opts);
        #gpb_oneof{name=FName, fields=OFields} ->
            case V of
                undefined ->
                    case get_unset_by_opts(Opts) of
                        omitted ->
                            fields_to_map(FRest, VRest, Acc, Defs, Opts);
                        present_undefined ->
                            Acc2 = maps:put(FName, undefined, Acc),
                            fields_to_map(FRest, VRest, Acc2, Defs, Opts)
                    end;
                {Tag, OV} ->
                    #?gpb_field{type=Type} = fetch_field_by_name(Tag, OFields),
                    OV2 = v_to_map(OV, Type, Defs, Opts),
                    Acc2 = case flat_oneof_for_maps(Opts) of
                               true  -> maps:put(Tag, OV2, Acc);
                               false -> maps:put(FName, {Tag, OV2}, Acc)
                           end,
                    fields_to_map(FRest, VRest, Acc2, Defs, Opts)
            end
    end;
fields_to_map([], [], Acc, _Defs, _Opts) ->
    Acc.

v_to_map(V, {msg, _MsgName}, Defs, Opts) ->
    msg_to_map(V, Defs, Opts);
v_to_map(V, _Type, _Defs, _Opts) ->
    V.

%% @doc Convert a message, on map format to a tuple, so that it can be
%%      used with eg {@link encode_msg/2}. The options control how the map
%%      representation is to be interpreted, see {@link msg_to_map/3} for
%%      further discussion on this.
-spec msg_from_map(map(), atom(), gpb_defs:defs(), map_opts()) -> tuple().
msg_from_map(Map, MsgName, Defs, Opts) ->
    Fields = keyfetch({msg, MsgName}, Defs),
    Values = fields_from_map(Fields, Map, [], Defs, Opts),
    list_to_tuple([MsgName | Values]).

fields_from_map([F | Rest], Map, Acc, Defs, Opts) ->
    case F of
        #?gpb_field{name=FName, occurrence=repeated, type={map,KType,VType}} ->
            KVs1 = case get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) of
                       maps      -> maps:to_list(maps:get(FName, Map, []));
                       '2tuples' -> maps:get(FName, Map, [])
                   end,
            KVs2 = [begin
                        ItemK2 = v_from_map(ItemK, KType, Defs, Opts),
                        ItemV2 = v_from_map(ItemV, VType, Defs, Opts),
                        {ItemK2, ItemV2}
                    end
                    || {ItemK, ItemV} <- KVs1],
            Acc2 = [KVs2 | Acc],
            fields_from_map(Rest, Map, Acc2, Defs, Opts);
        #?gpb_field{name=FName, occurrence=repeated, type=Type} ->
            Elems1 = maps:get(FName, Map, []),
            Elems2 = [v_from_map(Elem, Type, Defs, Opts) || Elem <- Elems1],
            Acc2 = [Elems2 | Acc],
            fields_from_map(Rest, Map, Acc2, Defs, Opts);
        #?gpb_field{name=FName, occurrence=optional, type=Type} ->
            V2 = case maps:find(FName, Map) of
                     {ok, V} -> v_from_map(V, Type, Defs, Opts);
                     error   -> undefined
                 end,
            Acc2 = [V2 | Acc],
            fields_from_map(Rest, Map, Acc2, Defs, Opts);
        #?gpb_field{name=FName, occurrence=defaulty, type=Type} ->
            V2 = case maps:find(FName, Map) of
                     {ok, V} -> v_from_map(V, Type, Defs, Opts);
                     error   -> undefined
                 end,
            Acc2 = [V2 | Acc],
            fields_from_map(Rest, Map, Acc2, Defs, Opts);
        #?gpb_field{name=FName, occurrence=required, type=Type} ->
            {ok, V} = maps:find(FName, Map),
            V2 = v_from_map(V, Type, Defs, Opts),
            Acc2 = [V2 | Acc],
            fields_from_map(Rest, Map, Acc2, Defs, Opts);
        #gpb_oneof{fields=OFields} ->
            V2 = case {get_unset_by_opts(Opts), flat_oneof_for_maps(Opts)} of
                     {omitted, true} ->
                         v_flat_oneof_from_map(OFields, Map, Defs, Opts);
                     {omitted, false} ->
                         v_tupled_ommittable_oneof_from_map(F, Map, Defs, Opts);
                     {present_undefined, _} ->
                         v_tupled_oneof_from_map(F, Map, Defs, Opts)
                 end,
            Acc2 = [V2 | Acc],
            fields_from_map(Rest, Map, Acc2, Defs, Opts)
    end;
fields_from_map([], _, Acc, _Defs, _Opts) ->
    lists:reverse(Acc).

v_flat_oneof_from_map(OFields, Map, Defs, Opts) ->
    try
        lists:foreach(
          fun(#?gpb_field{name=Tag, type=Type}) ->
                  case maps:find(Tag, Map) of
                      {ok, V} ->
                          V2 = v_from_map(V, Type, Defs, Opts),
                          throw({found, {Tag, V2}});
                      error ->
                          try_next
                  end
          end,
          OFields),
        undefined
    catch throw:{found, V} ->
            V
    end.

v_tupled_ommittable_oneof_from_map(#gpb_oneof{name=FName, fields=OFields},
                                   Map, Defs, Opts) ->
    case maps:find(FName, Map) of
        {ok, {Tag, V}} ->
            #?gpb_field{type=Type} = fetch_field_by_name(Tag, OFields),
            V2 = v_from_map(V, Type, Defs, Opts),
            {Tag, V2};
        error ->
            undefined
    end.

v_tupled_oneof_from_map(#gpb_oneof{name=FName, fields=OFields},
                        Map, Defs, Opts) ->
    case maps:get(FName, Map, undefined) of
        {Tag, V} ->
            #?gpb_field{type=Type} = fetch_field_by_name(Tag, OFields),
            V2 = v_from_map(V, Type, Defs, Opts),
            {Tag, V2};
        undefined ->
            undefined
    end.

v_from_map(V, {msg,SubMsgName}, Defs, Opts) ->
    msg_from_map(V, SubMsgName, Defs, Opts);
v_from_map(V, _Type, _Defs, _Opts) ->
    V.

get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) ->
    case proplists:get_value(mapfields_as_maps, Opts, true) of
        true  -> maps;
        false -> '2tuples'
    end.

get_unset_by_opts(Opts) ->
    case proplists:get_value(maps_unset_optional, Opts, omitted) of
        omitted           -> omitted;
        present_undefined -> present_undefined
    end.

flat_oneof_for_maps(SubOpts) ->
    case proplists:get_value(maps_oneof, SubOpts, tuples) of
        tuples -> false;
        flat   -> true
    end.

fetch_field_by_name(Name, Fields) ->
    case lists:keyfind(Name, #?gpb_field.name, Fields) of
        #?gpb_field{}=Field ->
            Field;
        false ->
            Names = [Nm || #?gpb_field{name=Nm} <- Fields],
            erlang:error({error, {no_such_field, Name, Names}})
    end.

-endif. % -ifndef(NO_HAVE_MAPS).

keyfetch(Key, KVPairs) ->
    case lists:keysearch(Key, 1, KVPairs) of
        {value, {Key, Value}} ->
            Value;
        false ->
            erlang:error({error, {no_such_key, Key, KVPairs}})
    end.

is_field_for_unknowns(#?gpb_field{type=unknown, occurrence=repeated}) ->
    true;
is_field_for_unknowns(_) ->
    false.

version_format_test() ->
    ok = assert_version_format("2"),
    ok = assert_version_format("2.1"),
    ok = assert_version_format("2.1.1"),
    ok = assert_version_format("2.1.1.1"),
    %% a development version after 2.1, but before any 2.2
    ok = assert_version_format("2.1-53-gb996fbe"),
    %% non-digit version components
    ?assertError(_, assert_version_format("2.2x")),
    ?assertError(_, assert_version_format("2.x")),
    ?assertError(_, assert_version_format("3y")),
    ?assertError(_, assert_version_format("y")),
    ?assertError(_, assert_version_format("2.1-4z-gb996fbe")),
    ?assertError(_, assert_version_format("2.1-z-gb996fbe")),
    %% malplaced dots
    ?assertError(_, assert_version_format(".")),
    ?assertError(_, assert_version_format(".2")),
    ?assertError(_, assert_version_format("..2")),
    ?assertError(_, assert_version_format("2.")),
    ?assertError(_, assert_version_format("2.1..")),
    ?assertError(_, assert_version_format("2..1")),
    %% missing bits and pieces
    ?assertError(_, assert_version_format("2.1-53-")),
    ?assertError(_, assert_version_format("2.1-53-")),
    ?assertError(_, assert_version_format("2.1-")),
    ?assertError(_, assert_version_format("2-")),
    ?assertError(_, assert_version_format("-")),
    %% misc other
    ?assertError(_, assert_version_format("2.1--53-gb996fbe")).

version_as_list_test() ->
    [2,1] = version_as_list("2.1"),
    [2,1,1] = version_as_list("2.1.1"),
    [2,1,0,0,53,"gb996fbe"] = version_as_list("2.1-53-gb996fbe"),
    [2,2] = version_as_list("2.2").

encode_zigzag_test() ->
    0 = encode_zigzag(0),
    1 = encode_zigzag(-1),
    2 = encode_zigzag(1),
    3 = encode_zigzag(-2),
    4294967294 = encode_zigzag(2147483647),
    4294967295 = encode_zigzag(-2147483648).

decode_zigzag_test() ->
    0  = decode_zigzag(0),
    -1 = decode_zigzag(1),
    1  = decode_zigzag(2),
    -2 = decode_zigzag(3),
    2147483647  = decode_zigzag(4294967294),
    -2147483648 = decode_zigzag(4294967295).

decode_invalid_varint_fails_test() ->
    %% This varint is invalid because it is too long.
    %% approx 2.3e105, which is much longer than 32 or 64 bits
    %% The limit is not set too narrowly above 64 bits; the purpose
    %% is more to catch malicious input causing the decoder to
    %% eat memory until the vm dies (denial of service).
    InvalidVarint = iolist_to_binary([lists:duplicate(50, 255), 0]),
    ?assertError(_, decode_varint(InvalidVarint)).

skips_empty_groups_test() ->
    <<99,88,77>> = test_run_skip_field(<<(mk_group_start(1))/binary,
                                         (mk_group_end(1))/binary,
                                         99,88,77>>).
skips_nested_groups_test() ->
    Int32F = #?gpb_field{fnum=4, type=int32, rnum=2, occurrence=required},
    Bytes  = #?gpb_field{fnum=5, type=bytes, rnum=2, occurrence=required},
    <<99,88,77>> = test_run_skip_field(
                     <<(mk_group_start(1))/binary,
                       (mk_group_start(2))/binary,
                       (encode_field(Int32F, {x,4711}, []))/binary,
                       (encode_field(Bytes, {x,<<1,2,3>>}, []))/binary,
                       (mk_group_end(2))/binary,
                       (mk_group_end(1))/binary,
                       99,88,77>>).

test_run_skip_field(Bin) ->
    {Key, Rest} = decode_varint(Bin),
    WireType = encode_wiretype(group_start),
    {FieldNum, WireType} = {Key bsr 3, Key band 7},
    {_, Rest2} = skip_field(Rest, FieldNum, WireType),
    Rest2.

mk_group_start(FieldNum) ->
    encode_varint((FieldNum bsl 3) bor encode_wiretype(group_start)).

mk_group_end(FieldNum) ->
    encode_varint((FieldNum bsl 3) bor encode_wiretype(group_end)).
