%%% Copyright (C) 2017  Tomas Abrahamsson
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

%%% @doc Analyzes proto defs. Result is used by the generator modules
%%% and functions.
%%% @private

-module(gpb_analyzer).

-export([analyze_defs/4]).

-include("../include/gpb.hrl").
-include("gpb_compile.hrl").

-define(is_map_type(X), (is_tuple(X)
                         andalso tuple_size(X) =:= 3
                         andalso element(1, X) =:= map)).

%% -- analysis -----------------------------------------------------

analyze_defs(Defs, Sources, Renamings, Opts) ->
    ProtoDefsVsnElem = find_or_make_proto_defs_version_elem(Defs),
    MapTypes = find_map_types(Defs),
    MapsAsMsgs = map_types_to_msgs(MapTypes),
    DMapsAsMsgs = map_types_to_msgs_for_decoding(MapTypes),
    MapMsgEnums = enums_for_maps_as_msgs(MapTypes, Defs),
    Translations = compute_translations(Defs, Opts),
    KnownMsgSize = find_msgsizes_known_at_compile_time(MapsAsMsgs ++ Defs),
    UnknownsInfo = gpb_lib:defs_contains_fields_for_unknows(Defs),
    #anres{source_filenames    = Sources,
           used_types          = find_used_types(Defs),
           known_msg_size      = KnownMsgSize,
           fixlen_types        = find_fixlen_types(MapsAsMsgs ++ Defs),
           num_packed_fields   = find_num_packed_fields(MapsAsMsgs ++ Defs),
           num_fields          = find_num_fields(MapsAsMsgs ++ Defs),
           d_field_pass_method = compute_decode_field_pass_methods(
                                   MapsAsMsgs ++ Defs, Opts),
           maps_as_msgs        = ProtoDefsVsnElem ++ MapsAsMsgs ++ MapMsgEnums,
           dec_maps_as_msgs    = ProtoDefsVsnElem ++ DMapsAsMsgs ++ MapMsgEnums,
           translations        = Translations,
           map_types           = MapTypes,
           map_value_types     = compute_map_value_types(MapTypes),
           group_occurrences   = find_group_occurrences(Defs),
           has_p3_opt_strings  = has_p3_opt_strings(Defs),
           unknowns_info       = UnknownsInfo,
           renamings           = Renamings}.

find_or_make_proto_defs_version_elem(Defs) ->
    Vsn = proplists:get_value(proto_defs_version, Defs, 1),
    [{proto_defs_version, Vsn}].

find_map_types(Defs) ->
    gpb_lib:fold_msg_or_group_fields(
      fun(_, _MsgName, #?gpb_field{type={map,KeyType,ValueType}}, Acc) ->
              sets:add_element({KeyType,ValueType}, Acc);
         (_, _MsgName, _Field, Acc) ->
              Acc
      end,
      sets:new(),
      Defs).

map_types_to_msgs(MapTypes) ->
    sets:fold(fun({KeyType, ValueType}, Acc) ->
                      MsgName = gpb_lib:map_type_to_msg_name(KeyType,ValueType),
                      Fields = gpb:map_item_pseudo_fields(KeyType, ValueType),
                      [{{msg, MsgName}, Fields} | Acc]
              end,
              [],
              MapTypes).

map_types_to_msgs_for_decoding(MapTypes) ->
    sets:fold(fun({KeyType, ValueType}, Acc) ->
                      MsgName = gpb_lib:map_type_to_msg_name(KeyType,ValueType),
                      Fields = gpb:map_item_pseudo_fields_for_decoding(
                                 KeyType, ValueType),
                      [{{msg, MsgName}, Fields} | Acc]
              end,
              [],
              MapTypes).

enums_for_maps_as_msgs(MapTypes, Defs) ->
    MapEnumNames = sets:fold(fun({_Key, {enum, EName}}, Acc) -> [EName | Acc];
                                ({_KeyType, _ValueType}, Acc) -> Acc
                             end,
                             [],
                             MapTypes),
    [Enum || {{enum, EnumName}, _}=Enum <- Defs,
             lists:member(EnumName, MapEnumNames)].

compute_map_value_types(MapTypes) ->
    sets:fold(
      fun({_KT, {msg,_}}, {_SubMsgs, NonSubMsgs})  -> {true, NonSubMsgs};
         ({_KT, _VT},     {SubMsgs,  _NonSubMsgs}) -> {SubMsgs, true}
      end,
      {false, false},
      MapTypes).

find_used_types(Defs) ->
    gpb_lib:fold_msg_or_group_fields(
      fun(_Type, _MsgName, #?gpb_field{type={map,KeyType,ValueType}}, Acc) ->
              Acc1 = sets:add_element(KeyType, Acc),
              sets:add_element(ValueType, Acc1);
         (_Type, _MsgName, #?gpb_field{type=Type}=Field, Acc) ->
              case gpb_lib:is_field_for_unknowns(Field) of
                  false -> sets:add_element(Type, Acc);
                  true  -> Acc
              end
      end,
      sets:new(),
      Defs).

find_fixlen_types(Defs) ->
    gpb_lib:fold_msg_or_group_fields(
      fun(_, _, #?gpb_field{type=Type, occurrence=Occ}=FieldDef, Acc) ->
              IsPacked = gpb_lib:is_packed(FieldDef),
              FixlenTypeInfo = #ft{type       = Type,
                                   occurrence = Occ,
                                   is_packed  = IsPacked},
              case Type of
                  fixed32  -> sets:add_element(FixlenTypeInfo, Acc);
                  sfixed32 -> sets:add_element(FixlenTypeInfo, Acc);
                  float    -> sets:add_element(FixlenTypeInfo, Acc);
                  fixed64  -> sets:add_element(FixlenTypeInfo, Acc);
                  sfixed64 -> sets:add_element(FixlenTypeInfo, Acc);
                  double   -> sets:add_element(FixlenTypeInfo, Acc);
                  _        -> Acc
              end
      end,
      sets:new(),
      Defs).

find_num_packed_fields(Defs) ->
    gpb_lib:fold_msg_or_group_fields(
      fun(_, _MsgName, FieldDef, Acc) ->
              case gpb_lib:is_packed(FieldDef) of
                  true  -> Acc + 1;
                  false -> Acc
              end
      end,
      0,
      Defs).

find_num_fields(Defs) ->
    lists:foldl(fun({_msg_or_group, MsgName, MsgDef}, Acc) ->
                        dict:store(MsgName, length(MsgDef), Acc)
                end,
                dict:new(),
                gpb_lib:msgs_or_groups(Defs)).

find_msgsizes_known_at_compile_time(Defs) ->
    T = ets:new(gpb_msg_sizes, [set, public]),
    [find_msgsize(MsgName, Defs, T) || {{msg,MsgName},_Fields} <- Defs],
    Result = dict:from_list(ets:tab2list(T)),
    ets:delete(T),
    Result.

find_msgsize(MsgName, Defs, T) ->
    case ets:lookup(T, MsgName) of
        [] ->
            {{msg,MsgName}, Fields} = lists:keyfind({msg,MsgName}, 1, Defs),
            Result = find_msgsize_2(Fields, 0, Defs, T),
            ets:insert(T, {MsgName, Result}),
            Result;
        [{MsgName, Result}] ->
            Result
    end.

find_groupsize(GroupName, Defs, T) ->
    {{group,GroupName}, Fields} = lists:keyfind({group,GroupName}, 1, Defs),
    find_msgsize_2(Fields, 0, Defs, T).

find_msgsize_2([#gpb_oneof{} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{occurrence=repeated} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{occurrence=optional} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{occurrence=defaulty} | _], _AccSize, _Defs, _T) ->
    undefined;
find_msgsize_2([#?gpb_field{type=Type, fnum=FNum} | Rest], AccSize, Defs, T) ->
    FKeySize =
        case Type of
            {group, _} ->
                not_applicable;
            _ ->
                FKey = (FNum bsl 3) bor gpb:encode_wiretype(Type),
                byte_size(gpb:encode_varint(FKey))
        end,
    case Type of
        sint32   -> undefined;
        sint64   -> undefined;
        int32    -> undefined;
        int64    -> undefined;
        uint32   -> undefined;
        uint64   -> undefined;
        bool     -> find_msgsize_2(Rest, AccSize+FKeySize+1, Defs, T);
        {enum,EnumName} ->
            case all_enum_values_encode_to_same_size(EnumName, Defs) of
                {yes, ESize} ->
                    find_msgsize_2(Rest, AccSize+FKeySize+ESize, Defs, T);
                no ->
                    undefined
            end;
        fixed64  -> find_msgsize_2(Rest, AccSize+FKeySize+8, Defs, T);
        sfixed64 -> find_msgsize_2(Rest, AccSize+FKeySize+8, Defs, T);
        double   -> find_msgsize_2(Rest, AccSize+FKeySize+8, Defs, T);
        string   -> undefined;
        bytes    -> undefined;
        {msg,MsgName} ->
            case find_msgsize(MsgName, Defs, T) of
                MsgSize when is_integer(MsgSize) ->
                    SizeOfLength = byte_size(gpb:encode_varint(MsgSize)),
                    SubMsgFieldSize = FKeySize + SizeOfLength + MsgSize,
                    find_msgsize_2(Rest, AccSize + SubMsgFieldSize, Defs, T);
                undefined ->
                    undefined
            end;
        {group,GroupName} ->
            case find_groupsize(GroupName, Defs, T) of
                GroupSize when is_integer(GroupSize) ->
                    StartTag = (FNum bsl 3) + gpb:encode_wiretype(group_start),
                    EndTag   = (FNum bsl 3) + gpb:encode_wiretype(group_end),
                    SizeOfStartTag = byte_size(gpb:encode_varint(StartTag)),
                    SizeOfEndTag = byte_size(gpb:encode_varint(EndTag)),
                    GroupFieldSize = SizeOfStartTag + GroupSize + SizeOfEndTag,
                    find_msgsize_2(Rest, AccSize + GroupFieldSize, Defs, T);
                undefined ->
                    undefined
            end;
        fixed32  -> find_msgsize_2(Rest, AccSize+FKeySize+4, Defs, T);
        sfixed32 -> find_msgsize_2(Rest, AccSize+FKeySize+4, Defs, T);
        float    -> find_msgsize_2(Rest, AccSize+FKeySize+4, Defs, T)
    end;
find_msgsize_2([], AccSize, _Defs, _T) ->
    AccSize.


all_enum_values_encode_to_same_size(EnumName, Defs) ->
    {{enum,EnumName}, EnumDef} = lists:keyfind({enum,EnumName}, 1, Defs),
    EnumSizes = [begin
                     <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
                     byte_size(gpb:encode_varint(N))
                 end
                 || {_EnumSym, Value, _} <- EnumDef],
    case lists:usort(EnumSizes) of
        [Size] -> {yes, Size};
        _      -> no
    end.

compute_decode_field_pass_methods(Defs, Opts) ->
    lists:foldl(fun({_Type, Name, Fields}, D) ->
                        PassHow = d_field_pass_method(Name, Fields, Opts),
                        %% FIXME:GROUP: are all group+msg names unique?
                        dict:store(Name, PassHow, D)
                end,
                dict:new(),
                gpb_lib:msgs_or_groups(Defs)).

d_field_pass_method(MsgName, MsgDef, Opts) ->
    %% Allow overriding options, mainly intended for testing
    case proplists:get_value({field_pass_method,MsgName}, Opts) of
        undefined ->
            case proplists:get_value(field_pass_method, Opts) of
                undefined ->
                    d_field_pass_method(MsgDef);
                Method when Method==pass_as_record; Method==pass_as_params ->
                    Method
            end;
        Method when Method==pass_as_record; Method==pass_as_params ->
            Method
    end.

d_field_pass_method(MsgDef) ->
    %% Compute estimated costs:
    %% Either passing a message record, or pass the fields as parameters
    %% to the functions, one parameter for each field, then as the last
    %% operation, stuff all parameters into a record.
    %%
    %% There are different advantages and disadvantages:
    %% - Updating fields in a record means the vm will have to verify
    %%   that the term is a record (for each time a field is parsed/added)
    %% - Passing the fields eliminates the cost above, but for each
    %%   (non-tail-recursive) function call, the field-parameters will
    %%   be saved to the stack, then restored after the call.
    %%   Such function calls, are: call to unicode:characters_to_list
    %%   for strings, calls to parse sub messages or packed fields and
    %%   final top-level calls to lists:reverse for repeated fields.
    NF = length(MsgDef), %% num fields (awk-istic terminology)
    if NF >= 250 ->
            pass_as_record; %% Functions can take at most 255 arguments
       NF == 0 ->
            pass_as_params;
       true ->
            NumSubMsgFields = count_submsg_fields(MsgDef),
            NumMapFields = count_map_fields(MsgDef),
            NumGroupFields = count_group_fields(MsgDef),
            IsMsgDominatedBySubMsgsOrMaps =
                (NumSubMsgFields + NumMapFields + NumGroupFields) / NF > 0.5,
            if IsMsgDominatedBySubMsgsOrMaps, NF >= 100 ->
                    pass_as_record;
               true ->
                    pass_as_params
            end
    end.

count_submsg_fields(MsgDef) ->
    gpb_lib:fold_msgdef_fields(
      fun(#?gpb_field{type={msg,_}}, N) -> N+1;
         (#?gpb_field{}, N)             -> N
      end,
      0,
      MsgDef).

count_map_fields(MsgDef) ->
    gpb_lib:fold_msgdef_fields(
      fun(#?gpb_field{type={map,_,_}}, N) -> N+1;
         (#?gpb_field{}, N)               -> N
      end,
      0,
      MsgDef).

count_group_fields(MsgDef) ->
    gpb_lib:fold_msgdef_fields(
      fun(#?gpb_field{type={group,_}}, N) -> N+1;
         (#?gpb_field{}, N)               -> N
      end,
      0,
      MsgDef).

%% Returns: [{Path, [{Op, [Calls]}]}]
compute_translations(Defs, Opts) ->
    remove_empty_translations(
      remove_merge_translations_for_repeated_elements(
        decode_init_default_for_decode_for_p3_non_compounds(
          lists:foldl(
            fun({_Name, Dict}, Acc) ->
                    dict:merge(
                      fun(_Key, Ts1, Ts2) -> merge_transls(Ts1, Ts2) end,
                      Acc, Dict)
            end,
            dict:new(),
            [{map_translations, compute_map_translations(Defs, Opts)},
             {type_translations, compute_type_translations(Defs, Opts)},
             {field_translations, compute_field_translations(Defs, Opts)}]),
          Defs))).

dict_from_translation_list(PTransls) ->
    lists:foldl(
      fun({Path, Transls1}, D) ->
              case dict:find(Path, D) of
                  {ok, Transls2} ->
                      dict:store(Path, merge_transls(Transls1, Transls2), D);
                  error ->
                      dict:store(Path, merge_transls(Transls1, []), D)
              end
      end,
      dict:new(),
      PTransls).

merge_transls(Transls1, Transls2) ->
    dict:to_list(
      dict:merge(
        fun(encode, L1, L2)                   -> L2 ++ L1;
           (decode, L1, L2)                   -> L1 ++ L2;
           (decode_init_default, L1, L2)      -> L1 ++ L2;
           (decode_repeated_add_elem, L1, L2) -> L1 ++ L2;
           (decode_repeated_finalize, L1, L2) -> L1 ++ L2;
           (merge, L1, L2)                    -> L1 ++ L2;
           (verify, _L1, L2)                  -> [hd(L2)];
           (type_spec, _V1, V2)               -> V2
        end,
        dict:from_list([{Op,ensure_list(Op,Call)} || {Op,Call} <- Transls1]),
        dict:from_list([{Op,ensure_list(Op,Call)} || {Op,Call} <- Transls2]))).

ensure_list(_Op, L) when is_list(L) -> L;
ensure_list(type_spec, Elem)        -> Elem;
ensure_list(_Op, Elem)              -> [Elem].

%% Default-valued fields in proto3-messages are not included in the
%% encoded bytes over the wire. Yet, on decoding, any specified
%% translator for such fields must be called. To make that happen, Add a
%% `decode_init_default' translator that does what the `decode'
%% translator does.
%%
%% This applies to proto3 scalar fields and enums, bytes and strings.
%% This does not apply to:
%% * repeated (has a decode_init_default translator)
%% * oneof fields and sub message fields (which don't have any default value)
%% * proto2 message fields
decode_init_default_for_decode_for_p3_non_compounds(Transls, Defs) ->
    case dict:size(Transls) of
        0 ->
            %% Common enough case: Save work when no translations are specified.
            Transls;
        _ ->
            decode_init_default_for_decode_for_p3_non_compounds2(Transls, Defs)
    end.

decode_init_default_for_decode_for_p3_non_compounds2(Transls, Defs) ->
    P3MsgNames = case lists:keyfind(proto3_msgs, 1, Defs) of
                     {proto3_msgs, Names} -> sets:from_list(Names);
                     false                -> sets:new()
                 end,
    dict:map(
      fun([MsgName,FName], FTransls) ->
              case sets:is_element(MsgName, P3MsgNames)
                  andalso is_scalar_msg_field(MsgName, FName, Defs) of
                  true ->
                      case lists:keyfind(decode, 1, FTransls) of
                          {decode, DTransl} ->
                              DInitDefault = {decode_init_default, DTransl},
                              FTransls ++ [DInitDefault];
                          false ->
                              FTransls
                      end;
                  false ->
                      FTransls
              end;
         (_, FTransls) ->
              FTransls
      end,
      Transls).

is_scalar_msg_field(MsgName, FName, Defs) ->
    {{msg,MsgName}, Fields} = lists:keyfind({msg,MsgName}, 1, Defs),
    is_scalar_field(FName, Fields).

is_scalar_field(FName, [#?gpb_field{name=FName, type=Type,
                                    occurrence=Occurrence} | _]) ->
    if Occurrence == optional -> is_scalar_type(Type);
       Occurrence == defaulty -> is_scalar_type(Type);
       Occurrence /= optional -> false
    end;
is_scalar_field(FName, [#gpb_oneof{name=FName} | _]) ->
    false;
is_scalar_field(FName, [_Field | Rest]) ->
    is_scalar_field(FName, Rest).

remove_merge_translations_for_repeated_elements(D) ->
    dict:map(fun(Key, Ops) ->
                     case is_repeated_element_path(Key) of
                         true -> lists:keydelete(merge, 1, Ops);
                         false -> Ops
                     end
             end,
             D).

is_repeated_element_path([_, _, []]) -> true;
is_repeated_element_path(_) -> false.

remove_empty_translations(D) ->
    dict:filter(fun(_Key, Ops) -> Ops /= [] end, D).

compute_map_translations(Defs, Opts) ->
    MapInfos =
        gpb_lib:fold_msg_fields(
          fun(MsgName, #?gpb_field{name=FName, type={map,KType,VType}}, Acc) ->
                  [{{MsgName, FName}, {KType, VType}} | Acc];
             (_MsgName, _Field, Acc) ->
                  Acc
          end,
          [],
          Defs),
    MapFieldFmt = gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts),
    dict_from_translation_list(
      lists:append(
        [mk_map_transls(MsgName, FName, KeyType, ValueType, MapFieldFmt)
         || {{MsgName, FName}, {KeyType, ValueType}} <- MapInfos])).

mk_map_transls(MsgName, FName, KeyType, ValueType, '2tuples')->
    MapAsMsgName = gpb_lib:map_type_to_msg_name(KeyType, ValueType),
    AddItemTrFn = case ValueType of
                    {msg,_} -> mt_add_item_r_verify_value;
                    _       -> mt_add_item_r
                  end,
    [{[MsgName,FName,[]],
      [{encode, {mt_maptuple_to_pseudomsg_r, ['$1', MapAsMsgName]}}]},
     {[MsgName,FName],
      [{decode_init_default,      {mt_empty_map_r,       []}},
       {decode_repeated_add_elem, {AddItemTrFn,          ['$1', '$2']}},
       {decode_repeated_finalize, {mt_finalize_items_r,  ['$1']}},
       {merge,                    {mt_merge_maptuples_r, ['$1', '$2']}}]}];
mk_map_transls(MsgName, FName, _KeyType, ValueType, maps)->
    AddItemTrFn = case ValueType of
                    {msg,_} -> mt_add_item_m_verify_value;
                    _       -> mt_add_item_m
                  end,
    [{[MsgName,FName,[]],
      [{encode,                   {mt_maptuple_to_pseudomsg_m, ['$1']}}]},
     {[MsgName,FName],
      [{encode,                   {mt_map_to_list_m, ['$1']}},
       {decode_init_default,      {mt_empty_map_m,   []}},
       {decode_repeated_add_elem, {AddItemTrFn,      ['$1', '$2']}},
       {decode_repeated_finalize, {id,               ['$1', '$user_data']}},
       {merge,                    {mt_merge_maps_m,  ['$1', '$2']}}]}].

compute_type_translations(Defs, Opts) ->
    TypeTranslations =
        lists:foldl(fun({translate_type, {Type, Transls}}, Acc) ->
                            [{Type, Transls} | Acc];
                       (_Opt, Acc) ->
                            Acc
                    end,
                    [],
                    Opts),
    %% Traverse all message definitions only when there are translations
    if TypeTranslations == [] ->
            dict:new();
       true ->
            compute_type_translations_2(Defs, TypeTranslations)
    end.

compute_type_translations_2(Defs, TypeTranslations) ->
    Infos = compute_type_translation_infos(Defs, TypeTranslations),
    Infos2 = add_type_translation_infos_for_msgs(TypeTranslations, Infos),
    dict_from_translation_list(
      [begin
           Decode = case Type of
                        {map,_,_} -> decode_repeated_finalize;
                        _ -> decode
                    end,
           Ctxt = {type, Type},
           Trs = [{encode, fetch_op_transl(encode, Translations, Ctxt)},
                  {Decode, fetch_op_transl(Decode, Translations, Ctxt)},
                  {verify, fetch_op_transl(verify, Translations, Ctxt)}
                  | [{merge, fetch_op_transl(merge, Translations, Ctxt)}
                     || not is_repeated_elem_path(Path),
                        not is_scalar_type(Type)]]
               ++ type_spec_tr(Translations),
           {Path, Trs}
       end
       || {Type, Path, Translations} <- Infos2]).

is_scalar_type(int32)    -> true;
is_scalar_type(int64)    -> true;
is_scalar_type(uint32)   -> true;
is_scalar_type(uint64)   -> true;
is_scalar_type(sint32)   -> true;
is_scalar_type(sint64)   -> true;
is_scalar_type(fixed32)  -> true;
is_scalar_type(fixed64)  -> true;
is_scalar_type(sfixed32) -> true;
is_scalar_type(sfixed64) -> true;
is_scalar_type(bool)     -> true;
is_scalar_type(float)    -> true;
is_scalar_type(double)   -> true;
is_scalar_type({enum,_}) -> true;
is_scalar_type(string)   -> true;
is_scalar_type(bytes)    -> true;
is_scalar_type(_)        -> false. % not: msg | map | group

compute_type_translation_infos(Defs, TypeTranslations) ->
    gpb_lib:fold_msg_or_group_fields_o(
      fun(_MsgOrGroup,
          MsgName, #?gpb_field{name=FName, type=FType, occurrence=Occ},
          Oneof,
          Acc) when not ?is_map_type(FType) ->
              case lists:keyfind(FType, 1, TypeTranslations) of
                  {FType, Translations} ->
                      Path =
                          case {Oneof, Occ} of
                              {false, repeated}  -> [MsgName,FName,[]];
                              {false, _}         -> [MsgName,FName];
                              {{true,CFName}, _} -> [MsgName,CFName,FName]
                          end,
                      [{FType, Path, Translations} | Acc];
                  false ->
                      Acc
              end;
         (_MsgOrGroup,
          MsgName, #?gpb_field{name=FName, type={map,KeyType,ValueType}=FType},
          _Oneof,
          Acc) ->
              %% check for translation of the map<_,_> itself
              Path = [MsgName,FName],
              MapTransl = case lists:keyfind(FType, 1, TypeTranslations) of
                              {FType, Translations} ->
                                  Ts2 = map_translations_to_internal(
                                          Translations),
                                  [{FType, Path, Ts2}];
                              false ->
                                  []
                          end,
              MsgName2 = gpb_lib:map_type_to_msg_name(KeyType, ValueType),
              Fields2 = gpb:map_item_pseudo_fields(KeyType, ValueType),
              Defs2 = [{{msg, MsgName2}, Fields2}],
              MapTransl
                  ++ compute_type_translation_infos(Defs2, TypeTranslations)
                  ++ Acc;
         (_Type, _MsgName, _Field, _Oneof, Acc) ->
              Acc
      end,
      [],
      Defs).

map_translations_to_internal(Translations) ->
    %% Under the hood, a map<_,_> is a repeated, and it is implemented
    %% using translations, so adapt type translations accordingly
    [case Op of
         encode -> {encode, Tr};
         decode -> {decode_repeated_finalize, Tr};
         merge  -> {merge, Tr};
         verify -> {verify, Tr};
         _      -> {Op, Tr}
     end
     || {Op, Tr} <- Translations].

add_type_translation_infos_for_msgs(TypeTranslations, Infos) ->
    lists:foldl(
      fun({{msg,MsgName}=Type, Translations}, Acc) ->
              Path = [MsgName],
              [{Type, Path, Translations} | Acc];
         (_OtherTransl, Acc) ->
              Acc
      end,
      Infos,
      TypeTranslations).

compute_field_translations(Defs, Opts) ->
    dict_from_translation_list(
      lists:append(
        [[{Path, augment_field_translations(Field, Path, Translations, Opts)}
          || {translate_field, {[_,_|_]=Path, Translations}} <- Opts,
             Field <- find_field_by_path(Defs, Path)],
         [{Path, augment_msg_translations(MsgName, Translations)}
          || {translate_field, {[MsgName]=Path, Translations}} <- Opts,
             lists:keymember({msg,MsgName}, 1, Defs)]])).

find_field_by_path(Defs, [MsgName,FieldName|RestPath]) ->
    case find_2_msg(Defs, MsgName) of
        {ok, {{_,MsgName}, Fields}} ->
            case find_3_field(Fields, FieldName) of
                {ok, #gpb_oneof{fields=OFields}} when RestPath =/= [] ->
                    case find_3_field(OFields, hd(RestPath)) of
                        {ok, OField} ->
                            [OField];
                        error ->
                            []
                    end;
                {ok, Field} ->
                    [Field];
                error ->
                    []
            end;
        error ->
            []
    end.

find_2_msg([{{msg, Name},_}=M | _], Name)   -> {ok, M};
find_2_msg([{{group, Name},_}=M | _], Name) -> {ok, M};
find_2_msg([_ | Rest], Name)                -> find_2_msg(Rest, Name);
find_2_msg([], _Name)                       -> error.

find_3_field([#?gpb_field{name=Name}=F | _], Name) -> {ok, F};
find_3_field([#gpb_oneof{name=Name}=F | _], Name)  -> {ok, F};
find_3_field([_ | Rest], Name)                     -> find_3_field(Rest, Name);
find_3_field([], _Name)                            -> error.

augment_field_translations(#?gpb_field{type=Type, occurrence=Occ},
                           Path, Translations, Opts) ->
    IsRepeated =  Occ == repeated,
    IsElem = IsRepeated andalso lists:last(Path) == [],
    IsScalar = is_scalar_type(Type),
    DoNif = proplists:get_bool(nif, Opts),
    %% Operations for which we can or sometimes must have translations:
    Ops = if DoNif ->
                  [merge, verify];
             IsElem ->
                  [encode,decode,merge,verify];
             IsRepeated ->
                  [encode,
                   decode_init_default,
                   decode_repeated_add_elem,
                   decode_repeated_finalize,
                   merge,
                   verify];
             true ->
                  [encode,decode,merge,verify]
          end -- [merge || IsScalar, not IsRepeated],
    augment_2_fetch_transl(Ops, Translations, {field, Path})
        ++ type_spec_tr(Translations);
augment_field_translations(#gpb_oneof{}, [_,_]=Path, Translations, Opts) ->
    DoNif = proplists:get_bool(nif, Opts),
    %% Operations for which we can or sometimes must have translations:
    Ops = if DoNif ->
                  [verify];
             true ->
                  [encode,decode,verify]
          end,
    augment_2_fetch_transl(Ops, Translations, {field, Path})
        ++ type_spec_tr(Translations).

augment_2_fetch_transl(Ops, Translations, Ctxt) ->
    [{Op, fetch_op_transl(Op, Translations, Ctxt)} || Op <- Ops].

augment_msg_translations(MagName, Translations) ->
    Ops = [encode,decode,merge,verify],
    augment_2_fetch_transl(Ops, Translations, {msg,[MagName]})
        ++ type_spec_tr(Translations).

type_spec_tr(Translations) ->
    case lists:keyfind(type_spec, 1, Translations) of
        {type_spec, TypeStr} ->
            [{type_spec, TypeStr}];
        false ->
            [{type_spec, '$default'}]
    end.

fetch_op_transl(Op, Translations, Ctxt) ->
    Default = fetch_default_transl(Op),
    fetch_op_translation(Op, Translations, Default, Ctxt).

fetch_default_transl(Op) ->
    case Op of
        merge  -> gpb_gen_translators:default_merge_translator();
        verify -> gpb_gen_translators:default_verify_translator();
        _      -> '$no_default'
    end.

fetch_op_translation(Op, Translations, Default, Ctxt) ->
    case lists:keyfind(Op, 1, Translations) of
        false when Default /= '$no_default' ->
            Default;
        false when Default == '$no_default' ->
            error({error, {missing_translation,
                           {op,Op}, Ctxt,
                           Translations}});
        {Op, {M,F,ArgTempl}} ->
            {M,F,ArgTempl};
        {Op, {F,ArgTempl}} ->
            {F,ArgTempl}
    end.

is_repeated_elem_path([_MsgName,_FName,[]]) -> true;
is_repeated_elem_path(_) -> false.

find_group_occurrences(Defs) ->
    gpb_lib:fold_msg_or_group_fields_o(
      fun(_msg_or_group, _MsgName,
          #?gpb_field{type={group,GroupName}, occurrence=Occurrence},
          _IsOneof, D)->
              dict:store(GroupName, Occurrence, D);
         (_msg_or_group, _MsgName, _Field, _IsOneof, D) ->
              D
      end,
      dict:new(),
      Defs).

has_p3_opt_strings(Defs) ->
    P3Msgs = case lists:keyfind(proto3_msgs, 1, Defs) of
                 {proto3_msgs, Names} -> Names;
                 false                -> []
             end,
    try gpb_lib:fold_msg_or_group_fields_o(
          fun(_msg_or_group, MsgName, #?gpb_field{type=Type,occurrence=Occ},
              _IsOneOf, Acc) ->
                  %% Consider only top-level fields, not inside oneof,
                  %% since these are handled as if required eg during encoding.
                  %% However, for oneof fields, occurrence is always optional;
                  %% occurrence = defaulty can only occor for top-level fields.
                  if Type == string, Occ == defaulty ->
                          case lists:member(MsgName, P3Msgs) of
                              true -> throw(true);
                              false -> Acc
                          end;
                     true ->
                          Acc
                  end
          end,
          false,
          Defs)
    catch throw:true ->
            true
    end.
