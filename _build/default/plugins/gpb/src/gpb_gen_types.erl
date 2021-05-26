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

%%% @doc Generation of type specs and record definitinos.
%%% @private
-module(gpb_gen_types).

-export([format_msg_record/5]).
-export([format_maps_as_msgs_record_defs/1]).
-export([format_enum_typespec/3]).
-export([format_record_typespec/5]).
-export([format_export_types/3]).

-include("../include/gpb.hrl").
-include("gpb_compile.hrl").

format_msg_record(Msg, Fields, AnRes, Opts, Defs) ->
    Def = list_to_atom(gpb_lib:uppercase(lists:concat([Msg, "_PB_H"]))),
    [?f("-ifndef(~p).~n", [Def]),
     ?f("-define(~p, true).~n", [Def]),
     ?f("-record(~p,~n", [Msg]),
     ?f("        {"),
     gpb_lib:outdent_first(format_hfields(Msg, 8+1, Fields, AnRes, Opts, Defs)),
     "\n",
     ?f("        }).~n"),
     ?f("-endif.~n")].

format_maps_as_msgs_record_defs(MapsAsMsgs) ->
    [begin
         FNames = [atom_to_list(FName) || #?gpb_field{name=FName} <- Fields],
         ?f("-record(~p,{~s}).~n", [MsgName, gpb_lib:comma_join(FNames)])
     end
     || {{msg,MsgName},Fields} <- MapsAsMsgs].

format_export_types(Defs, AnRes, Opts) ->
    case gpb_lib:get_type_specs_by_opts(Opts) of
        false ->
            "";
        true ->
            iolist_to_binary(
              ["%% enumerated types\n",
               gpb_lib:nl_join([format_enum_typespec(Enum, Enumeration, AnRes)
                                || {{enum, Enum}, Enumeration} <- Defs]),
               "\n",
               ?f("-export_type([~s]).",
                  [gpb_lib:comma_join(["'"++atom_to_list(Enum)++"'/0"
                                       || {{enum, Enum}, _} <- Defs])]),
               "\n\n",
               "%% message types\n",
               gpb_lib:nl_join(
                 [format_record_typespec(Name, Fields, Defs, AnRes, Opts)
                  || {_, Name, Fields} <- gpb_lib:msgs_or_groups(Defs)]),
               "\n",
               ?f("-export_type([~s]).",
                  [gpb_lib:comma_join(
                     ["'"++atom_to_list(Name)++"'/0"
                      || {_, Name, _} <- gpb_lib:msgs_or_groups(Defs)])]),
               "\n"])
    end.

format_enum_typespec(Enum, Enumeration, AnRes) ->
    Enum1 = rename_enum_type(Enum, AnRes),
    Enumerators = gpb_lib:or_join([?f("~p", [EName])
                                   || {EName, _} <- Enumeration]),
    ?f("-type ~p() :: ~s.", [Enum1, Enumerators]).

format_record_typespec(Msg, Fields, Defs, AnRes, Opts) ->
    MsgType = rename_msg_type(Msg, AnRes),
    case gpb_lib:get_records_or_maps_by_opts(Opts) of
        records ->
            ?f("-type ~p() :: #~p{}.~n", [MsgType, Msg]);
        maps ->
            HFields = format_hfields(Msg, 7 + 1, Fields, AnRes, Opts, Defs),
            BType = calc_keytype_override(Fields, Opts),
            if BType == no_override ->
                    ?f("-type ~p() ::~n"
                       "      #{~s~n"
                       "       }.~n",
                       [MsgType, gpb_lib:outdent_first(HFields)]);
               true ->
                    ?f("-type ~p() ::~n"
                       "      #{~s~n" % all fields gets rendered as comments
                       "        ~s~n"
                       "       }.~n",
                       [MsgType, gpb_lib:outdent_first(HFields), BType])
            end
    end.

calc_keytype_override([], _Opts) ->
    no_override;
calc_keytype_override(Fields, Opts) ->
    case gpb_lib:get_maps_key_type_by_opts(Opts) of
        atom ->
            no_override;
        binary ->
            TypespecsCanIndicateMapItemPresence =
                gpb_lib:target_can_specify_map_item_presence_in_typespecs(Opts),
            HaveMandatoryFields =
                lists:any(fun(F) ->
                                  gpb_lib:get_field_occurrence(F) /= optional
                          end,
                          Fields),
            if TypespecsCanIndicateMapItemPresence, HaveMandatoryFields ->
                    "binary() := _";
               TypespecsCanIndicateMapItemPresence, not HaveMandatoryFields ->
                    "binary() => _";
               true ->
                    "binary() => _"
            end
    end.

format_hfields(MsgName, Indent, Fields, AnRes, Opts, Defs) ->
    IsProto3 = gpb:is_msg_proto3(MsgName, Defs),
    TypeSpecs = gpb_lib:get_type_specs_by_opts(Opts),
    MapsOrRecords = gpb_lib:get_records_or_maps_by_opts(Opts),
    MappingAndUnset = gpb_lib:get_mapping_and_unset_by_opts(Opts),
    TypespecsCanIndicateMapItemPresence =
        gpb_lib:target_can_specify_map_item_presence_in_typespecs(Opts),
    FTCs = calc_zipped_fields_and_type_str_comments(MsgName, Fields,
                                                    Defs, AnRes, Opts),
    {Fields1, _TypeStrsComment} = lists:unzip(FTCs),
    LastIndex = case MappingAndUnset of
                    records ->
                        length(Fields1);
                    #maps{unset_optional=present_undefined} ->
                        length(Fields1);
                    #maps{unset_optional=omitted} ->
                        if TypespecsCanIndicateMapItemPresence ->
                                length(Fields1); % do typespecs for all fields
                           true ->
                                find_last_nonopt_field_index(Fields1)
                        end
                end,
    MapTypeFieldsRepr = gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(
                          Opts),
    KeyType = gpb_lib:get_maps_key_type_by_opts(Opts),
    gpb_lib:nl_join(
      lists:map(
        fun({I, {#?gpb_field{name=Name, fnum=FNum, opts=FOpts, type=Type,
                             occurrence=Occur}=Field,
                 {TypeStr, TypeComment}}}) ->
                TypeSpecifierSep = calc_field_type_sep(Field, Opts),
                LineLead = case MappingAndUnset of
                               #maps{unset_optional=omitted} when
                                     Occur == optional,
                                     not TypespecsCanIndicateMapItemPresence ->
                                   "%% ";
                               #maps{} ->
                                   case KeyType of
                                       atom   -> "";
                                       binary -> "%% "
                                   end;
                               _ ->
                                   ""
                           end,
                DefaultStr =
                    case proplists:get_value(default, FOpts, '$no') of
                        '$no' ->
                            case {Type, Occur, MapsOrRecords} of
                                {{map,_,_}, repeated, records} ->
                                    case MapTypeFieldsRepr of
                                        maps ->
                                            ?f(" = #{}");
                                        '2tuples' ->
                                            ?f(" = []")
                                    end;
                                {_, repeated, records} ->
                                    ?f(" = []");
                                {_, _, records} ->
                                    case IsProto3 of
                                        true ->
                                            Default =
                                                gpb_lib:proto3_type_default(
                                                  Type,
                                                  Defs,
                                                  Opts),
                                            ?f(" = ~p", [Default]);
                                        false -> ""
                                    end;
                                _ ->
                                    ""
                            end;
                        Default ->
                            case MapsOrRecords of
                                records ->
                                    ?f(" = ~p", [Default]);
                                maps ->
                                    ""
                            end
                    end,
                CommaSep = if I < LastIndex -> ",";
                              true          -> "" %% last entry
                           end,
                FieldTxt0 = ?f("~s~w~s", [LineLead, Name, DefaultStr]),
                FieldTxt1 = gpb_lib:indent(Indent, FieldTxt0),
                FieldTxt2 = if TypeSpecs ->
                                    LineUp = lineup(iolist_size(FieldTxt1), 32),
                                    ?f("~s~s~s ~s~s", [FieldTxt1, LineUp,
                                                       TypeSpecifierSep,
                                                       TypeStr, CommaSep]);
                               not TypeSpecs ->
                                    ?f("~s~s", [FieldTxt1, CommaSep])
                            end,
                LineUpCol2 = if TypeSpecs -> 52;
                                not TypeSpecs -> 40
                             end,
                LineUpStr2 = lineup(iolist_size(FieldTxt2), LineUpCol2),
                ?f("~s~s% = ~w~s~s",
                   [FieldTxt2, LineUpStr2, FNum,
                    [", " || TypeComment /= ""], TypeComment]);
           ({I, {#gpb_oneof{name=Name}=Field, {TypeStr, TypeComment}}}) ->
                TypeSpecifierSep = calc_field_type_sep(Field, Opts),
                LineLead = case MappingAndUnset of
                               #maps{unset_optional=omitted} when
                                     not TypespecsCanIndicateMapItemPresence->
                                   "%% ";
                               #maps{} ->
                                   case KeyType of
                                       atom   -> "";
                                       binary -> "%% "
                                   end;
                               _ ->
                                   ""
                           end,
                CommaSep = if I < LastIndex -> ",";
                              true          -> "" %% last entry
                           end,
                FieldTxt0 = ?f("~s~w", [LineLead, Name]),
                FieldTxt1 = gpb_lib:indent(Indent, FieldTxt0),
                FieldTxt2 = if TypeSpecs ->
                                    LineUp = lineup(iolist_size(FieldTxt1), 32),
                                    ?f("~s~s~s ~s~s", [FieldTxt1, LineUp,
                                                       TypeSpecifierSep,
                                                       TypeStr, CommaSep]);
                               not TypeSpecs ->
                                    ?f("~s~s", [FieldTxt1, CommaSep])
                            end,
                LineUpCol2 = if TypeSpecs -> 52;
                                not TypeSpecs -> 40
                             end,
                LineUpStr2 = lineup(iolist_size(FieldTxt2), LineUpCol2),
                ?f("~s~s% ~s",
                   [FieldTxt2, LineUpStr2, TypeComment])
        end,
        gpb_lib:index_seq(FTCs))).

calc_zipped_fields_and_type_str_comments(MsgName, Fields, Defs, AnRes, Opts) ->
    TypeSpecs = gpb_lib:get_type_specs_by_opts(Opts),
    FTIs = [{Field, calc_type_str_infos(MsgName, Field, TypeSpecs,
                                        Defs, AnRes, Opts)}
            || Field <- Fields],
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        records ->
            [{Field, combine_type_str_comments(TI)}
             || {Field, TI} <- FTIs];
        #maps{oneof=tuples} ->
            [{Field, combine_type_str_comments(TI)}
             || {Field, TI} <- FTIs];
        #maps{oneof=flat} ->
            flatten_oneof_fields_and_type_strs_comments(FTIs)
    end.

find_last_nonopt_field_index(Fields) ->
    lists:foldl(fun({I, F}, Acc) ->
                        case gpb_lib:get_field_occurrence(F) of
                            required -> I;
                            repeated -> I;
                            optional -> Acc
                        end
                end,
                0,
                gpb_lib:index_seq(Fields)).

calc_field_type_sep(#?gpb_field{occurrence=Occurrence}, Opts) ->
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        records ->
            "::";
        #maps{unset_optional=present_undefined} ->
            mandatory_map_item_type_sep(Opts);
        #maps{unset_optional=omitted} ->
            case Occurrence of
                required -> mandatory_map_item_type_sep(Opts);
                repeated -> "=>";
                optional -> "=>"
            end
    end;
calc_field_type_sep(#gpb_oneof{}, Opts) ->
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        records   -> "::";
        #maps{} -> "=>"
    end.

mandatory_map_item_type_sep(Opts) ->
    %% With Erlang 19 we write #{n := integer()} to say that a
    %% map must contain a map item with key `n' and an integer value.
    %%
    %% With earlier Erlang versions, we can only write #{n => integer()}
    %% and we can never distinguish between map items that may or must
    %% be present.
    %%
    %% Ideally, we would want to know for which version of Erlang we're
    %% generating code.  For now, we assume the run-time version is the
    %% same as the compile-time version, which is not necessarily true.  For
    %% instance, we can generate code for maps even on pre-map Erlang R15.
    %%
    %% (At the time of this writing, the OTP_RELEASE pre-defined macro
    %% does not exist, but even if it had existed, it would have been of
    %% limited value because it would have linked the Erlang version at
    %% proto-encoding run-time with the Erlang version at compile-time
    %% of `gpb' not at compile-time of the .proto file.  In some
    %% scenario with a package manager, it might have a `gpb'
    %% pre-compiled with an old Erlang-version to be compatible with
    %% many environments.  Better to check version at run-time.)
    %%
    case gpb_lib:target_can_specify_map_item_presence_in_typespecs(Opts) of
        true  -> ":=";
        false -> "=>"
    end.

-record(field_type_str_info,
        {type_str :: string(),
         type_comment :: string()}).
-record(oneof_type_str_info,
        {type_strs :: [string()],
         type_comments :: [string()]}).

calc_type_str_infos(MsgName, #?gpb_field{}=Field, TypeSpecs,
                    Defs, AnRes, Opts) ->
    #field_type_str_info{
       type_str = field_type_str(MsgName, Field, Defs, AnRes, Opts),
       type_comment = field_type_comment(MsgName, Field, TypeSpecs, AnRes)};
calc_type_str_infos(MsgName, #gpb_oneof{}=Field, TypeSpecs,
                    Defs, AnRes, Opts) ->
    #oneof_type_str_info{
       type_strs = oneof_type_strs(MsgName, Field, Defs, AnRes, Opts),
       type_comments = oneof_type_comments(MsgName, Field, TypeSpecs, AnRes)}.

combine_type_str_comments(#field_type_str_info{type_str=TypeStr,
                                               type_comment=TypeComment}) ->
    {TypeStr, TypeComment};
combine_type_str_comments(#oneof_type_str_info{type_strs=TypeStrs}) ->
    {gpb_lib:or_join(TypeStrs), "oneof"}.

flatten_oneof_fields_and_type_strs_comments(Fields) ->
    flatten_oneof_ftcs(Fields).

flatten_oneof_ftcs([F | Rest]) ->
    case F of
        {#?gpb_field{}=Field,
         #field_type_str_info{type_str=TypeStr,
                              type_comment=TypeComment}} ->
            [{Field, {TypeStr, TypeComment}} | flatten_oneof_ftcs(Rest)];
        {#gpb_oneof{fields=OFields},
         #oneof_type_str_info{type_strs=TypeStrs,
                              type_comments=TypeComments}} ->
            %% TypeStrs can have a trailing "or undefined",
            %% so take as many TypeStrs elements as we have OFields
            TypeStrs1 = lists:sublist(TypeStrs, length(OFields)),
            FTIs1 = [{Field, {TypeStr, TypeComment}}
                     || {Field, TypeStr, TypeComment}
                            <- lists:zip3(OFields, TypeStrs1, TypeComments)],
            FTIs1 ++ flatten_oneof_ftcs(Rest)
    end;
flatten_oneof_ftcs([]) ->
    [].

field_type_str(MsgName,
               #?gpb_field{name=FName, type=Type, occurrence=Occurrence},
               Defs, AnRes, Opts) ->
    OrUndefined = case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                      records ->
                          " | undefined";
                      #maps{unset_optional=present_undefined} ->
                          " | undefined";
                      #maps{unset_optional=omitted} ->
                          ""
                  end,
    ElemPath = [MsgName, FName],
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, TypeStr} ->
            case Occurrence of
                required -> TypeStr;
                repeated -> TypeStr ++ OrUndefined;
                optional -> TypeStr ++ OrUndefined
            end;
        false ->
            TypeStr = type_to_typestr_2(Type, Defs, AnRes, Opts),
            case Occurrence of
                required ->
                    TypeStr;
                repeated ->
                    case Type of
                        {map,_,_} ->
                            TypeStr;
                        _ ->
                            RElemPath = [MsgName, FName, []],
                            case gpb_gen_translators:has_type_spec_translation(
                                   RElemPath, AnRes) of
                                {true, RTs} ->
                                    "[" ++ RTs ++ "]";
                                false ->
                                    "[" ++ TypeStr ++ "]"
                            end
                    end
                        ++ OrUndefined;
                optional ->
                    TypeStr ++ OrUndefined
            end
    end.

oneof_type_strs(MsgName,
               #gpb_oneof{name=FName, fields=OFields},
               Defs, AnRes, Opts) ->
    OrUndefinedElems = case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                           records ->
                               ["undefined"];
                           #maps{unset_optional=present_undefined} ->
                               ["undefined"];
                           #maps{unset_optional=omitted} ->
                               []
                       end,
    OrUndefinedStr = case OrUndefinedElems of
                         [] -> "";
                         [U] -> " | " ++ U
                     end,
    TagWrap = case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                  #maps{oneof=flat} ->
                      fun(_Tag, TypeStr) -> TypeStr end;
                  _ ->
                      fun(Tag, TypeStr) -> ?f("{~p, ~s}", [Tag, TypeStr]) end
              end,
    ElemPath = [MsgName, FName],
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, TypeStr} ->
            [TypeStr, OrUndefinedStr];
        false ->
            [begin
                 OElemPath = [MsgName, FName, Name],
                 case gpb_gen_translators:has_type_spec_translation(
                        OElemPath, AnRes) of
                     {true, TypeStr} ->
                         TagWrap(Name, TypeStr);
                     false ->
                         TypeStr = type_to_typestr_2(Type, Defs, AnRes, Opts),
                         TagWrap(Name, TypeStr)
                 end
             end
             || #?gpb_field{name=Name, type=Type} <- OFields]
                ++ OrUndefinedElems
    end.

type_to_typestr_2(sint32, _Defs, _AnRes, _Opts)   -> "integer()";
type_to_typestr_2(sint64, _Defs, _AnRes, _Opts)   -> "integer()";
type_to_typestr_2(int32, _Defs, _AnRes, _Opts)    -> "integer()";
type_to_typestr_2(int64, _Defs, _AnRes, _Opts)    -> "integer()";
type_to_typestr_2(uint32, _Defs, _AnRes, _Opts)   -> "non_neg_integer()";
type_to_typestr_2(uint64, _Defs, _AnRes, _Opts)   -> "non_neg_integer()";
type_to_typestr_2(bool, _Defs, _AnRes, _Opts)     -> "boolean() | 0 | 1";
type_to_typestr_2(fixed32, _Defs, _AnRes, _Opts)  -> "non_neg_integer()";
type_to_typestr_2(fixed64, _Defs, _AnRes, _Opts)  -> "non_neg_integer()";
type_to_typestr_2(sfixed32, _Defs, _AnRes, _Opts) -> "integer()";
type_to_typestr_2(sfixed64, _Defs, _AnRes, _Opts) -> "integer()";
type_to_typestr_2(float, _Defs, _AnRes, _Opts)    -> float_spec();
type_to_typestr_2(double, _Defs, _AnRes, _Opts)   -> float_spec();
type_to_typestr_2(string, _Defs, _AnRes, _Opts)   -> "iodata()";
type_to_typestr_2(bytes, _Defs, _AnRes, _Opts)    -> "iodata()";
type_to_typestr_2({enum,E}, Defs, _AnRes, Opts) ->
    enum_typestr(E, Defs, Opts);
type_to_typestr_2({msg,M}, _Defs, AnRes, Opts) ->
    msg_to_typestr(M, AnRes, Opts);
type_to_typestr_2({group,G}, _Defs, AnRes, Opts) ->
    msg_to_typestr(G, AnRes, Opts);
type_to_typestr_2({map,KT,VT}, Defs, AnRes, Opts) ->
    KTStr = type_to_typestr_2(KT, Defs, AnRes, Opts),
    VTStr = type_to_typestr_2(VT, Defs, AnRes, Opts),
    MapSep = mandatory_map_item_type_sep(Opts),
    case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) of
        '2tuples' -> ?f("[{~s, ~s}]", [KTStr, VTStr]);
        maps      -> ?f("#{~s ~s ~s}", [KTStr, MapSep, VTStr])
    end.

float_spec() ->
    "float() | integer() | infinity | '-infinity' | nan".

msg_to_typestr(M, AnRes, Opts) ->
    MsgType = rename_msg_type(M, AnRes),
    case gpb_lib:get_records_or_maps_by_opts(Opts) of
        records ->
            %% Prefix with module since records live in an hrl file
            Mod = proplists:get_value(module, Opts),
            ?f("~p:~p()", [Mod, MsgType]);
        maps ->
            ?f("~p()", [MsgType])
    end.

enum_typestr(E, Defs, Opts) ->
    UnknownEnums = case proplists:get_bool(nif, Opts) of
                       false -> " | integer()";
                       true  -> ""
                   end,
    {value, {{enum,E}, Enumerations}} = lists:keysearch({enum,E}, 1, Defs),
    gpb_lib:or_join(
      [?f("~p", [EName]) || {EName, _} <- Enumerations])
        ++ UnknownEnums.

field_type_comment(MsgName, #?gpb_field{name=FName}=Field, TypeSpec, AnRes) ->
    ElemPath = [MsgName, FName],
    field_type_comment_2(Field, ElemPath, TypeSpec, AnRes).

oneof_type_comments(MsgName, #gpb_oneof{name=FName, fields=OFields},
                    TypeSpec, AnRes) ->
    ElemPath = [MsgName, FName],
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, _} ->
            "";
        false ->
            [begin
                 OElemPath = [MsgName, FName, OFName],
                 field_type_comment_2(OField, OElemPath, TypeSpec, AnRes)
             end
             || #?gpb_field{name=OFName}=OField <- OFields]
    end.

field_type_comment_2(Field, ElemPath, TypeSpec, AnRes) ->
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, _} ->
            "";
        false ->
            field_type_comment_3(Field, TypeSpec)
    end.

field_type_comment_3(#?gpb_field{type=Type}, true=_TypeSpec) ->
    case Type of
        sint32   -> "32 bits";
        sint64   -> "64 bits";
        int32    -> "32 bits";
        int64    -> "64 bits";
        uint32   -> "32 bits";
        uint64   -> "64 bits";
        fixed32  -> "32 bits";
        fixed64  -> "64 bits";
        sfixed32 -> "32 bits";
        sfixed64 -> "64 bits";
        {enum,E} -> "enum "++atom_to_list(E);
        _        -> ""
    end;
field_type_comment_3(#?gpb_field{type=Type, occurrence=Occurrence}, false) ->
    case Occurrence of
        required -> ?f("~w", [Type]);
        repeated -> "[" ++ ?f("~w", [Type]) ++ "]";
        optional -> ?f("~w (optional)", [Type])
    end.

lineup(CurrentCol, TargetCol) when CurrentCol < TargetCol ->
    lists:duplicate(TargetCol - CurrentCol, $\s);
lineup(_, _) ->
    " ".

rename_enum_type(Name, #anres{renamings=Renamings}) ->
    gpb_names:apply_enum_type_renaming(Name, Renamings).

rename_msg_type(Name, #anres{renamings=Renamings}) ->
    gpb_names:apply_msg_type_renaming(Name, Renamings).
