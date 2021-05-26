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
-export([format_export_types/3]).

-include("../include/gpb.hrl").
-include("gpb_compile.hrl").

%% Collect some type spec info top-level wise instead
%% of needing check opts at many places.
-record(t_env,
        {type_specs :: boolean(),
         can_do_map_presence :: boolean(),
         mapping_and_unset :: records | #maps{},
         map_key_type :: atom | binary,
         map_type_fields :: '2tuples' | maps,
         module :: module(),
         nif :: boolean()}).

-record(type_text,
        {text :: string(),
         tag :: atom() | undefined}).

%% List of fields with some info/annotations to be rendered
%% either as record fields
%% or as map associations
-record(field_info, % order sort of from left to right
        {field :: #?gpb_field{} | #gpb_oneof{},
         elem_path :: [atom() | []]  % translation elem path
                    | undefined,     % initially
         out_comment :: boolean()  % if entire field must be out-commented
                      | undefined, % initially
         name :: string()   % field name, single-quoted if needed
               | undefined, % initially
         default :: string()   % for records
                  | undefined, % if no default
         type_sep :: string()   % "::" (records) or "=>" or ":=" (maps)
                   | undefined, % initially
         base_type_comment :: string()
                         | undefined, % if no base info
         type_text :: #type_text{}   % for #?gpb_field{} fields
                    | [#type_text{}] % for #gpb_oneof{} fields
                    | undefined,     % if no type specs
         or_undefined :: boolean() % whether " | undefined"
                       | undefined,
         comment_chunks :: [string()]}).

format_msg_record(Msg, Fields, AnRes, Opts, Defs) ->
    Def = list_to_atom(gpb_lib:uppercase(lists:concat([Msg, "_PB_H"]))),
    TEnv = t_env(Opts),
    [?f("-ifndef(~p).~n", [Def]),
     ?f("-define(~p, true).~n", [Def]),
     ?f("-record(~p,~n", [Msg]),
     ?f("        {"),
     gpb_lib:outdent_first(
       format_hfields(Msg, 8+1, Fields, AnRes, Opts, Defs, TEnv)),
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
    #t_env{type_specs=TypeSpecs} = TEnv = t_env(Opts),
    AllMsgsTypesText = format_all_msgs_types(Defs, AnRes, TEnv),
    if not TypeSpecs ->
            AllMsgsTypesText;
       TypeSpecs ->
            iolist_to_binary(
              ["%% enumerated types\n",
               gpb_lib:nl_join([format_enum_typespec(Enum, Enumeration, AnRes)
                                || {{enum, Enum}, Enumeration} <- Defs]),
               "\n",
               ?f("-export_type([~s]).",
                  [gpb_lib:comma_join([format_enum_export(Enum, AnRes)
                                       || {{enum, Enum}, _} <- Defs])]),
               "\n\n",
               "%% message types\n",
               gpb_lib:nl_join(
                 [format_record_typespec(Name, Fields, Defs, AnRes, Opts, TEnv)
                  || {_, Name, Fields} <- gpb_lib:msgs_or_groups(Defs)]),
               "\n",
               ?f("-export_type([~s]).",
                  [gpb_lib:comma_join(
                     ["'"++atom_to_list(Name)++"'/0"
                      || {_, Name, _} <- gpb_lib:msgs_or_groups(Defs)])]),
               "\n",
               AllMsgsTypesText])
    end.

format_all_msgs_types(Defs, AnRes,
                      #t_env{type_specs=TypeSpecs,
                             mapping_and_unset=MappingAndUnset}) ->
    MsgNames = gpb_lib:msg_names(Defs),
    StrNames = if MsgNames == [] ->
                       "none()";
                  MsgNames /= [] ->
                       gpb_lib:or_join([?f("~p", [Nm]) || Nm <- MsgNames])
               end,

    StrTypes =
        if MsgNames == [] ->
                "none()";
           not TypeSpecs,
           MappingAndUnset == records ->
                gpb_lib:or_join([?f("#~p{}", [Nm]) || Nm <- MsgNames]);
           not TypeSpecs,
           is_record(MappingAndUnset, maps) ->
                "map()";
           TypeSpecs ->
                TypeNames = [rename_msg_type(Name, AnRes) || Name <- MsgNames],
                gpb_lib:or_join([?f("~p()", [Nm]) || Nm <- TypeNames])
        end,

    %% Use "$" to so types won't collide with messages' types.
    MsgNamesType = ?f("-type '$msg_name'() :: ~s.~n", [StrNames]),
    MsgsType = ?f("-type '$msg'() :: ~s.~n", [StrTypes]),

    ExportTypes = "-export_type(['$msg_name'/0, '$msg'/0]).\n",

    if MsgNames == [],
       not TypeSpecs ->
            %% No type specs and no messages: no -spec for eg encode_msg
            %% will refer to this, so don't generate anything
            "";
       TypeSpecs ->
            [MsgNamesType,
             MsgsType,
             ExportTypes];
       not TypeSpecs ->
            %% The generated encode_msg (eg) will refer to this,
            %% but don't export the types.
            [MsgNamesType,
             MsgsType]
    end.

format_enum_typespec(Enum, Enumeration, AnRes) ->
    Enum1 = rename_enum_type(Enum, AnRes),
    Enumerators = gpb_lib:or_join([?f("~p", [EName])
                                   || {EName, _, _} <- Enumeration]),
    ?f("-type ~p() :: ~s.", [Enum1, Enumerators]).

format_enum_export(Enum, AnRes) ->
    Enum1 = rename_enum_type(Enum, AnRes),
    ?f("~p/0", [Enum1]).

format_record_typespec(Msg, Fields, Defs, AnRes, Opts,
                       #t_env{mapping_and_unset=MappingAndUnset}=TEnv) ->
    MsgType = rename_msg_type(Msg, AnRes),
    case MappingAndUnset of
        records ->
            ?f("-type ~p() :: #~p{}.~n", [MsgType, Msg]);
        #maps{} ->
            HFields = format_hfields(Msg, 7 + 1, Fields,
                                     AnRes, Opts, Defs, TEnv),
            BType = calc_keytype_override(Fields, TEnv),
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

t_env(Opts) ->
    TypeSpecs = gpb_lib:get_type_specs_by_opts(Opts),
    TypespecsCanIndicateMapItemPresence =
        gpb_lib:target_can_specify_map_item_presence_in_typespecs(Opts),
    MappingAndUnset = gpb_lib:get_mapping_and_unset_by_opts(Opts),
    KeyType = gpb_lib:get_maps_key_type_by_opts(Opts),
    MapTypeFieldsRepr = gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(
                          Opts),
    Mod = proplists:get_value(module, Opts),
    Nif = proplists:get_bool(nif, Opts),
    #t_env{type_specs = TypeSpecs,
           can_do_map_presence = TypespecsCanIndicateMapItemPresence,
           mapping_and_unset = MappingAndUnset,
           map_key_type = KeyType,
           map_type_fields = MapTypeFieldsRepr,
           module = Mod,
           nif = Nif}.

calc_keytype_override([], _TEnv) ->
    no_override;
calc_keytype_override(_Fields, TEnv) ->
    #t_env{map_key_type=KeyType,
           mapping_and_unset=#maps{unset_optional=UnsetOpt},
           can_do_map_presence=TypespecsCanIndicateMapItemPresence}=TEnv,
    case KeyType of
        atom ->
            no_override;
        binary ->
            if TypespecsCanIndicateMapItemPresence,
               UnsetOpt == present_undefined ->
                    "binary() := _";
               TypespecsCanIndicateMapItemPresence,
               UnsetOpt == omitted ->
                    "binary() => _";
               true ->
                    "binary() => _"
            end
    end.

format_hfields(MsgName, Indent, Fields, AnRes, Opts, Defs, TEnv) ->
    FieldInfos = analyze_field_infos(MsgName, Fields, AnRes, Opts, Defs, TEnv),
    RenderedFieldTexts = render_field_infos(FieldInfos, Indent),
    Sep = "\n" ++ gpb_lib:indent(Indent, ""),
    gpb_lib:string_join(RenderedFieldTexts, Sep).

%% --------------------------------------------------------------
%% Collect info needed later for rendering
%%
analyze_field_infos(MsgName, Fields, AnRes, Opts, Defs, TEnv) ->
    FieldInfos0 = [#field_info{field=Field,
                               comment_chunks = []}
                   || Field <- Fields],
    lists:foldl(
      fun(F, FieldInfos) -> F(FieldInfos) end,
      FieldInfos0,
      [fun(FIs) -> augment_elem_paths(FIs, MsgName) end,
       fun(FIs) -> maybe_expand_oneofs(FIs, TEnv) end,
       fun(FIs) -> add_base_type_comment(FIs, TEnv) end,
       fun(FIs) -> augment_type_texts(FIs, Defs, AnRes, TEnv) end,
       fun(FIs) -> apply_type_transforms(FIs, AnRes, TEnv) end,
       fun(FIs) -> augment_type_or_undefined(FIs, TEnv) end,
       fun(FIs) -> augment_field_name_strs(FIs) end,
       fun(FIs) -> augment_default_values(FIs, Opts, Defs, TEnv) end,
       fun(FIs) -> augment_type_sep(FIs, TEnv) end,
       fun(FIs) -> augment_out_commentation(FIs, TEnv) end,
       fun(FIs) -> augment_occurrence(FIs) end,
       fun(FIs) -> augment_field_number(FIs) end]).

-ifndef(NO_HAVE_MAPS).
%% For debugging: add fun dump_field_infos/1 after interesting steps
-compile({nowarn_unused_function, dump_field_infos/1}).
dump_field_infos(FieldInfos) ->
    io:format(user, "~nFIs=~p~n",
              [[maps:from_list(
                  lists:zip(record_info(fields, field_info),
                            tl(tuple_to_list(FI))))
                || FI <- FieldInfos]]),
    FieldInfos.
-endif. % -ifndef(NO_HAVE_MAPS).

%% Step:
%% Add elem paths
augment_elem_paths(FieldInfos, MsgName) ->
    [FI#field_info{elem_path=[MsgName,gpb_lib:get_field_name(Field)]}
     || #field_info{field=Field}=FI <- FieldInfos].

%% Step:
%% If maps with flat oneofs, expand those to #field_info{field = #?gpb_field{}}
%% elements, with adjusted `elem_path's (for any type spec translations).
%%
%% NB: This step may leave #field_info{field = #gpb_oneof{}} if no flat oneofs.
maybe_expand_oneofs(FieldInfos, TEnv) ->
    #t_env{mapping_and_unset=MappingAndUnset} = TEnv,
    case MappingAndUnset of
        records ->
            FieldInfos;
        #maps{oneof=flat} ->
            expand_oneofs(FieldInfos);
        #maps{} ->
            FieldInfos
    end.

expand_oneofs([F | Rest]) ->
    case F of
        #field_info{elem_path=BaseElemPath,
                    field=#gpb_oneof{fields=OFields}} ->
            OFieldInfos =
                [#field_info{elem_path = BaseElemPath ++ [OFName],
                             field = OField,
                             comment_chunks = []}
                 || #?gpb_field{name=OFName}=OField <- OFields],
            OFieldInfos ++ expand_oneofs(Rest);
        _ ->
            [F | expand_oneofs(Rest)]
    end;
expand_oneofs([]) ->
    [].

%% Step:
%% Add a base type-comment.
%% (will be removed later for any type-spec translations)
add_base_type_comment(FieldInfos, TEnv) ->
    [FI#field_info{base_type_comment = base_type_comment(Field, TEnv)}
     || #field_info{field=Field}=FI <- FieldInfos].

base_type_comment(#?gpb_field{type=Type}=Field, TEnv) ->
    #t_env{type_specs=TypeSpecs,
           can_do_map_presence=TypespecsCanIndicateMapItemPresence} = TEnv,
    IsMapTypeField = is_map_type_field(Field),
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
        _ -> if not TypeSpecs ->
                     ?f("~p", [Type]);
                not TypespecsCanIndicateMapItemPresence,
                IsMapTypeField ->
                     ?f("~p", [Type]);
                true ->
                     undefined
             end
    end;
base_type_comment(#gpb_oneof{}, _TEnv) ->
    "oneof".

%% Step:
%% Calculate type text or texts (for oneof not expanded).
%% The reason for having a list of texts for unexpanded oneofs,
%% is to be able to replace individual ones in case of type spec translations.
augment_type_texts(FieldInfos, _Defs, _AnRes, #t_env{type_specs=false}) ->
    FieldInfos;
augment_type_texts(FieldInfos, Defs, AnRes, TEnv) ->
    [FI#field_info{type_text = type_text(Field, Defs, AnRes, TEnv)}
     || #field_info{field=Field}=FI <- FieldInfos].

type_text(#?gpb_field{type=Type, occurrence=Occurrence}=Field,
          Defs, AnRes, TEnv) ->
    TypeStr = type_to_typestr(Type, Defs, AnRes, TEnv),
    IsMapTypeField = is_map_type_field(Field),
    case Occurrence of
        required ->
            #type_text{text = TypeStr};
        repeated when IsMapTypeField ->
            #type_text{text = TypeStr};
        repeated ->
            #type_text{text = "[" ++ TypeStr ++ "]"};
        optional ->
            #type_text{text = TypeStr};
        defaulty ->
            #type_text{text = TypeStr}
    end;
type_text(#gpb_oneof{fields=OFields}, Defs, AnRes, TEnv) ->
    [#type_text{tag=OFName,
                text=type_to_typestr(OFType, Defs, AnRes, TEnv)}
     || #?gpb_field{name=OFName, type=OFType} <- OFields].

%% Step:
%% Apply any type spec translations
apply_type_transforms(FieldInfos, _AnRes, #t_env{type_specs=false}) ->
    FieldInfos;
apply_type_transforms(FieldInfos, AnRes, _TEnv) ->
    [case has_type_spec_translation(Field, ElemPath, TypeText0, AnRes) of
         {true, TypeStr} ->
             FI#field_info{type_text = TypeStr, base_type_comment = undefined};
         {partly, TypeStrs} ->
             FI#field_info{type_text = TypeStrs}; % retain "oneof" comment
         false ->
             FI
     end
     || #field_info{field=Field,
                    elem_path=ElemPath,
                    type_text=TypeText0}=FI <- FieldInfos].

has_type_spec_translation(#?gpb_field{occurrence=Occurrence}=Field,
                          ElemPath, _TypeText0, AnRes) ->
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, TypeStr} ->
            {true, #type_text{text = TypeStr}};
        false ->
            IsMapTypeField = is_map_type_field(Field),
            case Occurrence of
                required ->
                    false;
                repeated when IsMapTypeField ->
                    false;
                repeated ->
                    RElemPath = ElemPath ++ [[]],
                    case gpb_gen_translators:has_type_spec_translation(
                           RElemPath, AnRes) of
                        {true, RTypeStr} ->
                            {true, #type_text{text = "[" ++ RTypeStr ++ "]"}};
                        false ->
                            false
                    end;
                optional ->
                    false;
                defaulty ->
                    false
            end
    end;
has_type_spec_translation(#gpb_oneof{}, ElemPath, TypeTexts0, AnRes) ->
    case gpb_gen_translators:has_type_spec_translation(ElemPath, AnRes) of
        {true, TypeStr} ->
            {true, #type_text{text = TypeStr}};
        false ->
            {partly,
             lists:map(
               fun(#type_text{tag=OFName}=TT0) ->
                       OFElemPath = ElemPath ++ [OFName],
                       case gpb_gen_translators:has_type_spec_translation(
                              OFElemPath, AnRes) of
                           {true, TypeStr} ->
                               TT0#type_text{text=TypeStr};
                           false ->
                               TT0
                       end
               end,
               TypeTexts0)}
    end.

%% Step:
%% Add information in " | undefined" to maybe later be added to each type
augment_type_or_undefined(FieldInfos, TEnv) ->
    #t_env{mapping_and_unset=MappingAndUnset} = TEnv,
    OrUndefined = case MappingAndUnset of
                      records ->
                          true;
                      #maps{unset_optional=present_undefined} ->
                          true;
                      #maps{unset_optional=omitted} ->
                          false
                  end,
    [FI#field_info{or_undefined = OrUndefined}
     || #field_info{}=FI <- FieldInfos].

%% Step:
%% Set the 'name' field
augment_field_name_strs(FieldInfos) ->
    [FI#field_info{name = ?f("~p", [gpb_lib:get_field_name(Field)])}
     || #field_info{field=Field}=FI <- FieldInfos].

%% Step:
%% Set field default values (for use when generating records)
augment_default_values(FieldInfos, Opts, Defs, TEnv) ->
    #t_env{mapping_and_unset = MappingAndUnset} = TEnv,
    case MappingAndUnset of
        records ->
            [case Field of
                 #?gpb_field{}=Field ->
                     Default = record_field_default(Field, Opts, Defs, TEnv),
                     FI#field_info{default = Default};
                 #gpb_oneof{} ->
                     FI
             end
             || #field_info{field=Field}=FI <- FieldInfos];
        #maps{} ->
            FieldInfos
    end.

record_field_default(#?gpb_field{type=Type,
                                 occurrence=Occurence,
                                 opts=FOpts}=Field,
                     Opts, Defs, TEnv) ->
    #t_env{map_type_fields = MapTypeFieldsRepr} = TEnv,
    case proplists:get_value(default, FOpts, '$no') of
        '$no' ->
            IsMapTypeField = is_map_type_field(Field),
            if IsMapTypeField, MapTypeFieldsRepr == maps ->
                    "#{}";
               IsMapTypeField, MapTypeFieldsRepr == '2tuples' ->
                    "[]";
               Occurence == repeated ->
                    "[]";
               Occurence == defaulty ->
                    Default = gpb_lib:proto3_type_default(Type, Defs, Opts),
                    ?f("~p", [Default]);
               true ->
                    undefined
            end;
        Default ->
            ?f("~p", [Default])
    end.

%% Step:
%% Set the type separator to "::" (records) or "=>" or ":=" (for maps).
augment_type_sep(FieldInfos, TEnv) ->
    [FI#field_info{type_sep = calc_field_type_sep(Field, TEnv)}
     || #field_info{field=Field}=FI <- FieldInfos].

%% Step:
%% Set whether the field needs to be out-commented.
augment_out_commentation(FieldInfos, TEnv) ->
    #t_env{can_do_map_presence = TypespecsCanIndicateMapItemPresence,
           mapping_and_unset = MappingAndUnset,
           map_key_type = KeyType} = TEnv,
    case MappingAndUnset of
        records ->
            [FI#field_info{out_comment = false}
             || FI <- FieldInfos];
        #maps{} when KeyType == binary ->
            %% Unconveniently enough, this is not allowed:
            %%    #{<<"abc">> => some_type()}
            %% since <<"abc">> is not a valid type spec
            %% Only <<>>, <<_:M>>, <<_:_*N>> and a few more are.
            %% So out-comment entries
            [FI#field_info{out_comment = true}
             || FI <- FieldInfos];
        #maps{unset_optional=present_undefined} ->
            [FI#field_info{out_comment = false}
             || FI <- FieldInfos];
        #maps{unset_optional=omitted} ->
            %% If Erlang 17 or 18, treat also required as optional,
            %% since we cannot presence on decoding.
            OutComment = not TypespecsCanIndicateMapItemPresence,
            [FI#field_info{out_comment = OutComment}
             || FI <- FieldInfos]
    end.

%% Step:
%% Prepend the fields occurrence as a comment.
augment_occurrence(FieldInfos) ->
    [case Field of
         #?gpb_field{occurrence=Occurrence} ->
            IsMapTypeField = is_map_type_field(Field),
             Chunks2 = if IsMapTypeField -> Chunks;
                          Occurrence == required -> ["required" | Chunks];
                          Occurrence == optional -> ["optional" | Chunks];
                          Occurrence == defaulty -> ["optional" | Chunks];
                          Occurrence == repeated -> ["repeated" | Chunks]
                       end,
             FI#field_info{comment_chunks = Chunks2};
         #gpb_oneof{} ->
             FI#field_info{comment_chunks = Chunks}
     end
     || #field_info{field=Field, comment_chunks=Chunks}=FI <- FieldInfos].

%% Step:
%% Prepend the field's number (from the .proto) as a comment
augment_field_number(FieldInfos) ->
    [case Field of
         #?gpb_field{fnum=FNum} ->
             Text = ?f("= ~p", [FNum]),
             FI#field_info{comment_chunks = [Text | Chunks]};
         #gpb_oneof{} ->
             FI#field_info{comment_chunks = Chunks}
     end
     || #field_info{field=Field, comment_chunks=Chunks}=FI <- FieldInfos].

%% --------------------------------------------------------------
%% Render the fields to text
%% Each line is later to be indented `Indent' spaces. (This amount
%% is here used for lineup calculations.)
%%
%% Return a list (one per field) of iolists.
%%
render_field_infos(FieldInfos, Indent) ->
    IndexedFieldInfos = gpb_lib:index_seq(FieldInfos),
    LastIndex = find_last_non_out_commented_field(IndexedFieldInfos),
    lists:map(
      fun({I, #field_info{out_comment=OutCommented,
                          name=FName,
                          default=Default,
                          type_sep=TypeSep,
                          type_text=TypeText,
                          or_undefined=OrUndefined,
                          comment_chunks=CommentChunks,
                          base_type_comment=BaseTypeComment}}) ->
              Type = render_type_text(TypeText, OrUndefined),
              LineLead = if OutCommented     -> "%% ";
                            not OutCommented -> ""
                         end,
              DefaultStr = if Default == undefined -> "";
                              true -> ?f(" = ~s", [Default])
                           end,
              CommaSep = if OutCommented -> "";
                            not OutCommented ->
                                 case has_next(I, LastIndex) of
                                     true  -> ",";
                                     false -> ""
                                 end
                         end,
              Comment = render_comment(CommentChunks, BaseTypeComment),
              FieldTxt1 = ?f("~s~s~s", [LineLead, FName, DefaultStr]),
              FieldTxt2 = if Type /= undefined ->
                                  %% with type specs
                                  LineUp = lineup(Indent, FieldTxt1, 32),
                                  ?f("~s~s~s ~s~s", [FieldTxt1, LineUp,
                                                     TypeSep, Type, CommaSep]);
                             Type == undefined ->
                                  %% no type specs
                                  ?f("~s~s", [FieldTxt1, CommaSep])
                          end,
              LineUpCol2 = if Type /= undefined -> 52;
                              Type == undefined -> 40
                           end,
              LineUpStr2 = lineup(Indent, FieldTxt2, LineUpCol2),
              CommentStr = if Comment == "" -> "";
                              Comment /= "" -> "% " ++ Comment
                           end,
              ?f("~s~s~s", [FieldTxt2, LineUpStr2, CommentStr])
      end,
      IndexedFieldInfos).

find_last_non_out_commented_field(IndexedFieldInfos) ->
    lists:foldr(
      fun(_, Last) when is_integer(Last) ->
              Last;
         ({_, #field_info{out_comment=true}}, Acc) ->
              Acc;
         ({I, #field_info{out_comment=false}}, _Acc) ->
              I
      end,
      undefined,
      IndexedFieldInfos).

has_next(I, Last) ->
    if is_integer(Last) ->
            %% Index of last non-outcommented item
            I < Last;
       Last == undefined ->
            %% All items are out-com
            false
    end.

render_type_text(#type_text{text=TypeStr}, OrUndefined) ->
    if OrUndefined     -> TypeStr ++ " | undefined";
       not OrUndefined -> TypeStr
    end;
render_type_text(TypeTexts, OrUndefined) when is_list(TypeTexts) -> % oneof
    gpb_lib:or_join([?ff("{~p, ~s}", [Tag, TypeStr])
                     || #type_text{tag=Tag, text=TypeStr} <- TypeTexts]
                    ++ ["undefined" || OrUndefined]);
render_type_text(undefined, _) -> % no type specs
    undefined.


render_comment(CommentChunks, BaseTypeComment) ->
    if BaseTypeComment == undefined ->
            gpb_lib:comma_join(CommentChunks);
       is_list(BaseTypeComment) ->
            gpb_lib:comma_join(CommentChunks ++ [BaseTypeComment])
    end.

%% --------------------------------------------------------------
%% Helpers...

calc_field_type_sep(#?gpb_field{},
                    #t_env{mapping_and_unset=MappingAndUnset}=TEnv) ->
    case MappingAndUnset of
        records ->
            "::";
        #maps{unset_optional=present_undefined} ->
            mandatory_map_item_type_sep(TEnv);
        #maps{unset_optional=omitted} ->
            %% Even for required (proto2) fields, we use "=>", since we
            %% cannot guarantee that we will always decode to a map with
            %% all required fields always set, since it depends on the
            %% input binary.
            "=>"
    end;
calc_field_type_sep(#gpb_oneof{}, #t_env{mapping_and_unset=MappingAndUnset}) ->
    case MappingAndUnset of
        records -> "::";
        #maps{} -> "=>"
    end.

mandatory_map_item_type_sep(TEnv) ->
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
    #t_env{can_do_map_presence = TypespecsCanIndicateMapItemPresence} = TEnv,
    case TypespecsCanIndicateMapItemPresence of
        true  -> ":=";
        false -> "=>"
    end.

type_to_typestr(sint32, _Defs, _AnRes, _TEnv)   -> "integer()";
type_to_typestr(sint64, _Defs, _AnRes, _TEnv)   -> "integer()";
type_to_typestr(int32, _Defs, _AnRes, _TEnv)    -> "integer()";
type_to_typestr(int64, _Defs, _AnRes, _TEnv)    -> "integer()";
type_to_typestr(uint32, _Defs, _AnRes, _TEnv)   -> "non_neg_integer()";
type_to_typestr(uint64, _Defs, _AnRes, _TEnv)   -> "non_neg_integer()";
type_to_typestr(bool, _Defs, _AnRes, _TEnv)     -> "boolean() | 0 | 1";
type_to_typestr(fixed32, _Defs, _AnRes, _TEnv)  -> "non_neg_integer()";
type_to_typestr(fixed64, _Defs, _AnRes, _TEnv)  -> "non_neg_integer()";
type_to_typestr(sfixed32, _Defs, _AnRes, _TEnv) -> "integer()";
type_to_typestr(sfixed64, _Defs, _AnRes, _TEnv) -> "integer()";
type_to_typestr(float, _Defs, _AnRes, _TEnv)    -> float_spec();
type_to_typestr(double, _Defs, _AnRes, _TEnv)   -> float_spec();
type_to_typestr(string, _Defs, _AnRes, _TEnv)   -> "unicode:chardata()";
type_to_typestr(bytes, _Defs, _AnRes, _TEnv)    -> "iodata()";
type_to_typestr({enum,E}, Defs, _AnRes, TEnv) ->
    enum_typestr(E, Defs, TEnv);
type_to_typestr({msg,M}, _Defs, AnRes, TEnv) ->
    msg_to_typestr(M, AnRes, TEnv);
type_to_typestr({group,G}, _Defs, AnRes, TEnv) ->
    msg_to_typestr(G, AnRes, TEnv);
type_to_typestr({map,KT,VT}, Defs, AnRes, TEnv) ->
    #t_env{map_type_fields=MapTypeFieldsRepr,
           can_do_map_presence=TypespecsCanIndicateMapItemPresence} = TEnv,
    KTStr = type_to_typestr(KT, Defs, AnRes, TEnv),
    VTStr = type_to_typestr(VT, Defs, AnRes, TEnv),
    case {MapTypeFieldsRepr, TypespecsCanIndicateMapItemPresence} of
        {'2tuples', _} -> ?f("[{~s, ~s}]", [KTStr, VTStr]);
        {maps, true}   -> ?f("#{~s => ~s}", [KTStr, VTStr]); % map can be empty
        {maps, false}  -> "#{}" % map can be empty
    end;
type_to_typestr(unknown, _Defs, _AnRes, _TEnv) ->
    "term()".

float_spec() ->
    "float() | integer() | infinity | '-infinity' | nan".

msg_to_typestr(M, AnRes, TEnv) ->
    MsgType = rename_msg_type(M, AnRes),
    #t_env{mapping_and_unset=MappingAndUnset,
           module = Mod} = TEnv,
    case MappingAndUnset of
        records ->
            %% Prefix with module since records live in an hrl file
            ?f("~p:~p()", [Mod, MsgType]);
        #maps{} ->
            ?f("~p()", [MsgType])
    end.

enum_typestr(E, Defs, #t_env{nif=Nif}) ->
    UnknownEnums = if Nif  -> "";
                      true -> " | integer()"
                   end,
    {value, {{enum,E}, Enumerations}} = lists:keysearch({enum,E}, 1, Defs),
    gpb_lib:or_join(
      [?f("~p", [EName]) || {EName, _, _} <- Enumerations])
        ++ UnknownEnums.

lineup(BaseIndent, Text, TargetCol) ->
    CurrentCol = BaseIndent + iolist_size(Text),
    if CurrentCol < TargetCol ->
            lists:duplicate(TargetCol - CurrentCol, $\s);
       true ->
            " "
    end.

is_map_type_field(#?gpb_field{type={map,_,_}, occurrence=repeated}) -> true;
is_map_type_field(_) -> false.

rename_enum_type(Name, #anres{renamings=Renamings}) ->
    gpb_names:apply_enum_type_renaming(Name, Renamings).

rename_msg_type(Name, #anres{renamings=Renamings}) ->
    gpb_names:apply_msg_type_renaming(Name, Renamings).
