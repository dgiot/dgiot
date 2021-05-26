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

%%% @doc Helper functions for the code-generator module
%%% @private

-module(gpb_lib).

-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").
-include("../include/gpb.hrl").

-export([mk_fn/2, mk_fn/3]).
-export([replace_term/2]).
-export([replace_tree/2]).
-export([splice_trees/2]).
-export([repeat_clauses/2]).

-export([msgs_or_groups/1]).
-export([msg_or_group_names/1]).
-export([msg_names/1]).
-export([enum_names/1]).
-export([contains_messages/1]).
-export([get_field_name/1, get_field_names/1]).
-export([get_field_rnum/1]).
-export([get_field_occurrence/1]).
-export([map_type_to_msg_name/2]).
-export([unalias_enum/1]).
-export([zip_for_non_opt_fields/2]).
-export([any_field_is_sub_msg/1]).
-export([any_field_is_repeated/1]).
-export([any_enum_field_exists/1]).
-export([any_packed_field_exists/1]).
-export([at_least_one_submsg_with_size_not_known_at_compile_time_exists/1]).
-export([get_field_pass/2]).
-export([get_num_fields/2]).
-export([is_packed/1]).
-export([key_partition_on_optionality/2, key_partition_on_optionality/3]).
-export([classify_field_merge_action/1]).
-export([flatten_oneof_fields/1]).
-export([get_field_json_name/1]).

-export([fold_msg_fields/3]).
-export([fold_msg_or_group_fields/3]).
-export([fold_msg_or_group_fields_o/3]).
-export([fold_msgdef_fields/3]).
-export([fold_msgdef_fields_o/3]).

-export([mapping_match/3]).
-export([mapping_create/3]).
-export([mapping_create/4]).
-export([mapping_update/4]).
-export([record_match/2]).
-export([record_create/2]).
-export([record_update/3]).
-export([map_match/2]).
-export([map_create/2]).
-export([map_set/3]).

-export([get_2tuples_or_maps_for_maptype_fields_by_opts/1]).
-export([get_records_or_maps_by_opts/1]).
-export([get_mapping_and_unset_by_opts/1]).
-export([get_strings_as_binaries_by_opts/1]).
-export([get_type_specs_by_opts/1]).
-export([get_gen_descriptor_by_opts/1]).
-export([get_field_format_by_opts/1]).
-export([mk_get_defs_as_maps_or_records_fn/1]).
-export([get_defs_as_maps_or_records/1]).
-export([get_epb_functions_by_opts/1]).
-export([get_bypass_wrappers_by_opts/1]).
-export([is_target_major_version_at_least/2]).
-export([target_has_lists_join/1]).
-export([target_has_variable_key_map_update/1]).
-export([target_can_specify_map_item_presence_in_typespecs/1]).
-export([target_can_do_flat_oneof_for_maps/1]).
-export([target_may_fail_compilation_for_flat_oneof_for_maps/1]).
-export([target_has_stacktrace_syntax/1]).
-export([target_has_map_iterators/1]).
-export([current_otp_release/0]).
-export([proto2_type_default/3]).
-export([proto3_type_default/3]).
-export([get_maps_key_type_by_opts/1]).
-export([json_by_opts/1]).
-export([json_object_format_by_opts/1]).
-export([json_key_format_by_opts/1]).
-export([json_array_format_by_opts/1]).
-export([json_string_format_by_opts/1]).
-export([json_null/1]).
-export([get_gen_mergers/1]).
-export([get_gen_introspect/1]).

-export([var_f_n/1]).
-export([var_b_n/1]).
-export([var_j_n/1]).
-export([var_n/2]).
-export([var/2]).
-export([prefix_var/2]).
-export([assign_to_var/2]).
-export([match_bind_var/2]).
-export([varint_to_binary_fields/1]).
-export([do_exprs/3]).

-export([index_seq/1]).
-export([smember/2, smember_any/2]).
-export([indent/2, indent_lines/2]).
-export([outdent_first/1]).
-export([split_indent_iolist/2]).
-export([split_indent_butfirst_iolist/2]).
-export([cond_split_indent_iolist/3]).
-export([iolist_to_utf8_or_escaped_binary/2]).
-export([nowarn_unused_function/2]).
-export([nowarn_dialyzer_attr/3]).

-export([drop_filename_ext/1]).
-export([copy_filename_ext/2]).
-export([basenameify_ish/1]).

-export([comma_join/1]).
-export([nl_join/1]).
-export([or_join/1]).
-export([dot_join/1]).
-export([is_substr/2]).
-export([string_slice/2]).
-export([string_lexemes/2]).
-export([lowercase/1]).
-export([uppercase/1]).
-export([snake_case/1]).
-export([camel_case/1]).
-export([lower_camel_case/1]).

-include("../include/gpb.hrl").


mk_fn(Prefix, Suffix) ->
    list_to_atom(lists:concat([Prefix, Suffix])).

mk_fn(Prefix, Middlefix, Suffix) when is_integer(Middlefix) ->
    mk_fn(Prefix, list_to_atom(integer_to_list(Middlefix)), Suffix);
mk_fn(Prefix, Middlefix, Suffix) ->
    list_to_atom(lists:concat([Prefix, Middlefix, "_", Suffix])).

%% Helpers for gpb_codegen parse tree transform operations -----------
replace_term(Marker, NewTerm) when is_atom(Marker) ->
    {replace_term, Marker, NewTerm}.

replace_tree(Marker, NewTree) when is_atom(Marker) ->
    {replace_tree, Marker, NewTree}.

splice_trees(Marker, Trees) when is_atom(Marker) ->
    {splice_trees, Marker, Trees}.

repeat_clauses(Marker, RepetitionReplacements) ->
    {repeat_clauses, Marker, RepetitionReplacements}.

%% Various accessors -----

msgs_or_groups(Defs) ->
    [{Type,Name,Fields} || {{Type,Name},Fields} <- Defs,
                           Type =:= msg orelse Type =:= group].

msg_or_group_names(Defs) ->
    [Name || {_Type, Name, _Fields} <- msgs_or_groups(Defs)].

msg_names(Defs) ->
    [Name || {{msg, Name}, _Fields} <- Defs].

enum_names(Defs) ->
    [Name || {{enum, Name}, _Syms} <- Defs].

contains_messages(Defs) ->
    lists:any(fun({{msg, _}, _}) -> true;
                 (_)             -> false
              end,
              Defs).

get_field_names(MsgDef) ->
    [get_field_name(Field) || Field <- MsgDef].

get_field_name(#?gpb_field{name=FName}) -> FName;
get_field_name(#gpb_oneof{name=FName})  -> FName.

get_field_rnum(#?gpb_field{rnum=RNum}) -> RNum;
get_field_rnum(#gpb_oneof{rnum=RNum})  -> RNum.

get_field_occurrence(#?gpb_field{occurrence=Occurrence}) -> Occurrence;
get_field_occurrence(#gpb_oneof{})                       -> optional.

map_type_to_msg_name(KeyType, {msg,MsgName}) ->
    list_to_atom(?ff("map<~s,~s>", [KeyType, MsgName]));
map_type_to_msg_name(KeyType, {enum,EnumName}) ->
    list_to_atom(?ff("map<~s,~s>", [KeyType, EnumName]));
map_type_to_msg_name(KeyType, ValueType) ->
    list_to_atom(?ff("map<~s,~s>", [KeyType, ValueType])).

%% The "option allow_alias = true;" inside an enum X { ... }
%% says it is ok to have multiple symbols that map to the same numeric value.
%% Appeared in protobuf 2.5.0.
unalias_enum([{_Sym,Value}=Enum | Rest]) ->
    [Enum | unalias_enum([E || {_,V}=E <- Rest, V /= Value])];
unalias_enum([{option,_Name,_Value} | Rest]) ->
    unalias_enum(Rest);
unalias_enum([]) ->
    [].

zip_for_non_opt_fields([#?gpb_field{name=FName,
                                    occurrence=Occurrence} | FRest],
                       [Elem | ERest]) ->
    case Occurrence of
        optional -> zip_for_non_opt_fields(FRest, ERest);
        required -> [{FName, Elem} | zip_for_non_opt_fields(FRest, ERest)];
        repeated -> zip_for_non_opt_fields(FRest, ERest)
    end;
zip_for_non_opt_fields([#gpb_oneof{} | FRest], [_Elem | ERest]) ->
    zip_for_non_opt_fields(FRest, ERest);
zip_for_non_opt_fields([], []) ->
    [].

any_field_is_sub_msg(Fields) ->
    lists:any(fun(#?gpb_field{type={msg,_}}) -> true;
                 (#?gpb_field{type={group,_}}) -> true;
                 (#?gpb_field{type={map,_,_}}) -> true;
                 (#gpb_oneof{fields=Fs}) -> any_field_is_sub_msg(Fs);
                 (_) -> false
              end,
              Fields).

any_field_is_repeated(Fields) ->
    lists:any(fun(#?gpb_field{occurrence=Occ}) -> Occ == repeated;
                 (#gpb_oneof{}) -> false
              end,
              Fields).

any_enum_field_exists(UsedTypes) ->
    sets:fold(fun({enum,_}, _Acc) -> true;
                 (_, Acc)         -> Acc
              end,
              false,
              UsedTypes).

any_packed_field_exists(#anres{num_packed_fields=0}) -> false;
any_packed_field_exists(#anres{num_packed_fields=_}) -> true.

at_least_one_submsg_with_size_not_known_at_compile_time_exists(AnRes) ->
    #anres{used_types=UsedTypes,
           maps_as_msgs=MapsAsMsgs,
           known_msg_size=KnownSize} = AnRes,
    SubMsgNames = [MsgName || {msg,MsgName} <- sets:to_list(UsedTypes)],
    MapMsgNames = [MsgName || {{msg,MsgName},_} <- MapsAsMsgs],
    IsMsgSizeUnknown = fun(Nm) -> dict:fetch(Nm, KnownSize) == undefined end,
    lists:any(IsMsgSizeUnknown, SubMsgNames) orelse
        lists:any(IsMsgSizeUnknown, MapMsgNames).

get_field_pass(MsgName, #anres{d_field_pass_method=D}) ->
    dict:fetch(MsgName, D).

get_num_fields(MsgName, #anres{num_fields=D}) ->
    dict:fetch(MsgName, D).

is_packed(#?gpb_field{type=Type, opts=Opts}) ->
    gpb:is_type_packable(Type) andalso lists:member(packed, Opts).

is_maptype_field(#?gpb_field{type={map,_,_}}) -> true;
is_maptype_field(_) -> false.

%% -> {Optionals, NonOptionals}
key_partition_on_optionality(Key, Items) ->
    key_partition_on_optionality(Key, Items, []).
key_partition_on_optionality(Key, Items, Opts) ->
    lists:partition(
      fun(Item) ->
              Field = element(Key, Item),
              case get_field_occurrence(Field) of
                  optional -> true;
                  required -> false;
                  repeated -> case mapfields_considered_required(Opts) of
                                  false -> true;
                                  true  -> not is_maptype_field(Field)
                              end
              end
      end,
      Items).

mapfields_considered_required(Opts) ->
    proplists:get_bool(mapfields_are_required, Opts).

classify_field_merge_action(FieldDef) ->
    case FieldDef of
        #?gpb_field{occurrence=required, type={msg, _}}   -> msgmerge;
        #?gpb_field{occurrence=optional, type={msg, _}}   -> msgmerge;
        #?gpb_field{occurrence=required, type={group, _}} -> msgmerge;
        #?gpb_field{occurrence=optional, type={group, _}} -> msgmerge;
        #?gpb_field{occurrence=required}                  -> overwrite;
        #?gpb_field{occurrence=optional}                  -> overwrite;
        #?gpb_field{occurrence=repeated}                  -> seqadd
    end.

flatten_oneof_fields([#?gpb_field{}=F | Rest]) ->
    [F | flatten_oneof_fields(Rest)];
flatten_oneof_fields([#gpb_oneof{fields=OFields} | Rest]) ->
    OFields ++ flatten_oneof_fields(Rest);
flatten_oneof_fields([]) ->
    [].

get_field_json_name(#?gpb_field{name=FName, opts=Opts}) ->
    case proplists:get_value(json_name, Opts) of
        undefined ->
            lower_camel_case(atom_to_list(FName));
        Name when is_list(Name) ->
            Name
    end.

%% Msg iteration --------

%% Loop over all message fields, including oneof-fields
%% Call Fun for all #?gpb_fields{}, skip over non-msg defs
fold_msg_fields(Fun, InitAcc, Defs) ->
    fold_msg_fields_o(
      fun(MsgName, Field, _IsOneOf, Acc) -> Fun(MsgName, Field, Acc) end,
      InitAcc,
      Defs).

fold_msg_or_group_fields(Fun, InitAcc, Defs) ->
    fold_msg_or_group_fields_o(
      fun(Type, Name, Field, _IsOneOf, Acc) -> Fun(Type, Name, Field, Acc) end,
      InitAcc,
      Defs).

fold_msgdef_fields(Fun, InitAcc, Fields) ->
    fold_msgdef_fields_o(
      fun(Field, _IsOneOf, Acc) -> Fun(Field, Acc) end,
      InitAcc,
      Fields).

%% The fun takes 4 args: Fun(Msgname, #?gpb_field{}, IsOneof, Acc) -> Acc1
fold_msg_fields_o(Fun, InitAcc, Defs) ->
    lists:foldl(
      fun({{msg, MsgName}, Fields}, Acc) ->
              FFun = fun(Field, IsOneOf, FAcc) ->
                             Fun(MsgName, Field, IsOneOf, FAcc)
                     end,
              fold_msgdef_fields_o(FFun, Acc, Fields);
         (_Def, Acc) ->
              Acc
      end,
      InitAcc,
      Defs).

%% The fun takes 5 args:
%% Fun(msg | group, Name, #?gpb_field{}, IsOneof, Acc) -> Acc1
fold_msg_or_group_fields_o(Fun, InitAcc, Defs) ->
    lists:foldl(
      fun({{Type, Name}, Fields}, Acc) when Type =:= msg;
                                            Type =:= group ->
              FFun = fun(Field, IsOneOf, FAcc) ->
                             Fun(Type, Name, Field, IsOneOf, FAcc)
                     end,
              fold_msgdef_fields_o(FFun, Acc, Fields);
         (_Def, Acc) ->
              Acc
      end,
      InitAcc,
      Defs).



%% The fun takes 3 args: Fun(#?gpb_field{}, IsOneof, Acc) -> Acc1
fold_msgdef_fields_o(Fun, InitAcc, Fields) ->
    lists:foldl(
      fun(#?gpb_field{}=Field, Acc) ->
              Fun(Field, false, Acc);
         (#gpb_oneof{name=CFName, fields=OFields}, Acc) ->
              IsOneOf = {true, CFName},
              lists:foldl(fun(OField, OAcc) -> Fun(OField, IsOneOf, OAcc) end,
                          Acc,
                          OFields)
      end,
      InitAcc,
      Fields).

%% Record or map expr helpers --------

%% a mapping is either a record or a map
%%
%%
mapping_match(RName, Fields, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records -> record_match(RName, Fields);
        maps    -> map_match(Fields, Opts)
    end.

mapping_create(RName, Fields, Opts) when is_list(Opts) ->
    Fn = fun() -> get_records_or_maps_by_opts(Opts) end,
    mapping_create(RName, Fields, Fn, Opts).

mapping_create(RName, Fields, RecordsOrMaps, Opts)
  when is_function(RecordsOrMaps) ->
    case RecordsOrMaps() of
        records -> record_create(RName, Fields);
        maps    -> map_create(Fields, Opts)
    end.

mapping_update(Var, RName, FieldsValues, Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records ->
            record_update(Var, RName, FieldsValues);
        maps ->
            case get_mapping_and_unset_by_opts(Opts) of
                #maps{unset_optional=present_undefined} ->
                    map_update(Var, FieldsValues, Opts);
                #maps{unset_optional=omitted} ->
                    map_set(Var, FieldsValues, Opts)
            end
    end.

%% records
record_match(RName, Fields) -> record_create_or_match(RName, Fields).
record_create(RName, Fields) -> record_create_or_match(RName, Fields).

record_create_or_match(RecordName, FieldsValueTrees) ->
    record_update(none, RecordName, FieldsValueTrees).

record_update(Var, _RecordName, []) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
record_update(Var, RecordName, FieldsValueTrees) ->
    erl_syntax:record_expr(
      Var,
      erl_syntax:atom(RecordName),
      [erl_syntax:record_field(erl_syntax:atom(FName), ValueSyntaxTree)
       || {FName, ValueSyntaxTree} <- FieldsValueTrees]).

%% maps
-ifndef(NO_HAVE_MAPS).
map_match(Fields, Opts) ->
    Literal = mapkey_literal_by_opts(Opts),
    erl_syntax:map_expr(
      [erl_syntax:map_field_exact(Literal(FName), Expr)
       || {FName, Expr} <- Fields]).

map_create(Fields, Opts) ->
    map_set(none, Fields, Opts).

map_update(Var, [], _Opts) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
map_update(Var, FieldsValueTrees, Opts) ->
    Literal = mapkey_literal_by_opts(Opts),
    erl_syntax:map_expr(
      Var,
      [erl_syntax:map_field_exact(Literal(FName), Expr)
       || {FName, Expr} <- FieldsValueTrees]).

map_set(Var, [], _Opts) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
map_set(Var, FieldsValueTrees, Opts) ->
    Literal = mapkey_literal_by_opts(Opts),
    ExprF = mapkey_expr_by_opts(Opts),
    erl_syntax:map_expr(
      Var,
      [if is_atom(FName) ->
               erl_syntax:map_field_assoc(Literal(FName), Expr);
          true -> % Key can be a variable or other type too.
               erl_syntax:map_field_assoc(ExprF(FName), Expr)
       end
       || {FName, Expr} <- FieldsValueTrees]).

mapkey_literal_by_opts(Opts) ->
    case get_maps_key_type_by_opts(Opts) of
        atom ->
            fun(Atom) -> erl_syntax:atom(Atom) end;
        binary ->
            fun(Atom) ->
                    erl_syntax:binary(
                           [erl_syntax:binary_field(
                              erl_syntax:string(atom_to_list(Atom)))])
            end
    end.

mapkey_expr_by_opts(Opts) ->
    case get_maps_key_type_by_opts(Opts) of
        atom ->
            fun(Expr) -> Expr end;
        binary ->
            fun(Expr) ->
                    erl_syntax:binary(
                      [erl_syntax:binary_field(
                         erl_syntax:application(erl_syntax:atom(erlang),
                                                erl_syntax:atom(atom_to_list),
                                                [Expr]))])
            end
    end.

-else. %% on a pre Erlang 17 system

map_match(Fields, Opts) ->
    KVs = case get_maps_key_type_by_opts(Opts) of
              atom ->
                  [?ff("~p := ~s", [FName, Var])
                   || {FName, Var} <- map_kvars(Fields)];
              binary ->
                  [?ff("<<\"~s\">> := ~s", [FName, Var])
                   || {FName, Var} <- map_kvars(Fields)]
          end,
    erl_syntax:text(?ff("#{~s}", [string:join(KVs, ", ")])).

map_create(Fields, Opts) ->
    KVs = case get_maps_key_type_by_opts(Opts) of
              atom ->
                  [?ff("~p => ~s", [FName, Val])
                   || {FName, Val} <- map_kvalues(Fields)];
              binary ->
                  [?ff("<<\"~s\">> => ~s", [FName, Val])
                   || {FName, Val} <- map_kvalues(Fields)]
          end,
    erl_syntax:text(?ff("#{~s}", [string:join(KVs, ", ")])).

map_update(Var, [], _Opts) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
map_update(Var, FieldsValueTrees, Opts) ->
    KVs = case get_maps_key_type_by_opts(Opts) of
              atom ->
                  [?ff("~p := ~s", [FName, Val])
                   || {FName, Val} <- map_kvalues(FieldsValueTrees)];
              binary ->
                  [?ff("<<\"~s\">> := ~s", [FName, Val])
                   || {FName, Val} <- map_kvalues(FieldsValueTrees)]
          end,
    erl_syntax:text(?ff("~s#{~s}", [var_literal(Var), string:join(KVs, ", ")])).


map_set(Var, [], _Opts) when Var /= none ->
    %% No updates to be made, maybe no fields
    Var;
map_set(Var, FieldsValueTrees, Opts) ->
    KVs = case get_maps_key_type_by_opts(Opts) of
              atom ->
                  [?ff("~p => ~s", [FName, Val])
                   || {FName, Val} <- map_kvalues(FieldsValueTrees)];
              binary ->

                  [?ff("<<\"~s\">> => ~s", [FName, Val])
                   || {FName, Val} <- map_kvalues(FieldsValueTrees)]
          end,
    erl_syntax:text(?ff("~s#{~s}", [var_literal(Var), string:join(KVs, ", ")])).


%% -> [{atom(), string()}]
map_kvars(KVars) ->
    [{Key, var_literal(Var)} || {Key, Var} <- KVars].

var_literal(Var) ->
    variable = erl_syntax:type(Var),
    erl_syntax:variable_literal(Var).

%% -> [{atom(), string()}]
map_kvalues(KVars) ->
    [begin
         ExprAsStr = erl_prettypr:format(Expr),
         {Key, ExprAsStr}
     end
     || {Key, Expr} <- KVars].

-endif. %% NO_HAVE_MAPS

%% Option helpers ---------------

get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) ->
    Default = false,
    case proplists:get_value(mapfields_as_maps, Opts, Default) of
        true  -> maps;
        false -> '2tuples'
    end.

get_records_or_maps_by_opts(Opts) ->
    Default = false,
    case proplists:get_value(msgs_as_maps, Opts, Default) of
        false -> records;
        true  -> maps
    end.

get_mapping_and_unset_by_opts(Opts) ->
    case get_records_or_maps_by_opts(Opts) of
        records ->
            records;
        maps ->
            DefaultUnsetOptional = omitted,
            UnseOptional = proplists:get_value(maps_unset_optional, Opts,
                                               DefaultUnsetOptional),
            Oneof = proplists:get_value(maps_oneof, Opts, tuples),
            #maps{unset_optional=UnseOptional, oneof=Oneof}
    end.

get_strings_as_binaries_by_opts(Opts) ->
    proplists:get_bool(strings_as_binaries, Opts).

get_type_specs_by_opts(Opts) ->
    Default = true,
    proplists:get_value(type_specs, Opts, Default).

get_gen_descriptor_by_opts(Opts) ->
    proplists:get_bool(descriptor, Opts).

get_field_format_by_opts(Opts) ->
    case proplists:get_bool(defs_as_proplists, proplists:unfold(Opts)) of
        false -> %% default
            case get_defs_as_maps_or_records(Opts) of
                records -> fields_as_records;
                maps    -> fields_as_maps
            end;
        true ->
            fields_as_proplists
    end.

mk_get_defs_as_maps_or_records_fn(Opts) ->
    fun() -> get_defs_as_maps_or_records(Opts) end.

get_defs_as_maps_or_records(Opts) ->
    Default = false,
    case proplists:get_value(defs_as_maps, Opts, Default) of
        false -> records;
        true  -> maps
    end.

get_epb_functions_by_opts(Opts) ->
    proplists:get_bool(epb_functions, Opts).

get_bypass_wrappers_by_opts(Opts) ->
    proplists:get_bool(bypass_wrappers, Opts).

is_target_major_version_at_least(VsnMin, Opts) ->
    case proplists:get_value(target_erlang_version, Opts, current) of
        current ->
            is_current_major_version_at_least(VsnMin);
        N when is_integer(N) ->
            N >= VsnMin
    end.

is_current_major_version_at_least(VsnMin) ->
    current_otp_release() >= VsnMin.

current_otp_release() ->
    case erlang:system_info(otp_release) of
        "R"++Rest -> % R16 or ealier
            FirstChunkOfDigits = lists:takewhile(fun is_digit/1, Rest),
            list_to_integer(FirstChunkOfDigits);
        RelStr ->
            %% In Erlang 17 the leading "R" was dropped
            %% The exact format isn't super documented,
            %% so be prepared for some (future?) alternatives.
            try list_to_integer(RelStr) of
                N when is_integer(N) -> N
            catch error:badarg ->
                    Rel = lists:dropwhile(fun is_not_digit/1, RelStr),
                    FirstChunkOfDigits = lists:takewhile(fun is_digit/1, Rel),
                    list_to_integer(FirstChunkOfDigits)
            end
    end.

is_not_digit(C) -> not is_digit(C).

is_digit(C) when $0 =< C, C =< $9 -> true;
is_digit(_) -> false.

%% Whether target version has the function lists:join/2.
target_has_lists_join(Opts) ->
    is_target_major_version_at_least(19, Opts).

%% Whether target version supports M#{K => V} when K is a variable.
%% If before this support was added, one must use maps:put(K, V, M) instead.
target_has_variable_key_map_update(Opts) ->
    is_target_major_version_at_least(18, Opts).

%% Whether target version supports #{key := type()} type spec syntax.
%% In Erlang 19, := indicates mandatory presence and => optional presence.
%% In Erlang 18, only => was supported.
target_can_specify_map_item_presence_in_typespecs(Opts) ->
    is_target_major_version_at_least(19, Opts).

target_can_do_flat_oneof_for_maps(Opts) ->
    %% Not possible in Erlang 17 because:
    %%    Variables as map keys appeared in 18.0. In 17, supports only literals
    %%    as map keys.
    is_target_major_version_at_least(18, Opts).

target_may_fail_compilation_for_flat_oneof_for_maps(Opts) ->
    %% In Erlang 18.3.4.6 .. 18.3.4.9
    %% (ie the currently last/highest 4 Erlang 18 versions) this happens:
    %% --
    %%    % erlc <erl for flat oneof>.erl
    %%    beamvalidatorerror: function v_msg_m1/3+75:
    %%      Internal consistency check failed - please report this bug.
    %%      Instruction: {move,{x,2},{y,0}}
    %%      Error:       {uninitialized_reg,{x,2}}:
    %% --
    %% (introduced in c803276c9)
    %% All Erlang 19 versions and later seems fine.
    case target_can_do_flat_oneof_for_maps(Opts) of
        true ->
            AtLeast19 = is_target_major_version_at_least(19, Opts),
            AtLeast18 = is_target_major_version_at_least(18, Opts),
            AtLeast18 andalso (not AtLeast19);
        false ->
            true % On pre-18, it will definitely fail
    end.

%% In Erlang 21, the function erlang:get_stacktrace/0 was deprecated
%% and there is new syntax for retrieving the stacktrace:
%%
%%   try ...
%%   catch Class:Reason:Stacktrace -> ...
%%   end
target_has_stacktrace_syntax(Opts) ->
    is_target_major_version_at_least(21, Opts).

%% In Erlang 21, there is maps:iterator/1 and maps:next/1 where "the memory
%% usage is guaranteed to be bounded no matter the size of the map."
target_has_map_iterators(Opts) ->
    is_target_major_version_at_least(21, Opts).

proto2_type_default(Type, Defs, Opts) ->
    type_default(Type, Defs, Opts, fun gpb:proto2_type_default/2).

proto3_type_default(Type, Defs, Opts) ->
    type_default(Type, Defs, Opts, fun gpb:proto3_type_default/2).

type_default(Type, Defs, Opts, GetTypeDefault) ->
    if Type == string ->
            case get_strings_as_binaries_by_opts(Opts) of
                true ->
                    list_to_binary(GetTypeDefault(Type, Defs));
                false ->
                    GetTypeDefault(Type, Defs)
            end;
       Type /= string ->
            GetTypeDefault(Type, Defs)
    end.

get_maps_key_type_by_opts(Opts) ->
    proplists:get_value(maps_key_type, Opts, atom).

json_by_opts(Opts) ->
    proplists:get_bool(json, Opts).

json_object_format_by_opts(Opts) ->
    case proplists:get_value(json_object_format, Opts) of
        undefined ->
            case gpb_lib:get_records_or_maps_by_opts(Opts) of
                maps ->
                    map;
                records ->
                    eep18
            end;
        eep18 ->
            eep18;
        {proplist} ->
            {proplist};
        {Atom, proplist} when is_atom(Atom) ->
            {Atom, proplist};
        map ->
            map
    end.

json_key_format_by_opts(Opts) ->
    case proplists:get_value(json_key_format, Opts, binary) of
        atom ->
            atom;
        binary ->
            binary;
        string ->
            string
    end.

json_array_format_by_opts(Opts) ->
    case proplists:get_value(json_array_format, Opts, list) of
        list ->
            list;
        {Atom, list} when is_atom(Atom) ->
            {Atom, list}
    end.

json_string_format_by_opts(Opts) ->
    case proplists:get_value(json_string_format, Opts, binary) of
        binary ->
            binary;
        list ->
            list
    end.

json_null(Opts) ->
    proplists:get_value(json_string_format, Opts, null).

get_gen_mergers(Opts) ->
    proplists:get_value(gen_mergers, Opts, true).

get_gen_introspect(Opts) ->
    proplists:get_value(gen_introspect, Opts, true).

%% Syntax tree stuff ----

var_f_n(N) -> var_n("F", N).
var_b_n(N) -> var_n("B", N).
var_j_n(N) -> var_n("J", N).

var_n(S, N) ->
    var("~s~w", [S, N]).

var(Fmt, Args) ->
    erl_syntax:variable(?ff(Fmt, Args)).

prefix_var(Prefix, Var) ->
    erl_syntax:variable(Prefix ++ erl_syntax:variable_literal(Var)).

match_bind_var(Pattern, Var) ->
    ?expr('Pattern' = 'Var',
          [replace_tree('Pattern', Pattern),
           replace_tree('Var', Var)]).

assign_to_var(Var, Expr) ->
    ?expr('<Var>' = '<Expr>',
          [replace_tree('<Var>', Var),
           replace_tree('<Expr>', Expr)]).

varint_to_binary_fields(IntValue) ->
    [erl_syntax:binary_field(?expr('<n>', [replace_term('<n>', N)]), [])
     || N <- binary_to_list(gpb:encode_varint(IntValue))].

%% Given a sequence, `Seq', of expressions, and an initial expression,
%% Construct:
%%     TmpVar1 = InitialExpr,
%%     TmpVar2 = <1st expression in sequence, possibly involving TmpVar1>
%%     TmpVar3 = <2st expression in sequence, possibly involving TmpVar2>
%%     ...
%%     <final expression in sequence, possibly involving TmpVarN-1>
do_exprs(F, InitExpr, Seq) ->
    {LastExpr, ExprsReversed, _N} =
        lists:foldl(
          fun(Elem, {PrevE,Es,N}) ->
                  Var = var_n("S", N),
                  BoundPrevE = assign_to_var(Var, PrevE),
                  E = F(Elem, Var),
                  {E, [BoundPrevE | Es], N+1}
          end,
          {InitExpr, [], 1},
          Seq),
    lists:reverse([LastExpr | ExprsReversed]).


%% File name related ---

drop_filename_ext(Path) ->
    [B | RRest] = lists:reverse(filename:split(Path)),
    BNoExt = filename:basename(B, filename:extension(B)),
    filename:join(lists:reverse(RRest, [BNoExt])).

copy_filename_ext(FilenameSansExt, FilenameToCopyFrom) ->
    FilenameSansExt ++ filename:extension(FilenameToCopyFrom).

%% @doc Compute filenames (paths) to basenames
%% but include last part(s) of the directories as necessary
%% to make the basenames unique.
%%
%% Example:
%% ```
%% basenameify_ish(["/home/u/a/b/c/f.proto",
%%                  "/home/u/a/d/c/f.proto",
%%                  "/home/u/x/y/z/f.proto",
%%                  "/home/u/x/y/z/g.proto"]) ->
%%   ["b/c/f.proto", "d/c/f.proto", "z/f.proto", "g.proto"]
%% '''
%% The order of the returned paths is the same as the order of the input
%% paths, so to create a mapping, one can use for example
%% `lists:zip(Paths, basenameify_ish(Paths))'
basenameify_ish(Paths) ->
    %% Given the example above, split into:
    %% [{"f.proto", ["c", "b", "a", "u", "home"]},
    %%  {"f.proto", ["c", "d", "a", "u", "home"]},
    %%  {"f.proto", ["z", "y", "x", "u", "home"]},
    %%  {"g.proto", ["z", "y", "x", "u", "home"]}]
    %% Then for all dups in the first element of these 2-tuples,
    %% "f.proto" in this case, prepend one more component from the
    %% RPath (the 2nd elem of the 2-tuples), then iterate until all first
    %% elements are unique.

    case find_dup_rpath_comps(Paths, []) of
        []   -> ok;
        Dups -> error({gpb_error, {multiply_defined_file_or_files, Dups}})
    end,

    RPathComps = [begin
                      [Basename | RPath] = lists:reverse(filename:split(P)),
                      {[Basename], RPath}
                  end
                  || P <- Paths],
    include_dir_comps_until_unique(RPathComps).

include_dir_comps_until_unique(RPathComps) ->
    case find_dup_rpath_comps([B || {B, _} <- RPathComps], []) of
        [] ->
            [filename:join(Baseish) || {Baseish, _} <- RPathComps];
        Dups ->
            %% Include one dir component for each elem in dup
            include_dir_comps_until_unique(
              [case {lists:member(Baseish, Dups), RPath} of
                   {true, [Comp | RestRPath]} ->
                       {[Comp |  Baseish], RestRPath};
                   {false, _} ->
                       Item
               end
               || {Baseish, RPath}=Item <- RPathComps])
    end.

find_dup_rpath_comps([X | Rest], Acc) ->
    case lists:member(X, Rest) of
        true  -> find_dup_rpath_comps([Y || Y <- Rest, Y =/= X], [X | Acc]);
        false -> find_dup_rpath_comps(Rest, Acc)
    end;
find_dup_rpath_comps([], Acc) ->
    Acc.

%% Misc ---

index_seq([]) -> [];
index_seq(L)  -> lists:zip(lists:seq(1,length(L)), L).

smember(Elem, Set) -> %% set-member
    sets:is_element(Elem, Set).

smember_any(Elems, Set) -> %% is any elem a member in the set
    lists:any(fun(Elem) -> smember(Elem, Set) end,
              Elems).

indent(Indent, Str) ->
    lists:duplicate(Indent, $\s) ++ Str.

outdent_first(IoList) ->
    lists:dropwhile(fun(C) -> C == $\s end,
                    binary_to_list(iolist_to_binary(IoList))).

indent_lines(Indent, Lines) ->
    [indent(Indent, Line) || Line <- Lines].

split_indent_butfirst_iolist(Indent, IoList) ->
    strip_left(iolist_to_binary(split_indent_iolist(Indent, IoList))).

strip_left(<<" ", Rest/binary>>) -> strip_left(Rest);
strip_left(Other)                -> Other.

cond_split_indent_iolist(Condition, Indent, IoList) ->
    B = iolist_to_binary(IoList),
    case Condition(B) of
        true  -> split_indent_iolist(Indent, IoList);
        false -> B
    end.

split_indent_iolist(Indent, IoList) ->
    [if Line == <<>> -> "\n"; %% don't indent empty lines
        true -> [indent(Indent, Line), "\n"]
     end
     || Line <- linesplit_iolist(IoList)].

linesplit_iolist(Iolist) ->
    re:split(Iolist, ["\n"], [trim, {return,binary}]).

iolist_to_utf8_or_escaped_binary(IoList, Opts) ->
    case understands_coding(Opts) of
        true  ->
            unicode:characters_to_binary(
              ["%% -*- coding: utf-8 -*-\n",
               IoList]);
        false ->
            %% What to do if on Erlang R15 or earlier?  We can't utf8-encode
            %% the file, because Erlang R15 will read it as latin1.
            %%
            %% For now, Assume such encodings are in strings only.
            %% So far, this is safe, since neither message names nor field
            %% names nor enum symbols are allowed to be non-ascii.
            %%
            %% This means only place for non-ascii is in comments and
            %% in default strings. Hope I haven't overlooked some
            %% important place...
            iolist_to_binary(esc_non_ascii(IoList))
    end.

understands_coding(Opts) ->
    %% version   coding: X             default source encoding
    %% R15:      ignores               latin1
    %% R16:      understands           latin1
    %% 17:       understands           utf-8
    is_target_major_version_at_least(16, Opts).

esc_non_ascii([H|T]) -> [esc_non_ascii(H) | esc_non_ascii(T)];
esc_non_ascii([])    -> [];
esc_non_ascii(B) when is_binary(B) -> B;
esc_non_ascii(C) when is_integer(C), C =< 127 -> C;
esc_non_ascii(C) when is_integer(C), C > 127  -> ?f("\\x{~.16b}", [C]).

nowarn_dialyzer_attr(FnName,Arity,Opts) ->
    %% Especially for the verifiers, dialyzer's success typing can
    %% think that some code paths in the verifiers can't be reached,
    %% and in a sense, it is right: the verifiers do much the same
    %% work as dialyzer. But I think their existence is still
    %% warranted because (a) they work-time rather than compile-time,
    %% and (b) provide for shorter turn-around times when dialyzer
    %% can take some time to analyze a non-trivial proto file.
    %%
    %% So mute dialyzer for the verifier functions.
    case can_do_dialyzer_attr(Opts) of
        true ->
            ?f("-dialyzer({nowarn_function,~p/~w}).~n", [FnName,Arity]);
        false ->
            %% Too old system (Erlang 17 or older), which will see
            %% the dialyzer attr as just another plain attr,
            %% which must be located before all functions.
            %% Just don't silence dialyzer on these systems.
            ""
    end.

can_do_dialyzer_attr(Opts) ->
    is_target_major_version_at_least(18, Opts).

nowarn_unused_function(FnName, Arity) ->
    ?f("-compile({nowarn_unused_function,~p/~w}).~n", [FnName,Arity]).

-ifndef(NO_HAVE_ERL20_STR_FUNCTIONS).

comma_join(Elements) ->
    lists:append(lists:join(", ", Elements)).

nl_join(Elements) ->
    lists:append(lists:join("\n", Elements)).

or_join(Alternatives) ->
    lists:append(lists:join(" | ", Alternatives)).

dot_join(Alternatives) ->
    lists:append(lists:join(".", Alternatives)).

is_substr(SearchPattern, String) ->
    string:find(String, SearchPattern) /= nomatch.

string_slice(String, Start) ->
    string:slice(String, Start).

string_lexemes(String, Separators) ->
    string:lexemes(String, Separators).

lowercase(Str) ->
    string:lowercase(Str).

uppercase(Str) ->
    string:uppercase(Str).

-else.  % NO_HAVE_ERL20_STR_FUNCTIONS

comma_join(Elements) ->
    string:join(Elements, ", ").

nl_join(Elements) ->
    string:join(Elements, "\n").

or_join(Alternatives) ->
    string:join(Alternatives, " | ").

dot_join(Alternatives) ->
    string:join(Alternatives, ".").

is_substr(SearchPattern, String) ->
    string:str(String, SearchPattern) > 0.

string_slice(String, Start0) ->
    string:substr(String, Start0 + 1).

string_lexemes(String, Separators) ->
    string:tokens(String, Separators).

lowercase(Str) ->
    string:to_lower(Str).

uppercase(Str) ->
    string:to_upper(Str).

-endif. % NO_HAVE_ERL20_STR_FUNCTIONS

snake_case(Str) ->
    lowercase(
      lists:foldl(fun(RE, Snaking) ->
                          re:replace(Snaking, RE, "\\1_\\2", [{return, list},
                                                              global])
                  end, Str, [%% uppercase followed by lowercase
                             "([^.])([A-Z][a-z]+)",
                             %% any consecutive digits
                             "([^.])([0-9]+)",
                             %% uppercase with lowercase
                             %% or digit before it
                             "([a-z0-9])([A-Z])"])).

camel_case(Str) ->
    camel_case(Str, true).

%% Like camel case, but first letter is lower case
lower_camel_case(S) ->
    [C1 | Rest] = camel_case(S),
    [LC1] = lowercase([C1]),
    [LC1 | Rest].

-define(is_lower_case(C), $a =< C, C =< $z).
-define(is_upper_case(C), $A =< C, C =< $Z).
-define(is_digit(C),      $0 =< C, C =< $9).
camel_case([LC | Tl], CapNextLetter) when ?is_lower_case(LC) ->
    if CapNextLetter     -> [capitalize_letter(LC) | camel_case(Tl, false)];
       not CapNextLetter -> [LC | camel_case(Tl, false)]
    end;
camel_case([UC | Tl], _) when ?is_upper_case(UC) ->
    [UC | camel_case(Tl, false)];
camel_case([D | Tl], _) when ?is_digit(D) ->
    [D | camel_case(Tl, true)];
camel_case([_ | Tl], _) -> %% underscore and possibly more
    camel_case(Tl, true);
camel_case([], _) ->
    [].

capitalize_letter(C) ->
    C + ($A - $a).
