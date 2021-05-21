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

%%% @doc Generation of message verifiers
%%% @private

-module(gpb_gen_verifiers).

-export([format_exports/2]).
-export([format_verifiers_top_function/3]).
-export([format_verifiers/3]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2,
                  splice_trees/2, repeat_clauses/2]).

format_exports(_Defs, Opts) ->
    case gpb_lib:get_records_or_maps_by_opts(Opts) of
        records ->
            ?f("-export([verify_msg/1, verify_msg/2, verify_msg/3]).~n");
        maps ->
            ?f("-export([verify_msg/2, verify_msg/3]).~n")
    end.

format_verifiers_top_function(Defs, AnRes, Opts) ->
    case {gpb_lib:contains_messages(Defs),
          gpb_lib:get_records_or_maps_by_opts(Opts)} of
        {false, records} -> format_verifiers_top_no_msgs_r();
        {false, maps}    -> format_verifiers_top_no_msgs_m();
        {true,  _}       -> format_verifiers_top_with_msgs(Defs, AnRes, Opts)
    end.

format_verifiers_top_no_msgs_r() ->
    [?f("-spec verify_msg(_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg) -> call_self(Msg, []) end),
     ?f("-spec verify_msg(_,_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg,_OptsOrMsgName) ->
               mk_type_error(not_a_known_message, Msg, [])
       end),
     "\n",
     ?f("-spec verify_msg(_,_,_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg,_MsgName,_Opts) ->
               mk_type_error(not_a_known_message, Msg, [])
       end),
     "\n"].

format_verifiers_top_no_msgs_m() ->
    [?f("-spec verify_msg(_,_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, MsgName) -> call_self(Msg, MsgName, []) end),
     ?f("-spec verify_msg(_,_,_) -> no_return().~n", []),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, _MsgName, _Opts) ->
               mk_type_error(not_a_known_message, Msg, [])
       end),
     "\n"].

format_verifiers_top_with_msgs(Defs, AnRes, Opts) ->
    Mapping = gpb_lib:get_records_or_maps_by_opts(Opts),
    [[gpb_codegen:format_fn(
        verify_msg,
        fun(Msg) when tuple_size(Msg) >= 1 ->
                verify_msg(Msg, element(1, Msg), []);
           (X) ->
                mk_type_error(not_a_known_message, X, [])
        end) || Mapping == records],
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, MsgName) when is_atom(MsgName) ->
               call_self(Msg, MsgName, []);
          ('Msg', Opts) when tuple_size('Msg') >= 1 ->
               call_self('Msg', element(1,'Msg'), Opts);
          ('X', _Opts) ->
               mk_type_error(not_a_known_message, 'X', [])
       end,
       [repeat_clauses('Msg', [[replace_tree('Msg', ?expr(Msg))]
                               || Mapping == records]),
        repeat_clauses('X', [[replace_tree('X', ?expr(X))]
                             || Mapping == records])]),
     gpb_codegen:format_fn(
       verify_msg,
       fun(Msg, MsgName, Opts) ->
               TrUserData = proplists:get_value(user_data, Opts),
               case MsgName of
                   '<msg-name-match>' ->
                       '<verify-fn>'(Msg, [MsgName], TrUserData);
                   _ ->
                       mk_type_error(not_a_known_message, Msg, [])
               end
       end,
       [repeat_clauses(
          '<msg-name-match>',
          [begin
               DefaultVerifierFn = gpb_lib:mk_fn(v_msg_, MsgName),
               ElemPath = [MsgName],
               MVerifierFn = case gpb_gen_translators:has_translation(
                                    ElemPath, verify, AnRes) of
                                 {true, Transl} -> Transl;
                                 false -> DefaultVerifierFn
                             end,
               [replace_term('<msg-name-match>', MsgName),
                replace_term('<verify-fn>', MVerifierFn),
                replace_term('<MsgName>', MsgName)]
           end
           || {{msg, MsgName}, _MsgDef} <- Defs])])].

format_verifiers(Defs, AnRes, Opts) ->
    [format_msg_verifiers(Defs, AnRes, Opts),
     format_enum_verifiers(Defs, AnRes, Opts),
     format_type_verifiers(AnRes, Opts),
     format_map_verifiers(AnRes, Opts),
     format_verifier_auxiliaries(Defs, Opts)
    ].

format_msg_verifiers(Defs, AnRes, Opts) ->
    [format_msg_verifier(MsgName, MsgDef, AnRes, Opts)
     || {_Type, MsgName, MsgDef} <- gpb_lib:msgs_or_groups(Defs)].

format_msg_verifier(MsgName, MsgDef0, AnRes, Opts) ->
    MsgDef1 = drop_field_for_unknown_if_present(MsgDef0),
    FNames = gpb_lib:get_field_names(MsgDef1),
    FVars = [gpb_lib:var_f_n(I) || I <- lists:seq(1, length(FNames))],
    MsgVar = ?expr(M),
    {FieldMatching, NonOptKeys} =
        case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
            records ->
                {gpb_lib:mapping_match(MsgName, lists:zip(FNames, FVars), Opts),
                 FNames};
            #maps{unset_optional=present_undefined} ->
                {gpb_lib:mapping_match(MsgName, lists:zip(FNames, FVars), Opts),
                 FNames};
            #maps{unset_optional=omitted} ->
                FMap = gpb_lib:zip_for_non_opt_fields(MsgDef1, FVars),
                {?expr('mapmatch' = 'M',
                       [replace_tree('mapmatch', gpb_lib:map_match(FMap, Opts)),
                        replace_tree('M', MsgVar)]),
                 [K || {K, _} <- FMap]}
        end,
    NonOptKeys1 = erl_syntax:list(map_keys_to_strees(NonOptKeys, Opts)),
    ExtraneousFieldsChecks =
        case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
            records ->
                [];
            #maps{unset_optional=present_undefined} ->
                [];
            #maps{unset_optional=omitted}=Mapping ->
                Names1 = case Mapping of
                             #maps{oneof=tuples} ->
                                 FNames;
                             #maps{oneof=flat} ->
                                 field_names_oneofs_expanded(MsgDef1)
                         end,
                Names2 = map_keys_to_strees(Names1, Opts),
                [?expr(lists:foreach(
                         fun('<Key>') ->
                                 ok;
                            (OtherKey) ->
                                 mk_type_error({extraneous_key, OtherKey},
                                               'M', Path)
                         end,
                         maps:keys('M')),
                       [repeat_clauses(
                          '<Key>',
                          [[replace_tree('<Key>', Key)] || Key <- Names2]),
                        replace_tree('M', MsgVar)])]
        end,

    FnName = gpb_lib:mk_fn(v_msg_, MsgName),
    TrUserDataVar = ?expr(TrUserData),
    [gpb_lib:nowarn_unused_function(FnName, 3),
     gpb_lib:nowarn_dialyzer_attr(FnName,3,Opts),
     gpb_codegen:format_fn(
       FnName,
       fun('<msg-match>', '<Path>', 'TrUserData') ->
               '<verify-fields>',
               '<maybe-verify-no-extraneous-fields>',
               ok;
          ('<M>', Path, _TrUserData) when is_map('<M>') ->
               mk_type_error(
                 {missing_fields, 'NonOptKeys'--maps:keys('<M>'), '<MsgName>'},
                 '<M>', Path);
          (X, Path, _TrUserData) ->
               mk_type_error({expected_msg,'<MsgName>'}, X, Path)
       end,
       [replace_tree('<msg-match>', FieldMatching),
        replace_tree('<Path>', if MsgDef1 == [],
                                  ExtraneousFieldsChecks == [] ->
                                       ?expr(_Path);
                                  true ->
                                       ?expr(Path)
                               end),
        replace_tree('TrUserData', if FNames /= [] -> TrUserDataVar;
                                      FNames =:= [] -> ?expr(_)
                                   end),
        splice_trees('<verify-fields>',
                     field_verifiers(MsgName, MsgDef1, FVars, MsgVar,
                                     TrUserDataVar,
                                     AnRes, Opts)),
        splice_trees('<maybe-verify-no-extraneous-fields>',
                     ExtraneousFieldsChecks),
        repeat_clauses('<M>',
                       case gpb_lib:get_records_or_maps_by_opts(Opts) of
                           records ->
                               []; % omit this clause
                           maps ->
                               [[replace_tree('<M>', ?expr(M)),
                                 replace_tree('NonOptKeys', NonOptKeys1)]]
                       end),
        replace_term('<MsgName>', MsgName)])].

drop_field_for_unknown_if_present(Fields) ->
    [Field || Field <- Fields,
              not gpb_lib:is_field_for_unknowns(Field)].

field_names_oneofs_expanded(MsgDef) ->
    gpb_lib:fold_msgdef_fields(
      fun(#?gpb_field{name=FName}, Acc) -> [FName | Acc] end,
      [],
      MsgDef).

field_verifiers(MsgName, Fields, FVars, MsgVar, TrUserDataVar, AnRes, Opts) ->
    [field_verifier(MsgName, Field, FVar, MsgVar, TrUserDataVar, AnRes, Opts)
     || {Field, FVar} <- lists:zip(Fields, FVars)].

field_verifier(MsgName,
               #?gpb_field{name=FName, type=Type, occurrence=Occurrence}=Field,
               FVar, MsgVar, TrUserDataVar, AnRes, Opts) ->
    FVerifierFn =
        case Type of
            {msg,FMsgName}  -> gpb_lib:mk_fn(v_msg_, FMsgName);
            {group,GName}   -> gpb_lib:mk_fn(v_msg_, GName);
            {enum,EnumName} -> gpb_lib:mk_fn(v_enum_, EnumName);
            {map,KT,VT}     -> gpb_lib:mk_fn(v_, gpb_lib:map_type_to_msg_name(
                                                   KT,VT));
            Type            -> gpb_lib:mk_fn(v_type_, Type)
        end,
    ElemPath = gpb_gen_translators:mk_elempath_elem(MsgName, Field, false),
    FVerifierFn2 = gpb_gen_translators:find_translation(ElemPath, verify,
                                                        AnRes, FVerifierFn),
    BReplacements = [replace_tree('<F>', FVar),
                     replace_term('<FName>', FName),
                     replace_term('<Type>', Type),
                     replace_tree('TrUserData', TrUserDataVar)],
    Replacements = [replace_term('<verify-fn>', FVerifierFn2)
                    | BReplacements],
    IsMapField = case Type of
                     {map,_,_} -> true;
                     _ -> false
                 end,
    MElemPath = [MsgName, FName],
    MTransl = gpb_gen_translators:has_translation(MElemPath, verify, AnRes),
    HasTranslation = case MTransl of
                         {true, _} -> true;
                         false -> false
                     end,
    case Occurrence of
        required ->
            %% FIXME: check especially for `undefined'
            %% and if found, error out with required_field_not_set
            %% specifying expected type
            ?expr('<verify-fn>'('<F>', ['<FName>' | Path], 'TrUserData'),
                  Replacements);
        repeated when not IsMapField, HasTranslation ->
            {true, RFVerifierFn} = MTransl,
            RReplacements = [replace_term('<verify-fn>', RFVerifierFn)
                             | BReplacements],
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                records ->
                    ?expr('<verify-fn>'('<F>', ['<FName>' | Path],
                                        'TrUserData'),
                          RReplacements);
                #maps{unset_optional=present_undefined} ->
                    ?expr('<verify-fn>'('<F>', ['<FName>' | Path],
                                        'TrUserData'),
                          RReplacements);
                #maps{unset_optional=omitted} ->
                    ?expr(case 'M' of
                              '#{<FName> := <F>}' ->
                                  '<verify-fn>'('<F>', ['<FName>' | Path],
                                                'TrUserData');
                              _ ->
                                  ok
                          end,
                          [replace_tree('#{<FName> := <F>}',
                                        gpb_lib:map_match([{FName, FVar}],
                                                          Opts)),
                           replace_tree('M', MsgVar) | RReplacements])
            end;
        repeated when not IsMapField ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                records ->
                    ?expr(if is_list('<F>') ->
                                  %% _ = [...] to avoid dialyzer error
                                  %% "Expression produces a value of type
                                  %% ['ok'], but this value is unmatched"
                                  %% with the -Wunmatched_returns flag.
                                  _ = ['<verify-fn>'(Elem, ['<FName>' | Path],
                                                     'TrUserData')
                                       || Elem <- '<F>'],
                                  ok;
                             true ->
                                  mk_type_error(
                                    {invalid_list_of, '<Type>'},
                                    '<F>',
                                    ['<FName>' | Path])
                          end,
                          Replacements);
                #maps{unset_optional=present_undefined} ->
                    ?expr(if is_list('<F>') ->
                                  %% _ = [...] to avoid dialyzer error
                                  %% "Expression produces a value of type
                                  %% ['ok'], but this value is unmatched"
                                  %% with the -Wunmatched_returns flag.
                                  _ = ['<verify-fn>'(Elem, ['<FName>' | Path],
                                                     'TrUserData')
                                       || Elem <- '<F>'],
                                  ok;
                             true ->
                                  mk_type_error(
                                    {invalid_list_of, '<Type>'},
                                    '<F>',
                                    ['<FName>' | Path])
                          end,
                          Replacements);
                #maps{unset_optional=omitted} ->
                    ?expr(case 'M' of
                              '#{<FName> := <F>}' ->
                                  if is_list('<F>') ->
                                          %% _ = [...] to avoid dialyzer error
                                          %% "Expression produces a value of type
                                          %% ['ok'], but this value is unmatched"
                                          %% with the -Wunmatched_returns flag.
                                          _ = ['<verify-fn>'(Elem, ['<FName>' | Path],
                                                             'TrUserData')
                                               || Elem <- '<F>'],
                                          ok;
                                     true ->
                                          mk_type_error(
                                            {invalid_list_of, '<Type>'},
                                            '<F>',
                                            ['<FName>' | Path])
                                  end;
                              _ -> ok
                          end,
                          [replace_tree('#{<FName> := <F>}',
                                        gpb_lib:map_match([{FName, FVar}],
                                                          Opts)),
                           replace_tree('M', MsgVar) | Replacements])
            end;
        repeated when IsMapField ->
            MFVerifierFn = gpb_gen_translators:find_translation(
                             MElemPath, verify, AnRes, FVerifierFn),
            MReplacements = [replace_term('<verify-fn>', MFVerifierFn)
                             | BReplacements],
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                records ->
                    ?expr('<verify-fn>'('<F>', ['<FName>' | Path],
                                        'TrUserData'),
                          MReplacements);
                #maps{unset_optional=present_undefined} ->
                    ?expr('<verify-fn>'('<F>', ['<FName>' | Path],
                                        'TrUserData'),
                          MReplacements);
                #maps{unset_optional=omitted} ->
                    ?expr(case 'M' of
                              '#{<FName> := <F>}' ->
                                  '<verify-fn>'('<F>', ['<FName>' | Path],
                                                'TrUserData');
                              _ ->
                                  ok
                          end,
                          [replace_tree('#{<FName> := <F>}',
                                        gpb_lib:map_match([{FName, FVar}],
                                                          Opts)),
                           replace_tree('M', MsgVar) | MReplacements])
            end;
        optional ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                records ->
                    ?expr(if '<F>' == undefined -> ok;
                             true -> '<verify-fn>'('<F>', ['<FName>' | Path],
                                                   'TrUserData')
                          end,
                          Replacements);
                #maps{unset_optional=present_undefined} ->
                    ?expr(if '<F>' == undefined -> ok;
                             true -> '<verify-fn>'('<F>', ['<FName>' | Path],
                                                   'TrUserData')
                          end,
                          Replacements);
                #maps{unset_optional=omitted} ->
                    ?expr(case 'M' of
                              '#{<FName> := <F>}' ->
                                  '<verify-fn>'('<F>', ['<FName>' | Path],
                                                'TrUserData');
                              _ ->
                                  ok
                          end,
                          [replace_tree('#{<FName> := <F>}',
                                        gpb_lib:map_match([{FName, FVar}],
                                                          Opts)),
                           replace_tree('M', MsgVar) | Replacements])
            end;
        defaulty ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                records ->
                    ?expr(if '<F>' == undefined -> ok;
                             true -> '<verify-fn>'('<F>', ['<FName>' | Path],
                                                   'TrUserData')
                          end,
                          Replacements);
                #maps{unset_optional=present_undefined} ->
                    ?expr(if '<F>' == undefined -> ok;
                             true -> '<verify-fn>'('<F>', ['<FName>' | Path],
                                                   'TrUserData')
                          end,
                          Replacements);
                #maps{unset_optional=omitted} ->
                    ?expr(case 'M' of
                              '#{<FName> := <F>}' ->
                                  '<verify-fn>'('<F>', ['<FName>' | Path],
                                                'TrUserData');
                              _ ->
                                  ok
                          end,
                          [replace_tree('#{<FName> := <F>}',
                                        gpb_lib:map_match([{FName, FVar}],
                                                          Opts)),
                           replace_tree('M', MsgVar) | Replacements])
            end
    end;
field_verifier(MsgName, #gpb_oneof{name=FName, fields=OFields},
               FVar, MsgVar, TrUserDataVar, AnRes, Opts) ->
    CfElemPath = [MsgName,FName],
    case gpb_gen_translators:has_translation(CfElemPath, verify, AnRes) of
        {true, Transl} ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                records ->
                    tr_field_oneof_present_undefined_verifier(
                      FName, FVar, Transl, TrUserDataVar);
                #maps{unset_optional=present_undefined} ->
                    tr_field_oneof_present_undefined_verifier(
                      FName, FVar, Transl, TrUserDataVar);
                #maps{unset_optional=omitted, oneof=tuples} ->
                    tr_field_oneof_omitted_tuples_verifier(
                      MsgVar, FName, FVar, Transl, TrUserDataVar, Opts);
                #maps{unset_optional=omitted, oneof=flat} ->
                    %% translate on individual oneof fields instead
                    field_oneof_omitted_flat_verifier(
                      MsgName, FName, OFields,
                      FVar, MsgVar, TrUserDataVar,
                      AnRes, Opts)
            end;
        false ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                records ->
                    field_oneof_present_undefined_verifier(
                      MsgName, FName, OFields,
                      FVar, MsgVar, TrUserDataVar,
                      AnRes);
                #maps{unset_optional=present_undefined} ->
                    field_oneof_present_undefined_verifier(
                      MsgName, FName, OFields,
                      FVar, MsgVar, TrUserDataVar,
                      AnRes);
                #maps{unset_optional=omitted, oneof=tuples} ->
                    field_oneof_omitted_tuples_verifier(
                      MsgName, FName, OFields,
                      FVar, MsgVar, TrUserDataVar,
                      AnRes, Opts);
                #maps{unset_optional=omitted, oneof=flat} ->
                    field_oneof_omitted_flat_verifier(
                      MsgName, FName, OFields,
                      FVar, MsgVar, TrUserDataVar,
                      AnRes, Opts)
            end
    end.

field_oneof_present_undefined_verifier(MsgName, FName, OFields,
                                       FVar, MsgVar, TrUserDataVar,
                                       AnRes) ->
    ?expr(
       case '<F>' of
           undefined ->
               ok;
           '<oneof-pattern>' ->
               '<verify-fn>'('<OFVar>', ['<OFName>', '<FName>' | Path],
                             'TrUserData');
           _ ->
               mk_type_error(invalid_oneof, '<F>', ['<FName>' | Path])
       end,
       [replace_tree('<F>', FVar),
        replace_term('<FName>', FName),
        repeat_clauses(
          '<oneof-pattern>',
          [begin
               FVerifierFn =
                   case Type of
                       {msg,FMsgName} ->
                           gpb_lib:mk_fn(v_msg_, FMsgName);
                       {enum,EnumName} ->
                           gpb_lib:mk_fn(v_enum_, EnumName);
                       Type ->
                           gpb_lib:mk_fn(v_type_, Type)
                   end,
               ElemPath = gpb_gen_translators:mk_elempath_elem(
                            MsgName, F, {true, FName}),
               FVerifierFn2 = gpb_gen_translators:find_translation(
                                ElemPath, verify, AnRes,
                                FVerifierFn),
               OFVar = gpb_lib:prefix_var("O", FVar),
               [replace_tree('M', MsgVar),
                replace_tree('<oneof-pattern>',
                             ?expr({'<OFName>','<OFVar>'})),
                replace_term('<verify-fn>', FVerifierFn2),
                replace_tree('<OFVar>', OFVar),
                replace_term('<OFName>', OFName),
                replace_tree('TrUserData', TrUserDataVar)]
           end
           || #?gpb_field{name=OFName, type=Type}=F <- OFields])]).

tr_field_oneof_present_undefined_verifier(FName, FVar, Transl, TrUserDataVar) ->
    ?expr(if '<F>' =:= undefined ->
                  ok;
             true ->
                  'Tr'('<F>', ['fname' | Path], 'TrUserData')
          end,
          [replace_tree('<F>', FVar),
           replace_term('Tr', Transl),
           replace_term('fname', FName),
           replace_tree('TrUserData', TrUserDataVar)]).


field_oneof_omitted_tuples_verifier(MsgName, FName, OFields,
                                    FVar, MsgVar, TrUserDataVar,
                                    AnRes, Opts) ->
    ?expr(
       case 'M' of
           '<oneof-pattern>' ->
               '<verify-fn>'('<OFVar>', ['<OFName>', '<FName>' | Path],
                             'TrUserData');
           '#{<FName> := <F>}' ->
               mk_type_error(invalid_oneof, '<F>', ['<FName>' | Path]);
           _ ->
               ok
       end,
       [replace_tree('<F>', FVar),
        replace_term('<FName>', FName),
        replace_tree('M', MsgVar),
        replace_tree('#{<FName> := <F>}',
                     gpb_lib:map_match([{FName, FVar}], Opts)),
        repeat_clauses(
          '<oneof-pattern>',
          [begin
               FVerifierFn =
                   case Type of
                       {msg,FMsgName} -> gpb_lib:mk_fn(v_msg_, FMsgName);
                       {enum,EnumName} -> gpb_lib:mk_fn(v_enum_, EnumName);
                       Type -> gpb_lib:mk_fn(v_type_, Type)
                   end,
               ElemPath = gpb_gen_translators:mk_elempath_elem(
                            MsgName, F, {true, FName}),
               FVerifierFn2 = gpb_gen_translators:find_translation(
                                ElemPath, verify, AnRes,
                                FVerifierFn),
               OFVar = gpb_lib:prefix_var("O", FVar),
               Trs1 = [replace_tree('<OFVar>', OFVar),
                       replace_term('<OFName>', OFName)],
               OFPat = ?expr({'<OFName>','<OFVar>'}, Trs1),
               [replace_tree('<oneof-pattern>',
                             gpb_lib:map_match([{FName, OFPat}], Opts)),
                replace_term('<verify-fn>', FVerifierFn2),
                replace_tree('TrUserData', TrUserDataVar)
                | Trs1]
           end
           || #?gpb_field{name=OFName, type=Type}=F <- OFields])]).

tr_field_oneof_omitted_tuples_verifier(MsgVar, FName, FVar,
                                       Transl, TrUserDataVar, Opts) ->
    ?expr(case 'M' of
              '#{fname := F}' ->
                  'Tr'('F', ['fname' | Path], 'TrUserData');
              _ ->
                  ok
          end,
          [replace_tree('M', MsgVar),
           replace_tree('#{fname := F}', gpb_lib:map_match([{FName, FVar}],
                                                           Opts)),
           replace_term('Tr', Transl),
           replace_tree('F', FVar),
           replace_term('fname', FName),
           replace_tree('TrUserData', TrUserDataVar)]).

field_oneof_omitted_flat_verifier(MsgName, FName, OFields,
                                  FVar, MsgVar, TrUserDataVar,
                                  AnRes, Opts) ->
    %% FIXME: Verify at most one oneof field is set
    OFNames = gpb_lib:get_field_names(OFields),
    ?expr(
       case 'M' of
           '#{OFName := OFVar}' ->
               case maps:keys(maps:with('OFNames', 'M')) of
                   [_] ->
                       ok;
                   'OFDups' ->
                       mk_type_error({multiple_oneof_keys, 'OFDups', 'FName'},
                                     'M', ['FName' | Path])
               end,
               'verify-fn'('OFVar', ['OFName' | Path], 'TrUserData');
           _ ->
               ok
       end,
       [replace_tree('M', MsgVar),
        replace_term('FName', FName),
        replace_term('OFNames', OFNames),
        repeat_clauses(
          '#{OFName := OFVar}',
          [begin
               FVerifierFn =
                   case Type of
                       {msg,FMsgName} -> gpb_lib:mk_fn(v_msg_, FMsgName);
                       {enum,EnumName} -> gpb_lib:mk_fn(v_enum_, EnumName);
                       Type -> gpb_lib:mk_fn(v_type_, Type)
                   end,
               ElemPath = gpb_gen_translators:mk_elempath_elem(
                            MsgName, F, {true, FName}),
               FVerifierFn2 = gpb_gen_translators:find_translation(
                                ElemPath, verify, AnRes,
                                FVerifierFn),
               OFVar = gpb_lib:prefix_var("O", FVar),
               Trs1 = [replace_tree('OFVar', OFVar),
                       replace_term('OFName', OFName)],
               [replace_tree('#{OFName := OFVar}',
                             gpb_lib:map_match([{OFName, OFVar}], Opts)),
                replace_tree('OFDups', gpb_lib:prefix_var("OFDups", OFVar)),
                replace_term('verify-fn', FVerifierFn2),
                replace_tree('TrUserData', TrUserDataVar)
                | Trs1]
           end
           || #?gpb_field{name=OFName, type=Type}=F <- OFields])]).

format_enum_verifiers(Defs, #anres{used_types=UsedTypes}, Opts) ->
    [format_enum_verifier(EnumName, Def, Opts)
     || {{enum,EnumName}, Def} <- Defs,
        gpb_lib:smember({enum, EnumName}, UsedTypes)].

format_enum_verifier(EnumName, EnumMembers, Opts) ->
    FnName = gpb_lib:mk_fn(v_enum_, EnumName),
    [gpb_lib:nowarn_unused_function(FnName, 3),
     gpb_lib:nowarn_dialyzer_attr(FnName, 3, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun('<sym>', _Path, _TrUserData) ->
               ok;
          (V, _Path, _TrUserData) when -2147483648 =< V, V =< 2147483647,
                                       is_integer(V) ->
               ok;
          (X, Path, _TrUserData) ->
               mk_type_error({invalid_enum, '<EnumName>'}, X, Path)
       end,
       [repeat_clauses('<sym>', [[replace_term('<sym>', EnumSym)]
                                 || {EnumSym, _Value, _} <- EnumMembers]),
        replace_term('<EnumName>', EnumName)])].

format_type_verifiers(#anres{used_types=UsedTypes}, Opts) ->
    NeedBool   = gpb_lib:smember(bool, UsedTypes),
    NeedFloat  = gpb_lib:smember(float, UsedTypes),
    NeedDouble = gpb_lib:smember(double, UsedTypes),
    NeedString = gpb_lib:smember(string, UsedTypes),
    NeedBytes  = gpb_lib:smember(bytes, UsedTypes),
    [[format_int_verifier(Type, Signedness, Bits, Opts)
      || {Type, Signedness, Bits} <- [{sint32,   signed,   32},
                                      {sint64,   signed,   64},
                                      {int32,    signed,   32},
                                      {int64,    signed,   64},
                                      {uint32,   unsigned, 32},
                                      {uint64,   unsigned, 64},
                                      {fixed32,  unsigned, 32},
                                      {fixed64,  unsigned, 64},
                                      {sfixed32, signed,   32},
                                      {sfixed64, signed,   64}],
         gpb_lib:smember(Type, UsedTypes)],
     [format_bool_verifier(Opts)                || NeedBool],
     [format_float_verifier(float, Opts)        || NeedFloat],
     [format_float_verifier(double, Opts)       || NeedDouble],
     [format_string_verifier(Opts)              || NeedString],
     [format_bytes_verifier(Opts)               || NeedBytes]].

format_int_verifier(IntType, Signedness, NumBits, Opts) ->
    Min = case Signedness of
              unsigned -> 0;
              signed   -> -(1 bsl (NumBits-1))
          end,
    Max = case Signedness of
              unsigned -> 1 bsl NumBits - 1;
              signed   -> 1 bsl (NumBits-1) - 1
          end,
    FnName = gpb_lib:mk_fn(v_type_, IntType),
    [gpb_lib:nowarn_unused_function(FnName, 3),
     gpb_lib:nowarn_dialyzer_attr(FnName, 3, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(N, _Path, _TrUserData) when '<Min>' =< N, N =< '<Max>' ->
               ok;
          (N, Path, _TrUserData) when is_integer(N) ->
               mk_type_error({value_out_of_range, '<details>'}, N, Path);
          (X, Path, _TrUserData) ->
               mk_type_error({bad_integer, '<details>'}, X, Path)
       end,
       [replace_term('<Min>', Min),
        replace_term('<Max>', Max),
        splice_trees('<details>', [erl_syntax:atom(IntType),
                                   erl_syntax:atom(Signedness),
                                   erl_syntax:integer(NumBits)])])].

format_bool_verifier(Opts) ->
    FnName = gpb_lib:mk_fn(v_type_, bool),
    [gpb_lib:nowarn_unused_function(FnName, 3),
     gpb_lib:nowarn_dialyzer_attr(FnName, 3, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(false, _Path, _TrUserData) -> ok;
          (true, _Path, _TrUserData)  -> ok;
          (0, _Path, _TrUserData)  -> ok;
          (1, _Path, _TrUserData)  -> ok;
          (X, Path, _TrUserData) -> mk_type_error(bad_boolean_value, X, Path)
       end)].

format_float_verifier(FlType, Opts) ->
    BadTypeOfValue = list_to_atom(lists:concat(["bad_", FlType, "_value"])),
    FnName = gpb_lib:mk_fn(v_type_, FlType),
    [gpb_lib:nowarn_unused_function(FnName, 3),
     gpb_lib:nowarn_dialyzer_attr(FnName, 3, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(N, _Path, _TrUserData) when is_float(N) -> ok;
          %% It seems a float for the corresponding integer value is
          %% indeed packed when doing <<Integer:32/little-float>>.
          %% So let verify accept integers too.
          %% When such a value is unpacked, we get a float.
          (N, _Path, _TrUserData) when is_integer(N) -> ok;
          (infinity, _Path, _TrUserData)    -> ok;
          ('-infinity', _Path, _TrUserData) -> ok;
          (nan, _Path, _TrUserData)         -> ok;
          (X, Path, _TrUserData) ->
               mk_type_error('<bad_x_value>', X, Path)
       end,
       [replace_term('<bad_x_value>', BadTypeOfValue)])].

format_string_verifier(Opts) ->
    FnName = gpb_lib:mk_fn(v_type_, string),
    [gpb_lib:nowarn_unused_function(FnName, 3),
     gpb_lib:nowarn_dialyzer_attr(FnName, 3, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(S, Path, _TrUserData) when is_list(S); is_binary(S) ->
               try unicode:characters_to_binary(S) of
                   B when is_binary(B) ->
                       ok;
                   {error, _, _} -> %% a non-UTF-8 binary
                       mk_type_error(bad_unicode_string, S, Path)
               catch error:badarg ->
                       mk_type_error(bad_unicode_string, S, Path)
               end;
          (X, Path, _TrUserData) ->
               mk_type_error(bad_unicode_string, X, Path)
       end)].

format_bytes_verifier(Opts) ->
    FnName = gpb_lib:mk_fn(v_type_, bytes),
    [gpb_lib:nowarn_unused_function(FnName, 3),
     gpb_lib:nowarn_dialyzer_attr(FnName, 3, Opts),
     gpb_codegen:format_fn(
       FnName,
       fun(B, _Path, _TrUserData) when is_binary(B) ->
               ok;
          (B, _Path, _TrUserData) when is_list(B) ->
               ok;
          (X, Path, _TrUserData) ->
               mk_type_error(bad_binary_value, X, Path)
       end)].

format_map_verifiers(#anres{map_types=MapTypes}=AnRes, Opts) ->
    MapsOrTuples = gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts),
    [format_map_verifier(KeyType, ValueType, MapsOrTuples, AnRes, Opts)
     || {KeyType,ValueType} <- sets:to_list(MapTypes)].

format_map_verifier(KeyType, ValueType, MapsOrTuples, AnRes, Opts) ->
    MsgName = gpb_lib:map_type_to_msg_name(KeyType, ValueType),
    FnName = gpb_lib:mk_fn(v_, MsgName),
    KeyVerifierFn = gpb_lib:mk_fn(v_type_, KeyType),
    ValueVerifierFn1 = case ValueType of
                           {msg,FMsgName}  -> gpb_lib:mk_fn(v_msg_, FMsgName);
                           {enum,EnumName} -> gpb_lib:mk_fn(v_enum_, EnumName);
                           Type            -> gpb_lib:mk_fn(v_type_, Type)
                       end,
    ElemPath = [MsgName,value],
    ValueVerifierFn2 = gpb_gen_translators:find_translation(
                         ElemPath, verify, AnRes,
                         ValueVerifierFn1),
    [gpb_lib:nowarn_unused_function(FnName, 3),
     gpb_lib:nowarn_dialyzer_attr(FnName, 3, Opts),
     case MapsOrTuples of
         '2tuples' ->
             gpb_codegen:format_fn(
               FnName,
               fun(KVs, Path, TrUserData) when is_list(KVs) ->
                       [case X of
                            {Key, Value} ->
                                'VerifyKey'(Key, ['key' | Path], TrUserData),
                                'VerifyValue'(Value, ['value' | Path],
                                              TrUserData);
                            _ ->
                                mk_type_error(invalid_key_value_tuple, X, Path)
                        end
                        || X <- KVs],
                       ok;
                  (X, Path, _TrUserData) ->
                       mk_type_error(invalid_list_of_key_value_tuples, X, Path)
               end,
               [replace_term('VerifyKey', KeyVerifierFn),
                replace_term('VerifyValue', ValueVerifierFn2)]);
         maps ->
             gpb_codegen:format_fn(
               FnName,
               fun(M, Path, TrUserData) when is_map(M) ->
                       [begin
                            'VerifyKey'(Key, ['key' | Path], TrUserData),
                            'VerifyValue'(Value, ['value' | Path],
                                         TrUserData)
                        end
                        || {Key, Value} <- maps:to_list(M)],
                       ok;
                  (X, Path, _TrUserData) ->
                       mk_type_error(invalid_map, X, Path)
               end,
               [replace_term('VerifyKey', KeyVerifierFn),
                replace_term('VerifyValue', ValueVerifierFn2)])
     end].

format_verifier_auxiliaries(Defs, Opts) ->
    [gpb_lib:nowarn_unused_function(mk_type_error, 3),
     "-spec mk_type_error(_, _, list()) -> no_return().\n",
     gpb_codegen:format_fn(
       mk_type_error,
       fun(Error, ValueSeen, Path) ->
               Path2 = prettify_path(Path),
               erlang:error({gpb_type_error,
                             {Error, [{value, ValueSeen},{path, Path2}]}})
       end),
     "\n",
     case gpb_lib:contains_messages(Defs) of
         false ->
             gpb_codegen:format_fn(
               prettify_path,
               fun([]) -> top_level end);
         true ->
             [gpb_lib:nowarn_unused_function(prettify_path, 1),
              gpb_lib:nowarn_dialyzer_attr(prettify_path, 1, Opts),
              case gpb_lib:target_has_lists_join(Opts) of
                  true ->
                      format_prettify_path_with_lists_join();
                  false ->
                      format_prettify_path_with_string_join()
              end]
     end].

format_prettify_path_with_lists_join() ->
    gpb_codegen:format_fn(
      prettify_path,
      fun([]) ->
              top_level;
         (PathR) ->
              lists:append(
                lists:join(".", lists:map(fun atom_to_list/1,
                                          lists:reverse(PathR))))
      end).

format_prettify_path_with_string_join() ->
    gpb_codegen:format_fn(
      prettify_path,
      fun([]) ->
              top_level;
         (PathR) ->
              string:join(lists:map(fun atom_to_list/1,
                                    lists:reverse(PathR)),
                          ".")
      end).

map_keys_to_strees(Keys, Opts) ->
    case gpb_lib:get_maps_key_type_by_opts(Opts) of
        atom   -> [atom_to_stree(Key) || Key <- Keys];
        binary -> [atom_to_binary_string_stree(Key) || Key <- Keys]
    end.

atom_to_stree(Atom) ->
    erl_syntax:atom(Atom).

atom_to_binary_string_stree(Atom) ->
    erl_syntax:binary(
      [erl_syntax:binary_field(
         erl_syntax:string(atom_to_list(Atom)))]).
