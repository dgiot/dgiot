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

%%% @doc Generation of msg-merging functions.
%%%
%%% Merging is an integral part of decoding: optional and required
%%% messages that occur multiple times on the wire are merged
%%% recursively. Scalar optional or required fields a merged by
%%% overwriting. Repeated fields are merged by appending.
%%%
%%% @private

-module(gpb_gen_mergers).

-export([format_exports/2]).
-export([format_msg_merge_code/3]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2,
                  splice_trees/2, repeat_clauses/2]).

format_exports(_Defs, Opts) ->
    case gpb_lib:get_records_or_maps_by_opts(Opts) of
        records ->
            ?f("-export([merge_msgs/2, merge_msgs/3, merge_msgs/4]).~n");
        maps ->
            ?f("-export([merge_msgs/3, merge_msgs/4]).~n")
    end.

format_msg_merge_code(Defs, AnRes, Opts) ->
    case gpb_lib:contains_messages(Defs) of
        true  -> format_msg_merge_code_msgs(Defs, AnRes, Opts);
        false -> format_msg_merge_code_no_msgs(Opts)
    end.

format_msg_merge_code_no_msgs(Opts) ->
    case gpb_lib:get_records_or_maps_by_opts(Opts) of
        records ->
            ["-spec merge_msgs(_, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New) ->
                       merge_msgs(Prev, New, [])
               end),
             "-spec merge_msgs(_, _, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(_Prev, _New, _MsgNameOrOpts) ->
                       erlang:error({gpb_error, no_messages})
               end),
             gpb_codegen:format_fn(
               merge_msgs,
               fun(_Prev, _New, _MsgName, _Opts) ->
                       erlang:error({gpb_error, no_messages})
               end)];
        maps ->
            ["-spec merge_msgs(_, _, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(Prev, New, MsgName) ->
                       merge_msgs(Prev, New, MsgName, [])
               end),
             "-spec merge_msgs(_, _, _, _) -> no_return().\n",
             gpb_codegen:format_fn(
               merge_msgs,
               fun(_Prev, _New, _MsgName, _Opts) ->
                       erlang:error({gpb_error, no_messages})
               end)]
    end.

format_msg_merge_code_msgs(Defs, AnRes, Opts) ->
    MsgNames = [MsgName || {{msg, MsgName}, _MsgDef} <- Defs],
    [format_merge_msgs_top_level(MsgNames, AnRes, Opts),
     [case Type of
          msg ->
              format_msg_merger(Name, MsgDef, AnRes, Opts);
          group ->
              case is_repeated_group(Name, AnRes) of
                  true ->
                      %% merged with seq-add, not exported as top-level
                      %% ==> such groups are never merged recursively,
                      %%     thus never called
                      [];
                  false ->
                      format_msg_merger(Name, MsgDef, AnRes, Opts)
              end
      end
      || {Type, Name, MsgDef} <- gpb_lib:msgs_or_groups(Defs)]].

is_repeated_group(GroupName, #anres{group_occurrences=D}) ->
    dict:fetch(GroupName, D) == repeated.

format_merge_msgs_top_level(MsgNames, AnRes, Opts) ->
    Mapping = gpb_lib:get_records_or_maps_by_opts(Opts),
    [[gpb_codegen:format_fn(
       merge_msgs,
       fun(Prev, New) when element(1,Prev) =:= element(1,New) ->
               merge_msgs(Prev, New, element(1,Prev), [])
       end) || Mapping == records],
     gpb_codegen:format_fn(
       merge_msgs,
       fun(Prev, New, MsgName) when is_atom(MsgName) ->
               merge_msgs(Prev, New, MsgName, []);
          ('Prev', New, Opts) when element(1,'Prev') =:= element(1,New),
                                   is_list(Opts) ->
               merge_msgs(Prev, New, element(1,'Prev'), Opts)
       end,
       [repeat_clauses('Prev', [[replace_tree('Prev', ?expr(Prev))]
                                || Mapping == records])]),
     gpb_codegen:format_fn(
       merge_msgs,
       fun(Prev, New, MsgName, Opts) ->
               TrUserData = proplists:get_value(user_data, Opts),
               case MsgName of
                   '<msg-name>' -> '<merge-msg>'(Prev, New, TrUserData)
               end
       end,
       [repeat_clauses(
          '<msg-name>',
          [begin
               DefaultMergeFn = gpb_lib:mk_fn(merge_msg_, MsgName),
               ElemPath = [MsgName],
               MMergeFn = case gpb_gen_translators:has_translation(
                                 ElemPath, merge, AnRes) of
                              {true, Transl} -> Transl;
                              false -> DefaultMergeFn
                          end,
               [replace_term('<msg-name>', MsgName),
                replace_term('<merge-msg>', MMergeFn)]
           end
           || MsgName <- MsgNames])])].

format_msg_merger(MsgName, [], _AnRes, _Opts) ->
    FnName = gpb_lib:mk_fn(merge_msg_, MsgName),
    [gpb_lib:nowarn_unused_function(FnName, 3),
     gpb_codegen:format_fn(
       FnName,
       fun(_Prev, New, _TrUserData) -> New end)];
format_msg_merger(MsgName, MsgDef, AnRes, Opts) ->
    FnName = gpb_lib:mk_fn(merge_msg_, MsgName),
    TrUserDataVar = ?expr(TrUserData),
    {PrevMatch, NewMatch, ExtraInfo} =
        format_msg_merger_fnclause_match(MsgName, MsgDef, Opts),
    {MandatoryMergings, OptMergings} = compute_msg_field_mergers(
                                         ExtraInfo, MsgName, AnRes, Opts),
    [gpb_lib:nowarn_unused_function(FnName, 3),
     gpb_codegen:format_fn(
       FnName,
       fun('Prev', 'New', 'MaybeTrUserData') ->
               '<merge-it>'
       end,
       [replace_tree('Prev',  PrevMatch),
        replace_tree('New',   NewMatch),
        splice_trees(
          '<merge-it>',
          gpb_lib:do_exprs(
            fun(Elem, Var) ->
                    render_omissible_merger(Elem, Var, TrUserDataVar, Opts)
            end,
            render_field_mergers(MsgName, MandatoryMergings,
                                 TrUserDataVar, Opts),
            OptMergings)),
        replace_tree('TrUserData', TrUserDataVar), % needed by some field mergers
        replace_tree('MaybeTrUserData',
                     case gpb_lib:any_field_is_sub_msg(MsgDef)
                         orelse gpb_lib:any_field_is_repeated(MsgDef)
                         orelse gpb_gen_translators:exists_tr_for_msg(
                                  MsgName, merge, AnRes) of
                         true  -> TrUserDataVar;
                         false -> ?expr(_)
                     end)])].

format_msg_merger_fnclause_match(_MsgName, [], _Opts) ->
    {?expr(PF), ?expr(_), no_fields};
format_msg_merger_fnclause_match(MsgName, MsgDef, Opts) ->
    FNames  = [gpb_lib:get_field_name(Field) || Field <- MsgDef],
    PFVars  = [case is_required_overwrite_merge(Field) of
                   true  -> none;
                   false -> gpb_lib:var("PF~s", [gpb_lib:get_field_name(Field)])
               end
               || Field <- MsgDef],
    NFVars  = [gpb_lib:var("NF~s", [FName]) || FName <- FNames],
    PFields = lists:zip(FNames, PFVars),
    NFields = lists:zip(FNames, NFVars),
    Infos = zip4(FNames, PFVars, NFVars, MsgDef),
    case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
        records ->
            PFields1 = [{FName,PFVar} || {FName,PFVar} <- PFields,
                                         PFVar /= none],
            P = gpb_lib:mapping_match(MsgName, PFields1, Opts),
            N = gpb_lib:mapping_match(MsgName, NFields, Opts),
            {P, N, {pr, Infos}};
        #maps{unset_optional=present_undefined} ->
            PFields1 = [{FName,PFVar} || {FName,PFVar} <- PFields,
                                         PFVar /= none],
            P = gpb_lib:mapping_match(MsgName, PFields1, Opts),
            N = gpb_lib:mapping_match(MsgName, NFields, Opts),
            {P, N, {pr, Infos}};
        #maps{unset_optional=omitted} ->
            {OptInfos, MandInfos} = gpb_lib:key_partition_on_optionality(4,
                                                                         Infos),
            PMsg = ?expr(PMsg),
            NMsg = ?expr(NMsg),
            P = gpb_lib:map_match(
                  [{FName, PFVar} || {FName,PFVar,_,_} <- MandInfos,
                                     PFVar /= none],
                  Opts),
            N = gpb_lib:map_match(
                  [{FName, NFVar} || {FName,_,NFVar,_} <- MandInfos],
                  Opts),
            PB = gpb_lib:match_bind_var(P, PMsg),
            NB = gpb_lib:match_bind_var(N, NMsg),
            XInfo = {om, {MandInfos, OptInfos, PMsg, NMsg}},
            case {MandInfos, OptInfos} of
                {_, []} -> {P, N, XInfo};
                {[], _} -> {PMsg, NMsg, XInfo};
                {_,  _} -> {PB, NB, XInfo}
            end
    end.

is_required_overwrite_merge(#?gpb_field{occurrence=required}=Field) ->
    gpb_lib:classify_field_merge_action(Field) == overwrite;
is_required_overwrite_merge(_Field) ->
    false.

compute_msg_field_mergers({pr, XInfo}, MsgName, AnRes, Opts) ->
    Merges =
        [{FName, format_field_merge_expr(Field, PFVar, NFVar, MsgName,
                                         AnRes, Opts)}
         || {FName, PFVar, NFVar, Field} <- XInfo],
    {Merges, []};
compute_msg_field_mergers({om, {MandXInfo, OptXInfo, PMsg, NMsg}},
                          MsgName, AnRes, Opts) ->
    {MandMergs, []} = compute_msg_field_mergers({pr, MandXInfo},
                                                MsgName, AnRes, Opts),
    {OptMergs, []} = compute_msg_field_mergers({pr, OptXInfo},
                                               MsgName, AnRes, Opts),
    {MandMergs, reshape_cases_for_maps_find(OptMergs, PMsg, NMsg)}.

format_field_merge_expr(#?gpb_field{name=FName, occurrence=Occur}=Field,
                        PF, NF, MsgName, AnRes, _Opts)->
    case gpb_lib:classify_field_merge_action(Field) of
        overwrite when Occur == required ->
            {required, {PF, NF}};
        overwrite ->
            {overwrite, {PF, NF}};
        seqadd ->
            ElemPath = [MsgName, FName],
            Append = gpb_gen_translators:find_translation(
                       ElemPath, merge, AnRes, 'erlang_++'),
            Tr = fun (_,_) -> Append end,
            {cond_merge, {{PF, NF}, Tr, 'erlang_++'}};
        msgmerge when Occur == required ->
            Tr = gpb_gen_translators:mk_find_tr_fn_elem_or_default(
                   MsgName, Field, false, AnRes),
            #?gpb_field{type={_msg_or_group,SubMsgName}}=Field,
            MergeFn = gpb_lib:mk_fn(merge_msg_, SubMsgName),
            {uncond_merge, {{PF, NF}, Tr, MergeFn}};
        msgmerge ->
            Tr = gpb_gen_translators:mk_find_tr_fn_elem_or_default(
                   MsgName, Field, false, AnRes),
            #?gpb_field{type={_msg_or_group,SubMsgName}}=Field,
            MergeFn = gpb_lib:mk_fn(merge_msg_, SubMsgName),
            {cond_merge, {{PF, NF}, Tr, MergeFn}}
    end;
format_field_merge_expr(#gpb_oneof{name=CFName, fields=OFields},
                        PF, NF, MsgName, AnRes, Opts) ->
    case [OField || #?gpb_field{type={msg,_}}=OField <- OFields] of
        [] ->
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                #maps{unset_optional=omitted, oneof=flat} ->
                    Keys = gpb_lib:get_field_names(OFields),
                    {overwrite_drop, {{PF, NF}, Keys}};
                _ ->
                    {overwrite, {PF, NF}}
            end;
        MOFields ->
            OFMerges =
                [begin
                     Tr = gpb_gen_translators:mk_find_tr_fn_elem_or_default(
                            MsgName, F, {true, CFName}, AnRes),
                     {OFName, Tr, gpb_lib:mk_fn(merge_msg_, M2Name)}
                 end
                 || #?gpb_field{name=OFName, type={msg,M2Name}}=F <- MOFields],
            case gpb_lib:get_mapping_and_unset_by_opts(Opts) of
                #maps{unset_optional=omitted, oneof=flat} ->
                    Keys = gpb_lib:get_field_names(OFields),
                    {oneof_drop, {{PF, NF}, OFMerges, Keys}};
                _ ->
                    {oneof, {{PF, NF}, OFMerges}}
            end
    end.

reshape_cases_for_maps_find(Merges, PMsg, NMsg) ->
    [{FName, case Merge of
                 {overwrite, {_, _}} ->
                     {overwrite, {PMsg, NMsg}};
                 {overwrite_drop, {{_, _}, Keys}} ->
                     {overwrite_drop, {{PMsg, NMsg}, Keys}};
                 {cond_merge, {{_, _}, Tr, MergeFn}} ->
                     {cond_merge, {{PMsg, NMsg}, Tr, MergeFn}};
                 {oneof, {{_, _}, OFMerges}} ->
                     {oneof, {{PMsg, NMsg}, OFMerges}};
                 {oneof_drop, {{_, _}, OFMerges, Keys}} ->
                     {oneof_drop, {{PMsg, NMsg}, OFMerges, Keys}};
                 {expr, Expr} ->
                     {expr, Expr}
             end}
     || {FName, Merge} <- Merges].

render_field_mergers(MsgName, Mergings, TrUserDataVar, Opts) ->
    Fields = [{FName, render_field_merger(Merge, TrUserDataVar)}
              || {FName, Merge} <- Mergings],
    gpb_lib:mapping_create(MsgName, Fields, Opts).

render_field_merger({required, {none, NF}}, _TrUserDataVar) ->
    NF;
render_field_merger({overwrite, {PF, NF}}, _TrUserDataVar) ->
    ?expr(if 'NF' =:= undefined -> 'PF';
             true               -> 'NF'
          end,
          [replace_tree('PF', PF),
           replace_tree('NF', NF)]);
render_field_merger({expr, Expr}, _TrUserDataVar) ->
    Expr;
render_field_merger({uncond_merge, {{PF, NF}, Tr, MergeFn}}, TrUserDataVar) ->
    ?expr('merge'('PF', 'NF', 'TrUserData'),
          [replace_tree('PF', PF),
           replace_tree('NF', NF),
           replace_term('merge', Tr(merge, MergeFn)),
           replace_tree('TrUserData', TrUserDataVar)]);
render_field_merger({cond_merge, {{PF, NF}, Tr, MergeFn}}, TrUserDataVar) ->
    ?expr(if 'PF' /= undefined, 'NF' /= undefined -> 'merge'('PF', 'NF',
                                                             'TrUserData');
             'PF' == undefined -> 'NF';
             'NF' == undefined -> 'PF'
          end,
          [replace_tree('PF', PF),
           replace_tree('NF', NF),
           replace_term('merge', Tr(merge, MergeFn)),
           replace_tree('TrUserData', TrUserDataVar)]);
render_field_merger({oneof, {{PF, NF}, OFMerges}}, TrUserDataVar) ->
    Transforms = [replace_tree('PF', PF),
                  replace_tree('NF', NF),
                  replace_tree('OPF', gpb_lib:prefix_var("O", PF)),
                  replace_tree('ONF', gpb_lib:prefix_var("O", NF)),
                  replace_tree('TrUserData', TrUserDataVar)],
    ?expr(case {'PF', 'NF'} of
              '{{tag,OPF},{tag,ONF}}' -> {'tag', 'merge'('OPF','ONF',
                                                         'TrUserData')};
              {_, undefined}          -> 'PF';
              _                       -> 'NF'
          end,
          [repeat_clauses(
             '{{tag,OPF},{tag,ONF}}',
             [[replace_tree('{{tag,OPF},{tag,ONF}}',
                            ?expr({{'tag','OPF'},{'tag','ONF'}})),
               replace_term('tag', OFName),
               replace_term('merge', Tr(merge, OFMergeFn)) | Transforms]
              || {OFName, Tr, OFMergeFn} <- OFMerges])
           | Transforms]).

render_omissible_merger({FName, {overwrite, {PMsg, NMsg}}}, Var,
                        TrUserDataVar, Opts) ->
    ?expr(case {'PMsg', 'NMsg'} of
              {_, '#{fname := NF}'} -> 'Var#{fname=>NF}';
              {'#{fname := PF}', _} -> 'Var#{fname=>PF}';
              _                     -> 'Var'
          end,
          std_omitable_merge_transforms(PMsg, NMsg, FName, Var, TrUserDataVar,
                                        Opts));
render_omissible_merger({_CFName, {overwrite_drop, {{PMsg, NMsg}, FNames}}},
                        Var,
                        TrUserDataVar, Opts) ->
    Trs = std_omitable_merge_transforms(PMsg, NMsg, x, Var, TrUserDataVar,
                                        Opts),
    ?expr(case {'PMsg', 'NMsg'} of
              '{_, #{fname := NF}}' -> 'Var#{fname=>NF}';
              '{#{fname := PF}, _}' -> 'Var#{fname=>PF}';
              _                     -> 'Var'
          end,
          [repeat_clauses(
             '{_, #{fname := NF}}',
             [begin
                  StdXforms = std_omitable_merge_transforms(
                                PMsg, NMsg, FName,
                                Var, TrUserDataVar, Opts),
                  Match = ?expr({_, '#{fname := NF}'}, StdXforms),
                  NF = gpb_lib:var("NF~s", [FName]),
                  OverwriteExpr = gpb_lib:map_set(Var, [{FName, NF}], Opts),
                  [replace_tree('{_, #{fname := NF}}', Match),
                   replace_tree('Var#{fname=>NF}', OverwriteExpr)]
              end
              || FName <- FNames]),
           repeat_clauses(
             '{#{fname := PF}, _}',
             [begin
                  StdXforms = std_omitable_merge_transforms(
                                PMsg, NMsg, FName,
                                Var, TrUserDataVar, Opts),
                  Match = ?expr({'#{fname := PF}', _}, StdXforms),
                  PF = gpb_lib:var("PF~s", [FName]),
                  OverwriteExpr = gpb_lib:map_set(Var, [{FName, PF}], Opts),
                  [replace_tree('{#{fname := PF}, _}', Match),
                   replace_tree('Var#{fname=>PF}', OverwriteExpr)]
              end
              || FName <- FNames])
          | Trs]);
render_omissible_merger({FName, {cond_merge, {{PMsg, NMsg}, Tr, MergeFn}}}, Var,
                        TrUserDataVar, Opts) ->
    Trs = std_omitable_merge_transforms(PMsg, NMsg, FName, Var, TrUserDataVar,
                                        Opts),
    MergeCallTmpl = ?expr('merge'('PF','NF', 'TrUserData'), Trs),
    ?expr(case {'PMsg', 'NMsg'} of
              {'#{fname := PF}', '#{fname := NF}'} ->
                  'Var#{fname=>merge(PF,NF)}';
              {_, '#{fname := NF}'} ->
                  'Var#{fname=>NF}';
              {'#{fname := PF}', _} ->
                  'Var#{fname=>PF}';
              {_, _} ->
                  'Var'
          end,
          [replace_tree('Var#{fname=>merge(PF,NF)}',
                        gpb_lib:map_set(Var, [{FName,MergeCallTmpl}], Opts))]
          ++ Trs
          ++ [replace_term('merge', Tr(merge, MergeFn))]);
render_omissible_merger({FName, {oneof, {{PMsg, NMsg}, OFMerges}}}, Var,
                       TrUserDataVar, Opts) ->
    OPF = gpb_lib:var("OPF~s", [FName]),
    ONF = gpb_lib:var("ONF~s", [FName]),
    OneofTransforms = [replace_tree('OPF', OPF),
                       replace_tree('ONF', ONF)],
    ?expr(case {'PMsg', 'NMsg'} of
              '{#{fname := {tag,OPF}}, #{fname := {tag,ONF}}}' ->
                  'Var#{fname=>{tag,merge(OPF,ONF)}}';
              {_, '#{fname := NF}'} ->
                  'Var#{fname=>NF}';
              {'#{fname := PF}', _} ->
                  'Var#{fname=>PF}';
              {_, _} ->
                  'Var'
          end,
          [repeat_clauses(
             '{#{fname := {tag,OPF}}, #{fname := {tag,ONF}}}',
             [begin
                  Trs2 = [replace_term('tag', OFName),
                          replace_term('merge', Tr(merge, OFMergeFn))]
                      ++ OneofTransforms,
                  MmO = gpb_lib:map_match(
                          [{FName, ?expr({'tag', 'OPF'}, Trs2)}],
                          Opts),
                  MmN = gpb_lib:map_match(
                          [{FName, ?expr({'tag', 'ONF'}, Trs2)}],
                          Opts),
                  MergeCall = ?expr({'tag','merge'('OPF','ONF', 'TrUserData')},
                                    Trs2),
                  [replace_tree(
                     '{#{fname := {tag,OPF}}, #{fname := {tag,ONF}}}',
                     ?expr({'#{fname := {tag,OPF}}', '#{fname := {tag,ONF}}'},
                           [replace_tree('#{fname := {tag,OPF}}', MmO),
                            replace_tree('#{fname := {tag,ONF}}', MmN)])),
                   replace_tree('Var#{fname=>{tag,merge(OPF,ONF)}}',
                                gpb_lib:map_set(Var, [{FName,MergeCall}], Opts))
                   | Trs2]
              end
              || {OFName, Tr, OFMergeFn} <- OFMerges])
           | std_omitable_merge_transforms(PMsg, NMsg, FName, Var,
                                           TrUserDataVar, Opts)]);
render_omissible_merger({_FName,
                         {oneof_drop, {{PMsg, NMsg}, OFMerges, FNames}}},
                        Var,
                        TrUserDataVar, Opts) ->
    ?expr(case {'PMsg', 'NMsg'} of
              '{#{fname := PF}, #{fname := NF}}' ->
                  'Var#{fname=>merge(PF,NF)}';
              '{_, #{fname := NF}}' ->
                  'Var#{fname=>NF}';
              '{#{fname := PF}, _}' ->
                  'Var#{fname=>PF}';
              _ -> 'Var'
          end,
          [repeat_clauses(
             '{#{fname := PF}, #{fname := NF}}',
             [begin
                  PF = gpb_lib:var("PF~s", [OFName]),
                  NF = gpb_lib:var("NF~s", [OFName]),
                  Trs = std_omitable_merge_transforms(PMsg, NMsg, OFName,
                                                      Var, TrUserDataVar,
                                                      Opts),
                  Trs2 = Trs ++ [replace_term('merge', Tr(merge, OFMergeFn))],
                  MmO = gpb_lib:map_match([{OFName, PF}], Opts),
                  MmN = gpb_lib:map_match([{OFName, NF}], Opts),
                  MergeCall = ?expr('merge'('PF','NF', 'TrUserData'),
                                    Trs2),
                  [replace_tree(
                     '{#{fname := PF}, #{fname := NF}}',
                     ?expr({'#{fname := PF}', '#{fname := NF}'},
                           [replace_tree('#{fname := PF}', MmO),
                            replace_tree('#{fname := NF}', MmN)])),
                   replace_tree('Var#{fname=>merge(PF,NF)}',
                                gpb_lib:map_set(Var, [{OFName,MergeCall}],
                                                Opts))
                   | Trs2]
              end
              || {OFName, Tr, OFMergeFn} <- OFMerges]),
           repeat_clauses(
             '{_, #{fname := NF}}',
             [begin
                  Trs = std_omitable_merge_transforms(PMsg, NMsg, FName,
                                                      Var, TrUserDataVar,
                                                      Opts),
                  StdXforms = std_omitable_merge_transforms(
                                PMsg, NMsg, FName,
                                Var, TrUserDataVar, Opts),
                  Match = ?expr({_, '#{fname := NF}'}, StdXforms),
                  NF = gpb_lib:var("NF~s", [FName]),
                  OverwriteExpr = gpb_lib:map_set(Var, [{FName, NF}],
                                                  Opts),
                  [replace_tree('{_, #{fname := NF}}', Match),
                   replace_tree('Var#{fname=>NF}', OverwriteExpr)
                   | Trs]
              end
              || FName <- FNames]),
           repeat_clauses(
             '{#{fname := PF}, _}',
             [begin
                  Trs = std_omitable_merge_transforms(PMsg, NMsg, FName,
                                                      Var, TrUserDataVar,
                                                      Opts),
                  StdXforms = std_omitable_merge_transforms(
                                PMsg, NMsg, FName,
                                Var, TrUserDataVar, Opts),
                  Match = ?expr({'#{fname := PF}', _}, StdXforms),
                  PF = gpb_lib:var("PF~s", [FName]),
                  OverwriteExpr = gpb_lib:map_set(Var, [{FName, PF}],
                                                 Opts),
                  [replace_tree('{#{fname := PF}, _}', Match),
                   replace_tree('Var#{fname=>PF}', OverwriteExpr)
                  | Trs]
              end
              || FName <- FNames])
           | std_omitable_merge_transforms(PMsg, NMsg, dummy_fname, Var,
                                           TrUserDataVar, Opts)]).

std_omitable_merge_transforms(PMsg, NMsg, FName, Var, TrUserDataVar, Opts) ->
    PF = gpb_lib:var("PF~s", [FName]),
    NF = gpb_lib:var("NF~s", [FName]),
    [replace_term('fname', FName),
     replace_tree('PMsg', PMsg),
     replace_tree('NMsg', NMsg),
     replace_tree('PF', PF),
     replace_tree('NF', NF),
     replace_tree('Var', Var),
     replace_tree('Var#{fname=>NF}', gpb_lib:map_set(Var, [{FName, NF}],
                                                     Opts)),
     replace_tree('Var#{fname=>PF}', gpb_lib:map_set(Var, [{FName, PF}],
                                                     Opts)),
     replace_tree('#{fname := NF}', gpb_lib:map_match([{FName, NF}], Opts)),
     replace_tree('#{fname := PF}', gpb_lib:map_match([{FName, PF}], Opts)),
     replace_tree('#{fname := _}', gpb_lib:map_match([{FName, ?expr(_)}],
                                                     Opts)),
     replace_tree('TrUserData', TrUserDataVar)].

zip4([A|T1], [B|T2], [C|T3], [D|T4]) -> [{A,B,C,D} | zip4(T1, T2, T3, T4)];
zip4([], [], [], [])                 -> [].
