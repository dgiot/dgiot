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

%%% @doc Generation of translation functions.
%%% This can translate google.protobuf.Any messages into more
%%% useful values, eg by doing yet another interior decoding or
%%% encoding.
%%%
%%% @private

-module(gpb_gen_translators).

-export([format_translators/3]).
-export([format_aux_transl_helpers/0]).
-export([format_merge_translators/3]).

-export([mk_find_tr_fn/3]).
-export([mk_find_tr_fn_elem/4]).
-export([mk_find_tr_fn_elem_or_default/3]).
-export([mk_find_tr_fn_elem_or_default/4]).
-export([mk_elempath_elem/3]).
-export([find_translation/3, find_translation/4]).
-export([has_translation/3]).
-export([has_type_spec_translation/2]).
-export([default_fn_by_op/2]).
-export([default_merge_translator/0]).
-export([default_verify_translator/0]).
-export([exists_tr_for_msg/3]).
-export([args_by_op2/1]).
-export([maybe_userdata_param/2]).

-include("../include/gpb.hrl").
-include("gpb_codegen.hrl").
-include("gpb_compile.hrl").

-import(gpb_lib, [replace_term/2, replace_tree/2, splice_trees/2]).

format_translators(_Defs, #anres{translations=Ts}=AnRes, Opts) ->
    [[[format_field_op_translator(ElemPath, Op, CallTemplates, Opts)
       || {Op, CallTemplates} <- OpTransls,
          Op /= type_spec]
      || {ElemPath, OpTransls} <- dict:to_list(Ts)],
     format_default_translators(AnRes, Opts)].

format_merge_translators(_Defs, #anres{translations=Ts}=AnRes, Opts) ->
    [[[format_field_op_translator(ElemPath, Op, CallTemplates, Opts)
       || {Op, CallTemplates} <- OpTransls,
          Op == merge]
      || {ElemPath, OpTransls} <- dict:to_list(Ts)],
     format_default_merge_translators(AnRes, Opts)].

format_field_op_translator(ElemPath, Op, CallTemplates, Opts) ->
    FnName = mk_tr_fn_name(ElemPath, Op),
    {InPatterns, Body} =
        stack_transl_calls(
          Op, lists:unzip(
                [format_field_op_translator(Op, CallTemplate)
                 || CallTemplate <- CallTemplates])),
    [inline_attr(FnName,length(InPatterns)),
     if Op == verify ->
             %% Dialyzer might complain that "The created fun has no
             %% local return", for a $errorf, which is true, but also
             %% not surprising, so shut this warning down.
             gpb_lib:nowarn_dialyzer_attr(FnName,length(InPatterns),Opts);
        true ->
             ""
     end,
     gpb_codegen:format_fn(
       FnName,
       fun('$$InPatterns') ->
               '$$Body'
       end,
       [splice_trees('$$InPatterns', InPatterns),
        splice_trees('$$Body', Body)])].

%% When there are several translation calls, stack them like this:
%% Params:
%%   Ins = list of function param patterns
%%         for example: [(X, _, _), (X, Y, _), (X, _, _)]
%%   Bodyies: list of calls,
%%            for example [call1(X), call2(X, Y), call3(X, hello_world)]
%%
%% Assumption: Function param patterns are either single variable or underscore
%%             Function param pattenrs that are variables all have the same
%%             variable at each position, ie never like [(X, _), (Y, _)]
%%
%% Results:
%%   Function param pattern: (X, Y, _) in the example, ie a variable
%%     (the variable) at each position unless all Ins have underscore
%%     at that position.
%%   Body: In the example, the stacked body will be these three
%%     expressions:
%%       Res1 = call1(X),
%%       Res2 = call2(Res1, Y),
%%       call3(Res2, hello_world)
%%     The decision to replace X and not say Y, with the variable of the
%%     intermediate result is given by the out_to_in_by_op function.
stack_transl_calls(Op, {[Ins1 | RestInPatterns], [Body1 | RestBodies]}) ->
    MergedBody =
        gpb_lib:do_exprs(
          fun({I, {InPatterns, Body}}, Var) ->
                  Pos = out_to_in_by_op(Op),
                  Body2 = replace_inpattern(lists:nth(Pos, Ins1), Body, Var),
                  %% New vars in body
                  VarsToKeep = extract_vars(InPatterns ++ [Var]),
                  unquify_free_vars(Body2, VarsToKeep, I)
          end,
          Body1,
          gpb_lib:index_seq(lists:zip(RestInPatterns, RestBodies))),
    MergedInPatterns =
        lists:foldl(
          fun(InPatterns, Acc) ->
                  [case {is_underscore(InPattern), is_underscore(AccPattern)} of
                       {true, true}   -> AccPattern;
                       {false, true}  -> InPattern;
                       {true, false}  -> AccPattern;
                       {false, false} -> AccPattern  % assume same var
                   end
                   || {InPattern, AccPattern} <- lists:zip(InPatterns, Acc)]
          end,
          Ins1,
          RestInPatterns),
    {MergedInPatterns, MergedBody}.

replace_inpattern(InPattern, Body, Var) ->
    erl_syntax_lib:map(
      fun(Node) ->
              if Node == InPattern -> Var;
                 true -> Node
              end
      end,
      Body).

extract_vars(Patterns) ->
    [erl_syntax:variable_name(Pat) || Pat <- Patterns,
                                      erl_syntax:type(Pat) == variable].

unquify_free_vars(Body, VarsToKeep, I) ->
    erl_syntax_lib:map(
      fun(Node) ->
              case erl_syntax:type(Node) of
                  variable ->
                      VarName = erl_syntax:variable_name(Node),
                      case lists:member(VarName, VarsToKeep) of
                          true ->
                              Node;
                          false ->
                              VarName1 = lists:concat([VarName, I]),
                              Node1 = erl_syntax:variable(VarName1),
                              erl_syntax:copy_pos(Node, Node1)
                      end;
                  _ ->
                      Node
              end
      end,
      Body).

is_underscore(Pattern) ->
    erl_syntax:type(Pattern) =:= underscore.

format_field_op_translator(Op, CallTemplate) ->
    ArgTemplate = last_tuple_element(CallTemplate),
    {InArgs, OutArgs0, Relations0} =
        if Op /= verify ->
                Ins = Outs = tr_in_args_by_op(Op),
                InOutNames = [Name || {Name,_Value} <- Outs],
                {Ins, Outs, mk_pass_straight_through_rel(InOutNames)};
           Op == verify ->
                [{_,V},{_,Path},{_,UserData}] = Ins = tr_in_args_by_op(Op),
                ErrorF = ?expr(fun(Reason) ->
                                       mk_type_error(Reason, 'Actual', 'Path')
                               end,
                               [replace_tree('Actual', V),
                                replace_tree('Path', Path)]),
                Outs = [{'$1',V},{'$errorf',ErrorF},{'$user_data',UserData}],
                Rels = [{'$1',['$1']},
                        %% $errorf uses $1,$2 if present, else $1,$2 are used
                        {'$errorf',['$1','$2'], ['$1','$2']},
                        {'$user_data',['$user_data']}],
                {Ins, Outs, Rels}
        end,
    OutArgs = OutArgs0 ++ [{'$op', erl_syntax:abstract(Op)}],
    Relations = Relations0 ++ [{'$op', []}],
    {InPatterns, OutParams, _UsedInNames, UsedOutNames} =
        process_tr_params(InArgs, Relations, OutArgs, ArgTemplate),
    Call = case CallTemplate of
               {Fn, ArgTemplate} ->
                   ?expr('$$Fn'('$$OutParams'),
                         [replace_term('$$Fn', Fn),
                          splice_trees('$$OutParams', OutParams)]);
               {Mod, Fn, ArgTemplate} ->
                   ?expr('$$Mod':'$$Fn'('$$OutParams'),
                         [replace_term('$$Mod', Mod),
                          replace_term('$$Fn', Fn),
                          splice_trees('$$OutParams', OutParams)])
           end,
    UsesErrorF = lists:member('$errorf', UsedOutNames),
    Body = if Op == verify, not UsesErrorF ->
                   [Actual,EPath|_] = InPatterns,
                   ?expr(try '$$Call', ok
                         catch _:Reason ->
                                 mk_type_error(Reason,'Actual','Path')
                         end,
                         [replace_tree('$$Call', Call),
                          replace_tree('Actual', Actual),
                          replace_tree('Path', EPath)]);
              true ->
                   Call
           end,
    {InPatterns, Body}.

last_tuple_element(Tuple) ->
    element(tuple_size(Tuple), Tuple).

%% InArgs = [{Name, SyntaxTree}] % eg: [{'$1',?expr(ToPackForEncode)}, ...]
%%     Name = atom()
%% InOutArgRelations = [{OutParamName, [InArg1, InArg2, ...]}]
%%     Example if InOutArgRelations (for verify translators):
%%          [{'$1',['$1']},
%%           {'$errorf', ['$1','$2']},
%%           {'$user_data', ['$3']}]
%% Outs = [{Name, SyntaxTree}] % eg: [{'$1',?expr(ToPackForEncode)}, ...]
%%     Name = atom()
%% ArgsTemplate = [term()]     % eg: ['$1', '$2']
%%          ff                 % or  ['$user_data', ['$1', '$2', a, 4711]]
%% -> {InParams      :: [syntax_tree()],
%%     OutArgs       :: [syntax_tree()],
%%     UsedInArgs    :: [atom()],
%%     UsedOutParams :: [atom()]}
process_tr_params(InArgs, InOutArgRelations, Outs, ArgsTemplate) ->
    {OutArgs, UsedOutNames} =
        lists:mapfoldl(
          fun(ArgTempl, Used) ->
                  {Out, MoreUsed} = abstractify_tr_param(ArgTempl, Outs),
                  {Out, lists:usort(MoreUsed ++ Used)}
          end,
          [],
          ArgsTemplate),
    UnusedOutNames = [N || {N,_} <- Outs] -- UsedOutNames,
    UsedInArgs = compute_used_in_args(UsedOutNames, UnusedOutNames,
                                      InOutArgRelations),
    InParams = underscore_unused_params(InArgs, UsedInArgs),
    {InParams, OutArgs, UsedInArgs, UsedOutNames}.

abstractify_tr_param([H|T], Outs) ->
    {AbsH, UsedH} = abstractify_tr_param(H, Outs),
    {AbsT, UsedT} = abstractify_tr_param(T, Outs),
    {erl_syntax:cons(AbsH, AbsT), lists:usort(UsedH ++ UsedT)};
abstractify_tr_param([], _Outs) ->
    {erl_syntax:nil(), []};
abstractify_tr_param(Tuple, Outs) when is_tuple(Tuple) ->
    {AElems, AUsed} = lists:unzip([abstractify_tr_param(Elem, Outs)
                                   || Elem <- tuple_to_list(Tuple)]),
    {erl_syntax:tuple(AElems), lists:usort(lists:append(AUsed))};
abstractify_tr_param(I, _Outs) when is_integer(I) ->
    {erl_syntax:integer(I), []};
abstractify_tr_param(F, _Outs) when is_float(F) ->
    {erl_syntax:float(F), []};
abstractify_tr_param(A, Outs) when is_atom(A) ->
    case lists:keyfind(A, 1, Outs) of
        {A,Abstr} -> {Abstr, [A]};
        false     -> {erl_syntax:atom(A), []}
    end;
abstractify_tr_param(B, _Outs) when is_binary(B) ->
    {erl_syntax:abstract(B), []};
abstractify_tr_param(B, _Outs) when is_bitstring(B) ->
    %% Current version of erl_syntax (Erlang-18.3) can't do bitstrings,
    %% but erl_parse can. Maybe future erl_syntax versions will...
    try erl_syntax:abstract(B) of
        STree -> {STree, []}
    catch error:{badarg,_} ->
            erl_parse:abstract(B)
    end;
abstractify_tr_param(X, Outs) ->
    abstractify_tr_param_check_for_map(X, Outs).

-ifdef(NO_HAVE_MAPS).
abstractify_tr_param_check_for_map(X, _Outs) ->
    error({translator,cant_make_abstraxt_code_for,X}).
-else.
abstractify_tr_param_check_for_map(M, Outs) when is_map(M) ->
    {MItems, MUsed} =
        lists:unzip([begin
                         {AK,UK} = abstractify_tr_param(K, Outs),
                         {AV,UV} = abstractify_tr_param(V, Outs),
                         {erl_syntax:map_field_assoc(AK, AV), UK ++ UV}
                     end
                     || {K,V} <- maps:to_list(M)]),
    {erl_syntax:map_expr(MItems), lists:usort(lists:append(MUsed))};
abstractify_tr_param_check_for_map(X, _Outs) ->
    error({translator,cant_make_abstraxt_code_for,X}).
-endif. % NO_HAVE_MAPS.

mk_pass_straight_through_rel(Names) ->
    [{Name,[Name]} || Name <- Names].

compute_used_in_args(Used, Unused, InOutArgRelations) ->
    lists:usort(
      lists:append(
        [lists:append(
           [case lists:keyfind(U, 1, InOutArgRelations) of
                {U, Ins}         -> Ins;
                {U, Ins, _Elses} -> Ins;
                false            -> []
            end
            || U <- Used]),
         lists:append(
           [case lists:keyfind(Uu, 1, InOutArgRelations) of
                {Uu, _Ins}        -> [];
                {Uu, _Ins, Elses} -> Elses;
                false             -> []
            end
            || Uu <- Unused])])).

underscore_unused_params(InArgs, UsedInArgs) ->
    [case lists:member(InName, UsedInArgs) of
         true  -> InExpr;
         false -> ?expr(_)
     end
     || {InName, InExpr} <- InArgs].

dollar_i(Name) ->
    list_to_atom(?ff("$~w", [Name])).

tr_in_args_by_op(Op) ->
    [{dollar_i(I), A} || {I,A} <- gpb_lib:index_seq(args_by_op2(Op))]
        ++ [{'$user_data', ?expr(TrUserData)}].

args_by_op2(encode)                   -> [?expr(X)];
args_by_op2(decode)                   -> [?expr(X)];
args_by_op2(decode_init_default)      -> [?expr(InitialValue)];
args_by_op2(decode_repeated_add_elem) -> [?expr(Elem), ?expr(L)];
args_by_op2(decode_repeated_finalize) -> [?expr(L)];
args_by_op2(merge)                    -> [?expr(X1), ?expr(X2)];
args_by_op2(verify)                   -> [?expr(V), ?expr(Path)].

format_aux_transl_helpers() ->
    [gpb_lib:nowarn_unused_function(id,2),
     inline_attr(id,2),
     "id(X, _TrUserData) -> X.\n",
     "\n",
     gpb_lib:nowarn_unused_function(v_ok,3),
     inline_attr(v_ok,3),
     "v_ok(_Value, _Path, _TrUserData) -> ok.\n",
     "\n",
     gpb_lib:nowarn_unused_function(m_overwrite,3),
     inline_attr(m_overwrite,3),
     "m_overwrite(_Prev, New, _TrUserData) -> New.\n",
     "\n",
     gpb_lib:nowarn_unused_function(cons,3),
     inline_attr(cons,3),
     "cons(Elem, Acc, _TrUserData) -> [Elem | Acc].\n",
     "\n",
     gpb_lib:nowarn_unused_function('lists_reverse',2),
     inline_attr('lists_reverse',2),
     "'lists_reverse'(L, _TrUserData) -> lists:reverse(L)."
     "\n",
     gpb_lib:nowarn_unused_function('erlang_++',3),
     inline_attr('erlang_++',3),
     "'erlang_++'(A, B, _TrUserData) -> A ++ B."
     "\n"].

format_default_translators(AnRes, Opts) ->
    [format_default_map_translators(AnRes, Opts),
     format_default_msg_translators(AnRes, Opts)].

format_default_map_translators(#anres{map_types=MapTypes,
                                      map_value_types=MVT}=AnRes, Opts) ->
    HaveMaps = sets:size(MapTypes) > 0,
    {HaveMapSubmsgs, HaveMapNonSubmsgs} = MVT,
    [%% Auxiliary helpers in case of fields of type map<_,_>
     [case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) of
          '2tuples' ->
              [inline_attr(mt_maptuple_to_pseudomsg_r,2),
               gpb_codegen:format_fn(
                 mt_maptuple_to_pseudomsg_r,
                 fun({K,V},RName) -> {RName,K,V} end),
               "\n",
               inline_attr(mt_empty_map_r,0),
               gpb_codegen:format_fn(
                 mt_empty_map_r,
                 fun() -> [] end),
               [[inline_attr(mt_add_item_r,2),
                 gpb_codegen:format_fn(
                   mt_add_item_r,
                   fun({_RName,K,V}, Acc) -> [{K,V} | Acc] end),
                 "\n"]
                || HaveMapNonSubmsgs],
               [[inline_attr(mt_add_item_r_verify_value,2),
                 gpb_codegen:format_fn(
                   mt_add_item_r_verify_value,
                   fun({_,_,undefined}, _) -> error({gpb_error, missing_value});
                      ({_RName,K,V}, Acc) -> [{K,V} | Acc]
                   end),
                 "\n"]
                || HaveMapSubmsgs],
               inline_attr(mt_finalize_items_r,1),
               gpb_codegen:format_fn(
                 mt_finalize_items_r,
                 fun(Acc) ->
                         %% Reverse to store the items in the dict
                         %% in the same order they were decoded,
                         %% in case a key occurs more than once.
                         mt_finalize_items_r_aux(lists:reverse(Acc),
                                                 dict:new())
                 end),
               gpb_codegen:format_fn(
                 mt_finalize_items_r_aux,
                 fun([{K,V} | Tl], D) -> call_self(Tl, dict:store(K, V, D));
                    ([], D) -> dict:to_list(D)
                 end),
               "\n"];
          maps ->
              {M,K,V} = {?expr(M), ?expr(K), ?expr(V)},
              [inline_attr(mt_maptuple_to_pseudomsg_m,1),
               gpb_codegen:format_fn(
                 mt_maptuple_to_pseudomsg_m,
                 fun({K,V}) -> '#{key => K, value => V}' end,
                 [replace_tree('#{key => K, value => V}',
                               gpb_lib:map_create([{key,K}, {value,V}],
                                                  Opts))]),
               "\n",
               inline_attr(mt_map_to_list_m,1),
               gpb_codegen:format_fn(
                 mt_map_to_list_m,
                 fun(M) -> maps:to_list(M) end),
               "\n",
               inline_attr(mt_empty_map_m,0),
               gpb_codegen:format_fn(
                 mt_empty_map_m,
                 fun() -> '#{}' end,
                 [replace_tree('#{}', gpb_lib:map_create([], []))]),
               "\n",
               [[inline_attr(mt_add_item_m,2),
                 case gpb_lib:target_has_variable_key_map_update(Opts) of
                     true ->
                         gpb_codegen:format_fn(
                           mt_add_item_m,
                           fun('#{key := K,value := V}', M) -> 'M#{K => V}' end,
                           [replace_tree(
                              '#{key := K,value := V}',
                              gpb_lib:map_match([{key,K}, {value,V}], Opts)),
                            replace_tree(
                              'M#{K => V}',
                              gpb_lib:map_set(M, [{K,V}], []))]);
                     false ->
                         gpb_codegen:format_fn(
                           mt_add_item_m,
                           fun('#{key := K,value := V}', M) ->
                                   maps:put('K', 'V', 'M')
                           end,
                           [replace_tree(
                              '#{key := K,value := V}',
                              gpb_lib:map_match([{key,K}, {value,V}], Opts)),
                            replace_tree('K', K),
                            replace_tree('V', V),
                            replace_tree('M', M)])
                 end]
                || HaveMapNonSubmsgs],
               [[inline_attr(mt_add_item_m_verify_value,2),
                 case gpb_lib:target_has_variable_key_map_update(Opts) of
                     true ->
                         gpb_codegen:format_fn(
                           mt_add_item_m_verify_value,
                           fun('#{key := K,value := V}', M) ->
                                   if V =:= '$undef' ->
                                           error({gpb_error, missing_value});
                                      true ->
                                           'M#{K => V}'
                                   end
                           end,
                           [replace_tree(
                              '#{key := K,value := V}',
                              gpb_lib:map_match([{key,K}, {value,V}], Opts)),
                            replace_tree(
                              'M#{K => V}',
                              gpb_lib:map_set(M, [{K,V}], []))]);
                     false ->
                         gpb_codegen:format_fn(
                           mt_add_item_m_verify_value,
                           fun('#{key := K,value := V}', M) ->
                                   if V =:= '$undef' ->
                                           error({gpb_error, missing_value});
                                      true ->
                                           maps:put('K', 'V', 'M')
                                   end
                           end,
                           [replace_tree(
                              '#{key := K,value := V}',
                              gpb_lib:map_match([{key,K}, {value,V}], Opts)),
                            replace_tree('K', K),
                            replace_tree('V', V),
                            replace_tree('M', M)])
                 end]
                || HaveMapSubmsgs],
               "\n"]
      end,
      format_default_merge_translators(AnRes, Opts)]
     || HaveMaps].

format_default_merge_translators(#anres{map_types=MapTypes}, Opts) ->
    HaveMaps = sets:size(MapTypes) > 0,
    [%% Auxiliary helpers in case of fields of type map<_,_>
     case gpb_lib:get_2tuples_or_maps_for_maptype_fields_by_opts(Opts) of
         '2tuples' ->
             [inline_attr(mt_merge_maptuples_r,2),
              gpb_codegen:format_fn(
                mt_merge_maptuples_r,
                fun(L1, L2) ->
                        dict:to_list(dict:merge(fun(_Key, _V1, V2) -> V2 end,
                                                dict:from_list(L1),
                                                dict:from_list(L2)))
                end),
              "\n"];
         maps ->
             [inline_attr(mt_merge_maps_m,2),
              gpb_codegen:format_fn(
                mt_merge_maps_m,
                fun(M1, M2) -> maps:merge(M1,M2) end),
              "\n"]
     end
     || HaveMaps].

format_default_msg_translators(#anres{translations=Translations}, _Opts) ->
    Defaults = [{merge, default_merge_translator()},
                {verify, default_verify_translator()}],
    Needs = compute_needed_default_translations(Translations, Defaults),
    [[[inline_attr(msg_m_overwrite,2),
       gpb_codegen:format_fn(
         msg_m_overwrite,
         fun(Msg2,_) -> Msg2 end),
       "\n"] || sets:is_element(merge, Needs)],
     [[gpb_codegen:format_fn(
         msg_v_no_check,
         fun(_,_) -> ok end),
       "\n"] || sets:is_element(verify, Needs)]].

compute_needed_default_translations(Translations, Defaults) ->
    dict:fold(
      fun(_ElemPath, Ops, Acc) ->
              lists:foldl(
                fun({type_spec, _}, Acc2) ->
                        Acc2;
                   ({Op, Calls}, Acc2) ->
                        lists:foldl(
                          fun(Call, Acc3) ->
                                  case lists:member({Op, Call}, Defaults) of
                                      true  -> sets:add_element(Op, Acc3);
                                      false -> Acc3
                                  end
                          end,
                          Acc2,
                          Calls)
                end,
                Acc,
                Ops)
      end,
      sets:new(),
      Translations).

inline_attr(FnName,Arity) ->
    ?f("-compile({inline,~p/~w}).~n", [FnName,Arity]).

%% -- auxiliary helpers, also used from encoders/decoders/... ------------

mk_find_tr_fn(MsgName, #?gpb_field{name=FName}, AnRes) ->
    ElemPath = [MsgName,FName],
    fun(Op) -> find_translation(ElemPath, Op, AnRes) end;
mk_find_tr_fn(MsgName, #gpb_oneof{name=CFName}, AnRes) ->
    fun({update_elem_path,FName}) ->
            fun(Op) ->
                    ElemPath = [MsgName,CFName,FName],
                    find_translation(ElemPath, Op, AnRes)
            end
    end.

mk_find_tr_fn_elem(MsgName, Field, IsOneof, AnRes) ->
    ElemPath = mk_elempath_elem(MsgName, Field, IsOneof),
    fun(Op) -> find_translation(ElemPath, Op, AnRes) end.

mk_find_tr_fn_elem_or_default(MsgName, {Field, IsOneof}=_XField, AnRes) ->
    mk_find_tr_fn_elem_or_default(MsgName, Field, IsOneof, AnRes).

mk_find_tr_fn_elem_or_default(MsgName, Field, IsOneof, AnRes) ->
    ElemPath = mk_elempath_elem(MsgName, Field, IsOneof),
    fun(Op, Default) -> find_translation(ElemPath, Op, AnRes, Default) end.

mk_elempath_elem(MsgName, #?gpb_field{name=FName,occurrence=Occ}, false) ->
    case Occ of
        repeated -> [MsgName,FName,[]];
        _        -> [MsgName,FName]
    end;
mk_elempath_elem(MsgName, #?gpb_field{name=FName}, {true, CFName}) ->
    [MsgName,CFName,FName].

find_translation(ElemPath, Op, AnRes) ->
    find_translation(ElemPath, Op, AnRes, undefined).
find_translation(ElemPath, Op, AnRes, Default) ->
    case has_translation(ElemPath, Op, AnRes) of
        {true, Transl} ->
            Transl;
        false ->
            default_fn_by_op(Op, Default)
    end.

has_translation(ElemPath, Op, #anres{translations=Ts}) ->
    case dict:find(ElemPath, Ts) of
        {ok, OpTransls} ->
            case lists:keyfind(Op, 1, OpTransls) of
                {Op, _Calls} ->
                    {true, mk_tr_fn_name(ElemPath, Op)};
                false ->
                    false
            end;
        error ->
            false
    end.

has_type_spec_translation(ElemPath, #anres{translations=Ts}) ->
    case dict:find(ElemPath, Ts) of
        {ok, OpTransls} ->
            case lists:keyfind(type_spec, 1, OpTransls) of
                {type_spec, TypeSpec} when is_list(TypeSpec) ->
                    {true, TypeSpec};
                {type_spec, '$default'} ->
                    false;
                false ->
                    false
            end;
        error ->
            false
    end.

mk_tr_fn_name([MsgName], Op) ->
    list_to_atom(?ff("tr_~s_~s", [Op, MsgName]));
mk_tr_fn_name([MsgName,FieldName,[]], Op) ->
    list_to_atom(?ff("tr_~s_~s.~s[x]", [Op, MsgName,FieldName]));
mk_tr_fn_name([MsgName,FName,OneofName], Op) ->
    list_to_atom(?ff("tr_~s_~s.~s.~s", [Op, MsgName,FName,OneofName]));
mk_tr_fn_name([MsgName,FieldName], Op) ->
    list_to_atom(?ff("tr_~s_~s.~s", [Op, MsgName,FieldName])).

default_fn_by_op(decode_repeated_add_elem, undefined) ->
    cons;
default_fn_by_op(decode_repeated_finalize, undefined) ->
    lists_reverse;
default_fn_by_op(merge, undefined) ->
    m_overwrite;
default_fn_by_op(verify, undefined) ->
    v_ok;
default_fn_by_op(_, undefined) ->
    id;
default_fn_by_op(_, Fn) ->
    Fn.

out_to_in_by_op(merge) -> 2;
out_to_in_by_op(_) -> 1.

default_merge_translator() -> {msg_m_overwrite,['$2','$user_data']}.

default_verify_translator() -> {msg_v_no_check,['$1', '$user_data']}.

exists_tr_for_msg(MsgName, Op, #anres{translations=Translations}) ->
    dict:fold(fun(_Key, _OpCalls, true) ->
                      true;
                 ([Name,_Field|_], OpCalls, false) when Name == MsgName ->
                      lists:keymember(Op, 1, OpCalls);
                 (_Key, _OpCalls, false) ->
                      false
              end,
              false,
              Translations).

maybe_userdata_param(Field, Expr) ->
    case is_primitive_type(Field) of
        true -> [];
        false -> [Expr]
    end.

is_primitive_type(#?gpb_field{type={msg,_}}) -> false;
is_primitive_type(#?gpb_field{type={group,_}}) -> false;
is_primitive_type(#?gpb_field{type={map,_,_}}) -> false;
is_primitive_type(_) -> true.

