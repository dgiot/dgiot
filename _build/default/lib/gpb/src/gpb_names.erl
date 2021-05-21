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

%%% @doc Transformation of module names and names in parsed
%%% definitions, such as prefixing and lowercasing of message names etc.

-module(gpb_names).

-export([file_name_to_module_name/2]).
-export([rename_module/2]).
-export([rename_defs/2]).
-export([compute_renamings/2]).
-export([apply_renamings/2]).
-export([apply_enum_type_renaming/2]).
-export([apply_msg_type_renaming/2]).

-export([format_error/1]).

-export([is_renaming_opt/1]).
-export([is_not_renaming_opt/1]).

-export([original_pkg_name/2]).
-export([original_msg_name/2]).
-export([original_group_name/2]).
-export([original_enum_name/2]).

-export_type([renamings/0]).

-include("../include/gpb.hrl").

-type renamings() :: no_renamings |
                     [{item_type(), dictionary()}].
-type dictionary() :: term(). %% dict:dict().
-type item_type() :: pkgs | % not present if use_packages option is not set
                     msgs |
                     groups |
                     enums |
                     services |
                     rpcs |
                     msg_types |
                     enum_types |
                     inverse_pkgs |
                     inverse_msgs |
                     inverse_groups |
                     invert_enums.

-define(f(Fmt, Args), io_lib:format(Fmt, Args)).

%% @doc Given a file name of a proto file, turn it into a module name,
%% possibly with name transformations, such as prefix or suffix
%% according to options.
-spec file_name_to_module_name(string(), gpb_compile:opts()) -> atom().
file_name_to_module_name(ProtoFileName, Opts) ->
    Ext = filename:extension(ProtoFileName),
    BaseNameNoExt = filename:basename(ProtoFileName, Ext),
    rename_module(BaseNameNoExt, Opts).

%% @doc Given a module name, rename it according to opts, for example
%% by prefixing it.
-spec rename_module(atom() | string(), gpb_compile:opts()) -> atom().
rename_module(Mod, Opts) when is_atom(Mod) ->
    rename_module(atom_to_list(Mod), Opts);
rename_module(Mod, Opts) when is_list(Mod) ->
    list_to_atom(possibly_suffix_mod(
                   possibly_prefix_mod(
                     mod_name_from_opts_or_else_filename(Mod, Opts),
                     Opts),
                   Opts)).

mod_name_from_opts_or_else_filename(FileBaseName, Opts) ->
    proplists:get_value(module_name, Opts, FileBaseName).

possibly_prefix_mod(BaseNameNoExt, Opts) ->
    case proplists:get_value(module_name_prefix, Opts) of
        undefined ->
            BaseNameNoExt;
        Prefix ->
            lists:concat([Prefix, BaseNameNoExt])
    end.

possibly_suffix_mod(BaseNameNoExt, Opts) ->
    case proplists:get_value(module_name_suffix, Opts) of
        undefined ->
            BaseNameNoExt;
        Suffix ->
            lists:concat([BaseNameNoExt, Suffix])
    end.

%% @doc Rename definitions according to options, for example
%% lowercasing message names.
%%
%% This effectively invokes {@link apply_renamings/2} with the
%% result from {@link compute_renamings/2}.
-spec rename_defs(gpb_defs:defs(), gpb_compile:opts()) ->
                         {ok, gpb_defs:defs()} |
                         {error, Reason::term()}.
rename_defs(Defs, Opts) ->
    case compute_renamings(Defs, Opts) of
        {ok, Renamings} ->
            {ok, apply_renamings(Defs, Renamings)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Compute any renamings to be applied.
-spec compute_renamings(gpb_defs:defs(), gpb_compile:opts()) ->
                               {ok, renamings()} |
                               {error, Reason::term()}.
compute_renamings(Defs, Opts) ->
    Opts1 = convert_legacy_opts(Opts),
    case mk_rename_operations(Opts1) of
        [] ->
            {ok, no_renamings};
        RenameOpFs ->
            case mk_renamings(RenameOpFs, Defs, Opts1) of
                {ok, Renamings} ->
                    {ok, Renamings};
                {error, Reason} ->
                    {error, {rename_defs, Reason}}
            end
    end.

%% @doc Apply any renamings.
-spec apply_renamings(Defs, renamings()) -> Defs when
      Defs :: gpb_defs:defs().
apply_renamings(Defs, no_renamings) ->
    Defs;
apply_renamings(Defs, Renamings) ->
    RF = mk_renamer(Renamings),
    do_rename(RF, Defs).

%% @doc Apply enum type renamings
apply_enum_type_renaming(EnumName, no_renamings) ->
    EnumName;
apply_enum_type_renaming(EnumName, Renamings) ->
    do_rename_type(EnumName, enum_types, Renamings).

%% @doc Apply msg type renamings
apply_msg_type_renaming(MsgName, no_renamings) ->
    MsgName;
apply_msg_type_renaming(MsgName, Renamings) ->
    do_rename_type(MsgName, msg_types, Renamings).

%% @doc Check whether an option is a renaming option or not.
is_renaming_opt(Opt) ->
    case Opt of
        {rename, _} -> true;
        _ -> is_legacy_renaming_opt(Opt)
    end.

%% @doc The opposite of {@link is_renaming_opt/1}
is_not_renaming_opt(Opt) ->
    not is_renaming_opt(Opt).

%% @hidden
original_pkg_name(PkgName, no_renamings) -> PkgName;
original_pkg_name(PkgName, Renamings) ->
    InversePkgRenamings =
        proplists:get_value(inverse_pkgs, Renamings, dict:new()),
    dict_fetch_or_default(PkgName, InversePkgRenamings, '').

%% @hidden
original_msg_name(MsgName, no_renamings) -> MsgName;
original_msg_name(MsgName, Renamings) ->
    dict_fetch(MsgName, proplists:get_value(inverse_msgs, Renamings)).

%% @hidden
original_group_name(GName, no_renamings) -> GName;
original_group_name(GName, Renamings) ->
    dict_fetch(GName, proplists:get_value(inverse_groups, Renamings)).

%% @hidden
original_enum_name(EnumName, no_renamings) -> EnumName;
original_enum_name(EnumName, Renamings) ->
    dict_fetch(EnumName, proplists:get_value(inverse_enums, Renamings)).

format_error({error, {rename_defs, Reason}}) -> fmt_err(Reason);
format_error({rename_defs, Reason}) -> fmt_err(Reason);
format_error(Reason) -> fmt_err(Reason).

%% Note: do NOT include trailing newline (\n or ~n)
fmt_err({duplicates, Dups}) ->
    ["Renaming to same name:\n",
     gpb_lib:nl_join(
       lists:append(
         [[case K of
               {Service,Rpc} -> ?f("  ~s/~s -> ~s", [Service, Rpc, V]);
               K             -> ?f("  ~s -> ~s", [K, V])
           end
           || K <- Ks]
          || {Ks, V} <- Dups]))];
fmt_err(X) ->
    ?f("Unexpected error ~p", [X]).

%% -- Converting legacy opts ------------------

convert_legacy_opts([Opt | Opts]) ->
    case Opt of
        {msg_name_prefix, Prefix} ->
            l_msg_prefix_opts(Prefix) ++ convert_legacy_opts(Opts);
        {msg_name_suffix, Suffix} ->
            l_msg_suffix_opts(Suffix) ++ convert_legacy_opts(Opts);
        msg_name_to_snake_case ->
            l_msg_snake_case_opts() ++ convert_legacy_opts(Opts);
        {msg_name_to_snake_case=OptKey, Bool} ->
            if Bool -> l_msg_snake_case_opts() ++ convert_legacy_opts(Opts);
               true -> convert_legacy_opts(drop_opt(OptKey, Opts))
            end;
        msg_name_to_lower ->
            l_msg_lowercase_opts() ++ convert_legacy_opts(Opts);
        {msg_name_to_lower=OptKey, Bool} ->
            if Bool -> l_msg_lowercase_opts() ++ convert_legacy_opts(Opts);
               true -> convert_legacy_opts(drop_opt(OptKey, Opts))
            end;
        _ ->
            [Opt | convert_legacy_opts(Opts)]
    end;
convert_legacy_opts([]) ->
    [].

is_legacy_renaming_opt(Opt) ->
    convert_legacy_opts([Opt]) /= [Opt].

drop_opt(Opt, [Opt | Rest])      -> drop_opt(Opt, Rest);
drop_opt(Opt, [{Opt, _} | Rest]) -> drop_opt(Opt, Rest);
drop_opt(Opt, [Other | Rest])    -> [Other | drop_opt(Opt, Rest)];
drop_opt(_Opt, [])               -> [].

l_msg_prefix_opts({by_proto,_PrefixList}=ByProto) ->
    [{rename, {msg_fqname, {prefix, ByProto}}}];
l_msg_prefix_opts(Prefix) ->
    l_msg_only_opts({prefix, Prefix}).

l_msg_suffix_opts(Suffix) ->
    l_msg_only_opts({suffix, Suffix}).

l_msg_snake_case_opts() ->
    l_msg_and_service_and_rpc_opts(snake_case).

l_msg_lowercase_opts() ->
    l_msg_and_service_and_rpc_opts(lowercase).

l_msg_only_opts(Value) ->
    [{rename, {pkg_name, Value}},
     {rename, {msg_fqname, Value}},
     {rename, {group_fqname, Value}}].

l_msg_and_service_and_rpc_opts(Value) ->
    [{rename, {pkg_name, Value}},
     {rename, {service_fqname, Value}},
     {rename, {rpc_name, Value}},
     {rename, {msg_fqname, Value}},
     {rename, {group_fqname, Value}}].

%% -- Renaming opts -> renaming functions ------------------

mk_rename_operations(Opts) ->
    [{What, mk_rename_op(What, How)} || {rename, {What, How}} <- Opts].

mk_rename_op(pkg_name, How) -> mk_pkg_rename_op(How);
mk_rename_op(msg_fqname, How) -> mk_msg_rename_op(How);
mk_rename_op(msg_name, How) -> mk_msg_rename_op(How);
mk_rename_op(group_fqname, How) -> mk_msg_rename_op(How);
mk_rename_op(group_name, How) -> mk_group_rename_op(How);
mk_rename_op(service_fqname, How) -> mk_service_rename_op(How);
mk_rename_op(service_name, How) -> mk_service_rename_op(How);
mk_rename_op(rpc_name, How) -> mk_rpc_rename_op(How);
mk_rename_op(msg_typename, How) -> mk_msgtype_rename_op(How);
mk_rename_op(enum_typename, How) -> mk_enumtype_rename_op(How).

mk_pkg_rename_op(PrimOp) ->
    fun(Name, _Proto) -> do_prim_op(PrimOp, Name) end.

mk_msg_rename_op({prefix, {by_proto, PrefixList}}) ->
    fun(Name, Proto) ->
            ProtoName = list_to_atom(Proto),
            Prefix = proplists:get_value(ProtoName, PrefixList, ""),
            list_to_atom(lists:concat([Prefix, Name]))
    end;
mk_msg_rename_op(PrimOp) ->
    fun(Name, _Proto) -> do_prim_op(PrimOp, Name) end.

mk_group_rename_op(PrimOp) ->
    fun(Name, _Proto) -> do_prim_op(PrimOp, Name) end.

mk_service_rename_op(PrimOp) ->
    fun(Name, _Proto) -> do_prim_op(PrimOp, Name) end.

mk_rpc_rename_op(PrimOp) ->
    fun(Name, _Proto) -> do_prim_op(PrimOp, Name) end.

mk_msgtype_rename_op(PrimOp) ->
    fun(Name, _Proto) -> do_prim_op(PrimOp, Name) end.

mk_enumtype_rename_op(PrimOp) ->
    fun(Name, _Proto) -> do_prim_op(PrimOp, Name) end.

do_prim_op({prefix, Prefix}, Name) ->
    list_to_atom(lists:concat([Prefix, Name]));
do_prim_op({suffix, Suffix}, Name) ->
    list_to_atom(lists:concat([Name, Suffix]));
do_prim_op(lowercase, Name) ->
    list_to_atom(gpb_lib:lowercase(atom_to_list(Name)));
do_prim_op(snake_case, Name) ->
    list_to_atom(gpb_lib:snake_case(atom_to_list(Name)));
do_prim_op(dots_to_underscores, Name) ->
    list_to_atom(do_dot_uscore(atom_to_list(Name)));
do_prim_op(base_name, Name) ->
    list_to_atom(lists:last(gpb_lib:string_lexemes(atom_to_list(Name), "."))).


do_dot_uscore("."++Rest)  -> "_" ++ do_dot_uscore(Rest);
do_dot_uscore([C | Rest]) -> [C | do_dot_uscore(Rest)];
do_dot_uscore("")         -> "".

%% -- Compute old-name -> new name mappings -----------
%%
%% This stage is chiefly to call the RenameOp function---which could
%% possibly be a user-supplied function---only once or twice for every
%% msg, service or rpc name, but still be able to map all occurrences
%% of such names, which may be many times more (eg for messages: once
%% for the message name, again for each field of that type.)
%%

mk_renamings(RenameOps, Defs, Opts) ->
    PkgByProto = calc_package_by_proto(Defs),
    PkgRenamings = pkg_renamings(PkgByProto, RenameOps),
    MsgRenamings = msg_renamings(PkgByProto, PkgRenamings, Defs, RenameOps),
    GroupRenamings = group_renamings(PkgByProto, PkgRenamings, Defs,
                                     RenameOps),
    EnumRenamings = enum_renamings(PkgByProto, PkgRenamings, Defs, RenameOps),
    ServiceRenamings = service_renamings(PkgByProto, PkgRenamings, Defs,
                                         RenameOps),
    RpcRenamings = rpc_renamings(Defs, RenameOps),
    MostRenamings = [PkgRenamings, MsgRenamings, GroupRenamings,
                    ServiceRenamings],
    UsePackages = proplists:get_bool(use_packages, Opts),
    MsgTypeRenamings = msg_type_renamings(MsgRenamings, GroupRenamings,
                                          RenameOps),
    EnumTypeRenamings = enum_type_renamings(EnumRenamings, RenameOps),
    case check_no_dups(MostRenamings, RpcRenamings) of
        ok ->
            InversePkgRenamings = invert_renaming(PkgRenamings),
            InverseMsgRenamings = invert_renaming(MsgRenamings),
            InverseGroupRenamings = invert_renaming(GroupRenamings),
            InverseEnumRenamings = invert_renaming(EnumRenamings),
            Renamings = lists:append(
                          [[%% No pkg_containment items present in Defs
                            %% when the use_packages option is not set.
                            {pkgs, PkgRenamings} || UsePackages],
                           [{msgs, MsgRenamings},
                            {groups, GroupRenamings},
                            {enums, EnumRenamings},
                            {services, ServiceRenamings},
                            {rpcs, RpcRenamings},
                            {msg_types, MsgTypeRenamings},
                            {enum_types, EnumTypeRenamings}],
                            %% Reverse renamings:
                           [{inverse_pkgs, InversePkgRenamings}
                            || UsePackages],
                           [{inverse_msgs, InverseMsgRenamings},
                            {inverse_groups, InverseGroupRenamings},
                            {inverse_enums, InverseEnumRenamings}]]),
            {ok, Renamings};
        {error, Reason}  ->
            {error, Reason}
    end.

mk_renamer(Renamings) ->
    PkgRenamings = proplists:get_value(pkgs, Renamings),
    MsgRenamings = key1fetch(msgs, Renamings),
    GroupRenamings = key1fetch(groups, Renamings),
    EnumRenamings = key1fetch(enums, Renamings),
    ServiceRenamings = key1fetch(services, Renamings),
    RpcRenamings = key1fetch(rpcs, Renamings),
    fun(package, Name) ->
            if PkgRenamings /= undefined ->
                    dict_fetch(Name, PkgRenamings);
               PkgRenamings == undefined ->
                    %% No pkg_containment items present in Defs
                    %% when the use_packages option is not set.
                    Name
            end;
       (msg, Name) ->
            dict_fetch(Name, MsgRenamings);
       (group, Name) ->
            dict_fetch(Name, GroupRenamings);
       (enum, Name) ->
            dict_fetch(Name, EnumRenamings);
       (service, Name) ->
            dict_fetch(Name, ServiceRenamings);
       ({rpc, ServiceName}, RpcName) ->
            dict_fetch({ServiceName, RpcName}, RpcRenamings)
    end.



calc_package_by_proto(Defs) ->
    dict:from_list(
      [{Proto, PkgName}
       || {{pkg_containment, Proto}, PkgName} <- Defs]).

pkg_renamings(PkgByProto, RenameOps) ->
    dict:from_list(
      lists:map(
        fun({Proto, Pkg}) ->
                Pkg1 = run_ops(pkg_name, Pkg, Proto, RenameOps),
                {Pkg, Pkg1}
        end,
        dict:to_list(PkgByProto))).

msg_renamings(PkgByProto, PkgRenamings, Defs, RenameOps) ->
    dict:from_list(
      lists:append(
        [begin
             Pkg = dict_fetch_or_default(Proto, PkgByProto, ''),
             [begin
                  Name = drop_prefix(Pkg, FqName),
                  Name1 = run_ops(msg_name, Name, Proto, RenameOps),
                  Pkg1 = dict_fetch_or_default(Pkg, PkgRenamings, ''),
                  FqName1 = prefix(Pkg1, Name1),
                  FqName2 = run_ops(msg_fqname, FqName1, Proto, RenameOps),
                  {FqName, FqName2}
              end
              || FqName <- MsgNames]
         end
         || {{msg_containment, Proto}, MsgNames} <- Defs])).

group_renamings(PkgByProto, PkgRenamings, Defs, RenameOps) ->
    ProtoByMsg = dict:from_list(
                   lists:append(
                     [[{MsgName, Proto} || MsgName <- MsgNames]
                      || {{msg_containment, Proto}, MsgNames} <- Defs])),
    dict:from_list(
      [begin
           MsgName = group_name_to_msg_name(GroupFqName),
           Proto = dict:fetch(MsgName, ProtoByMsg),
           Pkg = dict_fetch_or_default(Proto, PkgByProto, ''),
           Name = drop_prefix(Pkg, GroupFqName),
           Name1 = run_ops(group_name, Name, Proto, RenameOps),
           Pkg1 = dict_fetch_or_default(Pkg, PkgRenamings, ''),
           FqName1 = prefix(Pkg1, Name1),
           FqName2 = run_ops(group_fqname, FqName1, Proto, RenameOps),
           {GroupFqName, FqName2}
       end
       || {{group,GroupFqName}, _Fields} <- Defs]).

group_name_to_msg_name(GName) ->
    Components = gpb_lib:string_lexemes(atom_to_list(GName), "."),
    [_G | ButLast] = lists:reverse(Components),
    list_to_atom(gpb_lib:dot_join(lists:reverse(ButLast))).

enum_renamings(PkgByProto, PkgRenamings, Defs, RenameOps) ->
    dict:from_list(
      lists:append(
        [begin
             Pkg = dict_fetch_or_default(Proto, PkgByProto, ''),
             [calc_enum_name_renaming(FqName, Pkg, PkgRenamings, Proto,
                                      RenameOps)
              || FqName <- EnumNames]
         end
         || {{enum_containment, Proto}, EnumNames} <- Defs])).

calc_enum_name_renaming(FqName, Pkg, PkgRenamings, Proto, RenameOps) ->
    Name = drop_prefix(Pkg, FqName),
    %% Enums can be nested inside messages, but not inside other
    %% enums, so anything we find between the last package and the
    %% last component is one or several message names that need to
    %% be renamed according to message renaming rules.
    case split_enum_name(Name) of
        {EnumName} ->
            Pkg1 = dict_fetch_or_default(Pkg, PkgRenamings, ''),
            {FqName, prefix(Pkg1, EnumName)};
        {MsgName, EnumBase} ->
            MsgName1 = run_ops(msg_name, MsgName, Proto, RenameOps),
            Pkg1 = dict_fetch_or_default(Pkg, PkgRenamings, ''),
            FqMsgName1 = prefix(Pkg1, MsgName1),
            FqMsgName2 = run_ops(msg_fqname, FqMsgName1, Proto, RenameOps),
            FqName2 = prefix(FqMsgName2, EnumBase),
            {FqName, FqName2}
    end.

split_enum_name(Name) ->
    case gpb_lib:string_lexemes(atom_to_list(Name), ".") of
        [_Component] ->
            {Name};
        Components ->
            [EnumName | MsgNamesCompsRev] = lists:reverse(Components),
            MsgNames = gpb_lib:dot_join(
                           lists:reverse(MsgNamesCompsRev)),
            {list_to_atom(MsgNames), list_to_atom(EnumName)}
    end.

service_renamings(PkgByProto, PkgRenamings, Defs, RenameOps) ->
    dict:from_list(
      lists:append(
        [begin
             Pkg = dict_fetch_or_default(Proto, PkgByProto, ''),
             [begin
                  Name = drop_prefix(Pkg, FqName),
                  Name1 = run_ops(service_name, Name, Proto, RenameOps),
                  Pkg1 = dict_fetch_or_default(Pkg, PkgRenamings, ''),
                  FqName1 = prefix(Pkg1, Name1),
                  FqName2 = run_ops(service_fqname, FqName1, Proto, RenameOps),
                  {FqName, FqName2}
              end
              || FqName <- ServiceNames]
         end
         || {{service_containment, Proto}, ServiceNames} <- Defs])).

rpc_renamings(Defs, RenameOps) ->
    dict:from_list(
      lists:append(
        [begin
             [begin
                  RpcName1 = run_ops(rpc_name, RpcName, Proto, RenameOps),
                  {{ServiceName, RpcName}, RpcName1}
              end
              || {ServiceName, RpcName} <- Rpcs]
         end
         || {{rpc_containment, Proto}, Rpcs} <- Defs])).

msg_type_renamings(MsgRenamings, GroupRenamings, RenameOps) ->
    Renamed = dict_values(MsgRenamings) ++ dict_values(GroupRenamings),
    dict:from_list(
      [begin
           Name2 = run_ops(msg_typename, Name, '', RenameOps),
           {Name, Name2}
       end
       || Name <- Renamed]).

enum_type_renamings(EnumRenamings, RenameOps) ->
    dict:from_list(
      [begin
           Name2 = run_ops(enum_typename, Name, '', RenameOps),
           {Name, Name2}
       end
       || Name <- dict_values(EnumRenamings)]).


dict_values(Dict) ->
    dict:fold(fun(_K, V, Acc) -> [V | Acc] end, [], Dict).

run_ops(What, Name0, Proto, RenameOps) ->
    lists:foldl(fun(F, Name) -> F(Name, Proto) end,
                Name0,
                [F || {W, F} <- RenameOps,
                      W =:= What]).

drop_prefix('', Value) when is_atom(Value) ->
    Value; % fast path (no package)
drop_prefix(Prefix, Value) when is_atom(Prefix), is_atom(Value) ->
    P = atom_to_list(Prefix),
    V = atom_to_list(Value),
    case lists:sublist(V, length(P) + 1, length(V) - length(P)) of
        "." ++ Rest -> list_to_atom(Rest);
        Rest        -> list_to_atom(Rest)
    end.

prefix('', V) ->
    V; % fast path (no package)
prefix(P, '') ->
    P; % fast path (no remainder)
prefix(P, V) ->
    list_to_atom(lists:concat([P, ".", V])).

dict_fetch_or_default(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.

dict_fetch(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Value} ->
            Value;
        error ->
            error({not_found_in_dict, Key, dict:to_list(Dict)})
    end.

key1fetch(Key, KVs) ->
    case lists:keyfind(Key, 1, KVs) of
        {Key, Value} ->
            Value;
        false ->
            error({not_found_among_kvs, Key, KVs})
    end.

check_no_dups(Renamings, RpcRenamings) ->
    Errs1 = lists:foldl(fun renaming_dups/2, [], Renamings),
    Errs2 = renaming_rpc_dups(RpcRenamings, Errs1),
    if Errs2 == [] ->
            ok;
       true ->
            {error, {duplicates, Errs2}}
    end.

renaming_dups(Dict, Errs) ->
    RDict = dict:fold(fun(K, V, RDict) -> dict:append(V, K, RDict) end,
                      dict:new(),
                      Dict),
    DupsDict = dict:filter(fun(_V, [_K1,_K2|_]) -> true; % >= 2 entries
                              (_V, [_]) -> false
                           end,
                           RDict),
    [{Keys, V} || {V, Keys} <- dict:to_list(DupsDict)] ++ Errs.

%% check for dups on a per service basis
renaming_rpc_dups(Dict, Errs) ->
    %% split into dict of dicts, one per service (service name is used as key)
    Ds = dict:fold(
           fun({Service,_Rpc}=Entry, NewName, D) ->
                   ED = case dict:find(Service, D) of
                            error    -> dict:store(Entry, NewName, dict:new());
                            {ok,ED0} -> dict:store(Entry, NewName, ED0)
                        end,
                   dict:store(Service, ED, D)
           end,
           dict:new(),
           Dict),
    lists:foldl(fun renaming_dups/2,
                Errs,
                [D || {_Service,D} <- dict:to_list(Ds)]).


invert_renaming(Renaming) ->
    dict:from_list(invert_pairs(dict:to_list(Renaming))).

invert_pairs(L) ->
    [{B, A} || {A, B} <- L].

%% -- Traversing defs, doing rename ----------

do_rename(RF, Defs) ->
    lists:map(
      fun({{msg,Name}, Fields}) ->
              {{msg, RF(msg, Name)}, rename_fields(RF, Fields, Defs)};
         ({{group,Name}, Fields}) ->
              {{group, RF(group, Name)}, rename_fields(RF, Fields, Defs)};
         ({{enum, Name}, Enumerators}) ->
              {{enum, RF(enum, Name)}, Enumerators};
         ({{extensions,Name}, Exts}) ->
              {{extensions, RF(msg, Name)}, Exts};
         ({{service,Name}, Rpcs}) ->
              {{service, RF(service, Name)}, rename_rpcs(RF, Name, Rpcs)};
         ({package,Name}) ->
              {package, RF(package, Name)};
         ({proto3_msgs,Names}) ->
              {proto3_msgs, [RF(msg, Name) || Name <- Names]};
         ({{msg_containment,Proto}, MsgNames}) ->
              {{msg_containment,Proto}, [RF(msg, Name) || Name <- MsgNames]};
         ({{enum_containment,Proto}, EnumNames}) ->
              {{enum_containment,Proto}, [RF(enum, Name) || Name <- EnumNames]};
         ({{pkg_containment,Proto}, PkgName}) ->
              {{pkg_containment,Proto}, RF(package, PkgName)};
         ({{service_containment,Proto}, ServiceNames}) ->
              {{service_containment,Proto},
               [RF(service, Name) || Name <- ServiceNames]};
         ({{rpc_containment,Proto}, Rpcs}) ->
              {{rpc_containment,Proto},
               [{RF(service, SvcName), RF({rpc, SvcName}, RpcName)}
                || {SvcName, RpcName} <- Rpcs]};
         (OtherElem) ->
              OtherElem
      end,
      Defs).

rename_fields(RF, Fields, Defs) ->
    lists:map(
      fun(#?gpb_field{type={msg,MsgName}}=F) ->
              F#?gpb_field{type={msg, RF(msg, MsgName)}};
         (#?gpb_field{type={map,KeyType,{msg,MsgName}}}=F) ->
              F#?gpb_field{type={map,KeyType,{msg, RF(msg, MsgName)}}};
         (#?gpb_field{type={group,MsgName}}=F) ->
              F#?gpb_field{type={group, RF(group, MsgName)}};
         (#?gpb_field{type={enum,EnumName}}=F) ->
              F#?gpb_field{type={enum, RF(enum, EnumName)}};
         (#?gpb_field{type={map,KeyType,{enum,EnumName}}}=F) ->
              F#?gpb_field{type={map,KeyType,{enum, RF(enum, EnumName)}}};
         (#gpb_oneof{fields=Fs}=F) ->
              F#gpb_oneof{fields=rename_fields(RF, Fs, Defs)};
         (#?gpb_field{}=F) ->
              F
      end,
      Fields).

rename_rpcs(RF, ServiceName, RPCs) ->
    lists:map(
      fun(#?gpb_rpc{name=RpcName, input=Arg, output=Return}=R) ->
              R#?gpb_rpc{name=RF({rpc, ServiceName}, RpcName),
                         input=RF(msg, Arg),
                         output=RF(msg, Return)}
      end,
      RPCs).

do_rename_type(Name, Key, Renamings) ->
    TypeRenamings = proplists:get_value(Key, Renamings),
    dict:fetch(Name, TypeRenamings).
