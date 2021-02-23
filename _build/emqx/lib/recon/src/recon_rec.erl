%%%-------------------------------------------------------------------
%%% @author bartlomiej.gorny@erlang-solutions.com
%%% @doc
%%% This module handles formatting records for known record types.
%%% Record definitions are imported from modules by user. Definitions are
%%% distinguished by record name and its arity, if you have multiple records
%%% of the same name and size, you have to choose one of them and some of your
%%% records may be wrongly labelled. You can manipulate your definition list by
%%% using import/1 and clear/1, and check which definitions are in use by executing
%%% list/0.
%%% @end
%%%-------------------------------------------------------------------
-module(recon_rec).
-author("bartlomiej.gorny@erlang-solutions.com").
%% API

-export([is_active/0]).
-export([import/1, clear/1, clear/0, list/0, get_list/0, limit/3]).
-export([format_tuple/1]).

-ifdef(TEST).
-export([lookup_record/2]).
-endif.

% basic types
-type field() :: atom().
-type record_name() :: atom().
% compound
-type limit() :: all | none | field() | [field()].
-type listentry() :: {module(), record_name(), [field()], limit()}.
-type import_result() :: {imported, module(), record_name(), arity()}
                       | {overwritten, module(), record_name(), arity()}
                       | {ignored, module(), record_name(), arity(), module()}.

%% @doc import record definitions from a module. If a record definition of the same name
%% and arity has already been imported from another module then the new
%% definition is ignored (returned info tells you from which module the existing definition was imported).
%% You have to choose one and possibly remove the old one using
%% clear/1. Supports importing multiple modules at once (by giving a list of atoms as
%% an argument).
%% @end
-spec import(module() | [module()]) -> import_result() | [import_result()].
import(Modules) when is_list(Modules) ->
    lists:foldl(fun import/2, [], Modules);
import(Module) ->
    import(Module, []).

%% @doc quickly check if we want to do any record formatting
-spec is_active() -> boolean().
is_active() ->
    case whereis(recon_ets) of
        undefined -> false;
        _ -> true
    end.

%% @doc remove definitions imported from a module.
clear(Module) ->
    lists:map(fun(R) -> rem_for_module(R, Module) end, ets:tab2list(records_table_name())).

%% @doc remove all imported definitions, destroy the table, clean up
clear() ->
    maybe_kill(recon_ets),
    ok.

%% @doc prints out all "known" (imported) record definitions and their limit settings.
%% Printout tells module a record originates from, its name and a list of field names,
%% plus the record's arity (may be handy if handling big records) and a list of field it
%% limits its output to, if set.
%% @end
list() ->
    F = fun({Module, Name, Fields, Limits}) ->
            Fnames = lists:map(fun atom_to_list/1, Fields),
            Flds = join(",", Fnames),
            io:format("~p: #~p(~p){~s} ~p~n",
                      [Module, Name, length(Fields), Flds, Limits])
        end,
    io:format("Module: #Name(Size){<Fields>} Limits~n==========~n", []),
    lists:foreach(F, get_list()).

%% @doc returns a list of active record definitions
-spec get_list() -> [listentry()].
get_list() ->
    ensure_table_exists(),
    Lst = lists:map(fun make_list_entry/1, ets:tab2list(records_table_name())),
    lists:sort(Lst).

%% @doc Limit output to selected fields of a record (can be 'none', 'all', a field or a list of fields).
%% Limit set to 'none' means there is no limit, and all fields are displayed; limit 'all' means that
%% all fields are squashed and only record name will be shown.
%% @end
-spec limit(record_name(), arity(), limit()) -> ok | {error, any()}.
limit(Name, Arity, Limit) when is_atom(Name), is_integer(Arity) ->
    case lookup_record(Name, Arity) of
        [] ->
            {error, record_unknown};
        [{Key, Fields, Mod, _}] ->
            ets:insert(records_table_name(), {Key, Fields, Mod, Limit}),
            ok
    end.

%% @private if a tuple is a known record, formats is as "#recname{field=value}", otherwise returns
%% just a printout of a tuple.
format_tuple(Tuple) ->
    ensure_table_exists(),
    First = element(1, Tuple),
    format_tuple(First, Tuple).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_list_entry({{Name, _}, Fields, Module, Limits}) ->
    FmtLimit = case Limits of
                   [] -> none;
                   Other -> Other
               end,
    {Module, Name, Fields, FmtLimit}.

import(Module, ResultList) ->
    ensure_table_exists(),
    lists:foldl(fun(Rec, Res) -> store_record(Rec, Module, Res) end,
                ResultList,
                get_record_defs(Module)).

store_record(Rec, Module, ResultList) ->
    {Name, Fields} = Rec,
    Arity = length(Fields),
    Result = case lookup_record(Name, Arity) of
        [] ->
            ets:insert(records_table_name(), rec_info(Rec, Module)),
            {imported, Module, Name, Arity};
        [{_, _, Module, _}] ->
            ets:insert(records_table_name(), rec_info(Rec, Module)),
            {overwritten, Module, Name, Arity};
        [{_, _, Mod, _}] ->
            {ignored, Module, Name, Arity, Mod}
    end,
    [Result | ResultList].

get_record_defs(Module) ->
    Path = code:which(Module),
    {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Path, [abstract_code]),
    lists:foldl(fun get_record/2, [], AC).

get_record({attribute, _, record, Rec}, Acc) -> [Rec | Acc];
get_record(_, Acc) -> Acc.

%% @private
lookup_record(RecName, FieldCount) ->
    ensure_table_exists(),
    ets:lookup(records_table_name(), {RecName, FieldCount}).

%% @private
ensure_table_exists() ->
    case ets:info(records_table_name()) of
        undefined ->
            case whereis(recon_ets) of
                undefined ->
                    Parent = self(),
                    Ref = make_ref(),
                    %% attach to the currently running session
                    {Pid, MonRef} = spawn_monitor(fun() ->
                        register(recon_ets, self()),
                        ets:new(records_table_name(), [set, public, named_table]),
                        Parent ! Ref,
                        ets_keeper()
                    end),
                    receive
                        Ref ->
                            erlang:demonitor(MonRef, [flush]),
                            Pid;
                        {'DOWN', MonRef, _, _, Reason} ->
                            error(Reason)
                    end;
                Pid ->
                    Pid
            end;
        Pid ->
            Pid
    end.

records_table_name() -> recon_record_definitions.

rec_info({Name, Fields}, Module) ->
    {{Name, length(Fields)}, field_names(Fields), Module, none}.

rem_for_module({_, _, Module, _} = Rec, Module) ->
    ets:delete_object(records_table_name(), Rec);
rem_for_module(_, _) ->
    ok.

ets_keeper() ->
    receive
        stop -> ok;
        _ -> ets_keeper()
    end.

field_names(Fields) ->
    lists:map(fun field_name/1, Fields).

field_name({record_field, _, {atom, _, Name}}) -> Name;
field_name({record_field, _, {atom, _, Name}, _Default}) -> Name;
field_name({typed_record_field, Field, _Type}) -> field_name(Field).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FORMATTER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_tuple(Name, Rec) when is_atom(Name) ->
    case lookup_record(Name, size(Rec) - 1) of
        [RecDef] -> format_record(Rec, RecDef);
        _ ->
            List = tuple_to_list(Rec),
            ["{", join(", ", [recon_trace:format_trace_output(true, El) || El <- List]), "}"]
    end;
format_tuple(_, Tuple) ->
    format_default(Tuple).

format_default(Val) ->
    io_lib:format("~p", [Val]).

format_record(Rec, {{Name, Arity}, Fields, _, Limits}) ->
    ExpectedLength = Arity + 1,
    case tuple_size(Rec) of
        ExpectedLength ->
            [_ | Values] = tuple_to_list(Rec),
            List = lists:zip(Fields, Values),
            LimitedList = apply_limits(List, Limits),
            ["#", atom_to_list(Name), "{",
             join(", ", [format_kv(Key, Val) || {Key, Val} <- LimitedList]),
             "}"];
        _ ->
            format_default(Rec)
    end.

format_kv(Key, Val) ->
    %% Some messy mutually recursive calls we can't avoid
    [recon_trace:format_trace_output(true, Key), "=", recon_trace:format_trace_output(true, Val)].

apply_limits(List, none) -> List;
apply_limits(_List, all) -> [];
apply_limits(List, Field) when is_atom(Field) ->
    [{Field, proplists:get_value(Field, List)}, {more, '...'}];
apply_limits(List, Limits) ->
    lists:filter(fun({K, _}) -> lists:member(K, Limits) end, List) ++ [{more, '...'}].

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

maybe_kill(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            unlink(Pid),
            exit(Pid, kill),
            wait_for_death(Pid, Name)
    end.

wait_for_death(Pid, Name) ->
    case is_process_alive(Pid) orelse whereis(Name) =:= Pid of
        true ->
            timer:sleep(10),
            wait_for_death(Pid, Name);
        false ->
            ok
    end.

-ifdef(OTP_RELEASE).
-spec join(term(), [term()]) -> [term()].
join(Sep, List) ->
    lists:join(Sep, List).
-else.
-spec join(string(), [string()]) -> string().
join(Sep, List) ->
    string:join(List, Sep).
-endif.
