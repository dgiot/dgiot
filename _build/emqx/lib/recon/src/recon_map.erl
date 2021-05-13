%%%-------------------------------------------------------------------
%%% @author bartlomiej.gorny@erlang-solutions.com
%%% @doc
%%% This module handles formatting maps.
%% It allows for trimming output to selected fields, or to nothing at all. It also adds a label
%% to a printout.
%% To set up a limit for a map, you need to give recon a way to tell the map you want to
%% trim from all the other maps, so you have to provide something like a 'type definition'.
%% It can be either another map which is compared to the arg, or a fun.
%%% @end
%%%-------------------------------------------------------------------
-module(recon_map).
-author("bartlomiej.gorny@erlang-solutions.com").
%% API

-export([limit/3, list/0, is_active/0, clear/0, remove/1, rename/2]).
-export([process_map/1]).

-type map_label() :: atom().
-type pattern() :: map() | function().
-type limit() :: all | none | atom() | binary() | [any()].

%% @doc quickly check if we want to do any record formatting
-spec is_active() -> boolean().
is_active() ->
    case whereis(recon_ets_maps) of
        undefined -> false;
        _ -> true
    end.

%% @doc remove all imported definitions, destroy the table, clean up
clear() ->
    maybe_kill(recon_ets_maps),
    ok.

%% @doc Limit output to selected keys of a map (can be 'none', 'all', a key or a list of keys).
%% Pattern selects maps to process: a "pattern" is just a map, and if all key/value pairs of a pattern
%% are present in a map (in other words, the pattern is a subset), then we say the map matches
%% and we process it accordingly (apply the limit).
%%
%% Patterns are applied in alphabetical order, until a match is found.
%%
%% Instead of a pattern you can also provide a function which will take a map and return a boolean.
%% @end
-spec limit(map_label(), pattern(), limit()) -> ok | {error, any()}.
limit(Label, #{} = Pattern, Limit) when is_atom(Label) ->
    store_pattern(Label, Pattern, Limit);
limit(Label, Pattern, Limit) when is_atom(Label), is_function(Pattern) ->
    store_pattern(Label, Pattern, Limit).

%% @doc prints out all "known" map definitions and their limit settings.
%% Printout tells a map's name, the matching fields required, and the limit options.
%% @end
list() ->
    ensure_table_exists(),
    io:format("~nmap definitions and limits:~n"),
    list(ets:tab2list(patterns_table_name())).

%% @doc remove a given map entry
-spec remove(map_label()) -> true.
remove(Label) ->
    ensure_table_exists(),
    ets:delete(patterns_table_name(), Label).

%% @doc rename a given map entry, which allows to to change priorities for
%% matching. The first argument is the current name, and the second
%% argument is the new name.
-spec rename(map_label(), map_label()) -> renamed | missing.
rename(Name, NewName) ->
    ensure_table_exists(),
    case ets:lookup(patterns_table_name(), Name) of
        [{Name, Pattern, Limit}] ->
            ets:insert(patterns_table_name(), {NewName, Pattern, Limit}),
            ets:delete(patterns_table_name(), Name),
            renamed;
        [] ->
            missing
    end.

%% @doc prints out all "known" map filter definitions and their settings.
%% Printout tells the map's label, the matching patterns, and the limit options
%% @end
list([]) ->
    io:format("~n"),
    ok;
list([{Label, Pattern, Limit} | Rest]) ->
    io:format("~p: ~p -> ~p~n", [Label, Pattern, Limit]),
    list(Rest).

%% @private given a map, scans saved patterns for one that matches; if found, returns a label
%% and a map with limits applied; otherwise returns 'none' and original map.
%% Pattern can be:
%% <ul>
%% <li> a map - then each key in pattern is checked for equality with the map in question</li>
%% <li> a fun(map()) -> boolean()</li>
%% </ul>
-spec process_map(map()) -> map() | {atom(), map()}.
process_map(M) ->
    process_map(M, ets:tab2list(patterns_table_name())).

process_map(M, []) ->
    M;
process_map(M, [{Label, Pattern, Limit} | Rest]) ->
    case map_matches(M, Pattern) of
        true ->
            {Label, apply_map_limits(Limit, M)};
        false ->
            process_map(M, Rest)
    end.

map_matches(#{} = M, Pattern) when is_function(Pattern) ->
    Pattern(M);
map_matches(_, []) ->
    true;
map_matches(M, [{K, V} | Rest]) ->
    case maps:is_key(K, M) of
        true ->
            case maps:get(K, M) of
                V ->
                    map_matches(M, Rest);
                _ ->
                    false
            end;
        false ->
            false
    end.

apply_map_limits(none, M) ->
    M;
apply_map_limits(all, _) ->
    #{};
apply_map_limits(Fields, M) ->
    maps:with(Fields, M).

patterns_table_name() -> recon_map_patterns.

store_pattern(Label, Pattern, Limit) ->
    ensure_table_exists(),
    ets:insert(patterns_table_name(), {Label, prepare_pattern(Pattern), prepare_limit(Limit)}),
    ok.

prepare_limit(all) -> all;
prepare_limit(none) -> none;
prepare_limit(Limit) when is_binary(Limit) -> [Limit];
prepare_limit(Limit) when is_atom(Limit) -> [Limit];
prepare_limit(Limit) when is_list(Limit) -> Limit.

prepare_pattern(Pattern) when is_function(Pattern) -> Pattern;
prepare_pattern(Pattern) when is_map(Pattern) -> maps:to_list(Pattern).


ensure_table_exists() ->
    case ets:info(patterns_table_name()) of
        undefined ->
            case whereis(recon_ets_maps) of
                undefined ->
                    Parent = self(),
                    Ref = make_ref(),
                    %% attach to the currently running session
                    {Pid, MonRef} = spawn_monitor(fun() ->
                        register(recon_ets_maps, self()),
                        ets:new(patterns_table_name(), [ordered_set, public, named_table]),
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

ets_keeper() ->
    receive
        stop -> ok;
        _ -> ets_keeper()
    end.

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

