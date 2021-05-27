%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_dcache).

-author("johnliu").
-include("dgiot.hrl").
-behaviour(gen_server).

-export([search/1, search/2, info/1, incr/2, incr/3, insert/2, loop/1, lookup/2, match_delete/2, match/2, delete/2, start_link/1, start_link/2]).
-export([start/0, start/1, start/2, insert/1, lookup/1, loop/2, match_delete/1, match/1, delete/1, save_to_disk/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DETS(Name), dgiot_utils:to_atom(lists:concat([d, Name]))).

-record(state, {name, opts = []}).


% --------------------------------------------------------------------
% External API
% --------------------------------------------------------------------
start() ->
    start(?DCACHE, #{}).

start(Name) ->
    start(Name, #{}).

start(Name, Options) ->
    Spec = ?CHILD2(Name, dgiot_dcache, worker, [Name, Options]),
    case supervisor:start_child(dgiot_sup, Spec) of
        {ok, _} ->
            {ok, Name};
        {error, Reason} ->
            {error, Reason}
    end.

info(Name) ->
    ets:info(Name).


insert(Objects) ->
    insert(?DCACHE, Objects).

insert(Name, {Key, Value}) ->
    dgiot_data:insert(Name, Key, Value).

match(Pattern) ->
    match(?DCACHE, Pattern).

match(Name, Pattern) ->
    dgiot_data:match(Name, Pattern).

delete(Key) ->
    delete(?DCACHE, Key).
delete(Name, Key) ->
    dgiot_data:delete(Name, Key).

lookup(Key) ->
    lookup(?DCACHE, Key).
lookup(Name, Key) ->
    dgiot_data:lookup(Name, Key).

search(Fun) ->
    search(?DCACHE, Fun).
search(Name, Fun) ->
    dgiot_data:search(Name, Fun).

loop(Fun) ->
    loop(?DCACHE, Fun).
loop(Name, Fun) ->
    dgiot_data:loop(Name, Fun).

incr(Key, Incr) ->
    incr(?DCACHE, Key, Incr).
incr(Name, Key, Incr) ->
    case catch ets:update_counter(Name, Key, Incr) of
        {'EXIT', _} ->
            case ets:insert(Name, {Key, Incr}) of
                ok ->
                    Incr;
                Err ->
                    Err
            end;
        Other ->
            Other
    end.


match_delete(Pattern) ->
    match_delete(?DCACHE, Pattern).
match_delete(Name, Pattern) ->
    dgiot_data:match_delete(Name, Pattern).

save_to_disk(Name) ->
    Name ! save.


start_link(Name) ->
    start_link(Name, #{}).

start_link(Name, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Options], []).


init([Options]) ->
    case maps:get(dets_info, Options, []) of
        [] ->
            dgiot_data:init(?DCACHE);
        Info ->
            dgiot_data:init(?DCACHE, Info)
    end,
    put(last, erlang:system_time(second)),
    load_from_dets(?DCACHE),
    save_to_dets(Options),
    {ok, #state{name = ?DCACHE, opts = Options}};

init([Name, Options]) ->
    case maps:get(dets_info, Options, []) of
        [] ->
            dgiot_data:init(Name);
        Info ->
            dgiot_data:init(Name, Info)
    end,
    put(last, erlang:system_time(second)),
    load_from_dets(Name),
    save_to_dets(Options),
    {ok, #state{name = Name, opts = Options}}.


handle_call({insert, Objects}, _From, State) ->
    Reply = insert(State#state.name, Objects),
    {reply, Reply, State};

handle_call({match, Pattern}, _From, State) ->
    Reply = match(State#state.name, Pattern),
    {reply, Reply, State};

handle_call({match_delete, Pattern}, _From, State) ->
    Reply = match_delete(State#state.name, Pattern),
    {reply, Reply, State};

handle_call({delete, Key}, _From, State) ->
    Reply = delete(State#state.name, Key),
    {reply, Reply, State};

handle_call({lookup, Key}, _From, State) ->
    Reply = lookup(State#state.name, Key),
    {reply, Reply, State};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(save_to_dets, #state{opts = Options} = State) ->
    check_save(State),
    save_to_dets(Options),
    {noreply, State};

handle_info(save, State) ->
    do_save(State),
    {noreply, State};

handle_info({save_gc,Pid}, State) ->
    case is_process_alive(Pid) of
        true ->
            erlang:garbage_collect(Pid),
            erlang:exit(Pid, brutal_kill);
        _ -> ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    dets:close(State#state.name),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


check_save(#state{name = Name, opts = Options} = State) ->
    Now = erlang:system_time(second),
    MaxSize = maps:get(size, Options, 50000),
    MaxTime = maps:get(max_time, Options, 60), % 60s
    MaxMemory = maps:get(memory, Options, 200 * 1024), % 200M
    Info = dgiot_dcache:info(Name),
    {size, CurSize} = lists:keyfind(size, 1, Info),
    {memory, CurMemory} = lists:keyfind(memory, 1, Info),
    case CurSize > 0 of
        true ->
            case CurMemory / 128 >= MaxMemory orelse CurSize >= MaxSize orelse Now - get(last) > MaxTime of
                true ->
                    do_save(State);
                false ->
                    ok
            end,
            open_dets(Name,
                fun() ->
                    dets:delete_all_objects(?DETS(Name)),
                    ets:to_dets(Name, ?DETS(Name))
                end);
        false ->
            ok
    end.

do_save(#state{name = Name, opts = Options}) ->
    Now = erlang:system_time(second),
    put(last, Now),
    case maps:get(handle, Options, undefined) of
        undefined ->
            ok;
        Callback ->
            put(last, Now),
            case Callback of
                {Mod, Fun} ->
                    Pid = erlang:spawn(fun() -> apply(Mod, Fun, []) end),
                    erlang:send_after(5000, self(), {save_gc, Pid});
                {Mod, Fun, Args} ->
                    Pid = erlang:spawn(fun() -> apply(Mod, Fun, Args) end),
                    erlang:send_after(5000, self(), {save_gc, Pid});
                Handle ->
                    Pid = erlang:spawn(fun() -> Handle(Name) end),
                    erlang:send_after(5000, self(), {save_gc, Pid})
            end
    end.

save_to_dets(Options) ->
    Time = maps:get(auto_save, Options, 30000),
    erlang:send_after(Time, self(), save_to_dets).

load_from_dets(Name) ->
    open_dets(Name,
        fun() ->
            dets:to_ets(?DETS(Name), Name)
        end).

open_dets(Name, Fun) ->
    case open_dets_(?DETS(Name)) of
        {ok, _Path} ->
            Fun(), dets:close(?DETS(Name));
        {error, Reason} ->
            {error, Reason}
    end.

open_dets_(Name) ->
    open_dets_(Name, []).
open_dets_(Name, Opts) ->
    Path = lists:concat(["data/", Name, ".dets"]),
    case filelib:ensure_dir(Path) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            case dets:open_file(Name, [{file, Path} | Opts]) of
                {ok, Name} ->
                    {ok, Path};
                {error, Why} ->
                    {error, Why}
            end
    end.
