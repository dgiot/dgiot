%% Copyright (c) 2012, Magnus Klaar <klaar@ninenines.eu>
%% Copyright (c) 2013, Pedram Nimreezi <deadzen@deadzen.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


%% @doc Top level supervisor for goldrush.
%%
%% Main supervisor responsible for the {@link gr_counter_sup:start_link/0. 
%% <em>Counter</em>}, {@link gr_param_sup:start_link/0. <em>Param</em>} and
%% their {@link gr_manager_sup:start_link/0. <em>Manager</em>} supervisors.
-module(gr_sup).
-behaviour(supervisor).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, { {one_for_one, 50, 10}, [supervisor:child_spec()]} }.
init([]) ->
    CounterSup = ?CHILD(gr_counter_sup, supervisor),
    ParamSup = ?CHILD(gr_param_sup, supervisor),
    MgrSup = ?CHILD(gr_manager_sup, supervisor),
    {ok, {{one_for_one, 50, 10}, [CounterSup, ParamSup, MgrSup]}}.
