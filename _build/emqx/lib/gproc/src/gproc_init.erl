%%%----------------------------------------------------------------------
%%% File     : gproc_init.erl
%%% Purpose  : GPROC init utilities
%%%----------------------------------------------------------------------

-module(gproc_init).

%% API
-export([
         %% soft reset
         soft_reset/0
         %% hard reset
         , hard_reset/0
        ]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% soft_reset

%% soft_reset
-spec soft_reset() -> ok.

soft_reset() ->
    ok = hard_reset(), %% soft reset isn't enough
    ok.

%%--------------------------------------------------------------------
%% hard_reset

%% hard_reset
-spec hard_reset() -> ok.

hard_reset() ->
    %% exit normal {n,'_','_'}
    _ = [ exit(Pid,normal) || Pid <- gproc:lookup_pids({n,'_','_'}),
                              (node(Pid) =/= node())
                                  orelse is_process_alive(Pid) ],
    %% kill via supervisor
    ok = supervisor:terminate_child(gproc_sup, gproc),
    %% delete ets table
    _ = [ ets:delete(Tab) || Tab <- ets:all(), Tab =:= gproc ],
    %% restart via supervisor
    {ok,_} = supervisor:restart_child(gproc_sup, gproc),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
