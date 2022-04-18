%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 八月 2020 17:52
%%%-------------------------------------------------------------------
-module(crypt2).
-author("kenneth").

-behaviour(gen_server).
-include_lib("dgiot/include/logger.hrl").
-import(dgiot_utils, [binary_to_hex/1]).
%% API
-export([run/2]).
-export([test/0]).

%% API
-export([new/1, start_link/1]).


%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).


-record(state, {}).


new(Count) ->
    Fun =
        fun(Id) ->
            Server = list_to_atom(lists:concat([crypt, Id])),
            supervisor:start_child(dgiot_sup, {Server, {?MODULE, start_link, [Server]}, permanent, 5000, worker, [Server]})
        end,
    dgiot:new_counter(crypt, Count),
    [Fun(Id) || Id <- lists:seq(1, Count)].


run(Fun, Args) ->
    {Fmt, Args1} = format(Args, {"java -classpath ~s Crypt ~s", [classpath(), Fun]}),
    Cmd = io_lib:format(Fmt, Args1),
    case run_cmd(Cmd) of
        {exit, Reason} ->
            case run_cmd(Cmd) of
                {exit, _} ->
                    {error, Reason};
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

run_cmd(CMD) ->
    Id = dgiot:update_counter(crypt),
    Server = list_to_atom(lists:concat([crypt, Id])),
%%    ?LOG(info,"~p,~p~n", [Server, list_to_binary(CMD)]),
    case catch gen_server:call(Server, {cmd, CMD}, 50000) of
        {'EXIT', Reason} ->
            {exit, Reason};
        Line ->
%%            ?LOG(info,"~p~n", [Line]),
            case re:replace(Line, "\n", "", [global, {return, binary}]) of
                <<"Exception ", _>> = Reason ->
                    {error, Reason};
                Result ->
                    Result
            end
    end.


start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).


init([]) ->
    {ok, #state{}}.

handle_call({cmd, CMD}, _From, State) ->
    Reply = os:cmd(CMD),
    os:cmd("ps -ef | grep java | grep -v grep | awk '{print $2}' | xargs kill -9"),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

format([], Acc) -> Acc;
format([Param | Params], {Fmt, Args}) ->
    format(Params, {Fmt ++ " ~s", Args ++ [format_param(Param)]}).

format_param(Param) ->
    binary_to_hex(Param).

classpath() ->
    {file, Here} = code:is_loaded(?MODULE),
    Dir = filename:dirname(filename:dirname(Here)),
    filename:join([Dir, "priv/"]).


test() ->
    crypt2:run(sign, [<<"1">>, <<"AF000001">>, <<"1111111111111111">>]).
