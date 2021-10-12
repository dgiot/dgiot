
-module(emqx_bridge_nats_driver).

-behaviour(gen_server).
-include_lib("emqx/include/logger.hrl").
-include("emqx_bridge_nats.hrl").

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {conn}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

init(Opts) ->
    case nats:connect(list_to_binary(proplists:get_value(address,  Opts)), proplists:get_value(port,  Opts)) of
        {ok, Conn} -> 
            {ok, #state{conn = Conn}};
        {error, Reason} ->
            ?LOG(error, "[NATS] Can't connect to NATS server: ~p", [Reason]),
            {error, Reason}
    end.

handle_call({Payload, Topic}, _From, #state{conn = Conn} = State) ->
    nats:pub(Conn, Topic, #{payload => Payload}),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    % Info => {{teacup@ref, nats@teacup, #Ref<0.4137357160.4058775566.137942>}, ready}
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    nats:disconnect(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
