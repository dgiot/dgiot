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

-module(dgiot_parse_loader).
-author("kenneth").
-include("dgiot_parse.hrl").
-include_lib("dgiot/include/logger.hrl").
-behaviour(gen_server).

%% API
-export([start/5, start/6, start/7, start_link/8]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {from, name, class, query, page_count, page_index, page_size, current_size, max_total, page_first, callback}).

%%%===================================================================
%%% API
%%%===================================================================

start(Class, Query, PageSize, MaxTotal, Success) ->
    start(?DEFAULT, Class, Query, PageSize, MaxTotal, Success).

start(Class, Query, Index, PageSize, MaxTotal, Success) when is_integer(Index) ->
    start(?DEFAULT, Class, Query, 1, PageSize, MaxTotal, Success);

start(Name, Class, Query, PageSize, MaxTotal, Success) when is_binary(Name) ->
    start(Name, Class, Query, 1, PageSize, MaxTotal, Success).

start(_Name, _Class, _Query, PageIndex, PageSize, MaxTotal, _Success) when PageIndex == 0; PageSize == 0; MaxTotal == 0 ->
    ok;
start(Name, Class, Query, PageIndex, PageSize, MaxTotal, Success) ->
    ?LOG(error," Name ~p",[ Name]),
    case supervisor:start_child(dgiot_parse_loader_sup, [self(), Name, Class, Query, PageIndex, PageSize, MaxTotal, Success]) of
        {ok, _Pid} -> ok;
        {error, Reason} -> {error, Reason}
    end.


start_link(From, Name, Class, Query, PageIndex, PageSize, MaxPage, Success) ->
    gen_server:start_link(?MODULE, [From, Name, Class, Query, PageIndex, PageSize, MaxPage, Success], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([From, Name, Class, Query, PageIndex, PageSize, MaxTotal, Success]) ->
    ?LOG(error," From ~p",[ From]),
    erlang:send_after(1000, self(), start),
    {ok, #state{
        from = From,
        name = Name,
        class = Class,
        query = Query,
        page_first = PageIndex,
        page_index = PageIndex,
        page_size = min(PageSize, MaxTotal),
        current_size = min(PageSize, MaxTotal),
        max_total = MaxTotal,
        callback = Success
    }};

init(Args) ->
    ?LOG(error," Args ~p",[ Args]).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(start, #state{ name = Name, class = Class, query = Query} = State) ->
%%    ?LOG(error," State ~p",[ State]),
    case dgiot_parse:query_object(Name, Class, Query#{<<"limit">> => 0, <<"count">> => 1}) of
        {error, _Reason} ->
            erlang:send_after(3000, self(), start),
            {noreply, State};
        {ok, #{<<"count">> := Count}} ->
            NewState = State#state{page_count = Count},
            erlang:send_after(500, self(), {load, NewState}),
            {noreply, NewState}
    end;

handle_info({load, NewState}, _State) ->
    load_page(NewState),
    {noreply, NewState};

handle_info(complete, #state{from = _From} = State) ->
    %From ! load_complete,
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_page(#state{
    name = Name,
    query = Query,
    class = Class,
    page_size = PageSize,
    current_size = CurrentSize,
    page_index = PageIndex,
    page_first = PageFirst,
    max_total = MaxTotal,
    page_count = Count,
    callback = Callback
} = State) ->
    Skip = (PageIndex - 1) * PageSize,
    case Count > 0 andalso Skip < Count of
        true ->
            case dgiot_parse:read_page(Name, Class, Query, Skip, CurrentSize) of
                {error, Reason} ->
                    ?LOG(error,"load ~p error, Query:~p, Reason:~p", [Class, Query, Reason]),
                    erlang:send_after(3000, self(), {load, State#state{
                        page_index = PageIndex
                    }});
                {ok, Page} ->
                    Total = min(Count - (PageFirst - 1) * PageSize, MaxTotal),
                    LoadCount = (PageIndex - PageFirst) * PageSize + length(Page),
                    Rate = io_lib:format("~.2f%", [LoadCount / Total * 100]),
                    Callback(Page),
                    LastCount = Total - LoadCount,
                    case LastCount =< 0 of
                        true ->
                            ?LOG(info,"~s Load ~s [~p/~p] Index:~p", [Rate, Class, LoadCount, Total, PageIndex]),
                            self() ! complete;
                        false ->
                            %?LOG(info,"~s Load ~s [~p/~p] Index:~p", [Rate, Class, LoadCount, Total, PageIndex]),
                            case LastCount > PageSize of
                                true ->
                                    load_page(State#state{page_index = PageIndex + 1});
                                false ->
                                    load_page(State#state{page_index = PageIndex + 1, current_size = LastCount})
                            end
                    end
            end;
        false ->
            self() ! complete
    end.
