%%
%% Parser of the Redis protocol, see http://redis.io/topics/protocol
%%
%% The idea behind this parser is that we accept any binary data
%% available on the socket. If there is not enough data to parse a
%% complete response, we ask the caller to call us later when there is
%% more data. If there is too much data, we only parse the first
%% response and let the caller call us again with the rest.
%%
%% This approach lets us write a "pure" parser that does not depend on
%% manipulating the socket, which erldis and redis-erl is
%% doing. Instead, we may ask the socket to send us data as fast as
%% possible and parse it continously. The overhead of manipulating the
%% socket when parsing multibulk responses is killing the performance
%% of erldis.
%%
%% Future improvements:
%%  * When we return a bulk continuation, we also include the size of
%%    the bulk. The caller may use this to explicitly call
%%    gen_tcp:recv/2 with the desired size.

-module(eredis_parser).
-include("eredis.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/0, parse/2]).

%% Exported for testing
-export([parse_bulk/1, parse_bulk/2,
         parse_multibulk/1, parse_multibulk/2, buffer_create/0, buffer_create/1]).

%%
%% API
%%

%% @doc: Initialize the parser
init() ->
    #pstate{}.


-spec parse(State::#pstate{}, Data::binary()) ->
                   {ok, return_value(), NewState::#pstate{}} |
                       {ok, return_value(), Rest::binary(), NewState::#pstate{}} |
                       {error, ErrString::binary(), NewState::#pstate{}} |
                       {error, ErrString::binary(), Rest::binary(), NewState::#pstate{}} |
                       {continue, NewState::#pstate{}}.

%% @doc: Parses the (possibly partial) response from Redis. Returns
%% either {ok, Value, NewState}, {ok, Value, Rest, NewState} or
%% {continue, NewState}. External entry point for parsing.
%%
%% In case {ok, Value, NewState} is returned, Value contains the value
%% returned by Redis. NewState will be an empty parser state.
%%
%% In case {ok, Value, Rest, NewState} is returned, Value contains the
%% most recent value returned by Redis, while Rest contains any extra
%% data that was given, but was not part of the same response. In this
%% case you should immeditely call parse again with Rest as the Data
%% argument and NewState as the State argument.
%%
%% In case {continue, NewState} is returned, more data is needed
%% before a complete value can be returned. As soon as you have more
%% data, call parse again with NewState as the State argument and any
%% new binary data as the Data argument.

%% Parser in initial state, the data we receive will be the beginning
%% of a response
parse(#pstate{state = undefined} = State, NewData) ->
    %% Look at the first byte to get the type of reply
    B = parse_type(NewData),
    case B of
        %% Status
        <<$+, Data/binary>> ->
            return_result(parse_simple(Data), State, status_continue);

        %% Error
        <<$-, Data/binary>> ->
            return_error(parse_simple(Data), State, status_continue);

        %% Integer reply
        <<$:, Data/binary>> ->
            return_result(parse_simple(Data), State, status_continue);

        %% Multibulk
        <<$*, _Rest/binary>> ->
            return_result(parse_multibulk(NewData), State, multibulk_continue);

        %% Bulk
        <<$$, _Rest/binary>> ->
            return_result(parse_bulk(NewData), State, bulk_continue);

        _ ->
            %% TODO: Handle the case where we start parsing a new
            %% response, but cannot make any sense of it
            {error, unknown_response}
    end;
%% The following clauses all match on different continuation states

parse(#pstate{state = bulk_continue,
              continuation_data = ContinuationData} = State, NewData) ->
    return_result(parse_bulk(ContinuationData, NewData), State, bulk_continue);

parse(#pstate{state = multibulk_continue,
              continuation_data = ContinuationData} = State, NewData) ->
    return_result(parse_multibulk(ContinuationData, NewData), State, multibulk_continue);

parse(#pstate{state = status_continue,
             continuation_data = ContinuationData} = State, NewData) ->
    return_result(parse_simple(ContinuationData, NewData), State, status_continue).

%%
%% MULTIBULK
%%
parse_type(B) when is_binary(B) -> B;
parse_type(T=L) when is_list(T) -> list_to_binary(L);
parse_type(_)  -> "".

parse_multibulk(Data) when is_binary(Data) -> parse_multibulk(buffer_create(Data));

parse_multibulk(Data) when is_list(Data) -> parse_multibulk(buffer_create(list_to_binary(Data)));

parse_multibulk(Buffer) ->
    case get_newline_pos(Buffer) of
        undefined ->
            {continue, {incomplete_size, Buffer}};
        NewlinePos ->
            OffsetNewlinePos = NewlinePos - 1,
            <<$*, Size:OffsetNewlinePos/binary, ?NL, Bulk/binary>> = buffer_to_binary(Buffer),
            IntSize = list_to_integer(binary_to_list(Size)),
            do_parse_multibulk(IntSize, buffer_create(Bulk))
    end.

%% Size of multibulk was incomplete, try again
parse_multibulk({incomplete_size, Buffer}, NewData0) ->
    NewBuffer = buffer_append(Buffer, NewData0),
    parse_multibulk(NewBuffer);

%% Ran out of data inside do_parse_multibulk in parse_bulk, must
%% continue traversing the bulks
parse_multibulk({in_parsing_bulks, Count, Buffer, Acc},
                NewData0) ->
    NewBuffer = buffer_append(Buffer, NewData0),

    %% Continue where we left off
    do_parse_multibulk(Count, NewBuffer, Acc).

%% @doc: Parses the given number of bulks from Data. If Data does not
%% contain enough bulks, {continue, ContinuationData} is returned with
%% enough information to start parsing with the correct count and
%% accumulated data.
do_parse_multibulk(Count, Buffer) ->
    do_parse_multibulk(Count, Buffer, []).

do_parse_multibulk(-1, Buffer, []) ->
    {ok, undefined, buffer_to_binary(Buffer)};
do_parse_multibulk(0, Buffer, Acc) ->
    {ok, lists:reverse(Acc), buffer_to_binary(Buffer)};
do_parse_multibulk(Count, Buffer, Acc) ->
    case buffer_size(Buffer) == 0 of
      true -> {continue, {in_parsing_bulks, Count, buffer_create(), Acc}};
      false ->
        %% Try parsing the first bulk in Data, if it works, we get the
        %% extra data back that was not part of the bulk which we can
        %% recurse on.  If the bulk does not contain enough data, we
        %% return with a continuation and enough data to pick up where we
        %% left off. In the continuation we will get more data
        %% automagically in Data, so parsing the bulk might work.
        case parse_bulk(Buffer) of
            {ok, Value, Rest} ->
                do_parse_multibulk(Count - 1, buffer_create(Rest), [Value | Acc]);
            {continue, _} ->
                {continue, {in_parsing_bulks, Count, Buffer, Acc}}
        end
    end.

%%
%% BULK
%%

parse_bulk(Data) when is_binary(Data) -> parse_bulk(buffer_create(Data));
parse_bulk(Data) when is_list(Data) -> parse_bulk(buffer_create(list_to_binary(Data)));

parse_bulk(Buffer) ->
  case buffer_hd(Buffer) of
    [$*] -> parse_multibulk(Buffer);
    [$+] -> parse_simple(buffer_tl(Buffer));
    [$-] -> parse_simple(buffer_tl(Buffer));
    [$:] -> parse_simple(buffer_tl(Buffer));
    [$$] -> do_parse_bulk(Buffer)
  end.

%% Bulk, at beginning of response
do_parse_bulk(Buffer) ->
    %% Find the position of the first terminator, everything up until
    %% this point contains the size specifier. If we cannot find it,
    %% we received a partial response and need more data
    case get_newline_pos(Buffer) of
        undefined ->
            {continue, {incomplete_size, Buffer}};
        NewlinePos ->
            OffsetNewlinePos = NewlinePos - 1, % Take into account the first $
            <<$$, Size:OffsetNewlinePos/binary, Bulk/binary>> = buffer_to_binary(Buffer),
            IntSize = list_to_integer(binary_to_list(Size)),

            if
                %% Nil response from redis
                IntSize =:= -1 ->
                    <<?NL, Rest/binary>> = Bulk,
                    {ok, undefined, Rest};
                %% We have enough data for the entire bulk
                size(Bulk) - (size(<<?NL>>) * 2) >= IntSize ->
                    <<?NL, Value:IntSize/binary, ?NL, Rest/binary>> = Bulk,
                    {ok, Value, Rest};
                true ->
                    %% Need more data, so we send the bulk without the
                    %% size specifier to our future self
                    {continue, {IntSize, buffer_create(Bulk)}}
            end
    end.

%% Bulk, continuation from partial bulk size
parse_bulk({incomplete_size, Buffer}, NewData0) ->
    NewBuffer = buffer_append(Buffer, NewData0),
    parse_bulk(NewBuffer);

%% Bulk, continuation from partial bulk value
parse_bulk({IntSize, Buffer0}, Data) ->
    Buffer = buffer_append(Buffer0, Data),

    case buffer_size(Buffer) - (size(<<?NL>>) * 2) >= IntSize of
        true ->
            <<?NL, Value:IntSize/binary, ?NL, Rest/binary>> = buffer_to_binary(Buffer),
            {ok, Value, Rest};
        false ->
            {continue, {IntSize, Buffer}}
    end.


%%
%% SIMPLE REPLIES
%%
%% Handles replies on the following format:
%%   TData\r\n
%% Where T is a type byte, like '+', '-', ':'. Data is terminated by \r\n

%% @doc: Parse simple replies. Data must not contain type
%% identifier. Type must be handled by the caller.
parse_simple(Data) when is_binary(Data) -> parse_simple(buffer_create(Data));

parse_simple(Buffer) ->
    case get_newline_pos(Buffer) of
        undefined ->
            {continue, {incomplete_simple, Buffer}};
        NewlinePos ->
            <<Value:NewlinePos/binary, ?NL, Rest/binary>> = buffer_to_binary(Buffer),
            {ok, Value, Rest}
    end.

parse_simple({incomplete_simple, Buffer}, NewData0) ->
    NewBuffer = buffer_append(Buffer, NewData0),
    parse_simple(NewBuffer).

%%
%% INTERNAL HELPERS
%%
get_newline_pos({B, _}) ->
    case re:run(B, ?NL) of
        {match, [{Pos, _}]} -> Pos;
        nomatch -> undefined
    end.

buffer_create() ->
  {[], 0}.

buffer_create(Data) ->
  {[Data], byte_size(Data)}.

buffer_append({List, Size}, Binary) ->
  NewList = case List of
    [] -> [Binary];
    [Head | Tail] -> [Head, Tail, Binary]
  end,
  {NewList, Size + byte_size(Binary)}.

buffer_hd({[<<Char, _/binary>> | _], _}) -> [Char];
buffer_hd({[], _}) -> [].

buffer_tl({[<<_, RestBin/binary>> | Rest], Size}) -> {[RestBin | Rest], Size - 1}.

buffer_to_binary({List, _}) -> iolist_to_binary(List).

buffer_size({_, Size}) -> Size.

%% @doc: Helper for handling the result of parsing. Will update the
%% parser state with the continuation of given name if necessary.
return_result({ok, Value, <<>>}, _State, _StateName) ->
    {ok, Value, init()};
return_result({ok, Value, Rest}, _State, _StateName) ->
    {ok, Value, Rest, init()};
return_result({continue, ContinuationData}, State, StateName) ->
    {continue, State#pstate{state = StateName, continuation_data = ContinuationData}}.

%% @doc: Helper for returning an error. Uses return_result/3 and just transforms the {ok, ...} tuple into an error tuple
return_error(Result, State, StateName) ->
    case return_result(Result, State, StateName) of
        {ok, Value, ParserState} ->
            {error, Value, ParserState};
        {ok, Value, Rest, ParserState} ->
            {error, Value, Rest, ParserState};
        Res ->
            Res
    end.
