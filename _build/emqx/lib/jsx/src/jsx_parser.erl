%% The MIT License

%% Copyright (c) 2010-2013 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.


-module(jsx_parser).

-export([parser/3, resume/5]).
-export([init/1, handle_event/2]).


-spec parser(Handler::module(), State::any(), Config::list()) -> jsx:parser().

parser(Handler, State, Config) ->
    fun(Tokens) -> value(Tokens, {Handler, Handler:init(State)}, [], jsx_config:parse_config(Config)) end.


%% resume allows continuation from interrupted decoding without having to explicitly export
%%  all states
-spec resume(
        Rest::jsx:token(),
        State::atom(),
        Handler::{atom(), any()},
        Stack::list(atom()),
        Config::jsx:config()
    ) -> jsx:parser() | {incomplete, jsx:parser()}.

resume(Rest, State, Handler, Stack, Config) ->
    case State of
        value -> value(Rest, Handler, Stack, Config);
        object -> object(Rest, Handler, Stack, Config);
        array -> array(Rest, Handler, Stack, Config);
        maybe_done -> maybe_done(Rest, Handler, Stack, Config);
        done -> done(Rest, Handler, Stack, Config)
    end.


-include("jsx_config.hrl").


%% error, incomplete and event macros
-ifndef(error).
-define(error(State, Terms, Handler, Stack, Config),
    case Config#config.error_handler of
        false -> erlang:error(badarg);
        F -> F(Terms, {parser, State, Handler, Stack}, jsx_config:config_to_list(Config))
    end

).
-endif.


incomplete(State, Handler, Stack, Config=#config{stream=false}) ->
    ?error(State, [], Handler, Stack, Config);
incomplete(State, Handler, Stack, Config=#config{incomplete_handler=false}) ->
    {incomplete, fun(End) when End == end_stream; End == end_json ->
                case resume([end_json], State, Handler, Stack, Config) of
                    {incomplete, _} -> ?error(State, [], Handler, Stack, Config);
                    Else -> Else
                end;
            (Tokens) ->
                resume(Tokens, State, Handler, Stack, Config)
            end
    };
incomplete(State, Handler, Stack, Config=#config{incomplete_handler=F}) ->
    F([], {parser, State, Handler, Stack}, jsx_config:config_to_list(Config)).


handle_event(Event, {Handler, State}, _Config) -> {Handler, Handler:handle_event(Event, State)}.


value([String|Tokens], Handler, Stack, Config) when is_binary(String) ->
    try clean_string(String, Config) of Clean ->
        maybe_done(Tokens, handle_event({string, Clean}, Handler, Config), Stack, Config)
    catch error:badarg ->
        ?error(value, [{string, String}|Tokens], Handler, Stack, Config)
    end;
value([true|Tokens], Handler, Stack, Config) ->
    maybe_done(Tokens, handle_event({literal, true}, Handler, Config), Stack, Config);
value([false|Tokens], Handler, Stack, Config) ->
    maybe_done(Tokens, handle_event({literal, false}, Handler, Config), Stack, Config);
value([null|Tokens], Handler, Stack, Config) ->
    maybe_done(Tokens, handle_event({literal, null}, Handler, Config), Stack, Config);
value([start_object|Tokens], Handler, Stack, Config) ->
    object(Tokens, handle_event(start_object, Handler, Config), [object|Stack], Config);
value([start_array|Tokens], Handler, Stack, Config) ->
    array(Tokens, handle_event(start_array, Handler, Config), [array|Stack], Config);
value([Number|Tokens], Handler, Stack, Config) when is_integer(Number) ->
    maybe_done(Tokens, handle_event({integer, Number}, Handler, Config), Stack, Config);
value([Number|Tokens], Handler, Stack, Config) when is_float(Number) ->
    maybe_done(Tokens, handle_event({float, Number}, Handler, Config), Stack, Config);
value([{raw, Raw}|Tokens], Handler, Stack, Config) when is_binary(Raw) ->
    value((jsx:decoder(?MODULE, [], []))(Raw) ++ Tokens, Handler, Stack, Config);
value([{_,_,_}=Timestamp|Tokens], Handler, Stack, Config) ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_datetime(
                                                   Timestamp),
  value([{string, unicode:characters_to_binary(io_lib:format(
         "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
         [Year, Month, Day, Hour, Min, Sec]
       ))}|Tokens],
        Handler,
        Stack,
        Config
       );
value([{{Year, Month, Day}, {Hour, Min, Sec}}|Tokens], Handler, Stack, Config)
when is_integer(Year), is_integer(Month), is_integer(Day), is_integer(Hour), is_integer(Min), is_integer(Sec) ->
    value([{string, unicode:characters_to_binary(io_lib:format(
            "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
            [Year, Month, Day, Hour, Min, Sec]
        ))}|Tokens],
        Handler,
        Stack,
        Config
    );
value([{{Year, Month, Day}, {Hour, Min, Sec}}|Tokens], Handler, Stack, Config)
when is_integer(Year), is_integer(Month), is_integer(Day), is_integer(Hour), is_integer(Min), is_float(Sec) ->
    value([{string, unicode:characters_to_binary(io_lib:format(
            "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~9.6.0fZ",
            [Year, Month, Day, Hour, Min, Sec]
        ))}|Tokens],
        Handler,
        Stack,
        Config
    );
value([{literal, Value}|Tokens], Handler, Stack, Config)
when Value == true; Value == false; Value == null ->
    value([Value] ++ Tokens, Handler, Stack, Config);
value([{integer, Value}|Tokens], Handler, Stack, Config)
when is_integer(Value) ->
    value([Value] ++ Tokens, Handler, Stack, Config);
value([{float, Value}|Tokens], Handler, Stack, Config)
when is_float(Value) ->
    value([Value] ++ Tokens, Handler, Stack, Config);
value([{string, Value}|Tokens], Handler, Stack, Config)
when is_binary(Value); is_atom(Value) ->
    value([Value] ++ Tokens, Handler, Stack, Config);
value([{number, Value}|Tokens], Handler, Stack, Config)
when is_float(Value); is_integer(Value) ->
    value([Value] ++ Tokens, Handler, Stack, Config);
value([String|Tokens], Handler, Stack, Config) when is_atom(String) ->
    value([{string, atom_to_binary(String, utf8)}] ++ Tokens, Handler, Stack, Config);
value([], Handler, Stack, Config) ->
    incomplete(value, Handler, Stack, Config);
value(BadTokens, Handler, Stack, Config) when is_list(BadTokens) ->
    ?error(value, BadTokens, Handler, Stack, Config);
value(Token, Handler, Stack, Config) ->
    value([Token], Handler, Stack, Config).


object([end_object|Tokens], Handler, [object|Stack], Config) ->
    maybe_done(Tokens, handle_event(end_object, Handler, Config), Stack, Config);
object([{key, Key}|Tokens], Handler, Stack, Config)
when is_atom(Key); is_binary(Key); is_integer(Key) ->
    object([Key|Tokens], Handler, Stack, Config);
object([Key|Tokens], Handler, [object|Stack], Config)
when is_atom(Key); is_binary(Key); is_integer(Key) ->
    try clean_string(fix_key(Key), Config)
    of K ->
        value(
            Tokens,
            handle_event({key, K}, Handler, Config),
            [object|Stack],
            Config
        )
    catch error:badarg ->
        ?error(object, [{string, Key}|Tokens], Handler, Stack, Config)
    end;
object([], Handler, Stack, Config) ->
    incomplete(object, Handler, Stack, Config);
object(Token, Handler, Stack, Config) ->
    object([Token], Handler, Stack, Config).


array([end_array|Tokens], Handler, [array|Stack], Config) ->
    maybe_done(Tokens, handle_event(end_array, Handler, Config), Stack, Config);
array([], Handler, Stack, Config) ->
    incomplete(array, Handler, Stack, Config);
array(Tokens, Handler, Stack, Config) when is_list(Tokens) ->
    value(Tokens, Handler, Stack, Config);
array(Token, Handler, Stack, Config) ->
    array([Token], Handler, Stack, Config).


maybe_done([end_json], Handler, [], Config) ->
    done([end_json], Handler, [], Config);
maybe_done(Tokens, Handler, [object|_] = Stack, Config) when is_list(Tokens) ->
    object(Tokens, Handler, Stack, Config);
maybe_done(Tokens, Handler, [array|_] = Stack, Config) when is_list(Tokens) ->
    array(Tokens, Handler, Stack, Config);
maybe_done([], Handler, Stack, Config) ->
    incomplete(maybe_done, Handler, Stack, Config);
maybe_done(BadTokens, Handler, Stack, Config) when is_list(BadTokens) ->
    ?error(maybe_done, BadTokens, Handler, Stack, Config);
maybe_done(Token, Handler, Stack, Config) ->
    maybe_done([Token], Handler, Stack, Config).


done([], Handler, [], Config=#config{stream=true}) ->
    incomplete(done, Handler, [], Config);
done(Tokens, Handler, [], Config) when Tokens == [end_json]; Tokens == [] ->
    {_, State} = handle_event(end_json, Handler, Config),
    State;
done(BadTokens, Handler, Stack, Config) when is_list(BadTokens) ->
    ?error(done, BadTokens, Handler, Stack, Config);
done(Token, Handler, Stack, Config) ->
    done([Token], Handler, Stack, Config).


fix_key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
fix_key(Key) when is_integer(Key) -> list_to_binary(integer_to_list(Key));
fix_key(Key) when is_binary(Key) -> Key.


clean_string(Bin, #config{dirty_strings=true}) -> Bin;
clean_string(Bin, Config) -> clean(Bin, [], Config).


%% unroll the control characters
clean(<<0, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(0, Config)], Config);
clean(<<1, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(1, Config)], Config);
clean(<<2, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(2, Config)], Config);
clean(<<3, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(3, Config)], Config);
clean(<<4, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(4, Config)], Config);
clean(<<5, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(5, Config)], Config);
clean(<<6, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(6, Config)], Config);
clean(<<7, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(7, Config)], Config);
clean(<<8, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(8, Config)], Config);
clean(<<9, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(9, Config)], Config);
clean(<<10, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(10, Config)], Config);
clean(<<11, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(11, Config)], Config);
clean(<<12, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(12, Config)], Config);
clean(<<13, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(13, Config)], Config);
clean(<<14, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(14, Config)], Config);
clean(<<15, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(15, Config)], Config);
clean(<<16, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(16, Config)], Config);
clean(<<17, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(17, Config)], Config);
clean(<<18, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(18, Config)], Config);
clean(<<19, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(19, Config)], Config);
clean(<<20, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(20, Config)], Config);
clean(<<21, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(21, Config)], Config);
clean(<<22, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(22, Config)], Config);
clean(<<23, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(23, Config)], Config);
clean(<<24, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(24, Config)], Config);
clean(<<25, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(25, Config)], Config);
clean(<<26, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(26, Config)], Config);
clean(<<27, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(27, Config)], Config);
clean(<<28, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(28, Config)], Config);
clean(<<29, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(29, Config)], Config);
clean(<<30, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(30, Config)], Config);
clean(<<31, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(31, Config)], Config);
clean(<<34, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(34, Config)], Config);
clean(<<47, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(47, Config)], Config);
clean(<<92, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(92, Config)], Config);
clean(<<X/utf8, Rest/binary>> = Bin, Acc, Config=#config{uescape=true}) ->
    case X of
        X when X < 16#80 -> start_count(Bin, Acc, Config);
        _ -> clean(Rest, [Acc, json_escape_sequence(X)], Config)
    end;
%% u+2028
clean(<<226, 128, 168, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(16#2028, Config)], Config);
%% u+2029
clean(<<226, 128, 169, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(16#2029, Config)], Config);
clean(<<_/utf8, _/binary>> = Bin, Acc, Config) -> start_count(Bin, Acc, Config);
%% surrogates
clean(<<237, X, _, Rest/binary>>, Acc, Config) when X >= 160 ->
    clean(Rest, [Acc, maybe_replace(surrogate, Config)], Config);
%% overlong encodings and missing continuations of a 2 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 192, X =< 223 ->
    clean(strip_continuations(Rest, 1), [Acc, maybe_replace(badutf, Config)], Config);
%% overlong encodings and missing continuations of a 3 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 224, X =< 239 ->
    clean(strip_continuations(Rest, 2), [Acc, maybe_replace(badutf, Config)], Config);
%% overlong encodings and missing continuations of a 4 byte sequence
clean(<<X, Rest/binary>>, Acc, Config) when X >= 240, X =< 247 ->
    clean(strip_continuations(Rest, 3), [Acc, maybe_replace(badutf, Config)], Config);
clean(<<_, Rest/binary>>, Acc, Config) ->
    clean(Rest, [Acc, maybe_replace(badutf, Config)], Config);
clean(<<>>, Acc, _) -> iolist_to_binary(Acc).


start_count(Bin, Acc, Config) ->
    Size = count(Bin, 0, Config),
    <<Clean:Size/binary, Rest/binary>> = Bin,
    clean(Rest, [Acc, Clean], Config).


%% again, unrolling ascii makes a huge difference. sadly
count(<<0, _/binary>>, N, _) -> N;
count(<<1, _/binary>>, N, _) -> N;
count(<<2, _/binary>>, N, _) -> N;
count(<<3, _/binary>>, N, _) -> N;
count(<<4, _/binary>>, N, _) -> N;
count(<<5, _/binary>>, N, _) -> N;
count(<<6, _/binary>>, N, _) -> N;
count(<<7, _/binary>>, N, _) -> N;
count(<<8, _/binary>>, N, _) -> N;
count(<<9, _/binary>>, N, _) -> N;
count(<<10, _/binary>>, N, _) -> N;
count(<<11, _/binary>>, N, _) -> N;
count(<<12, _/binary>>, N, _) -> N;
count(<<13, _/binary>>, N, _) -> N;
count(<<14, _/binary>>, N, _) -> N;
count(<<15, _/binary>>, N, _) -> N;
count(<<16, _/binary>>, N, _) -> N;
count(<<17, _/binary>>, N, _) -> N;
count(<<18, _/binary>>, N, _) -> N;
count(<<19, _/binary>>, N, _) -> N;
count(<<20, _/binary>>, N, _) -> N;
count(<<21, _/binary>>, N, _) -> N;
count(<<22, _/binary>>, N, _) -> N;
count(<<23, _/binary>>, N, _) -> N;
count(<<24, _/binary>>, N, _) -> N;
count(<<25, _/binary>>, N, _) -> N;
count(<<26, _/binary>>, N, _) -> N;
count(<<27, _/binary>>, N, _) -> N;
count(<<28, _/binary>>, N, _) -> N;
count(<<29, _/binary>>, N, _) -> N;
count(<<30, _/binary>>, N, _) -> N;
count(<<31, _/binary>>, N, _) -> N;
count(<<32, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<33, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<34, _/binary>>, N, _) -> N;
count(<<35, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<36, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<37, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<38, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<39, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<40, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<41, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<42, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<43, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<44, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<45, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<46, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<47, _/binary>>, N, _) -> N;
count(<<48, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<49, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<50, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<51, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<52, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<53, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<54, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<55, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<56, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<57, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<58, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<59, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<60, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<61, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<62, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<63, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<64, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<65, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<66, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<67, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<68, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<69, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<70, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<71, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<72, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<73, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<74, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<75, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<76, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<77, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<78, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<79, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<80, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<81, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<82, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<83, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<84, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<85, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<86, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<87, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<88, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<89, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<90, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<91, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<92, _/binary>>, N, _) -> N;
count(<<93, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<94, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<95, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<96, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<97, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<98, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<99, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<100, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<101, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<102, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<103, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<104, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<105, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<106, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<107, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<108, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<109, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<110, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<111, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<112, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<113, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<114, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<115, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<116, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<117, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<118, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<119, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<120, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<121, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<122, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<123, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<124, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<125, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<126, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<127, Rest/binary>>, N, Config) ->
    count(Rest, N + 1, Config);
count(<<_/utf8, _/binary>>, N, #config{uescape=true}) -> N;
count(<<X/utf8, Rest/binary>>, N, Config) ->
    case X of
        X when X < 16#800 -> count(Rest, N + 2, Config);
        16#2028 -> N;
        16#2029 -> N;
        X when X < 16#10000 -> count(Rest, N + 3, Config);
        _ -> count(Rest, N + 4, Config)
    end;
count(<<_, _/binary>>, N, _) -> N;
count(<<>>, N, _) -> N.


strip_continuations(Bin, 0) -> Bin;
strip_continuations(<<X, Rest/binary>>, N) when X >= 128, X =< 191 ->
    strip_continuations(Rest, N - 1);
%% not a continuation byte
strip_continuations(Bin, _) -> Bin.


maybe_replace($\b, #config{escaped_strings=true}) -> <<$\\, $b>>;
maybe_replace($\t, #config{escaped_strings=true}) -> <<$\\, $t>>;
maybe_replace($\n, #config{escaped_strings=true}) -> <<$\\, $n>>;
maybe_replace($\f, #config{escaped_strings=true}) -> <<$\\, $f>>;
maybe_replace($\r, #config{escaped_strings=true}) -> <<$\\, $r>>;
maybe_replace($\", #config{escaped_strings=true}) -> <<$\\, $\">>;
maybe_replace($/, Config=#config{escaped_strings=true}) ->
    case Config#config.escaped_forward_slashes of
        true -> <<$\\, $/>>;
        false -> <<$/>>
    end;
maybe_replace($\\, #config{escaped_strings=true}) -> <<$\\, $\\>>;
maybe_replace(X, #config{escaped_strings=true}) when X < 32 ->
    json_escape_sequence(X);
maybe_replace(X, Config=#config{escaped_strings=true})  when X == 16#2028; X == 16#2029 ->
    case Config#config.unescaped_jsonp of
        true -> <<X/utf8>>;
        false -> json_escape_sequence(X)
    end;
maybe_replace(Atom, #config{strict_utf8=true}) when is_atom(Atom) ->
    erlang:error(badarg);
maybe_replace(surrogate, _Config) ->
    <<16#fffd/utf8>>;
maybe_replace(badutf, _Config) ->
    <<16#fffd/utf8>>;
maybe_replace(X, _Config) ->
    <<X/utf8>>.


%% convert a codepoint to it's \uXXXX equiv.
json_escape_sequence(X) when X < 65536 ->
    <<A:4, B:4, C:4, D:4>> = <<X:16>>,
    <<$\\, $u, (to_hex(A)), (to_hex(B)), (to_hex(C)), (to_hex(D))>>;
json_escape_sequence(X) ->
    Adjusted = X - 16#10000,
    <<A:10, B:10>> = <<Adjusted:20>>,
    [json_escape_sequence(A + 16#d800), json_escape_sequence(B + 16#dc00)].


to_hex(10) -> $a;
to_hex(11) -> $b;
to_hex(12) -> $c;
to_hex(13) -> $d;
to_hex(14) -> $e;
to_hex(15) -> $f;
to_hex(X) -> X + 48.    %% ascii "1" is [49], "2" is [50], etc...


%% for raw input
-spec init(proplists:proplist()) -> list().

init([]) -> [].


-spec handle_event(Event::any(), Acc::list()) -> list().

handle_event(end_json, State) -> lists:reverse(State);
handle_event(Event, State) -> [Event] ++ State.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


parse(Events, Config) -> value(Events, {jsx, []}, [], jsx_config:parse_config(Config)).


error_test_() ->
    [
        {"value error", ?_assertError(badarg, parse([self()], []))},
        {"maybe_done error", ?_assertError(badarg, parse([start_array, end_array, start_array, end_json], []))},
        {"done error", ?_assertError(badarg, parse([{string, <<"">>}, {literal, true}, end_json], []))},
        {"string error", ?_assertError(badarg, parse([{string, <<237, 160, 128>>}, end_json], [strict]))}
    ].


custom_error_handler_test_() ->
    Error = fun(Rest, {_, State, _, _}, _) -> {State, Rest} end,
    [
        {"value error", ?_assertEqual(
            {value, [self()]},
            parse([self()], [{error_handler, Error}])
        )},
        {"maybe_done error", ?_assertEqual(
            {maybe_done, [start_array, end_json]},
            parse([start_array, end_array, start_array, end_json], [{error_handler, Error}])
        )},
        {"done error", ?_assertEqual(
            {maybe_done, [{literal, true}, end_json]},
            parse([{string, <<"">>}, {literal, true}, end_json], [{error_handler, Error}])
        )},
        {"string error", ?_assertEqual(
            {value, [{string, <<237, 160, 128>>}, end_json]},
            parse([{string, <<237, 160, 128>>}, end_json], [{error_handler, Error}, strict])
        )}
    ].


incomplete_test_() ->
    Cases = [
        {"incomplete value", []},
        {"incomplete object", [start_object]},
        {"incomplete array", [start_array]},
        {"incomplete maybe_done", [start_array, end_array]}
    ],
    [{Title, ?_assertError(badarg, parse(Events, []))}
        || {Title, Events} <- Cases
    ].


custom_incomplete_handler_test_() ->
    [
        {"custom incomplete handler", ?_assertError(
            badarg,
            parse([], [{incomplete_handler, fun(_, _, _) -> erlang:error(badarg) end}])
        )}
    ].


raw_test_() ->
    Parse = fun(Events, Config) -> (parser(?MODULE, [], Config))(Events ++ [end_json]) end,
    [
        {"raw empty list", ?_assertEqual(
            [start_array, end_array],
            Parse([{raw, <<"[]">>}], [])
        )},
        {"raw empty object", ?_assertEqual(
            [start_object, end_object],
            Parse([{raw, <<"{}">>}], [])
        )},
        {"raw chunk inside stream", ?_assertEqual(
            [start_object, {key, <<"key">>}, start_array, {literal, true}, end_array, end_object],
            Parse([start_object, {key, <<"key">>}, {raw, <<"[true]">>}, end_object], [])
        )}
    ].


%% erlang refuses to encode certain codepoints, so fake them
to_fake_utf8(N) when N < 16#0080 -> <<N:8>>;
to_fake_utf8(N) when N < 16#0800 ->
    <<0:5, Y:5, X:6>> = <<N:16>>,
    <<2#110:3, Y:5, 2#10:2, X:6>>;
to_fake_utf8(N) when N < 16#10000 ->
    <<Z:4, Y:6, X:6>> = <<N:16>>,
    <<2#1110:4, Z:4, 2#10:2, Y:6, 2#10:2, X:6>>;
to_fake_utf8(N) ->
    <<0:3, W:3, Z:6, Y:6, X:6>> = <<N:24>>,
    <<2#11110:5, W:3, 2#10:2, Z:6, 2#10:2, Y:6, 2#10:2, X:6>>.


codepoints() ->
    unicode:characters_to_binary(
        [32, 33]
        ++ lists:seq(35, 46)
        ++ lists:seq(48, 91)
        ++ lists:seq(93, 16#2027)
        ++ lists:seq(16#202a, 16#d7ff)
        ++ lists:seq(16#e000, 16#ffff)
    ).


extended_codepoints() ->
    unicode:characters_to_binary(
        lists:seq(16#10000, 16#1ffff) ++ [
            16#20000, 16#30000, 16#40000, 16#50000, 16#60000,
            16#70000, 16#80000, 16#90000, 16#a0000, 16#b0000,
            16#c0000, 16#d0000, 16#e0000, 16#f0000, 16#100000
        ]
    ).


surrogates() -> [ to_fake_utf8(N) || N <- lists:seq(16#d800, 16#dfff) ].


clean_string_helper(String) ->
    try clean_string(String, #config{strict_utf8=true}) of Clean -> Clean
    catch error:badarg -> {error, badarg}
    end.


clean_string_test_() ->
    [
        {"clean codepoints", ?_assertEqual(
            codepoints(),
            clean_string(codepoints(), #config{})
        )},
        {"clean extended codepoints", ?_assertEqual(
            extended_codepoints(),
            clean_string(extended_codepoints(), #config{})
        )},
        {"escape path codepoints", ?_assertEqual(
            codepoints(),
            clean_string(codepoints(), #config{escaped_strings=true})
        )},
        {"escape path extended codepoints", ?_assertEqual(
            extended_codepoints(),
            clean_string(extended_codepoints(), #config{escaped_strings=true})
        )},
        {"error surrogates", ?_assertEqual(
            lists:duplicate(length(surrogates()), {error, badarg}),
            lists:map(fun(Codepoint) -> clean_string_helper(Codepoint) end, surrogates())
        )},
        {"clean surrogates", ?_assertEqual(
            lists:duplicate(length(surrogates()), <<16#fffd/utf8>>),
            lists:map(fun(Codepoint) -> clean_string(Codepoint, #config{}) end, surrogates())
        )}
    ].


escape_test_() ->
    [
        {"maybe_escape backspace", ?_assertEqual(
            <<"\\b">>,
            clean_string(<<16#0008/utf8>>, #config{escaped_strings=true})
        )},
        {"don't escape backspace", ?_assertEqual(
            <<"\b">>,
            clean_string(<<16#0008/utf8>>, #config{})
        )},
        {"maybe_escape tab", ?_assertEqual(
            <<"\\t">>,
            clean_string(<<16#0009/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape newline", ?_assertEqual(
            <<"\\n">>,
            clean_string(<<16#000a/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape formfeed", ?_assertEqual(
            <<"\\f">>,
            clean_string(<<16#000c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape carriage return", ?_assertEqual(
            <<"\\r">>,
            clean_string(<<16#000d/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape quote", ?_assertEqual(
            <<"\\\"">>,
            clean_string(<<16#0022/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape forward slash", ?_assertEqual(
            <<"\\/">>,
            clean_string(<<16#002f/utf8>>, #config{escaped_strings=true, escaped_forward_slashes=true})
        )},
        {"do not maybe_escape forward slash", ?_assertEqual(
            <<"/">>,
            clean_string(<<16#002f/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape backslash", ?_assertEqual(
            <<"\\\\">>,
            clean_string(<<16#005c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape jsonp (u2028)", ?_assertEqual(
            <<"\\u2028">>,
            clean_string(<<16#2028/utf8>>, #config{escaped_strings=true})
        )},
        {"do not maybe_escape jsonp (u2028)", ?_assertEqual(
            <<16#2028/utf8>>,
            clean_string(<<16#2028/utf8>>, #config{escaped_strings=true, unescaped_jsonp=true})
        )},
        {"maybe_escape jsonp (u2029)", ?_assertEqual(
            <<"\\u2029">>,
            clean_string(<<16#2029/utf8>>, #config{escaped_strings=true})
        )},
        {"do not maybe_escape jsonp (u2029)", ?_assertEqual(
            <<16#2029/utf8>>,
            clean_string(<<16#2029/utf8>>, #config{escaped_strings=true, unescaped_jsonp=true})
        )},
        {"maybe_escape u0000", ?_assertEqual(
            <<"\\u0000">>,
            clean_string(<<16#0000/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0001", ?_assertEqual(
            <<"\\u0001">>,
            clean_string(<<16#0001/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0002", ?_assertEqual(
            <<"\\u0002">>,
            clean_string(<<16#0002/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0003", ?_assertEqual(
            <<"\\u0003">>,
            clean_string(<<16#0003/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0004", ?_assertEqual(
            <<"\\u0004">>,
            clean_string(<<16#0004/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0005", ?_assertEqual(
            <<"\\u0005">>,
            clean_string(<<16#0005/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0006", ?_assertEqual(
            <<"\\u0006">>,
            clean_string(<<16#0006/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0007", ?_assertEqual(
            <<"\\u0007">>,
            clean_string(<<16#0007/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000b", ?_assertEqual(
            <<"\\u000b">>,
            clean_string(<<16#000b/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000e", ?_assertEqual(
            <<"\\u000e">>,
            clean_string(<<16#000e/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u000f", ?_assertEqual(
            <<"\\u000f">>,
            clean_string(<<16#000f/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0010", ?_assertEqual(
            <<"\\u0010">>,
            clean_string(<<16#0010/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0011", ?_assertEqual(
            <<"\\u0011">>,
            clean_string(<<16#0011/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0012", ?_assertEqual(
            <<"\\u0012">>,
            clean_string(<<16#0012/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0013", ?_assertEqual(
            <<"\\u0013">>,
            clean_string(<<16#0013/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0014", ?_assertEqual(
            <<"\\u0014">>,
            clean_string(<<16#0014/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0015", ?_assertEqual(
            <<"\\u0015">>,
            clean_string(<<16#0015/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0016", ?_assertEqual(
            <<"\\u0016">>,
            clean_string(<<16#0016/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0017", ?_assertEqual(
            <<"\\u0017">>,
            clean_string(<<16#0017/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0018", ?_assertEqual(
            <<"\\u0018">>,
            clean_string(<<16#0018/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u0019", ?_assertEqual(
            <<"\\u0019">>,
            clean_string(<<16#0019/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001a", ?_assertEqual(
            <<"\\u001a">>,
            clean_string(<<16#001a/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001b", ?_assertEqual(
            <<"\\u001b">>,
            clean_string(<<16#001b/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001c", ?_assertEqual(
            <<"\\u001c">>,
            clean_string(<<16#001c/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001d", ?_assertEqual(
            <<"\\u001d">>,
            clean_string(<<16#001d/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001e", ?_assertEqual(
            <<"\\u001e">>,
            clean_string(<<16#001e/utf8>>, #config{escaped_strings=true})
        )},
        {"maybe_escape u001f", ?_assertEqual(
            <<"\\u001f">>,
            clean_string(<<16#001f/utf8>>, #config{escaped_strings=true})
        )}
    ].


bad_utf8_test_() ->
    [
        {"orphan continuation byte u+0080", ?_assertError(
            badarg,
            clean_string(<<16#0080>>, #config{strict_utf8=true})
        )},
        {"orphan continuation byte u+0080 replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#0080>>, #config{})
        )},
        {"orphan continuation byte u+00bf", ?_assertError(
            badarg,
            clean_string(<<16#00bf>>, #config{strict_utf8=true})
        )},
        {"orphan continuation byte u+00bf replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#00bf>>, #config{})
        )},
        {"2 continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(binary:copy(<<16#0080>>, 2))/binary>>, #config{strict_utf8=true})
        )},
        {"2 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 2),
            clean_string(<<(binary:copy(<<16#0080>>, 2))/binary>>, #config{})
        )},
        {"3 continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(binary:copy(<<16#0080>>, 3))/binary>>, #config{strict_utf8=true})
        )},
        {"3 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 3),
            clean_string(<<(binary:copy(<<16#0080>>, 3))/binary>>, #config{})
        )},
        {"4 continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(binary:copy(<<16#0080>>, 4))/binary>>, #config{strict_utf8=true})
        )},
        {"4 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 4),
            clean_string(<<(binary:copy(<<16#0080>>, 4))/binary>>, #config{})
        )},
        {"5 continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(binary:copy(<<16#0080>>, 5))/binary>>, #config{strict_utf8=true})
        )},
        {"5 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 5),
            clean_string(<<(binary:copy(<<16#0080>>, 5))/binary>>, #config{})
        )},
        {"6 continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(binary:copy(<<16#0080>>, 6))/binary>>, #config{strict_utf8=true})
        )},
        {"6 continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, 6),
            clean_string(<<(binary:copy(<<16#0080>>, 6))/binary>>, #config{})
        )},
        {"all continuation bytes", ?_assertError(
            badarg,
            clean_string(<<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>, #config{strict_utf8=true})
        )},
        {"all continuation bytes replaced", ?_assertEqual(
            binary:copy(<<16#fffd/utf8>>, length(lists:seq(16#0080, 16#00bf))),
            clean_string(
                <<(list_to_binary(lists:seq(16#0080, 16#00bf)))/binary>>,
                #config{}
            )
        )},
        {"lonely start byte", ?_assertError(
            badarg,
            clean_string(<<16#00c0>>, #config{strict_utf8=true})
        )},
        {"lonely start byte replaced", ?_assertEqual(
            <<16#fffd/utf8>>,
            clean_string(<<16#00c0>>, #config{})
        )},
        {"lonely start bytes (2 byte)", ?_assertError(
            badarg,
            clean_string(<<16#00c0, 32, 16#00df>>, #config{strict_utf8=true})
        )},
        {"lonely start bytes (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00c0, 32, 16#00df>>, #config{})
        )},
        {"lonely start bytes (3 byte)", ?_assertError(
            badarg,
            clean_string(<<16#00e0, 32, 16#00ef>>, #config{strict_utf8=true})
        )},
        {"lonely start bytes (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00e0, 32, 16#00ef>>, #config{})
        )},
        {"lonely start bytes (4 byte)", ?_assertError(
            badarg,
            clean_string(<<16#00f0, 32, 16#00f7>>, #config{strict_utf8=true})
        )},
        {"lonely start bytes (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32, 16#fffd/utf8>>,
            clean_string(<<16#00f0, 32, 16#00f7>>, #config{})
        )},
        {"missing continuation byte (3 byte)", ?_assertError(
            badarg,
            clean_string(<<224, 160, 32>>, #config{strict_utf8=true})
        )},
        {"missing continuation byte (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<224, 160, 32>>, #config{})
        )},
        {"missing continuation byte (4 byte missing one)", ?_assertError(
            badarg,
            clean_string(<<240, 144, 128, 32>>, #config{strict_utf8=true})
        )},
        {"missing continuation byte (4 byte missing one) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<240, 144, 128, 32>>, #config{})
        )},
        {"missing continuation byte (4 byte missing two)", ?_assertError(
            badarg,
            clean_string(<<240, 144, 32>>, #config{strict_utf8=true})
        )},
        {"missing continuation byte (4 byte missing two) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<240, 144, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (2 byte)", ?_assertError(
            badarg,
            clean_string(<<16#c0, 16#af, 32>>, #config{strict_utf8=true})
        )},
        {"overlong encoding of u+002f (2 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#c0, 16#af, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (3 byte)", ?_assertError(
            badarg,
            clean_string(<<16#e0, 16#80, 16#af, 32>>, #config{strict_utf8=true})
        )},
        {"overlong encoding of u+002f (3 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#e0, 16#80, 16#af, 32>>, #config{})
        )},
        {"overlong encoding of u+002f (4 byte)", ?_assertError(
            badarg,
            clean_string(<<16#f0, 16#80, 16#80, 16#af, 32>>, #config{strict_utf8=true})
        )},
        {"overlong encoding of u+002f (4 byte) replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#f0, 16#80, 16#80, 16#af, 32>>, #config{})
        )},
        {"highest overlong 2 byte sequence", ?_assertError(
            badarg,
            clean_string(<<16#c1, 16#bf, 32>>, #config{strict_utf8=true})
        )},
        {"highest overlong 2 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#c1, 16#bf, 32>>, #config{})
        )},
        {"highest overlong 3 byte sequence", ?_assertError(
            badarg,
            clean_string(<<16#e0, 16#9f, 16#bf, 32>>, #config{strict_utf8=true})
        )},
        {"highest overlong 3 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#e0, 16#9f, 16#bf, 32>>, #config{})
        )},
        {"highest overlong 4 byte sequence", ?_assertError(
            badarg,
            clean_string(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, #config{strict_utf8=true})
        )},
        {"highest overlong 4 byte sequence replaced", ?_assertEqual(
            <<16#fffd/utf8, 32>>,
            clean_string(<<16#f0, 16#8f, 16#bf, 16#bf, 32>>, #config{})
        )}
    ].


json_escape_sequence_test_() ->
    [
        {"json escape sequence test - 16#0000", ?_assertEqual(<<"\\u0000"/utf8>>, json_escape_sequence(16#0000))},
        {"json escape sequence test - 16#abc", ?_assertEqual(<<"\\u0abc"/utf8>>, json_escape_sequence(16#abc))},
        {"json escape sequence test - 16#def", ?_assertEqual(<<"\\u0def"/utf8>>, json_escape_sequence(16#def))}
    ].


uescape_test_() ->
    [
        {"\"\\u0080\"", ?_assertEqual(
            <<"\\u0080">>,
            clean_string(<<128/utf8>>, #config{uescape=true})
        )},
        {"\"\\u8ca8\\u5481\\u3002\\u0091\\u0091\"", ?_assertEqual(
            <<"\\u8ca8\\u5481\\u3002\\u0091\\u0091">>,
            clean_string(
                <<232,178,168,229,146,129,227,128,130,194,145,194,145>>,
                #config{uescape=true}
            )
        )},
        {"\"\\ud834\\udd1e\"", ?_assertEqual(
            <<"\\ud834\\udd1e">>,
            clean_string(<<240, 157, 132, 158>>, #config{uescape=true})
        )},
        {"\"\\ud83d\\ude0a\"", ?_assertEqual(
            <<"\\ud83d\\ude0a">>,
            clean_string(<<240, 159, 152, 138>>, #config{uescape=true})
        )}
    ].


fix_key_test_() ->
    [
        {"binary key", ?_assertEqual(fix_key(<<"foo">>), <<"foo">>)},
        {"atom key", ?_assertEqual(fix_key(foo), <<"foo">>)},
        {"integer key", ?_assertEqual(fix_key(123), <<"123">>)}
    ].


datetime_test_() ->
    [
        {"datetime", ?_assertEqual(
            [start_array, {string, <<"2014-08-13T23:12:34Z">>}, end_array, end_json],
            parse([start_array, {{2014,08,13},{23,12,34}}, end_array, end_json], [])
        )},
        {"datetime", ?_assertEqual(
            [start_array, {string, <<"2014-08-13T23:12:34.363369Z">>}, end_array, end_json],
            parse([start_array, {{2014,08,13},{23,12,34.363369}}, end_array, end_json], [])
        )}
    ].


timestamp_test_() ->
    [
        {"timestamp", ?_assertEqual(
            [start_array, {string, <<"2016-01-15T18:19:28Z">>}, end_array, end_json],
            parse([start_array, {1452,881968,111772}, end_array, end_json], [])
        )}
    ].


rogue_tuple_test_() ->
    [
        {"kv in value position of object", ?_assertError(
          badarg,
          parse([start_object, <<"key">>, {<<"key">>, <<"value">>}, end_object, end_json], [])
        )},
        {"kv in value position of list", ?_assertError(
          badarg,
          parse([start_array, {<<"key">>, <<"value">>}, end_array, end_json], [])
        )}
    ].


-endif.
