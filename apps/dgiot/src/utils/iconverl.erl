%%% Copyright (c) 2010-2013 Aleksey Yeschenko <aleksey@yeschenko.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(iconverl).

-on_load(load_nif/0).

-export([open/2, conv/2, conv/3,  get_utf8/2]).

-export([test/0]).

-opaque cd() :: binary().
-export_type([cd/0]).

%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------

-spec open(string(), string()) -> cd().
open(_To, _From) ->
    erlang:nif_error(not_loaded).

-spec conv(cd(), binary()) -> {ok, binary()} | {error, atom()}.
conv(_CD, _Binary) ->
    erlang:nif_error(not_loaded).

-spec conv(string(), string(), binary()) -> {ok, binary()} | {error, atom()}.
conv(To, From, Binary) ->
    conv(open(To, From), Binary).

%% -------------------------------------------------------------------------
%% on_load callback
%% -------------------------------------------------------------------------
load_nif() ->
    case os:type() of
        {win32, _} ->
            ok;
        _ ->
            erlang:load_nif(filename:join(code:priv_dir(dgiot), iconv), 0)
    end.

get_utf8(Block, Type) ->
    case Block of
        <<>> ->
            <<>>;
        _ ->
            case os:type() of
                {win32, _} ->
                    Block;
                _ ->
                    case iconverl:conv("utf-8", dgiot_utils:to_list(Type),Block) of
                        {ok, M} ->
                            M;
                        _ ->
                            <<>>
                    end
            end
    end.

test() ->
    {ok, M} = iconverl:conv("GB18030", "utf-8", unicode:characters_to_binary(<<"【火警】1号主机2位3号"/utf8>>)),
    {ok, DevAddr} = iconverl:conv("utf-8", "GB18030", M),
%%    L3 =  dgiot_utils:hex_to_binary(<<"31BAC5BDD3BFDAB0E531BBD8C2B73130BAC500C5D6F7BBFAF898C">>),
%%    M = dgiot_utils:to_utf8(L3,"GB18030").
%%    io:format("DevAddr ~ts ", [unicode:characters_to_list(M)]).
    io:format("DevAddr ~ts ", [unicode:characters_to_list(DevAddr)]).