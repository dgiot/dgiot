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

-export([open/2, conv/2, conv/3]).

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

test() ->
    {ok, M} = conv("gbk", "utf-8", unicode:characters_to_binary(<<"在线"/utf8>>)),
    {ok, DevAddr} = conv("utf-8", "gbk", M),
    io:format("DevAddr ~p ",[ unicode:characters_to_list(DevAddr)]).