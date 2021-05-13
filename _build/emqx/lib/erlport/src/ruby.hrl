%%% Copyright (c) 2009-2015, Dmitry Vasiliev <dima@hlabs.org>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%%  * Redistributions of source code must retain the above copyright notice,
%%%    this list of conditions and the following disclaimer.
%%%  * Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%  * Neither the name of the copyright holders nor the names of its
%%%    contributors may be used to endorse or promote products derived from
%%%    this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.

-ifndef(RUBY_HRL).
-define(RUBY_HRL, true).

-include("erlport.hrl").

-define(DEFAULT_RUBY, "ruby").
-define(RUBY_VAR_NAME, "ERLPORT_RUBY").

-record(ruby_options, {
    ruby = default :: string() | default,
    cd :: Path :: string() | undefined,
    use_stdio = use_stdio :: use_stdio | nouse_stdio,
    compressed = 0 :: 0..9,
    packet = 4 :: 1 | 2 | 4,
    env = [] :: [{EnvName :: string(), EnvValue :: string()}],
    ruby_lib = [] :: [Path :: string()],
    port_options = [binary, hide, exit_status]
        :: [Option :: atom() | {Name :: atom(), Value :: term()}],
    start_timeout = ?DEFAULT_START_TIMEOUT :: pos_integer() | infinity,
    call_timeout = ?DEFAULT_CALL_TIMEOUT :: pos_integer() | infinity,
    buffer_size = ?DEFAULT_BUFFER_SIZE :: pos_integer()
    }).

-define(RUBY_FIELDS, (lists:zip(record_info(fields, ruby_options),
    lists:seq(2, record_info(size, ruby_options))))).

-endif. % RUBY_HRL
