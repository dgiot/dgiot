PROJECT = minirest
PROJECT_DESCRIPTION = A Mini RESTful API Framework
PROJECT_VERSION = 0.2.2

BUILD_DEPS = jsx cowboy

dep_jsx    = git https://github.com/talentdeficit/jsx 2.9.0
dep_cowboy = git https://github.com/emqx/cowboy 2.7.1

ERLC_OPTS += +debug_info

EUNIT_OPTS = verbose

TEST_ERLC_OPTS += +debug_info

CT_SUITES = minirest

COVER = true

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

distclean::
	@rm -rf _build cover deps logs log data
	@rm -f rebar.lock compile_commands.json

rebar-deps:
	rebar3 get-deps

rebar-clean:
	@rebar3 clean

rebar-compile: rebar-deps
	rebar3 compile

rebar-ct:
	rebar3 ct

rebar-xref:
	@rebar3 xref
