PROJECT = getopt
PROJECT_DESCRIPTION = Erlang module to parse command line arguments using the GNU getopt syntax
PROJECT_VERSION = v1.0.2

APPLICATION := getopt

ERLC_OPTS += +debug_info
ERLC_OPTS += +warnings_as_errors +warn_export_all +warn_unused_import

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk

rebar-clean:
	@rebar3 clean

rebar-compile:
	@rebar3 compile

rebar-dialyzer: compile
	@rebar3 dialyzer

rebar-edoc:
	@rebar3 edoc

rebar-shell:
	@rebar3 shell

rebar-test:
	@rebar3 eunit

