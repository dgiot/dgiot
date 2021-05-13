PROJECT = sqlparse
PROJECT_DESCRIPTION = A production-ready SQL parser written in pure Erlang
PROJECT_VERSION = 4.6.1

DEPS = jpparse
dep_jpparse = git-emqx https://hub.fastgit.org/K2InformaticsGmbH/jpparse 1.3.0

COVER = true

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

$(shell [ -f erlang.mk ] || curl -s -o erlang.mk https://raw.githubusercontent.com/emqx/erlmk/master/erlang.mk)
include erlang.mk
