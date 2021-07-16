PROJECT = shuwa_group
PROJECT_DESCRIPTION = shuwa_group Plugin
PROJECT_VERSION = 1.5.4

CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)
BRANCH := $(if $(filter $(CUR_BRANCH), master develop), $(CUR_BRANCH), develop)

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx $(BRANCH)
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.1

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

ERLC_OPTS += +'{parse_transform, lager_transform}'


include erlang.mk

app.shuwa_group::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/shuwa_group.conf -i priv/shuwa_group.schema -d data
