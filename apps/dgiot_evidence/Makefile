PROJECT = dgiot_evidence
PROJECT_DESCRIPTION = dgiot_evidence Plugin
PROJECT_VERSION = 1.6.4

CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)
BRANCH := $(if $(filter $(CUR_BRANCH), master develop), $(CUR_BRANCH), develop)

BUILD_DEPS = emqx cuttlefish
dep_emqx = git-emqx https://github.com/emqx/emqx $(BRANCH)
dep_cuttlefish = git-emqx https://github.com/emqx/cuttlefish v2.2.1

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/dgiot_evidence.conf -i priv/dgiot_evidence.schema -d data

