## shallow clone for speed

REBAR_GIT_CLONE_OPTIONS += --depth 1
export REBAR_GIT_CLONE_OPTIONS

#TAG = $(shell git tag -l --points-at HEAD)

TAG = v4.0.0

#CUR_BRANCH := $(shell git branch | grep -e "^*" | cut -d' ' -f 2)

#CUR_BRANCH := release-4.0

#ifeq ($(EMQX_DEPS_DEFAULT_VSN),)
#	ifneq ($(TAG),)
		EMQX_DEPS_DEFAULT_VSN ?= $(TAG)
#	else
#		EMQX_DEPS_DEFAULT_VSN ?= $(CUR_BRANCH)
#	endif
#endif


REBAR = $(CURDIR)/rebar3

REBAR_URL = https://s3.amazonaws.com/rebar3/rebar3

export EMQX_DEPS_DEFAULT_VSN

PROFILE ?= dgiot
PROFILES := dgiot dgiot-edge
PKG_PROFILES := dgiot-pkg dgiot-edge-pkg

CT_APPS := emqx_auth_jwt emqx_auth_mysql emqx_auth_username \
		emqx_delayed_publish emqx_management emqx_recon emqx_rule_enginex \
		emqx_stomp emqx_auth_clientid  emqx_auth_ldap   emqx_auth_pgsql \
		emqx_coap emqx_lua_hook emqx_passwd emqx_reloader emqx_sn \
		emqx_web_hook emqx_auth_http emqx_auth_mongo emqx_auth_redis \
		emqx_dashboard emqx_lwm2m emqx_psk_file emqx_retainer emqx_statsd

.PHONY: default
default: $(REBAR) $(PROFILE)

.PHONY: all
all: $(REBAR) $(PROFILES)

.PHONY: distclean
distclean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock
	@rm -rf _checkouts

.PHONY: $(PROFILES)
$(PROFILES:%=%): $(REBAR)
ifneq ($(OS),Windows_NT)
	ln -snf _build/$(@)/lib ./_checkouts
endif
	$(REBAR) as $(@) release

.PHONY: $(PROFILES:%=build-%)
$(PROFILES:%=build-%): $(REBAR)
	$(REBAR) as $(@:build-%=%) compile

.PHONY: deps-all
deps-all: $(REBAR) $(PROFILES:%=deps-%) $(PKG_PROFILES:%=deps-%)

.PHONY: $(PROFILES:%=deps-%)
$(PROFILES:%=deps-%): $(REBAR)
	$(REBAR) as $(@:deps-%=%) get-deps

.PHONY: $(PKG_PROFILES:%=deps-%)
$(PKG_PROFILES:%=deps-%): $(REBAR)
	$(REBAR) as $(@:deps-%=%) get-deps

.PHONY: run $(PROFILES:%=run-%)
run: run-$(PROFILE)
$(PROFILES:%=run-%): $(REBAR)
ifneq ($(OS),Windows_NT)
	@ln -snf _build/$(@:run-%=%)/lib ./_checkouts
endif
	$(REBAR) as $(@:run-%=%) run

.PHONY: clean $(PROFILES:%=clean-%)
clean: $(PROFILES:%=clean-%)
$(PROFILES:%=clean-%): $(REBAR)
	@rm -rf _build/$(@:clean-%=%)
	@rm -rf _build/$(@:clean-%=%)+test

.PHONY: $(PROFILES:%=checkout-%)
$(PROFILES:%=checkout-%): $(REBAR) build-$(PROFILE)
	ln -s -f _build/$(@:checkout-%=%)/lib ./_checkouts

# Checkout current profile
.PHONY: checkout
checkout:
	@ln -s -f _build/$(PROFILE)/lib ./_checkouts

# Run ct for an app in current profile
.PHONY: $(REBAR) $(CT_APPS:%=ct-%)
ct: $(CT_APPS:%=ct-%)
$(CT_APPS:%=ct-%): checkout-$(PROFILE)
	$(REBAR) as $(PROFILE) ct --verbose --dir _checkouts/$(@:ct-%=%)/test --verbosity 50

$(REBAR):
ifneq ($(wildcard rebar3),rebar3)
	@curl -Lo rebar3 $(REBAR_URL) || wget $(REBAR_URL)
endif
	@chmod a+x rebar3

# Build packages
.PHONY: $(PKG_PROFILES)
$(PKG_PROFILES:%=%): $(REBAR)
	ln -snf _build/$(@)/lib ./_checkouts
	$(REBAR) as $(@) release
	EMQX_REL=$$(pwd) EMQX_BUILD=$(@) EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) make -C deploy/packages

# Build docker image
.PHONY: $(PROFILES:%=%-docker-build)
$(PROFILES:%=%-docker-build):
	@if [ ! -z `echo $(@) |grep -oE edge` ]; then \
		TARGET=dgiot/dgiot-edge EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) make -C deploy/docker; \
	else \
		TARGET=dgiot/dgiot EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) make -C deploy/docker; \
	fi;

# Save docker images
.PHONY: $(PROFILES:%=%-docker-save)
$(PROFILES:%=%-docker-save):
	@if [ ! -z `echo $(@) |grep -oE edge` ]; then \
		TARGET=dgiot/dgiot-edge EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) make -C deploy/docker save; \
	else \
		TARGET=dgiot/dgiot EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) make -C deploy/docker save; \
	fi;

# Push docker image
.PHONY: $(PROFILES:%=%-docker-push)
$(PROFILES:%=%-docker-push):
	@if [ ! -z `echo $(@) |grep -oE edge` ]; then \
		TARGET=dgiot/dgiot-edge EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) make -C deploy/docker push; \
		TARGET=dgiot/dgiot-edge EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) make -C deploy/docker manifest_list; \
	else \
		TARGET=dgiot/dgiot EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) make -C deploy/docker push; \
		TARGET=dgiot/dgiot EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) make -C deploy/docker manifest_list; \
	fi;

# Clean docker image
.PHONY: $(PROFILES:%=%-docker-clean)
$(PROFILES:%=%-docker-clean):
	@if [ ! -z `echo $(@) |grep -oE edge` ]; then \
		TARGET=dgiot/dgiot-edge EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) make -C deploy/docker clean; \
	else \
		TARGET=dgiot/dgiot EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) make -C deploy/docker clean; \
	fi;
