# See LICENSE for licensing information.

PROJECT = ranch
PROJECT_DESCRIPTION = Socket acceptor pool for TCP protocols.
PROJECT_VERSION = 1.7.1
PROJECT_REGISTERED = ranch_server

# Options.

CT_OPTS += -pa test -ct_hooks ranch_ct_hook [] # -boot start_sasl
PLT_APPS = crypto public_key tools

# Dependencies.

LOCAL_DEPS = ssl

DOC_DEPS = asciideck

TEST_DEPS = $(if $(CI_ERLANG_MK),ci.erlang.mk) ct_helper
dep_ct_helper = git https://github.com/ninenines/ct_helper master

# CI configuration.

dep_ci.erlang.mk = git https://github.com/ninenines/ci.erlang.mk master
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-19+
AUTO_CI_HIPE ?= OTP-LATEST
# AUTO_CI_ERLLVM ?= OTP-LATEST
AUTO_CI_WINDOWS ?= OTP-19+

# Standard targets.

include erlang.mk

# Compile options.

TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}'

# Dialyze the tests.

DIALYZER_OPTS += --src -r test

# Use erl_make_certs from the tested release during CI
# and ensure that ct_helper is always recompiled.
#
# Note that erl_make_certs was removed from OTP-20.1. For now
# we are fine using the most recent version from OTP-20.

ci-setup:: $(DEPS_DIR)/ct_helper
	$(gen_verbose) cp ~/.kerl/builds/$(CI_OTP_RELEASE)/otp_src_git/lib/ssl/test/erl_make_certs.erl deps/ct_helper/src/ || true
	$(gen_verbose) $(MAKE) -C $(DEPS_DIR)/ct_helper clean app
