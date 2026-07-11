$(shell $(CURDIR)/scripts/git-hooks-init.sh)
REBAR = $(CURDIR)/rebar3
BUILD = $(CURDIR)/build
SCRIPTS = $(CURDIR)/scripts
export OTP_VSN ?= $(shell $(CURDIR)/scripts/get-otp-vsn.sh)
export PKG_VSN ?= $(shell $(CURDIR)/pkg-vsn.sh)
export DOCKERFILE := deploy/docker/Dockerfile
export DOCKERFILE_TESTING := deploy/docker/Dockerfile.testing
ifeq ($(OS),Windows_NT)
	export REBAR_COLOR=none
	FIND=/usr/bin/find
else
	FIND=find
endif

GET_DASHBOARD=$(SCRIPTS)/get-dashboard.sh

PROFILE ?= dgiot
REL_PROFILES := dgiot
PKG_PROFILES := dgiot-pkg
PROFILES := $(REL_PROFILES) $(PKG_PROFILES) default
CT_READABLE ?= true

ifeq ($(OS),Windows_NT)
	QUIKRUN=$(CURDIR)/_build/$(PROFILE)/rel/dgiot/bin/dgiot.cmd console
else
	QUIKRUN=$(CURDIR)/_build/$(PROFILE)/rel/dgiot/bin/dgiot console
endif

export REBAR_GIT_CLONE_OPTIONS += --depth=1

.PHONY: default
default: $(REBAR) $(PROFILE)

.PHONY: all
all: $(REBAR) $(PROFILES)

.PHONY: ensure-rebar3
ensure-rebar3:
	@$(SCRIPTS)/fail-on-old-otp-version.escript
	@$(SCRIPTS)/ensure-rebar3.sh

$(REBAR): ensure-rebar3

.PHONY: get-dashboard
get-dashboard:
	 @$(GET_DASHBOARD)

.PHONY: eunit
eunit: $(REBAR)
	@ENABLE_COVER_COMPILE=1 $(REBAR) eunit -v -c

.PHONY: proper
proper: $(REBAR)
	@ENABLE_COVER_COMPILE=1 $(REBAR) proper -d test/props -c

.PHONY: ct
ct: $(REBAR)
	@ENABLE_COVER_COMPILE=1 $(REBAR) ct --name 'test@127.0.0.1' -c -v

APPS=$(shell $(CURDIR)/scripts/find-apps.sh)

## app/name-ct targets are intended for local tests hence cover is not enabled
.PHONY: $(APPS:%=%-ct)
define gen-app-ct-target
$1-ct: $(REBAR)
	$(REBAR) ct --name 'test@127.0.0.1' -v --readable $(CT_READABLE) --suite $(shell $(CURDIR)/scripts/find-suites.sh $1)
endef
$(foreach app,$(APPS),$(eval $(call gen-app-ct-target,$(app))))

## app/name-ct-pipeline targets are used in pipeline -> make cover data for each app
.PHONY: $(APPS:%=%-ct-pipeline)
define gen-app-ct-target-pipeline
$1-ct-pipeline: $(REBAR)
	$(REBAR) ct --name 'test@127.0.0.1' -c -v --readable $(CT_READABLE) --cover_export_name $(PROFILE)-$(subst /,-,$1) --suite $(shell $(CURDIR)/scripts/find-suites.sh $1)
endef
$(foreach app,$(APPS),$(eval $(call gen-app-ct-target-pipeline,$(app))))

## apps/name-prop targets
.PHONY: $(APPS:%=%-prop)
define gen-app-prop-target
$1-prop:
	$(REBAR) proper -d test/props -v -m $(shell $(CURDIR)/scripts/find-props.sh $1)
endef
$(foreach app,$(APPS),$(eval $(call gen-app-prop-target,$(app))))

.PHONY: cover
cover: $(REBAR)
	@ENABLE_COVER_COMPILE=1 $(REBAR) cover

.PHONY: coveralls
coveralls: $(REBAR)
	@ENABLE_COVER_COMPILE=1 $(REBAR) as test coveralls send

.PHONY: $(REL_PROFILES)

$(REL_PROFILES:%=%): $(REBAR) get-dashboard
	@$(REBAR) as $(@) do compile,release

## Not calling rebar3 clean because
## 1. rebar3 clean relies on rebar3, meaning it reads config, fetches dependencies etc.
## 2. it's slow
## NOTE: this does not force rebar3 to fetch new version dependencies
## make clean-all to delete all fetched dependencies for a fresh start-over
.PHONY: clean $(PROFILES:%=clean-%)
clean: $(PROFILES:%=clean-%)
$(PROFILES:%=clean-%):
	@if [ -d _build/$(@:clean-%=%) ]; then \
		rm -f rebar.lock; \
		rm -rf _build/$(@:clean-%=%)/rel; \
		$(FIND) _build/$(@:clean-%=%) -name '*.beam' -o -name '*.so' -o -name '*.app' -o -name '*.appup' -o -name '*.o' -o -name '*.d' -type f | xargs rm -f; \
		$(FIND) _build/$(@:clean-%=%) -type l -delete; \
	fi

.PHONY: clean-all
clean-all:
	@rm -f rebar.lock
	@rm -rf _build
	@rm -f rebar.lock

.PHONY: deps-all
deps-all: $(REBAR) $(PROFILES:%=deps-%)
	@make clean # ensure clean at the end

## deps-<profile> is used in CI scripts to download deps and the
## share downloads between CI steps and/or copied into containers
## which may not have the right credentials
.PHONY: $(PROFILES:%=deps-%)
$(PROFILES:%=deps-%): $(REBAR) get-dashboard
	@$(REBAR) as $(@:deps-%=%) get-deps
	@rm -f rebar.lock

.PHONY: xref
xref: $(REBAR) $(REL_PROFILES:%=%-rel)
	@$(REBAR) as check xref
	@scripts/xref-check.escript

.PHONY: dialyzer
dialyzer: $(REBAR)
	@$(REBAR) as check dialyzer

COMMON_DEPS := $(REBAR) get-dashboard $(CONF_SEGS)

## rel target is to create release package without relup
.PHONY: $(REL_PROFILES:%=%-rel) $(PKG_PROFILES:%=%-rel)
$(REL_PROFILES:%=%-rel) $(PKG_PROFILES:%=%-rel): $(COMMON_DEPS)
	@$(BUILD) $(subst -rel,,$(@)) rel

## download relup base packages
.PHONY: $(REL_PROFILES:%=%-relup-downloads)
define download-relup-packages
$1-relup-downloads:
	@$(CURDIR)/scripts/relup-base-packages.sh $1
endef
ALL_ZIPS = $(REL_PROFILES)
$(foreach zt,$(ALL_ZIPS),$(eval $(call download-relup-packages,$(zt))))

## relup target is to create relup instructions
.PHONY: $(REL_PROFILES:%=%-relup)
define gen-relup-target
$1-relup: $1-relup-downloads $(COMMON_DEPS)
	@$(BUILD) $1 relup
endef
ALL_ZIPS = $(REL_PROFILES)
$(foreach zt,$(ALL_ZIPS),$(eval $(call gen-relup-target,$(zt))))

## zip target is to create a release package .zip with relup
.PHONY: $(REL_PROFILES:%=%-zip)
define gen-zip-target
$1-zip: $1-relup
	@$(BUILD) $1 zip
endef
ALL_ZIPS = $(REL_PROFILES)
$(foreach zt,$(ALL_ZIPS),$(eval $(call gen-zip-target,$(zt))))

## A pkg target depend on a regular release
.PHONY: $(PKG_PROFILES)
define gen-pkg-target
$1: $1-rel
	@$(BUILD) $1 pkg
endef
$(foreach pt,$(PKG_PROFILES),$(eval $(call gen-pkg-target,$(pt))))

## docker target is to create docker instructions
.PHONY: $(REL_PROFILES:%=%-docker)
define gen-docker-target
$1-docker: $(COMMON_DEPS)
	@$(BUILD) $1 docker
endef
ALL_ZIPS = $(REL_PROFILES)
$(foreach zt,$(ALL_ZIPS),$(eval $(call gen-docker-target,$(zt))))

## emqx-docker-testing
## emqx-ee-docker-testing
## is to directly copy a unzipped zip-package to a
## base image such as ubuntu20.04. Mostly for testing
.PHONY: $(REL_PROFILES:%=%-docker-testing)
define gen-docker-target-testing
$1-docker-testing: $(COMMON_DEPS)
	@$(BUILD) $1 docker-testing
endef
ALL_ZIPS = $(REL_PROFILES)
$(foreach zt,$(ALL_ZIPS),$(eval $(call gen-docker-target-testing,$(zt))))

.PHONY: run
run: $(PROFILE)
	@echo "=== DGAIOT 启动 ==="
	@_build/dgiot/rel/dgiot/bin/dgiot start 2>/dev/null || true
	@sleep 10
	@_build/dgiot/rel/dgiot/bin/dgiot ping && echo "✅ dgiot" || echo "⚠️ dgiot"
	@cd apps/dgiot_app && npx nest build > /dev/null 2>&1 && nohup node dist/main.js > /tmp/nestjs.log 2>&1 &
	@cd apps/dgiot_frontend && nohup npx vite --host 0.0.0.0 --port 8080 > /tmp/vite.log 2>&1 &
	@sleep 3
	@echo "✅ http://localhost:8080 (admin/admin123)"

.PHONY: dev
dev: $(PROFILE)
	@_build/dgiot/rel/dgiot/bin/dgiot start 2>/dev/null || true
	@sleep 8
	@cd apps/dgiot_frontend && npx vite --host 0.0.0.0 --port 8080

.PHONY: ci
GET_DASHBOARD=$(SCRIPTS)/pre-ci.sh
ci: $(REBAR) $(PROFILE)

.PHONY: quickrun
quickrun:
	@$(QUIKRUN)

.PHONY: check
check: xref
	@echo "=== XRef check passed ==="

## ============================================================================
## ������Կ�ܣ�ͨ��test_framework.sh��
## ============================================================================

# �г����в��
.PHONY: list-plugins
list-plugins:
	@./scripts/test_framework.sh --list-plugins

# �г�ָ������Ĳ�������
.PHONY: list-testcases
list-testcases:
	@if [ -z "$(PLUGIN)" ]; then \
echo "�÷�: make list-testcases PLUGIN=<�����>"; \
echo "ʾ��: make list-testcases PLUGIN=dgiot_modbus"; \
exit 1; \
fi
	@./scripts/test_framework.sh --list $(PLUGIN)

# ִ�в����������
.PHONY: test-plugin
test-plugin:
	@if [ -z "$(PLUGIN)" ] || [ -z "$(TESTCASE)" ]; then \
echo "�÷�: make test-plugin PLUGIN=<�����> TESTCASE=<����������>"; \
echo "ʾ��: make test-plugin PLUGIN=dgiot_modbus TESTCASE=simple"; \
echo ""; \
echo "ʹ�� make list-plugins �鿴���ò��"; \
echo "ʹ�� make list-testcases PLUGIN=<�����> �鿴����Ĳ�������"; \
exit 1; \
fi
	@./scripts/test_framework.sh --run $(PLUGIN) $(TESTCASE)

# ִ�в�����в�������
.PHONY: test-plugin-all
test-plugin-all:
	@if [ -z "$(PLUGIN)" ]; then \
echo "�÷�: make test-plugin-all PLUGIN=<�����>"; \
echo "ʾ��: make test-plugin-all PLUGIN=dgiot_modbus"; \
exit 1; \
fi
	@./scripts/test_framework.sh --all $(PLUGIN)

# ���ٲ����������
.PHONY: test-modbus
test-modbus:
	@./scripts/test_framework.sh modbus

.PHONY: test-modbus-simple
test-modbus-simple:
	@./scripts/test_framework.sh modbus simple

.PHONY: test-modbus-register
test-modbus-register:
	@./scripts/test_framework.sh modbus register

.PHONY: test-modbus-simulator
test-modbus-simulator:
	@./scripts/test_framework.sh modbus simulator

# ������Ϣ
.PHONY: test-help
test-help:
	@./scripts/test_framework.sh --help

## ============================================================================
## ������Կ�����ͨ�÷�����
## ============================================================================

# �г����в���������״̬
.PHONY: plugin-debug-list
plugin-debug-list:
	@./scripts/plugin_debug_framework.sh list

# һ���������в��Ϊ����ģʽ
.PHONY: plugin-debug-all
plugin-debug-all:
	@./scripts/plugin_debug_framework.sh all debug

# һ���ָ����в��Ϊ����ģʽ
.PHONY: plugin-production-all
plugin-production-all:
	@./scripts/plugin_debug_framework.sh all production

# �鿴���в����־����
.PHONY: plugin-levels-all
plugin-levels-all:
	@./scripts/plugin_debug_framework.sh all levels

# ����ָ�����Ϊ����ģʽ
.PHONY: plugin-debug
plugin-debug:
	@if [ -z "$(PLUGIN)" ]; then \
echo "�÷�: make plugin-debug PLUGIN=<�����>"; \
echo "ʾ��: make plugin-debug PLUGIN=dgiot_drone"; \
echo ""; \
echo "ʹ�� make plugin-debug-list �鿴���ò��"; \
exit 1; \
fi
	@./scripts/plugin_debug_framework.sh $(PLUGIN) debug

# �ָ�ָ�����Ϊ����ģʽ
.PHONY: plugin-production
plugin-production:
	@if [ -z "$(PLUGIN)" ]; then \
echo "�÷�: make plugin-production PLUGIN=<�����>"; \
echo "ʾ��: make plugin-production PLUGIN=dgiot_drone"; \
echo ""; \
echo "ʹ�� make plugin-debug-list �鿴���ò��"; \
exit 1; \
fi
	@./scripts/plugin_debug_framework.sh $(PLUGIN) production

# �鿴ָ�������־����
.PHONY: plugin-levels
plugin-levels:
	@if [ -z "$(PLUGIN)" ]; then \
echo "�÷�: make plugin-levels PLUGIN=<�����>"; \
echo "ʾ��: make plugin-levels PLUGIN=dgiot_drone"; \
echo ""; \
echo "ʹ�� make plugin-debug-list �鿴���ò��"; \
exit 1; \
fi
	@./scripts/plugin_debug_framework.sh $(PLUGIN) levels

# ���ָ�������־
.PHONY: plugin-monitor
plugin-monitor:
	@if [ -z "$(PLUGIN)" ]; then \
echo "�÷�: make plugin-monitor PLUGIN=<�����>"; \
echo "ʾ��: make plugin-monitor PLUGIN=dgiot_drone"; \
echo ""; \
echo "ʹ�� make plugin-debug-list �鿴���ò��"; \
exit 1; \
fi
	@./scripts/plugin_debug_framework.sh $(PLUGIN) monitor

# ������Կ�ܰ���
.PHONY: plugin-debug-help
plugin-debug-help:
	@./scripts/plugin_debug_framework.sh help

# ����������Խű�
.PHONY: create-plugin-debug
create-plugin-debug:
	@if [ -z "$(PLUGIN)" ]; then \
echo "�÷�: make create-plugin-debug PLUGIN=<�����>"; \
echo "ʾ��: make create-plugin-debug PLUGIN=dgiot_modbus"; \
echo ""; \
echo "ʹ�� make plugin-debug-list �鿴���ò��"; \
exit 1; \
fi
	@./scripts/create_plugin_debug.sh $(PLUGIN)

# ���е����������
.PHONY: debug-help
debug-help:
	@echo "=== DG-IoT����������� ==="
	@echo ""
	@echo "������Կ������:"
	@echo "  make list-plugins          - �г����в��"
	@echo "  make list-testcases        - �г������������"
	@echo "  make test-plugin           - ִ�в����������"
	@echo "  make test-plugin-all       - ִ�в�����в�������"
	@echo "  make test-modbus           - ���ٲ���Modbus���"
	@echo "  make test-modbus-simple    - ����Modbus������"
	@echo "  make test-modbus-register  - ����Modbusע������"
	@echo "  make test-modbus-simulator - ����Modbusģ����"
	@echo "  make test-help             - ���Կ�ܰ���"
	@echo ""
	@echo "������Կ�����ͨ�÷�����:"
	@echo "  make plugin-debug-list     - �г����в���������״̬"
	@echo "  make plugin-debug-all      - һ���������в��Ϊ����ģʽ"
	@echo "  make plugin-production-all - һ���ָ����в��Ϊ����ģʽ"
	@echo "  make plugin-levels-all     - �鿴���в����־����"
	@echo "  make plugin-debug          - ����ָ�����Ϊ����ģʽ"
	@echo "  make plugin-production     - �ָ�ָ�����Ϊ����ģʽ"
	@echo "  make plugin-levels         - �鿴ָ�������־����"
	@echo "  make plugin-monitor        - ���ָ�������־"
	@echo "  make create-plugin-debug   - ����������Խű�"
	@echo "  make plugin-debug-help     - ������Կ�ܰ���"
	@echo ""
	@echo "ʹ��ʾ��:"
	@echo "  # �鿴���в��״̬"
	@echo "  make plugin-debug-list"
	@echo ""
	@echo "  # ����Modbus������Խű�"
	@echo "  make create-plugin-debug PLUGIN=dgiot_modbus"
	@echo ""
	@echo "  # �������˻����"
	@echo "  make plugin-debug PLUGIN=dgiot_drone"
	@echo ""
	@echo "  # �鿴Modbus�����־����"
	@echo "  make plugin-levels PLUGIN=dgiot_modbus"
	@echo ""
	@echo "  # �鿴���п�������"
	@echo "  make debug-help"
