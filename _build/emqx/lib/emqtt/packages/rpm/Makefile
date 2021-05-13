# Keep this short to avoid bloating beam files with long file path info
TOPDIR := $(shell rpm --eval "%{_topdir}")
RPM_VSN ?= $(shell echo $(PKG_VSN) | grep -oE "[0-9]+\.[0-9]+(\.[0-9]+)?")
RPM_REL ?= $(shell echo $(PKG_VSN) | grep -oE "(alpha|beta|rc)\.[0-9]")

ARCH := $(shell arch)
TARGET_PKG := emqtt-$(SYSTEM)-$(PKG_VSN).$(ARCH)
ifeq ($(RPM_REL),)
	# no tail
	RPM_REL := 1
endif
SOURCE_PKG := emqtt-$(SYSTEM)-v$(RPM_VSN)-$(RPM_REL).$(ARCH)

.PHONY: all
all: prepare
	rpmbuild -v -bb \
		--define "_package_name emqtt" \
		--define "_name emqtt" \
		--define "_builddir $(TOPDIR)/$(PKG_VSN)/BUILD" \
		--define "_version $(RPM_VSN)" \
		--define "_release $(RPM_REL)" \
		--define "_ostype -$(SYSTEM)" \
		emqtt.spec
	mkdir -p $(PACKAGES_PATH)
	cp $(TOPDIR)/RPMS/$(ARCH)/$(SOURCE_PKG).rpm $(PACKAGES_PATH)/$(TARGET_PKG).rpm

prepare:
	rm -rf $(TOPDIR)
	mkdir -p $(TOPDIR)/$(PKG_VSN)/BUILD
	cp -r $(REL_PATH)/emqtt $(TOPDIR)/$(PKG_VSN)/BUILD
