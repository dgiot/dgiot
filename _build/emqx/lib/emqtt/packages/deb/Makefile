# Keep this short to avoid bloating beam files with long file path info
TOPDIR := /tmp/emqx
SRCDIR := $(TOPDIR)/$(PKG_VSN)
DEB_VSN := $(PKG_VSN:v%=%)

ARCH := $(shell dpkg --print-architecture)
SOURCE_PKG := emqtt_$(DEB_VSN)_$(ARCH)
TARGET_PKG := emqtt-$(SYSTEM)-$(PKG_VSN)_$(ARCH)

.PHONY: all
all: prepare
	cp -r debian $(SRCDIR)/
	sed -i "s#<DATETIME>#$(shell date -u '+%a, %d %b %Y %T %z')#g" $(SRCDIR)/debian/changelog
	sed -i "s#<VERSION>#$(DEB_VSN)#g" $(SRCDIR)/debian/changelog
	cd $(SRCDIR) && dpkg-buildpackage -us -uc
	mkdir -p $(PACKAGES_PATH)
	cp $(SRCDIR)/../$(SOURCE_PKG).deb $(PACKAGES_PATH)/$(TARGET_PKG).deb

prepare:
	rm -rf $(SRCDIR)
	mkdir -p $(TOPDIR) $(SRCDIR)
	cp -r $(REL_PATH)/emqtt $(SRCDIR) 
