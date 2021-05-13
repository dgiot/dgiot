export

ifneq ($(shell uname -s),Linux)
	$(shell echo "Please execute this script under Linux")
	exit
endif

MKFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
SOURCE_PATH := $(shell dirname $(MKFILE))/..
REL_PATH := $(SOURCE_PATH)/_build/emqtt/rel
PACKAGES_PATH := $(SOURCE_PATH)/_packages

# The version-release used for package
PKG_VSN := $(shell git describe --tags --always)

ifneq ($(shell cat /etc/*-release |grep -o -i centos),)
	ID := centos
	VERSION_ID := $(shell rpm --eval '%{centos_ver}')
else
	ID := $(shell sed -n '/^ID=/p' /etc/os-release | sed -r 's/ID=(.*)/\1/g' | sed 's/"//g' )
	VERSION_ID := $(shell sed -n '/^VERSION_ID=/p' /etc/os-release | sed -r 's/VERSION_ID=(.*)/\1/g' | sed 's/"//g')
endif
SYSTEM := $(shell echo $(ID)$(VERSION_ID) | sed -r "s/([a-zA-Z]*)-.*/\1/g")
##
## Support RPM and Debian based linux systems
##
ifeq ($(ID),ubuntu)
	PKGERDIR := deb
else ifeq ($(ID),debian)
	PKGERDIR := deb
else ifeq ($(ID),raspbian)
	PKGERDIR := deb
else
	PKGERDIR := rpm
endif

.PHONY: all
all: zip
	$(if $(PKGERDIR),,$(error "Operating system '$(OS)' not supported"))
	$(MAKE) -C $(PKGERDIR)

.PHONY: zip
zip:
	$(MAKE) -C ../ emqtt
	mkdir -p $(PACKAGES_PATH)
	cd $(REL_PATH) && zip -rq emqtt-$(SYSTEM)-$(PKG_VSN).zip emqtt
	mv $(REL_PATH)/emqtt-$(SYSTEM)-$(PKG_VSN).zip $(PACKAGES_PATH)

.PHONY: deb
deb:
	make -C deb

.PHONY: rpm
rpm:
	make -C rpm

