export

ifneq ($(shell uname -s),Linux)
	$(shell echo "Please execute this script under Linux")
	exit
endif
# The version-release used for package
ifneq ($(shell echo $(EMQX_DEPS_DEFAULT_VSN) | grep -oE "v[0-9]+\.[0-9]+(\.[0-9]+)?"),)
	PKG_VSN := $(EMQX_DEPS_DEFAULT_VSN:v%=%)
else
	PKG_VSN := 4.0
endif

# Default name
EMQX_BUILD ?= emqx-pkg
ifneq ($(shell echo $(EMQX_BUILD) |grep edge),)
	EMQX_NAME := emqx-edge
else
	EMQX_NAME := emqx
endif

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
all: clean
	$(if $(PKGERDIR),,$(error "Operating system '$(OS)' not supported"))
	cd $(PKGERDIR) && $(MAKE)

clean:
	rm -rf package
	make -C rpm clean
	make -C deb clean

.PHONY: deb
deb:
	make -C deb

.PHONY: rpm
rpm:
	make -C rpm

