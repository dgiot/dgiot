## default globals
TARGET ?= emqx/emqx
QEMU_ARCH ?= x86_64
ARCH ?= amd64
QEMU_VERSION ?= v4.0.0
OS ?= alpine

## versioning
EMQX_DEPS_DEFAULT_VSN ?= develop
BUILD_VERSION ?= $(EMQX_DEPS_DEFAULT_VSN:v%=%)

EMQX_NAME = $(subst emqx/,,$(TARGET))
ARCH_LIST = amd64 arm64v8 arm32v7 i386 s390x

.PHONY: all
all: build tag

.PHONY: prepare
prepare:
	## Prepare the machine before any code installation scripts
	# @echo "PREPARE: Setting up dependencies."
	# @apt update -y
	# @apt install --only-upgrade docker-ce -y
	
	## Update docker configuration to enable docker manifest command
	@echo "PREPARE: Updating docker configuration"
	@mkdir -p $$HOME/.docker

	# enable experimental to use docker manifest command
	@echo '{ "experimental": "enabled" }' | tee $$HOME/.docker/config.json
	# enable experimental
	@echo '{ "experimental": true, "storage-driver": "overlay2", "max-concurrent-downloads": 50, "max-concurrent-uploads": 50 }' | tee /etc/docker/daemon.json 
	@service docker restart

.PHONY: build
build:
	## Build Docker image
	@echo "DOCKER BUILD: Build Docker image."
	@echo "DOCKER BUILD: build version -> $(BUILD_VERSION)."
	@echo "DOCKER BUILD: build from -> $(BUILD_FROM)."
	@echo "DOCKER BUILD: arch - $(ARCH)."
	@echo "DOCKER BUILD: qemu arch - $(QEMU_ARCH)."
	@echo "DOCKER BUILD: docker repo - $(TARGET) "
	@echo "DOCKER BUILD: emqx name - $(EMQX_NAME)."
	@echo "DOCKER BUILD: emqx version - $(EMQX_DEPS_DEFAULT_VSN)."

	## Prepare qemu to build images other then x86_64 on travis
	@echo "PREPARE: Qemu" \
	&& docker run --rm --privileged multiarch/qemu-user-static:register --reset
  
	@mkdir -p tmp \
	&& cd tmp \
	&& curl -L -o qemu-$(QEMU_ARCH)-static.tar.gz https://github.com/multiarch/qemu-user-static/releases/download/$(QEMU_VERSION)/qemu-$(QEMU_ARCH)-static.tar.gz \
	&& tar xzf qemu-$(QEMU_ARCH)-static.tar.gz \
	&& cd -

	@cd ../.. \
	&& docker build --no-cache \
		--build-arg EMQX_DEPS_DEFAULT_VSN=$(EMQX_DEPS_DEFAULT_VSN) \
		--build-arg BUILD_FROM=emqx/build-env:erl22.3-alpine-$(ARCH)  \
		--build-arg RUN_FROM=$(ARCH)/alpine:3.11 \
		--build-arg EMQX_NAME=$(EMQX_NAME) \
		--build-arg QEMU_ARCH=$(QEMU_ARCH) \
		--tag $(TARGET):build-$(OS)-$(ARCH) \
		-f deploy/docker/Dockerfile . \
	&& cd -

.PHONY: tag
tag:
	@echo "DOCKER TAG: Tag Docker image."
	@for arch in $(ARCH_LIST); do \
		if [ -n  "$$(docker images -q $(TARGET):build-$(OS)-$${arch})" ]; then \
			docker tag $(TARGET):build-$(OS)-$${arch}  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch}; \
			echo "DOCKER TAG: $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch}"; \
			if [ $${arch} = amd64 ]; then \
				docker tag $(TARGET):$(BUILD_VERSION)-$(OS)-amd64 $(TARGET):$(BUILD_VERSION); \
				echo "DOCKER TAG: $(TARGET):$(BUILD_VERSION)"; \
			fi; \
		fi; \
	done

.PHONY: save
save:
	@echo "DOCKER SAVE: Save Docker image." 

	@mkdir -p ../../_packages/$(EMQX_NAME)

	@if [ -n  "$$(docker images -q $(TARGET):$(BUILD_VERSION))" ]; then \
		docker save $(TARGET):$(BUILD_VERSION) > $(EMQX_NAME)-docker-$(BUILD_VERSION); \
		zip -r -m $(EMQX_NAME)-docker-$(BUILD_VERSION).zip $(EMQX_NAME)-docker-$(BUILD_VERSION); \
		mkdir -p ../../_packages/$(EMQX_NAME); \
		mv ./$(EMQX_NAME)-docker-$(BUILD_VERSION).zip ../../_packages/$(EMQX_NAME)/$(EMQX_NAME)-docker-$(BUILD_VERSION).zip; \
	fi
	
	@for arch in $(ARCH_LIST); do \
		if [ -n  "$$(docker images -q  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch})" ]; then \
			docker save  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch} > $(EMQX_NAME)-docker-$(BUILD_VERSION)-$(OS)-$${arch}; \
			zip -r -m $(EMQX_NAME)-docker-$(BUILD_VERSION)-$(OS)-$${arch}.zip $(EMQX_NAME)-docker-$(BUILD_VERSION)-$(OS)-$${arch}; \
			mkdir -p ../../_packages/$(EMQX_NAME); \
			mv ./$(EMQX_NAME)-docker-$(BUILD_VERSION)-$(OS)-$${arch}.zip ../../_packages/$(EMQX_NAME)/$(EMQX_NAME)-docker-$(BUILD_VERSION)-$(OS)-$${arch}.zip; \
		fi; \
	done

.PHONY: push
push:
	@echo "DOCKER PUSH: Push Docker image."; 
	@echo "DOCKER PUSH: pushing - $(TARGET):$(BUILD_VERSION)."; 

	@if [ -n "$$(docker images -q $(TARGET):$(BUILD_VERSION))" ]; then \
		docker push $(TARGET):$(BUILD_VERSION); \
		docker tag $(TARGET):$(BUILD_VERSION) $(TARGET):latest; \
		docker push $(TARGET):latest; \
	fi;

	@for arch in $(ARCH_LIST); do \
		if [ -n "$$(docker images -q  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch})" ]; then \
			docker push  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch}; \
		fi; \
	done

.PHONY: manifest_list
manifest_list:
	version="docker manifest create --amend $(TARGET):$(BUILD_VERSION)"; \
	latest="docker manifest create --amend $(TARGET):latest"; \
	for arch in $(ARCH_LIST); do \
		if [ -n "$$(docker images -q  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch})" ];then \
			version="$${version} $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch} "; \
			latest="$${latest} $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch} "; \
		fi; \
	done; \
	eval $$version; \
	eval $$latest; 

	for arch in $(ARCH_LIST); do \
		case $${arch} in \
			"amd64") \
				if [ -n "$$(docker images -q  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch})" ]; then \
					docker manifest annotate $(TARGET):$(BUILD_VERSION) $(TARGET):$(BUILD_VERSION)-$(OS)-amd64 --os=linux --arch=amd64; \
					docker manifest annotate $(TARGET):latest $(TARGET):$(BUILD_VERSION)-$(OS)-amd64 --os=linux --arch=amd64; \
				fi; \
				;; \
			"arm64v8") \
				if [ -n "$$(docker images -q  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch})" ]; then \
					docker manifest annotate $(TARGET):$(BUILD_VERSION) $(TARGET):$(BUILD_VERSION)-$(OS)-arm64v8 --os=linux --arch=arm64 --variant=v8; \
					docker manifest annotate $(TARGET):latest $(TARGET):$(BUILD_VERSION)-$(OS)-arm64v8 --os=linux --arch=arm64 --variant=v8; \
				fi; \
				;; \
			"arm32v7") \
				if [ -n "$$(docker images -q  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch})" ]; then \
					docker manifest annotate $(TARGET):$(BUILD_VERSION) $(TARGET):$(BUILD_VERSION)-$(OS)-arm32v7 --os=linux --arch=arm --variant=v7; \
					docker manifest annotate $(TARGET):latest $(TARGET):$(BUILD_VERSION)-$(OS)-arm32v7 --os=linux --arch=arm --variant=v7; \
				fi; \
				;; \
			"i386") \
				if [ -n "$$(docker images -q  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch})" ]; then \
					docker manifest annotate $(TARGET):$(BUILD_VERSION) $(TARGET):$(BUILD_VERSION)-$(OS)-i386 --os=linux --arch=386; \
					docker manifest annotate $(TARGET):latest $(TARGET):$(BUILD_VERSION)-$(OS)-i386 --os=linux --arch=386; \
				fi; \
				;; \
			"s390x") \
				if [ -n "$$(docker images -q  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch})" ]; then \
					docker manifest annotate $(TARGET):$(BUILD_VERSION) $(TARGET):$(BUILD_VERSION)-$(OS)-s390x --os=linux --arch=s390x; \
					docker manifest annotate $(TARGET):latest $(TARGET):$(BUILD_VERSION)-$(OS)-s390x --os=linux --arch=s390x; \
				fi; \
				;; \
		esac; \
	done; 

	docker manifest inspect $(TARGET):$(BUILD_VERSION)
	docker manifest push $(TARGET):$(BUILD_VERSION); 
	docker manifest inspect $(TARGET):latest
	docker manifest push $(TARGET):latest;

.PHONY: clean
clean:
	@echo "DOCKER CLEAN: Clean Docker image."

	@if [ -n "$$(docker images -q  $(TARGET):$(BUILD_VERSION))" ]; then docker rmi -f $$(docker images -q  $(TARGET):$(BUILD_VERSION)); fi

	@for arch in $(ARCH_LIST); do \
		if [ -n "$$(docker images -q  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch})" ]; then \
			docker rmi -f $$(docker images -q  $(TARGET):$(BUILD_VERSION)-$(OS)-$${arch}); \
		fi \
	done
