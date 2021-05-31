ARG BUILD_FROM=emqx/build-env:erl23.2.7.2-emqx-2-ubuntu20.04
FROM ${BUILD_FROM}

ARG EMQX_NAME=dgiot

COPY . /dgiot

WORKDIR /dgiot

RUN make ${EMQX_NAME}-zip || cat rebar3.crashdump

RUN make ${EMQX_NAME}-pkg || cat rebar3.crashdump

RUN /emqx/.ci/build_packages/tests.sh
