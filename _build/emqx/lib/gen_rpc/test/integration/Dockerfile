# Dockerfile to test gen_rpc via proper distributed nodes
FROM ubuntu:xenial
MAINTAINER Panagiotis PJ Papadomitsos <pj@ezgr.net>
WORKDIR "/gen_rpc"
ENV HOME=/root
ENV TERM=xterm-256color
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get -qqy update && \
apt-get -qqy install wget && \
wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
dpkg -i erlang-solutions_1.0_all.deb && \
apt-get -qqy update && \
apt-get -qqy install make gcc git esl-erlang
ENTRYPOINT ["/bin/bash"]
