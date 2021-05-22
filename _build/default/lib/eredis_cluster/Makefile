ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
REBAR ?= rebar3
PATH := ./redis-git/src:${PATH}

# CLUSTER REDIS NODES
define NODE1_CONF
daemonize yes
port 30001
cluster-node-timeout 5000
pidfile /tmp/redis_cluster_node1.pid
logfile /tmp/redis_cluster_node1.log
save ""
appendonly no
cluster-enabled yes
cluster-config-file /tmp/redis_cluster_node1.conf
endef

define NODE2_CONF
daemonize yes
port 30002
cluster-node-timeout 5000
pidfile /tmp/redis_cluster_node2.pid
logfile /tmp/redis_cluster_node2.log
save ""
appendonly no
cluster-enabled yes
cluster-config-file /tmp/redis_cluster_node2.conf
endef

define NODE3_CONF
daemonize yes
port 30003
cluster-node-timeout 5000
pidfile /tmp/redis_cluster_node3.pid
logfile /tmp/redis_cluster_node3.log
save ""
appendonly no
cluster-enabled yes
cluster-config-file /tmp/redis_cluster_node3.conf
endef

define NODE4_CONF
daemonize yes
port 30004
cluster-node-timeout 5000
pidfile /tmp/redis_cluster_node4.pid
logfile /tmp/redis_cluster_node4.log
save ""
appendonly no
cluster-enabled yes
cluster-config-file /tmp/redis_cluster_node4.conf
endef

define NODE5_CONF
daemonize yes
port 30005
cluster-node-timeout 5000
pidfile /tmp/redis_cluster_node5.pid
logfile /tmp/redis_cluster_node5.log
save ""
appendonly no
cluster-enabled yes
cluster-config-file /tmp/redis_cluster_node5.conf
endef

define NODE6_CONF
daemonize yes
port 30006
cluster-node-timeout 5000
pidfile /tmp/redis_cluster_node6.pid
logfile /tmp/redis_cluster_node6.log
save ""
appendonly no
cluster-enabled yes
cluster-config-file /tmp/redis_cluster_node6.conf
endef

export NODE1_CONF
export NODE2_CONF
export NODE3_CONF
export NODE4_CONF
export NODE5_CONF
export NODE6_CONF

all: clean compile xref

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref skip_deps=true

clean:
	@ $(REBAR) clean

eunit:
	@rm -rf .eunit
	@mkdir -p .eunit
	@ERL_FLAGS="-config test.config" $(REBAR) eunit

test: eunit

edoc:
	@$(REBAR) skip_deps=true doc

help:
	@echo "Please use 'make <target>' where <target> is one of"
	@echo "  start             starts a test redis cluster"
	@echo "  cleanup           cleanup config files after redis cluster"
	@echo "  stop              stops all redis servers"
	@echo "  travis-run        starts the redis cluster and runs your tests"
	@echo "  travis-install    install redis from 'unstable' branch"

start: cleanup
	echo "$$NODE1_CONF" | redis-server -
	echo "$$NODE2_CONF" | redis-server -
	echo "$$NODE3_CONF" | redis-server -
	echo "$$NODE4_CONF" | redis-server -
	echo "$$NODE5_CONF" | redis-server -
	echo "$$NODE6_CONF" | redis-server -

cleanup:
	- rm -vf /tmp/redis_cluster_node*.conf 2>/dev/null
	- rm -f /tmp/redis_cluster_node1.conf
	- rm dump.rdb appendonly.aof - 2>/dev/null

stop:
	kill `cat /tmp/redis_cluster_node1.pid` || true
	kill `cat /tmp/redis_cluster_node2.pid` || true
	kill `cat /tmp/redis_cluster_node3.pid` || true
	kill `cat /tmp/redis_cluster_node4.pid` || true
	kill `cat /tmp/redis_cluster_node5.pid` || true
	kill `cat /tmp/redis_cluster_node6.pid` || true
	make cleanup

travis-run:
	# Start all cluster nodes
	make start
	sleep 5

	# Join all nodes in the cluster
	echo "yes" | ruby redis-git/src/redis-trib.rb create --replicas 1 127.0.0.1:30001 127.0.0.1:30002 127.0.0.1:30003 127.0.0.1:30004 127.0.0.1:30005 127.0.0.1:30006
	sleep 5

	make compile && make test

	# Kill all redis nodes and do cleanup
	make stop

travis-install:
	[ ! -e redis-git ] && git clone -b 3.0.6 --single-branch https://github.com/antirez/redis.git redis-git || true
	make -C redis-git -j4
	gem install redis
	sleep 3
