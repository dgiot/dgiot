#!/bin/bash -l
# Copyright 2015 Panagiotis Papadomitsos. All Rights Reserved.
#
# Used to run automated integration tests using Docker
#

NUM_OF_NODES=${1:-3}
NODES=""

start_node() {
	export NAME=gen_rpc_${1}
	echo -n "Starting container ${NAME}: "
	docker run -tid --privileged --name ${NAME} -P gen_rpc:integration
	sleep 2
	export IP=$(docker inspect --format '{{ .NetworkSettings.IPAddress }}' ${NAME})
	export NODES="${IP}:${NODES}"
	docker exec -d ${NAME} epmd -daemon
	docker exec -ti ${NAME} bash -c 'mkdir -p /root/.cache'
	docker exec -ti ${NAME} bash -c 'echo gen_rpc > ~/.erlang.cookie'
	docker exec -ti ${NAME} bash -c 'chmod 600 ~/.erlang.cookie'
	docker exec -ti ${NAME} bash -c 'rm -fr /gen_rpc/*'
	docker cp ../../ ${NAME}:/
	docker cp ~/.cache/rebar3 ${NAME}:/root/.cache/rebar3
	docker exec -ti ${NAME} bash -c 'cd /gen_rpc && make'
	docker exec -ti -d ${NAME} bash -c "cd /gen_rpc && ./rebar3 as dev do shell --name gen_rpc@${IP}"
	return $?
}

start_master() {
	export NAME=gen_rpc_master
	echo -n "Starting container ${NAME}: "
	docker run -i -t -d --privileged --name ${NAME} -P gen_rpc:integration
	sleep 2
	export IP=$(docker inspect --format '{{ .NetworkSettings.IPAddress }}' gen_rpc_master)
	docker exec -d ${NAME} epmd -daemon
	docker exec -ti ${NAME} bash -c 'mkdir -p /root/.cache'
	docker exec -ti ${NAME} bash -c "echo gen_rpc > ~/.erlang.cookie"
	docker exec -ti ${NAME} bash -c "chmod 600 ~/.erlang.cookie"
	docker exec -ti ${NAME} bash -c "rm -fr /gen_rpc/*"
	docker cp ../../ ${NAME}:/
	docker cp ~/.cache/rebar3 ${NAME}:/root/.cache/rebar3
	docker exec -ti ${NAME} bash -c "cd /gen_rpc && make"
	echo Starting integration tests on container ${NAME}
	docker exec -ti gen_rpc_master bash -c "export NODES=${NODES} NODE=gen_rpc@${IP} && cd /gen_rpc && make && ./rebar3 as test do ct --suite test/ct/integration_SUITE"
}

destroy() {
	# Destroy slaves
	for NODE in $(seq 1 ${NUM_OF_NODES}); do
		export NAME=gen_rpc_${NODE}
		echo -n "Destroying container: "
		docker rm -f ${NAME} 2> /dev/null
	done
	# Destroy master
	echo -n "Destroying container: "
	docker rm -f gen_rpc_master 2> /dev/null
}

run() {
	echo Running integration tests with ${NUM_OF_NODES} nodes
	for NODE in $(seq 1 ${NUM_OF_NODES}); do
		start_node $NODE
		if [[ $? -ne 0 ]]; then
			destroy
		fi;
	done
	export NODES="${NODES%?}"
	start_master
	export RESULT=$?
	destroy
	exit ${RESULT}
}

run
