#!/bin/sh
set -e

emqx_exit(){
    # tail erlang.log.*
    erlang_log=$(echo $(ls -t /opt/emqx/log/erlang.log.*) | awk '{print $1}')
    num=$(sed -n -e '/LOGGING STARTED/=' ${erlang_log} | tail -1)
    tail -n +$((num-2)) ${erlang_log}

    echo "['$(date -u +"%Y-%m-%dT%H:%M:%SZ")']:emqx exit abnormally"
    exit 1
}

## EMQ Main script

# Start and run emqx, and when emqx crashed, this container will stop

/opt/emqx/bin/emqx start || emqx_exit

tail -f /opt/emqx/log/emqx.log.1 &

# Wait and ensure emqx status is running
WAIT_TIME=0
while [[ -z "$(/opt/emqx/bin/emqx_ctl status |grep 'is running'|awk '{print $1}')" ]]
do
    sleep 1
    echo "['$(date -u +"%Y-%m-%dT%H:%M:%SZ")']:waiting emqx"
    WAIT_TIME=$((WAIT_TIME+1))
    if [[ $WAIT_TIME -gt $EMQX_WAIT_TIME ]]; then
        echo "['$(date -u +"%Y-%m-%dT%H:%M:%SZ")']:timeout error"
        exit 1
    fi
done

# Sleep 5 seconds to wait for the loaded plugins catch up.
sleep 5

echo "['$(date -u +"%Y-%m-%dT%H:%M:%SZ")']:emqx start"

# monitor emqx is running, or the docker must stop to let docker PaaS know
# warning: never use infinite loops such as `` while true; do sleep 1000; done`` here
#          you must let user know emqx crashed and stop this container,
#          and docker dispatching system can known and restart this container.
NEW_LOG_NUM=2
IDLE_TIME=0
while [[ $IDLE_TIME -lt 5 ]]
do  
    IDLE_TIME=$((IDLE_TIME+1))
    if [[ ! -z "$(/opt/emqx/bin/emqx_ctl status |grep 'is running'|awk '{print $1}')" ]]; then
        IDLE_TIME=0
        # Print the latest emqx.log
        if [[ -f /opt/emqx/log/emqx.log.${NEW_LOG_NUM} ]];then
            tail -f /opt/emqx/log/emqx.log.${NEW_LOG_NUM} &
            [[ ! -z $(ps -ef |grep "tail -f /opt/emqx/log/emqx.log" | grep -vE "grep|emqx.log.${NEW_LOG_NUM}" | awk '{print $1}') ]] && kill $(ps -ef |grep "tail -f /opt/emqx/log/emqx.log" | grep -vE "grep|emqx.log.${NEW_LOG_NUM}" | awk '{print $1}') 
            NEW_LOG_NUM=$((NEW_LOG_NUM+1)) 
        fi
    else
        echo "['$(date -u +"%Y-%m-%dT%H:%M:%SZ")']:emqx not running, waiting for recovery in $((25-IDLE_TIME*5)) seconds"
    fi
    sleep 5
done
# If running to here (the result 5 times not is running, thus in 25s emqx is not running), exit docker image
# Then the high level PaaS, e.g. docker swarm mode, will know and alert, rebanlance this service

emqx_exit
