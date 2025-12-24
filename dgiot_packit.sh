#!/bin/bash
# This file is used to install dgiot on linux systems. The operating system
# is required to use systemd to manage services at boot
export PATH=$PATH:/usr/local/bin

DATE=`date +%Y-%m-%d`

echo ${DATE}

cd _build/emqx/rel/emqx
tar -zcvf dgiot-${DATE}.tar.gz lib

PRODUCT_PATH=/data/dgiot/go_fastdfs/files/package

cp dgiot-${DATE}.tar.gz ${PRODUCT_PATH}/

