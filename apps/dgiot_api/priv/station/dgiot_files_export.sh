#!/bin/bash

path=$1
name=$2
#rm -rf /home/station/${path}
mkdir -p /home/station/${path}
path1="/data/dgiot/go_fastdfs/files/${path}${name}"

cp -rf ${path1} /home/station/files/${path}
