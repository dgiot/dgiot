#!/bin/bash

path=$1
name=$2
rm -rf /home/${path}
mkdir -p /home/${path}
echo ${path}
echo ${name}
#psql -P format=latex -U postgres -d parse -c "SELECT path FROM \"Files\" where _rperm && array['*','role:admin'];"
path1="/data/dgiot/go_fastdfs/files/${path}${name}"

cp -rf ${path1} /home/${path}
echo ${path1}


