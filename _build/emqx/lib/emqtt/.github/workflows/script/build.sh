#!/bin/sh
set -ex

export PACKAGE_PATH="_packages"

build_pkg(){
    if [ $(uname) = Linux ]; then
        make pkg
    elif [ $(uname) = Darwin ]; then
        pkg=emqtt-macos-$(git describe --tags --always).zip
        make emqtt
        mkdir -p $PACKAGE_PATH
        
        cd _build/emqtt/rel && zip -rq $pkg emqtt && cd -
        mv _build/emqtt/rel/$pkg $PACKAGE_PATH
    fi
}

test_pkg(){
    for var in $(ls $PACKAGE_PATH/emqtt-*);do
        case ${var##*.} in
            "zip")
            unzip -q $var
            ./emqtt/bin/emqtt pub -t 'hello' --payload 'hello world' -h broker.emqx.io
            rm -rf emqtt
            ;;
            "deb")
            dpkg -i $var
            if [ $(dpkg -l |grep emqtt |awk '{print $1}') != 'ii' ];then
                echo 'package install error' && exit 1
            fi
            emqtt pub -t 'hello' --payload 'hello world' -h broker.emqx.io
            dpkg -r emqtt
            if [ $(dpkg -l |grep emqtt |awk '{print $1}') != 'rc' ];then
                echo 'package remove error' && exit 1
            fi
            dpkg -P emqtt
            if [ ! -z $(dpkg -l |grep emqtt) ];then
                echo 'package uninstall error' && exit 1
            fi
            ;;
            "rpm")
            rpm -ivh $var
            emqtt pub -t 'hello' --payload 'hello world' -h broker.emqx.io
            rpm -e emqtt
            ;;
        esac
    done
}

build_pkg
test_pkg