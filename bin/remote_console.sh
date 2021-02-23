#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo $DIR
cmd=`python $DIR/remote_console_cmd.py $DIR`
echo $cmd
$cmd