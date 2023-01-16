#!/bin/bash

cd /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/www/upload/station/files
tar -zxvf dgiot_file.tar.gz

\cp -rf dgiot_file/ /data/dgiot/go_fastdfs/files/
