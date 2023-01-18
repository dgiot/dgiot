#!/bin/bash

filename=$1
cd /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/www/upload
tar -zxvf ${filename}

cd station/postgres/
tar -zxvf parse_5.0.sql.tar.gz

sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -c "SELECT pg_terminate_backend(pg_stat_activity.pid) FROM pg_stat_activity WHERE datname='parse' AND pid<>pg_backend_pid();"
sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -c "Drop database parse;"
sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -c "CREATE DATABASE parse;"
sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -f ./parse_5.0.sql  parse
