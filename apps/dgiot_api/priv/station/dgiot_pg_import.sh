#!/bin/bash

filename=$1
cd /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/www/upload
tar -zxvf ${filename}
tables=(_Role _User Category Channel Device Dict Evidence Files Instruct Maintenance Menu Notification Permission Product ProductTemplet View)
chown -R postgres:postgres /data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/www/upload/station/postgres/

for i in ${tables[@]}; do
  sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "TRUNCATE \"$i\";"
  sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "COPY \"$i\"  FROM '/data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/www/upload/station/postgres/$i.csv' WITH HEADER CSV;"
done

relations=(_Join:children:Product _Join:deletedBy:Notification _Join:menus:_Role _Join:product:Channel _Join:products:_Role _Join:readBy:Notification _Join:role:_User _Join:roles:_Role _Join:roles:_User _Join:rules:_Role _Join:users:_Role _Join:users:_Session _Join:views:_Role)
for i in ${relations[@]}; do
  sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "TRUNCATE \"$i\";"
  sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "COPY \"$i\"  FROM '/data/dgiot/dgiot/lib/dgiot_api-4.3.0/priv/www/upload/station/postgres/relations/$i.csv' WITH HEADER CSV;"
done
