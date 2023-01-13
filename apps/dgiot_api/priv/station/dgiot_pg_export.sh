#!/bin/bash

acl=$1
tables=(_Role _User Category Channel Device Dict Evidence Files Instruct Maintenance Menu Notification Permission Product ProductTemplet View)
echo ${#tables[@]}
rm -rf /home/station/postgres
mkdir -p /home/station/postgres
chown -R postgres:postgres /home/station/postgres

for i in ${tables[@]}; do
  echo $i
  sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "COPY \"$i\" TO '/home/station/postgres/$i.csv' WITH  CSV HEADER;"

done

relations=(_Join:children:Product _Join:deletedBy:Notification _Join:menus:_Role _Join:product:Channel _Join:products:_Role _Join:readBy:Notification _Join:role:_User _Join:roles:_Role _Join:roles:_User _Join:rules:_Role _Join:users:_Role _Join:users:_Session _Join:views:_Role)
mkdir -p /home/station/postgres/relations
chown -R postgres:postgres /home/station/postgres/relations
for i in ${relations[@]}; do
  echo $i
  sudo -u postgres /usr/local/pgsql/12/bin/psql -U postgres -d parse -c "COPY \"$i\" TO '/home/station/postgres/relations/$i.csv' WITH  CSV HEADER;"
done
