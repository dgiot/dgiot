#!/bin/bash

acl=$1
tables=(_Role Category Channel Device Dict Evidence Files Instruct Maintenance Menu Notification Permission Product ProductTemplet View)
echo ${#tables[@]}
rm -rf /home/postgres/$1
mkdir /home/postgres/$1
chown -R postgres:postgres /home/postgres/$1

for i in ${tables[@]}; do
  echo $i
  psql -U postgres -d parse -c "COPY \"$i\"  FROM '/home/postgres/$1/$i.csv' WITH HEADER CSV where _rperm && array['*','role:$1'];"

done

relations=(_Join:children:Product _Join:deletedBy:Notification _Join:menus:_Role _Join:product:Channel _Join:products:_Role _Join:readBy:Notification _Join:role:_User _Join:roles:_Role _Join:roles:_User _Join:rules:_Role _Join:users:_Role _Join:users:_Session _Join:views:_Role)
mkdir /home/postgres/$1/relations
chown -R postgres:postgres /home/postgres/$1/relations
for i in ${relations[@]}; do
  echo $i
  psql -U postgres -d parse -c "COPY \"$i\"  FROM '/home/postgres/$1/relations/$i.csv' WITH HEADER CSV;"

done
