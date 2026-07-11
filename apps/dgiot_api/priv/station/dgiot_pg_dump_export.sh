#!/bin/bash

rm -rf /home/station/postgres
mkdir -p /home/station/postgres

pg_dump -h localhost -U postgres parse> /home/station/postgres/parse_5.0.sql

cd /home/station/postgres/
tar -zcvf parse_5.0.tar.gz parse_5.0.sql
