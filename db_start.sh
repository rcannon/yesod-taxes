#!/bin/bash

# set up postgres
initdb -D taxapp_db
pg_ctl -D taxapp_db -l taxapp_db/logfile -o "--unix_socket_directories='$(pwd)'" start

# set up database
createdb -h $(pwd) taxapp
psql -d taxapp -h $(pwd) -c "CREATE ROLE test WITH LOGIN PASSWORD 'test' CREATEDB;"

