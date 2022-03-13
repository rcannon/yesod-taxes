#!/bin/bash

# set up postgres
initdb -D taxapp_db
pg_ctl -D taxapp_db -l taxapp_db/logfile -o "--unix_socket_directories='$(pwd)'" start

# create a database
createdb -h $(pwd) taxapp
#sudo -u postgres psql -h $(pwd) -c "CREATE ROLE test WITH LOGIN PASSWORD 'test' CREATEDB;"
#sudo -u postgres psql -h $(pwd) -c "CREATE DATABASE taxapp;"
psql -d taxapp -h $(pwd) -c "CREATE ROLE test WITH LOGIN PASSWORD 'test' CREATEDB;"
#psql -d taxapp -h $(pwd) -c "CREATE DATABASE taxapp;"
