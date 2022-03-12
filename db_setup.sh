#!/bin/bash

initdb -D /taxapp_db
pg_ctl -D taxapp_db -l logfile -o start

# Create a database
createdb taxapp
sudo -u postgres psql -c "CREATE ROLE test WITH LOGIN PASSWORD 'test' CREATEDB;"
sudo -u postgres psql -c "CREATE DATABASE taxapp;"
