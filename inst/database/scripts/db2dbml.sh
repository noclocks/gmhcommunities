#!/usr/bin/env bash

# This script is used to convert a database connection to DBML.

npx -p @dbml/cli db2dbml postgres 'postgresql://postgres:f+40e+i=PGg?D4@3@34.75.86.90:5432/gmh_leasing_dev?schemas=public,app,auth,entrata,gmh,logs,mkt' -o gmh.database.dbml
