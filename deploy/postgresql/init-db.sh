#!/bin/bash

psql -U $POSTGRES_USER -d $POSTGRES_DB -c \
"CREATE TABLE cod (
  x SMALLINT,
  region VARCHAR,
  sex VARCHAR(6),
  period SMALLINT,
  cause_name VARCHAR,
  deaths FLOAT(12)
);
COPY cod FROM '/data/data_gbd2019_cod.csv' DELIMITER ',' CSV HEADER;"

psql -U $POSTGRES_USER -d $POSTGRES_DB -c \
"CREATE TABLE sdg (
  x SMALLINT,
  region VARCHAR,
  sex VARCHAR(6),
  period SMALLINT,
  cause_name VARCHAR,
  deaths FLOAT(12)
);
COPY sdg FROM '/data/data_gbd2019_sdg.csv' DELIMITER ',' CSV HEADER;"

psql -U $POSTGRES_USER -d $POSTGRES_DB -c \
"CREATE TABLE lt (
  region VARCHAR,
  period SMALLINT,
  sex VARCHAR(6),
  x_int VARCHAR(10),
  x SMALLINT,
  mx FLOAT(15),
  qx FLOAT(15),
  ax FLOAT(15),
  lx FLOAT(15),
  dx FLOAT(15),
  llx FLOAT(15),
  ttx FLOAT(15),
  ex FLOAT(15)
);
COPY lt FROM '/data/data_gbd2019_lt.csv' DELIMITER ',' CSV HEADER;"
