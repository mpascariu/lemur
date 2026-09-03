#!/bin/bash
# Table creation only. Data is loaded by the one-shot db-loader compose service
# (deploy/postgresql/load_data.sh), which writes directly from the .rds
# datasets bundled in the app image -- no CSV files involved.
# NOTE: this script only runs automatically on FIRST postgres initialization
# (empty data directory). For an existing volume run it manually:
#   docker compose exec postgres bash /docker-entrypoint-initdb.d/init-db.sh
# (tables are created IF NOT EXISTS-free: drop them first if reloading)

psql -U $POSTGRES_USER -d $POSTGRES_DB -c \
"CREATE TABLE api_requests (
	id SERIAL PRIMARY KEY,
  ip INET NOT NULL,
	date DATE NOT NULL DEFAULT CURRENT_DATE,
	requests INT NOT NULL DEFAULT 1,
	UNIQUE(date, ip)
);
"

psql -U $POSTGRES_USER -d $POSTGRES_DB -c \
"CREATE TABLE cod (
  x SMALLINT,
  region VARCHAR,
  sex VARCHAR(6),
  period SMALLINT,
  cause_name VARCHAR,
  deaths DOUBLE PRECISION
);
"

psql -U $POSTGRES_USER -d $POSTGRES_DB -c \
"CREATE TABLE sdg (
  x SMALLINT,
  region VARCHAR,
  sex VARCHAR(6),
  period SMALLINT,
  cause_name VARCHAR,
  deaths DOUBLE PRECISION
);
"

psql -U $POSTGRES_USER -d $POSTGRES_DB -c \
"CREATE TABLE lt (
  region VARCHAR,
  period SMALLINT,
  sex VARCHAR(6),
  x_int VARCHAR(16),
  x SMALLINT,
  mx DOUBLE PRECISION,
  qx DOUBLE PRECISION,
  ax DOUBLE PRECISION,
  lx DOUBLE PRECISION,
  dx DOUBLE PRECISION,
  llx DOUBLE PRECISION,
  ttx DOUBLE PRECISION,
  ex DOUBLE PRECISION
);
"