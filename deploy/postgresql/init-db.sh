#!/bin/bash
# Table creation only. Data is loaded by the one-shot db-loader compose service
# (deploy/postgresql/load_data.sh), which writes directly from the .rds
# datasets bundled in the app image -- no CSV files involved.
# NOTE: this script only runs automatically on FIRST postgres initialization
# (empty data directory). For an existing volume run it manually:
#   docker compose exec postgres bash /docker-entrypoint-initdb.d/init-db.sh
# (tables are created IF NOT EXISTS-free: drop them first if reloading)
# The db-loader (deploy/postgresql/load_data.sh) also creates api_requests
# with CREATE TABLE IF NOT EXISTS, so re-used volumes missing it are covered
# when the loader runs -- keep the two definitions identical.

# Fail before the first statement, not after.
#
# This script is run once, on the first boot of an empty data directory. If it
# aborts partway, the image still treats PGDATA as initialised and will not run
# it again -- so a configuration error caught after the CREATE TABLEs would
# leave a half-built database that fixing .env does not repair. Every check
# therefore happens up front, and set -e stops the script at the first failing
# command (the entrypoint sources this file under its own set -e when the file
# is not executable; this makes that independent of the file mode).
set -e

: "${LEMUR_DB_OWNER:?LEMUR_DB_OWNER must be set (see .env.example)}"
: "${LEMUR_DB_OWNER_PASSWORD:?LEMUR_DB_OWNER_PASSWORD must be set (see .env.example)}"
: "${LEMUR_DB_USER:?LEMUR_DB_USER must be set (see .env.example)}"
: "${LEMUR_DB_PASSWORD:?LEMUR_DB_PASSWORD must be set (see .env.example)}"

for pair in "POSTGRES_USER:LEMUR_DB_OWNER" "POSTGRES_USER:LEMUR_DB_USER" "LEMUR_DB_OWNER:LEMUR_DB_USER"; do
  a="${pair%%:*}"; b="${pair##*:}"
  if [ "${!a}" = "${!b}" ]; then
    echo "init-db.sh: $a and $b must name different roles (both are '${!a}')." >&2
    echo "  The superuser, the schema owner and the runtime role are three" >&2
    echo "  distinct roles by design. See .env.example." >&2
    exit 1
  fi
done

# Distinct names are not enough. If the runtime role shares a password with
# the owner or the superuser, a compromised app container can authenticate as
# that role over the internal network -- role names are readable from
# pg_roles -- and regain the DDL rights this split exists to remove.
for pair in "POSTGRES_PASSWORD:LEMUR_DB_OWNER_PASSWORD" \
            "POSTGRES_PASSWORD:LEMUR_DB_PASSWORD" \
            "LEMUR_DB_OWNER_PASSWORD:LEMUR_DB_PASSWORD"; do
  a="${pair%%:*}"; b="${pair##*:}"
  if [ "${!a}" = "${!b}" ]; then
    echo "init-db.sh: $a and $b must differ." >&2
    echo "  Sharing a password lets a container holding the weaker role" >&2
    echo "  authenticate as the stronger one. See .env.example." >&2
    exit 1
  fi
done

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
# ---------------------------------------------------------------------------
# Roles.
#
# POSTGRES_USER is the cluster's bootstrap superuser -- the postgres image
# creates it "with superuser power". A superuser session is equivalent to
# shell access inside this container: COPY ... FROM PROGRAM, lo_export,
# pg_read_file and untrusted procedural languages all execute as the postgres
# OS user. Nothing outside this container may hold that password or reach it
# over the network, so two lesser roles are created here:
#
#   LEMUR_DB_OWNER  NOSUPERUSER, owns the schema and tables. Used only by the
#                   one-shot db-loader, which issues DDL. Cannot COPY FROM
#                   PROGRAM (that needs superuser or pg_execute_server_program).
#   LEMUR_DB_USER   the runtime role for the Shiny app and the API:
#                     * SELECT on cod/sdg/lt   (R/app_data_filters.R runs
#                       dbGetQuery only)
#                     * SELECT/INSERT/UPDATE on api_requests + its sequence
#                       (deploy/api/api/utils.py rate limiter)
#                   No DDL, no DELETE, no TRUNCATE.
#
# The variables these use are validated at the top of this script.
# ---------------------------------------------------------------------------
# psql's :"x" interpolates as a quoted identifier and :'x' as a quoted
# literal, so passwords and role names never enter a shell-built SQL string.
psql -v ON_ERROR_STOP=1 -U "$POSTGRES_USER" -d "$POSTGRES_DB" \
  --set=owner="$LEMUR_DB_OWNER" \
  --set=owner_pass="$LEMUR_DB_OWNER_PASSWORD" \
  --set=app_user="$LEMUR_DB_USER" \
  --set=app_pass="$LEMUR_DB_PASSWORD" \
  --set=db="$POSTGRES_DB" <<'SQL'
SELECT format('CREATE ROLE %I LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE PASSWORD %L',
              :'owner', :'owner_pass')
WHERE NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = :'owner')
\gexec

SELECT format('CREATE ROLE %I LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE PASSWORD %L',
              :'app_user', :'app_pass')
WHERE NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = :'app_user')
\gexec

-- The owner needs the schema and the tables created above, so the loader can
-- DROP and recreate them without any superuser involvement.
ALTER SCHEMA public OWNER TO :"owner";
ALTER TABLE api_requests OWNER TO :"owner";
ALTER TABLE cod          OWNER TO :"owner";
ALTER TABLE sdg          OWNER TO :"owner";
ALTER TABLE lt           OWNER TO :"owner";
ALTER SEQUENCE api_requests_id_seq OWNER TO :"owner";

GRANT CONNECT ON DATABASE :"db" TO :"owner", :"app_user";
GRANT USAGE ON SCHEMA public TO :"app_user";

GRANT SELECT ON cod, sdg, lt TO :"app_user";
GRANT SELECT, INSERT, UPDATE ON api_requests TO :"app_user";
GRANT USAGE, SELECT ON SEQUENCE api_requests_id_seq TO :"app_user";

-- load_data.sh drops and recreates cod/sdg/lt on every run, which discards
-- the grants above along with the old tables.
ALTER DEFAULT PRIVILEGES FOR ROLE :"owner" IN SCHEMA public
  GRANT SELECT ON TABLES TO :"app_user";
SQL

# ---------------------------------------------------------------------------
# Keep the superuser off the network.
#
# The roles above are only worth having if POSTGRES_PASSWORD cannot simply be
# replayed from another container. Reject superuser logins over TCP; local
# socket connections still work, so `docker compose exec postgres psql` for
# administration is unaffected.
# ---------------------------------------------------------------------------
hba="${PGDATA:-/var/lib/postgresql/data}/pg_hba.conf"
marker="# lemur: superuser is not reachable over TCP"

if ! grep -qF "$marker" "$hba"; then
  tmp="$(mktemp)"
  {
    echo "$marker"
    echo "# A superuser session can execute shell commands inside this"
    echo "# container (COPY ... FROM PROGRAM and friends), so it must not be"
    echo "# reachable from the app, API or loader containers. Administration"
    echo "# goes through the local socket instead."
    echo "host all ${POSTGRES_USER} all reject"
    echo
    cat "$hba"
  } > "$tmp"
  cat "$tmp" > "$hba"
  rm -f "$tmp"
  echo "pg_hba.conf: rejecting TCP logins for superuser ${POSTGRES_USER}"
fi

echo "roles ready: ${LEMUR_DB_OWNER} (owner), ${LEMUR_DB_USER} (runtime)"
