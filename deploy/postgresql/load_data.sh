#!/usr/bin/env bash
# One-shot database loader: fills the postgres cod/sdg/lt tables directly from
# the .rds datasets bundled in the lemur package -- the app image already
# ships DBI/RPostgres, so no host R and no CSV files are needed.
# Run with: docker compose run --rm db-loader
#
# Connects as LEMUR_DB_OWNER, not LEMUR_DB_USER and not the superuser: this
# script issues DDL (DROP / CREATE / write) and owns the resulting tables,
# while LEMUR_DB_USER is the read-mostly role the app and API run as. The
# owner is NOSUPERUSER, so a compromise of this container cannot reach
# COPY ... FROM PROGRAM. See deploy/postgresql/init-db.sh.
set -euo pipefail

host="${LEMUR_DB_HOST:-postgres}"
port="${LEMUR_DB_PORT:-5432}"

: "${LEMUR_DB_OWNER:?LEMUR_DB_OWNER must be set (see .env.example)}"
: "${LEMUR_DB_OWNER_PASSWORD:?LEMUR_DB_OWNER_PASSWORD must be set (see .env.example)}"
: "${LEMUR_DB_USER:?LEMUR_DB_USER must be set (see .env.example)}"

echo "Waiting for postgres at ${host}:${port} ..."
for i in $(seq 1 60); do
  if Rscript -e "cn <- DBI::dbConnect(RPostgres::Postgres(), host='${host}', port=${port}, dbname=Sys.getenv('LEMUR_DB_NAME'), user=Sys.getenv('LEMUR_DB_OWNER'), password=Sys.getenv('LEMUR_DB_OWNER_PASSWORD')); DBI::dbDisconnect(cn)" 2>/dev/null; then
    break
  fi
  if [ "$i" -eq 60 ]; then echo "postgres unreachable after 5 minutes" >&2; exit 1; fi
  sleep 5
done

Rscript --vanilla - <<'RSCRIPT'
host <- Sys.getenv("LEMUR_DB_HOST", "postgres")
port <- Sys.getenv("LEMUR_DB_PORT", "5432")
cn <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = host, port = as.integer(port),
  dbname = Sys.getenv("LEMUR_DB_NAME"),
  user   = Sys.getenv("LEMUR_DB_OWNER"),
  password = Sys.getenv("LEMUR_DB_OWNER_PASSWORD")
)
on.exit(DBI::dbDisconnect(cn), add = TRUE)

# The runtime role the app and API connect as. Tables recreated below are
# owned by LEMUR_DB_OWNER, so each has to be granted to it explicitly.
app_user <- DBI::dbQuoteIdentifier(cn, Sys.getenv("LEMUR_DB_USER"))
# The API rate limiter (deploy/api/api/utils.py) reads and writes the
# api_requests table. init-db.sh creates it, but that script only runs on the
# FIRST boot of an empty postgres data volume -- on a re-used volume it never
# executes and every API call fails until the table exists. Create it here so
# the documented loader step covers that case too. Never drop this table: it
# accumulates daily request counts.
DBI::dbExecute(cn, "
  CREATE TABLE IF NOT EXISTS api_requests (
    id SERIAL PRIMARY KEY,
    ip INET NOT NULL,
    date DATE NOT NULL DEFAULT CURRENT_DATE,
    requests INT NOT NULL DEFAULT 1,
    UNIQUE(date, ip)
  )
")
DBI::dbExecute(cn, paste(
  "GRANT SELECT, INSERT, UPDATE ON api_requests TO", app_user))
DBI::dbExecute(cn, paste(
  "GRANT USAGE, SELECT ON SEQUENCE api_requests_id_seq TO", app_user))
cat("api_requests table ensured\n")

# The DDL in init-db.sh names the life-table columns x_int/llx/ttx (valid
# unquoted identifiers) while the .rds uses x.int/Lx/Tx (dotted/case names).
# The app's server-mode path expects the DDL names and renames on read
# (app_server.R: rename(x.int = x_int, Lx = llx, Tx = ttx)), so the loader
# must map to the DDL names before writing -- never let dbWriteTable
# recreate the tables with .rds names.
lt_name_map <- c("x.int" = "x_int", "Lx" = "llx", "Tx" = "ttx")

load_table <- function(name, df, rename_map = character(0)) {
  if (length(rename_map)) names(df)[match(names(rename_map), names(df))] <- unname(rename_map)
  DBI::dbExecute(cn, sprintf("DROP TABLE IF EXISTS %s", name))
  DBI::dbWriteTable(cn, name, df, row.names = FALSE)
  # DROP discards the old table's grants along with the table.
  DBI::dbExecute(cn, paste("GRANT SELECT ON", name, "TO", app_user))
  n <- as.numeric(DBI::dbGetQuery(cn, sprintf("SELECT count(*) AS n FROM %s", name))$n)
  cat(sprintf("%-4s loaded: %d rows\n", name, n))
}

load_table("cod", lemur::data_gbd_cod())
load_table("sdg", lemur::data_gbd_sdg())
load_table("lt",  lemur::data_gbd_lt(), lt_name_map)
cat("done\n")
RSCRIPT