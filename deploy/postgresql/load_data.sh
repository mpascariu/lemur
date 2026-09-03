#!/usr/bin/env bash
# One-shot database loader: fills the postgres cod/sdg/lt tables directly from
# the .rds datasets bundled in the lemur package -- the app image already
# ships DBI/RPostgres, so no host R and no CSV files are needed.
# Run with: docker compose run --rm db-loader
set -euo pipefail

host="${LEMUR_DB_HOST:-postgres}"
port="${LEMUR_DB_PORT:-5432}"

echo "Waiting for postgres at ${host}:${port} ..."
for i in $(seq 1 60); do
  if Rscript -e "cn <- DBI::dbConnect(RPostgres::Postgres(), host='${host}', port=${port}, dbname=Sys.getenv('LEMUR_DB_NAME'), user=Sys.getenv('LEMUR_DB_USER'), password=Sys.getenv('LEMUR_DB_PASSWORD')); DBI::dbDisconnect(cn)" 2>/dev/null; then
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
  user   = Sys.getenv("LEMUR_DB_USER"),
  password = Sys.getenv("LEMUR_DB_PASSWORD")
)
on.exit(DBI::dbDisconnect(cn), add = TRUE)

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
  n <- as.numeric(DBI::dbGetQuery(cn, sprintf("SELECT count(*) AS n FROM %s", name))$n)
  cat(sprintf("%-4s loaded: %d rows\n", name, n))
}

load_table("cod", lemur::data_gbd_cod())
load_table("sdg", lemur::data_gbd_sdg())
load_table("lt",  lemur::data_gbd_lt(), lt_name_map)
cat("done\n")
RSCRIPT