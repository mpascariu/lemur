# Running lemur from Docker

**Date:** 2026-09-03 · **Image:** `lemur_shiny` (see the [build guide](docker_building_guide.md))

This guide covers the two ways to run the lemur Shiny app from a Docker
container: **local mode** (no database, data bundled in the image) and
**server mode** (compose stack with PostgreSQL). Image construction is
covered in the [build guide](docker_building_guide.md).

---

## 0. Prerequisites

- Docker Desktop (Windows/macOS) or Docker Engine + compose plugin (Linux).
  Verify with:

  ``` bash
  docker --version
  docker compose version
  ```

- **The app image must exist.** Either pull it from GHCR (the default compose
  path):

  ``` bash
  docker pull ghcr.io/mpascariu/lemur-shiny:latest
  ```

  or build it locally once per machine as described in the
  [build guide](docker_building_guide.md) (tags the same image locally as
  `lemur_shiny`):

  ``` bash
  docker build -t lemur_shiny .
  ```

  Both work identically for every command below -- the commands are shown
  with the GHCR ref; if you built locally, substitute `lemur_shiny`.

- For **server mode** you also need the database credentials file:

  ``` bash
  cp .env.example .env
  # then edit .env: the POSTGRES_* block creates the role/database, the
  # LEMUR_DB_* block is what the app and API read. The two blocks must agree
  # or authentication fails.
  ```

---

## 1. Local mode (no database)

The image ships the complete GBD 2021+2023 datasets
(`inst/extdata/{cod,lt,sdg}_dt.rds`). With `serverMode = FALSE` (the default)
the app filters these in memory — PostgreSQL is never touched.

``` bash
docker run -d --name lemur -p 3838:3838 ghcr.io/mpascariu/lemur-shiny:latest \
  R -e "options(shiny.port = 3838, shiny.host = '0.0.0.0'); lemur::run_app(lb = FALSE)"

# locally-built equivalent:
docker run -d --name lemur -p 3838:3838 lemur_shiny \
  R -e "options(shiny.port = 3838, shiny.host = '0.0.0.0'); lemur::run_app(lb = FALSE)"
```

Then open <http://localhost:3838/>.

- `lb = FALSE` stops R from trying to open a browser inside the container;
  on a desktop Docker install remove it to auto-open your local browser.
- Stop and remove: `docker rm -f lemur`.

**Windows note:** the quoting of the `R -e` expression is fragile through
PowerShell. If it misbehaves, write the two lines to a file and mount it
(the same `lemur_shiny` / GHCR-ref substitution applies):

``` bash
# run_local.R:  options(shiny.port = 3838, shiny.host = '0.0.0.0')
#               lemur::run_app(lb = FALSE)
docker run -d --name lemur -p 3838:3838 \
  -v /path/to/run_local.R:/scripts/run_local.R:ro \
  lemur_shiny Rscript /scripts/run_local.R
```

---

## 2. Server mode (compose stack with PostgreSQL)

Server mode reads the same tables from PostgreSQL instead of the bundled
`.rds`. The compose file defines the whole stack:

| Service | Image | Port | Purpose |
|---|---|---|---|
| `db-loader` | `ghcr.io/mpascariu/lemur-shiny:latest` (profile `init`) | — | one-shot: fills the tables from the `.rds` in the app image |
| `shiny` | `ghcr.io/mpascariu/lemur-shiny:latest` | 3838 | the app, `run_app(serverMode = TRUE)` |
| `api` | built from `deploy/api/` | 5000 | Flask REST API |
| `nginx` | `nginx` | 80 | reverse proxy (expects the shinyproxy layout) |
| `shinyproxy` | `openanalytics/shinyproxy:2.6.0` | 8080 | app launcher (production path) |

### 2.1 Start the database and load the data

``` bash
docker compose up -d postgres        # creates role/db + empty tables on first boot
docker compose run --rm db-loader    # fills cod/sdg/lt from the bundled .rds
```

The loader is idempotent — it drops and rewrites the three tables, so rerun it
whenever a new package release ships new data. Progress prints per table
(cod ≈ 2.57 M rows, sdg ≈ 2.95 M, lt ≈ 142 K; takes ~1 min).

### 2.2 Start the app

``` bash
docker compose up -d shiny
```

Open <http://localhost:3838/>. The compose service already passes
`run_app(lb = FALSE, serverMode = TRUE)` and injects `LEMUR_DB_*` from `.env`;
`depends_on` + `restart: on-failure` make it wait for postgres and retry
while the loader runs.

### 2.3 Full stack

``` bash
docker compose up -d           # postgres + shiny + api (nginx/shinyproxy included)
```

`nginx` binds port 80 and proxies `/` to ShinyProxy (8080) and `/api/v1` to
the Flask API — that is the production layout of life-expectancy.org. When
running without shinyproxy, point it at the shiny container instead.

### 2.4 Updating data or code

``` bash
git pull
docker compose --profile build build shiny   # rebuild the app image (also the loader)
docker compose run --rm db-loader
docker compose up -d shiny api
```

### 2.5 Stopping the app when no longer needed

The app answers on <http://localhost:3838/> until its container is stopped —
it does not shut down on its own.

``` bash
docker compose stop shiny      # stop the app, keep containers + data
docker compose start shiny     # bring it back later (seconds, DB intact)
```

`stop` exits the container with code 137 (SIGKILL after the graceful
timeout — normal for Shiny) but keeps it, so a later `start` needs no
rebuild and no reload.

| Goal | Command |
|---|---|
| Stop app only, keep the stack | `docker compose stop shiny` |
| Stop everything (app, API, postgres) | `docker compose down` — removes containers, keeps the `db-data` volume, database survives |
| Also wipe the database | `docker compose down` then `docker volume rm db-data` (next `up` recreates empty tables; re-run the loader) |
| Plain `docker run` local-mode container (§1) | `docker rm -f lemur` |

`docker compose ps` shows which services are still up at any time.

---

## 3. Verifying the deployment

``` bash
# app (both modes)
curl -s -o /dev/null -w "%{http_code}\n" http://localhost:3838/    # -> 200

# database contents (server mode)
docker exec postgres psql -U lemur -d gbd_lemur_db \
  -c "SELECT DISTINCT period FROM cod ORDER BY period"
#   must list 1990 1995 2000 2005 2010 2015 2019 2020 2021 2023

# API (server mode; new GBD periods are valid)
curl "http://localhost:5000/cause_of_death?region=['Angola']&year=2023&sex=male&age=0"
curl "http://localhost:5000/life_table?region=['Angola']&year=2020&sex=both&age=0"
curl "http://localhost:5000/regions"
```

All three API calls return `200` with a JSON body (`status`, `message`,
`timestamp`, `data`).

**Data integrity check** (server mode) — the life expectancy stored in
PostgreSQL must match the bundled `.rds` bit for bit:

``` bash
docker run --rm --network lemur_net -e LEMUR_DB_HOST=postgres \
  -e LEMUR_DB_NAME=gbd_lemur_db -e LEMUR_DB_USER=lemur \
  -e LEMUR_DB_PASSWORD=change-me lemur_shiny \
  Rscript -e 'cn <- DBI::dbConnect(RPostgres::Postgres(), host = Sys.getenv("LEMUR_DB_HOST"),
    dbname = Sys.getenv("LEMUR_DB_NAME"), user = Sys.getenv("LEMUR_DB_USER"),
    password = Sys.getenv("LEMUR_DB_PASSWORD"));
    print(DBI::dbGetQuery(cn, "SELECT ex FROM lt WHERE region='\''Angola'\'' AND sex='\''male'\'' AND period=1990 AND x=0"))'
# -> 46.31469
```

---

## 4. The API service

`deploy/api/` builds a small Flask container exposing the same data over REST
(nginx path `/api/v1`, direct port 5000). Endpoints: `/cause_of_death`,
`/life_table`, `/sdg`, `/regions`, `/requests`. Accepted years:
1990, 1995, 2000, 2005, 2010, 2015, 2019, 2020, 2021, 2023; ages
0, 1, 2, 5, 10 … 95; sexes `male`, `female`, `both`. Interactive docs:
<http://localhost:5000/> (human-readable reference page).

The API reads `LEMUR_DB_*` from the environment (docker-compose fills them
from `.env`); it refuses to start without `LEMUR_DB_PASSWORD`.

---

## 5. Troubleshooting (runtime)

| Symptom | Cause / fix |
|---|---|
| App starts then exits with `no database password is configured` | `.env` missing or `LEMUR_DB_PASSWORD` empty (server mode only). Recreate from `.env.example`. |
| `shiny` container restarts in a loop | Postgres not up or loader not run yet — check `docker compose ps`, run `docker compose run --rm db-loader`. |
| App boots but tables/plots error with `column "x_int" does not exist` | The database was loaded with old tooling that named columns `x.int/Lx/Tx`. Re-run `docker compose run --rm db-loader` (it rewrites the tables with the DDL names). |
| `postgres` container exits immediately complaining about `18+` data layout | The data volume was created by `postgres:latest` (18+). Pin the image to `postgres:17` (compose does) and remove the old volume. |
| Port conflicts | 3838 (app), 5000 (API), 5432 (postgres), 8080 (shinyproxy), 80 (nginx) are bound on the host; change the left side of the `-p`/compose `ports` mapping if occupied. |

Build-time failures (`cmake not found`, missing `libuv1-dev`, ...) are
covered in the [build guide](docker_building_guide.md).

---

## 6. How the pieces fit

```
docker compose
├── postgres      <- data at rest: cod/sdg/lt/api_requests  (postgres:17)
│     ↑ COPY                  ↑ SQL (pool)
├── db-loader     one-shot: .rds --DBI::dbWriteTable--> postgres  [profile: init]
├── shiny         ghcr.io/mpascariu/lemur-shiny, run_app(serverMode=T) :3838
├── api           Flask + psycopg3, reads the same tables        :5000
├── nginx         :80 -> shinyproxy :8080, /api/v1 -> api :5000
└── shinyproxy    :8080, spawns per-session app containers (container-env map)
```

Local mode uses only the `lemur_shiny` image (data is inside it); server mode
adds postgres + loader. The loader replaces the older CSV export + `COPY`
pipeline — no host R and no CSV files are needed.