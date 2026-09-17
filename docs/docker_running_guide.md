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

- **The app image must exist.** The compose stack always uses the locally
  built image, tagged `lemur_shiny` (see the
  [build guide](docker_building_guide.md)); build it once per machine:

  ``` bash
  docker build -t lemur_shiny .
  ```

  A prebuilt image is also published on GHCR for the single-container
  local mode below (§1); the compose stack never pulls it:

  ``` bash
  docker pull ghcr.io/mpascariu/lemur-shiny:latest
  ```

- For **server mode** you also need the database credentials file:

  ``` bash
  cp .env.example .env
  # then edit .env. POSTGRES_* is the superuser that creates the database,
  # LEMUR_DB_OWNER is the schema owner the loader uses, and LEMUR_DB_USER is
  # what the app and API read at runtime. Three separate roles with three
  # separate passwords -- init-db.sh refuses to initialise if any two match.
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
`.rds`. The compose file defines the whole stack.

### 2.0 What is pulled, what is built, and what you configure

| Component | Source | What it contains |
|---|---|---|
| `lemur_shiny:latest` | **built locally** from the repo root (~35-40 min cold, incremental after) | R + Shiny app + the GBD datasets + the data loader |
| `postgres:17` | pulled from Docker Hub | empty database initialized on first boot from `deploy/postgresql/init-db.sh` |
| API (Flask) | **built locally** from `deploy/api/` (~40 s, no host prerequisites) | REST endpoints over the same tables |
| `nginx:latest`, `openanalytics/shinyproxy:3.2.4` | pulled | reverse proxy / app launcher (production topology) |

Credentials: nothing is hard-coded anywhere. Copy `.env.example` to `.env`
and fill it once -- every service reads the same file:

| Variable | Read by | Purpose |
|---|---|---|
| `POSTGRES_USER` / `POSTGRES_PASSWORD` / `POSTGRES_DB` | postgres container **only** | the superuser: creates the database and the two lesser roles on first boot (baked in at that moment; changing them later requires wiping the `db-data` volume). No other container receives this password |
| `LEMUR_DB_OWNER` / `LEMUR_DB_OWNER_PASSWORD` | postgres container + db-loader | owns the schema and tables; the loader connects as this to write `cod`/`sdg`/`lt` |
| `LEMUR_DB_HOST` | app + API + loader | postgres hostname (`postgres` inside compose; a managed-DB endpoint in the cloud) |
| `LEMUR_DB_NAME` / `LEMUR_DB_USER` / `LEMUR_DB_PASSWORD` / `LEMUR_DB_PORT` | app + API | the least-privilege connection the app's pool and the API use at runtime |

### Upgrading an existing deployment

This release changes `POSTGRES_USER`, adds two database roles, and changes the
API, so an existing deployment cannot be upgraded in place. Two things bite if
the order below is not followed:

- The **images must be rebuilt after `git pull`**. Every image in this stack is
  built locally (`pull_policy: never`), and `docker compose up` only builds an
  image that is missing -- an existing `lemur_shiny:latest` or
  `lemur-api:latest` is reused as-is. That matters for the API in particular,
  whose code is baked into its image: without the rebuild it keeps serving the
  previous, vulnerable version. The `db-loader` mounts its shell script from
  the working tree but still runs the R package baked into `lemur_shiny`, so
  it needs the rebuild too.
- The **backup must run while the old stack is still up and `.env` still holds
  the old values**, because it connects with `$POSTGRES_USER`.

```bash
# 1. back up the old database, while it is still running
docker compose exec postgres sh -c 'pg_dump -U "$POSTGRES_USER" -d "$POSTGRES_DB"' > backup.sql

# 2. fetch the new code, then rebuild BOTH locally-built images
git pull
docker compose --profile build build shiny    # app image + the db-loader (same image)
docker compose build api                      # the API has its own build context

# 3. update .env from .env.example: three roles, three DIFFERENT passwords
#    (init-db.sh refuses to start if any two role names or passwords match)

# 4. wipe the data volume -- the roles are baked in at first boot, so a re-used
#    volume keeps the old ones and the app cannot authenticate
docker compose down -v

# 5. rebuild the database and reload the data
docker compose up -d postgres
docker compose run --rm db-loader

# 6. bring the rest up -- plain `up` skips the `shiny` service, which sits
#    behind the build profile: it starts nginx + shinyproxy + postgres + api
docker compose up -d
#    ...and only if you ran the app as a single container (§2.2, no
#    ShinyProxy), start that one too:
# docker compose up -d shiny
```

Then confirm the deployment is healthy with the checks in §3 -- in particular
`\du` must list `lemur_owner` and `lemur_app`, and the API must answer.

Do not instead try `ALTER ROLE lemur NOSUPERUSER` on the old database: that
role is the cluster's bootstrap superuser and usually the only one, so
demoting it leaves no way to grant the privilege back short of single-user
mode.

Wiping the volume loses no scientific data: `cod`, `sdg` and `lt` are rebuilt
from the `.rds` files bundled in the app image. Only the request counters in
`api_requests` are discarded -- which is why the backup above is really only
worth taking if you want those counters.

For anything beyond a local test deployment, replace the `change-me`
placeholders in `.env` with real values -- three different passwords, one
per role.

### 2.1 Start the database and load the data

``` bash
docker compose up -d postgres        # creates role/db + empty tables on first boot
docker compose run --rm db-loader    # fills cod/sdg/lt, ensures api_requests
```

The loader is idempotent -- it drops and rewrites the three data tables, so
rerun it whenever a new package release ships new data. It also creates the
`api_requests` table the API depends on if it is missing (volumes initialized
before that table existed) and never drops it, so usage counts survive reruns.
Progress prints per table (cod ≈ 2.57 M rows, sdg ≈ 2.95 M, lt ≈ 142 K;
takes ~1 min).

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
docker compose up -d           # nginx + shinyproxy + postgres + api
```

This does **not** start the `shiny` service: it sits behind the `build`
profile, so plain `up` skips it. In this topology the app is not a
long-lived container at all -- ShinyProxy launches one per session on demand
(they appear in `docker ps` as `sp-container-...`). Use §2.2 only when you
want the single app container on its own, without ShinyProxy.

`nginx` binds port 80 and proxies `/` to ShinyProxy (8080) and `/api/v1` to
the Flask API — that is the production layout of life-expectancy.org. The
other published ports (8080, 3838, 5000) are loopback-only: reachable from
the host itself, unreachable from other machines; off-host traffic goes
through nginx. When running without shinyproxy, point it at the shiny
container instead.

### 2.4 Updating data or code

Rebuild after every `git pull`: the images are local (`pull_policy: never`) and
`docker compose up` reuses whatever is already tagged, so an un-rebuilt image
keeps running the previous code.

``` bash
git pull
docker compose --profile build build shiny   # rebuild the app image (also the loader)
docker compose build api                     # only if deploy/api changed
docker compose run --rm db-loader
docker compose up -d shiny api
```

Upgrading across a release that changes the database roles or `POSTGRES_USER`
needs the full sequence in "Upgrading an existing deployment" above, not this
one -- the volume must be wiped for the new roles to exist.

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
| Stop everything (app, API, postgres) | `docker compose down` — removes containers, keeps the database volume, database survives |
| Also wipe the database | `docker compose down -v` — removes the containers and the database volume. Next `up` recreates empty tables, so re-run the loader |
| See the database volume's full name | `docker volume ls` — Compose prefixes it with the project name, which defaults to the clone directory (`lemur_db-data` for a `lemur/` checkout) |
| Plain `docker run` local-mode container (§1) | `docker rm -f lemur` |

`docker compose ps` shows which services are still up at any time.

---

## 3. Verifying the deployment

``` bash
# app -- pick the line matching how you started it
curl -s -o /dev/null -w "%{http_code}\n" http://localhost/          # full stack, via nginx
curl -s -o /dev/null -w "%{http_code}\n" http://localhost:8080/app_direct/lemur/   # shinyproxy direct
curl -s -o /dev/null -w "%{http_code}\n" http://localhost:3838/     # only if you ran §2.2
```

Port 3838 answers `000` under the full stack: the `shiny` service is behind
the `build` profile and is not running. Check for `sp-container-...` entries
in `docker ps` instead -- ShinyProxy starts one per session.

The database examples below wrap psql in `sh -c` so that `$POSTGRES_USER`
is expanded inside the container. Those variables come from `.env`, which
compose reads -- they are not set in your own shell, and `-U ""` makes psql
fall back to the OS user and fail with `role "root" does not exist`.

Do not add `-h` either: the local socket trusts, while forcing TCP matches
the scram rule and psql then waits for a password with no visible prompt.

``` bash
# database contents
docker compose exec postgres sh -c 'psql -U "$POSTGRES_USER" -d "$POSTGRES_DB" \
  -c "SELECT DISTINCT period FROM cod ORDER BY period"'
#   must list 1990 1995 2000 2005 2010 2015 2019 2020 2021 2023

# roles: lemur_owner and lemur_app must both show no attributes
docker compose exec postgres sh -c 'psql -U "$POSTGRES_USER" -d "$POSTGRES_DB" -c "\du"'

# the superuser must be refused over TCP but work on the local socket
docker compose exec postgres grep -n "^host all .* reject" /var/lib/postgresql/data/pg_hba.conf

# the app and API containers must not hold the superuser password
docker compose exec api env | grep -c POSTGRES        # -> 0

# API (server mode; new GBD periods are valid).
# -g is required: curl treats [ and ] as glob metacharacters and refuses the
# URL without it -- with -s that failure is silent, printing nothing at all.
curl -sg "http://localhost:5000/cause_of_death?region=['Angola']&year=2023&sex=male&age=0"
curl -sg "http://localhost:5000/life_table?region=['Angola']&year=2020&sex=both&age=0"
curl -s  "http://localhost:5000/regions"
```

All three API calls return `200` with a JSON body (`status`, `message`,
`timestamp`, `data`).

**Data integrity check** (server mode) — the life expectancy stored in
PostgreSQL must match the bundled `.rds` bit for bit:

``` bash
# Only the runtime credentials: --env-file .env would also hand this
# container POSTGRES_PASSWORD, undoing the separation the stack sets up.
docker run --rm --network lemur_net -e LEMUR_DB_HOST=postgres \
  -e LEMUR_DB_NAME="$(grep -E '^LEMUR_DB_NAME=' .env | cut -d= -f2-)" \
  -e LEMUR_DB_USER="$(grep -E '^LEMUR_DB_USER=' .env | cut -d= -f2-)" \
  -e LEMUR_DB_PASSWORD="$(grep -E '^LEMUR_DB_PASSWORD=' .env | cut -d= -f2-)" \
  lemur_shiny \
  Rscript -e 'cn <- DBI::dbConnect(RPostgres::Postgres(), host = Sys.getenv("LEMUR_DB_HOST"),
    dbname = Sys.getenv("LEMUR_DB_NAME"), user = Sys.getenv("LEMUR_DB_USER"),
    password = Sys.getenv("LEMUR_DB_PASSWORD"));
    print(DBI::dbGetQuery(cn, "SELECT ex FROM lt WHERE region='\''Angola'\'' AND sex='\''male'\'' AND period=1990 AND x=0"))'
# -> 46.31469
```

---

## 4. The API service

`deploy/api/` (see the [build guide](docker_building_guide.md) for its
Dockerfile, pinned dependencies and build cost) builds a small Flask
container, served by gunicorn, exposing the same data over REST
(`/api/v1` via nginx; port 5000 is loopback-only, reachable from the host
itself). Endpoints: `/cause_of_death`,
`/life_table`, `/sdg`, `/regions`, `/requests`. Accepted years:
1990, 1995, 2000, 2005, 2010, 2015, 2019, 2020, 2021, 2023; ages
0, 1, 2, 5, 10 … 95; sexes `male`, `female`, `both`. Interactive docs:
<http://localhost:5000/> (human-readable reference page).

`/requests` reports request counts per day. It does not return the calling
addresses recorded in `api_requests`: the endpoint is public, so publishing
them would expose every visitor's IP.

The API reads `LEMUR_DB_*` from the environment (docker-compose fills them
from `.env`); it refuses to start without `LEMUR_DB_PASSWORD`. It connects
as `LEMUR_DB_USER`, which can read the data tables and update
`api_requests`, and nothing else.

---

## 5. Troubleshooting (runtime)

| Symptom | Cause / fix |
|---|---|
| App starts then exits with `no database password is configured` | `.env` missing or `LEMUR_DB_PASSWORD` empty (server mode only). Recreate from `.env.example`. |
| `shiny` container restarts in a loop | Postgres not up or loader not run yet — check `docker compose ps`, run `docker compose run --rm db-loader`. |
| App boots but tables/plots error with `column "x_int" does not exist` | The database was loaded with old tooling that named columns `x.int/Lx/Tx`. Re-run `docker compose run --rm db-loader` (it rewrites the tables with the DDL names). |
| API answers 500 with `relation "api_requests" does not exist` | The postgres data volume predates the table (`init-db.sh` runs only on the first boot of an empty volume). Run `docker compose run --rm db-loader` -- it creates the table and leaves existing data alone. |
| `postgres` container exits and the log ends with `init-db.sh: ... must differ` (or another `init-db.sh:` message) | The role configuration in `.env` was rejected before any table was created. Fix `.env`, then **wipe the volume**: `docker compose down -v && docker compose up -d postgres && docker compose run --rm db-loader`. Correcting `.env` and restarting is not enough -- the entrypoint reports `Skipping initialization` and leaves the database with no roles at all, so the app still cannot connect. |
| `postgres` container exits immediately complaining about `18+` data layout | The data volume was created by `postgres:latest` (18+). Pin the image to `postgres:17` (compose does) and remove the old volume. |
| Port conflicts | 80 (nginx) plus 8080, 3838 and 5000 bound on loopback; change the left side of the `-p`/compose `ports` mapping if occupied. Postgres publishes nothing to the host -- it is reachable only on the internal `net` network. |

Build-time failures (`cmake not found`, missing `libuv1-dev`, ...) are
covered in the [build guide](docker_building_guide.md).

---

## 6. How the pieces fit

```
docker compose
├── postgres      <- data at rest: cod/sdg/lt/api_requests  (postgres:17)
│     ↑ COPY                  ↑ SQL (pool)
├── db-loader     one-shot: .rds --DBI::dbWriteTable--> postgres  [profile: init]
├── shiny         lemur_shiny (built locally), run_app(serverMode=T) :3838
├── api           Flask + psycopg3, reads the same tables        :5000
├── nginx         :80 -> shinyproxy :8080, /api/v1 -> api :5000
└── shinyproxy    3.2.4 :8080, spawns per-session app containers (container-env map)
```

Local mode uses only the `lemur_shiny` image (data is inside it); server mode
adds postgres + loader. The loader replaces the older CSV export + `COPY`
pipeline — no host R and no CSV files are needed.