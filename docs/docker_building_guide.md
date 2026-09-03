# Building the lemur Docker images

**Date:** 2026-09-03 · **Repo:** `Dockerfile` (app), `deploy/api/Dockerfile` (API)

How to build the images that the [run guide](docker_running_guide.md) uses.
Do this once per machine (or whenever code/data/dependencies change); running
the app never rebuilds anything.

## 1. Prerequisites

- Docker Desktop (Windows/macOS) or Docker Engine + compose plugin (Linux):

  ``` bash
  docker --version
  docker compose version
  ```

- ~4 GB free disk per build cache generation; the final images use ~5 GB.

## 2. App image — `lemur_shiny`

The main image: R 4.3.2 + all package dependencies + the lemur package with
the bundled GBD 2021+2023 datasets. Built from the repository root:

``` bash
docker build -t lemur_shiny .
```

- **Duration:** ~10-30 min. The base image `rocker/rstudio:4.3.2` is pulled
  once; every R package is compiled from source (gdal/sf are the long pole).
- **Rebuilds are incremental.** Docker caches each layer, so after a data or
  code change only the steps from that layer onward rerun. The cache is
  invalidated automatically when `inst/extdata/*.rds` or `R/` sources change
  (they enter the image via `ADD . /build_zone`).
- **After a full cache wipe** (`docker builder prune`), expect the long build
  again.

Verify the result:

``` bash
docker run --rm lemur_shiny Rscript -e 'cat(as.character(packageVersion("lemur")), nrow(lemur::data_gbd_lt()), "\n")'
# -> 2.0.1 142560
```

### System dependencies baked into the image

The `apt-get` line in the `Dockerfile` carries three additions that are easy
to lose and painful to rediscover — keep them if you edit the Dockerfile:

| Package | Why |
|---|---|
| `libuv1-dev` | `fs` → `bslib` → `shiny` build chain fails without it |
| `cmake` | `s2` (dependency of `sf`) builds its vendored abseil with it |
| `libpq-dev`, `libgdal-dev`, `libgeos-dev`, `libproj-dev`, `libudunits2-dev` | sf/gdal/units system libraries |

The R package pins in the Dockerfile mirror the minimum versions declared in
`DESCRIPTION` — update both together when imports change.

## 3. API image — `lemur-api`

Small Flask container (Python) serving the REST API. Built automatically on
first `docker compose up -d api`, or manually:

``` bash
docker compose build api          # uses deploy/api/Dockerfile + requirements.txt
```

No build-time configuration; the database credentials are supplied at
runtime through `LEMUR_DB_*` environment variables (see the run guide).

## 4. Database and proxy images

These are pulled, not built:

| Image | Role | Note |
|---|---|---|
| `postgres:17` | data store | pinned on purpose — `postgres:18+` changed the data-directory layout and refuses the compose volume |
| `nginx:latest` | reverse proxy | pulls on `docker compose up` |
| `openanalytics/shinyproxy:2.6.0` | app launcher | config comes from `deploy/shinyproxy/application.yml`, bind-mounted at runtime — no custom build |

## 5. Build context

`.dockerignore` keeps the build context small (the repo's `data-raw/`,
`.git`, `docs/`, `deploy/` etc. are excluded — the image needs only package
sources and `inst/`). If the build seems to upload gigabytes, check that
`.dockerignore` is still intact in the repository root.

## 6. Rebuilding after changes

``` bash
git pull                         # or edit sources
docker compose build shiny       # app image + the db-loader (same image)
docker compose build api         # only if deploy/api changed
```

Then follow the run guide's "Updating data or code" section to reload the
database and restart the services.