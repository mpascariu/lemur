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

## 2. Pull instead of build (GHCR)

The app image is published to the GitHub Container Registry. If you only
want to *run* lemur, skip the build entirely:

``` bash
docker pull ghcr.io/mpascariu/lemur-shiny:latest
```

| Tag | Meaning |
|---|---|
| `v2.0.3` | built from the v2.0.3 release; immutable -- fixes ship as new versions |
| `latest` | points at the most recently published version |

The image version always matches the package version in DESCRIPTION --
bumping `Version:` and tagging the release (`v2.0.3`) is what publishes.
Images are private by default; flip to public under GitHub -> Packages ->
package settings if you want them pullable without authentication.

`docker compose up -d shiny` resolves this automatically: compose pulls
`ghcr.io/mpascariu/lemur-shiny:latest` when no local `lemur_shiny` image
exists. Maintainers keep the local build path (below) via the compose
`build` profile.

## 3. App image — `lemur_shiny`

The main image: R 4.3.2 + all package dependencies + the lemur package with
the bundled GBD 2021+2023 datasets. Built from the repository root:

``` bash
docker build -t lemur_shiny .
```

- **Duration:** ~35-40 min cold; R packages and gdal/sf are compiled from
  source. The base image `rocker/rstudio:4.3.2` is pulled once.
- **Rebuilds are incremental.** Docker caches each layer, so after a data or
  code change only the steps from that layer onward rerun (~2 min for a
  code-only change). The cache is invalidated automatically when
  `inst/extdata/*.rds` or `R/` sources change (they enter the image via
  `ADD . /build_zone`).
- **After a full cache wipe** (`docker builder prune`), expect the long build
  again.

Verify the result:

``` bash
docker run --rm lemur_shiny Rscript -e 'cat(as.character(packageVersion("lemur")), nrow(lemur::data_gbd_lt()), "\n")'
# -> 2.0.2 142560
```

### System dependencies baked into the image

The `apt-get` line in the `Dockerfile` carries additions that are easy to
lose and painful to rediscover -- keep them if you edit the Dockerfile:

| Package | Why |
|---|---|
| `libuv1-dev` | `fs` → `bslib` → `shiny` build chain fails without it |
| `cmake` | `s2` (dependency of `sf`) builds its vendored abseil with it |
| `libpq-dev`, `libgdal-dev`, `libgeos-dev`, `libproj-dev`, `libudunits2-dev` | sf/gdal/units system libraries |

The R package pins in the Dockerfile mirror the minimum versions declared in
`DESCRIPTION` -- update both together when imports change.

## 4. API image — `lemur-api`

Small Flask container (Python 3.13, Flask 3.1) serving the REST API. Built
automatically on first `docker compose up -d api`, or manually:

``` bash
docker compose build api          # uses deploy/api/Dockerfile + requirements.txt
```

~40 s cold build; all dependencies are pinned in `requirements.txt`
(current, supported versions -- no EOL base). The API is deliberately *not*
published to GHCR: at this build cost, pulling saves nothing, while a second
registry artifact would need version-bumping on every data refresh.

No build-time configuration; the database credentials are supplied at
runtime through `LEMUR_DB_*` environment variables (see the run guide).

## 5. Database and proxy images

These are pulled, not built:

| Image | Role | Note |
|---|---|---|
| `postgres:17` | data store | pinned on purpose -- `postgres:18+` changed the data-directory layout and refuses the compose volume |
| `nginx:latest` | reverse proxy | pulls on `docker compose up` |
| `openanalytics/shinyproxy:2.6.0` | app launcher | config comes from `deploy/shinyproxy/application.yml`, bind-mounted at runtime -- no custom build |

## 6. Build context

`.dockerignore` keeps the build context small (the repo's `data-raw/`,
`.git`, `docs/`, `deploy/` etc. are excluded -- the image needs only package
sources and `inst/`). If the build seems to upload gigabytes, check that
`.dockerignore` is still intact in the repository root.

## 7. Rebuilding after changes

``` bash
git pull                         # or edit sources
docker compose --profile build build shiny   # app image + the db-loader (same image)
docker compose build api         # only if deploy/api changed
```

Then follow the run guide's "Updating data or code" section to reload the
database and restart the services.