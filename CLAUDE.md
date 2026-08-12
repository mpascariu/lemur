# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

`lemur` is an R package (>= 4.3.0) and Shiny application: "Life expectancy monitor upscaled in R". Users select mortality changes over the lifespan or at specific ages, and for overall or cause-specific mortality, and see how life expectancy shifts (e.g. "what if cardiovascular mortality fell 50%?"). It also compares cause-of-death profiles and life tables across regions, sexes, and time. The GBD2021 datasets are bundled (accessors `data_gbd2021_lt()`, `data_gbd2021_cod()`, `data_gbd2021_sdg()`), so all analysis functions run without a database. Install: `pak::pak("mpascariu/lemur")`; launch: `lemur::run_app()`.

## Commands

### Dev loop (fast startup)
`devtools::load_all()` is slow (~33s). The documented fast path is:
1. `R CMD INSTALL . --no-multiarch --with-keep.source` — re-run after every change under `R/`; the app runs the **installed** copy.
2. `Rscript dev/launch_fast.R [port]` — default port 8181, ~8s startup, local data mode.

### Tests
- Full suite: `R CMD check --as-cran` (this is the CI gate too, see `.github/workflows/r.yml`).
- In-session: `testthat::test_local()`, or a single file: `testthat::test_file("tests/testthat/test-fig4-decompose.R")`.
- Fixtures are defined once in `tests/testthat/setup.R` as small slices (one region/sex/year) of the multi-million-row full tables — keep tests on those slices, never the full datasets.

### Docs & metadata
- README.md is the single source of truth for the readme — edit it directly (the former `README.Rmd` source was removed).
- NAMESPACE is roxygen-generated (roxygen2 8.1.0); regenerate with `devtools::document()`, never hand-edit.

### Deployment
- `.env` (gitignored; copy `.env.example`) is the single source of deployment secrets. `POSTGRES_*` and `LEMUR_DB_*` blocks must agree or the app fails to authenticate. `docker compose up` runs nginx → shinyproxy → shiny app + postgres + a Flask API (`deploy/api`).
- Postgres schema + CSV load: `deploy/postgresql/init-db.sh`. ShinyProxy launches the app with `run_app(serverMode=T)` and forwards `LEMUR_DB_*` env vars (`deploy/shinyproxy/application.yml`).

## Architecture

### App flow
`run_app()` → `app_ui()` (UI builder) + `app_server()`. All figure/table captions live in `R/app_captions.R` (kept separate to keep the server script short).

**UI** (`R/app_ui.R`, `R/app_ui_dashboard.R`, `R/app_ui_datatab.R`): bslib (Bootstrap 5) `page_navbar` with six panels — Dashboard, Data, Methods, Sources, About, Contact. The Dashboard is top panel (analysis mode selector) + side panel (all inputs) + main panel (four chart cards). The Data tab renders the four underlying tables plus the decomposition and the reduction matrix.

**Server** (`R/app_server.R`): five analysis modes — `mode_cod` (within one region), `mode_cntr` (between two regions), `mode_sex` (male vs female), `mode_sdg` / `mode_sdg2` (SDG target scenarios). All inputs converge on a single debounced reactive `data_fig()` (250ms, tunable via `shinyOptions("lemur.debounce")`); data reactives use `bindCache()`, all outputs use `suspendWhenHidden = TRUE`, and `data_cod_change` is deliberately *not* debounced. When the reduction matrix is all zeros (browse mode) the decomposition and figure 4 are skipped.

### Data pipeline
Accessors read pre-factorized `data.table`s from `inst/extdata/*_dt.rds` at first call. Two data sources:
- Local (`serverMode = FALSE`, the default): filter in memory via `dt_filter_local()` (data.table subsetting; `region2` aliases `region1` unless `mode_cntr`; sex filter skipped unless `mode_sex`).
- PostgreSQL (`serverMode = TRUE`): filter via parameterized SQL (`dt_filter_sql()`) over a pool/DBI/RPostgres connection. Credentials come from env vars `LEMUR_DB_HOST`/`LEMUR_DB_NAME`/`LEMUR_DB_USER`/`LEMUR_DB_PORT` (defaults: postgres/gbd2021/lemur/5432); `LEMUR_DB_PASSWORD` is **required** in server mode and the app refuses to start without it.

`prepare_data()` turns the filtered COD + LT tables into `list(cod_initial, cod_final, lt_initial, lt_final)`.

### Computation
- `build_cod_matrix()` → cause-share matrix (age × cause proportions, zero-death rows filled with a uniform share).
- `modify_cod()` applies a scalar/vector/matrix % change (−100 rejected as "100%"); `build_reduction_matrix()` fills the user-selected ages × causes; `modify_cod_table()` applies it to the deaths table; `modify_life_table()` recomputes the life table from the modified COD via the vendored MortalityLaws C3_qx path (`life_table_from_qx`).
- Decomposition: `decompose_by_cod()` (Andreev et al. 2002 stepwise replacement, symmetrical) and `decompose_by_age()` (DE + IE). Both return an S3 `"decompose"` data.frame whose `decomposition` column sums to the life-expectancy gap.

### Figures
Native plotly everywhere (ggplot2 removed in v1.6.0): `plot_change`, `plot_cod` (barplot/piechart; comparison modes build a two-panel subplot), `plot_decompose` (`by = "both" | "cod" | "age"`), `plot_map` (leaflet, skipped on CRAN). Cause colors come from `epidemiology_palette()` — a named cause→hex vector keyed by cause **name**, so colors stay stable across factor-level reordering. `plotly_decompose` reproduces the old ggplot `position_stack` geometry by adding bar traces in **reverse factor order** with `barmode = "relative"`, `base = NULL` (the last cause sits adjacent to zero).

## Conventions & pitfalls

- **Vendoring**: one- or two-function uses of third-party packages are inlined with attribution instead of adding the dependency (see the `R/fun-utils.R` header: `scales::label_number_si`/`number`, tibble rowname helpers; also `MortalityLaws` in `R/fun-LifeTable.R`). Continue this pattern: copy the function plus the attribution comment.
- **`data.table` `[-i]` is row subsetting**, not column subsetting — `modify_cod_table()` drops the COD column by *name*.
- **`ifelse()` on factors** returns integer codes — decompose functions coerce identification columns to character first.
- **`melt()`/`dcast()`** require `data.table` inputs, not `data.frame`.
- **`plotly::layout(annotations = ...)` replaces** existing annotations; `plot_change` appends to the list instead.
- **Lazy data binds at namespace load**, not attach: `R/zzz.R` `.onLoad()` calls `utils::data("data_app_input", ...)` so `lemur::` calls (e.g. `run_app()` in a fresh session) work without `library(lemur)`. Keep data objects loaded there, never rely on attach-time binding.
- **Tests pin the plotly encoding structurally** (see `test-fig4-decompose.R`): assert against `plotly::plotly_build()`'s `x$data`/`x$layout`, not pixels.
