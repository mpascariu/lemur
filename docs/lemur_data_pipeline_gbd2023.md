# lemur data pipeline — GBD 2023 update

**Author:** M. D. Pascariu · **Date:** 2026-08-31 · **Package version:** 2.0.0

This document is a complete, from-scratch runbook for producing the data the
`lemur` package ships. It explains the logic, lists the exact data requirements,
points at the R scripts that do the work, and describes how the 2023 update was
carried out. Anyone with the four input collections (below) and the listed R
packages can reproduce the bundled datasets end to end.

---

## 1. What the pipeline produces

`lemur` bundles three large tables, exposed through the accessor functions
`data_gbd_cod()`, `data_gbd_sdg()` and `data_gbd_lt()`:

| Dataset | Content | Source round(s) | Periods | Regions | Ages | Causes |
|---|---|---|---|---|---|---|
| `data_gbd_cod` | cause-of-death counts | GBD 2021 + 2023 deaths | 1990-2023 | 216 | 0-95 | 18 |
| `data_gbd_sdg` | COD grouped for SDG tracking | GBD 2021 + 2023 deaths | 1990-2023 | 216 | 0-95 | 21 |
| `data_gbd_lt` | abridged life tables | GBD 2023-round qxn only | 1990-2023 | 216 | 0-95 | — |

Each is built twice on disk:

1. **A rich `.Rdata`** written by the process scripts (e.g.
   `data_gbd_cod_20260831.Rdata`) — used for inspection and as the bridge to
   the package.
2. **A fast `.rds`** written by `build_fast_data.R` (`inst/extdata/cod_dt.rds`,
   `lt_dt.rds`, `sdg_dt.rds`) — pre-factorized, death-rounded and xz-compressed
   `data.table`s that the accessors and the Shiny app actually load (the three
   together total ~18 MB). Keeping them out of `data/` avoids shipping the same
   data twice at full size.

`process_app_input.R` builds a small `data_app_input` object that drives the
app's dropdowns (regions, countries, periods, cause levels, age grid).

---

## 2. Environment & dependencies

- **R >= 4.3**. Work is done from the **package root** (`getwd()` = the repo
  root), because every script builds paths relative to it.
- Required packages: `tidyverse` (dplyr, tidyr, readr, purrr), `readxl`,
  `janitor`, `data.table`, `MortalityLaws` (life tables), `usethis`
  (`data_app_input`). Regenerating docs additionally needs `roxygen2`
  (`devtools::document()`).

The pipeline is deterministic and repeatable: running it twice on the same
inputs yields identical tables.

---

## 3. Data requirements (the four input collections)

All raw data is **git-ignored** and lives under `data-raw/`. Reproducing from
scratch requires:

1. **The GBD 2021 tools guide** (kept on disk, not a download):
   `data-raw/GBD_2021_Data_Tools_Guide/IHME_GBD_2021_A1_HIERARCHIES_Y2024M05D15.XLSX`.
   This one xlsx supplies, via `gbd_utils::read_hierarchy()`:
   - the **cause hierarchy** (sheet *Cause Hierarchy*) — the `cod_selection`
     / `sdg_selection` grouping and the `cod_order` / `sdg_order` ranks;
   - the **location hierarchy** (sheet *GBD 2021 Locations Hierarchy*) — the
     `location_id` → `location_name` / `level` map. Cause and location ids are
     stable across GBD rounds, so one file keys both downloads.
2. **GBD 2021 results** (download, Apr 2025):
   `data-raw/IHME_GBD2021_Data/CoD_Level_2/*.zip` and `CoD_Level_3/*.zip`
   (deaths, 1990-2021). Selection & citation:
   `GBD2021_Download_Selection_Settings.xlsx`.
3. **GBD 2023 results** (download, Aug 2026):
   - deaths (year 2023): `data-raw/IHME_GBD2023_Data/IHME-GBD_2023-{1..4}.zip`;
   - life-table qxn (1990-2023): the two
     `IHME-GBD_LT_*.zip` ("LT", probability-of-death);
   - selection & citation: `GBD2023_Download_Selection_Settings.xlsx`.
4. **SDG targets** (for reference only): `data-raw/GBD_SDG_targets/*.xlsx`.

The raw tables carry GBD's standard columns
(`location_id, sex_name, age_name, year, cause_id, cause_name, val`); the qxn
files are the same schema but have **no `cause_id`/`cause_name`** (they are
all-cause by construction).

```
data-raw/
├── GBD_2021_Data_Tools_Guide/
│   └── IHME_GBD_2021_A1_HIERARCHIES_Y2024M05D15.XLSX   # cause + location maps
├── IHME_GBD2021_Data/                                   # 2021 round -- deaths 1990-2021
│   ├── CoD_Level_2/*.zip                                #   all level-2 causes (+ "All causes")
│   └── CoD_Level_3/*.zip                                #   level-3 subgroups (5 COD / 8 SDG)
├── IHME_GBD2023_Data/                                   # 2023 round
│   ├── IHME-GBD_2023-{1..4}.zip                         #   deaths, year 2023
│   └── IHME-GBD_LT_*.zip                                #   life-table qxn, 1990-2023
├── GBD_SDG_targets/*.xlsx                               #   (reference only)
├── gbd_utils.R  process_gbd_{cod,sdg,lt}.R              #   the pipeline scripts
├── build_fast_data.R  process_app_input.R
└── _archive/gbd2021_2026-08-31/                         #   backup of prior tables (untracked)
```

---

## 4. The logic & the design decisions

The original (v1.x) pipeline read only GBD 2021, ungrouped the terminal 95+
interval to 110 with a pclm model, and extended the life tables to 110 with a
Kannisto law. The 2023 refresh keeps the same cause grouping but **re-draws
four lines** (all agreed with the maintainer):

1. **Life tables use the GBD 2023 round only.** The 2023 qxn download covers
   1990-2023, so the whole life-table dataset is one consistent round (no
   2021/2023 seam, and no reliance on the 2021 `PoD` files at all).
2. **No ungroup, no tail.** The pclm ungroup and the Kannisto extension are
   dropped; the terminal `95+` group stays an **open interval at `x = 95`**.
   COD, SDG and LT share one 0-95 age grid, which the app requires (its age
   references were moved `110 → 95`).
3. **Macro regions removed for consistency.** Africa/Americas/Asia/Europe were
   in the 2021 download but not the 2023 pick list, so they are dropped from
   **both** rounds — the combined region set is exactly 216 everywhere.
4. **Korea mislabel fixed.** The GBD 2021 hierarchy calls location id 7
   (actually North Korea) "South Korea". Symmetric fix: id 7 → *North Korea*,
   id 68 → *South Korea*.
5. **Accessors renamed** `data_gbd2021_{cod,lt,sdg}()` →
   `data_gbd_{cod,lt,sdg}()` (breaking, hence v2.0.0).

The cause "logic" is unchanged from v1.x and worth stating precisely because it
drives correctness: the **Level_2 download already contains every death**
(including those counted again inside the Level_3 rows we keep). For the handful
of Level_3 subcategories we report on their own, those deaths are **subtracted
back out of their Level_2 parent** so nothing is double counted — see §5.
"COVID-19 (2)" (Other COVID-19 pandemic related outcomes) is folded **into**
COVID-19 before its row is dropped.

---

## 5. The scripts, step by step

The pipeline is a straight top-to-bottom sequence. Five stages; only stage ③
fans out (three scripts running in parallel against the same inputs):

```
①  RAW INPUTS                        (git-ignored, under data-raw/)
     - 2021 round : CoD_Level_2/_3 zips          -> deaths, 1990-2021
     - 2023 round : IHME-GBD_2023-*.zip          -> deaths, 2023
     - 2023 round : IHME-GBD_LT_*.zip            -> life-table qxn, 1990-2023
             │
             ▼
②  SHARED MACHINERY
     data-raw/gbd_utils.R
     # the same helpers used by all three datasets:
     #   read all zips -> dedup -> location & cause maps -> reshape
             │
             ▼
③  THREE PARALLEL PROCESS SCRIPTS     (each reads ① and ②, writes one .Rdata)
     process_gbd_cod.R  ->  data_gbd_cod.Rdata
     process_gbd_sdg.R  ->  data_gbd_sdg.Rdata
     process_gbd_lt.R   ->  data_gbd_lt.Rdata
             │
             ▼
④  SHIP IT
     data-raw/build_fast_data.R   (round deaths, factorize, xz)
        ->  inst/extdata/{cod,sdg,lt}_dt.rds       ★ the .rds the package ships
             │
             ▼
⑤  APP METADATA
     data-raw/process_app_input.R  ->  data/data_app_input.rda   (dropdowns)
```

### 5.1 `data-raw/gbd_utils.R` — the shared machinery

Idempotent helpers used by all three process scripts (no side effects):

- `read_hierarchy()` — reads the two hierarchy sheets of the 2021 xlsx.
- `build_location_map(loc)` — `location_id → region`, standardizing on the 2021
  names: countries (level 3) keep their title-case name, everything coarser is
  uppercased, then the Korea relabel is forced.
- `age_to_x(age_name)` — `"<1 year"→0`, `"12-23 months"→1`, `"2-4 years"→2`,
  `"5-9 years"→5`, …, `"95+ years"→95`; `"All ages"→NA` and is filtered out.
- `read_zips_csv(zips)` — unzips and reads every CSV inside with
  `data.table::fread` (much faster than readr on these tables) and binds them.
- `dedup_deaths(raw)` — removes duplicate rows on
  `(location_id, cause_id, sex, age, year)`. **Required**: the 2021 Level_2
  download is delivered as overlapping chunks and ~60% of its rows are exact
  duplicates; without dedup the totals are inflated and the reductions mis-applied.
- `read_deaths_2021(level3_causes)` — Level_2 + (Level_3 filtered to the
  requested causes), deduped.
- `read_deaths_2023()` — all `IHME-GBD_2023-*.zip`, deduped.
- `read_lt_2023()` — all `IHME-GBD_LT_*.zip`, deduped.
- `shape_deaths(raw, loc_map)` — to (region, sex, period, x, cause_id, deaths),
  dropping macro-region ids and non-numeric ages.
- `allcause_reference(raw, loc_map)` — per (region, sex, period, x) totals of
  the **"All causes"** row (cause id 294), used by the consistency checks.
- `add_both_sex(d)` — appends the `both` sex as male+female.
- `finalise(d, cause_levels)` — factors causes to the canonical rank and
  `complete()`s the age×cause×sex×period grid, filling holes with 0.

### 5.2 `data-raw/process_gbd_cod.R` — COD

1. `cod_map` (cause → `cod_selection`) and `cod_rank` (ordered category list)
   are read from the cause hierarchy. The Level_3 causes to keep are hard-coded:
   COVID-19, Colon and rectum cancer, Tracheal/bronchus/lung cancer, IHD, Stroke.
2. Read + shape both rounds, and the all-cause references.
3. `prep()` maps each row to its `cod_selection`, then aggregates deaths by
   (region, sex, period, x, cause). It keeps every mapped cause — including
   `COVID-19 (2)` — through the next step.
4. `reduce_cod()` subtracts the retained Level_3 children out of their Level_2
   parents (per x, region, sex, period): Other Cardiovascular −= IHD − Stroke;
   Other Neoplasms −= Colon − Lung; Respiratory Infections −= COVID-19;
   COVID-19 += COVID-19 (2). `sum()` over an absent cause is 0, which keeps the
   2023 file (no `COVID-19 (2)`) well-behaved. The `COVID-19 (2)` row is then
   dropped.

   The reduction, as a tree (Level_2 parent minus the kept Level_3 children):

   ```
                        Level_2 parent (holds all deaths)      Level_3 children kept
      Other Cardiovascular   ─  =  Ischemic Heart Disease   ─   Stroke
      Other Neoplasms        ─  =  Colon & Rectum Cancer    ─   Lung Cancer
      Respiratory Inf (excl COVID) ─ =  COVID-19
      COVID-19              ─  =  COVID-19 (2)      →  COVID-19 (2) row then dropped
   ```

5. `add_both_sex()` + `finalise(cod_rank)`.
6. **Consistency checks** (fail loudly if violated):
   - no NA deaths;
   - **all-cause**: sum of the 18 causes ≈ the cause-294 "All causes" total for
     every (region, sex, period, age) — tolerance 1 death;
   - `both == male + female`.
7. Saves `data-raw/IHME_GBD2021_Data/data_gbd_cod_<YYYYMMDD>.Rdata`.

### 5.3 `data-raw/process_gbd_sdg.R` — SDG grouping

Identical skeleton, with the **SDG** cause map:
`sdg_rank` has 21 categories (Enteric Infections, NTD, Malaria, Other
Communicable, HIV/STD, Neoplasms, Kidney disease, Diabetes, Cardiovascular,
Tuberculosis, Respiratory, Chronic respiratory, Maternal, Neonatal, Other
Non-Communicable, Transport, Poisonings, Forces of nature, Injuries,
Interpersonal violence, Self-harm). `reduce_sdg()` applies the five SDG
subtractions (Respiratory −= TB; NTD −= Malaria; Kidney −= Diabetes; Injuries −=
Poisonings − Forces of nature; Interpersonal violence −= Self-harm). Same
checks: all-cause consistency and both = male+female must pass with 0 failures.

### 5.4 `data-raw/process_gbd_lt.R` — life tables

1. Read the **2023-round qxn** (`measure_name == "Life table"`), shape to
   (region, sex, period, x, qxn) with the same location map / macro-drop /
   Korea rules.
2. For every (region, sex, period) — 216 × 3 × 10 = 6480 tables — build an
   abridged life table on `x = c(0, 1, 2, seq(5, 95, 5))` with
   `MortalityLaws::LifeTable()`, forcing the terminal 95+ `q = 1` (GBD already
   reports q(95+) = 1). No Kannisto tail — the 95+ open interval uses the
   default open-interval `ax`.
3. Assert no NA in `mx`/`ex`; saves `data_gbd_lt_<YYYYMMDD>.Rdata`.

### 5.5 Turn the `.Rdata` into the shipped `.rds`

- `data-raw/build_fast_data.R` — for each stem, pick the newest
  `data_gbd_{cod,sdg,lt}_*.Rdata`, round the `deaths` column to 2 decimals,
  `factorize()` the character columns (region/sex), convert to `data.table`,
  and xz `saveRDS()` to `inst/extdata/{cod,lt,sdg}_dt.rds`. The explicit output
  shows each table's in-memory and on-disk size.
- `data-raw/process_app_input.R` — (re)build `data_app_input`: regions (the 12
  level-0 locations, macro regions excluded), the 204 countries, the 18 COD /
  21 SDG cause ranks, `period = c(seq(1990, 2015, 5), 2019, 2020, 2021, 2023)`,
  `sex` and `x = c(0, 1, 2, seq(5, 95, 5))`.

### 5.6 Full reproduction, from a clean checkout

```bash
# 1. place the four input collections in data-raw/ (see §3)
# 2. run the three process scripts (several minutes each; they read ~30 zips)
Rscript data-raw/process_gbd_cod.R
Rscript data-raw/process_gbd_sdg.R
Rscript data-raw/process_gbd_lt.R
# 3. rebuild the shipped fast .rds and the app input
Rscript data-raw/build_fast_data.R
Rscript data-raw/process_app_input.R
# 4. (optional) regenerate the man pages after roxygen edits
Rscript -e 'devtools::document()'
# 5. validate
Rscript -e 'testthat::test_local()'
```

---

## 6. The 2023 update, specifically

**Why exactly the year 2023 was updated.** Following the GBD 2023 release,
IHME's GBD results-tool download was tightened: the free aggregate download no
longer serves the full multi-decade cause-of-death series at once — for the
*deaths* measure it hands out only the most recent calendar year (2023). That
restriction is the motivation for updating exactly the year 2023 here. The
historical deaths (1990-2021) therefore carry over unchanged from the GBD 2021
round, and only 2023 contributes genuinely new figures to the death counts.
(The life-table qxn measure was not subject to the same one-year cap — it came
through as a full 1990-2023 series — which is why the life tables are uniformly
GBD 2023 while the death counts mix the two rounds.)

- The 2023 download is **deaths only for year 2023**, plus life-table qxn for
  the full 1990-2023 series. It does **not** provide 2021-round-style separate
  Level_2/Level_3 folders — the deaths come as flat CSVs with the cause level
  implicit in each `cause_id` — which is why `read_deaths_2023()` grabs all
  `IHME-GBD_2023-*.zip` and lets the hierarchy map the ids.
- COD/SDG therefore **side by side** the 2021-round deaths (1990-2021) with the
  2023-round deaths (2023). The life tables, by contrast, are **entirely**
  2023-round, so the LT/COD seam at 2021-2023 differs slightly by round — the
  GBD re-estimate — by design.
- The 2023 pick list lacks the four macro regions, which drove decision #3
  (drop them from both rounds) so the region set is consistent.
- Two defects were found and fixed while building (both would otherwise have
  produced wrong totals): the duplicated 2021 Level_2 rows (`dedup_deaths`),
  and the premature dropping of `COVID-19 (2)` before the reduction folded it
  into COVID-19 (`prep` now keeps every mapped cause through `reduce_cod`).
- The prior in-use tables are backed up (git-ignored) at
  `data-raw/_archive/gbd2021_2026-08-31/` so nothing is lost.

---

## 7. Verification checklist

To confirm a build is correct before releasing:

```r
D <- data_gbd_cod(); L <- data_gbd_lt()
stopifnot(length(unique(D$period)) == 10, 2023 %in% unique(D$period))
stopifnot(max(D$x) == 95, !any(D$region %in% c("AFRICA","AMERICAS","ASIA","EUROPE")))
stopifnot(all(c("North Korea","South Korea") %in% unique(D$region)))
```

Plus the in-script checks (all-cause consistency and `both = male+female`) must
both report **0 failures**, and `testthat::test_local()` must be green.

---

## 8. Outputs

| Output | Writer | Purpose |
|---|---|---|
| `data-raw/IHME_GBD2021_Data/data_gbd_{cod,sdg,lt}_<date>.Rdata` | process scripts | inspection / archive |
| `inst/extdata/{cod,lt,sdg}_dt.rds` | `build_fast_data.R` | shipped fast tables |
| `data/data_app_input.rda` | `process_app_input.R` | app dropdowns |
| `data-raw/_archive/` | manual backup | prior in-use tables (not tracked) |
