#!/usr/bin/env Rscript
# ===========================================================================
# Reproduce Tables 1, A3 and A4 of the LE Monitor manuscript
# ---------------------------------------------------------------------------
# Manuscript: "Software Application Profile: The Life Expectancy Monitor --
# an interactive tool for assessing the effects of cause-specific mortality
# reductions" (Canudas-Romo, Pascariu, Lei, Leasure, Aburto).
#
# Data: the GBD 2021+2023 tables shipped with {lemur}
#       data_gbd_lt(), data_gbd_cod(), data_gbd_sdg() -- calendar year 2023.
#
# Usage
#   Rscript docs/reproduce_tables.R [output.xlsx]
#
# Output
#   One .xlsx workbook with:
#     Table 1   African Union SDG scenarios (females, males, both sexes)
#     Table A3  Sex gap under 30% reductions, four countries
#     Table A4  Between-country gaps under 30% reductions, five country pairs
#     Check     Computed vs published values, cell by cell
#     Notes     Method, column definitions and reproduction caveats
#
# Requirements
#   {lemur} installed from this repository (R CMD INSTALL .) and {openxlsx}.
# ===========================================================================

suppressPackageStartupMessages({
  library(lemur)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(openxlsx)
})

year <- 2023L
args <- commandArgs(trailingOnly = TRUE)
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
out_file <- if (length(args)) args[1] else
  file.path(if (length(script_file)) dirname(normalizePath(script_file)) else getwd(),
            "lemur_manuscript_tables.xlsx")
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

# Warn if the installed package is not the working tree.
installed_version <- as.character(packageVersion("lemur"))
description_version <- tryCatch(read.dcf("DESCRIPTION")[1, "Version"], error = function(e) NULL)

if (!is.null(description_version) && installed_version != description_version) {
  warning(
    "Installed lemur ", installed_version, " differs from DESCRIPTION ",
    description_version, ". Run R CMD INSTALL . first.",
    call. = FALSE
  )
}

# -- data and helpers -------------------------------------------------------

cod_all <- as_tibble(data_gbd_cod())
sdg_all <- as_tibble(data_gbd_sdg())
lt_all <- as_tibble(data_gbd_lt())

slice_cod <- function(region, sex, source = c("cod", "sdg")) {
  source <- match.arg(source)

  data <- if (source == "cod") cod_all else sdg_all

  out <- data |>
    filter(region == .env$region, sex == .env$sex, period == year)
  return(out)
}

slice_lt <- function(region, sex) {
  out <- lt_all |>
    filter(region == .env$region, sex == .env$sex, period == year)
  return(out)
}

empty_reduction <- function(cod) {
  ages <- sort(unique(cod$x))
  causes <- unique(as.character(cod$cause_name))
  out <- matrix(0, length(ages), length(causes),
                dimnames = list(as.character(ages), causes))
  return(out)
}

# All paths apply the matrix once by default. `twice = TRUE` reproduces the
# old comparison-mode pipeline for the audit workbook: modify the COD table,
# then rebuild the life table from it with the same matrix.
e0_after <- function(region, sex, change, source = "cod", twice = FALSE) {
  cod <- slice_cod(region, sex, source)
  lt <- slice_lt(region, sex)

  if (any(change != 0)) {
    if (twice) cod <- modify_cod_table(cod, change)
    lt <- modify_life_table(lt, cod, change)
  }

  out <- lt$ex[match(0, lt$x)]
  return(out)
}

# App display is two decimals; the manuscript rounds half-up to one.
round_pub <- function(x) {
  out <- floor(round(x, 2) * 10 + 0.5) / 10
  return(out)
}

# -- Table 1 ----------------------------------------------------------------

au <- "AFRICAN UNION"
epi_causes <- c("HIV/ AIDS / STD", "Tuberculosis", "Malaria",
                "Neglected Tropical Diseases (excl. Malaria)")
ncd_causes <- c("Cardiovascular Diseases", "Neoplasms", "Diabetes mellitus",
                "Chronic Respiratory diseases")

t1_spec <- list(
  "Under-five mortality*"     = list(ages = 0:2, pct = -54),
  "Epidemics"                 = list(causes = epi_causes, pct = -100),
  "Non-communicable diseases" = list(causes = ncd_causes, pct = -30),
  "Suicide"                   = list(causes = "Self-harm", pct = -30),
  "Road traffic injuries"     = list(causes = "Transport Injuries", pct = -50),
  "Disasters"                 = list(causes = "Exposure to forces of nature", pct = -75)
)
t1_scenarios <- c("Original", names(t1_spec), "All selected changes")

t1_matrix <- function(cod, scenario) {
  change <- empty_reduction(cod)

  if (scenario == "All selected changes") {
    for (spec in t1_spec[-1]) change[, spec$causes] <- spec$pct

    rows <- as.character(0:2)
    u5 <- t1_spec[["Under-five mortality*"]]
    change[rows, ] <- ((1 + u5$pct / 100) * ((change[rows, ] + 100) / 100) - 1) * 100
  } else if (scenario %in% names(t1_spec)) {
    spec <- t1_spec[[scenario]]

    if (is.null(spec$causes)) {
      change[as.character(spec$ages), ] <- spec$pct
    } else {
      change[, spec$causes] <- spec$pct
    }
  }

  return(change)
}

t1_cod <- map(set_names(c("female", "male", "both")), ~ slice_cod(au, .x, "sdg"))

t1_raw <- expand_grid(sex = names(t1_cod), Scenario = t1_scenarios) |>
  mutate(e0 = map2_dbl(sex, Scenario, ~ e0_after(au, .x, t1_matrix(t1_cod[[.x]], .y), "sdg"))) |>
  pivot_wider(names_from = sex, values_from = e0) |>
  mutate(across(-Scenario, round_pub))

table_1 <- tibble(
  Scenario        = t1_scenarios,
  `Reduction (%)` = c("0", "54", "100", "30", "30", "50", "75", "-")
) |>
  left_join(t1_raw, by = "Scenario") |>
  rename(Females = female, Males = male, `Both sexes` = both)

# -- Table A3 ---------------------------------------------------------------

a3_countries <- tribble(
  ~Country, ~region,
  "USA", "United States of America",
  "Russia", "Russia",
  "Indonesia", "Indonesia",
  "South Africa", "South Africa"
) |>
  mutate(Country = factor(Country, levels = Country))

cause_groups <- list(
  Neoplasms = c("Lung Cancer", "Colon and Rectum Cancer", "Other Neoplasms"),
  CVD       = c("Ischemic Heart Disease", "Stroke", "Other Cardiovascular"),
  External  = c("Injuries", "Self-Harm and Violence")
)
a3_scenarios <- c("Original", names(cause_groups), "Total")

a3_matrix <- function(cod, scenario) {
  change <- empty_reduction(cod)
  causes <- if (scenario == "Total") unlist(cause_groups) else cause_groups[[scenario]]

  if (!is.null(causes)) {
    change[, causes] <- -30
  }

  return(change)
}

make_a3_table <- function(twice = FALSE) {
  raw <- a3_countries |>
    expand_grid(sex = c("female", "male"), Scenario = a3_scenarios) |>
    mutate(e0 = pmap_dbl(
      list(region, sex, Scenario),
      ~ e0_after(..1, ..2, a3_matrix(slice_cod(..1, ..2, "cod"), ..3), "cod", twice)
    )) |>
    pivot_wider(names_from = Scenario, values_from = e0) |>
    mutate(Country = factor(Country, levels = a3_countries$Country))

  blocks <- raw |>
    group_by(Country) |>
    reframe(
      Sex = c("Female", "Male", "Difference"),
      across(where(is.numeric), function(x) {
        rounded <- round_pub(x)
        out <- c(rounded, round_pub(rounded[1] - rounded[2]))
        return(out)
      })
    )

  table <- blocks |>
    group_split(Country) |>
    map(~ bind_rows(.x, .x[NA_integer_, ])) |>
    bind_rows() |>
    slice(-n()) |>
    mutate(Country = if_else(Sex == "Female", as.character(Country), ""))

  return(list(raw = raw, blocks = blocks, table = table))
}

a3_after <- make_a3_table(FALSE)
a3_before <- make_a3_table(TRUE)
a3_blocks <- a3_after$blocks
table_a3 <- a3_after$table

# -- Table A4 ---------------------------------------------------------------

a4_pairs <- tribble(
  ~Pair, ~Country,
  "Australia / New Zealand", "Australia",
  "Australia / New Zealand", "New Zealand",
  "Spain / France", "Spain",
  "Spain / France", "France",
  "China / India", "China",
  "China / India", "India",
  "Brazil / Mexico", "Brazil",
  "Brazil / Mexico", "Mexico",
  "Morocco / Egypt", "Morocco",
  "Morocco / Egypt", "Egypt"
) |>
  mutate(Pair = factor(Pair, levels = unique(Pair)))

make_a4_table <- function(twice = FALSE) {
  raw <- a4_pairs |>
    expand_grid(Scenario = a3_scenarios) |>
    mutate(e0 = map2_dbl(
      Country,
      Scenario,
      ~ e0_after(.x, "both", a3_matrix(slice_cod(.x, "both", "cod"), .y), "cod", twice)
    )) |>
    pivot_wider(names_from = Scenario, values_from = e0)

  blocks <- raw |>
    group_by(Pair) |>
    reframe(
      Country = c(Country, "Difference"),
      across(where(is.numeric), function(x) {
        rounded <- round_pub(x)
        out <- c(rounded, round_pub(rounded[1] - rounded[2]))
        return(out)
      })
    )

  table <- blocks |>
    group_split(Pair) |>
    map(~ bind_rows(.x, .x[NA_integer_, ])) |>
    bind_rows() |>
    slice(-n()) |>
    select(-Pair)

  return(list(raw = raw, blocks = blocks, table = table))
}

a4_after <- make_a4_table(FALSE)
a4_before <- make_a4_table(TRUE)
a4_blocks <- a4_after$blocks
table_a4 <- a4_after$table

# -- published values and check --------------------------------------------

pub_t1 <- tribble(
  ~Scenario, ~Females, ~Males, ~`Both sexes`,
  "Original", 66.8, 63.5, 65.1,
  "Under-five mortality*", 68.9, 65.9, 67.4,
  "Epidemics", 69.9, 66.5, 68.2,
  "Non-communicable diseases", 68.8, 65.3, 67.0,
  "Suicide", 66.9, 63.6, 65.2,
  "Road traffic injuries", 67.1, 64.0, 65.5,
  "Disasters", 66.9, 63.5, 65.2,
  "All selected changes", 74.3, 71.5, 72.9
)

pub_a3_list <- list(
  USA = rbind(
    Female = c(80.8, 81.4, 81.3, 81.0, 82.3),
    Male = c(75.7, 76.3, 76.4, 76.1, 77.7),
    Difference = c(5.1, 5.1, 4.9, 4.9, 4.6)
  ),
  Russia = rbind(
    Female = c(78.5, 79.0, 79.7, 78.7, 80.7),
    Male = c(67.5, 67.9, 68.9, 68.4, 70.9),
    Difference = c(11.0, 11.1, 10.8, 10.3, 9.8)
  ),
  Indonesia = rbind(
    Female = c(74.7, 75.1, 75.9, 74.9, 76.6),
    Male = c(69.8, 70.2, 71.0, 70.1, 71.8),
    Difference = c(4.9, 4.9, 4.9, 4.8, 4.8)
  ),
  `South Africa` = rbind(
    Female = c(69.4, 69.8, 70.0, 69.7, 70.8),
    Male = c(63.1, 63.5, 63.6, 63.9, 64.9),
    Difference = c(6.3, 6.3, 6.4, 5.8, 5.9)
  )
)

pub_a3 <- vector("list", length(pub_a3_list))
for (i in seq_along(pub_a3_list)) {
  mat <- pub_a3_list[[i]]
  colnames(mat) <- a3_scenarios
  pub_a3[[i]] <- as_tibble(mat, rownames = "Sex") |>
    mutate(Country = names(pub_a3_list)[i], Row = paste(Country, Sex), .after = 1)
}
pub_a3 <- bind_rows(pub_a3)

pub_a4_list <- list(
  "Australia / New Zealand" = rbind(
    Australia = c(83.5, 84.2, 83.9, 83.7, 85.1),
    `New Zealand` = c(81.3, 82.1, 81.9, 81.5, 83.2),
    Difference = c(2.2, 2.1, 2.0, 2.2, 1.9)
  ),
  "Spain / France" = rbind(
    Spain = c(82.8, 83.6, 83.2, 83.0, 84.5),
    France = c(82.3, 83.3, 82.7, 82.6, 84.2),
    Difference = c(0.5, 0.3, 0.5, 0.4, 0.3)
  ),
  "China / India" = rbind(
    China = c(79.7, 80.3, 80.7, 79.9, 81.8),
    India = c(71.4, 71.7, 72.3, 71.7, 73.1),
    Difference = c(8.3, 8.6, 8.4, 8.2, 8.7)
  ),
  "Brazil / Mexico" = rbind(
    Brazil = c(76.4, 77.0, 77.1, 76.9, 78.4),
    Mexico = c(76.0, 76.4, 76.6, 76.4, 77.6),
    Difference = c(0.4, 0.6, 0.5, 0.5, 0.8)
  ),
  "Morocco / Egypt" = rbind(
    Morocco = c(72.8, 73.1, 74.3, 73.2, 75.3),
    Egypt = c(69.8, 70.1, 71.2, 70.0, 71.9),
    Difference = c(3.0, 3.0, 3.1, 3.2, 3.4)
  )
)

pub_a4 <- vector("list", length(pub_a4_list))
for (i in seq_along(pub_a4_list)) {
  mat <- pub_a4_list[[i]]
  colnames(mat) <- a3_scenarios
  pub_a4[[i]] <- as_tibble(mat, rownames = "Country") |>
    mutate(Pair = names(pub_a4_list)[i], Row = paste(Pair, "-", Country))
}
pub_a4 <- bind_rows(pub_a4)

table_1_original <- table_1 |>
  select(Scenario, `Reduction (%)`) |>
  bind_cols(pub_t1 |> select(-Scenario))
table_1_before <- table_1

pub_a3_blocks <- vector("list", length(pub_a3_list))
for (i in seq_along(pub_a3_list)) {
  mat <- pub_a3_list[[i]]
  colnames(mat) <- a3_scenarios
  pub_a3_blocks[[i]] <- as_tibble(mat, rownames = "Sex") |>
    mutate(
      Country = factor(names(pub_a3_list)[i], levels = a3_countries$Country),
      .before = 1
    )
}
pub_a3_blocks <- bind_rows(pub_a3_blocks)

table_a3_original <- pub_a3_blocks |>
  group_split(Country) |>
  map(~ bind_rows(.x, .x[NA_integer_, ])) |>
  bind_rows() |>
  slice(-n()) |>
  mutate(Country = if_else(Sex == "Female", as.character(Country), ""))

pub_a4_blocks <- vector("list", length(pub_a4_list))
for (i in seq_along(pub_a4_list)) {
  mat <- pub_a4_list[[i]]
  colnames(mat) <- a3_scenarios
  pub_a4_blocks[[i]] <- as_tibble(mat, rownames = "Country") |>
    mutate(
      Pair = factor(names(pub_a4_list)[i], levels = levels(a4_pairs$Pair)),
      .after = 1
    )
}
pub_a4_blocks <- bind_rows(pub_a4_blocks)

table_a4_original <- pub_a4_blocks |>
  group_split(Pair) |>
  map(~ bind_rows(.x, .x[NA_integer_, ])) |>
  bind_rows() |>
  slice(-n()) |>
  select(-Pair)

check_values <- function(before, after, original, table, cols) {
  prep <- function(data, value) {
    out <- data |>
      select(Row, all_of(cols)) |>
      pivot_longer(-Row, names_to = "Column", values_to = value)
    return(out)
  }

  out <- prep(before, "Before") |>
    left_join(prep(after, "After"), by = c("Row", "Column")) |>
    left_join(prep(original, "Original"), by = c("Row", "Column")) |>
    transmute(
      Table = table,
      Row,
      Column,
      Original = round_pub(Original),
      `Before fix` = round_pub(Before),
      `After fix` = round_pub(After),
      `Before fix - Original` = round(Before - Original, 2),
      `After fix - Original` = round(After - Original, 2)
    )
  return(out)
}

check <- bind_rows(
  check_values(
    before = rename(table_1_before, Row = Scenario),
    after = rename(table_1, Row = Scenario),
    original = rename(table_1_original, Row = Scenario),
    table = "Table 1",
    cols = c("Females", "Males", "Both sexes")
  ),
  check_values(
    before = a3_before$blocks |> mutate(Row = paste(Country, Sex)),
    after = a3_after$blocks |> mutate(Row = paste(Country, Sex)),
    original = pub_a3_blocks |> mutate(Row = paste(Country, Sex)),
    table = "Table A3",
    cols = a3_scenarios
  ),
  check_values(
    before = a4_before$blocks |> mutate(Row = paste(Pair, "-", Country)),
    after = a4_after$blocks |> mutate(Row = paste(Pair, "-", Country)),
    original = pub_a4_blocks |> mutate(Row = paste(Pair, "-", Country)),
    table = "Table A4",
    cols = a3_scenarios
  )
)

check_summary <- check |>
  summarise(
    `Max |Before fix - Original|` = max(abs(`Before fix - Original`)),
    `Max |After fix - Original|` = max(abs(`After fix - Original`)),
    .by = Table
  )

# -- workbook ---------------------------------------------------------------

title_style <- createStyle(textDecoration = "bold", fontSize = 12)
header_style <- createStyle(textDecoration = "bold", fgFill = "#DCE6F1",
                            halign = "center", valign = "center",
                            wrapText = TRUE, border = "bottom",
                            borderColour = "#8EA9DB")
number_style <- createStyle(numFmt = "0.0", halign = "center")
text_style <- createStyle(halign = "left", valign = "top", wrapText = TRUE)
country_style <- createStyle(valign = "center", textDecoration = "bold")

write_sheet <- function(wb, sheet, title, note, tab, num_cols) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, title, startRow = 1)
  mergeCells(wb, sheet, rows = 1, cols = seq_len(ncol(tab)))
  addStyle(wb, sheet, title_style, rows = 1, cols = 1)

  if (!is.null(note)) {
    writeData(wb, sheet, note, startRow = 2)
    mergeCells(wb, sheet, rows = 2, cols = seq_len(ncol(tab)))
  }

  start <- 4
  writeData(wb, sheet, tab, startRow = start, keepNA = FALSE)
  addStyle(wb, sheet, header_style, rows = start, cols = seq_len(ncol(tab)),
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, number_style, rows = (start + 1):(start + nrow(tab)),
           cols = num_cols, gridExpand = TRUE, stack = TRUE)

  if (names(tab)[1] == "Country") {
    rows <- start + seq_len(nrow(tab))
    labels <- which(!is.na(tab$Country) & nzchar(tab$Country))

    for (row_idx in labels) {
      below <- row_idx + 1:2

      if (max(below) <= nrow(tab) && all(!is.na(tab$Country[below])) &&
          all(!nzchar(tab$Country[below]))) {
        mergeCells(wb, sheet, rows = rows[row_idx]:(rows[row_idx] + 2), cols = 1)
        addStyle(wb, sheet, country_style, rows = rows[row_idx], cols = 1)
      }
    }
  }

  setColWidths(wb, sheet, cols = seq_len(ncol(tab)), widths = "auto")
  freezePane(wb, sheet, firstActiveRow = start + 1)
  return(NULL)
}

wb <- createWorkbook()

write_sheet(
  wb, "Table 1 - original",
  paste("Table 1. Life expectancy in the African Union under selected",
        "SDG-related mortality reduction scenarios (2023) - original manuscript"),
  paste("Values as published in the original manuscript.",
        "The NCD female cell is 68.8 here; both reproductions give 68.9."),
  table_1_original, 3:5
)

write_sheet(
  wb, "Table 1 - before fix",
  paste("Table 1. Life expectancy in the African Union under selected",
        "SDG-related mortality reduction scenarios (2023) - before fix"),
  paste("Single-region pipeline; before and after fix are identical.",
        "The original manuscript differs only in the NCD female cell",
        "(68.8 original; 68.9 reproduced)."),
  table_1_before, 3:5
)

write_sheet(
  wb, "Table 1 - after fix",
  paste("Table 1. Life expectancy in the African Union under selected",
        "SDG-related mortality reduction scenarios (2023) - after fix"),
  paste("Reductions are percentages of cause-specific mortality.",
        "The under-five scenario applies a 54% reduction at ages 0, 1 and 2."),
  table_1, 3:5
)

write_sheet(
  wb, "Table A3 - original",
  paste("Table A3. Life expectancy gap between females and males with 30%",
        "reductions in specific causes of death (2023) - original manuscript"),
  paste("Values as published in the original manuscript.",
        "Neoplasms: lung, colon and rectum, other neoplasms.",
        "CVD: ischaemic heart disease, stroke, other cardiovascular.",
        "External: injuries, self-harm and violence."),
  table_a3_original, 3:7
)

write_sheet(
  wb, "Table A3 - before fix",
  paste("Table A3. Life expectancy gap between females and males with 30%",
        "reductions in specific causes of death (2023) - before fix"),
  paste("Pre-fix comparison pipeline: modify the COD table, then rebuild",
        "the life table with the same matrix; the 30% change is applied twice.",
        "Neoplasms: lung, colon and rectum, other neoplasms.",
        "CVD: ischaemic heart disease, stroke, other cardiovascular.",
        "External: injuries, self-harm and violence."),
  a3_before$table, 3:7
)

write_sheet(
  wb, "Table A3 - after fix",
  paste("Table A3. Life expectancy gap between females and males with 30%",
        "reductions in specific causes of death (2023) - after fix"),
  paste("Fixed pipeline: life table built from the original COD table;",
        "the 30% change is applied once.",
        "Neoplasms: lung, colon and rectum, other neoplasms.",
        "CVD: ischaemic heart disease, stroke, other cardiovascular.",
        "External: injuries, self-harm and violence."),
  table_a3, 3:7
)

write_sheet(
  wb, "Table A4 - original",
  paste("Table A4. Life expectancy gap between countries with 30%",
        "reductions in specific causes of death (2023) - original manuscript"),
  paste("Values as published in the original manuscript.",
        "Both sexes combined; cause groups as in Table A3.",
        "Difference is the first country minus the second."),
  table_a4_original, 2:6
)

write_sheet(
  wb, "Table A4 - before fix",
  paste("Table A4. Life expectancy gap between countries with 30%",
        "reductions in specific causes of death (2023) - before fix"),
  paste("Pre-fix comparison pipeline: modify the COD table, then rebuild",
        "the life table with the same matrix; the 30% change is applied twice.",
        "Both sexes combined; difference is the first country minus the second."),
  a4_before$table, 2:6
)

write_sheet(
  wb, "Table A4 - after fix",
  paste("Table A4. Life expectancy gap between countries with 30%",
        "reductions in specific causes of death (2023) - after fix"),
  paste("Fixed pipeline: life table built from the original COD table;",
        "the 30% change is applied once.",
        "Both sexes combined; difference is the first country minus the second."),
  table_a4, 2:6
)

addWorksheet(wb, "Check")
writeData(wb, "Check", "Original, before-fix and after-fix values with differences to the original", startRow = 1)
addStyle(wb, "Check", title_style, rows = 1, cols = 1)
writeData(wb, "Check", check_summary, startRow = 3)
addStyle(wb, "Check", header_style, rows = 3, cols = 1:3,
         gridExpand = TRUE, stack = TRUE)
start <- 3 + nrow(check_summary) + 2
writeData(wb, "Check", check, startRow = start)
addStyle(wb, "Check", header_style, rows = start, cols = seq_len(ncol(check)),
         gridExpand = TRUE, stack = TRUE)
addFilter(wb, "Check", row = start, cols = seq_len(ncol(check)))
setColWidths(wb, "Check", cols = seq_len(ncol(check)), widths = "auto")

notes <- tibble(
  Item   = c("Script", "Data", "Table 1", "Tables A3 and A4",
             "Versions", "Rounding", "Cause groups", "Reproduce"),
  Detail = c(
    paste0("docs/reproduce_tables.R. Package version: ", installed_version, "."),
    "Bundled GBD 2021+2023 tables from {lemur}; calendar year 2023.",
    paste("African Union, eight scenarios, single-region pipeline.",
          "Table 1 is not affected by the comparison-mode bug: before and",
          "after fix are identical. The original manuscript differs only",
          "in the NCD female cell (68.8 original; 68.9 reproduced).",
          "The 54% under-five reduction is the manuscript input."),
    paste("Before fix: modify the COD table, then rebuild the life table",
          "with the same matrix; the 30% change is applied twice.",
          "After fix: life table built from the original COD table;",
          "the 30% change is applied once."),
    paste("Each table has three sheets: original manuscript, before fix",
          "and after fix. The Check sheet lists the original, before-fix",
          "and after-fix values together with both differences to the",
          "original."),
    paste("Two decimals (app display), then half-up to one decimal.",
          "Difference rows are computed from the one-decimal values."),
    paste("Neoplasms = lung, colon and rectum, other neoplasms.",
          "CVD = ischaemic heart disease, stroke, other cardiovascular.",
          "External = injuries, self-harm and violence."),
    paste("Rscript docs/reproduce_tables.R [output.xlsx].",
          "Run R CMD INSTALL . first if DESCRIPTION changed.")
  )
)

addWorksheet(wb, "Notes")
writeData(wb, "Notes", "Reproduction notes", startRow = 1)
addStyle(wb, "Notes", title_style, rows = 1, cols = 1)
writeData(wb, "Notes", notes, startRow = 3)
addStyle(wb, "Notes", header_style, rows = 3, cols = 1:2,
         gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Notes", text_style, rows = 4:(3 + nrow(notes)), cols = 1:2,
         gridExpand = TRUE, stack = TRUE)
setColWidths(wb, "Notes", cols = 1, widths = 24)
setColWidths(wb, "Notes", cols = 2, widths = 110)
setRowHeights(wb, "Notes", rows = 4:(3 + nrow(notes)), heights = 58)

saveWorkbook(wb, out_file, overwrite = TRUE)

cat("Workbook written to:", normalizePath(out_file), "\n\n")
print(check_summary)

if (any(check_summary$`Max |Before fix - Original|` > 0 |
        check_summary$`Max |After fix - Original|` > 0)) {
  cat("\nCells that differ from the original manuscript:\n")
  print(
    check |>
      filter(`Before fix - Original` != 0 | `After fix - Original` != 0),
    row.names = FALSE
  )
} else {
  cat("\nAll before-fix and after-fix cells match the original at one decimal.\n")
}
