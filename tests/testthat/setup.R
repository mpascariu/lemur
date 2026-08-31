# Shared fixtures for all lemur tests.
#
# Load the package data once per session and slice it down to small,
# deterministic subsets so that every test runs on a tiny fraction of the
# full (multi-million-row) datasets. Slices are single region / single sex /
# single year where the test only needs one population, and pair-of-regions
# where a comparison is required.

L_full <- lemur::data_gbd_lt()
D_full <- lemur::data_gbd_cod()

# Single population: Romania, both sexes, 2021 (22 age groups / 18 causes)
L_romania <- L_full[L_full$region == "Romania" & L_full$sex == "both" & L_full$period == 2021, ]
D_romania <- D_full[D_full$region == "Romania" & D_full$sex == "both" & D_full$period == 2021, ]

# Second population for comparisons: Mexico, both sexes, 2021
L_mexico <- L_full[L_full$region == "Mexico" & L_full$sex == "both" & L_full$period == 2021, ]
D_mexico <- D_full[D_full$region == "Mexico" & D_full$sex == "both" & D_full$period == 2021, ]

# Sex comparison within one region
L_ro_female <- L_full[L_full$region == "Romania" & L_full$sex == "female" & L_full$period == 2021, ]
