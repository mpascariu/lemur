# Data
L <- data_gbd_lt()

# Select Life Table 1 & 2
region1 = "Romania"
region2 = "Mexico"
sex_sel = "female"
year    = 2021

L1 <- L[L$region == region1 & L$sex == sex_sel & L$period == year, ]
L2 <- L[L$region == region2 & L$sex == sex_sel & L$period == year, ]

# Age decomposition
dec <- decompose_by_age(L1, L2)
dec

plot_decompose(dec)
