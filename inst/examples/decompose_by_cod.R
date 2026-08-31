L <- data_gbd_lt()  # life tables
D <- data_gbd_cod() # cod data

# Select two Life Tables
region1 = "Romania"
region2 = "Mexico"
sex_sel = "male"
year    = 2021

lt1 <- L[L$region == region1 & L$sex == sex_sel & L$period == year, ]
lt2 <- L[L$region == region2 & L$sex == sex_sel & L$period == year, ]

# Select COD corresponding data
cod1 <- D[D$region == region1 & D$sex == sex_sel & D$period == year, ]
cod2 <- D[D$region == region2 & D$sex == sex_sel & D$period == year, ]

## Example of decomposition by age and cause of death
dec  <- decompose_by_cod(L1 = lt1,
                         L2 = lt2,
                         C1 = cod1,
                         C2 = cod2)

dec

plot_decompose(dec)
