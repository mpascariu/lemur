# cod data
D <- data_gbd_cod()
# Select COD data for 1 region
cod <- D[D$region == "Romania" & D$sex == "both" & D$period == 1990, ]
# COD data in matrix format

build_cod_matrix(cod)
