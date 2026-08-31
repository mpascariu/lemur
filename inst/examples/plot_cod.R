D <- data_gbd_cod() # cod data
cod <- D[D$region == "Romania" & D$sex == "both" & D$period == 2021, ]
plot_cod(cod)
