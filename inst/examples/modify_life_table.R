
L <- data_gbd_lt()  # life tables
D <- data_gbd_cod() # cod data

# Select Life Table
lt <- L[L$region == "Romania" & L$sex == "both" & L$period == 2021, ]
# Select COD data
cod <- D[D$region == "Romania" & D$sex == "both" & D$period == 2021, ]
cod_change = -50

# Example 1:
# How does the life table modify if the cause-specific mortality is
# reduced by 50% (all ages, all causes of death)?
lt_reduced <- modify_life_table(lt, cod, cod_change = -50)
lt_reduced

# Example 2:
# Let's change the first cod by 1%, second one with 2% and so on until 17%
# Note, we are increasing death rates. This should result in a lower life
# expectancy.

unique(cod$cause_name) # we have 17 causes

lt_reduced2 <- modify_life_table(lt, cod, cod_change = 1:17)
lt_reduced2

# Example 3:
# Apply a specific change by cause and age
# Say, we want to decrease the cod's risk only between age 45 and 75
# with values between 24% and 40%.

# we have to build a matrix (AGES x CODs) to indicate the change for each
# combination -- here reducing mortality by 24%-40% between ages 45 and 75
M <- matrix(0, nrow = length(unique(cod$x)), ncol = length(unique(cod$cause_name)))
dimnames(M) <- list(unique(cod$x), unique(cod$cause_name))
M[rownames(M) %in% 45:75, ] <- seq(24, 40, length.out = length(unique(cod$cause_name)))

lt_reduced3 <- modify_life_table(lt, cod, cod_change = -M)
lt_reduced3
