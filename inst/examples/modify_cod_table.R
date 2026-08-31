D <- data_gbd_cod() # cod data

# Select COD data
cod <- D[D$region == "Romania" & D$sex == "both" & D$period == 2021, ]
cod_change = -50

# Example 1:
# Modify all COD values by 50%. This is trivial and not really needed.
modify_cod_table(cod, cod_change = -50)


# Example 2:
# Change the first cod by 1%, second one with 2% and so on until 17%
modify_cod_table(cod, cod_change = 1:17)

# Example 3:
# Apply a specific change by cause and age
# Say, we want to decrease the cod's risk only between age 45 and 75
# with values between 24% and 40%.

# we have to build a matrix (AGES x CODs) to indicate the change for each
# combination -- here reducing mortality by 24%-40% between ages 45 and 75
M <- matrix(0, nrow = length(unique(cod$x)), ncol = length(unique(cod$cause_name)))
dimnames(M) <- list(unique(cod$x), unique(cod$cause_name))
M[rownames(M) %in% 45:75, ] <- seq(24, 40, length.out = length(unique(cod$cause_name)))
M

modify_cod_table(cod, cod_change = -M)
