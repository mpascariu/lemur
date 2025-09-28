#!/usr/bin/bash

apt install r-base-core

# data to csv
Rscript -e "load('../../data/data_gbd2021_cod.rda'); write.csv(data_gbd2021_cod, file='data_gbd2021_cod.csv', row.names=F)"
Rscript -e "load('../../data/data_gbd2021_lt.rda'); write.csv(data_gbd2021_lt, file='data_gbd2021_lt.csv', row.names=F)"
Rscript -e "load('../../data/data_gbd2021_sdg.rda'); write.csv(data_gbd2021_sdg, file='data_gbd2021_sdg.csv', row.names=F)"
