#!/usr/bin/bash

apt install r-base-core

# data to csv
Rscript -e "load('../../data/data_gbd2019_cod.rda'); write.csv(data_gbd2019_cod, file='data_gbd2019_cod.csv', row.names=F)"
Rscript -e "load('../../data/data_gbd2019_lt.rda'); write.csv(data_gbd2019_lt, file='data_gbd2019_lt.csv', row.names=F)"
Rscript -e "load('../../data/data_gbd2019_sdg.rda'); write.csv(data_gbd2019_sdg, file='data_gbd2019_sdg.csv', row.names=F)"
