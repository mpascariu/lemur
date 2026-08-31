#!/usr/bin/bash

apt install r-base-core

# data to csv -- the datasets are served by the package's accessor functions
# (data_gbd_cod() etc.), which read the .rds in inst/extdata/
Rscript -e "library(lemur); write.csv(lemur::data_gbd_cod(), file='data_gbd_cod.csv', row.names=F)"
Rscript -e "library(lemur); write.csv(lemur::data_gbd_lt(), file='data_gbd_lt.csv', row.names=F)"
Rscript -e "library(lemur); write.csv(lemur::data_gbd_sdg(), file='data_gbd_sdg.csv', row.names=F)"
