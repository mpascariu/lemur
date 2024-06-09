# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T);

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# package documentation
devtools::document()

# dockerfile
# see https://rocker-project.org/images/ for other Rocker images
golem::add_dockerfile_shinyproxy(
  paste0("rocker/rstudio:", R.Version()$major, ".", R.Version()$minor))

# in Terminal
# 1. place yourself in the folder where your docker file is created
# 2. check that you have docker installed: docker --version
# 3. create the image: docker build -t lemurapp:v3 .
# 4. after 15min when all is ready: docker image ls
# 5. test image locally: docker run lemurapp:v3

# > options('shiny.port'=3838,shiny.host='0.0.0.0');library(lemur);lemur::run_app()
# Loading required package: shiny
# 
# Listening on http://0.0.0.0:3838
# sh: 1: xdg-open: not found.          ???????????


# restart R
rstudioapi::restartSession()

# install from source
install.packages(getwd(), repo=NULL, type='source')

# run app
library(lemur)
lemur::run_app()


