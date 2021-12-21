# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T);

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# package documentation
devtools::document()

# dockerfile
golem::add_dockerfile_shinyproxy()

# restart R
rstudioapi::restartSession()

# install from source
install.packages(getwd(), repo=NULL, type='source')

# run app
library(lemur)
lemur::run_app()


