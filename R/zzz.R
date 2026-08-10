# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Mon Aug 10 2026
# ------------------------------------------------- #

.onLoad <- function(libname, pkgname) {
  # Make the package's lazy data objects available in the namespace.
  #
  # R only binds data/ objects into the package namespace on attach
  # (library(pkg)). A bare reference such as data_app_input$regions inside a
  # package function therefore fails with "object 'data_app_input' not found"
  # when the package is used as pkg::fun() without being attached (e.g.
  # lemur::run_app() in a fresh session). Loading the data into the namespace
  # at load time fixes that for every call site, attached or not.
  utils::data("data_app_input", package = pkgname, envir = asNamespace(pkgname))
}
