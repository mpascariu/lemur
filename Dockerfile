FROM rocker/rstudio:4.6.1
RUN apt-get update && apt-get install -y  cmake gdal-bin libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libicu-dev libpng-dev libpq-dev libproj-dev libssl-dev libudunits2-dev libuv1-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
# Pinned dependency versions mirror the minimums declared in DESCRIPTION --
# update both together when imports change.
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.2.3")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.9.1")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.18.6.1")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.2.2")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.3.2")'
RUN Rscript -e 'remotes::install_version("plogr",upgrade="never", version = "0.2.0")'
RUN Rscript -e 'remotes::install_version("RPostgres",upgrade="never", version = "1.4.10")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.4")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.8.7")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never", version = "1.0.4")'
RUN Rscript -e 'remotes::install_version("markdown",upgrade="never", version = "1.13")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "1.0-17")'
RUN Rscript -e 'remotes::install_version("leaflet.extras",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.33")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
#EXPOSE 3838
#CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');library(lemur);lemur::run_app()"]
