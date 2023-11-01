FROM rocker/verse:4.3.0
RUN apt-get update && apt-get install -y  gdal-bin libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libicu-dev libpng-dev libpq-dev libproj-dev libssl-dev libudunits2-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.2.1")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.5")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.4.2")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.4.1")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.14.8")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.2.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.10")'
RUN Rscript -e 'remotes::install_version("RPostgres",upgrade="never", version = "1.4.5")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.2")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.8.0")'
RUN Rscript -e 'remotes::install_version("shinyBS",upgrade="never", version = "0.61.1")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("MortalityLaws",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "1.0-14")'
RUN Rscript -e 'remotes::install_version("leaflet.extras",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.30")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');library(lemur);lemur::run_app()"]
