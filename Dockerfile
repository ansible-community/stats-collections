# Base image
FROM rocker/r-ver:4.0.2

# Deps
RUN apt-get update && apt-get install -y git-core libcurl4-openssl-dev libgit2-dev libssh2-1-dev libssl-dev libsasl2-dev libxml2-dev make pkg-config pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*

# Setup
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("renv")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

# Packages
RUN R -e 'install.packages("stringi", force = TRUE)' #why.t.f is this needed?
RUN R -e 'renv::restore()'

# Go!
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');ansible.collections::run_app()"]
