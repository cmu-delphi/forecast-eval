FROM rocker/shiny-verse
LABEL org.opencontainers.image.source = "https://github.com/cmu-delphi/forecast-eval"
RUN apt-get update && apt-get install -qq -y \
    libgdal-dev \
    libudunits2-dev && \
    rm -rf /var/cache/apt && \
    apt-get clean

COPY devops/shiny_server.conf /etc/shiny-server/shiny-server.conf
WORKDIR /srv/shinyapp/
COPY DESCRIPTION ./
RUN Rscript -e "devtools::install_deps(dependencies = NA)"
COPY dist/*.rds ./
COPY app/ ./
RUN chmod -R a+rw .
