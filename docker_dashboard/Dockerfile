FROM rocker/shiny-verse

ADD docker_dashboard/dependencies.R .
RUN Rscript ./dependencies.R

COPY dist/*rds /srv/shiny-server/
COPY dashboard/* /srv/shiny-server/
