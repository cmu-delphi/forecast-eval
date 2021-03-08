FROM rocker/shiny-verse

ADD docker_dashboard/dependencies.R .
ADD docker_dashboard/shiny_server.conf .
RUN Rscript ./dependencies.R

COPY dist/*rds /srv/shiny-server/
COPY dashboard/* /srv/shiny-server/
