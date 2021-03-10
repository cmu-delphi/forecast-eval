FROM rocker/shiny-verse

ADD docker_dashboard/shiny_server.conf /etc/shiny-server/shiny-server.conf
RUN install2.r plotly shinyjs tsibble viridis aws.s3

COPY dist/*rds /srv/shiny-server/
COPY dashboard/* /srv/shiny-server/
