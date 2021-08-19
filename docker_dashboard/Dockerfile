FROM rocker/shiny-verse
LABEL org.opencontainers.image.source = "https://github.com/cmu-delphi/forecast-eval"


ADD docker_dashboard/shiny_server.conf /etc/shiny-server/shiny-server.conf
RUN install2.r plotly shinyjs tsibble viridis aws.s3 covidcast

COPY dist/*rds /srv/shiny-server/
COPY dashboard/* /srv/shiny-server/
COPY dashboard/www/*png /srv/shiny-server/www/
COPY dist/*css /srv/shiny-server/www/


