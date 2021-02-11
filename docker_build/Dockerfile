# docker image for setting up an R environment
FROM rocker/tidyverse

ADD ./dependencies.R .
RUN Rscript ./dependencies.R