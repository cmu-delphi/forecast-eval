# docker image for setting up an R environment
FROM continuumio/miniconda

RUN conda install --name base --channel conda-forge mamba=*

ADD ./environment.yml .
RUN mamba env create -f environment.yml

ADD ./dependencies.R .
RUN conda run -n forecast-eval-env Rscript ./dependencies.R

ENV PATH /opt/conda/envs/forecast-eval-env/bin:$PATH