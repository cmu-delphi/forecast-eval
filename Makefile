SHELL:=/bin/bash
PWD=$(shell pwd)

.DEFAULT_GOAL:=build
S3_URL=https://forecast-eval.s3.us-east-2.amazonaws.com
S3_BUCKET=s3://forecast-eval
# If not already set in calling environment, set PAT to an empty value.
GITHUB_PAT?=

# Change `imageTag` during `make` call via `make <command> imageTag=<tag name>`
#
# `imageTag` specifies the tag to be used for the production dashboard Docker
# image. If building from `main`, it should be `latest`. If building from
# `dev`, it should be `dev`. The default value used here is meant to prevent
# the actual `latest` and `dev` images in the image repository from being
# accidentally overwritten.
imageTag=local

build: build_dashboard

# Build a docker image suitable for running the scoring pipeline
#
# `docker_build/Dockerfile` is based on `ghcr.io/cmu-delphi/covidcast:latest`.
# Docker will try to fetch it from the image repository, which requires
# authentication. As a workaround, locally build a docker image
# from https://github.com/cmu-delphi/covidcast-docker/ using the `make build`
# target, and set `--pull=false` below.
r_build:
	docker build --no-cache --force-rm --pull -t forecast-eval-build docker_build

# Download the named file from the AWS S3 bucket
%.csv.gz: dist
	test -f dist/$@ || curl -o dist/$@ $(S3_URL)/$@

# Specify all the data files we want to download
pull_data: score_cards_state_deaths.csv.gz score_cards_state_cases.csv.gz score_cards_nation_cases.csv.gz score_cards_nation_deaths.csv.gz score_cards_state_hospitalizations.csv.gz score_cards_nation_hospitalizations.csv.gz datetime_created_utc.csv.gz

# Download all the predictions cards objects. This is
# useful for development and debugging
pull_pred_cards: predictions_cards_confirmed_admissions_covid_1d.csv.gz predictions_cards_confirmed_incidence_num.csv.gz predictions_cards_deaths_incidence_num.csv.gz

# Create the dist directory
dist:
	mkdir $@

# Remove the dist directory
clean:
	rm -rf dist

# Run the scoring pipeline in a docker container
score_forecast: r_build dist pull_data
	docker run --rm \
		-v ${PWD}/Report:/var/forecast-eval \
		-v ${PWD}/dist:/var/dist \
		-w /var/forecast-eval \
		-e GITHUB_PAT \
		forecast-eval-build \
		Rscript create_reports.R --dir /var/dist

# Post scoring pipeline output files to the AWS S3 bucket
deploy: score_forecast
	aws s3 cp dist/ $(S3_BUCKET)/ --recursive --exclude "*" --include "*csv.gz" --acl public-read

# Run bash in a docker container with a full preconfigured R environment
#
# If `--pull=always`, docker will try to fetch the
# `ghcr.io/cmu-delphi/forecast-eval:latest` image from the image repository,
# which requires authentication. As a workaround, set `--pull=never`below.
# The dependency `r-build` will creat the necessary image if you have a local
# version of `ghcr.io/cmu-delphi/covidcast:latest` available. See the `r-build`
# notes for details.
start_dev: r_build
	docker run --pull=always -ti --rm \
		-v ${PWD}/Report:/var/forecast-eval \
		-v ${PWD}/app:/var/forecast-eval-dashboard \
		-v ${PWD}/dist:/var/dist \
		-w /var/forecast-eval \
		ghcr.io/cmu-delphi/forecast-eval:latest bash

# Build a docker image for local use
build_dashboard_dev: pull_data
	docker build --no-cache --pull -t ghcr.io/cmu-delphi/forecast-eval:latest -f devops/Dockerfile .

# Run a local version of the dashboard in a docker container
start_dashboard: build_dashboard_dev
	docker run --rm -p 3838:80 ghcr.io/cmu-delphi/forecast-eval:latest

# Build a docker image for production use. Currently this isn't used anywhere,
# but could be useful if we need to manually build a docker image for
# production.
build_dashboard: pull_data
	docker build --no-cache=true --pull -t ghcr.io/cmu-delphi/forecast-eval:$(imageTag) -f devops/Dockerfile .

# Push a production docker image to the image repository. Currently this isn't
# used anywhere, but could be useful if we need to manually release a docker
# image for production.
deploy_dashboard: build_dashboard
	docker push ghcr.io/cmu-delphi/forecast-eval:$(imageTag)
