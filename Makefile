SHELL:=/bin/bash
PWD=$(shell pwd)

.DEFAULT_GOAL:=build
S3_URL=https://forecast-eval.s3.us-east-2.amazonaws.com
S3_BUCKET=s3://forecast-eval

build: build_dashboard

# Build a docker image suitable for running the scoring pipeline
#
# `docker_build/Dockerfile` is based on `ghcr.io/cmu-delphi/covidcast:latest`.
# Docker will try to fetch it from the image repository, which requires
# authentication. As a workaround, locally build a docker image with the same
# name and set `--pull=never`.
r_build:
	docker build --no-cache --force-rm --pull -t forecast-eval-build docker_build

# Download the named file from the AWS S3 bucket
%.rds: dist
	test -f dist/$@ || curl -o dist/$@ $(S3_URL)/$@

# Specify all the data files we want to download
pull_data: score_cards_state_deaths.rds score_cards_state_cases.rds score_cards_nation_cases.rds score_cards_nation_deaths.rds score_cards_state_hospitalizations.rds score_cards_nation_hospitalizations.rds datetime_created_utc.rds predictions_cards.rds

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
		forecast-eval-build \
		Rscript create_reports.R --dir /var/dist

# Post scoring pipeline output files to the AWS S3 bucket
deploy: score_forecast
	aws s3 cp dist/ $(S3_BUCKET)/ --recursive --exclude "*" --include "*rds" --acl public-read

# Run bash in a docker container with a full preconfigured R environment
#
# If `--pull=always`, docker will try to fetch the
# `ghcr.io/cmu-delphi/forecast-eval:latest` image from the image repository,
# which requires authentication. As a workaround, locally build a docker
# image with the same name and set `--pull=never`.
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

# Build a docker image for production use
build_dashboard: pull_data
	docker build --no-cache=true --pull -t ghcr.io/cmu-delphi/forecast-eval:$(imageTag) -f devops/Dockerfile .

# Push a production docker image to the image repository
deploy_dashboard: build_dashboard
	docker push ghcr.io/cmu-delphi/forecast-eval:$(imageTag)
