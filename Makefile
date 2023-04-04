.DEFAULT_GOAL:=build
S3_URL=https://forecast-eval.s3.us-east-2.amazonaws.com
S3_BUCKET=s3://forecast-eval

build: build_dashboard

r_build:
	docker build --no-cache --force-rm --pull -t forecast-eval-build docker_build

%.rds: dist
	test -f dist/$@ || curl -o dist/$@ $(S3_URL)/$@

pull_data: score_cards_state_deaths.rds score_cards_state_cases.rds score_cards_nation_cases.rds score_cards_nation_deaths.rds score_cards_state_hospitalizations.rds score_cards_nation_hospitalizations.rds datetime_created_utc.rds

dist:
	mkdir $@

clean:
	rm -rf dist

score_forecast: r_build dist pull_data
	docker run --rm \
		-v ${PWD}/Report:/var/forecast-eval \
		-v ${PWD}/dist:/var/dist \
		-w /var/forecast-eval \
		forecast-eval-build \
		Rscript create_reports.R --dir /var/dist

deploy: score_forecast
	aws s3 cp dist/ $(S3_BUCKET)/ --recursive --exclude "*" --include "*rds" --acl public-read

# Starts a docker image with a full preconfigured R environment
start_dev: r_build
	docker run --pull=always -ti --rm \
		-v ${PWD}/Report:/var/forecast-eval \
		-v ${PWD}/app:/var/forecast-eval-dashboard \
		-v ${PWD}/dist:/var/dist \
		-w /var/forecast-eval \
		ghcr.io/cmu-delphi/forecast-eval:latest bash

build_dashboard_dev: pull_data
	docker build --no-cache --pull -t ghcr.io/cmu-delphi/forecast-eval:latest -f devops/Dockerfile .

build_dashboard: pull_data
	docker build --no-cache=true --pull -t ghcr.io/cmu-delphi/forecast-eval:$(imageTag) -f devops/Dockerfile .

deploy_dashboard: build_dashboard
	docker push ghcr.io/cmu-delphi/forecast-eval:$(imageTag)

start_dashboard: build_dashboard_dev
	docker run --rm -p 3838:80 ghcr.io/cmu-delphi/forecast-eval:latest
