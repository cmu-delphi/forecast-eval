.DEFAULT_GOAL:=build
S3_URL=https://forecast-eval.s3.us-east-2.amazonaws.com

build: score_forecast

r_build:
	docker build -t forecast-eval-build docker_build

predictions_cards.rds score_cards_state_deaths.rds score_cards_state_cases.rds :
	test -f dist/$@ || wget --directory-prefix=dist $(S3_URL)/$@

pull_data: predictions_cards.rds score_cards_state_deaths.rds score_cards_state_cases.rds

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

start_repl: r_build
	docker run -ti --rm \
		-v ${PWD}/Report:/var/forecast-eval \
		-v ${PWD}/dist:/var/dist \
		-w /var/forecast-eval \
		forecast-eval-build bash

start_dashboard:
	cp Report/*.rds dashboard
	docker run --rm -p 3838:3838 -v ${PWD}/dashboard:/srv/shiny-server rocker/shiny-verse
