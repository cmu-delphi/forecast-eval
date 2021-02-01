.DEFAULT_GOAL:=build

build: score_forecast

r_build:
	docker build -t forecast-eval-build docker_build

dist:
	mkdir $@

clean:
	rm -rf dist
	rm dashboard/*.rds

score_forecast: r_build dist
	docker run --rm -v ${PWD}:/var/forecast-eval -w /var/forecast-eval forecast-eval-build Rscript Report/create_reports.R

start_repl: r_build
	docker run -ti --rm forecast-eval-build bash

start_dashboard:
	cp Report/*.rds dashboard
	docker run --rm -p 3838:3838 -v ${PWD}/dashboard:/srv/shiny-server rocker/shiny-verse
