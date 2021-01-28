.DEFAULT_GOAL:=build

build: score_forecast

r_build:
	docker build -t forecast-eval-build docker_build

dist:
	mkdir $@

clean:
	rm -rf dist

score_forecast: r_build dist
	docker run -v ${PWD}/Report:/var/forecast-eval -w /var/forecast-eval forecast-eval-build Rscript create_reports.R

start_dashboard:
	docker run --rm -p 3838:3838 -v ${PWD}/dashboard:/srv/shiny-server rocker/shiny-verse
