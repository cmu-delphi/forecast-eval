# Forecast Eval

Epiforecast scoring and interactive evaluation tools.

## Building

This project requires a recent version of gnu/make and docker.

Builds use a containerized R environment. See the `docker_build` directory for more details.

To build: 

```bash
> make build
```

## Starting a local shiny server

To start a docker image of the shiny server locally:

```bash
> make start_dashboard
```