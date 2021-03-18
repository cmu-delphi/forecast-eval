# Forecast Eval

Epiforecast scoring and interactive evaluation tools.

## Building

This project requires a recent version of gnu/make and docker.

Builds use a containerized R environment. See the `docker_build` directory for more details.

To build: 

```bash
> make build
```

To start `bash` shell in the docker container, which would let you start a R session:

```bash
> make start_repl
```

## Starting a local shiny server

To start a docker image of the shiny server locally:

```bash
> make start_dashboard
```

# Releasing
```
git checkout dev
git pull
git checkout -b release_v1.0 origin/dev
git add .
git commit -m "Version 1.0 updates"
git tag -a v1.0 -m "Version 1.0"
git push origin release_v1.0
git push origin v1.0
```
