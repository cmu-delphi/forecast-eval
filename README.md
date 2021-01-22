# Forecast Eval

Epiforecast scoring and interactive evaluation tools.

## Building

This project requires a recent version of node/npm and docker.

Builds use a containerized R environment. See the `docker_build` directory for more details.

To build first run

```bash
> npm run build-docker
```

This will build the R docker image, which will take a while to run the first time it runs.

Next run

```bash
> npm run build
```

Which will invoke `csv2rds.R` inside the container and convert it to `rds`.  This is a temporary "hello world" while as we work torward runing the scoring and report generation inside of the container as well.