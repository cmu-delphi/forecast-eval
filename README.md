# Forecast Eval

The forecast evaluation dashboard provides a robust set of tools and methods for evaluating the performance of epidemic forecasts. The project's goal is to help epidemiological researchers gain insights into the performance of their forecasts and lead to more accurate epidemic forecasting.

## Background

This app collects and scores COVID-19 forecasts submitted to the CDC. The dashboard was developed by [CMU Delphi](https://delphi.cmu.edu) in collaboration with the [Reich Lab](https://reichlab.io) and US [COVID-19 Forecast Hub](https://covid19forecasthub.org/) from UMass-Amherst, as part of the Forecast Evaluation Research Collaborative.

The Reich Lab created and maintains the [COVID-19 Forecast Hub](https://covid19forecasthub.org/), a collaborative effort with over 80 groups submitting forecasts to be part of the official [CDC COVID-19 ensemble forecast](https://www.cdc.gov/coronavirus/2019-ncov/covid-data/mathematical-modeling.html). All Forecase Hub forecasters that are designated "primary" or "secondary" are scored and included in the dashboard.

The Delphi Group created and maintains COVIDcast, a platform for [epidemiological surveillance data](https://delphi.cmu.edu/covidcast/). COVIDcast provides the ground truth data used to score forecasts against.

The [public version of the dashboard](https://delphi.cmu.edu/forecast-eval/) runs off of the `main` branch.

The version on the `dev` branch appears on the [staging website](https://staging.delphi.cmu.edu/forecast-eval/). The username and password are included in the [meeting notes doc](https://docs.google.com/document/d/1q8sKrbjzymEDsWQ9mUomOZ255-_5W6RPGgTdFlHmpmE/edit#bookmark=id.xqskfsdd2w4q) and [on Slack](https://delphi-org.slack.com/archives/C01H63T0QE7/p1682012756484679).

The dashboard is backed by the forecast evaluation pipeline. The pipeline runs three times a week, on Sunday, Monday, and Tuesday, using the code on the `dev` branch. It collects and scores forecasts from the Forecast Hub, and posts the resulting files to a publicly-accessible [AWS S3 bucket](https://forecast-eval.s3.us-east-2.amazonaws.com/).

See the ["About" writeup](https://github.com/cmu-delphi/forecast-eval/blob/f12ab6f303ba81d6cbc32d61720061474496a00f/app/assets/about.md) for more information about the data and processing steps.

# Contributing

1. Create a new branch off of `dev`
2. Create a pull request into `dev`

**Note:** the easiest way to view and develop this project locally is to use RStudio and run the RShiny app from inside the IDE

<img width="1111" alt="Screen Shot 2021-08-30 at 10 56 59 AM" src="https://user-images.githubusercontent.com/14190352/131359925-3b460d21-b9aa-4a40-a691-cd705ab98431.png">

Alternatively, ...


## Building

This project requires a recent version of GNU make and docker.

Builds use a containerized R environment. See the `docker_build` directory for more details.

To build: 

```bash
> make build
```

To start `bash` shell in the docker container, which would let you start an R session:

```bash
> make start_repl
```

## Starting a local shiny server

To start a docker image of the shiny server locally:

```bash
> make start_dashboard
```

# Releasing

`main` is the production branch and contains the code that the public dashboard uses. Code changes will accumulate on the `dev` branch and when we want to make a release, `dev` will be merged into `main` via the ["Create Release" workflow](https://github.com/cmu-delphi/forecast-eval/blob/f12ab6f303ba81d6cbc32d61720061474496a00f/.github/workflows/create_release.yml). Version bump type (major, minor, etc) is specified manually when running the action.

If there's some issue with the workflow-based release process, a release can be done manually with:
```bash
git checkout dev
git pull origin dev
git checkout -b release_v<major>.<minor>.<patch> origin/dev
```
Update version number in the [DESCRIPTION file](https://github.com/cmu-delphi/forecast-eval/blob/f12ab6f303ba81d6cbc32d61720061474496a00f/DESCRIPTION) and in the [dashboard](https://github.com/cmu-delphi/forecast-eval/blob/f12ab6f303ba81d6cbc32d61720061474496a00f/app/global.R#L13).
```bash
git add .
git commit -m "Version <major>.<minor>.<patch> updates"
git tag -a v<major>.<minor>.<patch> -m "Version <major>.<minor>.<patch>"
git push origin release_v<major>.<minor>.patch><
git push origin v<major>.<minor>.patch><
```
Create a PR into `main`. After the branch is merged to `main`, perform cleanup by merging `main` into `dev` so that `dev` stays up to date.

## Dependencies

The scoring pipeline runs in a docker container built from https://github.com/cmu-delphi/forecast-eval/blob/f12ab6f303ba81d6cbc32d61720061474496a00f/docker_build/Dockerfile, which is a straight copy of https://github.com/cmu-delphi/covidcast-docker/blob/c5adf4bd088268398d574fc0658c8ac70953f91d/docker/Dockerfile. The dashboard runs in a docker container built from https://github.com/cmu-delphi/forecast-eval/blob/f12ab6f303ba81d6cbc32d61720061474496a00f/devops/Dockerfile.

When updates are made in the `evalcast` package the behavior of the scoring script can be affected and the `covidcast` docker image must be rebuilt. The [workflow in the `covidcast-docker` repository](https://github.com/cmu-delphi/covidcast-docker/blob/c5adf4bd088268398d574fc0658c8ac70953f91d/.github/workflows/main.yml) that does this needs to be triggered manually. Before building the new image, ensure that the changes in `evalcast` will be compatible with the scoring pipeline.

Currently, the scoring pipeline [uses the `evalcast` branch](https://github.com/cmu-delphi/covidcast-docker/blob/c5adf4bd088268398d574fc0658c8ac70953f91d/docker/dependencies.R#L18) of the [`evalcast` package](https://github.com/cmu-delphi/covidcast/tree/evalcast/R-packages/evalcast). However, if we need to make forecast eval-specific changes to the `evalcast` package that would conflict with other use cases, we have in the past created a dedicated branch of `evalcast`.

## Performing a manual rollback

### For the dashboard
This should only be performed if absolutely necessary.

1. Change [this `forecasteval` line](https://github.com/cmu-delphi/delphi-ansible-web/blob/05d42535187a736ea997f42cb4c23706a762d9bc/vars.yml#L77) to point to the desired (most recently working) sha256 hash rather than the `latest` tag. The hashes can be found in [the Delphi ghcr.io image repository](https://github.com/orgs/cmu-delphi/packages/container/package/forecast-eval) -- these require special permissions to view. Ask Brian for permissions, ask Nat for hash info.
2. Create a PR into `main`. Tag Brian as reviewer and let him know over Slack. Changes will automatically propagate to production once merged.
3. When creating the next normal release, code changes will no longer automatically propagate via the `latest` image to the public dashboard; the tag in the `ansible` settings file must be manually changed back to `latest`.

### For the pipeline

1. Change the `FROM` line in the `docker_build` Dockerfile to point to the most recently working sha256 hash rather than the `latest` tag. The hashes can be found in [the Delphi ghcr.io image repository](https://github.com/orgs/cmu-delphi/packages/container/package/covidcast) -- these require special permissions to view. Ask Brian for permissions, ask Nat for hash info.
2. Create a PR into `dev`. Tag Katie or Nat as reviewer and let them know over Slack. Changes will automatically propagate to production once merged.
3. When building the next `covidcast` docker image, changes will no longer automatically propagate via the `latest` `covidcast` image to the local pipeline image; the tag in `docker_build/Dockerfile` must be manually changed back to `latest`.

# Code Structure
 - `.github/workflows` contains the weekly data pipeline workflow action (`s3_upload_ec2.yml`) and the `main.yml` that runs on branch merge
 - `Report` contains the scoring and data upload scripts that run weekly
 - `dashboard` contains all the code for the RShiny dashboard
   - `www` contains the styling and the assets
   - `app.R` is the main RShiny file with the UI and server functions
   - `common.R` is for code shared between the app and the download feature
   - `export_scores.R` contains the code for the download feature
   - `about.md` contains the code for the "About" tab in the dasboard (other .md files contain explanations of the scores and other text info that appears in the app)
 - `docker_buid` contains the `Dockerfile` specifying the version of the `covidcast` docker image to use
 - `docker_dashboard` contains the `Dockerfile` and `shiny_server.conf` for the RShiny app
   - ***Note: when adding a new package dependency to the app, it must be specified in this Dockerfile***
 - `DESCRIPTION` is where the version number is updated for each release
 - `Makefile` contains all commands to build and run the dashboard and score and upload the data
