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

`main` is the production branch and shouldn't be directly modified. Pull requests should be based on and merged into `dev`. When enough changes have accumulated on `dev`, a release will be made to sync `main` with it.

This project requires a recent version of GNU `make` and docker.

The easiest way to view and develop this project locally is to run the Shiny app from RStudio:

<img width="1111" alt="RStudio Screen Shot with Run App button circled" src="https://user-images.githubusercontent.com/14190352/131359925-3b460d21-b9aa-4a40-a691-cd705ab98431.png">

This is the same as running

```R
shiny::runApp("<directory>")
```

in R. However, dashboard behavior can differ running locally versus running in a container (due to package versioning, packages that haven't been properly added to the container environment, etc), so the dashboard should be also tested in a container.

The dashboard can be run in a Docker container using `make`. See notes in the Makefile for workarounds if you don't have image repository access.

The pipeline can be run locally with the `Report/create_reports.R` script or in a container. See notes in the Makefile for workarounds if you don't have image repository access.

## Running the scoring pipeline

The scoring pipline use a containerized R environment. See the `docker_build` directory for more details.

The pipeline can be run locally with the `Report/create_reports.R` script or in a container via

```bash
> make score_forecast
```

See notes in the Makefile for workarounds if you don't have image repository access.

## Running the Shiny app

The dashboard can be run in a Docker container using

```bash
> make start_dashboard
```

See notes in the Makefile for workarounds if you don't have image repository access.

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
git push origin release_v<major>.<minor>.<patch>
git push origin v<major>.<minor>.<patch>
```
Create a PR into `main`. After the branch is merged to `main`, perform cleanup by merging `main` into `dev` so that `dev` stays up to date.

## Dependencies

The scoring pipeline runs in a docker container built from https://github.com/cmu-delphi/forecast-eval/blob/f12ab6f303ba81d6cbc32d61720061474496a00f/docker_build/Dockerfile, which is a straight copy of https://github.com/cmu-delphi/covidcast-docker/blob/c5adf4bd088268398d574fc0658c8ac70953f91d/docker/Dockerfile. The dashboard runs in a docker container built from https://github.com/cmu-delphi/forecast-eval/blob/f12ab6f303ba81d6cbc32d61720061474496a00f/devops/Dockerfile.

When updates are made in the `evalcast` package the behavior of the scoring script can be affected and the `covidcast` docker image must be rebuilt. The [workflow in the `covidcast-docker` repository](https://github.com/cmu-delphi/covidcast-docker/blob/c5adf4bd088268398d574fc0658c8ac70953f91d/.github/workflows/main.yml) that does this needs to be triggered manually. Before building the new image, ensure that the changes in `evalcast` will be compatible with the scoring pipeline.

Currently, the scoring pipeline uses the the [`evalcast` package](https://github.com/cmu-delphi/covidcast/tree/evalcast/R-packages/evalcast) from [the`evalcast` branch](https://github.com/cmu-delphi/covidcast-docker/blob/c5adf4bd088268398d574fc0658c8ac70953f91d/docker/dependencies.R#L18) of the `covidcast` repository. However, if we need to make forecast eval-specific changes to the `evalcast` package that would conflict with other use cases, we have in the past created a dedicated forecast-eval branch of `evalcast`.

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
 - `.github`
   - `workflows` contains GitHub Actions workflow files
     - `ci.yml` runs linting on branch merge. Also builds new Docker images and pushes to the image repo for the `main` and `dev` branches
     - `create_release.yml` triggered manually to merge `dev` into `main`. Increments app version number, and creates PR into `main` and tags reviewer (currently Katie).
     - `release_main.yml` runs on merge of release branch. Creates tagged release using `release-drafter.yml` and merges updated `main` back into `dev` to keep them in sync.
     - `s3_upload_ec2.yml` runs the weekly self-hosted data pipeline workflow action (preceded by `s3_upload.yml` that ran the pipeline on a GitHub-provided VM)
   - `release-drafter.yml` creates a release
 - `Report` contains the code for fetching, scoring, and uploading forecasts. Runs 3 times a week
 - `app` contains all the code for the Shiny dashboard
   - `R` contains supporting R functions
     - `data.R` defines data-fetching functions
     - `data_manipulation.R` defines various filter functions
     - `delphiLayout.R` defines dashboard main and sub- UIs
     - `exportScores.R` contains tools to support the score CSV download tool included in the dashboard
   - `assets` contains supporting Markdown text. `about.md` contains the code for the "About" tab in the dasboard; other .md files contain explanations of the scores and other text info that appears in the app.
   - `www` contains CSS stylesheets and the logo images
   - `ui.R` sets up the UI for the dashboard, and defines starting values for selectors
   - `server.R` defines dashboard behavior. This is where the logic for the dashboard lives.
   - `global.R` defines constants and helper functions
 - `docker_buid` contains the Docker build configuration for the scoring pipeline
 - `devops` contains the Docker build configuration for the Shiny dashboard
   - ***Note: when adding a new package dependency to the app, it must be specified in this Dockerfile***
 - `DESCRIPTION` summarizes package information, such as contributors, version, and dependencies
 - `Makefile` contains commands to build and run the dashboard, and score and upload the data
