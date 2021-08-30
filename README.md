# Forecast Eval

https://delphi.cmu.edu/forecast-eval/
Epiforecast scoring and interactive evaluation tools.

## Steps to contribute
1. Create a new branch off of `dev`
2. Create a pull request into `dev`

Branch `main` is the production branch. Branch `dev` will be merged into main when a release is ready. See below for instructions on how to create a release.



**Note:** the easiest way to view and develop this project locally is to use RStudio and run the RShiny app from inside the IDE

<img width="1111" alt="Screen Shot 2021-08-30 at 10 56 59 AM" src="https://user-images.githubusercontent.com/14190352/131359925-3b460d21-b9aa-4a40-a691-cd705ab98431.png">

Alternatively, ...


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
git pull origin dev
git checkout -b release_v1.0 origin/dev
```
Update version number in DESCRIPTION FILE
```
git add .
git commit -m "Version 1.0 updates"
git tag -a v1.0 -m "Version 1.0"
git push origin release_v1.0
git push origin v1.0
```
Create a PR into `main`.  
After code is merged to `main`, perform cleanup by merging `main` into `dev` so that `dev` stays up to date.

# Code Structure
 - `workflows` contains the weekly data pipeline workflow action (`s3_upload_ec2.yml`) and the `main.yml` that runs on branch merge
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

## Note on Scoring Script

When updates are made in the evalcast package that affect the scoring script, the covidcast docker image must be rebuilt by kicking off the workflow here: https://github.com/cmu-delphi/covidcast-docker/actions/workflows/main.yml. Ensure that the changes in evalcast will be compatible with the dashboard and will not cause errors - when the scoring script is run on these changes the results show up automatically in prod.

## Perform Manual Rollback
This should only be performed if absolutely necessary.

1. Change [this forecasteval line](https://github.com/cmu-delphi/delphi-ansible-web/blob/main/vars.yml#L63) to point to the desired sha256 hash rather than `latest` tag. The hash tags can be found [here](https://github.com/orgs/cmu-delphi/packages/container/package/forecast-eval).
2. Create PR into `main` (tag Brian as reviewer and let him know). Changes will automatically propagate to prod.
3. When creating the next normal release, the hash tag will no longer automatically update to the `latest` tag. The change back to `latest` must be performed manually during the next release. 
