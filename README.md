# Forecast Eval

Epiforecast scoring and interactive evaluation tools.

## Steps to contribute
1. Create a new branch off of `dev`
2. Create a pull request into `dev`

Branch `main` is the production branch. Branch `dev` will be merged into main when a release is ready.


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

## Note on Scoring Script

When updates are made in the evalcast package that affect the scoring script, the covidcast docker image must be rebuilt by kicking off the workflow here: https://github.com/cmu-delphi/covidcast-docker/actions/workflows/main.yml. Ensure that the changes in evalcast will be compatible with the dashboard and will not cause errors - when the scoring script is run on these changes the results show up automatically in prod.

## Perform Manual Rollback
This should only be performed if absolutely necessary.

1. Change [this forecasteval line](https://github.com/cmu-delphi/delphi-ansible-web/blob/main/vars.yml#L63) to point to the desired sha256 hash rather than `latest` tag. The hash tags can be found [here](https://github.com/orgs/cmu-delphi/packages/container/package/forecast-eval).
2. Create PR into `main` (tag Brian as reviewer and let him know). Changes will automatically propagate to prod.
3. When creating the next normal release, the hash tag will no longer automatically update to the `latest` tag. The change back to `latest` must be performed manually during the next release. 
