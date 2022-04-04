#!/usr/bin/env Rscript
library("optparse")
library("dplyr")
library("evalcast")
library("lubridate")
library("jsonlite")
library("httr")


# Set up GitHub API authentication
## TODO: setup for Github Actions, so can pass token info into script
uname <- ""
token <- ""


# TODO: Contains fixed versions of WIS component metrics, to be ported over to evalcast
# Redefines overprediction, underprediction and sharpness
source("error_measures.R")
source("score.R")
source("utils.R")

option_list <- list(
  make_option(
    c("-d", "--dir"),
    type = "character",
    default = ".",
    help = "Directory to read/write data",
    metavar = "character"
  )
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
output_dir <- opt$dir
prediction_cards_filename <- "predictions_cards.rds"
prediction_cards_filepath <- case_when(
  !is.null(output_dir) ~ file.path(output_dir, prediction_cards_filename),
  TRUE ~ prediction_cards_filename
)

options(warn = 1)

# Requested forecasters that do not get included in final scores:
#    Auquan-SEIR: Only predicts cumulative deaths
#    CDDEP-ABM: No longer on Forecast Hub. Causes some warnings when trying to download.
#    CDDEP-SEIR_MCMC: County-level predictions only
#    CUBoulder-COVIDLSTM: County-level predictions only
#    FAIR-NRAR: County-level predictions only
#    HKUST-DNN: Only predicts cumulative deaths
#    ISUandPKU-vSEIdR: Folder but no forecasts on Forecast Hub
#    PandemicCentral-COVIDForest: County-level predictions only
#    UT_GISAG-SPDM: County-level predictions only
#    WalmartLabsML-LogForecasting: Only predicts cumulative deaths
#    Yu_Group-CLEP: County-level predictions only
forecasters <- unique(c(
  get_covidhub_forecaster_names(designations = c("primary", "secondary")),
  "COVIDhub-baseline", "COVIDhub-trained_ensemble", "COVIDhub-4_week_ensemble"
))

## Get list of new and modified files to download
# The `path` field filters commits to only those that modifying the listed dir
BASE_URL <- "https://api.github.com/repos/reichlab/covid19-forecast-hub/commits?sha=%s&per_page=%s&path=data-processed&since=%s&page=%s"
ITEMS_PER_PAGE <- 100
BRANCH <- "master"

# We want to fetch all commits made in the last week. (Really this only needs to
# be the last 5 days, since we run the pipeline on Sunday, Monday, and Tuesday,
# but this adds some buffer for outages and timezone shenanigans.)
## TODO: instead of `today()` use saved timestamp from `datetime_created_utc.rds`.
##   Make sure the timezone is in a format the GH API can read
since_date <- sprintf("%sT00:00:00Z", today() - 7)

page <- 0
commit_sha_dates <- list()

# Fetch list of commits from API, one page at a time. Each page contains up to
# 100 commits. If a page contains 100 commits, assume that there are more
# results and fetch the next page.
while ( !exists("temp_commits") || nrow(temp_commits) == 100 ) {
  page <- page + 1
  # Construct the URL
  commits_url <- sprintf(BASE_URL, BRANCH, ITEMS_PER_PAGE, since_date, page)

  request <- GET(commits_url, authenticate(uname, token))
  # Convert any HTTP errors to R erros automatically.
  stop_for_status(request)

  # Convert results from nested JSON/list to dataframe
  temp_commits <- content(request, as = "text") %>%
    fromJSON(simplifyDataFrame = TRUE, flatten=TRUE)

  ## TODO: will this stop the loop consistently? Aren't we converting
  #  temp_commits to a dataframe above?
  if ( identical(temp_commits, list()) ) { break }

  commit_sha_dates[[page]] <- select(temp_commits, sha, url, commit.author.date)
}

# Combine all requested pages of commits into one dataframe
commit_sha_dates <- bind_rows(commit_sha_dates)

added_modified_files <- lapply(commit_sha_dates$url, function(commit_url) {
  # For each commit in `temp_commits`, get a list of any modified files.
  print(commit_url)

  # Make API call for each commit sha
  request <- GET(commit_url, authenticate(uname, token))
  stop_for_status(request)
  commit <- content(request, as = "text") %>%
    fromJSON(simplifyDataFrame = TRUE, flatten=TRUE)

  commit_files <- commit$files

  # Return empty df if no files listed as modified (can happen with merges, e.g.)
  if ( identical(commit_files, list())) { return(data.frame()) }

  # Else return list of changed files for each commit
  commit_files %>% mutate(commit.date = commit$commit$author$date)
}) %>%
  bind_rows() %>%
  select(filename, status, commit.date) %>%
  # File must be in data-processed dir somewhere and be a csv with expected name
  # format. We're only interested in added or modified files (not deleted,
  # renamed, copied, etc).
  filter(
    grepl("data-processed/.*/[0-9]{4}-[0-9]{2}-[0-9]{2}-.*[.]csv", filename),
    status %in% c("added", "modified")
  ) %>%
  select(-status) %>%
  group_by(filename) %>%
  filter(
    commit.date == max(commit.date)
  ) %>%
  ungroup()

# Get forecaster and date from filename
filename_parts <- strsplit(added_modified_files$filename, "/")
added_modified_files <- added_modified_files %>%
  mutate(
    forecaster = lapply(
      filename_parts, function(parts) {
        parts[[2]]
      }) %>%
        unlist(),
    forecast.date = lapply(
      filename_parts, function(parts) {
        substr(parts[[3]], start=1, stop=10)
      }) %>%
        unlist()
  ) %>%
  filter(forecaster %in% forecasters)


locations <- covidHubUtils::hub_locations

# also includes "us", which is national level data
state_geos <- locations %>%
  filter(nchar(.data$geo_value) == 2) %>%
  pull(.data$geo_value)
signals <- c(
  "confirmed_incidence_num",
  "deaths_incidence_num",
  "confirmed_admissions_covid_1d"
)


data_pull_timestamp <- now(tzone = "UTC")
predictions_cards <- get_covidhub_predictions(
  unique(added_modified_files$forecaster),
  signal = signals,
  ahead = 1:28,
  geo_values = state_geos,
  forecast_dates = unique(added_modified_files$forecast.date),
  verbose = TRUE,
  use_disk = TRUE
) %>%
  filter(!(incidence_period == "epiweek" & ahead > 4))

options(warn = 0)

# Includes predictions for future dates, which will not be scored.
predictions_cards <- predictions_cards %>%
  filter(!is.na(target_end_date))

# For hospitalizations, drop all US territories except Puerto Rico (pr) and the
# Virgin Islands (vi); HHS does not report data for any territories except these two.
territories <- c("as", "gu", "mp", "fm", "mh", "pw", "um")
predictions_cards <- predictions_cards %>%
  filter(!(geo_value %in% territories & data_source == "hhs"))

# For epiweek predictions, only accept forecasts made Monday or earlier.
# target_end_date is the date of the last day (Saturday) in the epiweek
# For daily predictions, accept any forecast where the target_end_date is later
# than the forecast_date.
predictions_cards <- predictions_cards %>%
  filter(
    (incidence_period == "epiweek" & target_end_date - (forecast_date + 7 * ahead) >= -2) |
      (incidence_period == "day" & target_end_date > forecast_date)
  )

# Load old predictions cards.
## TODO: instead of `today()` use saved timestamp from `datetime_created_utc.rds`.
old_predictions_cards <- readRDS(prediction_cards_filepath) %>%
  mutate(creation_date = today() - 1)

# Merge old and updated predictions cards. If multiple forecasts were made for a
# given set of characteristics, keep the most recent forecast
predictions_cards <- bind_rows(
  mutate(predictions_cards, creation_date = today()),
  old_predictions_cards
) %>%
  group_by(forecaster, geo_value, target_end_date, quantile, ahead, signal) %>%
  filter(
    forecast_date == max(forecast_date),
    creation_date == max(creation_date)
  ) %>%
  ungroup() %>%
  select(-creation_date)
class(predictions_cards) <- c("predictions_cards", class(predictions_cards))

print("Saving predictions...")
saveRDS(predictions_cards,
  file = prediction_cards_filepath,
  compress = "xz"
)
print("Predictions saved")

## Create error measure functions
central_intervals <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98)
cov_names <- paste0("cov_", central_intervals * 100)
coverage_functions <- sapply(
  central_intervals,
  function(coverage) interval_coverage(coverage)
)
names(coverage_functions) <- cov_names


err_measures <- c(
  wis = weighted_interval_score,
  overprediction = overprediction,
  underprediction = underprediction,
  sharpness = sharpness,
  ae = absolute_error,
  coverage_functions,
  value_20 = get_quantile_prediction_factory(0.2),
  value_50 = get_quantile_prediction_factory(0.5),
  value_80 = get_quantile_prediction_factory(0.8)
)

nation_predictions <- predictions_cards %>% filter(geo_value == "us")
state_predictions <- predictions_cards %>% filter(geo_value != "us")

# predictions_cards not needed beyond this point, try free up the memory
rm(predictions_cards)
gc()

## Check if nation and state predictions objects contain the expected forecasters
for (signal_name in signals) {
  check_for_missing_forecasters(nation_predictions, forecasters, "nation", signal_name, output_dir)
  check_for_missing_forecasters(state_predictions, forecasters, "state", signal_name, output_dir)
}

## Score predictions
print("Evaluating state forecasts")
geo_type <- "state"
state_scores <- evaluate_covid_predictions(state_predictions,
  err_measures,
  geo_type = geo_type
)

save_score_errors <- list()

for (signal_name in signals) {
  status <- save_score_cards_wrapper(state_scores, geo_type, signal_name, output_dir)
  if (status != 0) {
    save_score_errors[paste(signal_name, geo_type)] <- status
  }
}

print("Evaluating national forecasts")
# TODO: When this function was created, COVIDcast did not return national level
# data, and CovidHubUtils was used instead. We could now switch to COVIDcast,
# but COVIDcast and CovidHubUtils don't produce exactly the same data. This
# requires more investigation. Also using CovidHubUtils might be faster.
geo_type <- "nation"
nation_scores <- evaluate_chu(nation_predictions, signals, err_measures)

for (signal_name in signals) {
  status <- save_score_cards_wrapper(nation_scores, geo_type, signal_name, output_dir)
  if (status != 0) {
    save_score_errors[paste(signal_name, geo_type)] <- status
  }
}

if (length(save_score_errors) > 0) {
  stop(paste(save_score_errors, collapse = "\n"))
}

saveRDS(data.frame(datetime = c(data_pull_timestamp)), file = file.path(output_dir, "datetime_created_utc.rds"))
print("Done")
