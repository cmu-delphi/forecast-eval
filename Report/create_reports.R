#!/usr/bin/env Rscript
library("optparse")
library("dplyr")
library("evalcast")
library("lubridate")
library("stringr")

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
prediction_cards_filename <- "predictions_cards_${signal}.rds"
prediction_cards_filepath <- case_when(
  !is.null(output_dir) ~ file.path(output_dir, prediction_cards_filename),
  TRUE ~ prediction_cards_filename
)

options(warn = 1)

forecasters <- unique(c(
  get_covidhub_forecaster_names(designations = c("primary", "secondary")),
  "COVIDhub-baseline", "COVIDhub-trained_ensemble", "COVIDhub-4_week_ensemble"
)) %>%
  # Drop invalid forecasters
  setdiff(c(
    # Forecasters that have no data on Forecast Hub
    "CDDEP-ABM", # No longer on Forecast Hub. Causes some warnings when trying to download.
    "ISUandPKU-vSEIdR", # Folder but no forecasts on Forecast Hub

    # Forecasters that don't qualify for inclusion in the dashboard (wrong geo
    # level, wrong target variable)
    "Auquan-SEIR", # Only predicts cumulative deaths
    "CDDEP-SEIR_MCMC", # County-level predictions only
    "CUBoulder-COVIDLSTM", # County-level predictions only
    "FAIR-NRAR", # County-level predictions only
    "HKUST-DNN", # Only predicts cumulative deaths
    "PandemicCentral-COVIDForest", # County-level predictions only
    "UT_GISAG-SPDM", # County-level predictions only
    "WalmartLabsML-LogForecasting", # Only predicts cumulative deaths
    "Yu_Group-CLEP" # County-level predictions only
  ))

locations <- covidHubUtils::hub_locations

# also includes "us", which is national level data
state_geos <- locations %>%
  filter(nchar(.data$geo_value) == 2) %>%
  pull(.data$geo_value)
signals <- c(
  "confirmed_admissions_covid_1d"
)

data_pull_timestamp <- now(tzone = "UTC")
predictions_cards <- get_covidhub_predictions(forecasters,
  signal = signals,
  ahead = 1:28,
  geo_values = state_geos,
  verbose = TRUE,
  use_disk = TRUE
) %>%
  filter(!(incidence_period == "epiweek" & ahead > 4))

options(warn = 0)

# Includes predictions for future dates, which will not be scored.
predictions_cards <- predictions_cards %>%
  filter(!is.na(target_end_date))

# For hospitalizations, drop all US territories except Puerto Rico and the
# Virgin Islands; HHS does not report data for any territories except PR and VI.
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

# And only a forecaster's last forecast if multiple were made
predictions_cards <- predictions_cards %>%
  group_by(forecaster, geo_value, target_end_date, quantile, ahead, signal) %>%
  filter(forecast_date == max(forecast_date)) %>%
  ungroup()
class(predictions_cards) <- c("predictions_cards", class(predictions_cards))

print("Saving predictions...")
if (length(signals) == 1) {
  signal <- signals
  saveRDS(predictions_cards,
    file = str_interp(prediction_cards_filepath),
    compress = "xz"
  )
} else {
  # Save each signal separately.
  for (signal_group in group_split(predictions_cards, signal)) {
    signal <- signal_group$signal[1]
    saveRDS(signal_group,
      file = str_interp(prediction_cards_filepath),
      compress = "xz"
    )
  }
}
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

save_score_errors <- list()

## Score predictions
print("Evaluating state forecasts")
geo_type <- "state"
offline_signal_dir <- "signal_cache"
# Take advantage of `evalcast`'s caching feature. Suppress output since we
# only care about generating the cache.
#
# Since cache files are named using only the provided as-of date, the first
# COVIDcast call for a given as-of will be used for all subsequent calls with
# the same as-of, whether or not the cache actually contains all the desired
# `time_value`s.
#
# Since data used for scoring is fetched one day or week at a time as-of
# "today", the first such call would create a cache covering a very narrow
# date range. Later API calls would attempt to use the incomplete cache file.
#
# Circumvent this by explicitly pulling the full date range and initializing a
# complete cache for each signal used.
sources <- list(
  list(data_source = "hhs", signal = "confirmed_admissions_covid_1d"),
  list(data_source = "jhu-csse", signal = "confirmed_incidence_num"),
  list(data_source = "jhu-csse", signal = "deaths_incidence_num")
)
invisible({
  for (source in sources) {
    download_signal(
      data_source = source$data_source, signal = source$signal,
      # "us" can also be included in `states_geos`. Drop to avoid "Data not
      # fetched for some geographies" error.
      geo_type = "state", geo_values = setdiff(state_geos, "us"), offline_signal_dir = offline_signal_dir
    )
  }
})

state_scores <- evaluate_covid_predictions(state_predictions,
  err_measures,
  geo_type = geo_type,
  offline_signal_dir = offline_signal_dir
)

for (signal_name in signals) {
  status <- save_score_cards_wrapper(state_scores, geo_type, signal_name, output_dir)
  if (status != 0) {
    save_score_errors[paste(signal_name, geo_type)] <- status
  }
}

rm(state_scores, state_predictions)
gc()

print("Evaluating national forecasts")
# TODO: When this function was created, COVIDcast did not return national level
# data, and CovidHubUtils was used instead. We could now switch to COVIDcast,
# but COVIDcast and CovidHubUtils don't produce exactly the same data. This
# requires more investigation. Not using `evalcast` is also faster.
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
