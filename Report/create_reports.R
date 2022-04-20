#!/usr/bin/env Rscript
library("optparse")
library("dplyr")
library("evalcast")
library("lubridate")
library("jsonlite")
library("httr")


# TODO: Contains fixed versions of WIS component metrics, to be ported over to evalcast
# Redefines overprediction, underprediction and sharpness
source("error_measures.R")
source("score.R")
source("utils.R")
source("fetch_data.R")
source("process_data.R")

# Define and fetch command-line options.
option_list <- list(
  make_option(
    c("-d", "--dir"),
    type = "character",
    default = "dist",
    help = "Directory to read/write data"
  ),
  make_option(
    c("--exhaustive-download"),
    type = "logical",
    default = FALSE,
    help = "Download full history of predictions?"
  ),
  make_option(
    c("--exhaustive-scoring"),
    type = "logical",
    default = TRUE,
    help = "Score full history of predictions?"
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

# Get token to authenticate GitHub REST API.
github_token <- Sys.getenv("GITHUB_TOKEN")
if (github_token == "") {
  stop("Token is not set or is not able to be fetched from the environment")
}

data_pull_timestamp <- now(tzone = "UTC")

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

if (opt[["exhaustive-download"]]) {
  predictions_cards <- download_all_forecasts(forecasters, state_geos)
} else {
  predictions_cards <- download_recent_forecasts(forecasters, state_geos, github_token)
}

options(warn = 0)

predictions_cards <- filter_predictions(predictions_cards)
if (!opt[["exhaustive-download"]]) {
  predictions_cards <- merge_new_old_predictions(predictions_cards, prediction_cards_filepath)
}

message("keeping most recent version of a given forecast")
predictions_cards <- predictions_cards %>%
  # If multiple forecasts were made for a given set of characteristics, keep the
  # newest version of the forecast.
  group_by(forecaster, geo_value, target_end_date, quantile, ahead, signal) %>%
  filter(
    forecast_date == max(forecast_date)
  ) %>%
  ungroup()

class(predictions_cards) <- c("predictions_cards", class(predictions_cards))

stop("we didn't change anything about scoring, so can skip that bit")
message("Saving predictions...")
saveRDS(predictions_cards,
  file = prediction_cards_filepath,
  compress = "xz"
)
message("Predictions saved")

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
save_score_errors <- list()

message("Evaluating state forecasts")
geo_type <- "state"
state_scores <- evaluate_covid_predictions(
  state_predictions, err_measures, geo_type = geo_type
)

for (signal_name in signals) {
  status <- save_score_cards_wrapper(state_scores, geo_type, signal_name, output_dir)
  if (status != 0) {
    save_score_errors[paste(signal_name, geo_type)] <- status
  }
}

message("Evaluating national forecasts")
# TODO: When this function was created, COVIDcast did not return national level
# data, and CovidHubUtils was used instead. We could now switch to COVIDcast,
# but COVIDcast and CovidHubUtils don't produce exactly the same data. This
# requires more investigation. Also using CovidHubUtils might be faster.
geo_type <- "nation"
nation_scores <- evaluate_chu(
  nation_predictions, signals, err_measures
)

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
message("Done")
