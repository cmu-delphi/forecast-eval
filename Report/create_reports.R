#!/usr/bin/env Rscript
library("optparse")
library("dplyr")
library("evalcast")
library("lubridate")

option_list = list(
    make_option(
        c("-d", "--dir"),
        type = "character",
        default = ".",
        help = "Directory to read/write data",
        metavar = "character"
    )
);

opt_parser = OptionParser(option_list = option_list);
opt = parse_args(opt_parser);

prediction_cards_filename = "predictions_cards.rds"
prediction_cards_filepath = case_when(
    !is.null(opt$dir) ~ file.path(opt$dir, prediction_cards_filename),
    TRUE~prediction_cards_filename
)

forecasters = unique(c(get_covidhub_forecaster_names(designations = c("primary", "secondary")),
                "COVIDhub-baseline", "COVIDhub-trained_ensemble"))
locations = covidHubUtils::hub_locations

# also includes "us", which is national level data
state_geos = locations %>%
                filter(nchar(.data$geo_value) == 2) %>%
                pull(.data$geo_value)
signals = c("confirmed_incidence_num",
            "deaths_incidence_num",
            "confirmed_admissions_covid_1d")

predictions_cards = get_covidhub_predictions(forecasters,
                                             signal = signals,
                                             ahead = 1:28,
                                             geo_values = state_geos,
                                             verbose = TRUE,
                                             use_disk = TRUE) %>% 
    filter(!(incidence_period == "epiweek" & ahead > 4))

predictions_cards = predictions_cards %>%
    filter(!is.na(target_end_date)) %>%
    filter(target_end_date < today())

# For hospitalizations, drop all US territories except Puerto Rico and the
# Virgin Islands; HHS does not report data for any territories except PR and VI.
territories <- c("as", "gu", "mp", "fm", "mh", "pw", "um")
predictions_cards = predictions_cards %>%
    filter(!(geo_value %in% territories & data_source == "hhs"))

# For epiweek predictions, only accept forecasts made Monday or earlier.
# target_end_date is the date of the last day (Saturday) in the epiweek
# For daily predictions, accept any forecast where the target_end_date is later
# than the forecast_date.
predictions_cards = predictions_cards %>%
    filter(
        (incidence_period == "epiweek" & target_end_date - (forecast_date + 7 * ahead) >= -2) |
            (incidence_period == "day" & target_end_date > forecast_date)
    )

# And only a forecaster's last forecast if multiple were made
predictions_cards = predictions_cards %>%
    group_by(forecaster, geo_value, target_end_date, quantile, ahead, signal) %>%
    filter(forecast_date == max(forecast_date)) %>%
    ungroup()
class(predictions_cards) = c("predictions_cards", class(predictions_cards))

print("Saving predictions...")
saveRDS(predictions_cards,
        file = prediction_cards_filepath,
        compress = "xz")
print("Predictions saved")

# Create error measure functions
central_intervals = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98)
cov_names = paste0("cov_", central_intervals * 100)
coverage_functions = sapply(central_intervals,
                            function(coverage) interval_coverage(coverage))
names(coverage_functions) = cov_names

# TODO: Contains fixed versions of WIS component metrics, to be ported over to evalcast
# Redefines overprediction, underprediction and sharpness
source("error_measures.R")

err_measures = c(wis = weighted_interval_score,
                 overprediction = overprediction,
                 underprediction = underprediction,
                 sharpness = sharpness,
                 ae = absolute_error,
                 coverage_functions,
                 value_20 = get_quantile_prediction_factory(0.2),
                 value_50 = get_quantile_prediction_factory(0.5),
                 value_80 = get_quantile_prediction_factory(0.8))

nation_predictions = predictions_cards %>% filter(geo_value == "us")
state_predictions = predictions_cards %>% filter(geo_value != "us")

# predictions_cards not needed beyond this point, try free up the memory
rm(predictions_cards)
gc()

print("Evaluating state forecasts")
state_scores = evaluate_covid_predictions(state_predictions,
                                          err_measures,
                                          geo_type = "state")

source("score.R")
if ( "confirmed_incidence_num" %in% unique(state_scores$signal)) {
    print("Saving state confirmed incidence...")
    save_score_cards(state_scores, "state", signal_name = "confirmed_incidence_num",
                     output_dir = opt$dir)
} else {
    warning("State confirmed incidence should generally be available. Please 
            verify that you expect not to have any cases incidence forecasts")
}
if ( "deaths_incidence_num" %in% unique(state_scores$signal)) {
    print("Saving state deaths incidence...")
    save_score_cards(state_scores, "state", signal_name = "deaths_incidence_num",
                     output_dir = opt$dir)
} else {
    warning("State deaths incidence should generally be available. Please 
            verify that you expect not to have any deaths incidence forecasts")
}
if ( "confirmed_admissions_covid_1d" %in% unique(state_scores$signal)) {
    print("Saving state hospitalizations...")
    save_score_cards(state_scores, "state", signal_name = "confirmed_admissions_covid_1d",
                     output_dir = opt$dir)
}


print("Evaluating national forecasts")
nation_scores = evaluate_covid_predictions(nation_predictions,
                                                     err_measures,
                                                     geo_type = "nation")

if ( "confirmed_incidence_num" %in% unique(nation_scores$signal)) {
    print("Saving nation confirmed incidence...")
    save_score_cards(nation_scores, "nation",
                     signal_name = "confirmed_incidence_num", output_dir = opt$dir)
} else {
    warning("Nation confirmed incidence should generally be available. Please 
            verify that you expect not to have any cases incidence forecasts")
}
if ( "deaths_incidence_num" %in% unique(nation_scores$signal)) {
    print("Saving nation deaths incidence...")
    save_score_cards(nation_scores, "nation", signal_name = "deaths_incidence_num",
                     output_dir = opt$dir)
} else {
    warning("Nation deaths incidence should generally be available. Please 
            verify that you expect not to have any deaths incidence forecasts")
}
if ( "confirmed_admissions_covid_1d" %in% unique(nation_scores$signal)) {
    print("Saving nation hospitalizations...")
    save_score_cards(nation_scores, "nation", signal_name = "confirmed_admissions_covid_1d",
                     output_dir = opt$dir)
}

print("Done")
