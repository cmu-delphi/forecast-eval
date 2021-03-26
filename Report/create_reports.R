#!/usr/bin/env Rscript
library("optparse")
library("dplyr")
library("evalcast")
library("lubridate")

option_list = list(
    make_option(
        c("-d", "--dir"), 
        type="character", 
        default=".", 
        help="Directory to read/write data", 
        metavar="character"
    )
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

prediction_cards_filename = "predictions_cards.rds"
prediction_cards_filepath = case_when(
    !is.null(opt$dir) ~ file.path(opt$dir,prediction_cards_filename),
    TRUE~prediction_cards_filename
)

forecasters = c(get_covidhub_forecaster_names(designations = "primary"),
                "COVIDhub-baseline")
locations = covidHubUtils::hub_locations

# also includes "us", which is national level data
state_geos = locations %>%
                filter(nchar(.data$geo_value) == 2) %>%
                pull(.data$geo_value)
signals = c("confirmed_incidence_num",
            "deaths_incidence_num")

predictions_cards = get_covidhub_predictions(forecasters,
                                             signal = signals,
                                             geo_values = state_geos,
                                             verbose = TRUE)
predictions_cards = predictions_cards %>%
    filter(!is.na(predictions_cards$target_end_date)) 
predictions_cards = predictions_cards %>% filter(target_end_date < today())

# Only accept forecasts made Monday or earlier
predictions_cards = predictions_cards %>%
    filter(target_end_date - (forecast_date + 7 * ahead) >= -2)

# And only a forecaster's last forecast if multiple were made
predictions_cards = predictions_cards %>% 
    group_by(forecaster, geo_value, target_end_date, quantile, ahead, signal) %>%
    filter(forecast_date == max(forecast_date)) %>%
    ungroup()
class(predictions_cards) = c("predictions_cards", class(predictions_cards))

print("Saving predictions...")
saveRDS(predictions_cards,
        file = "predictions_cards_raw.rds", 
        compress = "xz")
print("Predictions saved")

# Create error measure functions
central_intervals = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98)
cov_names = paste0("cov_", central_intervals * 100)
coverage_functions = sapply(central_intervals, 
                            function(coverage) interval_coverage(coverage))
names(coverage_functions) = cov_names

err_measures = c(wis = weighted_interval_score, 
                 ae = absolute_error,
                 coverage_functions) 

nation_predictions = predictions_cards %>% filter(geo_value == "us")
state_predictions = predictions_cards %>% filter(geo_value != "us")

print("Evaluating state forecasts")
scores_state = evaluate_covid_predictions(state_predictions, err_measures, geo_type = "state")

source("score.R")
print("Saving state confirmed incidence...")
save_score_cards(scores_state, "state", signal_name = "confirmed_incidence_num", output_dir=opt$dir)
print("Saving state deaths incidence...")
save_score_cards(scores_state, "state", signal_name = "deaths_incidence_num", output_dir=opt$dir)

print("Evaluating national forecasts")
# COVIDcast does not return national level data, using CovidHubUtils instead
nation_scores = c()
for(signal_name in signals){
  nation_preds_signal = nation_predictions %>%
                      filter(signal == signal_name)
  if (signal_name == "confirmed_incidence_num"){
    jhu_signal = "inc case"
  } else {
    jhu_signal = "inc death"
  }
  chu_truth = covidHubUtils::load_truth("JHU", jhu_signal)
  chu_truth = chu_truth %>%
                rename(actual = value) %>%
                select(-c(model,
                          target_variable,
                          location,
                          location_name,
                          population,
                          geo_type,
                          abbreviation))
  signal_scores = evaluate_predictions(nation_preds_signal, 
                                    truth_data = chu_truth,
                                    err_measures,
                                    grp_vars = c("target_end_date", "geo_value", "ahead", "forecaster"))
  nation_scores = rbind(nation_scores, signal_scores)
}

print("Saving nation confirmed incidence...")
save_score_cards(nation_scores, "nation", signal_name = "confirmed_incidence_num", output_dir=opt$dir)
print("Saving nation deaths incidence...")
save_score_cards(nation_scores, "nation", signal_name = "deaths_incidence_num", output_dir=opt$dir)

print("Done")
