#!/usr/bin/env Rscript
library("optparse")
library("dplyr")

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
                filter(nchar(geo_value) == 2) %>%
                pull(geo_value)

predictions_cards = get_covidhub_predictions(covidhub_forecaster_name = forecasters,
                                             signal = c("confirmed_incidence_num",
                                                        "deaths_incidence_num"),
                                             geo_values = state_geos)
predictions_cards = predictions_cards %>%
    filter(!is.na(predictions_cards$target_end_date)) 

# Only accept forecasts made Monday or earlier
predictions_cards = predictions_cards %>%
    filter(target_end_date - (forecast_date + 7 * ahead) >= -2)

# And only a forecaster's last forecast if multiple were made
predictions_cards = predictions_cards %>% 
    group_by(forecaster, geo_value, target_end_date, quantile, ahead, signal) %>%
    filter(forecast_date == max(forecast_date)) %>%
    ungroup()
class(predictions_cards) = c("predictions_cards", class(predictions_cards))

saveRDS(predictions_cards,
        file = "predictions_cards_raw.rds", 
        compress = "xz")

source("score.R")
print("Scoring state confirmed incidence...")
create_score_cards(prediction_cards_filepath, "state", signal_name = "confirmed_incidence_num", output_dir=opt$dir)
print("Scoring state deaths incidence...")
create_score_cards(prediction_cards_filepath, "state", signal_name = "deaths_incidence_num", output_dir=opt$dir)
print("Scoring nation confirmed incidence...")
create_score_cards(prediction_cards_filepath, "nation", signal_name = "confirmed_incidence_num", output_dir=opt$dir)
print("Scoring nation deaths incidence...")
create_score_cards(prediction_cards_filepath, "nation", signal_name = "deaths_incidence_num", output_dir=opt$dir)

print("Done")
