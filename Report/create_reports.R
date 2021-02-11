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

source("predictions.R")
create_prediction_cards(prediction_cards_filepath)

source("score.R")
print("Scoring confirmed incidence...")
create_score_cards(prediction_cards_filepath, "state", signal = "confirmed_incidence_num", output_dir=opt$dir)
print("Scoring deaths incidence...")
create_score_cards(prediction_cards_filepath, "state", signal = "deaths_incidence_num", output_dir=opt$dir)
print("Done")
