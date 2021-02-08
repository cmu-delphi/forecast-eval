source("predictions.R")
create_prediction_cards()

source("score.R")
create_score_cards("state", signal_name = "confirmed_incidence_num")
create_score_cards("state", signal_name = "deaths_incidence_num")
create_score_cards("nation", signal_name = "confirmed_incidence_num")
create_score_cards("nation", signal_name = "deaths_incidence_num")
