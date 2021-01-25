source("predictions.R")
create_prediction_cards()

source("score.R")
create_score_cards("state", signal = "confirmed_incidence_num")
create_score_cards("state", signal = "deaths_incidence_num")
