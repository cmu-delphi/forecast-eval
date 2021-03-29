library("dplyr")
library("assertthat")

save_score_cards = function(score_card, geo_type = c("state", "nation"), 
                              signal_name = c("confirmed_incidence_num",
                                              "deaths_incidence_num"),
                              output_dir = "."){
  signal_name = match.arg(signal_name)
  geo_type = match.arg(geo_type)
  signals = score_card %>%
              distinct(signal) %>%
              pull(signal)
  assert_that(signal_name %in% signals,
                msg = "signal is not in score_card")
  score_card = score_card %>% filter(signal == signal_name)
  if (signal_name == "confirmed_incidence_num"){
    sig_suffix = "cases"
  } else {
    sig_suffix = "deaths"
  }
  output_file_name = file.path(output_dir, paste0("score_cards_", geo_type, "_", sig_suffix, ".rds"))
  
  if (geo_type == "state"){
    score_card = score_card %>% 
      filter(nchar(geo_value) == 2, geo_value != "us")
  } else if (geo_type == "nation"){
    score_card = score_card %>%
      filter(geo_value == "us")
  }
  
  saveRDS(score_card, 
       file = output_file_name, 
       compress = "xz")
}

evaluate_chu = function(predictions, signals, err_measures){
  allowed_signals = c("confirmed_incidence_num",
                      "deaths_incidence_num")
  assert_that(all(signals %in% allowed_signals),
              msg = paste("Signal not allowed:",
                          setdiff(signals, allowed_signals)))
  scores = c()
  for(signal_name in signals){
    preds_signal = predictions %>%
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
    signal_scores = evaluate_predictions(preds_signal, 
                                         truth_data = chu_truth,
                                         err_measures,
                                         grp_vars = c("target_end_date",
                                                      "geo_value",
                                                      "ahead",
                                                      "forecaster"))
    scores = rbind(scores, signal_scores)
  }
  return(scores)
}
