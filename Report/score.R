library("evalcast")
library("dplyr")
library("lubridate")
library("assertthat")

create_score_cards = function(prediction_cards_filepath, geo_type, signal_name = NULL, output_file_name = NULL, output_dir="."){
  if (!exists("predictions_cards")){
    predictions_cards = readRDS(prediction_cards_filepath)
  }
  signals = (predictions_cards %>% distinct(signal))$signal
  if (is.null(signal_name)){
    assert_that(length(signals) == 1,
                msg = "If no signal is specified, prediction_cards may only have 1 signal")
    signal_name = signals
  } else {
    assert_that(signal_name %in% signals,
                msg = "signal is not in prediction_cards")
    predictions_cards = predictions_cards %>% filter(signal == signal_name)
  }
  if (is.null(output_file_name)){
    cases_sig = "confirmed_incidence_num"
    deaths_sig = "deaths_incidence_num"
    assert_that(signal_name %in% c(cases_sig, deaths_sig),
                msg = paste("If no output file is provided, signal must be in:",
                            cases_sig,
                            deaths_sig))
    if (signal_name == cases_sig){
      sig_suffix = "cases"
    } else {
      sig_suffix = "deaths"
    }
    output_file_name = file.path(output_dir,paste0("score_cards_", geo_type, "_", sig_suffix, ".rds"))
  }
  # central coverage functions named cov_10, cov_20, etc.
  central_intervals = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98)
  cov_names = paste0("cov_", central_intervals * 100)
  coverage_functions = sapply(central_intervals, 
                              function(coverage) interval_coverage(coverage))
  names(coverage_functions) = cov_names
  
  err_measures = c(wis = weighted_interval_score, 
                   ae = absolute_error,
                  coverage_functions) 
  preds_to_eval = predictions_cards %>% 
    filter(target_end_date < today())
  
  if (geo_type == "state"){
    preds_to_eval = preds_to_eval %>% 
      filter(nchar(geo_value) == 2)
  } else if (geo_type == "county"){
    preds_to_eval = preds_to_eval %>% 
      filter(nchar(geo_value) == 5)
  }
  if (file.exists(output_file_name)) {
    score_cards = readRDS(output_file_name)
  }
  if(exists("score_cards")){
    preds_to_eval = anti_join(preds_to_eval, 
                              score_cards, 
                              by = c("ahead", "forecaster", "forecast_date"))
  }
  
  #Only score forecasters with atleast 3 forecasts (i.e. more than mean and median)
  quantile_forecasts = preds_to_eval %>% 
    group_by(forecaster, forecast_date, geo_value, ahead) %>% 
    summarize(num_quantiles = n_distinct(quantile)) %>%
    filter(num_quantiles > 2) %>%
    select(-c(num_quantiles))
  preds_to_eval = semi_join(preds_to_eval, quantile_forecasts)
  if(nrow(preds_to_eval) > 0){
    score_cards_new = evaluate_predictions(preds_to_eval, 
                                           err_measures,
                                           backfill_buffer = 0)
  } else {
    score_cards_new = data.frame()
  }
  
  if(exists("score_cards")){
    score_cards = rbind(score_cards, score_cards_new)
  } else {
    score_cards = score_cards_new
  }
 # score_cards = score_cards %>% filter(forecast_date >= start_date)
  
  saveRDS(score_cards, 
       file = output_file_name, 
       compress = "xz")
}