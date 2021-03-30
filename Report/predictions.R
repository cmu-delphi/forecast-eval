library(lubridate)
library(evalcast)
library(dplyr)
library(stringr)

create_prediction_cards = function(prediction_cards_filename){
  start_date = today() - 100 * 7 # last 100 weeks
  
  forecasters = c("COVIDhub-baseline",
                  get_covidhub_forecaster_names(designations = "primary"))
  num_forecasters = length(forecasters)
  print(str_interp("Getting forecasts for ${num_forecasters} forecasters."))
  
  # Get all forecast dates for these forecasters from COVID Hub
  forecast_dates = vector("list", length = length(forecasters))
  for (i in 1:length(forecasters)) {
    forecast_dates[[i]] = tryCatch({
      as_date(get_covidhub_forecast_dates(forecasters[i]))
    },
    error = function(e) cat(sprintf("%i. %s\n", i, e$message))
    )
  }
  # Convert NULL to empty vector. Useful when a model has no forecasts,
  # resulting in a 404 and no forecast_dates.
  forecast_dates = lapply(forecast_dates,
                          function(dates) if (is.null(dates)) Date() else dates)
  forecast_dates = lapply(forecast_dates, function(date) date[date >= start_date])

  names(forecast_dates) = forecasters
  
  # Now get new predictions for each forecaster
  
  predictions_cards_list = vector("list", length = length(forecasters))
  deaths_sig = "deaths_incidence_num"
  cases_sig = "confirmed_incidence_num"
  for (i in 1:length(forecasters)) {
    cat(str_interp("${i}/${num_forecasters}:${forecasters[i]} ...\n"))
    if (length(forecast_dates[[i]] > 0)){
      predictions_cards_list[[i]] = tryCatch({
        get_covidhub_predictions(forecasters[i], 
                                 rev(forecast_dates[[i]])) %>% 
          filter(ahead < 5) %>% 
          filter(nchar(geo_value) == 2 & signal %in% c(deaths_sig, cases_sig))
      },
      error = function(e) cat(e$message))
    }
  }
  predictions_cards_new = bind_rows(predictions_cards_list)
  
  # Combine old and new predictions cards
  if(exists("predictions_cards")){
    predictions_cards = rbind(predictions_cards, predictions_cards_new)
  } else {
    predictions_cards = predictions_cards_new
  }
  predictions_cards = predictions_cards %>%
                        filter(forecast_date >= start_date) %>%
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
          file = prediction_cards_filename, 
          compress = "xz")
}