filter_predictions <- function(predictions_cards) {
  message("filtering to only valid predictions")
   # Includes predictions for future dates, which will not be scored.
  message("by target end date")
	predictions_cards <- predictions_cards %>%
	  filter(!is.na(target_end_date))

	# For hospitalizations, drop all US territories except Puerto Rico (pr) and the
	# Virgin Islands (vi); HHS does not report data for any territories except these two.
	message("drop territories")
	territories <- c("as", "gu", "mp", "fm", "mh", "pw", "um")
	predictions_cards <- predictions_cards %>%
	  filter(!(geo_value %in% territories & data_source == "hhs"))

	# For epiweek predictions, only accept forecasts made Monday or earlier.
	# target_end_date is the date of the last day (Saturday) in the epiweek
	# For daily predictions, accept any forecast where the target_end_date is later
	# than the forecast_date.
	message("by forecast date day of week")
	predictions_cards <- predictions_cards %>%
	  filter(
	    (incidence_period == "epiweek" & target_end_date - (forecast_date + 7 * ahead) >= -2) |
	      (incidence_period == "day" & target_end_date > forecast_date)
	  )

	return(predictions_cards)
}

merge_new_old_predictions <- function(predictions_cards, prediction_cards_filepath) {
  message("combining recent and old predictions cards")
	# Load old predictions cards.
  message("loading old predictions cards")
	if (file.exists(prediction_cards_filepath)) {
	  old_predictions_cards <- readRDS(prediction_cards_filepath) %>%
	    mutate(updated_cards_flag = 0)
	} else {
	  warning("Could not find prediction cards at ", prediction_cards_filepath)
	  old_predictions_cards <- data.frame()
	}

  message("binding together and deduping")
	predictions_cards <- bind_rows(
	  mutate(predictions_cards, updated_cards_flag = 1),
	  old_predictions_cards
	) %>%
	  # If a given forecast appears in both old and new prediction cards, keep the new one.
	  group_by(forecaster, geo_value, target_end_date, quantile, ahead, signal) %>%
	  filter(
	    updated_cards_flag == max(updated_cards_flag)
	  ) %>%
	  ungroup() %>%
	  select(-updated_cards_flag)

	rm(old_predictions_cards)

	return(predictions_cards)
}



