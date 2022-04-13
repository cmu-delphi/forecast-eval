library("dplyr")
library("assertthat")

type_map <- list(
  "confirmed_incidence_num" = "cases",
  "deaths_incidence_num" = "deaths",
  "confirmed_admissions_covid_1d" = "hospitalizations"
)

generate_score_card_file_path <- function(geo_type, signal_name, output_dir) {
  sig_suffix <- type_map[[signal_name]]
  output_file_name <- file.path(
    output_dir,
    paste0(
      "score_cards_", geo_type, "_",
      sig_suffix, ".rds"
    )
  )
  return(output_file_name)
}

save_score_cards <- function(score_card, geo_type = c("state", "nation"),
                             signal_name = c(
                               "confirmed_incidence_num",
                               "deaths_incidence_num",
                               "confirmed_admissions_covid_1d"
                             ),
                             output_dir = ".") {
  signal_name <- match.arg(signal_name)
  geo_type <- match.arg(geo_type)
  signals <- score_card %>%
    distinct(signal) %>%
    pull(signal)
  assert_that(signal_name %in% signals,
    msg = "signal is not in score_card"
  )
  score_card <- score_card %>% filter(signal == signal_name)
  if (geo_type == "state") {
    score_card <- score_card %>%
      filter(nchar(geo_value) == 2, geo_value != "us")
  } else if (geo_type == "nation") {
    score_card <- score_card %>%
      filter(geo_value == "us")
  }

  output_file_name <- generate_score_card_file_path(geo_type, signal_name, output_dir)
  saveRDS(score_card,
    file = output_file_name,
    compress = "xz"
  )
}

save_score_cards_wrapper <- function(score_card, geo_type, signal_name, output_dir) {
  if (signal_name %in% unique(score_card[["signal"]])) {
    print(paste("Saving", geo_type, type_map[[signal_name]], "..."))
    save_score_cards(score_card, geo_type,
      signal_name = signal_name, output_dir = output_dir
    )
    return(0)
  } else {
    return(paste("No", signal_name, "available at the", geo_type, "level"))
  }
}

# Fetch national truth data from CovidHubUtils.
evaluate_chu <- function(predictions, signals, err_measures) {
  allowed_signals <- c(
    "confirmed_incidence_num",
    "deaths_incidence_num",
    "confirmed_admissions_covid_1d"
  )
  assert_that(all(signals %in% allowed_signals),
    msg = paste(
      "Signal not allowed:",
      setdiff(signals, allowed_signals)
    )
  )

  target_map <- list(
    "confirmed_incidence_num" = "inc case",
    "deaths_incidence_num" = "inc death",
    "confirmed_admissions_covid_1d" = "inc hosp"
  )
  source_map <- list(
    "confirmed_incidence_num" = "JHU",
    "deaths_incidence_num" = "JHU",
    "confirmed_admissions_covid_1d" = "HealthData"
  )
  scores <- list()
  for (signal_name in signals) {
    preds_signal <- predictions %>%
      filter(signal == signal_name)
    signal <- target_map[[signal_name]]
    source <- source_map[[signal_name]]
    chu_truth <- covidHubUtils::load_truth(source, signal)
    chu_truth <- chu_truth %>%
      rename(actual = value) %>%
      select(-c(
        model,
        target_variable,
        location,
        location_name,
        population,
        geo_type,
        abbreviation
      ))
    ## select equivalent to
    # select(target_end_date, actual, geo_value, full_location_name)
    signal_scores <- evaluate_predictions(preds_signal,
      truth_data = chu_truth,
      err_measures,
      grp_vars = c(
        "target_end_date",
        "geo_value",
        "ahead",
        "forecaster"
      )
    )
    scores[[signal_name]] <- signal_scores
  }
  return(bind_rows(scores))
}

# Fetch truth data from COVIDcast. This function bypasses some of the slow parts
# of the `evalcast` pipeline by pulling all data from COVIDcast together.
evaluate_covidcast <- function(predictions, signals, err_measures, geo_type) {
  allowed_signals <- c(
    "confirmed_incidence_num",
    "deaths_incidence_num",
    "confirmed_admissions_covid_1d"
  )
  assert_that(all(signals %in% allowed_signals),
    msg = paste(
      "Signal not allowed:",
      setdiff(signals, allowed_signals)
    )
  )

  source_map <- list(
    "confirmed_incidence_num" = "jhu-csse",
    "deaths_incidence_num" = "jhu-csse",
    "confirmed_admissions_covid_1d" = "hhs"
  )
  scores <- list()
  for (signal_name in signals) {
    preds_signal <- predictions %>%
      filter(signal == signal_name)
    source <- source_map[[signal_name]]
    covidcast_truth <- get_covidcast_period_actuals(preds_signal)
    signal_scores <- evaluate_predictions(preds_signal,
      truth_data = covidcast_truth,
      err_measures,
      grp_vars = c(
        "target_end_date",
        "geo_value",
        "ahead",
        "forecaster"
      )
    )
    scores[[signal_name]] <- signal_scores
  }

  scores <- bind_rows(scores) %>%
    arrange(ahead, geo_value, forecaster, forecast_date, data_source, signal, target_end_date, incidence_period) %>%
    select(ahead, geo_value, forecaster, forecast_date, data_source, signal, target_end_date, incidence_period, everything)
  return(scores)
}


get_covidcast_period_actuals <- function(response) {
  # Get start/end dates of each period we want to sum truth values over.
  target_periods <- response %>%
    select(.data$forecast_date, .data$incidence_period, .data$ahead) %>%
    distinct() %>%
    purrr::pmap_dfr(get_target_period) %>%
    distinct()

  # Compute the actual values that the forecaster is trying to
  # predict. In particular,
  # - get most recent data available from covidcast for these target periods
  # - sum up the response over the target incidence period
  target_periods <- target_periods %>%
    mutate(available = .data$end <= Sys.Date()) %>%
    filter(.data$available) %>%
    select(-.data$available)

  covidcast_truth <- covidcast::covidcast_signal(
    source,
    signal_name,
    geo_type = geo_type,
    start_day = as.Date(min(target_periods$start)),
    end_day = as.Date(max(target_periods$end))
  ) %>%
    select(data_source, signal, geo_value, time_value, value)

  # Expand out each period by day so easier to join on.
  target_periods <- target_periods %>% pmap_dfr(function(start_date, end_date) {
    tibble(
      start = start_date,
      target_end_date = end_date,
      day = seq.Date(from = start_date, to = end_date, by = 1)
    )
  })

  period_truth <- full_join(covidcast_truth, target_periods, by = c("time_value" = "day"))

  check_count <- period_truth %>%
    group_by(.data$geo_value, .data$start, .data$target_end_date) %>%
    summarize(num = n(), .groups = "drop") %>%
    filter(num < 7)

  if (nrow(check_count) != 0) {
    warning(paste0(
      "Some or all data missing for the following target periods: ",
      paste(
        paste(period_truth$start, period_truth$target_end_date, sep = "-"),
        collapse = ", "
      ),
      "."
    ))
  }

  period_truth <- period_truth %>%
    group_by(.data$geo_value, .data$target_end_date) %>%
    summarize(actual = sum(.data$value), .groups = "drop") %>%
    select(.data$target_end_date, .data$actual, .data$geo_value)

  return(period_truth)
}
