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
  scores <- c()
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
    scores <- rbind(scores, signal_scores)
  }
  return(scores)
}
