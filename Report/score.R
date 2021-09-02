library("dplyr")
library("assertthat")

save_score_cards = function(score_card, geo_type = c("state", "nation"),
                              signal_name = c("confirmed_incidence_num",
                                              "deaths_incidence_num",
                                              "confirmed_admissions_covid_1d"),
                              output_dir = ".") {
  signal_name = match.arg(signal_name)
  geo_type = match.arg(geo_type)
  signals = score_card %>%
              distinct(signal) %>%
              pull(signal)
  assert_that(signal_name %in% signals,
                msg = "signal is not in score_card")
  score_card = score_card %>% filter(signal == signal_name)
  
  type_map <- list("confirmed_incidence_num" = "cases",
                     "deaths_incidence_num" = "deaths",
                     "confirmed_admissions_covid_1d" = "hospitalizations")
  sig_suffix <- type_map[[signal_name]]
  output_file_name = file.path(output_dir,
                               paste0("score_cards_", geo_type, "_",
                                      sig_suffix, ".rds"))

  if (geo_type == "state") {
    score_card = score_card %>%
      filter(nchar(geo_value) == 2, geo_value != "us")
  } else if (geo_type == "nation") {
    score_card = score_card %>%
      filter(geo_value == "us")
  }

  saveRDS(score_card,
       file = output_file_name,
       compress = "xz")
}
