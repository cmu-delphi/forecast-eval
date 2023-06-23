library(dplyr)
library(tidyr)
library(aws.s3)

Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
s3bucket <- tryCatch(
  {
    get_bucket(bucket = "forecast-eval")
  },
  error = function(e) {
    e
  }
)

readbucket <- function(name) {
  tryCatch(
    {
      s3readRDS(object = name, bucket = s3bucket)
    },
    error = function(e) {
      e
    }
  )
}

# Cases, deaths, hosp scores: needed for "actual"s
cases <- bind_rows(
  readbucket("score_cards_nation_cases.rds"),
  readbucket("score_cards_state_cases.rds")
)
deaths <- bind_rows(
  readbucket("score_cards_nation_deaths.rds"),
  readbucket("score_cards_state_deaths.rds")
)
hosp <- bind_rows(
  readbucket("score_cards_nation_hospitalizations.rds"),
  readbucket("score_cards_state_hospitalizations.rds")
)

# The big one: predictions from all forecasters
pred <- readbucket("predictions_cards.rds")

# Cases
pred_cases <- pred %>%
  filter(signal == "confirmed_incidence_num") %>%
  mutate(signal = NULL, data_source = NULL, incidence_period = NULL) %>%
  pivot_wider(
    names_from = quantile,
    values_from = value,
    names_prefix = "forecast_"
  )

actual_cases <- cases %>%
  select(ahead, geo_value, forecaster, forecast_date, target_end_date, actual)

joined_cases <- left_join(pred_cases, actual_cases)
sum(is.na(actual_cases$actual)) == sum(is.na(joined_cases$actual))
write.csv(joined_cases, "cases.csv")

# Deaths
pred_deaths <- pred %>%
  filter(signal == "deaths_incidence_num") %>%
  mutate(signal = NULL, data_source = NULL, incidence_period = NULL) %>%
  pivot_wider(
    names_from = quantile,
    values_from = value,
    names_prefix = "forecast_"
  )

actual_deaths <- deaths %>%
  select(ahead, geo_value, forecaster, forecast_date, target_end_date, actual)

joined_deaths <- left_join(pred_deaths, actual_deaths)
sum(is.na(actual_deaths$actual)) == sum(is.na(joined_deaths$actual))
write.csv(joined_deaths, "deaths.csv")

# Hospitalizations: break up by weeks since we run into memory errors o/w!
pred_hosp <- actual_hosp <- joined_hosp <- vector(mode = "list", length = 4)
for (k in 1:4) {
  cat(k, "... ")
  days <- (k - 1) * 7 + 1:7
  pred_hosp[[k]] <- pred %>%
    filter(signal == "confirmed_admissions_covid_1d", ahead %in% days) %>%
    mutate(signal = NULL, data_source = NULL, incidence_period = NULL) %>%
    pivot_wider(
      names_from = quantile,
      values_from = value,
      names_prefix = "forecast_"
    )

  actual_hosp[[k]] <- hosp %>%
    filter(ahead %in% days) %>%
    select(ahead, geo_value, forecaster, forecast_date, target_end_date, actual)

  joined_hosp[[k]] <- left_join(pred_hosp[[k]], actual_hosp[[k]])
  cat(sum(is.na(actual_hosp[[k]]$act)) == sum(is.na(joined_hosp[[k]]$act)))
  write.csv(joined_hosp[[k]], sprintf("hospitalizations_%iwk.csv", k))
}
