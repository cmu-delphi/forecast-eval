library(dplyr)
library(tidyr)
library(aws.s3)
library(data.table)

Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
s3bucket <- tryCatch(
  {
    aws.s3::get_bucket(bucket = "forecast-eval")
  },
  error = function(e) {
    e
  }
)

readbucket <- function(name) {
  tryCatch(
    {
      s3read_using(fread, data.table = FALSE, object = name, bucket = s3bucket)
    },
    error = function(e) {
      e
    }
  )
}

# Cases, deaths, hosp scores: needed for "actual"s
cases <- bind_rows(
  readbucket("score_cards_nation_cases.csv.gz"),
  readbucket("score_cards_state_cases.csv.gz")
)
deaths <- bind_rows(
  readbucket("score_cards_nation_deaths.csv.gz"),
  readbucket("score_cards_state_deaths.csv.gz")
)
hosp <- bind_rows(
  readbucket("score_cards_nation_hospitalizations.csv.gz"),
  readbucket("score_cards_state_hospitalizations.csv.gz")
)

# Cases
pred <- readbucket("predictions_cards_confirmed_incidence_num.csv.gz")
pred_cases <- pred %>%
  mutate(signal = NULL, data_source = NULL, incidence_period = NULL) %>%
  pivot_wider(
    names_from = quantile,
    values_from = value,
    names_prefix = "forecast_"
  )

rm(pred)
gc()

actual_cases <- cases %>%
  select(ahead, geo_value, forecaster, forecast_date, target_end_date, actual)

joined_cases <- left_join(pred_cases, actual_cases)
sum(is.na(actual_cases$actual)) == sum(is.na(joined_cases$actual))
write.csv(joined_cases, "cases.csv")

# Deaths
pred <- readbucket("predictions_cards_deaths_incidence_num.csv.gz")
pred_deaths <- pred %>%
  mutate(signal = NULL, data_source = NULL, incidence_period = NULL) %>%
  pivot_wider(
    names_from = quantile,
    values_from = value,
    names_prefix = "forecast_"
  )

rm(pred)
gc()

actual_deaths <- deaths %>%
  select(ahead, geo_value, forecaster, forecast_date, target_end_date, actual)

joined_deaths <- left_join(pred_deaths, actual_deaths)
sum(is.na(actual_deaths$actual)) == sum(is.na(joined_deaths$actual))
write.csv(joined_deaths, "deaths.csv")

# Hospitalizations: break up by weeks since we run into memory errors o/w!
pred <- readbucket("predictions_cards_confirmed_admissions_covid_1d.csv.gz")
pred_hosp <- actual_hosp <- joined_hosp <- vector(mode = "list", length = 4)
for (k in 1:4) {
  cat(k, "... ")
  days <- (k - 1) * 7 + 1:7
  pred_hosp[[k]] <- pred %>%
    filter(ahead %in% days) %>%
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
