library(shiny)
library(shinyjs)
library(tools)
library(bslib)
library(plotly)
library(tidyr)
library(dplyr)
library(lubridate)
library(viridis)
library(tsibble)
library(covidcast)
library(stringr)

appVersion <- "4.0.0"

COVERAGE_INTERVALS <- c("10", "20", "30", "40", "50", "60", "70", "80", "90", "95", "98")
DEATH_FILTER <- "deaths_incidence_num"
CASE_FILTER <- "confirmed_incidence_num"
CASES_DEATHS_TARGET_DAY <- "Saturday"
HOSPITALIZATIONS_FILTER <- "confirmed_admissions_covid_1d"
HOSPITALIZATIONS_TARGET_DAY <- "Wednesday"
TOTAL_LOCATIONS <- "Totaled Over States*"
AHEAD_OPTIONS <- c(1, 2, 3, 4)

# Num days to offset the forecast week by
# Example: if HOSPITALIZATIONS_TARGET_DAY is Wednesday and HOSPITALIZATIONS_OFFSET is 2,
# ahead 1 has to have forecast date of Monday or earlier,
# ahead 2 has to have forecast date of Monday + 7 days or earlier (offset + 7 days or more), etc
HOSPITALIZATIONS_OFFSET <- 2
HOSPITALIZATIONS_AHEAD_OPTIONS <- c(
  HOSPITALIZATIONS_OFFSET, HOSPITALIZATIONS_OFFSET + 7,
  HOSPITALIZATIONS_OFFSET + 14, HOSPITALIZATIONS_OFFSET + 21
)

# Earliest 'as of' date available from covidcast API
MIN_AVAIL_NATION_AS_OF_DATE <- as.Date("2021-01-09")
MIN_AVAIL_HOSP_AS_OF_DATE <- as.Date("2020-11-11")
MIN_AVAIL_TERRITORY_AS_OF_DATE <- as.Date("2021-02-10")

TERRITORIES <- c("AS", "GU", "MP", "VI")

resolveCurrentCasesDeathDay <- function() {
  # Get most recent target end date
  # Prev Saturday for Cases and Deaths, prev Wednesday for Hospitalizations
  # Since we don't upload new observed data until Monday:
  # Use 8 and 2 for Cases and Deaths so that Sundays will not use the Saturday directly beforehand
  # since we don't have data for it yet.
  prevWeek <- seq(Sys.Date() - 8, Sys.Date() - 2, by = "day")
  prevWeek[weekdays(prevWeek) == CASES_DEATHS_TARGET_DAY]
}

# Use 5 and 11 for Hospitalizations since Thurs-Sun should also not use the Wednesday directly beforehand.
# (This means that on Mondays until the afternoon when pipeline completes, the "as of" will show
# most recent Saturday / Wednesday date even though the actual updated data won't be there yet)


resolveCurrentHospDay <- function() {
  # Get most recent target end date
  # Prev Saturday for Cases and Deaths, prev Wednesday for Hospitalizations
  # Since we don't upload new observed data until Monday:
  # Use 5 and 11 for Hospitalizations since Thurs-Sun should also not use the Wednesday directly beforehand.
  # (This means that on Mondays until the afternoon when pipeline completes, the "as of" will show
  # most recent Saturday / Wednesday date even though the actual updated data won't be there yet)
  prevHospWeek <- seq(Sys.Date() - 11, Sys.Date() - 5, by = "day")
  prevHospWeek[weekdays(prevHospWeek) == HOSPITALIZATIONS_TARGET_DAY]
}
