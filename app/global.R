library(shiny)
library(shinyjs)
library(plotly)
library(tidyr)
library(purrr)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(viridis)
library(tsibble)
library(covidcast)
library(data.table)

appVersion <- "7.3.0"

COVERAGE_INTERVALS <- c("10", "20", "30", "40", "50", "60", "70", "80", "90", "95", "98")
CASES_DEATHS_TARGET_DAY <- "Saturday"
HOSPITALIZATIONS_TARGET_DAY <- "Wednesday"
TOTAL_LOCATIONS <- "Totaled Over States*"
AHEAD_OPTIONS <- c(1, 2, 3, 4)

INIT_SCORE_TYPE <- "wis"
INIT_TARGET <- "Hospitalizations"
TARGET_OPTIONS <- c("Deaths", "Cases", "Hospitalizations")

# Num days to offset the forecast week by
# Example: if HOSPITALIZATIONS_TARGET_DAY is Wednesday and HOSPITALIZATIONS_OFFSET is 2,
# ahead 1 has to have forecast date of Monday or earlier,
# ahead 2 has to have forecast date of Monday + 7 days or earlier (offset + 7 days or more), etc
HOSPITALIZATIONS_OFFSET <- 2
HOSPITALIZATIONS_AHEAD_OPTIONS <- c(
  HOSPITALIZATIONS_OFFSET, HOSPITALIZATIONS_OFFSET + 7,
  HOSPITALIZATIONS_OFFSET + 14, HOSPITALIZATIONS_OFFSET + 21
)

CURRENT_TAB_SUFFIX <- ""
ARCHIVE_TAB_SUFFIX <- "_archive"


TARGET_VARS_BY_TAB <- list()
TARGET_VARS_BY_TAB[[paste0("evaluations", CURRENT_TAB_SUFFIX)]] <- c(
  "Hospital Admissions" = "Hospitalizations"
)
TARGET_VARS_BY_TAB[[paste0("evaluations", ARCHIVE_TAB_SUFFIX)]] <- c(
  "Incident Deaths" = "Deaths",
  "Incident Cases" = "Cases"
)

# When RE_RENDER_TRUTH = TRUE
# summaryPlot will be called only to update TruthPlot
RE_RENDER_TRUTH <- FALSE

# USE_CURR_TRUTH indicates when we can use the previous TruthPlot
USE_CURR_TRUTH <- FALSE

# Earliest 'as of' date available from covidcast API
MIN_AVAIL_NATION_AS_OF_DATE <- as.Date("2020-04-02")
MIN_AVAIL_HOSP_AS_OF_DATE <- as.Date("2020-11-16")
MIN_AVAIL_TERRITORY_AS_OF_DATE <- as.Date("2020-04-02")

TERRITORIES <- c("AS", "GU", "MP", "VI")
STATE_ABB <- c(state.abb, TERRITORIES, "PR", "DC")
STATE_NAME <- c(state.name, "American Samoa", "Guam", "Northern Mariana Islands", "US Virgin Islands", "Puerto Rico", "District of Columbia")

resolveCurrentCasesDeathDay <- function() {
  # Get most recent target end date
  # Prev Saturday for Cases and Deaths, prev Wednesday for Hospitalizations
  # Since we don't upload new observed data until Sunday:
  # Use 7 and 1 for Cases and Deaths so that Sundays will use the Saturday directly beforehand.
  # (This means that on Sundays until the afternoon when the pipeline completes, the "as of" will show
  # the most recent Saturday / Wednesday date even though the actual updated data won't be there yet)
  prevWeek <- seq(Sys.Date() - 7, Sys.Date() - 1, by = "day")
  prevWeek[weekdays(prevWeek) == CASES_DEATHS_TARGET_DAY]
}

# Use 5 and 11 for Hospitalizations since Thurs-Sun should also not use the Wednesday directly beforehand.
# (This means that on Mondays until the afternoon when pipeline completes, the "as of" will show
# most recent Saturday / Wednesday date even though the actual updated data won't be there yet)


resolveCurrentHospDay <- function() {
  # Get most recent target end date
  # Prev Saturday for Cases and Deaths, prev Wednesday for Hospitalizations
  # Since we don't upload new observed data until Sunday:
  # Use 4 and 10 for Hospitalizations since Thurs-Sat should not use the Wednesday directly beforehand.
  # (This means that on Sundays until the afternoon when the pipeline completes, the "as of" will show
  # the most recent Saturday / Wednesday date even though the actual updated data won't be there yet)
  prevHospWeek <- seq(Sys.Date() - 10, Sys.Date() - 4, by = "day")
  prevHospWeek[weekdays(prevHospWeek) == HOSPITALIZATIONS_TARGET_DAY]
}
