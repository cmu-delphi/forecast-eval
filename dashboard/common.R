
COVERAGE_INTERVALS = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "95", "98")
DEATH_FILTER = "deaths_incidence_num"
CASE_FILTER = "confirmed_incidence_num"
TOTAL_LOCATIONS = "Totaled Over States*"

# Num days to offset the forecast week by
# Example: if HOSPITALIZATIONS_TARGET_DAY is Wednesday and HOSPITALIZATIONS_OFFSET is 2,
# ahead 1 has to have forecast date of Monday or earlier,
# ahead 2 has to have forecast date of Monday + 7 days or earlier (offset + 7 days or more), etc
HOSPITALIZATIONS_OFFSET = 2
HOSPITALIZATIONS_AHEAD_OPTIONS = c(HOSPITALIZATIONS_OFFSET, HOSPITALIZATIONS_OFFSET + 7, HOSPITALIZATIONS_OFFSET + 14, HOSPITALIZATIONS_OFFSET + 21)

