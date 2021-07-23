
COVERAGE_INTERVALS = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "95", "98")
DEATH_FILTER = "deaths_incidence_num"
CASE_FILTER = "confirmed_incidence_num"
HOSPITALIZATIONS_FILTER = "confirmed_admissions_covid_1d"
HOSPITALIZATIONS_TARGET_DAY = "Wednesday"
TOTAL_LOCATIONS = "Totaled Over States*"
AHEAD_OPTIONS = c(1,2,3,4)

# Num days to offset the forecast week by
# Example: if HOSPITALIZATIONS_TARGET_DAY is Wednesday and HOSPITALIZATIONS_OFFSET is 2,
# ahead 1 has to have forecast date of Monday or earlier,
# ahead 2 has to have forecast date of Monday + 7 days or earlier (offset + 7 days or more), etc
HOSPITALIZATIONS_OFFSET = 2
HOSPITALIZATIONS_AHEAD_OPTIONS = c(HOSPITALIZATIONS_OFFSET, HOSPITALIZATIONS_OFFSET + 7, HOSPITALIZATIONS_OFFSET + 14, HOSPITALIZATIONS_OFFSET + 21)


renameScoreCol = function(filteredScoreDf, scoreType, coverageInterval) {
  if (scoreType == "wis") {
    filteredScoreDf <- filteredScoreDf %>% rename(Score = wis)
  }
  else if (scoreType == "sharpness") {
    filteredScoreDf <- filteredScoreDf %>% rename(Score = sharpness)
  }
  else if (scoreType == "ae") {
    filteredScoreDf <- filteredScoreDf %>% rename(Score = ae)
  }
  else if (scoreType == "coverage") {
    filteredScoreDf <- filteredScoreDf %>% rename(Score = !!coverageInterval)
  }
  return (filteredScoreDf)
}


filterOverAllLocations = function(filteredScoreDf, scoreType) {
    locationsIntersect = list()
    filteredScoreDf = filteredScoreDf %>% filter(!is.na(Score))
    # Create df with col for all locations across each unique date, ahead and forecaster combo
    locationDf = filteredScoreDf %>% group_by(forecaster, target_end_date, ahead) %>%
      summarize(location_list = paste(sort(unique(geo_value)),collapse=","))
    locationDf = locationDf %>% filter(location_list != c('us'))
    # Create a list containing each row's location list
    locationList = sapply(locationDf$location_list, function(x) strsplit(x, ","))
    locationList = lapply(locationList, function(x) x[x != 'us'])
    # Get the intersection of all the locations in these lists
    locationsIntersect = unique(Reduce(intersect, locationList))
    filteredScoreDf = filteredScoreDf %>% filter(geo_value %in% locationsIntersect)
    if (scoreType == "coverage") {
      filteredScoreDf = filteredScoreDf %>%
        group_by(forecaster, forecast_date, target_end_date, ahead) %>%
        summarize(Score = sum(Score)/length(locationsIntersect), actual = sum(actual))
    }
    else {
      filteredScoreDf = filteredScoreDf %>%
        group_by(forecaster, forecast_date, target_end_date, ahead) %>%
        summarize(Score = sum(Score), actual = sum(actual))
    }
    return (list(filteredScoreDf, locationsIntersect))
}

# Only use weekly aheads for hospitalizations
# May change in the future
filterHospitalizationsAheads = function(scoreDf) {
  scoreDf['weekday'] = weekdays(as.Date(scoreDf$target_end_date))
  scoreDf = scoreDf %>% filter(weekday == HOSPITALIZATIONS_TARGET_DAY)
  
  oneAheadDf = scoreDf %>% filter(ahead >= HOSPITALIZATIONS_OFFSET) %>% filter(ahead < 7 + HOSPITALIZATIONS_OFFSET)  %>%
    group_by(target_end_date, forecaster) %>% filter(ahead == min(ahead)) %>%
    mutate(ahead = HOSPITALIZATIONS_AHEAD_OPTIONS[1])
  twoAheadDf = scoreDf %>% filter(ahead >= 7 + HOSPITALIZATIONS_OFFSET) %>% filter(ahead < 14 + HOSPITALIZATIONS_OFFSET) %>%
    group_by(target_end_date, forecaster) %>% filter(ahead == min(ahead)) %>%
    mutate(ahead = HOSPITALIZATIONS_AHEAD_OPTIONS[2])
  threeAheadDf = scoreDf %>% filter(ahead >= 14 + HOSPITALIZATIONS_OFFSET) %>% filter(ahead < 21 + HOSPITALIZATIONS_OFFSET) %>%
    group_by(target_end_date, forecaster) %>% filter(ahead == min(ahead)) %>%
    mutate(ahead = HOSPITALIZATIONS_AHEAD_OPTIONS[3])
  fourAheadDf = scoreDf %>% filter(ahead >= 21 + HOSPITALIZATIONS_OFFSET) %>% filter(ahead < 28 + HOSPITALIZATIONS_OFFSET) %>%
    group_by(target_end_date, forecaster) %>% filter(ahead == min(ahead)) %>%
    mutate(ahead = HOSPITALIZATIONS_AHEAD_OPTIONS[4])
  
  return(rbind(oneAheadDf, twoAheadDf, threeAheadDf, fourAheadDf))
}