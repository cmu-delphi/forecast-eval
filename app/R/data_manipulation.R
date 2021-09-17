
renameScoreCol <- function(filteredScoreDf, scoreType, coverageInterval) {
  if (scoreType == "wis") {
    filteredScoreDf <- filteredScoreDf %>% rename(Score = wis)
  } else if (scoreType == "sharpness") {
    filteredScoreDf <- filteredScoreDf %>% rename(Score = sharpness)
  } else if (scoreType == "ae") {
    filteredScoreDf <- filteredScoreDf %>% rename(Score = ae)
  } else if (scoreType == "coverage") {
    filteredScoreDf <- filteredScoreDf %>% rename(Score = !!coverageInterval)
  }
  return(filteredScoreDf)
}


filterOverAllLocations <- function(filteredScoreDf, scoreType, hasAsOfData = FALSE) {
  locationsIntersect <- list()
  filteredScoreDf <- filteredScoreDf %>% filter(!is.na(Score))
  # Create df with col for all locations across each unique date, ahead and forecaster combo
  locationDf <- filteredScoreDf %>%
    group_by(forecaster, target_end_date, ahead) %>%
    summarize(location_list = paste(sort(unique(geo_value)), collapse = ","))
  locationDf <- locationDf %>% filter(location_list != c("us"))
  # Create a list containing each row's location list
  locationList <- sapply(locationDf$location_list, function(x) strsplit(x, ","))
  locationList <- lapply(locationList, function(x) x[x != "us"])
  # Get the intersection of all the locations in these lists
  locationsIntersect <- unique(Reduce(intersect, locationList))
  filteredScoreDf <- filteredScoreDf %>% filter(geo_value %in% locationsIntersect)
  if (scoreType == "coverage") {
    if (hasAsOfData) {
      filteredScoreDf <- filteredScoreDf %>%
        group_by(forecaster, forecast_date, target_end_date, ahead) %>%
        summarize(
          Score = sum(Score) / length(locationsIntersect),
          actual = sum(actual), as_of_actual = sum(as_of_actual)
        )
    } else {
      filteredScoreDf <- filteredScoreDf %>%
        group_by(forecaster, forecast_date, target_end_date, ahead) %>%
        summarize(Score = sum(Score) / length(locationsIntersect), actual = sum(actual))
    }
  } else {
    if (hasAsOfData) {
      filteredScoreDf <- filteredScoreDf %>%
        group_by(forecaster, forecast_date, target_end_date, ahead) %>%
        summarize(Score = sum(Score), actual = sum(actual), as_of_actual = sum(as_of_actual))
    } else {
      filteredScoreDf <- filteredScoreDf %>%
        group_by(forecaster, forecast_date, target_end_date, ahead) %>%
        summarize(Score = sum(Score), actual = sum(actual))
    }
  }
  return(list(filteredScoreDf, locationsIntersect))
}

# Only use weekly aheads for hospitalizations
# May change in the future
filterHospitalizationsAheads <- function(scoreDf) {
  scoreDf["weekday"] <- weekdays(as.Date(scoreDf$target_end_date))
  scoreDf <- scoreDf %>% filter(weekday == HOSPITALIZATIONS_TARGET_DAY)

  oneAheadDf <- scoreDf %>%
    filter(ahead >= HOSPITALIZATIONS_OFFSET) %>%
    filter(ahead < 7 + HOSPITALIZATIONS_OFFSET) %>%
    group_by(target_end_date, forecaster) %>%
    filter(ahead == min(ahead)) %>%
    mutate(ahead = HOSPITALIZATIONS_AHEAD_OPTIONS[1])
  twoAheadDf <- scoreDf %>%
    filter(ahead >= 7 + HOSPITALIZATIONS_OFFSET) %>%
    filter(ahead < 14 + HOSPITALIZATIONS_OFFSET) %>%
    group_by(target_end_date, forecaster) %>%
    filter(ahead == min(ahead)) %>%
    mutate(ahead = HOSPITALIZATIONS_AHEAD_OPTIONS[2])
  threeAheadDf <- scoreDf %>%
    filter(ahead >= 14 + HOSPITALIZATIONS_OFFSET) %>%
    filter(ahead < 21 + HOSPITALIZATIONS_OFFSET) %>%
    group_by(target_end_date, forecaster) %>%
    filter(ahead == min(ahead)) %>%
    mutate(ahead = HOSPITALIZATIONS_AHEAD_OPTIONS[3])
  fourAheadDf <- scoreDf %>%
    filter(ahead >= 21 + HOSPITALIZATIONS_OFFSET) %>%
    filter(ahead < 28 + HOSPITALIZATIONS_OFFSET) %>%
    group_by(target_end_date, forecaster) %>%
    filter(ahead == min(ahead)) %>%
    mutate(ahead = HOSPITALIZATIONS_AHEAD_OPTIONS[4])

  return(rbind(oneAheadDf, twoAheadDf, threeAheadDf, fourAheadDf))
}
