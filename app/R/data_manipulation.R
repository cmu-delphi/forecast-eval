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


filterOverAllLocations <- function(filteredScoreDf, scoreType, hasAsOfData = FALSE, filterDate) {
  locationsIntersect <- list()
  filteredScoreDf <- filter(filteredScoreDf, !is.na(Score) | target_end_date >= filterDate)
  # Create df with col for all locations across each unique date, ahead and forecaster combo
  locationDf <- filteredScoreDf %>%
    group_by(forecaster, target_end_date, ahead) %>%
    summarize(location_list = paste(sort(unique(geo_value)), collapse = ","))
  locationDf <- filter(locationDf, location_list != c("us"))
  # Create a list containing each row's location list
  locationList <- sapply(locationDf$location_list, function(x) strsplit(x, ","))
  locationList <- lapply(locationList, function(x) x[x != "us"])
  # Get the intersection of all the locations in these lists
  locationsIntersect <- unique(Reduce(intersect, locationList))
  filteredScoreDf <- filter(filteredScoreDf, geo_value %in% locationsIntersect)
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
  scoreDf["weekday"] <- format(as.Date(scoreDf$target_end_date, '%Y-%m-%d'), "%A")
  scoreDf <- filter(scoreDf, weekday == HOSPITALIZATIONS_TARGET_DAY)
  scoreDf$ahead_group <- case_when(
    scoreDf$ahead >= HOSPITALIZATIONS_OFFSET & scoreDf$ahead < 7 + HOSPITALIZATIONS_OFFSET ~ 1L,
    scoreDf$ahead >= 7 + HOSPITALIZATIONS_OFFSET & scoreDf$ahead < 14 + HOSPITALIZATIONS_OFFSET ~ 2L,
    scoreDf$ahead >= 14 + HOSPITALIZATIONS_OFFSET & scoreDf$ahead < 21 + HOSPITALIZATIONS_OFFSET ~ 3L,
    scoreDf$ahead >= 21 + HOSPITALIZATIONS_OFFSET & scoreDf$ahead < 28 + HOSPITALIZATIONS_OFFSET ~ 4L,
    TRUE ~ NA_integer_
  )

  return(
    scoreDf %>%
      filter(!is.na(ahead_group)) %>%
      group_by(target_end_date, forecaster, ahead_group) %>%
      filter(ahead == min(ahead)) %>%
      mutate(ahead = HOSPITALIZATIONS_AHEAD_OPTIONS[ahead_group])
  )
}
