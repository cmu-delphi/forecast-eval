library(aws.s3)

# Set application-level caching location. Stores up to 1GB of caches. Removes
# least recently used objects first.
shinyOptions(cache = cachem::cache_mem(max_size = 1000 * 1024^2, evict="lru"))
cache <- getShinyOption("cache")

# Since covidcast data updates about once a day. Add date arg to
# covidcast_signal so caches aren't used after that.
covidcast_signal_mem <- function(..., date=Sys.Date()) {
  return(covidcast_signal(...))
}
covidcast_signal_mem <- memoise::memoise(covidcast_signal_mem, cache = cache)

# Get and prepare data
getS3Bucket <- function() {
  # Connect to AWS s3bucket
  Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
  s3bucket <- tryCatch(
    {
      aws.s3::get_bucket(bucket = "forecast-eval")
    },
    error = function(e) {
      e
      return(NULL)
    }
  )

  return(s3bucket)
}

getData <- function(filename, s3bucket) {
  if (!is.null(s3bucket)) {
    tryCatch(
      {
        aws.s3::s3readRDS(object = filename, bucket = s3bucket)
      },
      error = function(e) {
        e
        getFallbackData(filename)
      }
    )
  } else {
    getFallbackData(filename)
  }
}

createS3DataFactory <- function(s3bucket) {
  function(filename) {
    getData(filename, s3bucket)
  }
}

getFallbackData <- function(filename) {
  path <- ifelse(
    file.exists(filename),
    filename,
    file.path("../dist/", filename)
  )
  readRDS(path)
}

getAllData <- function(loadFile) {
  dfStateCases <- loadFile("score_cards_state_cases.rds")
  dfStateDeaths <- loadFile("score_cards_state_deaths.rds")
  dfStateHospitalizations <- loadFile("score_cards_state_hospitalizations.rds")
  dfNationCases <- loadFile("score_cards_nation_cases.rds")
  dfNationDeaths <- loadFile("score_cards_nation_deaths.rds")
  dfNationHospitalizations <- loadFile("score_cards_nation_hospitalizations.rds")

  # Pick out expected columns only
  covCols <- paste0("cov_", COVERAGE_INTERVALS)
  expectedCols <- c(
    "ahead", "geo_value", "forecaster", "forecast_date",
    "data_source", "signal", "target_end_date", "incidence_period",
    "actual", "wis", "sharpness", "ae", "value_50",
    covCols
  )

  dfStateCases <- dfStateCases %>% select(all_of(expectedCols))
  dfStateDeaths <- dfStateDeaths %>% select(all_of(expectedCols))
  dfStateHospitalizations <- dfStateHospitalizations %>% select(all_of(expectedCols))
  dfNationCases <- dfNationCases %>% select(all_of(expectedCols))
  dfNationDeaths <- dfNationDeaths %>% select(all_of(expectedCols))
  dfNationHospitalizations <- dfNationHospitalizations %>% select(all_of(expectedCols))

  df <- rbind(
    dfStateCases, dfStateDeaths, dfStateHospitalizations,
    dfNationCases, dfNationDeaths, dfNationHospitalizations
  )
  df <- df %>% rename(
    "10" = cov_10, "20" = cov_20, "30" = cov_30,
    "40" = cov_40, "50" = cov_50, "60" = cov_60, "70" = cov_70,
    "80" = cov_80, "90" = cov_90, "95" = cov_95, "98" = cov_98
  )

  return(df)
}

createS3DataLoader <- function() {
  s3bucket <- getS3Bucket()
  df <- data.frame()

  getRecentData <- function() {
    newS3bucket <- getS3Bucket()

    s3Contents <- s3bucket[attr(s3bucket, "names", exact = TRUE)]
    newS3Contents <- newS3bucket[attr(newS3bucket, "names", exact = TRUE)]

    # Fetch new score data if contents of S3 bucket has changed (including file
    # names, sizes, and last modified timestamps). Ignores characteristics of
    # bucket and request, including bucket region, name, content type, request
    # date, request ID, etc.
    if (nrow(df) == 0 || !identical(s3Contents, newS3Contents)) {
      # Save new data and new bucket connection info to vars in env of
      # `getRecentDataHelper`. They persist between calls to `getRecentData` a
      # la https://stackoverflow.com/questions/1088639/static-variables-in-r
      s3bucket <<- newS3bucket
      df <<- getAllData(createS3DataFactory(s3bucket))
    }

    return(df)
  }

  return(getRecentData)
}


#' create a data loader with fallback data only
createFallbackDataLoader <- function() {
  df <- getAllData(getFallbackData)

  dataLoader <- function() {
    df
  }
  dataLoader
}


createDataLoader <- createS3DataLoader
