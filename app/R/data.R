library(aws.s3)

# Set application-level caching location. Stores up to 1GB of caches. Removes
# least recently used objects first.
shinyOptions(cache = cachem::cache_mem(max_size = 1000 * 1024^2, evict = "lru"))
cache <- getShinyOption("cache")

# Since covidcast data updates about once a day, add date arg to
# covidcast_signal so caches aren't used after that.
covidcast_signal_mem <- function(..., date = Sys.Date()) {
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


getCreationDate <- function(loadFile) {
  dataCreationDate <- loadFile("datetime_created_utc.rds")
  return(dataCreationDate %>% pull(datetime) %>% as.Date())
}


getAllData <- function(loadFile) {
  dfStateHospitalizations <- loadFile("score_cards_state_hospitalizations.rds")
  dfNationHospitalizations <- loadFile("score_cards_nation_hospitalizations.rds")
  dfStateCases <- loadFile("score_cards_state_cases.rds")
  dfStateDeaths <- loadFile("score_cards_state_deaths.rds")
  dfNationCases <- loadFile("score_cards_nation_cases.rds")
  dfNationDeaths <- loadFile("score_cards_nation_deaths.rds")
  df <- bind_rows(
    dfStateHospitalizations,
    dfNationHospitalizations,
    dfStateCases,
    dfStateDeaths,
    dfNationCases,
    dfNationDeaths
  )
  
  # The names of the `covCols` elements become the new names of those columns
  # when we use this vector in the `select` below.
  covCols <- setNames(paste0("cov_", COVERAGE_INTERVALS), COVERAGE_INTERVALS)
  keepCols <- c(
    "ahead", "geo_value", "forecaster", "forecast_date",
    "data_source", "signal", "target_end_date", "incidence_period",
    "actual", "wis", "sharpness", "ae", "value_50",
    covCols
  )
  df <- select(df, all_of(keepCols))

  return(df)
}

createS3DataLoader <- function() {
  s3bucket <- getS3Bucket()
  df <- data.frame()
  dataCreationDate <- as.Date(NA)

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
      dataCreationDate <<- getCreationDate(createS3DataFactory(s3bucket))
    }

    return(list(df = df, dataCreationDate = dataCreationDate))
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
