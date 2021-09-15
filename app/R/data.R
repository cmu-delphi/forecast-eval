
# Get and prepare data
getS3Bucket <- function() {
  # Connect to AWS s3bucket
  Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
  s3bucket <- tryCatch(
    {
      get_bucket(bucket = "forecast-eval")
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
        s3readRDS(object = filename, bucket = s3bucket)
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

getFallbackData <- function(filename) {
  path <- ifelse(
    file.exists(filename),
    filename,
    file.path("../dist/", filename)
  )
  readRDS(path)
}

getAllData <- function(s3bucket) {
  dfStateCases <- getData("score_cards_state_cases.rds", s3bucket)
  dfStateDeaths <- getData("score_cards_state_deaths.rds", s3bucket)
  dfNationCases <- getData("score_cards_nation_cases.rds", s3bucket)
  dfNationDeaths <- getData("score_cards_nation_deaths.rds", s3bucket)
  dfStateHospitalizations <- getData("score_cards_state_hospitalizations.rds", s3bucket)
  dfNationHospitalizations <- getData("score_cards_nation_hospitalizations.rds", s3bucket)

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
  dfNationCases <- dfNationCases %>% select(all_of(expectedCols))
  dfNationDeaths <- dfNationDeaths %>% select(all_of(expectedCols))
  dfStateHospitalizations <- dfStateHospitalizations %>% select(all_of(expectedCols))
  dfNationHospitalizations <- dfNationHospitalizations %>% select(all_of(expectedCols))

  df <- rbind(dfStateCases, dfStateDeaths, dfNationCases, dfNationDeaths, dfStateHospitalizations, dfNationHospitalizations)
  df <- df %>% rename("10" = cov_10, "20" = cov_20, "30" = cov_30, "40" = cov_40, "50" = cov_50, "60" = cov_60, "70" = cov_70, "80" = cov_80, "90" = cov_90, "95" = cov_95, "98" = cov_98)

  return(df)
}

createDataLoader <- function() {
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
      df <<- getAllData(s3bucket)
    }

    return(df)
  }

  return(getRecentData)
}