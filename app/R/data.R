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


getAllData <- function(loadFile, targetVariable) {
  df <- switch(targetVariable,
    "Deaths" = bind_rows(
      loadFile("score_cards_state_deaths.rds"),
      loadFile("score_cards_nation_deaths.rds")
    ),
    "Cases" = bind_rows(
      loadFile("score_cards_state_cases.rds"),
      loadFile("score_cards_nation_cases.rds")
    ),
    "Hospitalizations" = bind_rows(
      loadFile("score_cards_state_hospitalizations.rds"),
      loadFile("score_cards_nation_hospitalizations.rds")
    )
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
  # Cached connection info
  s3bucket <- getS3Bucket()
  s3DataFetcher <- createS3DataFactory(s3bucket)
  s3Contents <- s3bucket[attr(s3bucket, "names", exact = TRUE)]

  # Cached data
  df_list <- list()
  dataCreationDate <- as.Date(NA)

  getRecentData <- function(targetVariable = TARGET_OPTIONS) {
    targetVariable <- match.arg(targetVariable)

    newS3bucket <- getS3Bucket()
    newS3Contents <- newS3bucket[attr(newS3bucket, "names", exact = TRUE)]
    s3BucketHasChanged <- !identical(s3Contents, newS3Contents)

    # Fetch new data if contents of S3 bucket has changed (including file
    # names, sizes, and last modified timestamps). Ignores characteristics of
    # bucket and request, including bucket region, name, content type,
    # request date, request ID, etc.
    #
    # Save new score data and new bucket connection info to vars in env of
    # `createS3DataLoader`. They persist between calls to `getRecentData` a
    # la https://stackoverflow.com/questions/1088639/static-variables-in-r
    if (s3BucketHasChanged) {
      s3bucket <<- newS3bucket
      s3DataFetcher <<- createS3DataFactory(newS3bucket)
      s3Contents <<- newS3Contents
    }
    if (s3BucketHasChanged ||
      !(targetVariable %in% names(df_list)) ||
      nrow(df_list[[targetVariable]]) == 0) {
      df_list[[targetVariable]] <<- getAllData(s3DataFetcher, targetVariable)
      dataCreationDate <<- getCreationDate(s3DataFetcher)
    }

    return(list(df_list = df_list, dataCreationDate = dataCreationDate))
  }

  return(getRecentData)
}


#' create a data loader with fallback data only
createFallbackDataLoader <- function() {
  df_list <- list()
  for (targetVariable in TARGET_OPTIONS) {
    df_list[[targetVariable]] <- getAllData(getFallbackData, targetVariable)
  }
  dataCreationDate <- getCreationDate(getFallbackData)

  dataLoader <- function() {
    return(list(df_list = df_list, dataCreationDate = dataCreationDate))
  }

  return(dataLoader)
}


createDataLoader <- createS3DataLoader
