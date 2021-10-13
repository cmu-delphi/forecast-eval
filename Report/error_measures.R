library(assertthat)

overprediction <- function(quantile, value, actual_value) {
  score_func_param_checker(quantile, value, actual_value, "overprediction")
  if (!is_symmetric(quantile)) {
    warning(paste0(
      "overprediction/underprediction/sharpness require",
      "symmetric quantile forecasts. Using NA."
    ))
    return(NA)
  }
  if (all(is.na(actual_value))) {
    return(NA)
  }
  actual_value <- unique(actual_value)

  lower <- value[!is.na(quantile) & quantile < .5]
  med <- value[find_quantile_match(quantile, 0.5)]

  if (length(med) > 1L) {
    return(NA)
  } else if (length(med) == 1L) {
    m <- (med - actual_value) * (med > actual_value)
  } else {
    m <- NULL
  }

  ans <- mean(c(
    rep((lower - actual_value) * (lower > actual_value), 2), m
  ))


  return(ans)
}

underprediction <- function(quantile, value, actual_value) {
  score_func_param_checker(quantile, value, actual_value, "underprediction")
  if (!is_symmetric(quantile)) {
    warning(paste0(
      "overprediction/underprediction/sharpness require",
      "symmetric quantile forecasts. Using NA."
    ))
    return(NA)
  }
  if (all(is.na(actual_value))) {
    return(NA)
  }
  actual_value <- unique(actual_value)

  upper <- value[!is.na(quantile) & quantile > .5]
  med <- value[find_quantile_match(quantile, 0.5)]

  if (length(med) > 1L) {
    return(NA)
  } else if (length(med) == 1L) {
    m <- (actual_value - med) * (med < actual_value)
  } else {
    m <- NULL
  }

  ans <- mean(c(
    rep((actual_value - upper) * (upper < actual_value), 2), m
  ))

  return(ans)
}

sharpness <- function(quantile, value, actual_value) {
  weighted_interval_score(quantile, value, actual_value) -
    overprediction(quantile, value, actual_value) -
    underprediction(quantile, value, actual_value)
}

# Utility functions required from evalcast that are not exported

is_symmetric <- function(x, tol = 1e-8) {
  x <- sort(x)
  all(abs(x + rev(x) - 1) < tol)
}

find_quantile_match <- function(quantiles, val_to_match, tol = 1e-8) {
  return(abs(quantiles - val_to_match) < tol & !is.na(quantiles))
}

get_quantile_prediction_factory <- function(val_to_match, tol = 1e-8) {
  get_quantile_prediction <- function(quantile, value, actual_value) {
    if (all(is.na(quantile))) {
      return(NA)
    }

    value <- value[!is.na(quantile)]
    quantile <- quantile[!is.na(quantile)]

    val <- value[find_quantile_match(quantile, val_to_match, tol)]

    if (length(val) != 1L) {
      return(NA)
    }

    return(val)
  }

  return(get_quantile_prediction)
}

score_func_param_checker <- function(quantiles, values, actual_value, id = "") {
  id_str <- paste0(id, ": ")
  if (length(actual_value) > 1) {
    assert_that(length(actual_value) == length(values),
      msg = paste0(
        id_str,
        "actual_value must be a scalar or the same length",
        " as values"
      )
    )
    actual_value <- unique(actual_value)
  }
  assert_that(length(actual_value) == 1,
    msg = paste0(
      id_str,
      "actual_value must have exactly 1 unique value"
    )
  )
  assert_that(length(quantiles) == length(values),
    msg = paste0(
      id_str,
      "quantiles and values must be of the same length"
    )
  )
  assert_that(!any(duplicated(quantiles)),
    msg = paste0(
      id_str,
      "quantiles must be unique."
    )
  )
}
