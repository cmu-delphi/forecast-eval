check_for_missing_forecasters <- function(predictions_cards, forecasters_list, geo_type, signal_name, output_dir) {
  output_file_name <- generate_score_card_file_path(geo_type, signal_name, output_dir)
  previous_run_forecasters <- fread(output_file_name, data.table = FALSE) %>%
    filter(signal == signal_name) %>%
    distinct(forecaster) %>%
    pull()
  current_run_forecasters <- predictions_cards %>%
    filter(signal == signal_name) %>%
    distinct(forecaster) %>%
    pull()

  # Find forecasters included in the previous run (based on which ones are
  # included in the relevant score file downloaded from the S3 bucket) that are
  # not in the current run.
  missing_forecasters <- setdiff(previous_run_forecasters, current_run_forecasters)
  assert_that(length(missing_forecasters) == 0,
    msg = paste(
      paste(missing_forecasters, collapse = ", "),
      "were available in the most recent pipeline run but are no longer present for",
      geo_type, signal_name
    )
  )
}
