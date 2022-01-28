check_for_missing_forecasters <- function(predictions_cards, forecasters_list, geo_type, signal_name, output_dir) {
  output_file_name <- generate_score_card_file_path(geo_type, signal_name, output_dir)
  previous_run_forecasters <- readRDS(output_file_name) %>%
    filter(signal == signal_name) %>%
    distinct(forecaster) %>%
    pull()
  current_run_forecasters <- predictions_cards %>% 
    filter(signal == signal_name) %>%
    distinct(forecaster) %>%
    pull()
  
  # Find forecasters we asked for that weren't produced. This just prints a 
  # message because we already know that some forecasters, like CDDEP-ABM,
  # aren't available.
  missing_forecasters <- setdiff(forecasters_list, current_run_forecasters)
  if (length(missing_forecasters) != 0) {
    print(paste(
      paste(missing_forecasters, collapse = ", "), 
      "were asked for but not generated")
    )
  }
  
  # Find forecasters included in the previous run (based on which ones are
  # included in the relevant score file downloaded from the S3 bucket) that are
  # not in the current run.
  missing_forecasters <- setdiff(previous_run_forecasters, current_run_forecasters)
  assert_that(length(missing_forecasters) == 0,
         msg = paste(
           paste(missing_forecasters, collapse = ", "),
           "were available in the most recent pipeline run but are no longer present")
  )
}
