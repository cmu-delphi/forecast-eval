download_recent_forecasts <- function(forecasters, state_geos, github_token) {
	## Get list of new and modified files to download
	# The `path` field filters commits to only those that modifying the listed dir
	BASE_URL <- "https://api.github.com/repos/reichlab/covid19-forecast-hub/commits?sha=%s&per_page=%s&path=data-processed&since=%s&page=%s"
	ITEMS_PER_PAGE <- 100
	MAX_ITEMS_PER_PAGE <- 100
	BRANCH <- "master"

	# We want to fetch all commits made since the previous run. Add 1 day in as buffer.
	#
	# Timestamp should be in ISO 8601 format. See
	# https://docs.github.com/en/rest/reference/commits#list-commits--parameters for
	# details.
	previous_run_ts <- readRDS(file.path(output_dir, "datetime_created_utc.rds")) %>%
	  pull(datetime)
	since_date <- strftime(previous_run_ts - days(1), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

	page <- 0
	commit_sha_dates <- list()

	# Fetch list of commits from API, one page at a time. Each page contains up to
	# 100 commits. If a page contains exactly 100 commits, assume that there are more
	# results and fetch the next page.
	while (!exists("temp_commits") || nrow(temp_commits) == MAX_ITEMS_PER_PAGE) {
	  page <- page + 1
	  # Construct the URL
	  commits_url <- sprintf(BASE_URL, BRANCH, ITEMS_PER_PAGE, since_date, page)

	  request <- GET(commits_url, add_headers(Authorization = paste("Bearer", github_token)))
	  # Convert any HTTP errors to R errors automatically.
	  stop_for_status(request)

	  # Convert results from nested JSON/list to dataframe. If no results returned,
	  # `temp_commits` will be an empty list.
	  temp_commits <- content(request, as = "text") %>%
	    fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)

	  if (identical(temp_commits, list())) {
	    break
	  }

	  commit_sha_dates[[page]] <- select(temp_commits, sha, url)
	}

	# Combine all requested pages of commits into one dataframe
	commit_sha_dates <- bind_rows(commit_sha_dates)

    # For each commit in `temp_commits`, get a list of any modified files.
	added_modified_files <- lapply(commit_sha_dates$url, function(commit_url) {
	  # Make API call for each commit sha
	  request <- GET(commit_url, add_headers(Authorization = paste("Bearer", github_token)))
	  stop_for_status(request)
	  commit <- content(request, as = "text") %>%
	    fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)

	  commit_files <- commit$files

	  # Return empty df if no files listed as modified (can happen with merges, e.g.)
	  if (identical(commit_files, list())) {
	    return(data.frame())
	  }

	  # Else return list of changed files for each commit
	  return(commit_files %>% mutate(commit_date = commit$commit$author$date))
	}) %>%
	  bind_rows() %>%
	  select(filename, status, commit_date) %>%
	  # File must be in `data-processed` dir somewhere and be a csv with expected name
	  # format. We're only interested in added or modified files (not deleted,
	  # renamed, copied, etc).
	  filter(
	    grepl("data-processed/.*/[0-9]{4}-[0-9]{2}-[0-9]{2}-.*[.]csv", filename),
	    status %in% c("added", "modified")
	  ) %>%
	  select(-status) %>%
	  group_by(filename) %>%
	  # Keep most recent reference to a given file. Implicitly deduplicates by filename.
	  filter(
	    commit_date == max(commit_date)
	  ) %>%
	  ungroup()

	# Get forecaster name and date from filename
	filename_parts <- strsplit(added_modified_files$filename, "/")
	added_modified_files <- added_modified_files %>%
	  mutate(
	    forecaster = lapply(
	      filename_parts, function(parts) {
	        parts[[2]]
	      }
	    ) %>%
	      unlist(),
	    forecast_date = lapply(
	      filename_parts, function(parts) {
	        substr(parts[[3]], start = 1, stop = 10)
	      }
	    ) %>%
	      unlist()
	  ) %>%
	  filter(forecaster %in% forecasters)

	 forecasters <- unique(added_modified_files$forecaster)

	fetch_dates <- lapply(forecasters, function(forecaster_name) {
		added_modified_files %>%
	      filter(forecaster == forecaster_name) %>%
	      distinct(forecast_date) %>%
	      pull()
	  }
	)

	predictions_cards <- download_forecasts(forecasters, fetch_dates, state_geos)

	return(predictions_cards)
}

download_all_forecasts <- function(forecasters, state_geos) {
	predictions_cards <- download_forecasts(forecasters, list(), state_geos)

	return(predictions_cards)
}

download_forecasts <- function(forecasters, fetch_dates, state_geos) {
	# Since forecast dates are shared across all forecasters, if a new forecaster
	# is added that backfills forecast dates, we will end up requesting all those
	# dates for forecasters we've already seen before. To prevent that, make a new
	# call to `get_covidhub_predictions` for each forecaster with its own dates.
	predictions_cards <- lapply(
	  forecasters,
	  function(forecaster_name) {
	    get_covidhub_predictions(
	      forecaster_name,
	      signal = signals,
	      ahead = 1:28,
	      geo_values = state_geos,
	      # fetch_dates returns NULL if requested item doesn't exist in the list.
	      forecast_dates = fetch_dates[[forecaster_name]],
	      verbose = TRUE,
	      use_disk = TRUE
	    ) %>%
	      filter(!(incidence_period == "epiweek" & ahead > 4))
	  }
	) %>%
	  bind_rows()
}