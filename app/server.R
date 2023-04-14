################
# UTIL FUNCTIONS before server definition
################
updateTargetChoices <- function(session, targetChoices) {
  updateRadioButtons(session, "targetVariable", choices = targetChoices)
}

updateForecasterChoices <- function(session, df, forecasterInput, scoreType) {
  if (scoreType == "wis") {
    df <- filter(df, !is.na(wis))
  }
  if (scoreType == "ae") {
    df <- filter(df, !is.na(ae))
  }
  forecasterChoices <- distinct(df, forecaster) %>% pull()
  updateSelectInput(session, "forecasters",
    choices = forecasterChoices,
    selected = forecasterInput
  )
}

updateCoverageChoices <- function(session, df, targetVariable, forecasterChoices, coverageInput, output) {
  df <- filter(df, forecaster %in% forecasterChoices)
  df <- Filter(function(x) !all(is.na(x)), df)
  coverageChoices <- intersect(colnames(df), COVERAGE_INTERVALS)
  # Ensure previously selected options are still allowed
  if (any(coverageInput == coverageChoices)) {
    selectedCoverage <- coverageInput
  } else if (any("95" == coverageChoices)) {
    selectedCoverage <- "95"
  } else {
    selectedCoverage <- coverageChoices[1]
  }
  updateSelectInput(session, "coverageInterval",
    choices = coverageChoices,
    selected = selectedCoverage
  )
}

updateLocationChoices <- function(session, df, targetVariable, forecasterChoices, locationInput) {
  df <- filter(df, forecaster %in% forecasterChoices)
  locationChoices <- distinct(df, geo_value) %>%
    pull() %>%
    toupper()
  # Move US to front of list
  locationChoices <- locationChoices[c(length(locationChoices), seq_len(length(locationChoices) - 1))]
  # Add totaled states option to front of list
  locationChoices <- c(TOTAL_LOCATIONS, locationChoices)

  # Display full names for subset of locations
  longnames <- STATE_NAME[match(locationChoices, STATE_ABB)]
  names(locationChoices) <- paste(locationChoices, "-", longnames)
  unmatched <- which(is.na(longnames))
  names(locationChoices)[unmatched] <- locationChoices[unmatched]

  # Ensure previously selected options are still allowed
  if (any(locationInput == locationChoices)) {
    selectedLocation <- locationInput
  } else {
    selectedLocation <- locationChoices[1]
  }
  updateSelectInput(session, "location",
    choices = locationChoices,
    selected = selectedLocation
  )
}

updateAheadChoices <- function(session, df, targetVariable, forecasterChoices, aheads, targetVariableChange) {
  df <- filter(df, forecaster %in% forecasterChoices)
  if (targetVariable == "Hospitalizations") {
    aheadOptions <- HOSPITALIZATIONS_AHEAD_OPTIONS
    title <- "Forecast Horizon (Days)"
    showElement("horizon-disclaimer")
  } else {
    aheadOptions <- AHEAD_OPTIONS
    title <- "Forecast Horizon (Weeks)"
    hideElement("horizon-disclaimer")
  }
  aheadChoices <- Filter(function(x) any(unique(df$ahead) %in% x), aheadOptions)
  # Ensure previsouly selected options are still allowed
  if (!is.null(aheads) && all(aheads %in% aheadChoices)) {
    selectedAheads <- aheads
  } else {
    selectedAheads <- aheadOptions[1]
  }
  # If we are changing target variable, always reset ahead selection to first option
  if (targetVariableChange) {
    selectedAheads <- aheadOptions[1]
  }
  updateCheckboxGroupInput(session, "aheads",
    title,
    choices = aheadChoices,
    selected = selectedAheads,
    inline = TRUE
  )
}

showScoreExplanation <- function(session, scoreType, dash_suffix) {
  if (scoreType == "wis") {
    showElement(paste0("wisExplanation", dash_suffix))
    hideElement(paste0("sharpnessExplanation", dash_suffix))
    hideElement(paste0("aeExplanation", dash_suffix))
    hideElement(paste0("coverageExplanation", dash_suffix))
  }
  if (scoreType == "sharpness") {
    showElement(paste0("sharpnessExplanation", dash_suffix))
    hideElement(paste0("wisExplanation", dash_suffix))
    hideElement(paste0("aeExplanation", dash_suffix))
    hideElement(paste0("coverageExplanation", dash_suffix))
  }
  if (scoreType == "ae") {
    hideElement(paste0("wisExplanation", dash_suffix))
    hideElement(paste0("sharpnessExplanation", dash_suffix))
    showElement(paste0("aeExplanation", dash_suffix))
    hideElement(paste0("coverageExplanation", dash_suffix))
  }
  if (scoreType == "coverage") {
    hideElement(paste0("wisExplanation", dash_suffix))
    hideElement(paste0("sharpnessExplanation", dash_suffix))
    hideElement(paste0("aeExplanation", dash_suffix))
    showElement(paste0("coverageExplanation", dash_suffix))
  }
}

# All data is fully loaded from AWS
DATA_LOADED <- FALSE
loadData <- createDataLoader()

server <- function(input, output, session) {
  CASES_DEATHS_CURRENT <- resolveCurrentCasesDeathDay()
  HOSP_CURRENT <- resolveCurrentHospDay()

  PREV_AS_OF_DATA <- reactiveVal(NULL)
  AS_OF_CHOICES <- reactiveVal(HOSP_CURRENT)
  SUMMARIZING_OVER_ALL_LOCATIONS <- reactive(input$scoreType == "coverage" || input$location == TOTAL_LOCATIONS)

  # Set asof selection on startup to avoid creating initial plot multiple
  # times. If asof selection is empty, the initial plot is created twice,
  # once on startup and once when the as-of date is set later.
  observeEvent(input$asOf,
    {
      if (input$asOf == "") {
        updateSelectInput(session, "asOf",
          choices = AS_OF_CHOICES(),
          selected = HOSP_CURRENT
        )
      }
    },
    once = TRUE
  )

  DASH_SUFFIX <- ""
  COLOR_SEED <- reactiveVal(171)

  CURRENT_WEEK_END_DATE <- reactiveVal(
    if_else(INIT_TARGET == "Hospitalizations", HOSP_CURRENT, CASES_DEATHS_CURRENT)
  )

  # Get scores
  loaded <- loadData(INIT_TARGET)
  df_list <- loaded$df_list
  dataCreationDate <- loaded$dataCreationDate
  DATA_LOADED <- TRUE

  ##################
  # CREATE MAIN PLOT
  ##################
  summaryPlot <- function() {
    if (input$location == "") {
      return()
    }
    if (!(input$targetVariable %in% TARGET_VARS_BY_TAB[[isolate(input$tabset)]])) {
      return()
    }

    ## Setting target signal to be compared with asOfData
    if (input$targetVariable == "Cases") {
      targetSignal <- "confirmed_incidence_num"
    } else if (input$targetVariable == "Deaths") {
      targetSignal <- "deaths_incidence_num"
    } else if (input$targetVariable == "Hospitalizations") {
      targetSignal <- "confirmed_admissions_covid_1d"
    }

    filteredScoreDf <- filterScoreDf()
    dfWithForecasts <- NULL
    if (input$showForecasts) {
      dfWithForecasts <- filteredScoreDf
    }
    # Need to do this after setting dfWithForecasts to leave in aheads for forecasts
    filteredScoreDf <- filter(filteredScoreDf, ahead %in% input$aheads)
    if (nrow(filteredScoreDf) == 0) {
      output[[paste0("renderWarningText", DASH_SUFFIX)]] <- renderText(paste0(
        "The selected forecasters do not have enough data ",
        "to display the selected scoring metric."
      ))
      return()
    }

    asOfData <- NULL
    ## fill asOfData with PREV_AS_OF_DATA()
    if (!is.null(PREV_AS_OF_DATA()) &&
      input$asOf != "" &&
      nrow(PREV_AS_OF_DATA()) != 0 &&
      input$asOf < CURRENT_WEEK_END_DATE()) {
      asOfData <- PREV_AS_OF_DATA()
      asOfData$target_end_date <- asOfData$time_value
      asOfData$as_of_actual <- asOfData$value
      asOfData <- asOfData[, c("target_end_date", "geo_value", "as_of_actual")]
    }

    if (!is.null(asOfData) && nrow(asOfData) != 0) {
      # Get the 'as of' dates that are the target_end_dates in the scoring df
      dateGroupDf <- filter(asOfData, target_end_date %in% filteredScoreDf$target_end_date)
      if (nrow(dateGroupDf) != 0) {
        # Since cases and deaths are shown as weekly incidence, but the "as of" data from the covidcast API
        # is daily, we need to sum over the days leading up to the target_end_date of each week to get the
        # weekly incidence
        asOfData <- filterAsOfData(asOfData, dateGroupDf, filteredScoreDf)
        filteredScoreDf <- full_join(filteredScoreDf, asOfData, by = c("target_end_date", "geo_value"))
      } else {
        # Input 'as of' date chosen does not match the available target_end_dates that result from the rest of the selected inputs
        # It is too far back or we are switching between hosp and cases/deaths which have different target date days
        # As of input will be updated to the default (latest) and plot will re-render with the just the normal truth data, no 'as of'
        asOfData <- NULL
      }
    }

    # Totaling over all locations
    if (SUMMARIZING_OVER_ALL_LOCATIONS()) {
      filteredScoreDfAndIntersections <- filterOverAllLocations(filteredScoreDf, input$scoreType, !is.null(asOfData), filterDate = dataCreationDate)
      filteredScoreDf <- filteredScoreDfAndIntersections[[1]]
      locationsIntersect <- filteredScoreDfAndIntersections[[2]]
      if (input$showForecasts) {
        dfWithForecasts <- filter(dfWithForecasts, geo_value %in% locationsIntersect)
      }
      aggregateText <- "*For fair comparison, all displayed forecasters on all displayed dates are compared across a common set of states and territories."
      if (input$scoreType == "coverage") {
        aggregate <- "Averaged"
        output[[paste0("renderAggregateText", DASH_SUFFIX)]] <- renderText(paste(
          aggregateText,
          " Some forecasters may not have any data for the coverage interval chosen. Locations inlcuded: "
        ))
      } else {
        aggregate <- "Totaled"
        output[[paste0("renderAggregateText", DASH_SUFFIX)]] <- renderText(paste(aggregateText, " Locations included: "))
      }
      if (length(locationsIntersect) == 0) {
        output[[paste0("renderWarningText", DASH_SUFFIX)]] <- renderText("The selected forecasters do not have data for any locations in common on all dates.")
        output[[paste0("renderLocations", DASH_SUFFIX)]] <- renderText("")
        output[[paste0("renderAggregateText", DASH_SUFFIX)]] <- renderText("")
        hideElement(paste0("truthPlot", DASH_SUFFIX))
        hideElement(paste0("refresh-colors", DASH_SUFFIX))
        return()
      } else {
        locationSubtitleText <- paste0(", Location: ", aggregate, " over all states and territories common to these forecasters*")
        output[[paste0("renderLocations", DASH_SUFFIX)]] <- renderText(toupper(locationsIntersect))
        output[[paste0("renderWarningText", DASH_SUFFIX)]] <- renderText("")
        showElement(paste0("truthPlot", DASH_SUFFIX))
      }
      # Not totaling over all locations
    } else {
      if (!is.null(asOfData)) {
        filteredScoreDf <- filteredScoreDf %>%
          filter(geo_value == tolower(input$location)) %>%
          group_by(forecaster, forecast_date, target_end_date, ahead) %>%
          summarize(Score = Score, actual = actual, as_of_actual = as_of_actual)
      } else {
        filteredScoreDf <- filteredScoreDf %>%
          filter(geo_value == tolower(input$location)) %>%
          group_by(forecaster, forecast_date, target_end_date, ahead) %>%
          summarize(Score = Score, actual = actual)
      }
      locationSubtitleText <- paste0(", Location: ", input$location)
      output[[paste0("renderAggregateText", DASH_SUFFIX)]] <- renderText("")
      output[[paste0("renderLocations", DASH_SUFFIX)]] <- renderText("")
      output[[paste0("renderWarningText", DASH_SUFFIX)]] <- renderText("")
    }

    showElement(paste0("refresh-colors", DASH_SUFFIX))
    if (nrow(filteredScoreDf) == 0) {
      # no data to show
      return()
    }

    # Rename columns that will be used as labels and for clarity on CSV exports
    filteredScoreDf <- filteredScoreDf %>% rename(
      Forecaster = forecaster, Forecast_Date = forecast_date,
      Week_End_Date = target_end_date
    )

    ## Create color palette
    set.seed(COLOR_SEED())
    forecasterRand <- input$forecasters
    # Pad selected forecasters up to 8 items to get more-different colors for a
    # small number of requested forecasters
    minForecasters <- 8
    if (length(forecasterRand) < minForecasters) {
      nForecasters <- minForecasters - length(forecasterRand)
      forecasterRand <- c(forecasterRand, paste0("blank_names", 1:nForecasters))
    }

    colorPalette <- setNames(
      object = viridis(length(forecasterRand), option = "turbo"),
      nm = sample(forecasterRand)
    )

    if (!is.null(asOfData)) {
      colorPalette["Reported_Incidence"] <- "grey"
      colorPalette["Reported_As_Of_Incidence"] <- "black"
    }

    # Render truth plot with observed values
    truthDf <- filteredScoreDf
    ## this first condition is necessary when loading the dash
    if (input$asOf == "" || !exists("TRUTH_PLOT")) {
      TRUTH_PLOT <<- truthPlot(truthDf, locationsIntersect, !is.null(asOfData), dfWithForecasts, colorPalette)
      output[[paste0("truthPlot", DASH_SUFFIX)]] <- renderPlotly({
        TRUTH_PLOT
      })
    } else {
      if (USE_CURR_TRUTH && input$showForecasts == FALSE) {
        # Render existing truth plot
        output[[paste0("truthPlot", DASH_SUFFIX)]] <- renderPlotly({
          TRUTH_PLOT
        })
      } else {
        TRUTH_PLOT <<- truthPlot(truthDf, locationsIntersect, !is.null(asOfData), dfWithForecasts, colorPalette)
        output[[paste0("truthPlot", DASH_SUFFIX)]] <- renderPlotly({
          TRUTH_PLOT
        })
      }
    }
    USE_CURR_TRUTH <<- FALSE
    # If we are just re-rendering the truth plot with as of data
    # we don't need to re-render the score plot
    if (RE_RENDER_TRUTH) {
      RE_RENDER_TRUTH <<- FALSE
      return()
    }

    # If we are re-rendering scoring plot with new inputs that were just selected
    # we need to make sure the as of input options are valid with those inputs
    updateAsOfChoices(session, truthDf)

    # Format and transform data for plot
    filteredScoreDf <- filter(filteredScoreDf, !is.na(Week_End_Date))
    filteredScoreDf <- mutate(
      filteredScoreDf[, c("Forecaster", "Forecast_Date", "Week_End_Date", "Score", "ahead")],
      across(where(is.numeric), ~ round(., 2))
    )
    if (input$scoreType != "coverage") {
      if (input$scaleByBaseline) {
        baselineDf <- filter(filteredScoreDf, Forecaster == "COVIDhub-baseline")
        filteredScoreDf <- merge(filteredScoreDf, baselineDf, by = c("Week_End_Date", "ahead"))

        # Scale score by baseline forecaster
        filteredScoreDf$Score <- filteredScoreDf$Score.x / filteredScoreDf$Score.y
        filteredScoreDf$Forecaster <- filteredScoreDf$Forecaster.x
        filteredScoreDf$Forecast_Date <- filteredScoreDf$Forecast_Date.x
        filteredScoreDf <- filteredScoreDf[, c("Forecaster", "Forecast_Date", "Week_End_Date", "ahead", "Score")]
      }
      if (input$logScale) {
        filteredScoreDf$Score <- log10(filteredScoreDf$Score)
      }
    }

    # Set plot title
    if (input$scoreType == "wis") {
      plotTitle <- "Weighted Interval Score"
    } else if (input$scoreType == "sharpness") {
      plotTitle <- "Spread"
    } else if (input$scoreType == "ae") {
      plotTitle <- "Absolute Error"
    } else {
      plotTitle <- "Coverage"
    }

    titleText <- paste0(
      "<b>", plotTitle, "</b>", "<br>", "<sup>",
      "Target Variable: ", input$targetVariable,
      locationSubtitleText, "<br>",
      tags$span(id = "drag-to-zoom", " Drag to zoom"),
      "</sup>"
    )

    # Fill gaps so there are line breaks on weeks without data
    # This is failing for CU-select on US deaths (https://github.com/cmu-delphi/forecast-eval/issues/157)
    filteredScoreDf <- filteredScoreDf %>%
      as_tsibble(key = c(Forecaster, ahead), index = Week_End_Date) %>%
      group_by(Forecaster, Forecast_Date, ahead) %>%
      fill_gaps(.full = TRUE)
    # Set labels for faceted horizon plots
    horizonOptions <- AHEAD_OPTIONS
    horizonLabels <- lapply(AHEAD_OPTIONS, function(x) paste0("Horizon: ", x, " Week(s)"))
    if (input$targetVariable == "Hospitalizations") {
      horizonOptions <- HOSPITALIZATIONS_AHEAD_OPTIONS
      horizonLabels <- lapply(HOSPITALIZATIONS_AHEAD_OPTIONS, function(x) paste0("Horizon: ", x, " Days"))
    }
    filteredScoreDf$ahead <- factor(filteredScoreDf$ahead,
      levels = horizonOptions,
      labels = horizonLabels
    )

    p <- ggplot(
      filteredScoreDf,
      aes(x = Week_End_Date, y = Score, color = Forecaster, shape = Forecaster, label = Forecast_Date)
    ) +
      geom_line() +
      geom_point(size = 2) +
      labs(x = "", y = "", title = titleText) +
      scale_x_date(date_labels = "%b %Y") +
      facet_wrap(~ahead, ncol = 1) +
      scale_color_manual(values = colorPalette) +
      theme_bw() +
      theme(panel.spacing = unit(0.5, "lines"))

    if (input$showForecasts) {
      maxLim <- if_else(
        as.Date(input$asOf) + 7 * 4 > CURRENT_WEEK_END_DATE(),
        as.Date(input$asOf) + 7 * 4,
        as.Date(NA)
      )
      p <- p + scale_x_date(limits = c(as.Date(NA), maxLim), date_labels = "%b %Y")
    }
    if (input$scoreType == "coverage") {
      p <- p + geom_hline(yintercept = .01 * as.integer(input$coverageInterval))
    }
    if (input$logScale) {
      p <- p + scale_y_continuous(label = function(x) paste0("10^", x))
    } else {
      p <- p + scale_y_continuous(limits = c(0, NA), labels = scales::comma)
    }
    plotHeight <- 550 + (length(input$aheads) - 1) * 100
    finalPlot <-
      ggplotly(p, tooltip = c("x", "y", "shape", "label")) %>%
      layout(
        height = plotHeight,
        legend = list(orientation = "h", y = -0.1, title = list(text = NULL)),
        margin = list(t = 90),
        hovermode = "x unified",
        xaxis = list(
          title = list(text = "Target Date", standoff = 8L),
          titlefont = list(size = 12)
        )
      ) %>%
      config(displayModeBar = FALSE)
    return(finalPlot)
  }

  ###################
  # CREATE TRUTH PLOT
  ###################
  # Create the plot for target variable ground truth
  truthPlot <- function(filteredDf = NULL, locationsIntersect = NULL, hasAsOfData = FALSE, dfWithForecasts = NULL, colorPalette = NULL) {
    observation <- paste0("Incident ", input$targetVariable)
    if (input$targetVariable == "Hospitalizations") {
      observation <- paste0("Hospital Admissions")
    }
    titleText <- paste0("<b>Observed ", observation, "</b>")
    if (SUMMARIZING_OVER_ALL_LOCATIONS()) {
      titleText <- paste0("<b>Observed ", observation, "</b>", " <br><sup>Totaled over all states and territories common to selected forecasters*</sup>")
    }

    if (hasAsOfData) {
      filteredDf <- filteredDf %>%
        group_by(Week_End_Date) %>%
        summarize(Forecaster = Forecaster, Reported_Incidence = actual, Reported_As_Of_Incidence = as_of_actual) %>%
        distinct()
    } else {
      filteredDf <- filteredDf %>%
        group_by(Week_End_Date) %>%
        summarize(Forecaster = Forecaster, Reported_Incidence = actual) %>%
        distinct()
    }
    if (input$showForecasts) {
      filteredDf <- filterForecastData(filteredDf, dfWithForecasts, hasAsOfData)
    }

    finalPlot <- ggplot(filteredDf, aes(x = Week_End_Date)) +
      labs(x = "", y = "", title = titleText) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      scale_x_date(date_labels = "%b %Y") +
      scale_color_manual(values = colorPalette) +
      theme_bw()

    if (hasAsOfData) {
      finalPlot <- finalPlot +
        geom_line(aes(y = Reported_Incidence, color = "Reported_Incidence")) +
        geom_point(aes(y = Reported_Incidence, color = "Reported_Incidence")) +
        geom_line(aes(y = Reported_As_Of_Incidence, color = "Reported_As_Of_Incidence")) +
        geom_point(aes(y = Reported_As_Of_Incidence, color = "Reported_As_Of_Incidence"))
    } else {
      finalPlot <- finalPlot + geom_line(aes(y = Reported_Incidence)) +
        geom_point(aes(y = Reported_Incidence))
    }
    if (input$showForecasts) {
      maxLim <- if_else(
        as.Date(input$asOf) + 7 * 4 > CURRENT_WEEK_END_DATE(),
        as.Date(input$asOf) + 7 * 4,
        as.Date(NA)
      )

      finalPlot <- finalPlot +
        geom_line(aes(y = Quantile_50, color = Forecaster, shape = Forecaster)) +
        geom_point(aes(y = Quantile_50, color = Forecaster, shape = Forecaster)) +
        scale_x_date(limits = c(as.Date(NA), maxLim), date_labels = "%b %Y")
    }
    finalPlot <- ggplotly(finalPlot, tooltip = c("shape", "x", "y")) %>%
      layout(
        hovermode = "x unified",
        legend = list(orientation = "h", y = -0.1, title = list(text = NULL))
      ) %>%
      config(displayModeBar = FALSE)
    # Remove the extra grouping from the legend: "(___,1)"
    for (i in seq_along(finalPlot$x$data)) {
      if (!is.null(finalPlot$x$data[[i]]$name)) {
        finalPlot$x$data[[i]]$name <- gsub("\\(", "", stringr::str_split(finalPlot$x$data[[i]]$name, ",")[[1]][1])
      }
    }
    return(finalPlot)
  }

  #############
  # PLOT OUTPUT
  #############
  output$summaryPlot <- output$summaryPlot_archive <- renderPlotly({
    summaryPlot()
  })

  # Filter scoring df by inputs chosen (targetVariable, forecasters, aheads)
  filterScoreDf <- function() {
    filteredScoreDf <- filter(
      df_list[[input$targetVariable]],
      forecaster %in% input$forecasters
    )

    if (input$targetVariable == "Hospitalizations") {
      filteredScoreDf <- filterHospitalizationsAheads(filteredScoreDf)
    }
    if (input$scoreType == "wis" || input$scoreType == "sharpness") {
      # Only show WIS or Sharpness for forecasts that have all intervals or are for future dates
      filteredScoreDf <- filter(
        filteredScoreDf,
        (
          !is.na(`50`) &
            !is.na(`80`) &
            !is.na(`95`)
        ) | target_end_date >= dataCreationDate
      )
      if (input$targetVariable == "Deaths") {
        filteredScoreDf <- filter(
          filteredScoreDf,
          (
            !is.na(`10`) &
              !is.na(`20`) &
              !is.na(`30`) &
              !is.na(`40`) &
              !is.na(`60`) &
              !is.na(`70`) &
              !is.na(`90`) &
              !is.na(`98`)
          ) | target_end_date >= dataCreationDate
        )
      }
    }
    filteredScoreDf <- renameScoreCol(filteredScoreDf, input$scoreType, input$coverageInterval)
    return(filteredScoreDf)
  }

  # Filter as of data so that it matches weekly incidence for the target end dates in the score df
  filterAsOfData <- function(asOfData, dateGroupDf, filteredScoreDf) {
    # Hospitalization scores are shown as daily incidence, not weekly incidence, no summing necessary
    if (input$targetVariable != "Hospitalizations") {
      # Create a df to fill in the corresponding target_end_date in a new date_group column for all intervening days
      dateGroupDf[, "date_group"] <- NA
      dateGroupDf$date_group <- dateGroupDf$target_end_date
      asOfData <- full_join(asOfData, dateGroupDf, by = c("target_end_date", "geo_value", "as_of_actual"))

      # Cut off the extra days on beginning and end of series so that when we sum the values we are only
      # summing over the weeks included in the score plot
      asOfData <- filter(
        asOfData,
        target_end_date >= min(filteredScoreDf$target_end_date) - 6,
        target_end_date <= isolate(input$asOf)
      )

      # Fill in the date_group column with the target week end days for all intervening days
      asOfData <- asOfData %>%
        arrange(geo_value) %>%
        fill(date_group, .direction = "up")

      # In the case where there are target week end days missing from the scoring or as of data
      # we don't want to end up summing values over multiple weeks so we make sure each date_group only spans one week
      asOfData <- filter(asOfData, asOfData$date_group - asOfData$target_end_date < 7)

      asOfData <- asOfData[c("geo_value", "as_of_actual", "date_group")]
      # Sum over preceding week for all weekly target variables
      asOfData <- asOfData %>%
        group_by(geo_value, date_group) %>%
        summarize(as_of_actual = sum(as_of_actual))
      asOfData <- asOfData %>% rename(target_end_date = date_group)
      # If targetVariable is Hospitalizations
    } else {
      asOfData <- dateGroupDf
      # Need to make sure that we are only matching the target_end_dates shown in the scoring plot
      # and not using fetched data for as of dates before those target_end_dates.
      # This is taken care of above for cases and deaths.
      minDate <- min(filteredScoreDf$target_end_date)
      if (!SUMMARIZING_OVER_ALL_LOCATIONS()) {
        chosenLocationDf <- filter(filteredScoreDf, geo_value == tolower(input$location))
        minDate <- min(chosenLocationDf$target_end_date)
      }
      asOfData <- filter(asOfData, target_end_date >= minDate)
    }
    return(asOfData)
  }

  filterForecastData <- function(filteredDf, dfWithForecasts, hasAsOfData) {
    dfWithForecasts <- dfWithForecasts %>%
      rename(Week_End_Date = target_end_date, Forecaster = forecaster, Quantile_50 = value_50)
    if (!SUMMARIZING_OVER_ALL_LOCATIONS()) {
      dfWithForecasts <- filter(dfWithForecasts, geo_value == tolower(input$location))
    } else {
      # Sum the predictions for all included locations
      dfWithForecasts <- dfWithForecasts %>%
        group_by(Forecaster, forecast_date, Week_End_Date, ahead) %>%
        summarize(Quantile_50 = sum(Quantile_50))
    }

    if (hasAsOfData) {
      # We want the forecasts to be later than latest as of date with data
      lastEndDate <- tail(filter(filteredDf, !is.na(Reported_As_Of_Incidence)), n = 1)$Week_End_Date[1]
      dfWithForecasts <- dfWithForecasts %>%
        filter(forecast_date >= lastEndDate) %>%
        group_by(Week_End_Date) %>%
        summarize(Forecaster, forecast_date, Quantile_50)
    } else {
      # Get the latest predictions for each forecaster
      dfWithForecasts <- dfWithForecasts %>%
        group_by(Forecaster) %>%
        filter(forecast_date == max(forecast_date)) %>%
        ungroup() %>%
        group_by(Week_End_Date) %>%
        summarize(Forecaster, forecast_date, Quantile_50) %>%
        filter(Week_End_Date > CURRENT_WEEK_END_DATE())
    }

    # Get the next as of choice available in dropdown menu
    dfWithForecasts <- dfWithForecasts %>% arrange(forecast_date)
    nextAsOfInList <- AS_OF_CHOICES()[which.min(abs(AS_OF_CHOICES() - min(dfWithForecasts$forecast_date))) + 1]
    if (!hasAsOfData) {
      nextAsOfInList <- NA
    }

    # Take only those forecasts with a forecast date before the next as of date in dropdown
    # aka within the week after the current as of shown
    if (length(nextAsOfInList) != 0 && !is.na(nextAsOfInList)) {
      dfWithForecasts <- filter(dfWithForecasts, forecast_date < nextAsOfInList)
    }

    # Hospitalizations will have multiple forecast dates within this target week
    # So we want to take the earliest forecast date for each forecaster & week end date pair
    if (input$targetVariable == "Hospitalizations") {
      dfWithForecasts <- dfWithForecasts %>%
        group_by(Week_End_Date, Forecaster) %>%
        top_n(n = 1, wt = desc(forecast_date))
      dfWithForecasts <- dfWithForecasts %>%
        group_by(Forecaster) %>%
        filter(forecast_date == first(forecast_date))
    }

    keepCols <- c("Week_End_Date", "Quantile_50", "Forecaster", "Reported_Incidence")
    if (hasAsOfData) {
      keepCols <- c(keepCols, "Reported_As_Of_Incidence")
    }
    filteredDf <- merge(filteredDf, dfWithForecasts, by = c("Week_End_Date", "Forecaster"), all = TRUE) %>%
      group_by(Week_End_Date)
    # Remove rows of NAs
    filteredDf <- filter(filteredDf[, keepCols], !is.null(Forecaster))
    filteredDf <- filteredDf %>%
      arrange(Week_End_Date) %>%
      fill(Reported_Incidence, .direction = "downup")
    return(filteredDf)
  }

  ###################
  # EVENT OBSERVATION
  ###################

  observeEvent(input$tabset,
    {
      if (input$tabset == paste0("evaluations", CURRENT_TAB_SUFFIX)) {
        DASH_SUFFIX <<- CURRENT_TAB_SUFFIX
      } else if (input$tabset == paste0("evaluations", ARCHIVE_TAB_SUFFIX)) {
        DASH_SUFFIX <<- ARCHIVE_TAB_SUFFIX
      } else {
        return()
      }
      choices <- TARGET_VARS_BY_TAB[[paste0("evaluations", DASH_SUFFIX)]]
      updateTargetChoices(session, choices)
      showScoreExplanation(session, input$scoreType, DASH_SUFFIX)
    },
    ignoreInit = TRUE
  )

  observeEvent(input$refreshColors | input$refreshColors_archive,
    {
      COLOR_SEED(floor(runif(1, 1, 1000)))
      USE_CURR_TRUTH <<- TRUE
    },
    ignoreInit = TRUE
  )

  # When the target variable changes, update available forecasters, locations, and CIs to choose from
  observeEvent(input$targetVariable,
    {
      DATA_LOADED <<- FALSE

      ## summaryPlot will try to use PREV_AS_OF_DATA()
      ## since it has wrong data information, it needs to be removed
      PREV_AS_OF_DATA(NULL)

      loaded <<- loadData(input$targetVariable)
      df_list <<- loaded$df_list
      dataCreationDate <<- loaded$dataCreationDate
      DATA_LOADED <<- TRUE
      df <- df_list[[input$targetVariable]]

      ## Update available options
      updateAheadChoices(session, df, input$targetVariable, input$forecasters, input$aheads, TRUE)
      updateForecasterChoices(session, df, input$forecasters, input$scoreType)
      updateLocationChoices(session, df, input$targetVariable, input$forecasters, input$location)
      updateCoverageChoices(session, df, input$targetVariable, input$forecasters, input$coverageInterval, output)

      ## updateAsOf sets if we need to call updateAsOfData
      ## the only necessary case is when going from
      ## cases, deaths -> cases,deaths
      updateAsOf <- FALSE

      ## Resolving dates
      ### If going from Deaths|Cases to Deaths|Cases we will call updateAsOfData
      ### only when
      if (any(PREV_TARGET == c("Deaths", "Cases")) && any(input$targetVariable == c("Deaths", "Cases"))) {
        CURRENT_WEEK_END_DATE(CASES_DEATHS_CURRENT)
        currentFetch <- input$asOf
        if (input$asOf != "" &&
          CURRENT_WEEK_END_DATE() != currentFetch) {
          updateAsOf <- TRUE
        }
      } else if (any(PREV_TARGET == c("Deaths", "Cases")) && input$targetVariable == "Hospitalizations") {
        CURRENT_WEEK_END_DATE(HOSP_CURRENT)
      } else if (PREV_TARGET == "Hospitalizations" && any(input$targetVariable == c("Deaths", "Cases"))) {
        CURRENT_WEEK_END_DATE(CASES_DEATHS_CURRENT)
      }

      ## Storing current target
      PREV_TARGET <<- input$targetVariable

      ## Call the API only if we need as of data
      if (updateAsOf) {
        updateAsOfData(fetchDate = currentFetch)
      }
    },
    # Make higher priority than other `observe`s since we need to have data loaded first.
    priority = 1
  )

  observeEvent(input$scoreType, {
    df <- df_list[[input$targetVariable]]

    # Only show forecasters that have data for the score chosen
    updateForecasterChoices(session, df, input$forecasters, input$scoreType)

    # If we are switching between coverage and other score types we need to
    # update the as of data we have so it matches the correct locations shown
    if (input$location == "US") {
      updateAsOfData()
    }

    showScoreExplanation(session, input$scoreType, DASH_SUFFIX)
    USE_CURR_TRUTH <<- TRUE
  })

  # When forecaster selections change, update available aheads, locations, and CIs to choose from
  observeEvent(input$forecasters, {
    df <- filter(
      df_list[[input$targetVariable]],
      forecaster %in% input$forecasters
    )

    updateAheadChoices(session, df, input$targetVariable, input$forecasters, input$aheads, FALSE)
    updateLocationChoices(session, df, input$targetVariable, input$forecasters, input$location)
    updateCoverageChoices(session, df, input$targetVariable, input$forecasters, input$coverageInterval, output)
    USE_CURR_TRUTH <<- TRUE
  })

  observeEvent(input$scaleByBaseline, {
    USE_CURR_TRUTH <<- TRUE
  })

  observeEvent(input$logScale, {
    USE_CURR_TRUTH <<- TRUE
  })

  observeEvent(input$location,
    {
      # call only if asOf selected is not the newest available.
      if (input$asOf != "" &&
        input$asOf < CURRENT_WEEK_END_DATE()) {
        updateAsOfData()
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(input$asOf,
    {
      ## We just need to call updateAsOfData
      ## when it is not the most recent one
      if (input$asOf < CURRENT_WEEK_END_DATE()) {
        updateAsOfData()
      }
    },
    ignoreInit = TRUE
  )

  # The following checks ensure the minimum necessary input selections
  observe({
    # Show data loading message and hide other messages until all data is loaded
    if (DATA_LOADED) {
      hideElement(paste0("data-loading-message", DASH_SUFFIX))
      showElement(paste0("refresh-colors", DASH_SUFFIX))
      showElement(paste0("notes", DASH_SUFFIX))
      showElement(paste0("scoreExplanations", DASH_SUFFIX))
      showElement(paste0("scoringDisclaimer", DASH_SUFFIX))
    }
    # Ensure there is always one ahead selected
    if (length(input$aheads) < 1) {
      if (input$targetVariable == "Hospitalizations") {
        updateCheckboxGroupInput(session, "aheads",
          selected = HOSPITALIZATIONS_AHEAD_OPTIONS[1]
        )
      } else {
        updateCheckboxGroupInput(session, "aheads",
          selected = AHEAD_OPTIONS[1]
        )
      }
    }
    # Ensure there is always one forecaster selected
    if (length(input$forecasters) < 1) {
      updateSelectInput(session, "forecasters",
        selected = c("COVIDhub-baseline")
      )
    }
    # Ensure COVIDhub-baseline is selected when scaling by baseline
    if (input$scaleByBaseline && !any("COVIDhub-baseline" == input$forecasters)) {
      updateSelectInput(session, "forecasters", selected = c(input$forecasters, "COVIDhub-baseline"))
    }
  })

  updateAsOfData <- function(fetchDate = input$asOf) {
    if (as.character(fetchDate) == "") {
      return()
    }
    dataSource <- "jhu-csse"
    if (input$targetVariable == "Cases") {
      targetSignal <- "confirmed_incidence_num"
    } else if (input$targetVariable == "Deaths") {
      targetSignal <- "deaths_incidence_num"
    } else if (input$targetVariable == "Hospitalizations") {
      targetSignal <- "confirmed_admissions_covid_1d"
      dataSource <- "hhs"
    }

    if (input$location == "US" && input$scoreType != "coverage") {
      location <- "nation"
    } else {
      location <- "state"
    }

    if (fetchDate < CURRENT_WEEK_END_DATE()) {
      hideElement(paste0("truthPlot", DASH_SUFFIX))
      hideElement(paste0("notes", DASH_SUFFIX))
      hideElement(paste0("scoringDisclaimer", DASH_SUFFIX))
      hideElement(paste0("scoreExplanations", DASH_SUFFIX))
      hideElement(paste0("renderAggregateText", DASH_SUFFIX))
      hideElement(paste0("renderLocations", DASH_SUFFIX))
      showElement(paste0("truth-plot-loading-message", DASH_SUFFIX))

      # Since as_of matches to the issue date in covidcast (rather than the time_value)
      # we need to add one extra day to get the as of we want.
      fetchDate <- as.Date(fetchDate) + 1

      # Covidcast API call
      asOfTruthData <- covidcast_signal_mem(
        data_source = dataSource, signal = targetSignal,
        start_day = "2020-02-15", end_day = fetchDate,
        as_of = fetchDate,
        geo_type = location
      )
      showElement(paste0("truthPlot", DASH_SUFFIX))
      showElement(paste0("notes", DASH_SUFFIX))
      showElement(paste0("scoringDisclaimer", DASH_SUFFIX))
      showElement(paste0("scoreExplanations", DASH_SUFFIX))
      showElement(paste0("renderAggregateText", DASH_SUFFIX))
      showElement(paste0("renderLocations", DASH_SUFFIX))
      hideElement(paste0("truth-plot-loading-message", DASH_SUFFIX))
      PREV_AS_OF_DATA(asOfTruthData)

      if (nrow(asOfTruthData) == 0) {
        return()
      }
    } else if (fetchDate == CURRENT_WEEK_END_DATE()) {
      PREV_AS_OF_DATA(NULL)
      RE_RENDER_TRUTH <<- TRUE
    }
  }

  updateAsOfChoices <- function(session, truthDf) {
    asOfChoices <- truthDf %>%
      # To prevent grouping columns from being added back on.
      ungroup() %>%
      distinct(Week_End_Date) %>%
      filter(Week_End_Date <= CURRENT_WEEK_END_DATE(), !is.na(Week_End_Date)) %>%
      pull()
    selectedAsOf <- isolate(input$asOf)
    if (input$targetVariable == "Hospitalizations") {
      minChoice <- MIN_AVAIL_HOSP_AS_OF_DATE
      asOfChoices <- asOfChoices[asOfChoices >= minChoice]
    } else if (input$location == "US" && input$scoreType != "coverage") {
      minChoice <- MIN_AVAIL_NATION_AS_OF_DATE
      asOfChoices <- asOfChoices[asOfChoices >= minChoice]
    } else if (any(input$location == TERRITORIES) || input$location == TOTAL_LOCATIONS || input$scoreType == "coverage") {
      minChoice <- MIN_AVAIL_TERRITORY_AS_OF_DATE
      asOfChoices <- asOfChoices[asOfChoices >= minChoice]
    }
    asOfChoices <- c(asOfChoices, CURRENT_WEEK_END_DATE())
    # Make sure we have a valid as of selection
    nonValidAsOf <- selectedAsOf == "" || !any(selectedAsOf == asOfChoices)
    if (length(asOfChoices) != 0 && nonValidAsOf) {
      selectedAsOf <- max(asOfChoices)
    }
    AS_OF_CHOICES(sort(asOfChoices))
    updateSelectInput(session, "asOf",
      choices = AS_OF_CHOICES(),
      selected = selectedAsOf
    )
  }
  exportScoresServer(
    "exportScores", shiny::reactive(generateExportFilename(input)),
    shiny::reactive(createExportScoresDataFrame(
      df_list, input$targetVariable, input$scoreType, input$forecasters,
      input$location, input$coverageInterval
    ))
  )
}
