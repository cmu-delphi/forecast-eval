
exportScoresUI <- function(id = "exportScores") {
  ns <- shiny::NS(id)
  div(
    downloadButton(ns("exportScores"), "Download CSV")
  )
}

createExportScoresDataFrame <- function(scoreDf, targetVariable, scoreType, forecasters, loc, coverageInterval) {
  signalFilter <- CASE_FILTER
  if (targetVariable == "Deaths") {
    signalFilter <- DEATH_FILTER
  } else if (targetVariable == "Hospitalizations") {
    signalFilter <- HOSPITALIZATIONS_FILTER
  }
  scoreDf <- renameScoreCol(scoreDf, scoreType, coverageInterval)
  scoreDf <- scoreDf %>%
    filter(signal == signalFilter) %>%
    filter(forecaster %in% forecasters)
  if (loc == TOTAL_LOCATIONS || scoreType == "coverage") {
    if (signalFilter == HOSPITALIZATIONS_FILTER) {
      scoreDf <- filterHospitalizationsAheads(scoreDf)
    }
    scoreDf <- filterOverAllLocations(scoreDf, scoreType)
    return(scoreDf[[1]])
  } else {
    scoreDf <- scoreDf %>% filter(geo_value == tolower(loc))
    scoreDf <- scoreDf[c("ahead", "geo_value", "forecaster", "forecast_date", "data_source", "target_end_date", "Score", "actual")]
    return(scoreDf)
  }
}

generateExportFilename <- function(input) {
  score <- input$scoreType
  if (input$scoreType == "sharpness") {
    score <- "spread"
  }
  filename <- paste0("forecast-eval-", input$targetVariable, "-", score)
  if (input$location != TOTAL_LOCATIONS) {
    filename <- paste0(filename, "-", input$location)
  } else if (input$scoreType == "coverage") {
    filename <- paste0(filename, "-", "averaged-over-common-locations-Coverage-interval-", input$coverageInterval)
  } else {
    filename <- paste0(filename, "-totaled-over-common-locations")
  }
  shiny::reactive(filename)
}

exportScoresServer <- function(id, filename, export_df) {
  shiny::moduleServer(id, function(input, output, session) {
    output$exportScores <- downloadHandler(
      filename = function() {
        paste0(filename, "-", Sys.Date(), ".csv")
      },
      contentType = "text/csv",
      content = function(file) {
        withProgress(
          message = "Preparing export",
          detail = "This may take a while...",
          value = 0,
          max = 2,
          {
            incProgress(1)
            write.csv(export_df, file, row.names = FALSE)
            incProgress(2)
          }
        )
      }
    )
  })
}
