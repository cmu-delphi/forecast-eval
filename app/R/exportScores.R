exportScoresUI <- function(id = "exportScores") {
  ns <- shiny::NS(id)
  div(
    downloadButton(ns("exportScores"), "Download CSV")
  )
}

createExportScoresDataFrame <- function(scoreDf, targetVariable, scoreType, forecasters, loc, coverageInterval, filterDate) {
  scoreDf <- filter(
    scoreDf[[targetVariable]],
    forecaster %chin% forecasters
  )
  scoreDf <- renameScoreCol(scoreDf, scoreType, coverageInterval)

  if (loc == TOTAL_LOCATIONS || scoreType == "coverage") {
    if (targetVariable == "Hospitalizations") {
      scoreDf <- filterHospitalizationsAheads(scoreDf)
    }
    scoreDf <- filterOverAllLocations(scoreDf, scoreType, filterDate = filterDate)
    return(scoreDf[[1]])
  } else {
    scoreDf <- filter(scoreDf, geo_value == tolower(loc))
    scoreDf <- scoreDf[c(
      "ahead", "geo_value", "forecaster", "forecast_date",
      "data_source", "target_end_date", "Score", "actual"
    )]
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
  filename
}

exportScoresServer <- function(id, filenameReactive, dataReactive) {
  shiny::moduleServer(id, function(input, output, session) {
    output$exportScores <- downloadHandler(
      filename = function() {
        paste0(filenameReactive(), "-", Sys.Date(), ".csv")
      },
      contentType = "text/csv",
      content = function(file) {
        shiny::withProgress(
          message = "Preparing export",
          detail = "This may take a while...",
          value = 0,
          max = 2,
          {
            shiny::incProgress(1)
            write.csv(dataReactive(), file, row.names = FALSE)
            shiny::incProgress(2)
          }
        )
      }
    )
  })
}
