source('./common.R')

create_export_df = function(scoreDf, targetVariable, scoreType, forecasters, loc, coverageInterval) {
    signalFilter = CASE_FILTER
    if (targetVariable == "Deaths") {
      signalFilter = DEATH_FILTER
    } else if (targetVariable == "Hospitalizations") {
      signalFilter = HOSPITALIZATIONS_FILTER
    }
    scoreDf = renameScoreCol(scoreDf, scoreType, coverageInterval)
    scoreDf = scoreDf %>%
      filter(signal == signalFilter) %>%
      filter(forecaster %in% forecasters)
    if (loc == TOTAL_LOCATIONS || input$scoreType == "coverage") {
      scoreDf = filterOverAllLocations(scoreDf, scoreType, coverageInterval)
      return(scoreDf[[1]])
    } else {
      scoreDf = scoreDf %>% filter(geo_value == tolower(loc))
      scoreDf = scoreDf[c("ahead", "geo_value", "forecaster", "forecast_date", "data_source", "target_end_date", "Score", "actual")]
      return(scoreDf)
    }
}

export_scores_ui = div(
  downloadButton("exportScores", "Download CSV")
)

export_scores_server = function(input, output, df) {
  output$exportScores <- downloadHandler(
    filename = function() {
      score = input$scoreType
      if (input$scoreType == 'sharpness') {
        score = 'spread'
      }
      filename = paste0("forecast-eval-", input$targetVariable, "-", score)
      if (input$location != TOTAL_LOCATIONS) {
        filename = paste0(filename, '-', input$location)
      } else if (input$scoreType == 'coverage') {
        filename = paste0(filename, '-', 'average-over-common-locations-coverage-interval-', input$coverageInterval)
      } else {
        filename = paste0(filename, '-total-over-common-locations-')
      }
      paste0(filename,'-', Sys.Date(), ".csv")
    },
    contentType = 'text/csv',
    content = function(file) {
      withProgress(message = 'Preparing export',
                   detail = 'This may take a while...', value = 0, max = 2, {
        out_df = create_export_df(df, input$targetVariable, input$scoreType, input$forecasters,
                                  input$location, input$coverageInterval)
        incProgress(1)
        write.csv(out_df, file, row.names=FALSE)
        incProgress(2)
      })
    }
  )
}
