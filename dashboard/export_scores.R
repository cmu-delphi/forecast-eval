source('./common.R')

create_export_df = function(scoreDf, targetVariable, forecasters, horizon, loc) {
    signalFilter = CASE_FILTER
    if (targetVariable == "Deaths") {
      signalFilter = DEATH_FILTER
    }
    scoreDf = scoreDf %>%
      filter(signal == signalFilter) %>%
      filter(ahead %in% horizon) %>%
      filter(forecaster %in% forecasters)
    if (loc != TOTAL_LOCATIONS) {
      scoreDf = scoreDf %>% filter(geo_value == tolower(loc))
    }
    return(scoreDf)
}

export_scores_ui = div(
  downloadButton("exportScores", "Download CSV")
)

export_scores_server = function(input, output, df) {
  output$exportScores <- downloadHandler(
    filename = function() {
      filename = paste0("forecast-eval-scores-", input$targetVariable)
      if (input$location != TOTAL_LOCATIONS) {
        filename = paste0(filename, '-', input$location)
      }
      paste0(filename,'-', Sys.Date(), ".csv")
    },
    contentType = 'text/csv',
    content = function(file) {
      withProgress(message = 'Preparing export',
                   detail = 'This may take a while...', value = 0, max = 2, {
        out_df = create_export_df(df, input$targetVariable, input$forecasters, input$aheads, input$location)
        incProgress(1)
        write.csv(out_df, file, row.names=FALSE)
        incProgress(2)
      })
    }
  )
}
