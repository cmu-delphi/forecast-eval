
create_export_df = function(scoreDf, targetVariable, forecasters, horizon, loc) {
    allLocations = FALSE
    if (loc == TOTAL_LOCATIONS) {
      allLocations = TRUE
    }
    signalFilter = CASE_FILTER
    if (targetVariable == "Deaths") {
      signalFilter = DEATH_FILTER
    }
    scoreDf = scoreDf %>%
      filter(signal == signalFilter) %>%
      filter(ahead %in% horizon) %>%
      filter(forecaster %in% forecasters)
    return(scoreDf)
}

export_scores_ui = downloadButton(id="exportScores", "Download CSV")

export_scores_server = function(output, input, df) {
  output$exportScores <- downloadHandler(
    filename = function() {
      paste0("forecast-eval-scores-", Sys.Date(), ".csv")
    },
    contentType = 'text/csv',
    content = function(file) {
      out_df = create_export_df(df, input$targetVariable, input$forecasters,
                input$aheads, input$location)
      write.csv(out_df, file)
    }
  )
}
