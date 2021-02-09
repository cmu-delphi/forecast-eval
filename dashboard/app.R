library(shiny)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shinyjs)

COVERAGE_INTERVALS = c('cov_10','cov_20','cov_30','cov_40','cov_50','cov_60','cov_70','cov_80','cov_90','cov_95','cov_98')
DEATH_FILTER = "deaths_incidence_num"
CASE_FILTER = "confirmed_incidence_num"

dfCases <- readRDS("../Report/score_cards_state_cases.rds")
dfDeaths <- readRDS("../Report/score_cards_state_deaths.rds")
df <- rbind(dfCases, dfDeaths) %>% filter(!is.na(ae))
# df <- df %>% rename(cov_10 = 10, cov_20 = 20, cov_30 = 30, cov_40 = 40, cov_50 = 50, cov_60 = 60, cov_70 = 70, cov_80 = 80, cov_90 = 90, cov95 = 95, cov_98 = 98)
# df <- df %>% rename("10" = cov_10, "20" = cov_20, "30" = cov_30, "40" = cov_40, "50" = cov_50, "60" = cov_60, "70" = cov_70, "80" = cov_80, "90" = cov_90, "95" = cov_95, "98" = cov_98)

modelChoices = sort(unique(df$forecaster))
aheadChoices = unique(df$ahead)
locationChoices = unique(toupper(df$geo_value))
dateChoices = rev(sort(as.Date(unique(df$target_end_date))))
coverageLables = lapply(intersect(colnames(df), COVERAGE_INTERVALS), function (x) gsub("[a-zA-Z _]", "",x))
coverageChoices = intersect(colnames(df), COVERAGE_INTERVALS)

wisExplanation = "The <b>weighted interval score</b> takes into account all the quantile predictions submitted..."
aeExplanation = "AE"
coverageExplanation = "The <b>coverage plot</b> shows how well a forecaster's confidence intervals performed on a given week, across all locations.
                      The x-axis is the confidence interval, and the y-xis is the percentage of time that the ground-truth target variable value fell into that confidence interval.
                      A perfect forecaster on this measure would follow the bold black line.
                      <br>
                      For example, a forecaster wants the ground-truth value to be within the 50% confidence interval in 50% of locations for the given week (or 50% of weeks for the given location).
                      If the y-value at the 50% confidence interval is above the black line, it means that the ground-truth values fell within the forecaster's 50% CI more than 50% of the time, aka the forecaster's 50% CI was under-confident, or too wide that week.
                      Conversly, if the y-values are below the black line, it means that the forecaster's CIs were overconfident that week, or too narrow.
                      Note: Since forecasters only submit 7 quantiles for case predicitons, there are only 3 confidence intervals for cases."


ui <- fluidPage(
    useShinyjs(),
    titlePanel("Forecast Eval"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("targetVariable", "Target Variable",
                     choices = list("Incident Cases" = "Cases", 
                                    "Incident Deaths" = "Deaths")),
        radioButtons("scoreType", "Scoring Metric",
                   choices = list("Weighted Interval Score" = "wis", 
                                  "Absolute Error" = "ae",
                                  "Coverage" = "coverage")),
        selectInput(
          "forecasters",
          "Forecasters (type a name to select)",
          choices = modelChoices,
          multiple = TRUE,
          selected = c("COVIDhub-ensemble", "COVIDhub-baseline")
        ),
        checkboxGroupInput(
          "aheads", 
          "Forecast Horizon (Weeks)",
          choices = aheadChoices,
          selected = 1,
          inline = TRUE
        ),
        conditionalPanel(condition = "input.scoreType != 'coverage'",
          checkboxInput(
            "averageAllLocations",
            "Average Over All Locations",
            value = FALSE,
          ),
        ),
        conditionalPanel(condition = "!input.averageAllLocations && input.scoreType != 'coverage'",
          selectInput(
            "location",
            "Location",
            choices = locationChoices,
            multiple = FALSE,
            selected = "CA"
          )
        ),
        conditionalPanel(condition = "input.scoreType == 'coverage'",
          selectInput(
            "coverageInterval",
            "Coverage Interval",
            choices = coverageChoices,
            multiple = FALSE,
            selected = "cov_95"
          )
        ),
        # conditionalPanel(condition = "input.scoreType == 'coverage' && input.coverageAggregate == 'location'",
        #   selectInput(
        #     "date",
        #     "Date",
        #     choices = dateChoices,
        #     multiple = FALSE,
        #   )
        # ), 
        width=3),
      
      mainPanel(
        width=9,
        plotlyOutput(outputId = "summaryPlot"),
        dataTableOutput('renderTable'),
        tags$div(HTML("<br>")),
        tags$div(HTML("<br>")),
        tags$div(HTML("<br>")),
        tags$div(HTML("<br>")),
        plotlyOutput(outputId = "truthPlot"),
        tags$div(HTML("<br>")),
        h3(tags$div(HTML("<br/>Explanation of scoring methods"))),
        actionLink("showExplanationWIS", "WIS"),
        tags$span(HTML("/")),
        actionLink("showExplanationAE", "AE"),
        tags$span(HTML("/")),
        actionLink("showExplanationCoverage", "Coverage"),
        hidden(div(id='explainWIS',
                   tags$div(style = "width: 90%", HTML(wisExplanation)))),
        hidden(div(id='explainAE',
                   tags$div(style = "width: 90%", HTML(aeExplanation)))),
        hidden(div(id='explainCoverage',
                   tags$div(style = "width: 90%", HTML(coverageExplanation)))),
      ),
    ),
)


server <- function(input, output, session) {
  summaryPlot = function(scoreDf, targetVariable, scoreType, forecasters,
                         horizon, loc, averageOverAllLocations, coverageInterval) {
    signalFilter = CASE_FILTER
    if (targetVariable == "Deaths") {
      signalFilter = DEATH_FILTER
    }
    scoreDf <- scoreDf %>% filter(signal == signalFilter) %>%
                           filter(ahead %in% horizon) %>%
                           filter(forecaster %in% forecasters)
    # output$renderTable <- renderDataTable(scoreDf)
    filteredScoreDf <- scoreDf %>% rename(Forecaster = forecaster, Date = target_end_date)
    if (scoreType == "wis") {
      filteredScoreDf <- filteredScoreDf %>% rename(Score = wis)
      title = "Weighted Interval Score"
    }
    if (scoreType == "ae") {
      filteredScoreDf <- filteredScoreDf %>% rename(Score = ae)
      title = "Absolute Error"
    }
    if (scoreType == "coverage") {
      filteredScoreDf <- filteredScoreDf %>% rename(Score = !!coverageInterval)
      title = "Coverage"
    }
    if (averageOverAllLocations || scoreType == "coverage") {
      #TODO should this not include US (when US is added)?
      filteredScoreDf = filteredScoreDf %>% filter(!is.na(Score))
      locations = unique(filteredScoreDf$geo_value)
      filteredScoreDf = filteredScoreDf %>%
        group_by(Forecaster, Date, ahead) %>%
        summarize(Score = sum(Score)/length(locations))
    } else {
      filteredScoreDf <- filteredScoreDf %>% filter(geo_value == tolower(loc)) %>%
        group_by(Forecaster, Date, ahead) %>%
        summarize(Score = Score)
    }
    filteredScoreDf <- filteredScoreDf[c("Forecaster", "Date", "Score", "ahead")]
    facetLabels <- c("1" = "Horizon: 1 Week", "2" = "Horizon: 2 Weeks", "3" = "Horizon: 3 Weeks", "4" = "Horizon: 4 Weeks")
    titleText = paste0(title,'<br>', '<sup>',
                       'Target Variable: ', input$targetVariable,
                       ', Location: ', input$location,
                       '</sup>')
    if (scoreType == "coverage") {
      titleText = paste0(title,'<br>', '<sup>',
                         'Target Variable: ', input$targetVariable,
                         '</sup>')
    }
    
    p = ggplot(filteredScoreDf, aes(x = Date, y = Score, color = Forecaster)) +
      geom_line() +
      geom_point() +
      labs(x = "", y = title, title=titleText) +
      scale_x_date(date_labels = "%b %Y") +
      scale_y_continuous(labels = scales::comma) +
      facet_wrap(~ ahead, ncol=1, labeller = labeller(ahead = facetLabels))
    if (scoreType == "coverage") {
      p = p + geom_hline(yintercept = .01 * as.integer(gsub("[a-zA-Z _]", "",coverageInterval)))
    }
    
    return(ggplotly(p + theme_bw() + theme(panel.spacing=unit(2, "lines"))) 
           %>% layout(legend = list(orientation = "h", y = -0.1), margin = list(t=90), height=470, hovermode = 'x unified') 
           %>% config(displayModeBar = F))
  }
  
  # Create the plot for target variable ground truth
  truthPlot = function(scoreDf, targetVariable = "Cases", location = "US", averageAllLocations = FALSE) {
    signalFilter = CASE_FILTER
    if (targetVariable == "Deaths") {
      signalFilter = DEATH_FILTER
    }
    if (input$averageAllLocations) {
      title = paste0('Incident ', input$targetVariable, " <br><sup>Summed Over All Locations</sup>")
      scoreDf = scoreDf %>% filter(signal == signalFilter) %>%
        group_by(target_end_date) %>% distinct(geo_value, .keep_all = TRUE)
      
      #TODO should this not include US????
      scoreDf = scoreDf %>% filter(!is.na(actual)) %>%
        group_by(target_end_date) %>%
        summarize(actual = sum(actual))
      
    } else {
      title = paste0('Incident ', input$targetVariable)
      scoreDf <- scoreDf %>% filter(signal == signalFilter) %>%
        filter(geo_value %in% tolower(location)) %>% distinct(target_end_date, .keep_all = TRUE) 
    }
    scoreDf = scoreDf %>%
      rename(Date = target_end_date, Incidence = actual)
    
    return (ggplotly(ggplot(scoreDf, aes(x = Date, y = Incidence)) +
      geom_line() +
      geom_point() +
      labs(x = "", y = "Incidence", title = title) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%b %Y") + theme_bw()) %>% config(displayModeBar = F))
  }
  

  #######
  # PLOTS
  #######
  output$summaryPlot <- renderPlotly({
    summaryPlot(df, input$targetVariable, input$scoreType, input$forecasters, 
                input$aheads, input$location, input$averageAllLocations, input$coverageInterval)
  })
  
  output$truthPlot <- renderPlotly({
    truthPlot(df, input$targetVariable, input$location, input$averageAllLocations)
  })
  
  
  
  ###################
  # EVENT OBSERVATION
  ###################
  observeEvent(input$showExplanationWIS, {
    toggle('explainWIS')
    hide('explainAE')
    hide('explainCoverage')
  })
  observeEvent(input$showExplanationAE, {
      toggle('explainAE')
      hide('explainWIS')
      hide('explainCoverage')
  })
  observeEvent(input$showExplanationCoverage, {
    toggle('explainCoverage')
    hide('explainWIS')
    hide('explainAE')
  })
  
  # When the target variable changes, update available forecasters, locations, and CIs to choose from
  observeEvent(input$targetVariable, {
    if (input$targetVariable == 'Deaths') {
      df = df %>% filter(signal == DEATH_FILTER)
    } else {
      df = df %>% filter(signal == CASE_FILTER)
    }
    forecasterChoices = unique(df$forecaster)
    # Ensure previsouly selected options are still allowed
    if (input$forecasters %in% forecasterChoices) {
      selectedForecasters = input$forecasters
    } else {
      selectedForecasters = c("COVIDhub-ensemble", "COVIDhub-baseline")
    }
    updateSelectInput(session, "forecasters",
                      choices = forecasterChoices,
                      selected = selectedForecasters)
    updateLocationChoices(session, df, input$targetVariable, input$forecasters, input$location)
    updateCoverageChoices(session, df, input$targetVariable, input$forecasters, input$coverageInterval)
  })

  # When forecaster selections change, update available aheads, locations, and CIs to choose from
  observeEvent(input$forecasters, {
    df = df %>% filter(forecaster %in% input$forecasters)
    aheadChoices = unique(df$ahead)
    # Ensure previsouly selected options are still allowed
    if (input$aheads %in% aheadChoices) {
      selectedAheads = input$aheads
    } else {
      selectedAheads = 1
    }
    updateCheckboxGroupInput(session, "aheads",
                      choices = aheadChoices,
                      selected = selectedAheads,
                      inline = TRUE)
    updateLocationChoices(session, df, input$targetVariable, input$forecasters, input$location)
    updateCoverageChoices(session, df, input$targetVariable, input$forecasters, input$coverageInterval)
  })
  
  #TODO update CI and/or horizon based on location?
  
  # Ensure there is always one ahead value selected
  observe({
    if(length(input$aheads) < 1){
      updateCheckboxGroupInput(session, "aheads",
                               choices = aheadChoices,
                               selected = 1,
                               inline = TRUE)
    }
  }) 
}

################
# UTIL FUNCTIONS
################
updateCoverageChoices = function(session, df, targetVariable, forecasterChoices, coverageInput) {
  df = df %>% filter(forecaster %in% forecasterChoices)
  df = Filter(function(x)!all(is.na(x)), df)
  coverageLables = lapply(intersect(colnames(df), COVERAGE_INTERVALS), function (x) gsub("[a-zA-Z _]", "",x))
  coverageChoices = intersect(colnames(df), COVERAGE_INTERVALS)
  if (coverageInput %in% coverageChoices) {
    selectedCoverage = coverageInput
  } else {
    selectedCoverage = coverageChoices[1] #TODO might this throw an error?
  }
  updateSelectInput(session, "coverageInterval",
                    choices = coverageChoices,
                    selected = selectedCoverage)
}


updateLocationChoices = function(session, df, targetVariable, forecasterChoices, locationInput) {
  df = df %>% filter(forecaster %in% forecasterChoices)
  locationChoices = unique(toupper(df$geo_value))
  if (locationInput %in% locationChoices) {
    selectedLocation = locationInput
  } else {
    selectedLocation = locationChoices[1] #TODO might this throw an error? less likely
  }
  updateSelectInput(session, "location",
                    choices = locationChoices,
                    selected = selectedLocation)
}

# TO USE LATER FOR OTHER TYPE OF COVERAGE PLOT
# if (scoreType == "coverage") {
#   # If aggregating over all dates, filter based on the location chosen
#   if (coverageAggregateBy == "date") {
#     scoreDf <- scoreDf %>% filter(geo_value == tolower(loc))
#   }
#   if (coverageAggregateBy == "location") {
#     scoreDf <- scoreDf %>% filter(target_end_date == date)
#   }
#   if (nrow(scoreDf) == 0) {
#     output$warningText <- renderText("No data for these options.")
#     return()
#   } else {
#     output$warningText <- renderText("")
#   }
#   
#   # Case forecasts only cover 7 quantiles, thus only 3 coverage intervals
#   intervals <- c(10,20,30,40,50,60,70,80,90,95,98)
#   if (targetVariable == "Cases") {
#     intervals <- c(50,80,95)
#   }
#   coverageDf <- data.frame(Forecaster=character(), Interval=double(), Score=double(), stringsAsFactors=FALSE)
#   forecasters <- unique(scoreDf$forecaster)
#   for(i in 1:length(forecasters)) {
#     forecasterDf <- scoreDf %>% filter(forecaster == forecasters[i])
#     for (j in 1:length(intervals)) {
#       covScore <- switch(toString(intervals[j]),
#                          "10" = forecasterDf$cov_10,
#                          "20" = forecasterDf$cov_20,
#                          "30" = forecasterDf$cov_30,
#                          "40" = forecasterDf$cov_40,
#                          "50" = forecasterDf$cov_50,
#                          "60" = forecasterDf$cov_60,
#                          "70" = forecasterDf$cov_70,
#                          "80" = forecasterDf$cov_80,
#                          "90" = forecasterDf$cov_90,
#                          "95" = forecasterDf$cov_95,
#                          "98" = forecasterDf$cov_98)
#       if (targetVariable == "Cases") {
#         covScore <- switch(toString(intervals[j]),
#                            "50" = forecasterDf$cov_50,
#                            "80" = forecasterDf$cov_80,
#                            "95" = forecasterDf$cov_95)
#       }
#       forecasterDf <- forecasterDf %>% filter(!is.na(covScore))
#       covScore <- covScore[!is.na(covScore)]
#       if (coverageAggregateBy == "date") {
#         dates <- unique(forecasterDf$target_end_date)
#         covScore <- sum(covScore)/length(dates) * 100
#       }
#       if (coverageAggregateBy == "location") {
#         locations <- unique(forecasterDf$geo_value)
#         covScore <- sum(covScore)/length(locations) * 100
#       }
#       coverageDf[nrow(coverageDf) + 1,] = c(forecasters[i], intervals[j], covScore)
#     }
#   }
#   coverageDf$Score <- as.numeric(as.character(coverageDf$Score))
#   coverageDf$Interval <- as.numeric(as.character(coverageDf$Interval))
#   # output$renderTable <- renderDataTable(scoreDf)
#   
#   title = "Coverage"
#   pad = -410
#   p = ggplot(coverageDf, aes(x = Interval, y = Score, color = Forecaster)) +
#     geom_line() +
#     geom_point() +
#     geom_abline(intercept = 0, slope = 1) +
#     labs(x = "Coverage Percentage", y = "Coverage Interval") +
#     scale_y_continuous(limits=c(0, 100)) +
#     scale_x_continuous(limits=c(0, 100))
#   
# }


shinyApp(ui = ui, server = server)

