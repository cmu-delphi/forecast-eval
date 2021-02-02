library(shiny)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

dfCases <- readRDS("../Report/score_cards_state_cases.rds")
dfDeaths <- readRDS("../Report/score_cards_state_deaths.rds")
df <- rbind(dfCases, dfDeaths)
modelChoices = sort(unique(df$forecaster))
# modelChoicesDeaths = sort(unique(dfDeaths$forecaster))
aheadChoices = unique(df$ahead)
locationChoices = unique(toupper(df$geo_value))

dateChoices = rev(sort(as.Date(unique(df$target_end_date))))

ui <- fluidPage(
    titlePanel("Forecast Eval"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("targetVariable", "Target Variable",
                     choices = list("Incident Cases" = "cases", 
                                    "Incident Deaths" = "deaths")),
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
        radioButtons("ahead", "Forecast Horizon",
                     choices = list("1 week" = aheadChoices[1], 
                                    "2 weeks" = aheadChoices[2], 
                                    "3 weeks" = aheadChoices[3], 
                                    "4 weeks" = aheadChoices[4])),
        
        conditionalPanel(condition = "input.scoreType == 'coverage'",
           radioButtons("coverageAggregate", "Aggregate Over",
                        choices = list("All Locations" = "location", 
                                       "All Forecast Target Dates" = "date"))
        ),
        conditionalPanel(condition = "input.scoreType != 'coverage' || input.coverageAggregate == 'date'",
          selectInput(
            "location",
            "Location",
            choices = locationChoices,
            multiple = FALSE,
            selected = "CA"
          )
        ),
        conditionalPanel(condition = "input.scoreType == 'coverage' && input.coverageAggregate == 'location'",
          selectInput(
            "date",
            "Date",
            choices = dateChoices,
            multiple = FALSE,
          )
        ),
      ),
      mainPanel(
        plotlyOutput(outputId = "summaryPlot"),
        dataTableOutput('renderTable'),
        textOutput("warningText"),
        h3(tags$div(HTML("<br/>Explanation of scoring methods"))),
        actionLink("showExplanationWIS", "WIS"),
        tags$span(HTML("/")),
        actionLink("showExplanationMAE", "MAE"),
        tags$span(HTML("/")),
        actionLink("showExplanationCoverage", "Coverage"),
        textOutput("scoreExplanation"),
      ),
    ),
)


server <- function(input, output, session) {
  
  summaryPlot = function(scoreDf, targetVariable = "cases", scoreType = "wis", 
                         forecasters = "All", horizon = 1, loc = "us", 
                         coverageAggregateBy = "date", date = NULL) {
    signalFilter = "confirmed_incidence_num"
    if (targetVariable == "deaths") {
      signalFilter = "deaths_incidence_num"
    }
    scoreDf <- scoreDf %>% filter(signal == signalFilter) %>%
                           filter(ahead == horizon) %>%
                           filter(forecaster %in% forecasters)
    
    # TODO better way of doing this
    if (nrow(scoreDf) == 0) {
      output$warningText <- renderText("No data for these options.")
      return()
    } else {
      output$warningText <- renderText("")
    }
    if (scoreType == "coverage") {
      # If aggregating over all dates, filter based on the location chosen
      if (coverageAggregateBy == "date") {
        scoreDf <- scoreDf %>% filter(geo_value == tolower(loc))
      }
      if (coverageAggregateBy == "location") {
        scoreDf <- scoreDf %>% filter(target_end_date == date)
      }
      if (nrow(scoreDf) == 0) {
        output$warningText <- renderText("No data for these options.")
        return()
      } else {
        output$warningText <- renderText("")
      }
      
      # Case forecasts only cover 7 quantiles, thus only 3 coverage intervals
      intervals <- c(10,20,30,40,50,60,70,80,90,95,98)
      if (targetVariable == "cases") {
        intervals <- c(50,80,95)
      }
      coverageDf <- data.frame(Forecaster=character(), Interval=double(), Score=double(), stringsAsFactors=FALSE)
      forecasters <- unique(scoreDf$forecaster)
      for(i in 1:length(forecasters)) {
        forecasterDf <- scoreDf %>% filter(forecaster == forecasters[i])
        for (j in 1:length(intervals)) {
          covScore <- switch(toString(intervals[j]),
                   "10" = forecasterDf$cov_10,
                   "20" = forecasterDf$cov_20,
                   "30" = forecasterDf$cov_30,
                   "40" = forecasterDf$cov_40,
                   "50" = forecasterDf$cov_50,
                   "60" = forecasterDf$cov_60,
                   "70" = forecasterDf$cov_70,
                   "80" = forecasterDf$cov_80,
                   "90" = forecasterDf$cov_90,
                   "95" = forecasterDf$cov_95,
                   "98" = forecasterDf$cov_98)
          if (targetVariable == "cases") {
            covScore <- switch(toString(intervals[j]),
                    "50" = forecasterDf$cov_50,
                    "80" = forecasterDf$cov_80,
                    "95" = forecasterDf$cov_95)
          }
          forecasterDf <- forecasterDf %>% filter(!is.na(covScore))
          covScore <- covScore[!is.na(covScore)]
          if (coverageAggregateBy == "date") {
            dates <- unique(forecasterDf$target_end_date)
            covScore <- sum(covScore)/length(dates) * 100
          }
          if (coverageAggregateBy == "location") {
            locations <- unique(forecasterDf$geo_value)
            covScore <- sum(covScore)/length(locations) * 100
          }
          coverageDf[nrow(coverageDf) + 1,] = c(forecasters[i], intervals[j], covScore)
        }
      }
      coverageDf$Score <- as.numeric(as.character(coverageDf$Score))
      coverageDf$Interval <- as.numeric(as.character(coverageDf$Interval))
      output$renderTable <- renderDataTable(scoreDf)
      
      p = ggplot(coverageDf, aes(x = Interval, y = Score, color = Forecaster)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limits=c(0, 100)) +
        scale_x_continuous(limits=c(0, 100)) +
        geom_abline(intercept = 0, slope = 1) +
        labs(x = "Coverage Percentage", y = "Coverage Interval")
    }
    else {
      scoreDf <- scoreDf %>% filter(geo_value == tolower(loc)) %>% rename(Forecaster = forecaster, Date = target_end_date)
      if (scoreType == "wis") {
        scoreDf <- scoreDf %>% rename(Score = wis)
        ylab = "Weighted Interval Score"
      }
      if (scoreType == "ae") {
        scoreDf <- scoreDf %>% rename(Score = ae)
        ylab = "Absolute Error"
      }
      keep <- c("Forecaster", "Date", "Score")
      scoreDf <- scoreDf[keep]
      output$renderTable <- renderDataTable(scoreDf)
      
      p = ggplot(scoreDf, aes(x = Date, y = Score, color = Forecaster)) +
        geom_line() +
        geom_point() +
        labs(x = "", y = ylab, title=ylab) +
        scale_x_date(date_labels = "%b %Y") 

    }
    return(ggplotly(p + theme_bw()) %>% config(displayModeBar = F) 
           %>% layout(hovermode = 'x unified') %>%  layout(yaxis=list(fixedrange=TRUE))) %>%
      layout(title = list(text = paste0('US State Population and Life Expectancy',
                                        '<br>',
                                        '<sup>',
                                        'Life expectancy 1969-1971; Population estimate as of July 1, 1975',
                                        '</sup>')))
    
  }
  

  output$summaryPlot <- renderPlotly({
    summaryPlot(df, input$targetVariable, input$scoreType, input$forecasters, 
                input$ahead, input$location, input$coverageAggregate, input$date)
  })
  
  
  ###################
  # EVENT OBSERVATION
  ###################
  observeEvent(input$showExplanationWIS, {
    output$scoreExplanation <- renderText({
      "The weighted interval score takes into account all the quantile predictions submitted..."
    })
  })
  observeEvent(input$showExplanationMAE, {
    output$scoreExplanation <- renderText({
      "Absolute error is calculated by..."
    })
  })
  observeEvent(input$showExplanationCoverage, {
    output$scoreExplanation <- renderText({
      "The coverage plot shows how well a forecaster's confidence intervals performed on a given week, across all locations.
      The x-axis is the confidence interval, and the y-xis is the percentage of time that the ground-truth target variable value fell into that confidence interval.
      A perfect forecaster on this measure would follow the bold black line.
      For example, a forecaster wants the ground-truth value to be within the 50% confidence interval in 50% of locations for the given week (or 50% of weeks for the given location).
      If the y-value at the 50% confidence interval is above the black line, it means that the ground-truth values fell within the forecaster's 50% CI more than 50% of the time, aka the forecaster's 50% CI was under-confident, or too wide that week.
      Conversly, if the y-values are below the black line, it means that the forecaster's CIs were overconfident that week, or too narrow.
      Note: Since forecasters only submit 7 quantiles for case predicitons, there are only 3 confidence intervals for cases."
    })
  })
  
  # When the target variable changes, update available forecasters to choose from
  observeEvent(input$targetVariable, {
    forecasterChoices = unique(dfCases$forecaster)
    if (input$targetVariable == 'deaths') {
      forecasterChoices = unique(dfDeaths$forecaster)
    }
    updateSelectInput(session, "forecasters",
                      choices = forecasterChoices,
                      selected = input$forecasters)
  })
  
  # When forecaster selections change, update available aheads to choose from
  observeEvent(input$forecasters, {
    dfAhead = df %>% filter(forecaster %in% input$forecasters)
    aheadChoices = unique(dfAhead$ahead)
    updateRadioButtons(session, "ahead",
                       choices = aheadChoices)
  })
}

shinyApp(ui = ui, server = server)

