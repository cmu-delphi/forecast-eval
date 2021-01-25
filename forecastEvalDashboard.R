library(shiny)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(stringr)

dfCases <- readRDS("Report/score_cards_state_cases.rds")
dfDeaths <- readRDS("Report/score_cards_state_deaths.rds")
df <- rbind(dfCases, dfDeaths)
modelChoices = sort(unique(df$forecaster))
aheadChoices = unique(df$ahead)
locationChoices = unique(df$geo_value) # TODO maybe make this capitalized
dateChoices = rev(unique(df$target_end_date))

ui <- fluidPage(
    titlePanel("Forecast Eval"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("targetVariable", "Target Variable",
                     choices = list("Incident Cases" = "cases", 
                                    "Incident Deaths" = "deaths")),
        radioButtons("scoreType", "Score Type",
                   choices = list("Weighted Interval Score" = "wis", 
                                  "Mean Average Error" = "ae",
                                  "Coverage" = "coverage")),
        selectInput(
          "forecasters",
          "Forecasters",
          choices = modelChoices,
          multiple = TRUE,
          selected = "COVIDhub-ensemble"
        ),
        radioButtons("ahead", "Ahead",
                     choices = list("1 week" = aheadChoices[1], "2 weeks" = aheadChoices[2], 
                                    "3 weeks" = aheadChoices[3], "4 weeks" = aheadChoices[4])),
        conditionalPanel(condition = "input.scoreType != 'coverage'",
          selectInput(
            "location",
            "Location",
            choices = locationChoices,
            multiple = FALSE,
            selected = "US"
          )
        ),
        conditionalPanel(condition = "input.scoreType == 'coverage'",
          selectInput(
            "date",
            "Date",
            choices = dateChoices,
            multiple = FALSE,
          )
        ),
      ),
      mainPanel(
        plotOutput(outputId = "summaryPlot"),
        dataTableOutput('renderTable'),
        textOutput("printText")
      ),
    ),
)


server <- function(input, output, session) {
  
  summaryPlot = function(scoreDf, target_variable = "cases", score_type = "wis", forecasters = "All",
                          horizon = 1, loc = "US", date = NULL) {
    
    signalFilter = "confirmed_incidence_num"
    if (target_variable == "deaths") {
      signalFilter = "deaths_incidence_num"
    }
    scoreDf <- scoreDf %>% filter(signal == signalFilter) %>%
                           filter(ahead == horizon) %>%
                           filter(forecaster %in% forecasters)
    
    output$renderTable <- renderDataTable(scoreDf)
    # TODO better way of doing this
    if (nrow(scoreDf) == 0) {
      output$printText <- renderText("No data for these options.")
      return()
    } else {
      output$printText <- renderText("")
    }
    if (score_type == "coverage") {      
      scoreDf <- scoreDf %>% filter(target_end_date == date)
      if (nrow(scoreDf) == 0) {
        output$printText <- renderText("No data for these options.")
        return()
      } else {
        output$printText <- renderText("")
      }
      
      intervals <- c(10,20,30,40,50,60,70,80,90,95,98)
      df <- data.frame(Date=as.Date(character()),
                       File=character(), 
                       User=character(), 
                       stringsAsFactors=FALSE) 
      
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
          forecasterDf <- forecasterDf %>% filter(!is.na(covScore))
          locations <- unique(forecasterDf$geo_value)
          covScore <- covScore[!is.na(covScore)]
          covScore <- sum(covScore)/length(locations) * 100
          coverageDf[nrow(coverageDf) + 1,] = c(forecasters[i], intervals[j], covScore)
        }
      }
      coverageDf$Score <- as.numeric(as.character(coverageDf$Score))
      coverageDf$Interval <- as.numeric(as.character(coverageDf$Interval))
      output$renderTable <- renderDataTable(coverageDf)
      
      
      ggplot(coverageDf, aes(x = Interval, y = Score)) +
        geom_line(aes(color = Forecaster, linetype = Forecaster)) +
        geom_point(aes(color = Forecaster)) +
        scale_y_continuous(limits=c(0, 100)) +
        scale_x_continuous(limits=c(0, 100)) +
        geom_abline(intercept = 0, slope = 1) +
        ylab("Coverage")
    }
    else {
      scoreDf <- scoreDf %>% filter(geo_value == loc)
      axes = aes(x = target_end_date, y = wis)
      ylab = "Weighted Interval Score"
      if (score_type == "ae") {
        ylab = "Mean Average Error"
        axes = aes(x = target_end_date, y = ae)
      }
      
      ggplot(scoreDf, axes) +
        geom_line(aes(color = forecaster, linetype = forecaster)) +
        geom_point(aes(color = forecaster)) +
        labs(x = "Date", y = ylab, color = "Forecaster", linetype = "Forecaster")
    }
  }
  # output$renderTable <- renderDataTable(scoreDf)
  output$summaryPlot <- renderPlot({
    summaryPlot(df, input$targetVariable, input$scoreType, input$forecasters, input$ahead,
                input$location, input$date)
  })
  
  
  
  # observe({
    # forecasterChoices = unique(dfCases$forecaster)
    # df = dfCases
    # if (input$targetVariable == 'deaths') {
    #   forecasterChoices = unique(dfDeaths$forecaster)
    #   df = dfDeaths
    # }
    # dfAhead = df %>% filter(forecaster %in% input$forecasters)
    # aheadChoices = unique(dfAhead$ahead)
    # dfLocation = dfAhead %>% filter(ahead %in% input$ahead)
    # locationChoices = unique(dfLocation$geo_value)
    # 
    # output$renderTable <- renderDataTable(dfLocation)
    # 
    # updateSelectInput(session, "forecasters",
    #                   choices = forecasterChoices,
    #                   selected = "COVIDhub-ensemble",)
    # updateRadioButtons(session, "ahead",
    #                   choices = aheadChoices)
    # updateSelectInput(session, "location",
    #                   choices = locationChoices,
    #                   selected = "ak",)
  # })
  
}

shinyApp(ui = ui, server = server)

