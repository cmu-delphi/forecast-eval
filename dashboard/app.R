library(shiny)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shinyjs)

# COVERAGE_INTERVALS = c('cov_10','cov_20','cov_30','cov_40','cov_50','cov_60','cov_70','cov_80','cov_90','cov_95','cov_98')
COVERAGE_INTERVALS = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "95", "98")
DEATH_FILTER = "deaths_incidence_num"
CASE_FILTER = "confirmed_incidence_num"
FACET_LABELS = c("1" = "Horizon: 1 Week", "2" = "Horizon: 2 Weeks", "3" = "Horizon: 3 Weeks", "4" = "Horizon: 4 Weeks")

# Get and prepare data
getData <- function(filename){
  path = ifelse(
    file.exists(filename),
    filename,
    file.path("../dist/",filename)
  )
  readRDS(path)
}

dfCases <- getData("score_cards_state_cases.rds")
dfDeaths <- getData("score_cards_state_deaths.rds")
df <- rbind(dfCases, dfDeaths)
df <- rbind(dfCases, dfDeaths) %>% filter(!is.na(ae)) #TODO explain
df <- df %>% rename("10" = cov_10, "20" = cov_20, "30" = cov_30, "40" = cov_40, "50" = cov_50, "60" = cov_60, "70" = cov_70, "80" = cov_80, "90" = cov_90, "95" = cov_95, "98" = cov_98)

# Prepare input choices
modelChoices = sort(unique(df$forecaster))
aheadChoices = unique(df$ahead)
locationChoices = unique(toupper(df$geo_value))
dateChoices = rev(sort(as.Date(unique(df$target_end_date))))
coverageChoices = intersect(colnames(df), COVERAGE_INTERVALS)

# About page content
aboutPageText = "
<div style='width: 80%'>
<b><h3>Who We Are</h3></b><br>
Attribution goes here.
<br>
<b><h3>Our Mission</h3></b>
<br>
The goal of the Forecast Evaluation Working Group is to provide a robust set of tools and methods for evaluating the
performance of COVID-19 forecasting models to help epidemiological researchers gain insights into the models' performance, 
and ultimately lead to more accurate forecasting of COVID-19 and other diseases. TODO: obviously this needs work.
<b><h3>About the Data</h3></b><br>
TODO: Is this useful to include? What else should we have here?<br>
Explanation of where data comes from...<br>
<h5><b>Terms</b></h5>
<b>Forecaster:</b> a model producing forecasts<br>
<b>Forecast:</b> a set of data that, for all locales in a geo type, 
includes 4 different “horizons” with predictions for the target variable for each of a certain number of quantiles 
for each horizon <br>
<b>Target Variable:</b> what the forecast is predicting, ie: “weekly incident cases” <br>
<b>Horizon:</b> 1 epi-week, some number of epi-weeks ahead of the current week <br>
<b>Epi-week:</b> Week that starts on a Sunday. If it is Sunday or Monday, 
the next epi-week is the week that starts on that Sunday (going back a day if it is Monday). 
If it is Tuesday-Saturday, it is the week that starts on the subsequent Sunday. <br>
<b>Point Forecast:</b> The value that each forecaster picks as their “most important” prediction. 
For many forecasters this is the 50% quantile prediction. <br>
<b>Geo Type:</b> states, counties or nation
<br>
<b><h3>Explanation of Scoring Methods</h3></b>
<br>
<b>Weighted Interval Score</b><br>
The weighted interval score (WIS) is a proper score that combines a set of interval scores.
See <a href='https://arxiv.org/pdf/2005.12881.pdf'>this preprint</a> about the WIS method for a more in depth explanation.
TODO: How is it actually calculated from the intervals?
<br><br>
<b>Absolute Error</b><br>
The absolute error of a forecast is calculated from the <b>Point Forecast</b>. Usually this is the 50% quantile prediction,
but forecasters can specify their own Point Forecast value. When none is provided explicity, we use the 50% quantile prediction.
<br><br>
<b>Coverage</b><br>
We demonstrate a forecaster's coverage score in multiple ways.
Our <b>single interval coverage plot</b> shows how well a forecaster performed over time for a certain coverage interval.
Our <b>multi-interval coverage plot</b> shows how well all of a forecaster's coverage intervals performed in a given location
or on a given week. <br>
For more detailed information on each plot see the score explanation on the plot itself.
<br><br>
</div>"


# Score explanations
wisExplanation = "<div style = 'margin-left:40px;'> The <b>weighted interval score</b> 
                    takes into account all the quantile predictions submitted...</div>"
aeExplanation = "<div style = 'margin-left:40px;'>
                  The <b>absolute error</b> of a forecast is calculated from the Point Forecast. Usually this is the 50% quantile prediction, but forecasters can specify their own Point Forecast value. When none is provided explicity, we use the 50% quantile prediction. 
                </div>"
coverageExplanation = "<div style = 'margin-left:40px;'>The <b>coverage plot</b> shows how well a forecaster's confidence intervals performed on a given week, across all locations.
                      The x-axis is the confidence interval, and the y-xis is the percentage of time that the observed values of the target variable value fell into that confidence interval.
                      A perfect forecaster on this measure would follow the bold black line.
                      <br>
                      For example, a forecaster wants the observed value to be within the 50% confidence interval in 50% of locations for the given week (or 50% of weeks for the given location).
                      If the y-value at the 50% confidence interval is above the black line, it means that the observed values fell within the forecaster's 50% CI more than 50% of the time, aka the forecaster's 50% CI was under-confident, or too wide that week.
                      Conversly, if the y-values are below the black line, it means that the forecaster's CIs were overconfident that week, or too narrow.
                      Note: Since forecasters only submit 7 quantiles for case predicitons, there are only 3 confidence intervals for cases.
                      </div>"


ui <- fluidPage(
    useShinyjs(),
    titlePanel("Forecaster Evaluation Dashboard"),
    tags$br(),
    sidebarLayout(
      sidebarPanel(id = "inputOptions",
        conditionalPanel(condition = "input.tabset == 'evaluations'",
                                    
          radioButtons("targetVariable", "Target Variable",
                       choices = list("Incident Deaths" = "Deaths", 
                                      "Incident Cases" = "Cases")),
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
          conditionalPanel(condition = "input.scoreType == 'coverage'",
                           selectInput(
                             "coverageInterval",
                             "Coverage Interval",
                             choices = coverageChoices,
                             multiple = FALSE,
                             selected = "95"
                           ),
                        tags$br(),
                        tags$hr(),
                        h5("For Multi-Interval Coverage Plot"),
          ),
          conditionalPanel(condition = "input.scoreType != 'coverage'",
            checkboxInput(
                "averageAllLocations",
                "Average Over All Locations",
                value = FALSE,
            ),
          ),
          conditionalPanel(condition = "!input.averageAllLocations",
            selectInput(
              "location",
              "Location",
              choices = locationChoices,
              multiple = FALSE,
              selected = "CA"
            ),
          ),
          tags$hr(),
        ),
        tags$div(HTML("This app was conceived and built by the Forecast Evaluation Working Group, a collaboration between 
                  the Reich Lab's <a href='https://covid19forecasthub.org/'> Forecast Hub </a> and 
                  Carnegie Mellon's <a href = 'https://delphi.cmu.edu'> Delphi Research Group</a>.
                  <br><br>
                  This data can also be viewed in a weekly report on the Forecast Hub site.")),
        a("View Weekly Report", href = "#"),
        width=3
      ),
      
      mainPanel(
        width=9,
        tabsetPanel(id = "tabset",
          selected = "evaluations",
          tabPanel("About",
                   tags$div(HTML("<br>", aboutPageText))),
          tabPanel("Evaluation Plots", value = "evaluations",
            plotlyOutput(outputId = "summaryPlot"),
            dataTableOutput('renderTable'),
            tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
            hidden(actionLink("multiIntervalCoverage",
                       h4(tags$div(style = "color: black; padding-left:40px;", HTML("Multi-Interval Coverage Plot"),
                                   icon("arrow-circle-down"))))),
            hidden(div(id='multiIntervalCoveragePlot', plotlyOutput(outputId = "multiIntervalCoveragePlot"))),
            actionLink("truthValues",
                       h4(tags$div(style = "color: black; padding-left:40px;", HTML("Observed Values"),
                                   icon("arrow-circle-down")))),
            hidden(div(id='truthPlot', plotlyOutput(outputId = "truthPlot"))),
            actionLink("scoreExplanation",
                       h4(tags$div(style = "color: black; padding-left:40px;", HTML("Explanation Of Score"),
                                   icon("arrow-circle-down")))),
            hidden(div(id='explainScore',
                       tags$div(style = "width: 90%", HTML(wisExplanation)))),
            tags$br(),tags$br()
          )
        ),
      ),
    ),
)


server <- function(input, output, session) {
  
  ##############
  # CREATE PLOTS
  ##############
  summaryPlot = function(scoreDf, targetVariable, scoreType, forecasters,
                         horizon, loc, averageOverAllLocations, coverageInterval = NULL) {
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
      locationText = paste0(', Location: Averaged Over All')
    } else {
      filteredScoreDf <- filteredScoreDf %>% filter(geo_value == tolower(loc)) %>%
        group_by(Forecaster, Date, ahead) %>%
        summarize(Score = Score)
      locationText = paste0(', Location: ', input$location)
    }
    filteredScoreDf <- filteredScoreDf[c("Forecaster", "Date", "Score", "ahead")]
    titleText = paste0('<b>',title,'</b>','<br>', '<sup>',
                       'Target Variable: ', input$targetVariable,
                       locationText,
                       '</sup>')

    p = ggplot(filteredScoreDf, aes(x = Date, y = Score, color = Forecaster)) +
      geom_line() +
      geom_point() +
      labs(x = "", y = title, title=titleText) +
      scale_x_date(date_labels = "%b %Y") +
      scale_y_continuous(labels = scales::comma) +
      facet_wrap(~ ahead, ncol=1, labeller = labeller(ahead = FACET_LABELS))
    if (scoreType == "coverage") {
      p = p + geom_hline(yintercept = .01 * as.integer(coverageInterval))
    }
    return(ggplotly(p + theme_bw() + theme(panel.spacing=unit(2, "lines"))) 
           %>% layout(legend = list(orientation = "h", y = -0.1), margin = list(t=90), height=500, 
                      hovermode = 'x unified') 
           %>% config(displayModeBar = F))
  }
  
  # Create the plot for target variable ground truth
  truthPlot = function(scoreDf, targetVariable = "Cases", location = "US", averageAllLocations = FALSE) {
    signalFilter = CASE_FILTER
    if (targetVariable == "Deaths") {
      signalFilter = DEATH_FILTER
    }
    if (input$averageAllLocations) {
      title = paste0('<b>Incident ', input$targetVariable, '</b>', ' <br><sup>Summed Over All Locations</sup>')
      scoreDf = scoreDf %>% filter(signal == signalFilter) %>%
        group_by(target_end_date) %>% distinct(geo_value, .keep_all = TRUE)
      #TODO should this not include US when US becomes available?
      scoreDf = scoreDf %>% filter(!is.na(actual)) %>%
        group_by(target_end_date) %>%
        summarize(actual = sum(actual))
      
    } else {
      title = paste0('<b> Incident ', input$targetVariable, '</b>')
      scoreDf <- scoreDf %>% filter(signal == signalFilter) %>%
        filter(geo_value %in% tolower(location)) %>% distinct(target_end_date, .keep_all = TRUE) %>% 
        group_by(target_end_date) %>% summarize(actual = actual) #TODO confirm this still works
    }
    scoreDf = scoreDf %>%
      rename(Date = target_end_date, Incidence = actual)
    
    return (ggplotly(ggplot(scoreDf, aes(x = Date, y = Incidence)) +
      geom_line() +
      geom_point() +
      labs(x = "", y = "Incidence", title = title) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%b %Y") + theme_bw()) 
      %>% layout(hovermode = 'x unified')
      %>% config(displayModeBar = F))
  }
  
  # Create alternative, multi-interval coverage plot
  multiIntervalCoveragePlot = function(scoreDf, targetVariable, scoreType, forecasters,
                          horizon, loc, averageOverAllLocations) {
    signalFilter = CASE_FILTER
    if (targetVariable == "Deaths") {
      signalFilter = DEATH_FILTER
    }
    filteredDf = scoreDf %>% filter(signal == signalFilter) %>%
    filter(ahead %in% horizon) %>%
    filter(forecaster %in% forecasters)
    filteredDf = filteredDf %>% rename(Forecaster = forecaster, Date = target_end_date)
    
      filteredDf = filteredDf %>% filter(geo_value == tolower(loc))
      forecasters = unique(filteredDf$Forecaster)
      aheads = unique(filteredDf$ahead)
      coverageDf = data.frame(Ahead=integer(), Forecaster=character(), Interval=character(), Score=double(), stringsAsFactors=FALSE)
      for (horizon in aheads) {
        for (forecaster in forecasters) {
          print(forecaster)
          forecasterDf = filteredDf %>% filter(Forecaster == forecaster) %>% filter(ahead == horizon)
          forecasterDf = Filter(function(x)!all(is.na(x)), forecasterDf)
          intervals = intersect(colnames(forecasterDf), COVERAGE_INTERVALS)
          
          for (interval in intervals) {
            dates = unique(forecasterDf$Date)
            score = sum(forecasterDf[[interval]])/length(dates) * 100
            coverageDf[nrow(coverageDf) + 1,] = c(horizon, forecaster, interval, score)
          }
        }
      }
      coverageDf$Score <- as.numeric(as.character(coverageDf$Score))
      coverageDf$Interval <- as.numeric(as.character(coverageDf$Interval))
      titleText = paste0('<b>Multi-Interval Coverage</b>','<br>', '<sup>',
                         'Target Variable: ', input$targetVariable,
                         ', Location: ', loc,
                         ', Averaged Over All Dates',
                         '</sup>')
      p = ggplot(coverageDf, aes(x = Interval, y = Score, color = Forecaster)) +
        geom_line() +
        geom_point() +
        geom_abline(intercept = 0, slope = 1) +
        scale_y_continuous(limits=c(0, 100)) +
        scale_x_continuous(limits=c(0, 100)) +
        labs(x = "", y = "Coverage Interval", title=titleText) +
        facet_wrap(~ Ahead, ncol=1, labeller = labeller(Ahead = FACET_LABELS))
      return(ggplotly(p + theme_bw() + theme(panel.spacing=unit(2, "lines"))) 
             %>% layout(legend = list(orientation = "h", y = -0.1), margin = list(t=90), height=500, 
                        hovermode = 'x unified', xaxis = list(title = 'Coverage Percentage', titlefont=list(size=11)), 
                        yaxis = list(title = 'Coverage Interval', titlefont=list(size=12))) 
             %>% config(displayModeBar = F))
  }
  

  #############
  # PLOT OUTPUT
  #############
  output$summaryPlot <- renderPlotly({
    summaryPlot(df, input$targetVariable, input$scoreType, input$forecasters, 
                input$aheads, input$location, input$averageAllLocations, input$coverageInterval)
  })
  
  output$multiIntervalCoveragePlot <- renderPlotly({
    multiIntervalCoveragePlot(df, input$targetVariable, input$scoreType, input$forecasters, 
                input$aheads, input$location, input$averageAllLocations)
  })
  
  output$truthPlot <- renderPlotly({
    truthPlot(df, input$targetVariable, input$location, input$averageAllLocations)
  })
  
  
  
  ###################
  # EVENT OBSERVATION
  ###################
  observeEvent(input$truthValues, {
    toggle('truthPlot')
    if (input$truthValues %% 2 == 1) {
      icon = icon("arrow-circle-up")
    } else {
      icon = icon("arrow-circle-down")
    }
    updateActionLink(session, "truthValues",
                       paste0(h4(tags$div(style = "color: black; padding-left:40px;", HTML("Observed Values"), 
                                   icon))))
  })
  observeEvent(input$scoreExplanation, {
    toggle('explainScore')
    if (input$scoreExplanation %% 2 == 1) {
      icon = icon("arrow-circle-up")
    } else {
      icon = icon("arrow-circle-down")
    }
    updateActionButton(session, "scoreExplanation",
                       paste0(h4(tags$div(style = "color: black; padding-left:40px;", HTML("Explanation Of Score"), 
                                          icon))))
  })
  observeEvent(input$multiIntervalCoverage, {
    toggle('multiIntervalCoveragePlot')
    if (input$multiIntervalCoverage %% 2 == 1) {
      icon = icon("arrow-circle-up")
    } else {
      icon = icon("arrow-circle-down")
    }
    updateActionLink(session, "multiIntervalCoverage",
                     paste0(h4(tags$div(style = "color: black; padding-left:40px;", HTML("Multi-Interval Coverage Plot"), 
                                        icon))))
  })
  observeEvent(input$scoreType, {
    if (input$scoreType == "wis") {
      html("explainScore", paste0(wisExplanation))
      hide('multiIntervalCoverage')
      hide('multiIntervalCoveragePlot')
    }
    if (input$scoreType == "ae") {
      html("explainScore", paste0(aeExplanation))
      hide('multiIntervalCoverage')
      hide('multiIntervalCoveragePlot')
    }
    if (input$scoreType == "coverage") {
      html("explainScore", paste0(coverageExplanation))
      show('multiIntervalCoverage')
    }
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
  #TODO update CI and/or horizon based on location? I don't think it is necessary, but can look into it
  
  # Ensure there is always one ahead value selected and one forecaster selected
  observe({
    if(length(input$aheads) < 1) {
      updateCheckboxGroupInput(session, "aheads",
                               selected = 1)
    }
    if(length(input$forecasters) < 1) {
      updateSelectInput(session, "forecasters",
                        selected = c("COVIDhub-ensemble"))
    }
  }) 
}

################
# UTIL FUNCTIONS
################
updateCoverageChoices = function(session, df, targetVariable, forecasterChoices, coverageInput) {
  df = df %>% filter(forecaster %in% forecasterChoices)
  df = Filter(function(x)!all(is.na(x)), df)
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

shinyApp(ui = ui, server = server)
