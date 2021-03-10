library(shiny)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(plotly)
library(shinyjs)
library(tsibble)
library(aws.s3)

COVERAGE_INTERVALS = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "95", "98")
DEATH_FILTER = "deaths_incidence_num"
CASE_FILTER = "confirmed_incidence_num"

# Connect to AWS s3bucket
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
s3bucket = tryCatch(
  {
    get_bucket(bucket = 'forecast-eval')
  },
  error = function(e) {
    e
    return(NULL)
  }
)

# Get and prepare data
getData <- function(filename){
  if(!is.null(s3bucket)) {
    tryCatch(
      {
        s3readRDS(object = filename, bucket = s3bucket)
      },
      error = function(e) {
        e
        getFallbackData(filename)
      }
    )
  } else {
    getFallbackData(filename)
  }
}

getFallbackData = function(filename) {
  path = ifelse(
    file.exists(filename),
    filename,
    file.path("../dist/",filename)
  )
  readRDS(path)
}

dfStateCases <- getData("score_cards_state_cases.rds")
dfStateDeaths <- getData("score_cards_state_deaths.rds")
dfNationCases = getData("score_cards_nation_cases.rds")
dfNationDeaths = getData("score_cards_nation_deaths.rds")
df <- rbind(dfStateCases, dfStateDeaths, dfNationCases, dfNationDeaths)
df <- df %>% rename("10" = cov_10, "20" = cov_20, "30" = cov_30, "40" = cov_40, "50" = cov_50, "60" = cov_60, "70" = cov_70, "80" = cov_80, "90" = cov_90, "95" = cov_95, "98" = cov_98)

# Prepare color palette
set.seed(100)
forecaster_rand <- sample(unique(df$forecaster))
color_palette = setNames(object = viridis(length(unique(df$forecaster))), nm = forecaster_rand)

# Prepare input choices
forecasterChoices = sort(unique(df$forecaster))
aheadChoices = unique(df$ahead)
locationChoices = unique(toupper(df$geo_value))
locationChoices = locationChoices[c(length(locationChoices), (1:length(locationChoices)-1))] # Move US to front of list
coverageChoices = intersect(colnames(df), COVERAGE_INTERVALS)

# Score explanations
wisExplanation = "<div style = 'margin-left:40px;'> The <b>weighted interval score</b> (WIS) is a proper score that combines a set of interval scores.
                   See <a href='https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008618'>this article</a> about the WIS method for 
                   a more in depth explanation. The WIS factors in both the sharpness of prediction intervals and their calibration (or coverage) of the 
                   actual observations.
                </div>"
aeExplanation = "<div style = 'margin-left:40px;'>
                  The <b>absolute error</b> of a forecast is calculated from the Point Forecast. 
                  Usually this is the 50% quantile prediction, but forecasters can specify their own Point Forecast value. 
                  When none is provided explicity, we use the 50% quantile prediction. 
                </div>"
coverageExplanation = "<div style = 'margin-left:40px;'>
                        The <b>coverage plot</b> shows how well a forecaster's confidence intervals performed on a given week, across all locations.
                        The horizontal black line is the selected confidence interval, and the y-values are the percentage of time that the observed
                        values of the target variable value fell into that confidence interval. 
                        A perfect forecaster on this measure would follow the black line.
                        <br><br>
                        For example, a forecaster wants the observed values to be within the 50% confidence interval in 50% of locations for the given week. 
                        If the y-value is above the horizontal line, it means that the observed values fell within the forecaster's 50% CI more than 50% of 
                        the time, aka the forecaster's 50% CI was under-confident that week, or too wide. Conversely, if the y-value is below the line, 
                        it means that the forecaster's 50% CI was over-confident that week, or too narrow.
                      </div>"
# Truth data disclaimer
observedValueDisclaimer = 
  "All forecasts are evaluated against the latest version of observed data. Scores of past forecasts may change as observed data is revised."

# About page content
aboutPageText = HTML("
<div style='width: 80%'>
<b><h3><u>Who We Are</u></h3></b><br>
The Forecast Evaluation Research Collaborative was founded by the <a href='https://reichlab.io/'>Reich Lab</a>
at University of Massachusetts Amherst and the Carnegie Mellon University <a href = 'https://delphi.cmu.edu'> Delphi Group</a>.
Both groups are funded by the CDC as Centers of Excellence for Influenza and COVID-19 Forecasting. 
We have partnered together on this project to focus on providing a robust set of tools and methods for evaluating the performance of epidemic forecasts.
<br><br>
The collaborative’s mission is to help epidemiological researchers gain insights into the performance of their forecasts, 
and ultimately lead to more accurate forecasting of epidemics. 
<br><br>
Both groups have led initiatives related to COVID-19 data and forecast curation. 
The Reich Lab has created the <a href='https://covid19forecasthub.org/'>COVID-19 Forecast Hub</a>, 
a collaborative effort with over 80 groups submitting forecasts to be part of the official 
<a href='https://www.cdc.gov/coronavirus/2019-ncov/covid-data/mathematical-modeling.html'> CDC COVID-19 ensemble forecast</a>.
The Delphi Group has created COVIDcast, a platform for <a href='https://delphi.cmu.edu/covidcast/'>epidemiological surveillance data</a>, 
and runs the <a href='https://delphi.cmu.edu/covidcast/surveys/'>Delphi Pandemic Survey via Facebook</a>, 
which is a <a href='https://delphi.cmu.edu/blog/2020/09/21/can-symptoms-surveys-improve-covid-19-forecasts/'>valuable signal</a> 
for Delphi’s participation in the ensemble forecast.
<br><br>
The Forecaster Evaluation Dashboard is a collaborative project, which has been made possible by the 13 pro bono Google.org Fellows 
who have spent 6 months working full-time with the Delphi Group. 
Google.org is <a href='https://www.google.org/covid-19/'>committed</a> to the recovery of lives 
and communities that have been impacted by COVID-19 and investing in developing the science to mitigate the damage of future pandemics.
<br><br>
<br><h4><b>Collaborators</b></h4>
<br>
From the Forecast Hub: Estee Cramer, Nicholas Reich, <a href='https://covid19forecasthub.org/doc/team/'>the COVID-19 Forecast Hub Team</a>
<br>
From the Delphi Research Group: Jed Grabman, Kate Harwood, Chris Scott, Jacob Bien, Daniel McDonald, Logan Brooks
<br><br><br><b><h3><u>About the Data</u></h3></b>
<br><h4><b>Sources</b></h4>
<b>Observed values</b> are from the 
<a href='https://github.com/CSSEGISandData/COVID-19'>COVID-19 Data Repository</a> 
by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University.
<br><br><b>Forecaster predictions</b> are drawn from the <a href='https://github.com/reichlab/covid19-forecast-hub/'>COVID-19 Forecast Hub GitHub repository</a>
<br><br>Data for the dashboard is pulled once a week from these sources, on Tuesdays.
<br><br>
<h4><b>Terms</b></h4>
<ul><li><b>Forecaster</b> 
<div style = 'margin-left:40px;'>A model producing quantile predictions</div></li>
<li><b>Forecast</b>
<div style = 'margin-left:40px;'>A set of data that, for all locales in a geo type, 
includes predictions for a target variable for each of a certain number of quantiles 
for each of a certain number of horizons </div></li>
<li><b>Target Variable</b>
<div style = 'margin-left:40px;'>What the forecast is predicting, ie: “weekly incident cases”</div></li>
<li><b>Horizon</b>
<div style = 'margin-left:40px;'>
The duration of time between when the prediction was made and the predicted event, typically in units of epidemiological weeks.
</div></li>
<li><b>Epidemiological Week (Epi-week)</b>
<div style = 'margin-left:40px;'>Week that starts on a Sunday. If it is Sunday or Monday, 
the next epi-week is the week that starts on that Sunday (going back a day if it is Monday). 
If it is Tuesday-Saturday, it is the week that starts on the subsequent Sunday, following 
<a href='https://wwwn.cdc.gov/nndss/document/MMWR_week_overview.pdf'>CDC convention</a>.</div></li>

<li><b>Point Forecast</b>
<div style = 'margin-left:40px;'>The value that each forecaster picks as their “most likely” prediction. 
For many forecasters this is the 50th quantile of the predictive distribution, for others it might be the mean of the distribution.
</div></li>
<li><b>Geo Type</b>
<div style = 'margin-left:40px;'>States or U.S. as a nation</div></li></ul>
<br><h4><b>Dashboard Inclusion Criteria</b></h4>
<ul>
<li> Includes only weekly deaths incidence and weekly case incidence target variables</li>
<li> Includes only horizon < 5 weeks ahead</li>
<li> Includes only geo values that are 2 characters (states / territories / nation)</li>
<li> Includes only non-NA target dates (if the date is not in yyyy/mm/dd, the prediction will not be included)</li>
<li> Includes only predictions with at least 3 quantile values</li>
<li> Includes only one file per forecaster per week (according to forecast date). That file must be from a Sunday or Monday. If both are present, we keep the Monday data.</li>
<li> If a forecaster updates a file after that Monday, we do not include the new predictions</li>
</ul>
<br><h4><b>Notes on the Data</b></h4>
<ul>
<li>When totaling over all locations, these locations include states and territories and do not include nationwide forecasts. 
We only include states and territories common to the selected forecasters (over all time) that have data for at least one location.</li>
<li>We do include revisions of observed values, meaning the scores for forecasts made in the past can change. 
Scores change as our understanding of the truth changes.</li>
</ul>
<br><br>
<b><h3><u>Explanation of Scoring Methods</u></h3></b>
<br>
<b>Weighted Interval Score</b><br>", wisExplanation,
"<br><br>
<b>Absolute Error</b><br>", aeExplanation, 
"<br><br>
<b>Coverage</b><br>", coverageExplanation, 
"<br><br></div>")


ui <- fluidPage(
    useShinyjs(),
    titlePanel("COVID-19 Forecaster Evaluation Dashboard"),
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
              p("Forecasters", tags$br(), tags$span("Type a name or select from dropdown", style="font-weight:normal; font-size:13px")),
              choices = forecasterChoices,
              multiple = TRUE,
              selected = c("COVIDhub-baseline", "COVIDhub-ensemble")
            ),
            tags$p("Some forecasters may not have data for the chosen location", style="margin-top:-20px; font-size:12px"),
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
            ),
            conditionalPanel(condition = "!input.allLocations && input.scoreType != 'coverage'",
                             selectInput(
                               "location",
                               "Location",
                               choices = locationChoices,
                               multiple = FALSE,
                               selected = "US"
                             )
            ),
            conditionalPanel(condition = "input.scoreType != 'coverage'",
                             checkboxInput(
                               "allLocations",
                               "Totals Over All Locations",
                               value = FALSE,
                             )
            ),
            tags$hr(),
        ),
        tags$div(HTML("This app was conceived and built by the Forecast Evaluation Research Collaborative, 
                  a collaboration between the <a href='http://reichlab.io/'>UMass-Amherst Reich Lab's</a> 
                  <a href='https://covid19forecasthub.org/'>COVID-19 Forecast Hub</a>
                  and Carnegie Mellon's <a href = 'https://delphi.cmu.edu'>Delphi Research Group</a>.")),
                  # <br><br>
                  # This data can also be viewed in a weekly report on the Forecast Hub site. TODO need link")),
        # a("View Weekly Report", href = "#"),
        width=3,
      ),
      
      mainPanel(
        width=9,
        tabsetPanel(id = "tabset",
          selected = "evaluations",
          tabPanel("About",
                   tags$div(HTML("<br>", aboutPageText))),
          tabPanel("Evaluation Plots", value = "evaluations",
            textOutput('renderWarningText'),
            plotlyOutput(outputId = "summaryPlot"),
            tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),
            HTML('<div style=padding-left:40px>'),
            textOutput('renderAggregateText'),
            textOutput('renderLocations'),
            HTML('</div>'),
            
            actionLink("scoreExplanation",
                       h4(tags$div(style = "color: black; padding-left:40px;", HTML("Explanation Of Score"),
                                   icon("arrow-circle-down")))),
            hidden(div(id='explainScore',
                       tags$div(style = "width: 90%", HTML("")))),
            actionLink("truthValues",
                       h4(tags$div(style = "color: black; padding-left:40px;", HTML("Observed Values"),
                                   icon("arrow-circle-down")))),
            hidden(div(id="truthSection", hidden(div(id='truthPlot', 
                                HTML('<div style=padding-left:40px>'), textOutput('renderObservedValueDisclaimer'), HTML('</div>'), 
                                plotlyOutput(outputId = "truthPlot"))))),
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
                         horizon, loc, allLocations, coverageInterval = NULL) {
    signalFilter = CASE_FILTER
    if (targetVariable == "Deaths") {
      signalFilter = DEATH_FILTER
    }
    scoreDf = scoreDf %>% 
      filter(signal == signalFilter) %>%
      filter(ahead %in% horizon) %>%
      filter(forecaster %in% forecasters)
    
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
    locationsIntersect = list()
    if (allLocations || scoreType == "coverage") {
      filteredScoreDf = filteredScoreDf %>% filter(!is.na(Score))
      # Create df with col for all locations across each unique date, ahead and forecaster combo
      locationDf = filteredScoreDf %>% group_by(Forecaster, Date, ahead) %>% 
        summarize(location_list = paste(sort(unique(geo_value)),collapse=","))
      # Create a list containing each row's location list
      locationList = sapply(locationDf$location_list, function(x) strsplit(x, ","))
      locationList = lapply(locationList, function(x) x[x != 'us'])
      # Get the intersection of all the locations in these lists
      locationsIntersect = unique(Reduce(intersect, locationList))
      filteredScoreDf = filteredScoreDf %>% filter(geo_value %in% locationsIntersect)
      aggregateText = "*All states and territories common to the selected forecasters (over all time) that have data for at least one location."
      if (scoreType == "coverage") {
        aggregate = "Averaged"
        filteredScoreDf = filteredScoreDf %>%
          group_by(Forecaster, Date, ahead) %>%
          summarize(Score = sum(Score)/length(locationsIntersect), actual = sum(actual))
        output$renderAggregateText = renderText(paste(aggregateText," Some forecasters may not have any data for the coverage interval chosen. Locations inlcuded: "))
      }
      else {
        aggregate = "Totaled"
        filteredScoreDf = filteredScoreDf %>%
          group_by(Forecaster, Date, ahead) %>%
          summarize(Score = sum(Score), actual = sum(actual))
        output$renderAggregateText = renderText(paste(aggregateText, " Locations included: "))
      }

      if (length(locationsIntersect) == 0) {
        output$renderWarningText <- renderText("The selected forecasters do not have data for any locations in common.")
        output$renderLocations <- renderText("")
        output$renderAggregateText = renderText("")
        hideElement("truthSection")
        return()
      }
      else {
        locationSubtitleText = paste0(', Location: ', aggregate ,' over all locations common to these forecasters*')
        output$renderLocations <- renderText(toupper(locationsIntersect))
        output$renderWarningText = renderText("")
      }
    # Not totaling over all locations
    } else {
      filteredScoreDf <- filteredScoreDf %>% filter(geo_value == tolower(loc)) %>%
        group_by(Forecaster, Date, ahead) %>%
        summarize(Score = Score, actual = actual)
      locationSubtitleText = paste0(', Location: ', input$location)
      output$renderAggregateText = renderText("")
      output$renderLocations <- renderText("")
      output$renderWarningText <- renderText("")
    }
    
    # Render truth plot with observed values
    showElement("truthSection")
    truthDf = filteredScoreDf
    output$truthPlot <- renderPlotly({
      truthPlot(truthDf, targetVariable, locationsIntersect, allLocations)
    })
    
    filteredScoreDf = filteredScoreDf[c("Forecaster", "Date", "Score", "ahead")]
    filteredScoreDf = filteredScoreDf %>% mutate(across(where(is.numeric), ~ round(., 2)))
    titleText = paste0('<b>',title,'</b>','<br>', '<sup>',
                       'Target Variable: ', targetVariable,
                       locationSubtitleText,
                       '</sup>')
    # Fill gaps so there are line breaks on weeks without data
    filteredScoreDf = filteredScoreDf %>%
      as_tsibble(key = c(Forecaster, ahead), index = Date) %>%
      group_by(Forecaster, ahead) %>%
      fill_gaps(.full = TRUE)
    
    filteredScoreDf$ahead = factor(filteredScoreDf$ahead, levels = c(1, 2, 3, 4), 
                                    labels = c("Horizon: 1 Week", "Horizon: 2 Weeks", "Horizon: 3 Weeks", "Horizon: 4 Weeks"))
    p = ggplot(filteredScoreDf, aes(x = Date, y = Score, color = Forecaster)) +
      geom_line() +
      geom_point() +
      labs(x = "", y = "", title=titleText) +
      scale_x_date(date_labels = "%b %Y") +
      scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
      facet_wrap(~ahead, ncol=1) +
      scale_color_manual(values = color_palette)

    if (scoreType == "coverage") {
      p = p + geom_hline(yintercept = .01 * as.integer(coverageInterval))
    }
    return(ggplotly(p + theme_bw() + theme(panel.spacing=unit(0.5, "lines"))) 
           %>% layout(legend = list(orientation = "h", y = -0.1), margin = list(t=90), height=500, 
                      hovermode = 'x unified', xaxis = list(title = list(text = "Target Date",
                                                                               standoff = 8L), titlefont = list(size = 12))) 
           %>% config(displayModeBar = F))
  }
  
  # Create the plot for target variable ground truth
  truthPlot = function(scoreDf = NULL, targetVariable = NULL, locationsIntersect = NULL, allLocations = FALSE) {
    titleText = paste0('<b> Incident ', targetVariable, '</b>')
    if (allLocations) {
      titleText = paste0('<b>Incident ', targetVariable, '</b>', ' <br><sup>Totaled over all locations common to selected forecasters*</sup>')
    } 
    scoreDf <- scoreDf %>%
      group_by(Date) %>% summarize(Reported_Incidence = actual)
    
    output$renderObservedValueDisclaimer = renderText(observedValueDisclaimer)
    return (ggplotly(ggplot(scoreDf, aes(x = Date, y = Reported_Incidence)) +
      geom_line() +
      geom_point() +
      labs(x = "", y = "", title = titleText) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_labels = "%b %Y") + theme_bw()) 
      %>% layout(hovermode = 'x unified')
      %>% config(displayModeBar = F))
  }

  #############
  # PLOT OUTPUT
  #############
  output$summaryPlot <- renderPlotly({
    summaryPlot(df, input$targetVariable, input$scoreType, input$forecasters, 
                input$aheads, input$location, input$allLocations, input$coverageInterval)
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

  # When the target variable changes, update available forecasters, locations, and CIs to choose from
  observeEvent(input$targetVariable, {
    if (input$targetVariable == 'Deaths') {
      df = df %>% filter(signal == DEATH_FILTER)
    } else {
      df = df %>% filter(signal == CASE_FILTER)
    }
    updateForecasterChoices(session, df, input$forecasters, input$scoreType)
    updateLocationChoices(session, df, input$targetVariable, input$forecasters, input$location)
    updateCoverageChoices(session, df, input$targetVariable, input$forecasters, input$coverageInterval, output)
  })
  
  observeEvent(input$scoreType, {
    if (input$targetVariable == 'Deaths') {
      df = df %>% filter(signal == DEATH_FILTER)
    } else {
      df = df %>% filter(signal == CASE_FILTER)
    }
    # Only show forecasters that have data for the score chosen 
    updateForecasterChoices(session, df, input$forecasters, input$scoreType)
    
    if (input$scoreType == "wis") {
      html("explainScore", paste0(wisExplanation))
    }
    if (input$scoreType == "ae") {
      html("explainScore", paste0(aeExplanation))
    }
    if (input$scoreType == "coverage") {
      html("explainScore", paste0(coverageExplanation))
    }
  })

  # When forecaster selections change, update available aheads, locations, and CIs to choose from
  observeEvent(input$forecasters, {
    if (input$targetVariable == 'Deaths') {
      df = df %>% filter(signal == DEATH_FILTER)
    } else {
      df = df %>% filter(signal == CASE_FILTER)
    }
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
    updateCoverageChoices(session, df, input$targetVariable, input$forecasters, input$coverageInterval, output)
  })
  
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
updateForecasterChoices = function(session, df, forecasterInput, scoreType) {
  if (scoreType == "wis") {
    df = df %>% filter(!is.na(wis))
  }
  if (scoreType == "ae") {
    df = df %>% filter(!is.na(ae))
  }
  forecasterChoices = unique(df$forecaster)
  # Ensure previsouly selected options are still allowed
  if (forecasterInput %in% forecasterChoices) {
    selectedForecasters = forecasterInput
  } else {
    selectedForecasters = c("COVIDhub-ensemble", "COVIDhub-baseline")
  }
  updateSelectInput(session, "forecasters",
                    choices = forecasterChoices,
                    selected = selectedForecasters)
}


updateCoverageChoices = function(session, df, targetVariable, forecasterChoices, coverageInput, output) {
  df = df %>% filter(forecaster %in% forecasterChoices)
  df = Filter(function(x)!all(is.na(x)), df)
  coverageChoices = intersect(colnames(df), COVERAGE_INTERVALS)
  # Ensure previsouly selected options are still allowed
  if (coverageInput %in% coverageChoices) {
    selectedCoverage = coverageInput
  } else {
    selectedCoverage = coverageChoices[1]
  }
  updateSelectInput(session, "coverageInterval",
                    choices = coverageChoices,
                    selected = selectedCoverage)
}


updateLocationChoices = function(session, df, targetVariable, forecasterChoices, locationInput) {
  df = df %>% filter(forecaster %in% forecasterChoices)
  locationChoices = unique(toupper(df$geo_value))
  locationChoices = locationChoices[c(length(locationChoices), (1:length(locationChoices)-1))] # Move US to front of list
  # Ensure previsouly selected options are still allowed
  if (locationInput %in% locationChoices) {
    selectedLocation = locationInput
  } else {
    selectedLocation = locationChoices[1]
  }
  updateSelectInput(session, "location",
                    choices = locationChoices,
                    selected = selectedLocation)
}

shinyApp(ui = ui, server = server)
