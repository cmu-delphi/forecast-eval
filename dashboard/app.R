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
library(covidcast)
library(stringr)

source('./common.R')

# All data is fully loaded from AWS
DATA_LOADED = FALSE

# Earliest 'as of' date available from covidcast API
MIN_AVAIL_NATION_AS_OF_DATE = as.Date('2021-01-09')
MIN_AVAIL_HOSP_AS_OF_DATE  = as.Date('2020-11-11')
MIN_AVAIL_TERRITORY_AS_OF_DATE = as.Date('2021-02-10')

# Score explanations
wisExplanation = includeMarkdown("wis.md")
sharpnessExplanation = includeMarkdown("sharpness.md")
aeExplanation = includeMarkdown("ae.md")
coverageExplanation = includeMarkdown("coverageplot.md")
scoringDisclaimer = includeMarkdown("scoring-disclaimer.md")

# About page content
aboutPageText = includeMarkdown("about.md")

# Get css file
cssFiles = list.files(path="www",pattern="style*")
if(length(cssFiles)!=1){
  cat(file=stderr(),"Error: couldn't load style files\n")
}
cssFile = cssFiles[1]
cat(file=stderr(),"Loaded css file:",cssFile,"\n")

source('./export_scores.R')

########
# Layout
########

ui <- fluidPage(padding=0, title="Forecast Eval Dashboard",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = cssFile)
    ),
    tags$head(includeHTML(("google-analytics.html"))),
    useShinyjs(),
    div(id="header",class="row",
      div(id="logo",class="col-sm-2",
        a(href="https://delphi.cmu.edu",
          img(src="cmu_brand.png",width="220px",heigh="50px",alt="Carnegie Mellon University Delphi Group")
        )
      ),
      div(id="title", class="col-sm-6",
        HTML("FORECAST <span id='bold-title'>EVALUATION DASHBOARD</span> <a id='back-button' href='https://delphi.cmu.edu'>",
             includeHTML("arrow-left.svg"), "   Back</a>"),
      ),
      div(id="github-logo-container", class="col-sm-1",
        a(id="github-logo",href="https://github.com/cmu-delphi/forecast-eval/",
            includeHTML("github.svg"),
            HTML("&nbsp;GitHub")
          )
      ),
    ),
    tags$br(),
    sidebarLayout(
      sidebarPanel(id = "inputOptions",
        conditionalPanel(condition = "input.tabset == 'evaluations'",
            radioButtons("targetVariable", "Target Variable",
                                      choices = list("Incident Deaths" = "Deaths", 
                                                     "Incident Cases" = "Cases",
                                                     "Hospital Admissions" = "Hospitalizations")),
            radioButtons("scoreType", "Scoring Metric",
                                      choices = list("Weighted Interval Score" = "wis",
                                                     "Spread" = "sharpness",
                                                     "Absolute Error" = "ae",
                                                     "Coverage" = "coverage")),
            conditionalPanel(condition = "input.scoreType != 'coverage'",
                             tags$p(id="scale-score", "Y-Axis Score Scale"),
                             checkboxInput(
                               "logScale",
                               "Log Scale",
                               value = FALSE,
                             )),
            conditionalPanel(condition = "input.scoreType != 'coverage'",
                             checkboxInput(
                               "scaleByBaseline",
                               "Scale by Baseline Forecaster",
                               value = FALSE,
                             )),
            selectInput(
              "forecasters",
              p("Forecasters", tags$br(), tags$span(id="forecaster-input", "Type a name or select from dropdown")),
              choices = c("COVIDhub-baseline", "COVIDhub-ensemble"),
              multiple = TRUE,
              selected = c("COVIDhub-baseline", "COVIDhub-ensemble")
            ),
            tags$p(id="missing-data-disclaimer", "Some forecasters may not have data for the chosen location or scoring metric"),
            checkboxGroupInput(
              "aheads",
              "Forecast Horizon (Weeks)",
              choices = AHEAD_OPTIONS,
              selected = AHEAD_OPTIONS[1],
              inline = TRUE
            ),
            hidden(tags$p(id="horizon-disclaimer", "Forecasters submitted earlier than Mondays may have longer actual prediction horizons")),
            conditionalPanel(condition = "input.scoreType == 'coverage'",
                             selectInput(
                               "coverageInterval",
                               "Coverage Interval",
                               choices = '',
                               multiple = FALSE,
                               selected = "95"
                             ),
            ),
            conditionalPanel(condition = "input.scoreType != 'coverage'",
                             selectInput(
                               "location",
                               "Location",
                               choices = '',
                               multiple = FALSE,
                               selected = "US"
                             )
            ),
            selectInput(
              "asOf",
              "As Of",
              choices = '',
              multiple = FALSE,
              selected = ''
            ),
            tags$p(id="missing-data-disclaimer", "Some locations may not have 'as of' data for the chosen 'as of' date"),
            hidden(div(id="showForecastsCheckbox",
                             checkboxInput(
                               "showForecasts",
                               "Show Forecasters' Predictions",
                               value = FALSE,
                             )
            )),
            tags$hr(),
            export_scores_ui,
            tags$hr(),
        ),
        includeMarkdown("about-dashboard.md"),
        width=3,
      ),

      mainPanel(
        width=9,
        tabsetPanel(id = "tabset",
          selected = "evaluations",
          tabPanel("About",
            fluidRow(column(10,
              div(
                id="aboutContentArea",
                aboutPageText,
                tags$br(),
                h3("Explanation of Scoring Methods"),
                h4("Weighted Interval Score"),
                wisExplanation,
                h4("Spread"),
                sharpnessExplanation,
                h4("Absolute Error"),
                aeExplanation,
                h4("Coverage"),
                coverageExplanation
              ),
              tags$br()
            )),
          ),
          tabPanel("Evaluation Plots", value = "evaluations",
            fluidRow(column(11, textOutput('renderWarningText'))),
            plotlyOutput(outputId = "summaryPlot", height="auto"),
            fluidRow(
              column(11, offset=1, 
                     hidden(div(id="refresh-colors", actionButton(inputId="refreshColors", label= "Recolor")))
            )),
            tags$br(),
            plotlyOutput(outputId = "truthPlot", height="auto"),
            fluidRow(
              column(11, offset=1,
                div(id="data-loading-message", "DATA IS LOADING...(this may take a while)"),
                     hidden(div(id="truth-plot-loading-message", "Fetching 'as of' data and loading observed values...")),
                     hidden(div(id="notes", "About the Scores")),
                     hidden(div(id="scoreExplanations",
                       hidden(div(id = "wisExplanation", wisExplanation)),
                       hidden(div(id = "sharpnessExplanation", sharpnessExplanation)),
                       hidden(div(id = "aeExplanation", aeExplanation)),
                       hidden(div(id = "coverageExplanation", coverageExplanation))
                     )),
                     hidden(div(id = "scoringDisclaimer", scoringDisclaimer))
              )
            ),
            fluidRow(
              column(11,offset=1,
                textOutput('renderLocationText'),
                textOutput('renderAggregateText'),
                textOutput('renderLocations'),
                tags$br()
              )
            )
          )
        ),
      ),
    ),
)


# Get and prepare data
getS3Bucket <- function() {
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
  
  return(s3bucket)
}

getData <- function(filename, s3bucket){
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

getAllData = function(s3bucket) {
  dfStateCases <- getData("score_cards_state_cases.rds", s3bucket)
  dfStateDeaths <- getData("score_cards_state_deaths.rds", s3bucket)
  dfNationCases = getData("score_cards_nation_cases.rds", s3bucket)
  dfNationDeaths = getData("score_cards_nation_deaths.rds", s3bucket)
  dfStateHospitalizations = getData("score_cards_state_hospitalizations.rds", s3bucket)
  dfNationHospitalizations = getData("score_cards_nation_hospitalizations.rds", s3bucket)
  
  # Pick out expected columns only
  covCols = paste0("cov_", COVERAGE_INTERVALS)
  expectedCols = c("ahead", "geo_value", "forecaster", "forecast_date",
                   "data_source", "signal", "target_end_date", "incidence_period",
                   "actual", "wis", "sharpness", "ae", "value_50",
                   covCols)
  
  dfStateCases = dfStateCases %>% select(all_of(expectedCols))
  dfStateDeaths = dfStateDeaths %>% select(all_of(expectedCols))
  dfNationCases = dfNationCases %>% select(all_of(expectedCols))
  dfNationDeaths = dfNationDeaths %>% select(all_of(expectedCols))
  dfStateHospitalizations = dfStateHospitalizations %>% select(all_of(expectedCols))
  dfNationHospitalizations = dfNationHospitalizations %>% select(all_of(expectedCols))
  
  df = rbind(dfStateCases, dfStateDeaths, dfNationCases, dfNationDeaths, dfStateHospitalizations, dfNationHospitalizations)
  df = df %>% rename("10" = cov_10, "20" = cov_20, "30" = cov_30, "40" = cov_40, "50" = cov_50, "60" = cov_60, "70" = cov_70, "80" = cov_80, "90" = cov_90, "95" = cov_95, "98" = cov_98)
  
  return(df)
}

getRecentDataHelper = function() {
  s3bucket <- getS3Bucket()
  df <- data.frame()
  
  getRecentData = function() {
    newS3bucket <- getS3Bucket()
    
    s3Contents <- s3bucket[attr(s3bucket, "names", exact=TRUE)]
    newS3Contents <- newS3bucket[attr(newS3bucket, "names", exact=TRUE)]
    
    # Fetch new score data if contents of S3 bucket has changed (including file
    # names, sizes, and last modified timestamps). Ignores characteristics of
    # bucket and request, including bucket region, name, content type, request
    # date, request ID, etc.
    if ( nrow(df) == 0 || !identical(s3Contents, newS3Contents) ) {
      # Save new data and new bucket connection info to vars in env of
      # `getRecentDataHelper`. They persist between calls to `getRecentData` a
      # la https://stackoverflow.com/questions/1088639/static-variables-in-r
      s3bucket <<- newS3bucket
      df <<- getAllData(s3bucket)
    }
    
    return(df)
  }
  
  return(getRecentData)
}

getRecentData <- getRecentDataHelper()


server <- function(input, output, session) {
  TERRITORIES = c('AS', 'GU', 'MP', 'VI')
  PREV_AS_OF_DATA = reactiveVal(NULL)
  AS_OF_CHOICES = reactiveVal(NULL)
  SUMMARIZING_OVER_ALL_LOCATIONS = reactive(input$scoreType == 'coverage' || input$location == TOTAL_LOCATIONS)

  # Get most recent target end date
  # Prev Saturday for Cases and Deaths, prev Wednesday for Hospitalizations
  # Since we don't upload new observed data until Monday:
  # Use 8 and 2 for Cases and Deaths so that Sundays will not use the Saturday directly beforehand
  # since we don't have data for it yet.
  # Use 5 and 11 for Hospitalizations since Thurs-Sun should also not use the Wednesday directly beforehand.
  # (This means that on Mondays until the afternoon when pipeline completes, the "as of" will show
  # most recent Saturday / Wednesday date even though the actual updated data won't be there yet)
  prevWeek <- seq(Sys.Date()-8,Sys.Date()-2,by='day')
  CASES_DEATHS_CURRENT = prevWeek[weekdays(prevWeek)=='Saturday']
  CURRENT_WEEK_END_DATE = reactiveVal(CASES_DEATHS_CURRENT)
  prevHospWeek <- seq(Sys.Date()-11,Sys.Date()-5,by='day')
  HOSP_CURRENT = prevHospWeek[weekdays(prevHospWeek)=='Wednesday']
  
  # Get scores
  df = getRecentData()
  DATA_LOADED = TRUE

  # Prepare input choices
  forecasterChoices = sort(unique(df$forecaster))
  updateForecasterChoices(session, df, forecasterChoices, 'wis')

  
  ##################
  # CREATE MAIN PLOT
  ##################
  summaryPlot = function(colorSeed = 100, reRenderTruth = FALSE, asOfData = NULL) {
    filteredScoreDf = filterScoreDf()
    dfWithForecasts = NULL
    if (input$showForecasts) {
      dfWithForecasts = filteredScoreDf
    }
    # Need to do this after setting dfWithForecasts to leave in aheads for forecasts
    filteredScoreDf = filteredScoreDf %>% filter(ahead %in% input$aheads)
    if (dim(filteredScoreDf)[1] == 0) {
      output$renderWarningText <- renderText("The selected forecasters do not have enough data to display the selected scoring metric.")
      return()
    }
    if (is.null(asOfData)) {
      if (!is.null(isolate(PREV_AS_OF_DATA())) && dim(isolate(PREV_AS_OF_DATA()))[1] != 0 &&
          isolate(input$asOf) != '' && isolate(input$asOf) != isolate(CURRENT_WEEK_END_DATE())) {
         asOfData = isolate(PREV_AS_OF_DATA())
      }
    }
    if (!is.null(asOfData) && dim(asOfData)[1] != 0) {
      asOfData = asOfData %>% rename(target_end_date = time_value, as_of_actual = value)
      asOfData = asOfData[c("target_end_date", "geo_value", "as_of_actual")]

      # Get the 'as of' dates that are the target_end_dates in the scoring df
      dateGroupDf = asOfData %>% filter(asOfData$target_end_date %in% filteredScoreDf$target_end_date)
      if (dim(dateGroupDf)[1] != 0) {
        # Since cases and deaths are shown as weekly incidence, but the "as of" data from the covidcast API
        # is daily, we need to sum over the days leading up to the target_end_date of each week to get the
        # weekly incidence
        asOfData = filterAsOfData(asOfData, dateGroupDf, filteredScoreDf)
        filteredScoreDf = merge(filteredScoreDf, asOfData, by=c("target_end_date", "geo_value"), all = TRUE)
      } else {
        # Input 'as of' date chosen does not match the available target_end_dates that result from the rest of the selected inputs
        # It is too far back or we are switching between hosp and cases/deaths which have different target date days
        # As of input will be updated to the default (latest) and plot will re-render with the just the normal truth data, no 'as of'
        asOfData = NULL
      }
    }

    # Totaling over all locations
    if (SUMMARIZING_OVER_ALL_LOCATIONS()) {
      filteredScoreDfAndIntersections = filterOverAllLocations(filteredScoreDf, input$scoreType, !is.null(asOfData))
      filteredScoreDf = filteredScoreDfAndIntersections[[1]]
      locationsIntersect = filteredScoreDfAndIntersections[[2]]
      if (input$showForecasts) {
        dfWithForecasts = dfWithForecasts %>% filter(geo_value %in% locationsIntersect)
      }
      aggregateText = "*For fair comparison, all displayed forecasters on all displayed dates are compared across a common set of states and territories."
      if (input$scoreType == "coverage") {
        aggregate = "Averaged"
        output$renderAggregateText = renderText(paste(aggregateText," Some forecasters may not have any data for the coverage interval chosen. Locations inlcuded: "))
      }
      else {
        aggregate = "Totaled"
        output$renderAggregateText = renderText(paste(aggregateText, " Locations included: "))
      }
      if (length(locationsIntersect) == 0) {
        output$renderWarningText <- renderText("The selected forecasters do not have data for any locations in common on all dates.")
        output$renderLocations <- renderText("")
        output$renderAggregateText = renderText("")
        hideElement("truthPlot")
        hideElement("refresh-colors")
        return()
      }
      else {
        locationSubtitleText = paste0(', Location: ', aggregate ,' over all states and territories common to these forecasters*')
        output$renderLocations <- renderText(toupper(locationsIntersect))
        output$renderWarningText = renderText("")
        showElement("truthPlot")
      }
    # Not totaling over all locations
    } else {
      if (!is.null(asOfData)) {
        filteredScoreDf <- filteredScoreDf %>% filter(geo_value == tolower(input$location)) %>%
          group_by(forecaster, forecast_date, target_end_date, ahead) %>%
          summarize(Score = Score, actual = actual, as_of_actual = as_of_actual)
      } else {
        filteredScoreDf <- filteredScoreDf %>% filter(geo_value == tolower(input$location)) %>%
          group_by(forecaster, forecast_date, target_end_date, ahead) %>%
          summarize(Score = Score, actual = actual)
      }
      locationSubtitleText = paste0(', Location: ', input$location)
      output$renderAggregateText = renderText("")
      output$renderLocations <- renderText("")
      output$renderWarningText <- renderText("")
    }

    showElement("refresh-colors")
    if(dim(filteredScoreDf)[1] == 0) {
      return()
    }

    # Rename columns that will be used as labels and for clarity on CSV exports
    filteredScoreDf = filteredScoreDf %>% rename(Forecaster = forecaster, Forecast_Date = forecast_date,
                                                 Week_End_Date = target_end_date)

    # Set forecaster colors for plot
    set.seed(colorSeed)
    forecasterRand <- sample(unique(df$forecaster))
    colorPalette = setNames(object = viridis(length(unique(df$forecaster))), nm = forecasterRand)
    if (!is.null(asOfData)) {
      colorPalette['Reported_Incidence'] = 'grey'
      colorPalette['Reported_As_Of_Incidence'] = 'black'
    }

    # Render truth plot with observed values
    truthDf = filteredScoreDf
    output$truthPlot <- renderPlotly({
      truthPlot(truthDf, locationsIntersect, !is.null(asOfData), dfWithForecasts, colorPalette)
    })

    # If we are just re-rendering the truth plot with as of data
    # we don't need to re-render the score plot
    if (reRenderTruth) {
      return()
    }
    # If we are re-rendering scoring plot with new inputs that were just selected
    # we need to make sure the as of input options are valid with those inputs
    updateAsOfChoices(session, truthDf)

    # Format and transform data for plot
    filteredScoreDf = filteredScoreDf %>% filter(!is.na(Week_End_Date))
    filteredScoreDf = filteredScoreDf[c("Forecaster", "Forecast_Date", "Week_End_Date", "Score", "ahead")]
    filteredScoreDf = filteredScoreDf %>% mutate(across(where(is.numeric), ~ round(., 2)))
    if (input$scoreType != 'coverage') {
      if (input$scaleByBaseline) {
        baselineDf = filteredScoreDf %>% filter(Forecaster %in% 'COVIDhub-baseline')
        filteredScoreDfMerged = merge(filteredScoreDf, baselineDf, by=c("Week_End_Date","ahead"))
        # Scaling score by baseline forecaster
        filteredScoreDfMerged$Score.x = filteredScoreDfMerged$Score.x / filteredScoreDfMerged$Score.y
        filteredScoreDf = filteredScoreDfMerged %>%
          rename(Forecaster = Forecaster.x, Score = Score.x, Forecast_Date = Forecast_Date.x) %>%
          select(Forecaster, Forecast_Date, Week_End_Date, ahead, Score)
      }
      if (input$logScale) {
        filteredScoreDf$Score = log10(filteredScoreDf$Score)
      }
    }

    # Title plot
    if (input$scoreType == "wis") {
      plotTitle = "Weighted Interval Score"
    }
    else if (input$scoreType == "sharpness") {
      plotTitle = "Spread"
    }
    else if (input$scoreType == "ae") {
      plotTitle = "Absolute Error"
    }
    else {
      plotTitle = "Coverage"
    }

    titleText = paste0('<b>', plotTitle,'</b>','<br>', '<sup>',
                       'Target Variable: ', input$targetVariable,
                       locationSubtitleText, '<br>',
                       tags$span(id="drag-to-zoom", " Drag to zoom"),
                       '</sup>')

    # Fill gaps so there are line breaks on weeks without data
    # This is failing for CU-select on US deaths (https://github.com/cmu-delphi/forecast-eval/issues/157)
    filteredScoreDf = filteredScoreDf %>%
      as_tsibble(key = c(Forecaster, ahead), index = Week_End_Date) %>%
      group_by(Forecaster, Forecast_Date, ahead) %>%
      fill_gaps(.full = TRUE)
    # Set labels for faceted horizon plots
    horizonOptions = AHEAD_OPTIONS
    horizonLabels = lapply(AHEAD_OPTIONS, function (x) paste0("Horizon: ", x, " Week(s)"))
    if (input$targetVariable == 'Hospitalizations') {
      horizonOptions = HOSPITALIZATIONS_AHEAD_OPTIONS
      horizonLabels = lapply(HOSPITALIZATIONS_AHEAD_OPTIONS, function (x) paste0("Horizon: ", x, " Days"))
    }
    filteredScoreDf$ahead = factor(filteredScoreDf$ahead, levels = horizonOptions,
                                   labels = horizonLabels)

    p = ggplot(
        filteredScoreDf, 
        aes(x = Week_End_Date, y = Score, color = Forecaster, shape = Forecaster, label = Forecast_Date)
      ) +
      geom_line() +
      geom_point(size=2) +
      labs(x = "", y = "", title=titleText) +
      scale_x_date(date_labels = "%b %Y") +
      facet_wrap(~ahead, ncol=1) +
      scale_color_manual(values = colorPalette) +
      theme_bw() +
      theme(panel.spacing=unit(0.5, "lines")) +
      theme(legend.title = element_blank())

    if (input$scoreType == "coverage") {
      p = p + geom_hline(yintercept = .01 * as.integer(input$coverageInterval))
    }
    if (input$logScale) {
      p = p + scale_y_continuous(label = function(x) paste0("10^", x))
    } else {
      p = p + scale_y_continuous(limits = c(0,NA), labels = scales::comma)
    }
    plotHeight = 550 + (length(input$aheads)-1)*100
    finalPlot <- 
      ggplotly(p, tooltip = c("x", "y", "shape", "label")) %>%
      layout(
        height = plotHeight,
        legend = list(orientation = "h", y = -0.1),
        margin = list(t=90),
        height=500,
        hovermode = 'x unified',
        xaxis = list(
          title = list(text = "Target Date",standoff = 8L),
          titlefont = list(size = 12))
      ) %>%
      config(displayModeBar = F)

    return(finalPlot)
  }

  ###################
  # CREATE TRUTH PLOT
  ###################
  # Create the plot for target variable ground truth
  truthPlot = function(filteredDf = NULL, locationsIntersect = NULL, hasAsOfData = FALSE, dfWithForecasts = NULL, colorPalette = NULL) {
    observation = paste0('Incident ', input$targetVariable)
    if (input$targetVariable == "Hospitalizations") {
      observation = paste0('Hospital Admissions')
    }
    titleText = paste0('<b>Observed ', observation, '</b>')
    if (SUMMARIZING_OVER_ALL_LOCATIONS()) {
      titleText = paste0('<b>Observed ', observation, '</b>', ' <br><sup>Totaled over all states and territories common to selected forecasters*</sup>')
    }

    if (hasAsOfData) {
      filteredDf = filteredDf %>%
        group_by(Week_End_Date) %>% summarize(Forecaster = Forecaster, Reported_Incidence = actual, Reported_As_Of_Incidence = as_of_actual) %>%
        distinct()
      if(input$showForecasts) {
        filteredDf = filterForecastData(filteredDf, dfWithForecasts)
      }
    } else {
      filteredDf <- filteredDf %>%
        group_by(Week_End_Date) %>% summarize(Reported_Incidence = actual)
    }

    finalPlot = ggplot(filteredDf, aes(x = Week_End_Date)) +
      labs(x = "", y = "", title = titleText) +
      scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
      scale_x_date(date_labels = "%b %Y") +
      scale_color_manual(values = colorPalette) +
      theme_bw() +
      theme(legend.title = element_blank())

    if (hasAsOfData) {
      finalPlot = finalPlot +
        geom_line(aes(y = Reported_Incidence, color = "Reported_Incidence")) +
        geom_point(aes(y = Reported_Incidence, color = "Reported_Incidence")) +
        geom_line(aes(y = Reported_As_Of_Incidence, color = "Reported_As_Of_Incidence")) +
        geom_point(aes(y = Reported_As_Of_Incidence, color = "Reported_As_Of_Incidence"))
      if(input$showForecasts) {
        finalPlot = finalPlot +
          geom_line(aes(y = Quantile_50, color = Forecaster, shape = Forecaster)) +
          geom_point(aes(y = Quantile_50, color = Forecaster, shape = Forecaster))
      }
    } else {
      finalPlot = finalPlot + geom_line(aes(y = Reported_Incidence)) +
        geom_point(aes(y = Reported_Incidence))
    }
    finalPlot = ggplotly(finalPlot, tooltip = c("shape","x", "y")) %>%
      layout(hovermode = 'x unified', legend = list(orientation = "h", y = -0.1)) %>%
      config(displayModeBar = F)
    # Remove the extra grouping from the legend: "(___,1)"
    for (i in 1:length(finalPlot$x$data)){
      if (!is.null(finalPlot$x$data[[i]]$name)){
        finalPlot$x$data[[i]]$name =  gsub("\\(","",str_split(finalPlot$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return (finalPlot)
  }

  #############
  # PLOT OUTPUT
  #############
  output$summaryPlot <- renderPlotly({
    summaryPlot()
  })

  # Filter scoring df by inputs chosen (targetVariable, forecasters, aheads)
  filterScoreDf = function() {
    signalFilter = CASE_FILTER
    if (input$targetVariable == "Deaths") {
      signalFilter = DEATH_FILTER
    }
    if (input$targetVariable == "Hospitalizations") {
      signalFilter = HOSPITALIZATIONS_FILTER
    }
    filteredScoreDf = df %>%
      filter(signal == signalFilter) %>%
      filter(forecaster %in% input$forecasters)

    if (signalFilter == HOSPITALIZATIONS_FILTER) {
      filteredScoreDf = filterHospitalizationsAheads(filteredScoreDf)
    }
    if (input$scoreType == "wis" || input$scoreType == "sharpness") {
      # Only show WIS or Sharpness for forecasts that have all intervals
      filteredScoreDf = filteredScoreDf %>% filter(!is.na(`50`)) %>% filter(!is.na(`80`)) %>% filter(!is.na(`95`))
      if (input$targetVariable == "Deaths") {
        filteredScoreDf = filteredScoreDf %>% filter(!is.na(`10`)) %>% filter(!is.na(`20`)) %>% filter(!is.na(`30`)) %>%
          filter(!is.na(`40`)) %>% filter(!is.na(`60`)) %>% filter(!is.na(`70`)) %>% filter(!is.na(`90`)) %>% filter(!is.na(`98`))
      }
    }
    filteredScoreDf = renameScoreCol(filteredScoreDf, input$scoreType, input$coverageInterval)
    return(filteredScoreDf)
  }

  # Filter as of data so that it matches weekly incidence for the target end dates in the score df
  filterAsOfData = function(asOfData, dateGroupDf, filteredScoreDf) {
    # Hospitalization scores are shown as daily incidence, not weekly incidence, no summing necessary
    if (input$targetVariable != "Hospitalizations") {
      # Create a df to fill in the corresponding target_end_date in a new date_group column for all intervening days
      dateGroupDf[,"date_group"] <- NA
      dateGroupDf$date_group = dateGroupDf$target_end_date
      asOfData = merge(asOfData, dateGroupDf, by=c('target_end_date', 'geo_value', 'as_of_actual'), all = TRUE)

      # Cut off the extra days on beginning and end of series so that when we sum the values we are only
      # summing over the weeks included in the score plot
      asOfData = asOfData %>% filter(target_end_date >= min(filteredScoreDf$target_end_date) - 6)
      asOfData = asOfData %>% filter(target_end_date <= isolate(input$asOf))

      # Fill in the date_group column with the target week end days for all intervening days
      asOfData = asOfData %>% arrange(geo_value) %>% fill(date_group, .direction = "up")

      # In the case where there are target week end days missing from the scoring or as of data
      # we don't want to end up summing values over multiple weeks so we make sure each date_group only spans one week
      asOfData = asOfData %>% filter(asOfData$date_group - asOfData$target_end_date < 7)

      asOfData = asOfData[c('geo_value', 'as_of_actual', 'date_group')]
      # Sum over preceding week for all weekly target variables
      asOfData = asOfData %>% group_by(geo_value, date_group) %>% summarize(as_of_actual = sum(as_of_actual))
      asOfData = asOfData %>% rename(target_end_date = date_group)
    # If targetVariable is Hospitalizations
    } else {
      asOfData = dateGroupDf
      # Need to make sure that we are only matching the target_end_dates shown in the scoring plot
      # and not using fetched data for as of dates before those target_end_dates.
      # This is taken care of above for cases and deaths.
      minDate = min(filteredScoreDf$target_end_date)
      if (!SUMMARIZING_OVER_ALL_LOCATIONS()) {
        chosenLocationDf = filteredScoreDf %>% filter(geo_value == tolower(input$location))
        minDate = min(chosenLocationDf$target_end_date)
      }
      asOfData = asOfData %>% filter(target_end_date >= minDate)
    }
    return(asOfData)
  }

  filterForecastData = function(filteredDf, dfWithForecasts) {
    dfWithForecasts = dfWithForecasts %>%
      rename(Week_End_Date = target_end_date, Forecaster = forecaster, Quantile_50 = value_50)
    if (!SUMMARIZING_OVER_ALL_LOCATIONS()) {
      dfWithForecasts  = dfWithForecasts %>% filter(geo_value == tolower(input$location))
    } else {
      # Sum the predictions for all included locations
      dfWithForecasts = dfWithForecasts %>%
        group_by(Forecaster, forecast_date, Week_End_Date, ahead) %>%
        summarize(Quantile_50 = sum(Quantile_50))
    }
    dfWithForecasts  = dfWithForecasts %>%
      # We want the forecasts to be later than latest as of date with data
      filter(forecast_date >= tail(filteredDf %>% filter(!is.na(Reported_As_Of_Incidence)), n=1)$Week_End_Date[1]) %>%
      group_by(Week_End_Date) %>%
      summarize(Forecaster, forecast_date, Quantile_50)

    # Get the next as of choice available in dropdown menu
    dfWithForecasts = dfWithForecasts[order(dfWithForecasts$forecast_date),]
    AS_OF_CHOICES(sort(AS_OF_CHOICES() %>% unique()))
    nextAsOfInList = AS_OF_CHOICES()[which.min(abs(AS_OF_CHOICES()-dfWithForecasts$forecast_date[1])) + 1]

    # Take only those forecasts with a forecast date before the next as of date in dropdown
    # aka within the week after the current as of shown
    if(length(nextAsOfInList) != 0 && !is.na(nextAsOfInList)) {
      dfWithForecasts = dfWithForecasts %>%
        filter(forecast_date < nextAsOfInList)
    }

    # Hospitalizations will have multiple forecast dates within this target week
    # So we want to take the earliest forecast date for each forecaster & week end date pair
    if (input$targetVariable == "Hospitalizations") {
      dfWithForecasts = dfWithForecasts %>% group_by(Week_End_Date, Forecaster) %>% top_n(n=1, wt=desc(forecast_date))
      dfWithForecasts = dfWithForecasts %>% group_by(Forecaster) %>% filter(forecast_date == first(forecast_date))
    }
    filteredDf = merge(filteredDf, dfWithForecasts, by=c('Week_End_Date', 'Forecaster'), all = TRUE) %>%
      group_by(Week_End_Date) %>%
      select(Quantile_50, Forecaster, Reported_Incidence, Reported_As_Of_Incidence)
    # Remove rows of NAs
    filteredDf = filteredDf %>% filter(!is.null(Forecaster))
    filteredDf = filteredDf %>% arrange(Week_End_Date) %>% fill(Reported_Incidence, .direction = "downup")
    return (filteredDf)
  }

  ###################
  # EVENT OBSERVATION
  ###################

  observeEvent(input$refreshColors, {
    colorSeed = floor(runif(1, 1, 1000))
    output$summaryPlot <- renderPlotly({
      summaryPlot(colorSeed)
    })
  })

  # When the target variable changes, update available forecasters, locations, and CIs to choose from
  observeEvent(input$targetVariable, {
    CURRENT_WEEK_END_DATE(CASES_DEATHS_CURRENT)
    if (input$targetVariable == 'Deaths') {
      df = df %>% filter(signal == DEATH_FILTER)
    } else if (input$targetVariable == 'Cases') {
      df = df %>% filter(signal == CASE_FILTER)
    } else {
      df = df %>% filter(signal == HOSPITALIZATIONS_FILTER)
      CURRENT_WEEK_END_DATE(HOSP_CURRENT)
    }

    updateAheadChoices(session, df, input$targetVariable, input$forecasters, input$aheads, TRUE)
    updateForecasterChoices(session, df, input$forecasters, input$scoreType)
    updateLocationChoices(session, df, input$targetVariable, input$forecasters, input$location)
    updateCoverageChoices(session, df, input$targetVariable, input$forecasters, input$coverageInterval, output)
    updateAsOfData()
 })

  observeEvent(input$scoreType, {
    if (input$targetVariable == 'Deaths') {
      df = df %>% filter(signal == DEATH_FILTER)
    } else if (input$targetVariable == 'Cases') {
      df = df %>% filter(signal == CASE_FILTER)
    } else {
      df = df %>% filter(signal == HOSPITALIZATIONS_FILTER)
    }
    # Only show forecasters that have data for the score chosen
    updateForecasterChoices(session, df, input$forecasters, input$scoreType)

    # If we are switching between coverage and other score types we need to
    # update the as of data we have so it matches the correct locations shown
    if (input$location == 'US') {
      updateAsOfData()
    }

    if (input$asOf != '' && input$asOf == CURRENT_WEEK_END_DATE()) {
      hideElement("showForecastsCheckbox")
    } else {
      showElement("showForecastsCheckbox")
    }
    if (input$scoreType == "wis") {
      show("wisExplanation")
      hide("sharpnessExplanation")
      hide("aeExplanation")
      hide("coverageExplanation")
    }
    if (input$scoreType == "sharpness") {
      show("sharpnessExplanation")
      hide("wisExplanation")
      hide("aeExplanation")
      hide("coverageExplanation")
    }
    if (input$scoreType == "ae") {
      hide("wisExplanation")
      hide("sharpnessExplanation")
      show("aeExplanation")
      hide("coverageExplanation")
    }
    if (input$scoreType == "coverage") {
      hide("wisExplanation")
      hide("sharpnessExplanation")
      hide("aeExplanation")
      show("coverageExplanation")
    }
  })

  # When forecaster selections change, update available aheads, locations, and CIs to choose from
  observeEvent(input$forecasters, {
    if (input$targetVariable == 'Deaths') {
      df = df %>% filter(signal == DEATH_FILTER)
    } else if (input$targetVariable == 'Cases') {
      df = df %>% filter(signal == CASE_FILTER)
    } else {
      df = df %>% filter(signal == HOSPITALIZATIONS_FILTER)
    }
    df = df %>% filter(forecaster %in% input$forecasters)

    updateAheadChoices(session, df, input$targetVariable, input$forecasters, input$aheads, FALSE)
    updateLocationChoices(session, df, input$targetVariable, input$forecasters, input$location)
    updateCoverageChoices(session, df, input$targetVariable, input$forecasters, input$coverageInterval, output)
  })

  observeEvent(input$location, {
    updateAsOfData()
    # Only show forecast check box option if we are showing as of data
    if (input$asOf != '' && input$asOf == CURRENT_WEEK_END_DATE()) {
      hideElement("showForecastsCheckbox")
    } else {
      showElement("showForecastsCheckbox")
    }
  })

  observeEvent(input$asOf, {
    updateAsOfData()
    # Only show forecast check box option if we are showing as of data
    if (input$asOf != '' && input$asOf == CURRENT_WEEK_END_DATE()) {
      hideElement("showForecastsCheckbox")
    } else {
      showElement("showForecastsCheckbox")
    }
  })

  # The following checks ensure the minimum necessary input selections
  observe({
    # Show data loading message and hide other messages until all data is loaded
    if (DATA_LOADED) {
      hide("data-loading-message")
      show("refresh-colors")
      show("notes")
      show("scoreExplanations")
      show("scoringDisclaimer")
    }
    # Ensure there is always one ahead selected
    if(length(input$aheads) < 1) {
      if (input$targetVariable == 'Hospitalizations') {
        updateCheckboxGroupInput(session, "aheads",
                                 selected = HOSPITALIZATIONS_AHEAD_OPTIONS[1])
      } else {
        updateCheckboxGroupInput(session, "aheads",
                                 selected = AHEAD_OPTIONS[1])
      }
    }
    # Ensure there is always one forecaster selected
    if(length(input$forecasters) < 1) {
      updateSelectInput(session, "forecasters",
                        selected = c("COVIDhub-ensemble")) # Use ensemble rather than baseline bc it has hospitalization scores
    }
    # Ensure COVIDhub-baseline is selected when scaling by baseline
    if(input$scaleByBaseline && !("COVIDhub-baseline" %in% input$forecasters)) {
      updateSelectInput(session, "forecasters", selected = c(input$forecasters, "COVIDhub-baseline"))
    }
  })

  updateAsOfData = function() {
    dataSource = "jhu-csse"
    if(input$targetVariable == "Cases") {
      targetSignal = "confirmed_incidence_num"
    } else if (input$targetVariable == "Deaths") {
      targetSignal = "deaths_incidence_num"
    } else if (input$targetVariable == "Hospitalizations") {
      targetSignal = "confirmed_admissions_covid_1d"
      dataSource = "hhs"
    }

    if (input$location == 'US' && input$scoreType != 'coverage') {
      location = "nation"
    } else {
      location = "state"
    }
    if (input$asOf < CURRENT_WEEK_END_DATE() && input$asOf != '') {
      hideElement("truthPlot")
      hideElement("notes")
      hideElement("scoringDisclaimer")
      hideElement("scoreExplanations")
      hideElement("renderAggregateText")
      hideElement("renderLocations")
      showElement("truth-plot-loading-message")

      # Since as_of matches to the issue date in covidcast (rather than the time_value)
      # we need to add one extra day to get the as of we want.
      fetchDate = as.Date(input$asOf) + 1

      # Covidcast API call
      asOfTruthData = covidcast_signal(data_source = dataSource, signal = targetSignal,
                                       start_day = "2020-02-15", end_day = fetchDate,
                                       as_of = fetchDate,
                                       geo_type = location)
      showElement("truthPlot")
      showElement("notes")
      showElement("scoringDisclaimer")
      showElement("scoreExplanations")
      showElement("renderAggregateText")
      showElement("renderLocations")
      hideElement("truth-plot-loading-message")
      PREV_AS_OF_DATA(asOfTruthData)

      if(dim(asOfTruthData)[1] == 0) {
        return()
      }
      summaryPlot(reRenderTruth = TRUE, asOfData = asOfTruthData)
    } else if(input$asOf == CURRENT_WEEK_END_DATE() && input$asOf != '') {
      summaryPlot(reRenderTruth = TRUE)
    }
  }

  updateAsOfChoices = function(session, truthDf) {
    asOfChoices = truthDf$Week_End_Date
    selectedAsOf = isolate(input$asOf)
    if (input$targetVariable == "Hospitalizations") {
      minChoice = MIN_AVAIL_HOSP_AS_OF_DATE
      asOfChoices = asOfChoices[asOfChoices >= minChoice]
    } else if(input$location == 'US' && input$scoreType != 'coverage') {
      minChoice = MIN_AVAIL_NATION_AS_OF_DATE
      asOfChoices = asOfChoices[asOfChoices >= minChoice]
    } else if(input$location %in% TERRITORIES || input$location == TOTAL_LOCATIONS || input$scoreType == 'coverage') {
      minChoice = MIN_AVAIL_TERRITORY_AS_OF_DATE
      asOfChoices = asOfChoices[asOfChoices >= minChoice]
    }
    asOfChoices = c(asOfChoices, CURRENT_WEEK_END_DATE())
    # Make sure we have a valid as of selection
    nonValidAsOf = selectedAsOf == '' || !(as.Date(selectedAsOf) %in% asOfChoices)
    if (length(asOfChoices) != 0 && nonValidAsOf) {
      selectedAsOf = max(asOfChoices, na.rm=TRUE)
    }
    AS_OF_CHOICES(asOfChoices)
    updateSelectInput(session, "asOf",
                      choices = sort(asOfChoices),
                      selected = selectedAsOf)
  }
  export_scores_server(input, output, df)
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
  updateSelectInput(session, "forecasters",
                    choices = forecasterChoices,
                    selected = forecasterInput)
}


updateCoverageChoices = function(session, df, targetVariable, forecasterChoices, coverageInput, output) {
  df = df %>% filter(forecaster %in% forecasterChoices)
  df = Filter(function(x)!all(is.na(x)), df)
  coverageChoices = intersect(colnames(df), COVERAGE_INTERVALS)
  # Ensure previsouly selected options are still allowed
  if (coverageInput %in% coverageChoices) {
    selectedCoverage = coverageInput
  } else if ("95" %in% coverageChoices) {
    selectedCoverage = "95"
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
  locationChoices = c(TOTAL_LOCATIONS, locationChoices)
  # Ensure previously selected options are still allowed
  if (locationInput %in% locationChoices) {
    selectedLocation = locationInput
  } else {
    selectedLocation = locationChoices[1]
  }
  updateSelectInput(session, "location",
                    choices = locationChoices,
                    selected = selectedLocation)
}

updateAheadChoices = function(session, df, targetVariable, forecasterChoices, aheads, targetVariableChange) {
  df = df %>% filter(forecaster %in% forecasterChoices)
  if (targetVariable == 'Hospitalizations') {
    aheadOptions = HOSPITALIZATIONS_AHEAD_OPTIONS
    title = "Forecast Horizon (Days)"
    show("horizon-disclaimer")
  } else {
    aheadOptions = AHEAD_OPTIONS
    title = "Forecast Horizon (Weeks)"
    hide("horizon-disclaimer")
  }
  aheadChoices = Filter(function(x) any(unique(df$ahead) %in% x), aheadOptions)
  # Ensure previsouly selected options are still allowed
  if (!is.null(aheads) && aheads %in% aheadChoices) {
    selectedAheads = aheads
  } else {
    selectedAheads = aheadOptions[1]
  }
  # If we are changing target variable, always reset ahead selection to first option
  if (targetVariableChange) {
    selectedAheads = aheadOptions[1]
  }
  updateCheckboxGroupInput(session, "aheads",
                           title,
                           choices = aheadChoices,
                           selected = selectedAheads,
                           inline = TRUE)
}

shinyApp(ui = ui, server = server)
