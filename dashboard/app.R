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

source('./common.R')

# All data is fully loaded from AWS
dataLoaded = FALSE

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

ui <- fluidPage(padding=0,
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
            conditionalPanel(condition = "input.scoreType != 'coverage' && input.targetVariable != 'Hospitalizations'",
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
            tags$p(id="forecaster-disclaimer", "Some forecasters may not have data for the chosen location or scoring metric"),
            checkboxGroupInput(
              "aheads",
              "Forecast Horizon (Weeks)",
              choices = AHEAD_OPTIONS,
              selected = AHEAD_OPTIONS[1],
              inline = TRUE
            ),
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
                     div(id="loading-message", "DATA IS LOADING..."),
                     hidden(div(id="notes", "About the Scores")),
                     hidden(div(id = "wisExplanation", wisExplanation)),
                     hidden(div(id = "sharpnessExplanation", sharpnessExplanation)),
                     hidden(div(id = "aeExplanation", aeExplanation)),
                     hidden(div(id = "coverageExplanation", coverageExplanation)),
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


server <- function(input, output, session) {
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
  dfStateHospitalizations = getData("score_cards_state_hospitalizations.rds")
  dfNationHospitalizations = getData("score_cards_nation_hospitalizations.rds")
  dataLoaded = TRUE

  # Pick out expected columns only
  covCols = paste0("cov_", COVERAGE_INTERVALS)
  expectedCols = c("ahead", "geo_value", "forecaster", "forecast_date",
                   "data_source", "signal", "target_end_date", "incidence_period",
                   "actual", "wis", "sharpness", "ae",
                   covCols)

  dfStateCases = dfStateCases %>% select(all_of(expectedCols))
  dfStateDeaths = dfStateDeaths %>% select(all_of(expectedCols))
  dfNationCases = dfNationCases %>% select(all_of(expectedCols))
  dfNationDeaths = dfNationDeaths %>% select(all_of(expectedCols))
  dfStateHospitalizations = dfStateHospitalizations %>% select(all_of(expectedCols))
  dfNationHospitalizations = dfNationHospitalizations %>% select(all_of(expectedCols))
  
  df <- rbind(dfStateCases, dfStateDeaths, dfNationCases, dfNationDeaths, dfStateHospitalizations, dfNationHospitalizations)
  df <- df %>% rename("10" = cov_10, "20" = cov_20, "30" = cov_30, "40" = cov_40, "50" = cov_50, "60" = cov_60, "70" = cov_70, "80" = cov_80, "90" = cov_90, "95" = cov_95, "98" = cov_98)

  # Prepare color palette
  colorSeed = 100

  # Prepare input choices
  forecasterChoices = sort(unique(df$forecaster))
  updateForecasterChoices(session, df, forecasterChoices, 'wis')

  
  ##################
  # CREATE MAIN PLOT
  ##################
  summaryPlot = function(scoreDf, colorSeed) {
    allLocations = FALSE
    if (input$location == TOTAL_LOCATIONS) {
      allLocations = TRUE
    }
    signalFilter = CASE_FILTER
    if (input$targetVariable == "Deaths") {
      signalFilter = DEATH_FILTER
    }
    if (input$targetVariable == "Hospitalizations") {
      signalFilter = HOSPITALIZATIONS_FILTER
    }
    filteredScoreDf = scoreDf %>%
      filter(signal == signalFilter) %>%
      filter(forecaster %in% input$forecasters)

    if (signalFilter == HOSPITALIZATIONS_FILTER) {
      filteredScoreDf = filterHospitalizationsAheads(filteredScoreDf)
    }
    filteredScoreDf = filteredScoreDf %>% filter(ahead %in% input$aheads)
    if (input$scoreType == "wis" || input$scoreType == "sharpness") {
      # Only show WIS or Sharpness for forecasts that have all intervals
      filteredScoreDf = filteredScoreDf %>% filter(!is.na(`50`)) %>% filter(!is.na(`80`)) %>% filter(!is.na(`95`))
      if (input$targetVariable == "Deaths") {
        filteredScoreDf = filteredScoreDf %>% filter(!is.na(`10`)) %>% filter(!is.na(`20`)) %>% filter(!is.na(`30`)) %>%
                          filter(!is.na(`40`)) %>% filter(!is.na(`60`)) %>% filter(!is.na(`70`)) %>% filter(!is.na(`90`)) %>% filter(!is.na(`98`))
      }
      if (dim(filteredScoreDf)[1] == 0) {
        output$renderWarningText <- renderText("The selected forecasters do not have enough data to display the selected scoring metric.")
        return()
      }
      if (input$scoreType == "wis") {
        plotTitle = "Weighted Interval Score"
      }
      else {
        plotTitle = "Spread"
      }
    }
    if (input$scoreType == "ae") {
      plotTitle = "Absolute Error"
    }
    if (input$scoreType == "coverage") {
      plotTitle = "Coverage"
    }
    filteredScoreDf = renameScoreCol(filteredScoreDf, input$scoreType, input$coverageInterval)

    # Totaling over all locations
    if (allLocations || input$scoreType == "coverage") {
      filteredScoreDfAndIntersections = filterOverAllLocations(filteredScoreDf, input$scoreType)
      filteredScoreDf = filteredScoreDfAndIntersections[[1]]
      locationsIntersect = filteredScoreDfAndIntersections[[2]]
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
      }
    # Not totaling over all locations
    } else {
      filteredScoreDf <- filteredScoreDf %>% filter(geo_value == tolower(input$location)) %>%
        group_by(forecaster, forecast_date, target_end_date, ahead) %>%
        summarize(Score = Score, actual = actual)
      locationSubtitleText = paste0(', Location: ', input$location)
      output$renderAggregateText = renderText("")
      output$renderLocations <- renderText("")
      output$renderWarningText <- renderText("")
    }

    # Rename columns that will be used as labels
    filteredScoreDf = filteredScoreDf %>% rename(Forecaster = forecaster, Forecast_Date = forecast_date,
                                                 Week_End_Date = target_end_date)
    # Render truth plot with observed values
    showElement("truthPlot")
    showElement("refresh-colors")
    truthDf = filteredScoreDf
    output$truthPlot <- renderPlotly({
      truthPlot(truthDf, locationsIntersect, allLocations || input$scoreType == "coverage")
    })

    # Format and transform data
    filteredScoreDf = filteredScoreDf[c("Forecaster", "Forecast_Date", "Week_End_Date", "Score", "ahead")]
    filteredScoreDf = filteredScoreDf %>% mutate(across(where(is.numeric), ~ round(., 2)))
    if (input$scoreType != 'coverage') {
      if (input$scaleByBaseline && input$targetVariable != "Hospitalizations") {
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

    titleText = paste0('<b>', plotTitle,'</b>','<br>', '<sup>',
                       'Target Variable: ', input$targetVariable,
                       locationSubtitleText, '<br>',
                       tags$span(id="drag-to-zoom", " Drag to zoom"),
                       '</sup>')
    # Fill gaps so there are line breaks on weeks without data
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
    # Set forecaster colors for plot
    set.seed(colorSeed)
    forecasterRand <- sample(unique(df$forecaster))
    colorPalette = setNames(object = viridis(length(unique(df$forecaster))), nm = forecasterRand)

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
  truthPlot = function(scoreDf = NULL, locationsIntersect = NULL, allLocations = FALSE) {
    observation = paste0('Incident ', input$targetVariable)
    if (input$targetVariable == "Hospitalizations") {
      observation = paste0('Hospital Admissions')
    }
    titleText = paste0('<b>Observed ', observation, '</b>')
    if (allLocations) {
      titleText = paste0('<b>Observed ', observation, '</b>', ' <br><sup>Totaled over all states and territories common to selected forecasters*</sup>')
    }
    scoreDf <- scoreDf %>%
      group_by(Week_End_Date) %>% summarize(Reported_Incidence = actual)

    return (ggplotly(ggplot(scoreDf, aes(x = Week_End_Date, y = Reported_Incidence)) +
      geom_line() +
      geom_point() +
      labs(x = "", y = "", title = titleText) +
      scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
      scale_x_date(date_labels = "%b %Y") + theme_bw())
      %>% layout(hovermode = 'x unified')
      %>% config(displayModeBar = F))
  }

  #############
  # PLOT OUTPUT
  #############
  output$summaryPlot <- renderPlotly({
    summaryPlot(df, colorSeed)
  })

  ###################
  # EVENT OBSERVATION
  ###################

  observeEvent(input$refreshColors, {
    colorSeed = floor(runif(1, 1, 1000))
    output$summaryPlot <- renderPlotly({
      summaryPlot(df, colorSeed)
    })
  })

  # When the target variable changes, update available forecasters, locations, and CIs to choose from
  observeEvent(input$targetVariable, {
    if (input$targetVariable == 'Deaths') {
      df = df %>% filter(signal == DEATH_FILTER)
    } else if (input$targetVariable == 'Cases') {
      df = df %>% filter(signal == CASE_FILTER)
    } else {
      df = df %>% filter(signal == HOSPITALIZATIONS_FILTER)
    }

    updateAheadChoices(session, df, input$targetVariable, input$forecasters, input$aheads, TRUE)
    updateForecasterChoices(session, df, input$forecasters, input$scoreType)
    updateLocationChoices(session, df, input$targetVariable, input$forecasters, input$location)
    updateCoverageChoices(session, df, input$targetVariable, input$forecasters, input$coverageInterval, output)
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

  # Ensure the minimum necessary input selections
  observe({
    # Show data loading message and hide other messages until all data is loaded
    if (dataLoaded) {
      hide("loading-message")
      show("refresh-colors")
      show("notes")
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

updateAheadChoices = function(session, df, targetVariable, forecasterChoices, aheads, targetVariableChange) {
  df = df %>% filter(forecaster %in% forecasterChoices)
  aheadOptions = AHEAD_OPTIONS
  title = "Forecast Horizon (Weeks)"
  if (targetVariable == 'Hospitalizations') {
    aheadOptions = HOSPITALIZATIONS_AHEAD_OPTIONS
    title = "Forecast Horizon (Days)"
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
