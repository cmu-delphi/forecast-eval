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

# Prepare color palette and linetypes
set.seed(100)
forecaster_rand <- sample(unique(df$forecaster))
color_palette = setNames(object = viridis(length(unique(df$forecaster))), nm = forecaster_rand)
avail_linetypes = rep(c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), length.out = length(unique(df$forecaster)))
linetypes = setNames(object = avail_linetypes, nm = forecaster_rand)

# Prepare input choices
forecasterChoices = sort(unique(df$forecaster))
aheadChoices = unique(df$ahead)
locationChoices = unique(toupper(df$geo_value))
locationChoices = locationChoices[c(length(locationChoices), (1:length(locationChoices)-1))] # Move US to front of list
coverageChoices = intersect(colnames(df), COVERAGE_INTERVALS)

# Score explanations
wisExplanation = includeMarkdown("wis.md")
aeExplanation = includeMarkdown("ae.md")
coverageExplanation = includeMarkdown("coverageplot.md")
scoringDisclaimer = includeMarkdown("scoring-disclaimer.md")

# About page content
aboutPageText = includeMarkdown("about.md")

########
# Layout
########
ui <- fluidPage(padding=0,
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    useShinyjs(),
    div(id="header",class="row",
      div(id="logo",class="col-sm-3",
        a(href="https://delphi.cmu.edu",
          img(src="cmu_brand.png",width="220px",heigh="50px",alt="Carnegie Mellon University Delphi Group")
        )
      ),
      div(class="col-sm-7",
          span(id="title","FORECAST EVALUATION DASHBOARD"),
        ),
      div(id="github-logo-container", class="col-sm-2",
        a(id="github-logo",href="https://github.com/cmu-delphi/forecast-eval/",
            includeHTML("github.svg"),
            HTML("&nbsp;GITHUB")
          )
      )
    ),
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
              p("Forecasters", tags$br(), tags$span(id="forecaster-input", "Type a name or select from dropdown")),
              choices = forecasterChoices,
              multiple = TRUE,
              selected = c("COVIDhub-baseline", "COVIDhub-ensemble")
            ),
            tags$p(id="forecaster-disclaimer", "Some forecasters may not have data for the chosen location or scoring metric"),
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
                               "Totals Over All States and Territories (common to selected forecasters)*",
                               value = FALSE,
                             )
            ),
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
            tags$br(),
            plotlyOutput(outputId = "truthPlot", height="auto"),
            fluidRow(
              column(11, offset=1,
                     div(id="notes", "About the Scores"),
                     hidden(div(id = "wisExplanation", wisExplanation)),
                     hidden(div(id = "aeExplanation", aeExplanation)),
                     hidden(div(id = "coverageExplanation", coverageExplanation)),
                     div(id = "scoringDisclaimer", scoringDisclaimer)
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
    
    filteredScoreDf <- scoreDf %>% rename(Forecaster = forecaster, Week_End_Date = target_end_date)
    
    if (scoreType == "wis") {
      filteredScoreDf <- filteredScoreDf %>% rename(Score = wis)
      title = "Weighted Interval Score"
      # Only show WIS for forecasts that have all intervals
      filteredScoreDf = filteredScoreDf %>% filter(!is.na(`50`)) %>% filter(!is.na(`80`)) %>% filter(!is.na(`95`))
      if (targetVariable == "Deaths") {
        filteredScoreDf = filteredScoreDf %>% filter(!is.na(`10`)) %>% filter(!is.na(`20`)) %>% filter(!is.na(`30`)) %>%
                          filter(!is.na(`40`)) %>% filter(!is.na(`60`)) %>% filter(!is.na(`70`)) %>% filter(!is.na(`90`)) %>% filter(!is.na(`98`))
      }
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
      locationDf = filteredScoreDf %>% group_by(Forecaster, Week_End_Date, ahead) %>% 
        summarize(location_list = paste(sort(unique(geo_value)),collapse=","))
      # Create a list containing each row's location list
      locationList = sapply(locationDf$location_list, function(x) strsplit(x, ","))
      locationList = lapply(locationList, function(x) x[x != 'us'])
      # Get the intersection of all the locations in these lists
      locationsIntersect = unique(Reduce(intersect, locationList))
      filteredScoreDf = filteredScoreDf %>% filter(geo_value %in% locationsIntersect)
      aggregateText = "*For fair comparison, all displayed forecasters on all displayed dates are compared across a common set of states and territories."
      if (scoreType == "coverage") {
        aggregate = "Averaged"
        filteredScoreDf = filteredScoreDf %>%
          group_by(Forecaster, Week_End_Date, ahead) %>%
          summarize(Score = sum(Score)/length(locationsIntersect), actual = sum(actual))
        output$renderAggregateText = renderText(paste(aggregateText," Some forecasters may not have any data for the coverage interval chosen. Locations inlcuded: "))
      }
      else {
        aggregate = "Totaled"
        filteredScoreDf = filteredScoreDf %>%
          group_by(Forecaster, Week_End_Date, ahead) %>%
          summarize(Score = sum(Score), actual = sum(actual))
        output$renderAggregateText = renderText(paste(aggregateText, " Locations included: "))
      }

      if (length(locationsIntersect) == 0) {
        output$renderWarningText <- renderText("The selected forecasters do not have data for any locations in common.")
        output$renderLocations <- renderText("")
        output$renderAggregateText = renderText("")
        hideElement("truthPlot")
        return()
      }
      else {
        locationSubtitleText = paste0(', Location: ', aggregate ,' over all states and territories common to these forecasters*')
        output$renderLocations <- renderText(toupper(locationsIntersect))
        output$renderWarningText = renderText("")
      }
    # Not totaling over all locations
    } else {
      filteredScoreDf <- filteredScoreDf %>% filter(geo_value == tolower(loc)) %>%
        group_by(Forecaster, Week_End_Date, ahead) %>%
        summarize(Score = Score, actual = actual)
      locationSubtitleText = paste0(', Location: ', input$location)
      output$renderAggregateText = renderText("")
      output$renderLocations <- renderText("")
      output$renderWarningText <- renderText("")
    }
    
    # Render truth plot with observed values
    showElement("truthPlot")
    truthDf = filteredScoreDf
    output$truthPlot <- renderPlotly({
      truthPlot(truthDf, targetVariable, locationsIntersect, allLocations || scoreType == "coverage")
    })
    
    filteredScoreDf = filteredScoreDf[c("Forecaster", "Week_End_Date", "Score", "ahead")]
    filteredScoreDf = filteredScoreDf %>% mutate(across(where(is.numeric), ~ round(., 2)))
    titleText = paste0('<b>',title,'</b>','<br>', '<sup>',
                       'Target Variable: ', targetVariable,
                       locationSubtitleText, '<br>',
                       tags$span(id="drag-to-zoom", " Drag to zoom"),
                       '</sup>')
    # Fill gaps so there are line breaks on weeks without data
    filteredScoreDf = filteredScoreDf %>%
      as_tsibble(key = c(Forecaster, ahead), index = Week_End_Date) %>%
      group_by(Forecaster, ahead) %>%
      fill_gaps(.full = TRUE)
    
    filteredScoreDf$ahead = factor(filteredScoreDf$ahead, levels = c(1, 2, 3, 4), 
                                    labels = c("Horizon: 1 Week", "Horizon: 2 Weeks", "Horizon: 3 Weeks", "Horizon: 4 Weeks"))
    p = ggplot(
        filteredScoreDf, 
        aes(x = Week_End_Date, y = Score, color = Forecaster, linetype = Forecaster)
      ) +
      geom_line() +
      geom_point() +
      labs(x = "", y = "", title=titleText) +
      scale_x_date(date_labels = "%b %Y") +
      scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
      facet_wrap(~ahead, ncol=1) +
      scale_color_manual(values = color_palette) +
      scale_linetype_manual(values = linetypes) + 
      theme_bw() + 
      theme(panel.spacing=unit(0.5, "lines"))

    if (scoreType == "coverage") {
      p = p + geom_hline(yintercept = .01 * as.integer(coverageInterval))
    }
    plotHeight = 550 + (length(horizon)-1)*100
    
    finalPlot <- 
      ggplotly(p,tooltip = c("x", "y", "linetype")) %>% 
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
  
  # Create the plot for target variable ground truth
  truthPlot = function(scoreDf = NULL, targetVariable = NULL, locationsIntersect = NULL, allLocations = FALSE) {
    titleText = paste0('<b>Observed Incident ', targetVariable, '</b>')
    if (allLocations) {
      titleText = paste0('<b>Observed Incident ', targetVariable, '</b>', ' <br><sup>Totaled over all states and territories common to selected forecasters*</sup>')
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
    summaryPlot(df, input$targetVariable, input$scoreType, input$forecasters, 
                input$aheads, input$location, input$allLocations, input$coverageInterval)
  })

  ###################
  # EVENT OBSERVATION
  ###################
  
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
      show("wisExplanation")
      hide("aeExplanation")
      hide("coverageExplanation")
    }
    if (input$scoreType == "ae") {
      hide("wisExplanation")
      show("aeExplanation")
      hide("coverageExplanation")
    }
    if (input$scoreType == "coverage") {
      hide("wisExplanation")
      hide("aeExplanation")
      show("coverageExplanation")
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
