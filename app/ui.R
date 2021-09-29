# Score explanations
wisExplanation <- includeMarkdown("assets/wis.md")
sharpnessExplanation <- includeMarkdown("assets/sharpness.md")
aeExplanation <- includeMarkdown("assets/ae.md")
coverageExplanation <- includeMarkdown("assets/coverageplot.md")
scoringDisclaimer <- includeMarkdown("assets/scoring-disclaimer.md")

# About page content
aboutPageText <- includeMarkdown("assets/about.md")
aboutDashboardText <- includeMarkdown("assets/about-dashboard.md")

########
# Layout
########

sidebar <- tags$div(
  conditionalPanel(
    condition = "input.tabset == 'evaluations'",
    radioButtons("targetVariable", "Target Variable",
      choices = list(
        "Incident Deaths" = "Deaths",
        "Incident Cases" = "Cases",
        "Hospital Admissions" = "Hospitalizations"
      )
    ),
    radioButtons("scoreType", "Scoring Metric",
      choices = list(
        "Weighted Interval Score" = "wis",
        "Spread" = "sharpness",
        "Absolute Error" = "ae",
        "Coverage" = "coverage"
      )
    ),
    conditionalPanel(
      condition = "input.scoreType != 'coverage'",
      class = "checkbox-grouper",
      tags$div(class = "control-label", "Y-Axis Score Scale"),
      checkboxInput(
        "logScale",
        "Log Scale",
        value = FALSE,
      ),
      conditionalPanel(
        condition = "input.targetVariable != 'Hospitalizations'",
        checkboxInput(
          "scaleByBaseline",
          "Scale by Baseline Forecaster",
          value = FALSE,
        )
      ),
    ),
    selectInput(
      "forecasters",
      tags$div("Forecasters", tags$div(id = "forecaster-input", "Type a name or select from dropdown")),
      choices = c("COVIDhub-baseline", "COVIDhub-ensemble"),
      multiple = TRUE,
      selected = c("COVIDhub-baseline", "COVIDhub-ensemble")
    ),
    tags$p(
      id = "missing-data-disclaimer",
      "Some forecasters may not have data for the chosen location or scoring metric"
    ),
    checkboxGroupInput(
      "aheads",
      "Forecast Horizon (Weeks)",
      choices = AHEAD_OPTIONS,
      selected = AHEAD_OPTIONS[1],
      inline = TRUE
    ),
    hidden(tags$p(
      id = "horizon-disclaimer",
      "Forecasters submitted earlier than Mondays may have longer actual prediction horizons"
    )),
    conditionalPanel(
      condition = "input.scoreType == 'coverage'",
      selectInput(
        "coverageInterval",
        "Coverage Interval",
        choices = "",
        multiple = FALSE,
        selected = "95"
      ),
    ),
    conditionalPanel(
      condition = "input.scoreType != 'coverage'",
      selectInput(
        "location",
        "Location",
        choices = "",
        multiple = FALSE,
        selected = "US"
      )
    ),
    selectInput(
      "asOf",
      "As Of",
      choices = "",
      multiple = FALSE,
      selected = ""
    ),
    tags$p(id = "missing-data-disclaimer", "Some locations may not have 'as of' data for the chosen 'as of' date"),
    hidden(div(
      id = "showForecastsCheckbox",
      checkboxInput(
        "showForecasts",
        "Show Forecasters' Predictions",
        value = FALSE,
      )
    )),
    tags$hr(),
    exportScoresUI("exportScores"),
    tags$hr()
  ),
  aboutDashboardText,
  tags$hr()
)

main <- tabsetPanel(
  id = "tabset",
  selected = "evaluations",
  tabPanel(
    "About",
    fluidRow(column(
      10,
      div(
        id = "aboutContentArea",
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
  tabPanel("Evaluation Plots",
    value = "evaluations",
    fluidRow(column(11, textOutput("renderWarningText"))),
    plotlyOutput(outputId = "summaryPlot", height = "auto"),
    fluidRow(
      column(11,
        offset = 1,
        hidden(div(id = "refresh-colors", actionButton(inputId = "refreshColors", label = "Recolor")))
      )
    ),
    tags$br(),
    plotlyOutput(outputId = "truthPlot", height = "auto"),
    fluidRow(
      column(11,
        offset = 1,
        div(id = "data-loading-message", "DATA IS LOADING...(this may take a while)"),
        hidden(div(id = "truth-plot-loading-message", "Fetching 'as of' data and loading observed values...")),
        hidden(div(id = "notes", "About the Scores")),
        hidden(div(
          id = "scoreExplanations",
          hidden(div(id = "wisExplanation", wisExplanation)),
          hidden(div(id = "sharpnessExplanation", sharpnessExplanation)),
          hidden(div(id = "aeExplanation", aeExplanation)),
          hidden(div(id = "coverageExplanation", coverageExplanation))
        )),
        hidden(div(id = "scoringDisclaimer", scoringDisclaimer))
      )
    ),
    fluidRow(
      column(11,
        offset = 1,
        textOutput("renderLocationText"),
        textOutput("renderAggregateText"),
        textOutput("renderLocations"),
        tags$br()
      )
    )
  )
)

ui <- delphiLayoutUI(
  title = "Forecast Evaluation Dashboard",
  repo = "https://github.com/cmu-delphi/forecast-eval",
  sidebar = sidebar,
  main = main
)
