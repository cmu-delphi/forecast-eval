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

create_output_panel <- function(title, suffix) {
  tabPanel(title,
    value = paste0("evaluations", suffix),
    fluidRow(column(11, textOutput(paste0("renderWarningText", suffix)))),
    plotlyOutput(outputId = paste0("summaryPlot", suffix), height = "auto"),
    fluidRow(
      column(11,
        offset = 1,
        hidden(div(id = paste0("refresh-colors", suffix), actionButton(inputId = paste0("refreshColors", suffix), label = "Recolor")))
      )
    ),
    tags$br(),
    plotlyOutput(outputId = paste0("truthPlot", suffix), height = "auto"),
    fluidRow(
      column(11,
        offset = 1,
        div(id = paste0("data-loading-message", suffix), "DATA IS LOADING...(this may take a while)"),
        hidden(div(id = paste0("truth-plot-loading-message", suffix), "Fetching 'as of' data and loading observed values...")),
        hidden(div(id = paste0("notes", suffix), "About the Scores")),
        hidden(div(
          id = paste0("scoreExplanations", suffix),
          hidden(div(id = paste0("wisExplanation", suffix), wisExplanation)),
          hidden(div(id = paste0("sharpnessExplanation", suffix), sharpnessExplanation)),
          hidden(div(id = paste0("aeExplanation", suffix), aeExplanation)),
          hidden(div(id = paste0("coverageExplanation", suffix), coverageExplanation))
        )),
        hidden(div(id = paste0("scoringDisclaimer", suffix), scoringDisclaimer))
      )
    ),
    fluidRow(
      column(11,
        offset = 1,
        textOutput(paste0("renderLocationText", suffix)),
        textOutput(paste0("renderAggregateText", suffix)),
        textOutput(paste0("renderLocations", suffix)),
        tags$br()
      )
    )
  )
}

sidebar <- tags$div(
  conditionalPanel(
    # NB conditions are written in JavaScript!!
    condition = "input.tabset.startsWith('evaluations')",
    radioButtons("targetVariable", "Target Variable",
      choices = TARGET_VARS_BY_TAB[[paste0("evaluations", CURRENT_TAB_SUFFIX)]]
    ),
    radioButtons("scoreType", "Scoring Metric",
      choices = list(
        "Weighted Interval Score" = "wis",
        "Spread" = "sharpness",
        "Absolute Error" = "ae",
        "Coverage" = "coverage"
      ),
      selected = INIT_SCORE_TYPE
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
      checkboxInput(
        "scaleByBaseline",
        "Scale by Baseline Forecaster",
        value = FALSE,
      )
    ),
    selectInput(
      "forecasters",
      tags$div("Forecasters", tags$div(id = "forecaster-input", "Type a name or select from dropdown")),
      choices = c("COVIDhub-baseline", "COVIDhub-4_week_ensemble"),
      multiple = TRUE,
      selected = c("COVIDhub-baseline", "COVIDhub-4_week_ensemble")
    ),
    tags$p(
      id = "missing-data-disclaimer",
      "Some forecasters may not have data for the chosen location or scoring metric"
    ),
    checkboxGroupInput(
      "aheads",
      "Forecast Horizon (Days)",
      choices = HOSPITALIZATIONS_AHEAD_OPTIONS,
      selected = HOSPITALIZATIONS_AHEAD_OPTIONS[1L],
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
    div(
      id = "showForecastsCheckbox",
      checkboxInput(
        "showForecasts",
        "Show Forecasters' Predictions",
        value = FALSE
      )
    ),
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
  create_output_panel("Evaluation Plots", ""),
  create_output_panel("Archive Evaluation Plots", ARCHIVE_TAB_SUFFIX)
)

ui <- delphiLayoutUI(
  title = "Forecast Evaluation Dashboard",
  repo = "https://github.com/cmu-delphi/forecast-eval",
  sidebar = sidebar,
  main = main
)
