library(shiny)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(stringr)

df <- read.csv("score_case_edit.csv", header = TRUE,sep = ",")
modelChoices = unique(df$model)
locationChoices = unique(df$location)

ui <- fluidPage(
    titlePanel("Forecast Eval"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("scoreType", "Score type",
                   choices = list("WIS" = "wis", "MAE" = "abs_error")),
        selectInput(
          "forecasters",
          "Forecasters",
          choices = modelChoices,
          multiple = TRUE,
          selected = "COVIDhub-ensemble"
        ),
        radioButtons("ahead", "Ahead",
                     choices = list("1 week" = 1, "2 weeks" = 2, "3 weeks" = 3, "4 weeks" = 4)),
        selectInput(
          "location",
          "Location",
          choices = locationChoices,
          multiple = FALSE,
          selected = "US"
        ),
    ),
    mainPanel(
      plotOutput(outputId = "summaryPlot"),
      dataTableOutput('renderTable')
    ),
  ),
)


server <- function(input, output) {
  
  # From https://stackoverflow.com/questions/15282580/
  # color_picker = function(n) {
  #   qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  #   unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  # }
  
  summary_plot = function(score_df, score_type = "wis", forecasters = "All", 
                          ahead = 1, loc = "US", ylab = score_type, 
                          ylim = NULL) {
    score_df <- score_df %>% filter(score_name == score_type) %>%
                             filter(horizon == ahead) %>% 
                             filter(location == loc) %>%
                             filter(model %in% forecasters)
          
    output$renderTable <- renderDataTable(score_df)
    ggplot(score_df, aes(x = target_end_date, y = score_value, group=model)) +
      geom_line(aes(color = model, linetype = model)) +
      geom_point(aes(color = model)) +
      labs(x = "Date", y = ylab)
      # scale_color_manual(values = color_picker(length(unique(model))))
  }

  output$summaryPlot <- renderPlot({
    summary_plot(df, input$scoreType, input$forecasters, input$ahead, input$location)})
  
}

shinyApp(ui = ui, server = server)

