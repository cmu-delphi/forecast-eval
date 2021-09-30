
# create a common delphi header
delphiHeaderUI <- function(id = "delphi-header", title = "My App") {
  # Get css file
  cssFiles <- list.files(path = "www", pattern = "*.css")

  toCSSLink <- function(f) {
    # append a cache buster
    md5 <- tools::md5sum(paste0("www/", f))
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = paste0(f, "?md5=", md5))
  }

  shiny::conditionalPanel(
    condition = "window.location.search.indexOf('embed') < 0",
    id = id,
    class = "delphi-header",
    tags$head(
      lapply(cssFiles, toCSSLink),
      includeHTML("assets/google-analytics.html"),
    ),
    useShinyjs(),
    a(
      class = "delphi-header-logo", href = "https://delphi.cmu.edu",
      img(src = "./cmu_brand.png", alt = "Carnegie Mellon University Delphi Group")
    ),
    div(
      class = "delphi-header-title",
      h1(title),
    ),
  )
}

# delphi credits
delphiCredits <- function(title, repo) {
  div(
    class = "delphi-credits",
    a(href = paste0(repo, "/releases/v", appVersion), paste0(title, " v", appVersion)),
    tags$br(),
    HTML("&copy; 2021 Delphi Group"),
    tags$br(),
    a(href = repo, "Source Code"),
    " under ",
    a(href = paste0(repo, "/blob/main/LICENSE"), "MIT license")
  )
}

delphiLayoutUI <- function(id = "delphi-root", title = "My App",
                           repo = "https://github.com/cmu-delphi/forecast-eval",
                           sidebar = list(),
                           main = list()) {
  ns <- shiny::NS(id)
  font <- bslib::font_google("Open Sans", local = TRUE)
  div(
    id = id,
    class = "delphi-root",
    delphiHeaderUI(id = ns("header"), title = title),
    fluidPage(
      theme = bslib::bs_theme(
        version = 4,
        bootswatch = "default",
        primary = "#0f6ecd",
        fg = "#232735",
        bg = "#ffffff",
        base_font = font,
      ),
      sidebarLayout(
        div(
          class = "col-sm-3 delphi-sidebar p-0 px-1",
          tags$form(
            class = "well", role = "complementary",
            div(
              class = "delphi-sidebar-container",
              sidebar,
            ),
            delphiCredits(title, repo),
          )
        ),
        mainPanel(
          width = 9,
          class = "delphi-main-panel",
          main
        ),
      ),
    )
  )
}
