#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr filter select distinct pull
#' @importFrom sjmisc trim
#' @importFrom rlang sym
#' @import gt
#' @import gtExtras
#' @import plotly
#' @import shinydashboardPlus
#' @import ggplot2
#' @import odbc
#' @noRd
#'


app_ui <- function(request) {
  fluidPage(
    h1("Partnumber Analysis"),

    sidebarPanel(
      style = "background-color: #203354; padding: 8px; max-width: 200px;",
      textInput("pn_input", label = HTML("<span style='color: white;'>Enter PN:</span>"), placeholder = "e.g., ABC123", width = "150px"),
      actionButton("submit_button", "Submit", style = "margin-top: 8px;")
    ),

    mainPanel(
      fluidRow(
        column(width = 5,
               gt::gt_output(outputId = "tbl_mtcars"),
               style = "overflow-x: auto;"
        ),
        column(width = 7,
               tabsetPanel(
                 tabPanel("Demand", plotOutput("plot"), style = "background-color: #f5f5f5; padding: 20px; border-radius: 5px;"),
                 tabPanel("Histogram and Density Plot", plotOutput("hist_density_plot"), style = "background-color: #f5f5f5; padding: 20px; border-radius: 5px;"),
                 # Adicionando o novo tabPanel para plot2
                 tabPanel("Forecast", plotOutput("plot2"), style = "background-color: #f5f5f5; padding: 20px; border-radius: 5px;")
               )
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "misc2"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
