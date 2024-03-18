#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom tidyr complete
#' @import ggplot2
#' @import gtExtras
#' @import DBI
#' @import gt
#' @importFrom sjmisc trim
#' @importFrom rlang sym
#' @import odbc
#' @noRd

app_server <- function(input, output, session) {
  library(shiny)
  library(dplyr)
  library(odbc)
  # Your application server logic


  tbl <- reactive({
    con <- DBI::dbConnect(odbc(),
                          Driver = 'ODBC Driver 17 for SQL Server',
                          Server = "smldbtest.database.windows.net",
                          Database = "SMLCloudDev",
                          UID = "SMLDevUser",
                          PWD = "devUserRdOnly@1")


    df <- dbGetQuery(con,"SELECT * FROM [DIP].[DemandHistorysubst];")

    df <- df |>
      dplyr::mutate(PN = sjmisc::trim(PN))


    df
  })

  tbl2 <- reactive({
    con <- DBI::dbConnect(odbc(),
                          Driver = "ODBC Driver 17 for SQL Server",
                          Server = "153.112.64.200\\SQL1",
                          Database = "CTSPPCP",
                          UID = "cs-ws-s-ANDON-PROD",
                          PWD = "CTSPPCP@123")



    time <- paste0(lubridate::year(Sys.time()),
                   if (lubridate::month(Sys.time()) < 10) paste0("0", lubridate::month(Sys.time())) else lubridate::month(Sys.time()))

    query <- paste0("SELECT Partnumber as Partnumber,
                        Period as Period,
                        ArimaFc as ArimaFc
                 FROM dbo.ExcelSupplies
                 WHERE Period >= '", time, "';")


    df_forcast <- dbGetQuery(con, query)

    df_forcast <- df_forcast |>
      dplyr::filter(!Partnumber %in% grepl('deprecated', Partnumber)) |>
      dplyr::mutate(PN = as.character(sjmisc::trim(Partnumber)),
                    Qty =as.numeric(stringr::str_replace(ArimaFc,",",".")))
    df_forcast
  })


  output$tbl_mtcars <- gt::render_gt({
    req(input$pn_input) # Move this inside the render function

    # Move the reactive tbl() call and subsequent operations inside render_gt
    filtered_tbl <- reactive({
      color_palette <- c("tomato", "#009E73")

      filtered_tbl <-  tbl() |>
        dplyr::filter(PN == input$pn_input)  |>
        dplyr::mutate(Dt = lubridate::year(Month))  |>
        dplyr::group_by(Dt) |>
        dplyr::summarise(Sum      = sum(Qty),
                         S.Dev    = sd(Qty),
                         Average  = mean(Qty),
                         Coef.Var = S.Dev/Average,
                         min      = min(Sum),
                         max      = max(Sum)) |>
        dplyr::ungroup()

      min <- min(filtered_tbl$min)
      max <- max(filtered_tbl$max)

      filtered_tbl |>
        dplyr::select(-min,-max) |>
        gt::gt() |>
        gt::tab_header(title    = gt::md("**Summary - PN**"),
                       subtitle = gt::md("*Statistics*"))  |>
        gt::tab_source_note(source_note = gt::md("Source: Demandhistorysubst"))  |>
        gtExtras::gt_color_rows(columns = "Sum", domain = c(min, max), palette = color_palette)
    })

    # Use the reactive filtered_tbl() here
    filtered_tbl()
  })



  output$plot2 <- renderPlot({
    req(input$pn_input)

    m <- tbl2() |>
      dplyr::filter(PN == input$pn_input)

    m <- m |>
      dplyr::arrange(Period) |>
      dplyr::mutate(Period = as.Date(paste0(Period, "01"), format = "%Y%m%d")) |>
      dplyr::ungroup() |>
      dplyr::select(Qty, Period)

    m |>
      ggplot2::ggplot()+
      ggplot2::aes(y = Qty, x = Period)+
      ggplot2::geom_line()+
      ggplot2::geom_point()+
      ggplot2::theme_bw()+
      ggplot2::geom_text(ggplot2::aes(label = Qty),
                         vjust = "inward", hjust = "inward",
                         show.legend = FALSE) +
      ggplot2::scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")


  })

  output$plot <- renderPlot({
    req(input$pn_input)

    m <- tbl() |>
      dplyr::filter(PN == input$pn_input)

    dataset <- m |>
      tidyr::complete(Month = seq.Date(from = min(as.Date(Month)), to = max(as.Date(Month)), by = "month"), fill = list(Qty = 0)) |>
      dplyr::arrange(Month)

    dataset |>
      ggplot2::ggplot() +
      ggplot2::aes(y = Qty, x = Month) +
      ggplot2::geom_line() +
      ggplot2::theme_bw() +
      ggplot2::geom_smooth() +
      ggplot2::labs(title = "Historical Demand")



  })

  output$hist_density_plot <- renderPlot({
    req(input$pn_input)

    m <- tbl() |>
      dplyr::filter(PN == input$pn_input)

    ggplot2::ggplot(m, ggplot2::aes(x = Qty)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), color = "black", fill = "grey") +
      ggplot2::geom_density(alpha = .3, fill = "tomato") +
      ggplot2::theme_bw()+
      ggplot2::labs(title = "Dispersion")
  })

}
