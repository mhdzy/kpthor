#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("table"))
  )

}

#' table Server Functions
#'
#' @noRd
#'
#' @importFrom dplyr arrange filter desc
#' @importFrom golem get_golem_options
#' @importFrom shiny invalidateLater renderUI
#' @importFrom shinyMobile f7Table
mod_table_server <- function(id, datetime) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dbi <- get_golem_options("dbi")

    df_data <- reactive({
      logger::log_trace("df refresh")
      invalidateLater(5000)
      dbi$query_self_param("kpthor", "events3")
    })

    output$table <- renderUI({
      f7Table(
        arrange(
          filter(
            df_data(),
            date == datetime$date()
          ),
          desc(time)
        )
      )
    })

  })
}

## To be copied in the UI
# mod_table_ui("table_ui_1")

## To be copied in the server
# mod_table_server("table_ui_1")
