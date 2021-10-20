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
#' @importFrom dplyr arrange desc filter select
#' @importFrom golem get_golem_options
#' @importFrom logger log_trace log_debug
#' @importFrom magrittr %>%
#' @importFrom shiny eventReactive invalidateLater renderUI
#' @importFrom shinyMobile f7Table
mod_table_server <- function(id, refresh_trigger, datetime) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df_data_rt <- reactive({
      # force invalidation and update on df_data without hiding the result
      # when input$ptr (refresh_trigger()) becomes NULL at end of animation
      refresh_trigger()
      1L
    })

    df_data <- eventReactive(df_data_rt(), {
      log_trace("[{id}] df refresh")
      # invalidateLater(5000)
      dbi <- get_golem_options("dbi")
      dbi$query_self_param("kpthor", "events3")
    })

    output$table <- renderUI({
      log_trace("[{id}] table render")
      f7Table(
        df_data() %>%
          filter(
            date == datetime$date(),
            pet == get_golem_options("pet")
          ) %>%
          select(
            -c(pet)
          ) %>%
          arrange(
            desc(time)
          )
      )
    })

  })
}
