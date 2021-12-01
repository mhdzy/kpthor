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
mod_table_server <- function(id, refresh_pull, refresh_tabs, datetime) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh <- reactiveVal(0L)

    # no observeEvent because only act on certain conditions
    observe({
      # this observer exists to force invalidation and update on df_data
      # without hiding the result when input$ptr (refresh_tabs()) becomes
      # NULL at end of animation
      #
      # also react to tab switch, but only when the user is trying to view tbl
      if (!is.null(refresh_pull()) || refresh_tabs()  == "table") {
        # need to use isolate to prevent this from observing itself & reacting
        isolate(refresh(refresh() + 1))
      }
    })

    df_data <- eventReactive(refresh, {
      log_trace("[{id}] df refresh")
      get_golem_options("dbi")$query_self_param_clear("kpthor", "events")
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
            desc(time),
            desc(minute)
          )
      )
    })

  })
}
