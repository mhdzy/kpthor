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
#' @importFrom lubridate date
#' @importFrom magrittr %>%
#' @importFrom shiny eventReactive invalidateLater renderUI
#' @importFrom shinyMobile f7Table
mod_table_server <- function(id, appdata, appdate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    localdata <- reactive({
      appdata$data() %>%
        filter(
          lubridate::date(datetime) == appdate$date(),
          pet == get_golem_options("pet")
        ) %>%
        select(
          -c(id, pet, hash)
        ) %>%
        arrange(
          desc(datetime)
        )
    })

    output$table <- renderUI({
      log_trace("[{id}] table render")
      f7Table(localdata())
    })

  })
}
