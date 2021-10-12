#' monitor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_monitor_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "monitor",
    icon = f7Icon("graph_square")
  )
}

#' monitor Server Functions
#'
#' @noRd
mod_monitor_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_monitor_ui("monitor_ui_1")

## To be copied in the server
# mod_monitor_server("monitor_ui_1")
