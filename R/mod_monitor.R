#' monitor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_monitor_ui <- function(id) {
  ns <- NS(id)

  tagList(

  )
}

#' monitor Server Functions
#'
#' @noRd
#'
#' @importFrom shiny moduleServer
mod_monitor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}
