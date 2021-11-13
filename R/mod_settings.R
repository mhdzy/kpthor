#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_settings_ui <- function(id) {
  ns <- NS(id)

  tagList(

  )
}

#' settings Server Functions
#'
#' @noRd
#'
#' @importFrom shiny moduleServer
mod_settings_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}
