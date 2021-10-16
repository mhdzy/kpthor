#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Tab f7Icon
#'
#' @noRd
#'
mod_settings_ui <- function(id) {
  ns <- NS(id)
  f7Tab(
    tabName = "settings",
    icon = f7Icon("gear")
  )
}

#' settings Server Functions
#'
#' @importFrom shiny moduleServer
#'
#' @noRd
#'
mod_settings_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}
