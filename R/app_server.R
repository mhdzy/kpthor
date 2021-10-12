#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  mod_inputs_server("inputs_ui_1")
  mod_monitor_server("monitor_ui_1")
  mod_settings_server("settings_ui_1")
}
