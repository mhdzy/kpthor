#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic

  button_row <- mod_button_row_server("button_row_ui_1")
  mod_popup_box_server("popup_box_ui_1", button_row)

  mod_monitor_server("monitor_ui_1")
  mod_settings_server("settings_ui_1")
}
