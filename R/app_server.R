#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic

  button_row <- mod_button_row_server("buttons")
  mod_popup_box_server("food_vars", button_row$food)
  mod_popup_box_server("play_vars", button_row$play)
  mod_popup_box_server("poop_vars", button_row$poop)

  mod_monitor_server("monitor")
  mod_settings_server("settings")
}
