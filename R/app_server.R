#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @noRd
#'
app_server <- function( input, output, session ) {

  datetime <- mod_datetime_row_server("datetime_row_ui_1")
  button_row <- mod_button_row_server("buttons")

  mod_popup_box_server("food_vars", button_row$food, datetime)
  mod_popup_box_server("play_vars", button_row$play, datetime)
  mod_popup_box_server("poop_vars", button_row$poop, datetime)

  mod_monitor_server("monitor")
  mod_settings_server("settings")

}
