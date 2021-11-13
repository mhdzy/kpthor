#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @noRd
#'
#' @importFrom logger log_threshold log_layout layout_glue_colors TRACE
app_server <- function( input, output, session ) {
  log_threshold(TRACE)
  log_layout(layout_glue_colors)

  refresh_pull <- reactive(input$ptr)
  refresh_tabs <- reactive(input$f7_tabs)

  mod_navbar_server("navbar")

  datetime <- mod_datetime_row_server("time_vars")
  action_row <- mod_button_action_server("actions", datetime)
  input_row <- mod_button_input_server("inputs")

  mod_popup_box_server("food_vars", input_row$food, datetime)
  mod_popup_box_server("play_vars", input_row$play, datetime)
  mod_popup_box_server("poop_vars", input_row$poop, datetime)

  mod_table_server("table", refresh_pull, refresh_tabs, datetime)

  mod_monitor_server("monitor")
  mod_settings_server("settings")

}
