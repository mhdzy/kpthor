#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @noRd
#'
#' @importFrom logger log_threshold log_layout layout_glue_colors TRACE
#' @importFrom shiny reactive
#'
app_server <- function( input, output, session ) {
  log_threshold(TRACE)
  log_layout(layout_glue_colors)

  session$allowReconnect(TRUE)

  refresh_pull <- reactive(input$ptr)
  refresh_tabs <- reactive(input$f7_tabs)

  appdata <- mod_data_server("data", refresh_pull, refresh_tabs)

  mod_navbar_server("navbar")

  datetime <- mod_datetime_row_server("time_vars")
  action_row <- mod_button_action_server("actions", datetime)
  input_row <- mod_button_input_server("inputs")

  mod_popup_box_server("food_vars", input_row$food, datetime)
  mod_popup_box_server("play_vars", input_row$play, datetime)
  mod_popup_box_server("poop_vars", input_row$poop, datetime)

  mod_monitor_server("monitor", appdata, datetime)
  mod_table_server("table", appdata, datetime)
  mod_report_server("report", appdata, datetime)
  mod_settings_server("settings")
}
