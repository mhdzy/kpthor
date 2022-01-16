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

  #session$allowReconnect(TRUE)

  refresh_pull <- reactive(input$ptr)
  refresh_tabs <- reactive(input$f7_tabs)

  appdata <- mod_data_server("data", refresh_pull, refresh_tabs)

  mod_navbar_server("navbar")

  appdate <- mod_appdate_row_server("time_vars")
  action_row <- mod_button_action_server("actions", appdata, appdate)
  input_row <- mod_button_input_server("inputs")

  # event predictions, cluster based std. deviation ranges
  predictions <- mod_predictions_server("predictions", appdata, appdate)
  mod_predlist_server("input_preds", appdata, appdate, predictions)

  mod_popup_box_server("food_vars", input_row$food, appdata, appdate)
  mod_popup_box_server("play_vars", input_row$play, appdata, appdate)
  mod_popup_box_server("poop_vars", input_row$poop, appdata, appdate)

  mod_monitor_server("monitor", appdata, appdate)
  mod_home_server("home", appdata, appdate, predictions)
  mod_table_server("table", appdata, appdate)
  mod_settings_server("settings", appdata, appdate)
}
