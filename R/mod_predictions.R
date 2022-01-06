#' predictions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_predictions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tbl"))
  )
}

#' predictions Server Functions
#'
#' @importFrom lubridate force_tz ymd_hms
#' @importFrom logger log_trace
#'
#' @noRd
mod_predictions_server <- function(id, appdata, appdate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    datetime <- reactive({
      force_tz(
        ymd_hms(paste0(appdate$date(), " ", appdate$hour(), ":", appdate$minute(), ":", "00")),
        "EST5EDT")
    })

    browser()

    predictions <- eventReactive(appdata$data(), {
      dat <- dbMigrate(df = appdata$data())
      vals <- dbCluster(datetime(), dat)
      predictions <- upcomingEvents(datetime(), vals$eventdf, vals$clusterdf)
      return(predictions)
    })

    output$tbl <- renderUI({
      log_trace("[{id}] predicting...")
      print(predictions())
      predictions()
    })
  })
}

## To be copied in the UI
# mod_predictions_ui("predictions_ui_1")

## To be copied in the server
# mod_predictions_server("predictions_ui_1")
