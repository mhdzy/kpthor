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

  )
}

#' predictions Server Functions
#'
#' @importFrom lubridate force_tz ymd_hms
#' @importFrom logger log_trace
#' @importFrom shiny moduleServer reactive eventReactive isolate req
#'
#' @noRd
mod_predictions_server <- function(id, appdata, appdate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    datetime_now <- reactive({
      log_trace("[{id}] recalculating datetime now (internal)")
      force_tz(
        ymd_hms(paste0(appdate$date(), " ", appdate$hour(), ":", appdate$minute(), ":", "00")),
        "EST5EDT"
      )
    })

    clusters_now <- reactive({
      log_trace("[{id}] recalculating clusters")
      req(appdata$data())
      dbCluster(datetime_now(), appdata$data())
    })

    # predictions are datetime sensitive, hence why we take a dependence
    # on all coponents of appdate. we also want to re-run predictions when
    # new data is entered or updated.
    predictions <- eventReactive(c(appdata$data(), datetime_now()), {
      log_trace("[{id}] recalculating predictions")

      vals <- isolate(clusters_now())
      date <- isolate(datetime_now())
      predictions <- upcomingEvents(date, vals$eventdf, vals$clusterdf)

      return(predictions)
    })

    # daily history does *not* depend on the time, and we should want to display
    # the full daily history even if we are going back in time
    dailyhistory <- eventReactive(c(appdata$data(), appdate$date()), {
      log_trace("[{id}] recalculating daily history")

      vals <- isolate(clusters_now())
      date <- isolate(datetime_now())
      hist <- filterEvents(date, vals$eventdf)

      return(hist)
    })

    return(
      list(
        predictions = predictions,
        dailyhistory = dailyhistory
      )
    )
  })
}
