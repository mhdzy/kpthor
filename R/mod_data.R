#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' data Server Functions
#'
#' @noRd
#'
#' @importFrom golem get_golem_options
#' @importFrom logger log_trace
#' @importFrom promises %...>% %...T>% %...!% future_promise
#' @importFrom shiny moduleServer reactiveVal observe isolate reactive
#'
mod_data_server <- function(id, refresh_pull, refresh_tabs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dat <- NULL
    dat_cache <- reactiveVal()
    refresh <- reactiveVal(0L)

    # no observeEvent because only act on certain conditions
    observe({
      log_trace("[{id}] data refresh trigger detected")
      # this observer exists to force invalidation and update on df_data
      # without hiding the result when input$ptr (refresh_tabs()) becomes
      # NULL at end of animation
      #
      # also react to tab switch, but only when the user is trying to view
      # information via 'monitor' or 'table' tabs
      if (!is.null(refresh_pull()) || (refresh_tabs() %in% c("monitor", "home", "table"))) {
        log_trace("[{id}] data needs refreshing")
        # need to use isolate to prevent this from observing itself & reacting
        isolate(refresh(refresh() + 1))
      }
    })

    dat_refresh <- eventReactive(refresh(), {
      log_trace("[{id}] df refresh detected")
      promises::future_promise({
        get_golem_options("dbi")$query_self_param_clear(
          get_golem_options("schema"), get_golem_options("table")
        )
      })
    })

    dat_latest <- eventReactive(dat_refresh(), {
      log_trace("[{id}] dat_refresh triggered a latest pull")
      dat <<- environment(dat_refresh()[["then"]])[["private"]][["value"]] %>%
        dplyr::mutate(datetime = lubridate::with_tz(datetime, "EST5EDT"))
      dat
    })

    observeEvent(dat_latest(), {
      log_trace("[{id}] dat_latest triggered observer")
      # only update data object if we get new info
      if (!identical(dat_cache(), dat)) {
        log_trace("[{id}] latest data does not match cached data; updating")
        dat_cache(dat)
      }
    })

    return(
      list(
        data = reactive(dat_cache())
      )
    )

  })
}
