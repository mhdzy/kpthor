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
#' @importFrom shiny moduleServer reactiveVal observe isolate reactive
#'
mod_data_server <- function(id, refresh_pull, refresh_tabs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      if (!is.null(refresh_pull()) || (refresh_tabs() %in% c("monitor", "table"))) {
        log_trace("[{id}] data needs refreshing")
        # need to use isolate to prevent this from observing itself & reacting
        isolate(refresh(refresh() + 1))
      }
    })

    df_data <- eventReactive(refresh(), {
      log_trace("[{id}] df refresh")
      get_golem_options("dbi")$query_self_param_clear("kpthor", "events")
    })

    return(
      list(
        data = reactive(df_data())
      )
    )

  })
}
