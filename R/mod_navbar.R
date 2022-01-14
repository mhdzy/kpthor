#' navbar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_navbar_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("message"))
  )
}

#' navbar Server Functions
#'
#' @noRd
#'
#' @importFrom golem get_golem_options
#' @importFrom logger log_trace
#' @importFrom lubridate hour
#' @importFrom shiny moduleServer renderUI h2
mod_navbar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$message <- renderUI({
      log_trace("[{id}] render navbar message")
      local <- hour(Sys.time())
      greeting <-
        if (identical("development", Sys.getenv("GOLEM_CONFIG_ACTIVE"))) {
          "dev mode"
        } else if (0 <= local && local < 12) {
          "Morning"
        } else if (12 <= local && local < 18) {
          "Afternoon"
        } else if (18 <= local && local < 20) {
          "Evening"
        } else if (20 <= local && local <= 24) {
          "Night"
        } else {
          "<sys err>"
        }
      log_trace("[{id}] time-of-day greeting found: '{local}', '{greeting}'")

      h2(paste0("Good ", greeting, ", ", get_golem_options("pet"), "."))
    })

  })
}
