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
      local <- hour(Sys.time())
      greeting <-
        if (identical("development", Sys.getenv("GOLEM_CONFIG_ACTIVE"))) {
          "dev mode"
        } else if (0 <= local && local < 12) {
          "morning"
        } else if (12 <= local && local < 18) {
          "afternoon"
        } else if (18 <= local && local < 20) {
          "evening"
        } else if (20 <= local && local <= 24) {
          "night"
        } else {
          "<sys err>"
        }
     log_trace("[{id}] time-of-day greeting found: '{local}', '{greeting}'")

      h2(paste0("good ", greeting, ", ", get_golem_options("pet"), "."))
    })

  })
}
