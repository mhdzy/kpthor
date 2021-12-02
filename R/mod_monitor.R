#' monitor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Row f7Col f7ExpandableCard
mod_monitor_ui <- function(id) {
  ns <- NS(id)

  tagList(
    f7Row(
      tags$style(
        ".card-expandable {
          height: 150px;
        }"
      ),
      f7Col(
        f7ExpandableCard(
          id = ns("intake"),
          title = "food",
          subtitle = uiOutput(ns("food"))
        )
      ),
      f7Col(
        f7ExpandableCard(
          id = ns("playtake"),
          title = "play",
          subtitle = uiOutput(ns("play"))
        )
      ),
      f7Col(
        f7ExpandableCard(
          id = ns("outtake"),
          title = "out",
          subtitle = uiOutput(ns("poop"))
        )
      )
    )
  )
}

#' monitor Server Functions
#'
#' @noRd
#'
#' @importFrom dplyr filter group_by mutate n select summarise
#' @importFrom shiny moduleServer
#'
mod_monitor_server <- function(id, appdata, datetime) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # how to access (this) server's vars:
    # appdata$data()
    # datetime$date()

    agg_data <- reactive({
      appdata$data() |>
        filter(date == datetime$date()) |>
        group_by(action) |>
        summarise(
          total = sum(as.numeric(value)),
          count = n()
        )
    })

    # act_data
    #
    # Get the actual (aggregated) data for a particular `action` and `type`.
    #
    # @param action A valid input 'action', eg. 'food', 'water', 'play'
    # @param type A column to return from the data
    #
    act_data <- function(act, type) {
      return((agg_data() |> filter(action == act))[[type]])
    }

    # act_form
    #
    # Get the HTML formatted sprintf display string for the monitor panels.
    #
    # @param string The sprintf-formatted string
    # @param ... The sprintf variables, must match order in `string`
    act_form <- function(string, ...) {
      return(HTML(sprintf(string, ...)))
    }

    output$food <- renderUI({
      act_form(
        "%s cups food<br>%s cups water",
        act_data("food", "total"),
        act_data("water", "total")
      )
    })

    output$play <- renderUI({
      act_form(
        "played for %s mins<br>walked for %s mins",
        act_data("play", "total"),
        act_data("walk", "total")
      )
    })

    output$poop <- renderUI({
      act_form(
        "peed %s times<br>pooped %s times",
        act_data("pee", "count"),
        act_data("poop", "count")
      )
    })

  })
}
