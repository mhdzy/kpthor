#' monitor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom plotly plotlyOutput
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Row f7Col f7ExpandableCard
mod_monitor_ui <- function(id) {
  ns <- NS(id)

  tagList(

    br(),

    f7Row(
      class = "margin",
      tags$style(
        ".card-expandable {
          height: 150px;
        }"
      ),
      f7Col(
        f7ExpandableCard(
          id = ns("intake"),
          title = "food",
          subtitle = uiOutput(ns("food")),
          plotlyOutput(ns("food_content"))
        )
      ),
      f7Col(
        f7ExpandableCard(
          id = ns("playtake"),
          title = "play",
          subtitle = uiOutput(ns("play")),
          plotlyOutput(ns("play_content"))
        )
      ),
      f7Col(
        f7ExpandableCard(
          id = ns("outtake"),
          title = "out",
          subtitle = uiOutput(ns("poop")),
          plotlyOutput(ns("poop_content"))
        )
      )
    )
  )
}

#' monitor Server Functions
#'
#' @noRd
#'
#' @importFrom dplyr filter group_by mutate n select summarise pull
#' @importFrom lubridate date
#' @importFrom ggplot2 aes ggplot geom_point
#' @importFrom magrittr %>%
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom shiny moduleServer renderImage
#'
mod_monitor_server <- function(id, appdata, appdate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # how to access (this) server's vars:
    # appdata$data()
    # appdate$date()

    today_data <- reactive({
      appdata$data() %>%
        filter(lubridate::date(datetime) == appdate$date())
    })

    # transforms the daily (selected) data to totals & counts by action
    total_count_data <- reactive({
      today_data() %>%
        group_by(action) %>%
        summarise(
          value = sum(as.numeric(value)),
          count = n()
        )
    })

    # tod_data
    #
    # Get the daily (selected via input date) raw data.
    #
    # @param act A valid input action, eg. 'food', 'water', 'play'.
    #
    tod_data <- function(act) {
      return(
        today_data() %>%
          filter(action %in% act) %>%
          group_by(datetime, action) %>%
          summarise(value = sum(as.numeric(value)))
      )
    }

    # tc_data
    #
    # Get the actual (aggregated) total count data for a particular `action` and `type`.
    #
    # @param act A valid input action, eg. 'food', 'water', 'play'.
    # @param type A column to return from the data.
    #
    tc_data <- function(act, type) {
      if (!nrow(total_count_data())) return(0)
      return((total_count_data() %>% filter(action %in% act)) %>% pull(value))
    }

    # html_format
    #
    # Get the HTML formatted sprintf display string.
    #
    # @param string The sprintf-formatted string.
    # @param ... The sprintf variables, must match order in `string`.
    #
    html_format <- function(string, ...) {
      return(HTML(sprintf(string, ...)))
    }

    plotly_format <- function(dat) {
      (
        dat %>%
          ggplot(aes(x = datetime, y = value, color = action)) +
          geom_point()
      ) %>%
        ggplotly()
    }

    output$food <- renderUI({
      tagList(
        html_format(
          "%s cups food",
          tc_data("food", "value")
        ),
        br(),
        html_format(
          "%s cups water",
          tc_data("water", "value")
        )
      )
    })

    output$play <- renderUI({
      tagList(
        html_format(
          "outside for %s hours",
          # need sum because we pass 2 possible values
          round(sum(tc_data(c("play", "out"), "value")/3600), 1)
        ),
        br(),
        html_format(
          "walked for %s hours",
          round(tc_data("walk", "value")/3600, 1)
        )
      )
    })

    output$poop <- renderUI({
      tagList(
        html_format(
          "peed %s times",
          tc_data("pee", "count")
        ),
        br(),
        html_format(
          "pooped %s times",
          tc_data("poop", "count")
        )
      )
    })

    output$food_content <- renderPlotly({
      tod_data(c("food", "water")) %>%
        plotly_format()
    })

    output$play_content <- renderPlotly({
      tod_data(c("out", "play", "walk")) %>%
        plotly_format()
    })

    output$poop_content <- renderPlotly({
      tod_data(c("pee", "poop")) %>%
        plotly_format()
    })

  })
}
