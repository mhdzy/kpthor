#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(

    br(),

    f7Row(
      column(
        12,
        align = "center",
        imageOutput(ns("photo"), inline = TRUE)
      )
    ),

    br(),

    f7Row(
      f7Col(
        align = "center",
        f7Gauge(
          id = ns("food-gauge"),
          size = 80,
          value = 0,

          borderBgColor = "#e2e2e2",
          borderColor = "#32cd32",
          borderWidth = 10,
          valueTextColor = "#4ea7ff",
          valueFontSize = 12,
          valueFontWeight = 12,
          labelText = "food",
          labelTextColor = "#4e4eff",
          labelFontSize = 12,
          labelFontWeight = 12
        )
      ),
      f7Col(
        align = "center",
        f7Gauge(
          id = ns("water-gauge"),
          size = 80,
          value = 0,

          borderBgColor = "#e2e2e2",
          borderColor = "#4e4eff",
          borderWidth = 10,
          valueTextColor = "#4ea7ff",
          valueFontSize = 14,
          valueFontWeight = 14,
          labelText = "water",
          labelTextColor = "#4e4eff",
          labelFontSize = 14,
          labelFontWeight = 14
        )
      ),
      f7Col(
        align = "center",
        f7Gauge(
          id = ns("play-gauge"),
          size = 80,
          value = 0,

          borderBgColor = "#e2e2e2",
          borderColor = "#ffb469",
          borderWidth = 10,
          valueTextColor = "#ffce9c",
          valueFontSize = 14,
          valueFontWeight = 14,
          labelText = "play",
          labelTextColor = "#ffb469",
          labelFontSize = 14,
          labelFontWeight = 14
        )
      )
    )

  )
}

#' home Server Functions
#'
#' @importFrom dplyr filter select arrange desc
#' @importFrom golem get_golem_options
#' @importFrom logger log_trace log_warn
#' @importFrom shiny moduleServer reactive renderUI observeEvent
#' @importFrom shinyMobile f7Picker f7SwipeoutItem f7Dialog f7Gauge updateF7Gauge
#'
#' @noRd
mod_home_server <- function(id, appdata, appdate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # initialize to be the app data (no dependency)
    todaydf <- reactiveVal(isolate(appdata$data()))

    todaydf_update <- observeEvent(appdata$data(), {
      update <- appdata$data() %>%
        mutate(date = lubridate::date(datetime)) %>%
        filter(date == appdate$date())

      # only update the internally used reactive if the data changed
      if (!identical(update, todaydf())) {
        todaydf(isolate(update))
      }
    })

    home_gauge_struct <- list(
      `food` = list(
        units = "cups",
        daily_max = 3,
        actions = c("food")
      ),
      `water` = list(
        units = "cups",
        daily_max = 5,
        actions = c("water")
      ),
      `play` = list(
        units = "mins",
        daily_max = 90,
        actions = c("walk", "out", "play")
      )
    )

    calculate_daily_total <- function(actions) {
      return(
        (isolate(todaydf()) %>%
           dplyr::filter(action %in% actions) %>%
           dplyr::summarise(sum = sum(value)))$sum
      )
    }

    observeEvent(todaydf(), {
      log_trace("[{id}] updating gauge")

      # operates in scope of the mod_home module
      updateGauge <- function(gauge) {
        hgs = home_gauge_struct[[gauge]]
        dtot = calculate_daily_total(hgs[['actions']])
        updateF7Gauge(
          id = ns(paste0(gauge, "-gauge")),
          value = dtot/hgs[['daily_max']] * 100,
          valueText = paste(dtot, hgs[['units']])
        )
      }

      # apply updates
      lapply(names(home_gauge_struct), updateGauge)
    })

    output$photo <- renderImage({
      list(
        src = file.path("inst/app/www/favicon.ico"),
        contentType = "image/jpeg",
        class = "pet_photo",
        height = "100%",
        width = "100%"
      )
    }, deleteFile = FALSE)

  })
}
