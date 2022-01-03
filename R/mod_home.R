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
        align = "right",
        f7Gauge(
          id = ns("food-gauge"),
          size = 80,
          value = 0,

          borderBgColor = "#afafaf", # gray (dark 3, darkest)
          borderColor = "#57d777", # light green
          borderWidth = 10,
          valueTextColor = "#26a145", # green
          valueFontSize = 12,
          valueFontWeight = 12,
          labelText = "food",
          labelTextColor = "#1c7833", # dark green
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

          borderBgColor = "#c8c8c8", # gray (dark 2)
          borderColor = "#57b7d7", # light blue
          borderWidth = 10,
          valueTextColor = "#4ea7ff", # blue
          valueFontSize = 12,
          valueFontWeight = 12,
          labelText = "water",
          labelTextColor = "#1c6178", # dark blue
          labelFontSize = 12,
          labelFontWeight = 12
        )
      ),
      f7Col(
        align = "left",
        f7Gauge(
          id = ns("play-gauge"),
          size = 80,
          value = 0,

          borderBgColor = "#d2d2d2", # gray (dark 1, lightest)
          borderColor = "#ffcd9c", # light orange
          borderWidth = 10,
          valueTextColor = "#ffb469",
          valueFontSize = 12,
          valueFontWeight = 12,
          labelText = "play",
          labelTextColor = "#ff9a36", # dark orange
          labelFontSize = 12,
          labelFontWeight = 12
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

    todaydf <- reactive({
      # no need to wrap in a 'change detector' as appdata() is only updated
      # if different from previously cached values
      #' @seealso `R/mod_data.R`
      appdata$data() %>%
        mutate(date = lubridate::date(datetime)) %>%
        filter(date == appdate$date())
    })

    # used for programmatic gauge updates
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

    output$photo <- renderImage({
      list(
        src = file.path("inst/app/www/favicon.ico"),
        contentType = "image/jpeg",
        class = "pet_photo",
        height = "60%",
        width = "60%"
      )
    }, deleteFile = FALSE)

    observeEvent(todaydf(), {
      log_trace("[{id}] updating gauge")

      # operates in scope of the mod_home module
      updateGauge <- function(gauge) {

        hgs <- home_gauge_struct[[gauge]]
        dtot <- todaydf() %>%
          dplyr::filter(action %in% hgs[['actions']]) %>%
          dplyr::summarise(sum = sum(value)) |>
          dplyr::pull(sum)

        val <- dtot/hgs[['daily_max']] * 100
        valtext <- paste(dtot, hgs[['units']])

        updateF7Gauge(
          id = ns(paste0(gauge, "-gauge")),
          value = val,
          valueText = valtext
        )

      }

      # apply updates
      lapply(names(home_gauge_struct), updateGauge)
    })

  })
}
