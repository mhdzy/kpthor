#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput imageOutput
mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(

    br(),

    f7Row(
      f7Col(
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
          borderColor = "#ffc083", # light orange
          borderWidth = 10,
          valueTextColor = "#ff9937",
          valueFontSize = 12,
          valueFontWeight = 12,
          labelText = "play",
          labelTextColor = "#ff7f04", # dark orange
          labelFontSize = 12,
          labelFontWeight = 12
        )
      )
    ),

    br(),

    f7Row(
      f7Col(
        align = "center",
        uiOutput(ns("historylist"))
      )
    )
  )
}

#' home Server Functions
#'
#' @importFrom dplyr filter select arrange desc
#' @importFrom golem get_golem_options
#' @importFrom hms as_hms hms
#' @importFrom logger log_trace log_warn
#' @importFrom lubridate hour minute
#' @importFrom magrittr %>%
#' @importFrom shiny moduleServer reactive renderUI observeEvent
#' @importFrom shinyMobile f7Picker f7SwipeoutItem f7Dialog f7Gauge updateF7Gauge
#' @importFrom shinyMobile f7List f7ListItem
#'
#' @noRd
mod_home_server <- function(id, appdata, appdate, predictions) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sortedhistory <- reactive({
      predictions$dailyhistory() %>%
        arrange(desc(time))
    })

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
        daily_max = 7,
        actions = c("water")
      ),
      `play` = list(
        units = "mins",
        daily_max = 60,
        actions = c("walk", "play")
      )
    )

    output$photo <- renderImage({
      log_trace("[{id}] updating photo")
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

        sec_to_min <- c(
          "out",
          "play",
          "walk",
          "sleep"
        )

        hgs <- home_gauge_struct[[gauge]]
        dtot <- todaydf() %>%
          dplyr::filter(action %in% hgs[['actions']]) %>%
          dplyr::mutate(value = dplyr::if_else(action %in% sec_to_min, round(value/60, 0), value)) %>%
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

    output$historylist <- renderUI({
      log_trace("[{id}] rendering historylist")

      if (!nrow(sortedhistory())) {
        return(
          f7List(
            inset = TRUE,
            mode = "media",
            f7ListItem(
              header = "",
              footer = "please add an event",
              "there are no event records for today",
              media = NULL,
              right = NULL
            )
          )
        )
      }

      f7List(
        inset = TRUE,
        lapply(
          seq(nrow(sortedhistory())),
          function(x) {

            tmp <- sortedhistory()[x, ]

            f7ListItem(
              header = paste(get_golem_options("pet"), "..."),
              footer = NULL,
              # prefixes make the labels more natural: 'walk' -> 'go on a walk', etc.
              paste(get_golem_options("eventPrefix")[[tmp$action]], tmp$action),
              media = f7Icon(get_golem_options("eventIcons")[[tmp$action]]),
              right = astime(isolate(tmp$time))
            )
          }
        )
      )
    })

  })
}
