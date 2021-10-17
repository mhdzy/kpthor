#' datetime_row UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom lubridate hour
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Row f7Col f7DatePicker f7Slider f7Icon
mod_datetime_row_ui <- function(id) {
  ns <- NS(id)

  tagList(
    f7Row(
      f7Col(
        f7DatePicker(
          inputId = ns("date"),
          label = "date",
          value = Sys.Date(),
          multiple = FALSE,
          dateFormat = "mm/dd/yyyy",
          closeByOutsideClick = TRUE
        )
      ),
      f7Col(
        f7Slider(
          inputId = ns("time"),
          label = "time",
          min = 0,
          max = 24,
          value = hour(Sys.time()),
          scaleSteps = 24,
          scaleSubSteps = 4,
          labels = tagList(f7Icon("sunrise"), f7Icon("sunset-fill")),
          color = "green"
        )
      )
    )
  )

}

#' datetime_row Server Functions
#'
#' @noRd
mod_datetime_row_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(
      list(
        date = reactive(input$date),
        time = reactive(input$time)
      )
    )

  })
}

