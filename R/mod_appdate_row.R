#' appdate_row UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom lubridate hour minute today
#' @importFrom shiny NS tagList h5
#' @importFrom shinyMobile f7Row f7Col f7DatePicker f7Slider f7Icon f7Stepper
mod_appdate_row_ui <- function(id) {
  ns <- NS(id)

  tagList(
    f7Row(
      f7Col(
        width = 6,
        f7DatePicker(
          inputId = ns("date"),
          label = "date",
          value = lubridate::today() + 1,
          multiple = FALSE,
          dateFormat = "mm/dd/yyyy",
          closeByOutsideClick = TRUE
        )
      ),
      f7Col(
        align = "center",
        lapply(
          seq_along(get_golem_options(id)),
          function(x) {
            golem_opts <- get_golem_options(id)
            f7Row(
              f7Col(
                align = "left",
                h4(names(golem_opts)[x]),
              ),
              f7Col(
                align = "center",
                f7Stepper(
                  inputId = ns(paste0("time_", names(golem_opts)[x])),
                  label = "",
                  #label = paste0("* time (", names(golem_opts)[x], ") *"),
                  min = golem_opts[[x]]$min,
                  max = golem_opts[[x]]$max,
                  value = golem_opts[[x]]$value(Sys.time()),
                  color = golem_opts[[x]]$color,
                  #size = "small",
                  raised = TRUE,
                  wraps = TRUE,
                  manual = TRUE,
                  autorepeat = TRUE,
                  buttonsEndInputMode = TRUE
                )
              )
            )
          }
        )
      )
    )
  )

}

#' appdate_row Server Functions
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive
mod_appdate_row_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(
      list(
        date = reactive(input$date),
        hour = reactive(input$time_hour),
        minute = reactive(input$time_minute)
      )
    )

  })
}

