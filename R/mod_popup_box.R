#' popup_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyMobile
#'
mod_popup_box_ui <- function(id, vars) {
  ns <- NS(id)
  ## food sheet ----
  tagList(
    f7Sheet(
      id = ns("sheet"),
      label = "food/water",
      orientation = "bottom",
      swipeToClose = TRUE,
      swipeToStep = FALSE,
      closeByOutsideClick = TRUE,
      backdrop = FALSE,
      tagList(
        h4("food"),
        f7Row(
          f7Col(
            f7Stepper(
              inputId = ns("food"),
              label = NULL,
              min = 0,
              max = 5,
              step = 0.5,
              value = 2.5,
              color = "teal"
            )
          ),
          f7Col(
            f7Button(
              inputId = ns("food_submit"),
              label = "submit",
              size = "small",
              color = "teal",
              fill = FALSE
            )
          )
        ),
        h4("water"),
        f7Row(
          f7Col(
            f7Stepper(
              inputId = ns("water"),
              label = NULL,
              min = 0,
              max = 5,
              step = 0.5,
              value = 1.0,
              color = "lightblue"
            )
          ),
          f7Col(
            f7Button(
              inputId = ns("water_submit"),
              label = "submit",
              size = "small",
              color = "lightblue",
              fill = FALSE
            )
          )
        ),
        br(), br(),
        f7Row(
          f7Col(
            f7Button(
              inputId = ns("cancel"),
              label = "cancel",
              color = "gray"
            )
          ),
          f7Col(
            f7Button(
              inputId = ns("confirm"),
              label = "confirm",
              color = "blue"
            )
          )
        )
      )
    )
  )
}

#' popup_box Server Functions
#'
#' @noRd
mod_popup_box_server <- function(id, sheet_trigger) {
  stopifnot(is.list(sheet_trigger))
  stopifnot(is.reactive(sheet_trigger$food))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(sheet_trigger$food(), updateF7Sheet("sheet"))

  })
}

## To be copied in the UI
# mod_popup_box_ui("popup_box_ui_1")

## To be copied in the server
# mod_popup_box_server("popup_box_ui_1")
