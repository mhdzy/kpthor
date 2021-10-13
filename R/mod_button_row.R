#' button_row UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Row f7Col f7Button
#'
mod_button_row_ui <- function(id) {
  ns <- NS(id)
  tagList(
    f7Row(
      f7Col(
        f7Button(
          inputId = ns("food"),
          label = "food",
          color = "green"
        )
      ),
      f7Col(
        f7Button(
          inputId = ns("play"),
          label = "play",
          color = "deeppurple"
        )
      ),
      f7Col(
        f7Button(
          inputId = ns("poop"),
          label = "poop",
          color = "orange"
        )
      )
    )
  )
}

#' button_row Server Functions
#'
#' @noRd
#'
#' @import shiny
#'
mod_button_row_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(
      list(
        food = reactive(input$food),
        play = reactive(input$play),
        poop = reactive(input$poop)
      )
    )
  })
}

## To be copied in the UI
# mod_button_row_ui("button_row_ui_1")

## To be copied in the server
# mod_button_row_server("button_row_ui_1")
