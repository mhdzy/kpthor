#' button_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_button_input_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("row"))
  )
}

#' button_input Server Functions
#'
#' @noRd
#'
#' @importFrom logger log_trace
#' @importFrom shiny moduleServer reactive renderUI tagList
#' @importFrom shinyMobile f7Row f7Col f7Button
mod_button_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    row_struct <- get_golem_options(id)

    ## o$ row ----
    output$row <- renderUI({
      log_trace("[{id}] rendering input buttons")
      f7Row(
        class = "margin",
        #style = "margin: 20px",
        lapply(row_struct, function(x) {
          f7Col(
            f7Button(
              inputId = ns(x[[1]]),
              label = x[[2]],
              color = x[[3]]
            )
          )
        })
      )
    })

    return(
      list(
        food = reactive(input$food),
        play = reactive(input$play),
        poop = reactive(input$poop)
      )
    )
  })
}
