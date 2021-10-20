#' button_action UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_button_action_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("row"))
  )
}

#' button_action Server Functions
#'
#' @noRd
#'
#' @importFrom logger log_trace
#' @importFrom shiny moduleServer reactive renderUI tagList observeEvent
#' @importFrom shinyMobile f7Row f7Col f7Button updateF7Button
mod_button_action_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    row_struct = list(
      walk  = c("walk",  "start walk", "blue"),
      out   = c("out",   "go outside", "blue"),
      sleep = c("sleep", "go to sleep", "blue")
    )

    output$row <- renderUI({
      f7Row(
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

    ## walk timer ----
    observeEvent(input$walk, {
        log_trace("{id} walk triggered")
        updateF7Button(inputId = "walk", label = "end walk", color = "red")
    })

    ## outside timer ----
    observeEvent(input$out, {
        log_trace("{id} out triggered")
        updateF7Button(inputId = "out", label = "come inside", color = "red")
    })

    ## sleep timer ----
    observeEvent(input$sleep, {
        log_trace("{id} sleep triggered")
        updateF7Button(inputId = "sleep", label = "wake up", color = "red")
    })

    return(
      list(
        walk = reactive(input$walk),
        out = reactive(input$out),
        sleep = reactive(input$sleep)
      )
    )

  })
}
