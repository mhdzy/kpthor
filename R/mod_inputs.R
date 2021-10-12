#' inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Tab f7Icon f7DatePicker f7Segment f7Button
#'
#' @noRd
#'
mod_inputs_ui <- function(id){
  ns <- NS(id)
  f7Tab(
    tabName = "inputs",
    icon = f7Icon("calendar_badge_plus"),
    active = TRUE,

    f7DatePicker(
      inputId = ns("date"),
      label = "today's date",
      value = Sys.Date(),
      multiple = FALSE,
      dateFormat = "mm/dd/yyyy",
      closeByOutsideClick = TRUE
    ),

    f7Row(
      f7Col(
        f7Button(
          inputId = ns("food_btn"),
          label = "food",
          color = "green"
        )
      ),
      f7Col(
        f7Button(
          inputId = ns("play_btn"),
          label = "play",
          color = "deeppurple"
        )
      ),
      f7Col(
        f7Button(
          inputId = ns("poop_btn"),
          label = "poop",
          color = "orange"
        )
      )
    ),
    f7Sheet(
      id = ns("food_box"),
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
              inputId = ns("food_dry"),
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
              inputId = ns("food_dry_submit"),
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
              inputId = ns("food_wet"),
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
              inputId = ns("food_wet_submit"),
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
              inputId = ns("submit"),
              label = "submit",
              color = "blue"
            )
          )
        )
      )
    )
  )
}

#' inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(
      eventExpr = input$food_btn,
      handlerExpr = {
        updateF7Sheet("food_box", session)
      }
    )

  })
}
