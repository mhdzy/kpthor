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

    ## date picker ----
    f7DatePicker(
      inputId = ns("date"),
      label = "today's date",
      value = Sys.Date(),
      multiple = FALSE,
      dateFormat = "mm/dd/yyyy",
      closeByOutsideClick = TRUE
    ),

    ## button row ----
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

    ## food sheet ----
    f7Sheet(
      id = ns("food_sheet"),
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
              inputId = ns("food_cancel"),
              label = "cancel",
              color = "gray"
            )
          ),
          f7Col(
            f7Button(
              inputId = ns("food_confirm"),
              label = "confirm",
              color = "blue"
            )
          )
        )
      )
    ),

    ## play sheet ----
    f7Sheet(
      id = ns("play_sheet"),
      label = "walk/play",
      orientation = "bottom",
      swipeToClose = TRUE,
      swipeToStep = FALSE,
      closeByOutsideClick = TRUE,
      backdrop = FALSE,
      tagList(
        h4("play"),
        f7Row(
          f7Col(
            f7Stepper(
              inputId = ns("play"),
              label = NULL,
              min = 0,
              max = 60,
              step = 5,
              value = 30,
              color = "deeppurple"
            )
          ),
          f7Col(
            f7Button(
              inputId = ns("play_submit"),
              label = "submit",
              size = "small",
              color = "deeppurple",
              fill = FALSE
            )
          )
        ),
        h4("walk"),
        f7Row(
          f7Col(
            f7Stepper(
              inputId = ns("walk"),
              label = NULL,
              min = 0,
              max = 90,
              step = 5,
              value = 15,
              color = "orange"
            )
          ),
          f7Col(
            f7Button(
              inputId = ns("walk_submit"),
              label = "submit",
              size = "small",
              color = "orange",
              fill = FALSE
            )
          )
        ),
        br(), br(),
        f7Row(
          f7Col(
            f7Button(
              inputId = ns("play_cancel"),
              label = "cancel",
              color = "gray"
            )
          ),
          f7Col(
            f7Button(
              inputId = ns("play_confirm"),
              label = "confirm",
              color = "blue"
            )
          )
        )
      )
    ),

    ## poop sheet ----
    f7Sheet(
      id = ns("poop_sheet"),
      label = "pee/poop",
      orientation = "bottom",
      swipeToClose = TRUE,
      swipeToStep = FALSE,
      closeByOutsideClick = TRUE,
      backdrop = FALSE,
      tagList(
        h4("pee"),
        f7Row(
          f7Col(
            f7Stepper(
              inputId = ns("pee"),
              label = NULL,
              min = 0L,
              max = 3L,
              step = 1L,
              value = 1L,
              color = "yellow"
            )
          ),
          f7Col(
            f7Button(
              inputId = ns("pee_submit"),
              label = "submit",
              size = "small",
              color = "yellow",
              fill = FALSE
            )
          )
        ),
        h4("poop"),
        f7Row(
          f7Col(
            f7Stepper(
              inputId = ns("poop"),
              label = NULL,
              min = 0,
              max = 3L,
              step = 1L,
              value = 1L,
              color = "deeporange"
            )
          ),
          f7Col(
            f7Button(
              inputId = ns("poop_submit"),
              label = "submit",
              size = "small",
              color = "deeporange",
              fill = FALSE
            )
          )
        ),
        br(), br(),
        f7Row(
          f7Col(
            f7Button(
              inputId = ns("potty_cancel"),
              label = "cancel",
              color = "gray"
            )
          ),
          f7Col(
            f7Button(
              inputId = ns("potty_confirm"),
              label = "confirm",
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
        updateF7Sheet("food_sheet", session)
      }
    )

    observeEvent(
      eventExpr = input$play_btn,
      handlerExpr = {
        updateF7Sheet("play_sheet", session)
      }
    )

    observeEvent(
      eventExpr = input$poop_btn,
      handlerExpr = {
        updateF7Sheet("poop_sheet", session)
      }
    )

  })
}
