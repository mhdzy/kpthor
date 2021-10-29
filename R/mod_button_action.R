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
    uiOutput(ns("timer")),
    uiOutput(ns("row"))
  )
}

#' button_action Server Functions
#'
#' @noRd
#'
#' @importFrom logger log_trace
#' @importFrom lubridate seconds_to_period hour minute second
#' @importFrom shiny moduleServer tagList observe observeEvent reactive
#' @importFrom shiny reactiveVal reactiveValues req renderUI isolate
#' @importFrom shinyMobile f7Row f7Col f7Button updateF7Button f7Block f7BlockHeader
#'
mod_button_action_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    action_struct = list(
      walk = list(
        name = "walk",
        start_activity = "start walk",
        end_activity = "end walk",
        active_color = "red",
        inactive_color = "blue"
      ),
      out = list(
        name = "out",
        start_activity = "go outside",
        end_activity = "come inside",
        active_color = "red",
        inactive_color = "blue"),
      sleep = list(
        name = "sleep",
        start_activity = "go to sleep",
        end_activity = "wake up",
        active_color = "red",
        inactive_color = "blue"
      )
    )

    active_timer_stat <- reactiveVal(FALSE)
    active_timer_mode <- reactiveVal("")
    active_timer_time <- reactiveVal(0)
    active_timer_nice <- reactive({
      req(active_timer_stat())
      pd <- seconds_to_period(active_timer_time())
      pt_time <- c(hr = hour(pd), min = minute(pd), sec = second(pd))
      pt_time <- pt_time[pt_time != 0]

      paste(uapply(seq_along(pt_time), function(x) paste(pt_time[x], names(pt_time)[x])), collapse = " ")
    })

    button_pressed <- reactiveVal(NA_character_)
    button_cache <- reactiveVal(rep(0, times = length(action_struct)))
    button_input <- reactive({
      unlist(lapply(names(action_struct), function(x) input[[x]]))
    })

    output$timer <- renderUI({
      f7Row(
        lapply(names(action_struct), function(x) {
          f7Col(
            if (identical(x, active_timer_mode())) {
              f7Block(
                HTML(paste0("<b>", x, " time: ", "</b>", active_timer_nice()))
              )
            } else {
              f7Block()
            }
          )
        })
      )
    })

    output$row <- renderUI({
      f7Row(
        lapply(action_struct, function(x) {
          f7Col(
            f7Button(
              inputId = ns(x[['name']]),
              label = x[['start_activity']],
              color = x[['inactive_color']]
            )
          )
        })
      )
    })

    #
    # updates the button_pressed and button_cache reactives
    #
    observeEvent(button_input(), {
      req(button_input())
      log_trace("[{id}] an input was pressed {paste0(button_input(), collapse = \" \")}")

      fetched_row <- button_input() # cached for later update
      row_diff <- fetched_row - button_cache()
      pressed <- names(action_struct)[which(as.logical(row_diff))]

      if (length(pressed)) {
        button_pressed(pressed)
        button_cache(fetched_row)
        log_trace("[{id}] button_pressed updated to '{button_pressed()}'")
      }
    })

    observe({
      invalidateLater(1000)
      isolate({
        if (active_timer_stat()) {
          active_timer_time(active_timer_time() + 1)
          if (!(active_timer_stat() %% 60)) {
            log_trace("[{id}] time: {active_timer_time()}")
          }
        }
      })
    })

    observeEvent(button_cache(), {
      req(button_pressed())
      log_trace("[{id}] button press detected: {button_pressed()}")

      if (identical("", active_timer_mode())) {
        print("new")
        # NO ACTIVE TIMER       START NEW TIMER
        active_timer_stat(TRUE)
        active_timer_mode(button_pressed())
        updateF7Button(
          inputId = active_timer_mode(),
          label = action_struct[[active_timer_mode()]][['end_activity']],
          color = action_struct[[active_timer_mode()]][['active_color']]
        )
      } else if (identical(active_timer_mode(), button_pressed())) {
        print("identical")
        # END ACTIVE TIMER      RECORD TIME       DO NOT START NEW TIMER
        mode <- isolate(active_timer_mode())
        time <- isolate(active_timer_time())
        # TODO: record to db
        log_trace("[{id}] mode '{mode}' ran for '{time}' seconds")

        active_timer_stat(FALSE)
        active_timer_mode("")
        active_timer_time(0)
        updateF7Button(
          inputId = mode,
          label = action_struct[[mode]][['start_activity']],
          color = action_struct[[mode]][['inactive_color']]
        )
      } else {
        print("different")
        # END ACTIVE TIMER      RECORD TIME       START NEW TIMER
        mode <- isolate(active_timer_mode())
        time <- isolate(active_timer_time())
        # TODO: record to db
        log_trace("[{id}] mode '{mode}' ran for '{time}' seconds")

        active_timer_stat(FALSE)
        active_timer_mode("")
        active_timer_time(0)
        updateF7Button(
          inputId = mode,
          label = action_struct[[mode]][['start_activity']],
          color = action_struct[[mode]][['inactive_color']]
        )

        active_timer_stat(TRUE)
        active_timer_mode(button_pressed())
        updateF7Button(
          inputId = active_timer_mode(),
          label = action_struct[[active_timer_mode()]][['end_activity']],
          color = action_struct[[active_timer_mode()]][['active_color']]
        )
      }
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
