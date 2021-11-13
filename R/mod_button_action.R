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
#' @importFrom shiny reactiveVal reactiveValues req renderUI isolate div
#' @importFrom shinyMobile f7Row f7Col f7Button updateF7Button f7Block f7BlockHeader
#'
mod_button_action_server <- function(id, datetime) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # names are used to define button inputs
    # items are lists of visual parameters; labels and colors
    action_struct <- get_golem_options(id)

    # active_timer_* are reactive values used to administer the timer mechanism
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

    ## o timer ----
    observe({
      invalidateLater(1000)
      isolate({
        if (active_timer_stat()) active_timer_time(active_timer_time() + 1)
      })
    })

    # button_* are reactive values used to determine button press names
    button_pressed <- reactiveVal(NA_character_)
    button_cache <- reactiveVal(rep(0, times = length(action_struct)))
    button_input <- reactive({
      uapply(names(action_struct), function(x) input[[x]])
    })

    ## o$ timer ----
    output$timer <- renderUI({
      f7Row(
        lapply(names(action_struct), function(x) {
          f7Col(
            style = "text-align: center; padding-bottom: 20px;",
            HTML(ifelse(
              identical(x, active_timer_mode()),
              paste0("<b>", active_timer_nice(), "</b>"),
              ""
            ))
          )
        })
      )
    })

    ## o$ row ----
    output$row <- renderUI({
      f7Row(
        lapply(action_struct, function(x) {
          f7Col(
            f7Button(
              inputId = ns(x[['name']]),
              label = x[['start_label']],
              color = x[['inactive_color']]
            )
          )
        })
      )
    })

    ## oE button input ----
    # button_input
    #
    # Determines (by name) which button was pressed. This is achieved by keeping
    # a cache of the button press values to check against when a button press
    # is detected. Once the name is found, updates the button_pressed and
    # button_cache reactives.
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

    ## oE button cache ----
    # button_cache
    #
    #  Handles the mode/timer switching when action buttons are pressed. The
    #  default (non-active) mode is "", but can be made one of 'out', 'walk',
    #  or 'sleep'. Each of these is an identical abstract timer with identical
    #  event handlers.
    #
    #  The relationship between each press can be one of: 'new', 'same', or
    #  'swap'.
    #   * If 'new', the button pressed becomes the active timer mode, and the
    #     timer begins. There is nothing to record and thus is a simple event.
    #   * If 'same', the button pressed is the same as the active timer mode,
    #     thus the timer ends. The time elapsed is recorded to the db.
    #   * If 'swap', then the timer mode is active, and the button pressed is
    #     different from the active timer. The old timer ends, elapsed time
    #     recorded, and the new timer mode begins.
    #
    observeEvent(button_cache(), {
      req(button_pressed())
      log_trace("[{id}] button press detected: {button_pressed()}")

      type <-
        if (identical("", active_timer_mode())) {
          "new"
        } else if (identical(button_pressed(), active_timer_mode())) {
          "same"
        } else { # !identical(button_pressed(), active_timer_mode())
          "swap"
        }

      if (type == "same" || type == "swap") {
        mode <- isolate(active_timer_mode())
        time <- isolate(active_timer_time())
        log_trace("[{id}] mode '{mode}' ran for '{time}' seconds")

        df <- data.frame(
          date = datetime$date(),
          time = datetime$hour(),
          minute = datetime$minute(),
          pet = get_golem_options("pet"),
          action = mode,
          value = time
        )
        get_golem_options("dbi")$append("kpthor", "events", df)
        log_debug("[{id}] appended ", nrow(df), " rows to kpthor.events")

        active_timer_stat(FALSE)
        active_timer_mode("")
        active_timer_time(0)
        updateF7Button(
          inputId = mode,
          label = action_struct[[mode]][['start_label']],
          color = action_struct[[mode]][['start_color']]
        )
      }

      if (type == "new" || type == "swap") {
        active_timer_stat(TRUE)
        active_timer_mode(button_pressed())
        updateF7Button(
          inputId = active_timer_mode(),
          label = action_struct[[active_timer_mode()]][['end_label']],
          color = action_struct[[active_timer_mode()]][['end_color']]
        )
      }
    })

    return(
      list(
        walk  = reactive(input$walk),
        out   = reactive(input$out),
        sleep = reactive(input$sleep)
      )
    )

  })
}
