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
#' @importFrom dplyr if_else
#' @importFrom liteq ack consume is_empty list_messages list_failed_messages requeue_failed_messages publish try_consume
#' @importFrom logger log_info log_trace log_warn
#' @importFrom lubridate seconds_to_period hour minute second seconds
#' @importFrom magrittr %>%
#' @importFrom shiny moduleServer tagList observe observeEvent reactive
#' @importFrom shiny reactiveVal reactiveValues req renderUI isolate div
#' @importFrom shinyMobile f7Row f7Col f7Button updateF7Button f7Block f7BlockHeader
#'
mod_button_action_server <- function(id, appdata, appdate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # names are used to define button inputs
    # items are lists of visual parameters; labels and colors
    action_struct <- get_golem_options(id)

    # safety check variable to ensure app does not refresh inactive timers
    # continuously. needs to be TRUE on init to color buttons blue
    needs_refresh <- reactiveVal(TRUE)

    # active_timer_* are reactive values used to administer the timer mechanism
    active_timer_mode <- reactiveVal("")
    active_timer_time <- reactiveVal(0)
    active_timer_nice <- reactive({
      if (!nchar(active_timer_mode())) return("")
      pd <- seconds_to_period(active_timer_time())
      pt_time <- c(hr = hour(pd), min = minute(pd), sec = second(pd))
      pt_time <- pt_time[pt_time != 0]

      paste(uapply(seq_along(pt_time), function(x) paste(pt_time[x], names(pt_time)[x])), collapse = " ")
    })

    ## o timer ----
    observe({
      invalidateLater(get_golem_options("timer_interval"))
      log_trace("[{id}] observing timer stuff...")
      isolate({
        timerq <- get_golem_options("timerq")

        if (!is_empty(timerq)) {
          log_trace("[{id}] queue not empty, work to be done")
          msg <- liteq::try_consume(timerq)

          if (is.null(msg)) {
            # need to fix queue and try again
            if (nrow(liteq::list_failed_messages(timerq))) {
              log_warn("[{id}] need to requeue a failed message")
              liteq::requeue_failed_messages(timerq)
              msg <- liteq::try_consume(timerq)
            } else {
              log_fatal("[{id}] liteq queue seems corrupted, check db")
              stop("non-empty liteq queue 'timerq' with no FAILED messages is returning NULL")
            }
          }

          # republish fetched message to the queue before acknowledging old msg
          liteq::publish(timerq, title = msg$title, message = msg$message)
          log_trace("[{id}] message published")

          # acknowledge "working" message from queue for safe removal of object
          liteq::ack(msg)
          log_trace("[{id}] message acknowledged")

          # if msg contains a timer we aren't tracking, update our mode
          if (active_timer_mode() != msg$title) active_timer_mode(msg$title)

          # set timer time as diff from start and curr
          active_timer_time(round(as.numeric(Sys.time() - as.POSIXlt(msg$message), units = "secs"), 0L))

          # TODO: make this not brute-forced
          # sets active timer button to red
          updateF7Button(
            inputId = action_struct[[active_timer_mode()]][['name']],
            label = action_struct[[active_timer_mode()]][['end_label']],
            color = action_struct[[active_timer_mode()]][['end_color']]
          )
          log_trace("[{id}] active timer set")

          # TODO: make this not brute-forced
          # clears out non-active timer rows to blue
          lapply(names(action_struct)[!(names(action_struct) %in% active_timer_mode())], function(x) {
            updateF7Button(
              inputId = action_struct[[x]][['name']],
              label = action_struct[[x]][['start_label']],
              color = action_struct[[x]][['start_color']]
            )
          })
          log_trace("[{id}] inactive timers reset")

          # inform the timer clearing section to clear the timers, bypass stopper
          needs_refresh(TRUE)
        } else {
          # clear everybody out
          # only update DOM if it actually needs refreshing
          if (needs_refresh()) {
          active_timer_mode("")
          active_timer_time(0)

            log_warn("[{id}] we are refreshing buttons")
            lapply(names(action_struct), function(x) {
              updateF7Button(
                inputId = action_struct[[x]][['name']],
                label = action_struct[[x]][['start_label']],
                color = action_struct[[x]][['start_color']]
              )
            })
            needs_refresh(FALSE)
          } else {
            log_trace("[{id}] refresh avoided")
          }
        }
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
      log_trace("[{id}] render timer output row")
      f7Row(
        class = "margin-lr",
        lapply(names(action_struct), function(x) {
          f7Col(
            style = "text-align: center;",
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
      log_trace("[{id}] render timer button row")
      f7Row(
        class = "margin-lr",
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

      timerq <- get_golem_options("timerq")

      type <-
        if (identical("", active_timer_mode())) {
          "new"
        } else if (identical(button_pressed(), active_timer_mode())) {
          "same"
        } else {
          # !identical(button_pressed(), active_timer_mode())
          "swap"
        }

      if (type == "same" || type == "swap") {
        mode <- isolate(active_timer_mode())
        timer_time <- isolate(active_timer_time())
        log_trace("[{id}] mode '{mode}' ran for '{timer_time}' seconds")

        df <-
          data.frame(
            pet = get_golem_options("pet"),
            datetime = lubridate::with_tz(lubridate::force_tz(
              lubridate::ymd_hms(paste0(appdate$date(), " ", appdate$hour(), ":", appdate$minute(), ":", "00")),
              "EST5EDT"), "UTC"),
            action = mode,
            value = timer_time
          ) %>%
          dplyr::mutate(
            # all timers store values in seconds, no unit conversion
            # value = dplyr::if_else(action %in% sec_to_min, value/60, value),
            hash = purrr::map_chr(paste0(pet, datetime, action, value), digest::digest, algo = "sha256")
          )

        # live query to ensure we get the latest data, even if user didn't update
        hashtbl <- get_golem_options("dbi")$query(
          paste0(
            "select hash from ",
            get_golem_options("schema"), ".", get_golem_options("table"), ";"
          )
        )

        if (df$hash %in% hashtbl$hash) {
          f7Dialog(
            id = ns("error"),
            title = "Duplicate Event Rejected!",
            text = "You (or your device) submitted an event which currently exists.",
            type = "alert"
          )
          log_warn("[{id}] append attempt blocked with hash {df$hash}")
        } else {
          get_golem_options("dbi")$append(get_golem_options("schema"), get_golem_options("table"), df)
          log_debug("[{id}] appended ", nrow(df), " rows to {get_golem_options('schema')}.{get_golem_options('table')}")
          f7Toast(text = "Event added successfully!", position = "bottom", closeButtonColor = "green")
        }

        # remove the active timer mode from the queue to "stop" the timer
        liteq::ack(try_consume(timerq))
        active_timer_mode("")
        active_timer_time(0)
        updateF7Button(
          inputId = mode,
          label = action_struct[[mode]][['start_label']],
          color = action_struct[[mode]][['start_color']]
        )
      }

      if (type == "new" || type == "swap") {
        # publish the timer type & status to the queue
        publish(timerq, title = button_pressed(), message = as.character(Sys.time()))
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
        walk = reactive(input$walk),
        out = reactive(input$out),
        sleep = reactive(input$sleep)
      )
    )

  })
}
