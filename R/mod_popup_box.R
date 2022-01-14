#' popup_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom golem get_golem_options
#' @importFrom shiny NS tagList br h4
#' @importFrom shinyMobile f7Sheet f7Row f7Col f7Stepper f7Button
mod_popup_box_ui <- function(id) {
  ns <- NS(id)

  tagList(
    f7Sheet(
      id = ns("sheet"),
      label = paste0(names(get_golem_options(id)), collapse = "/"),
      orientation = "bottom",
      swipeToClose = TRUE,
      swipeToStep = FALSE,
      closeByOutsideClick = TRUE,
      backdrop = FALSE,
      tagList(
        uiOutput(ns("progress")),
        uiOutput(ns("buttons")),
        br(),
        br(),
        f7Row(
          style = "margin: 20px",
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
#'
#' @importFrom digest digest
#' @importFrom dplyr if_else mutate
#' @importFrom golem get_golem_options
#' @importFrom logger log_debug log_trace log_warn
#' @importFrom magrittr %>%
#' @importFrom purrr map_chr
#' @importFrom shiny is.reactive moduleServer observeEvent tagList
#' @importFrom shiny reactiveValues reactiveValuesToList renderUI
#' @importFrom shinyjs hide
#' @importFrom shinyMobile f7Button f7Block f7Col f7Dialog f7Row f7Stepper
#' @importFrom shinyMobile f7Progress updateF7Progress updateF7Sheet f7Toast
#'
mod_popup_box_server <- function(id, sheet_trigger, appdata, appdate) {
  stopifnot(is.reactive(sheet_trigger))
  stopifnot(is.list(appdate))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    obj <- get_golem_options(id)
    nams <- names(obj)

    # 'pre-stage' values used to store submissions before confirmation
    pstg <- reactiveValues()
    pstg_reset <- function() {
      log_trace("[{id}] pstg_reset called")
      lapply(nams, function(x) {
        log_trace("[{id}] {x} resetting")
        updateF7Progress(id = ns(paste0(x, "_progress")), value = 0)
        pstg[[x]] <<- 0
      })
    }
    pstg_reset()

    ## o$ progress ----
    output$progress <- renderUI({
      log_trace("[{id}] progress bars rendered")
      tagList(
        lapply(nams, function(x) {
          f7Block(
            tags$style("height: 4px"),
            f7Progress(
              id = ns(paste0(x, "_progress")),
              value = 0,
              color = obj[[x]][['color']]
            )
          )
        })
      )
    })

    ## o$ buttons ----
    output$buttons <- renderUI({
      log_trace("[{id}] buttons rendered")
      lapply(seq_along(obj), function(x) {
        tagList(
          h4(nams[x]),
          f7Row(
            f7Col(
              f7Stepper(
                inputId = ns(nams[x]),
                label = NULL,
                min = obj[[x]]$min,
                max = obj[[x]]$max,
                step = obj[[x]]$step,
                value = obj[[x]]$value,
                color = obj[[x]]$color
              )
            ),
            f7Col(
              f7Button(
                inputId = ns(paste(nams[x], "submit", sep = "_")),
                label = "submit",
                size = "small",
                color = obj[[x]]$color,
                fill = FALSE
              )
            )
          )
        )
      })
    })

    ## oE sheet trigger ----
    observeEvent(sheet_trigger(), {
      # this will toggle on the input sheet
      log_trace("[{id}] sheet (update) triggered")
      updateF7Sheet("sheet")
    }
    )

    ## oE submit btn ----
    # create observers for pstg submissions via module id's
    lapply(nams, function(x) {
      # observes the submit (but not confirm) buttons; updates pstg with the raw
      # input value, and the progress bar with a value 0-100 as a percent of the
      # max allowed input value for a given measured input:
      #   pstg[x] = input_val/max_val * 100
      observeEvent(input[[paste0(x, "_submit")]], {
        pstg[[x]] <- input[[x]]
        updateF7Progress(
          id = ns(paste0(x, "_progress")),
          value = pstg[[x]]*100/obj[[x]][['max']]
        )
        log_trace("[{id}] progress bar for {x} updated to {pstg[[x]]}")
      })
    })

    ## oE confirm btn ----
    observeEvent(input$confirm, {
      log_trace("[{id}] confirm button press detected")
      # grab pstg (pre-stage) values, which were submitted by user pre-confirm
      vals <- uapply(nams, function(x) pstg[[x]])
      idx <- which(vals != 0)

      if (length(idx)) {
        min_to_sec <- c("play", "walk")

        df <-
          data.frame(
            pet = get_golem_options("pet"),
            datetime = lubridate::with_tz(lubridate::force_tz(
              lubridate::ymd_hms(paste0(appdate$date(), " ", appdate$hour(), ":", appdate$minute(), ":", "00")),
              "EST5EDT"), "UTC"),
            action = nams[idx],
            value = vals[idx]
          ) %>%
          dplyr::mutate(
            # TODO: ensure units of 'seconds' for time-based inputs
            value = dplyr::if_else(action %in% min_to_sec, value * 60, value),
            hash = purrr::map_chr(paste0(pet, datetime, action, value), digest::digest, algo = "sha256")
          )

        # live query to ensure we get the latest data, even if user didn't update
        hashtbl <- get_golem_options("dbi")$query(
          paste0(
            "select hash from ",
            get_golem_options("schema"), ".", get_golem_options("table"), ";"
          )
        )

        # need to check 'any()' because we may have more than 1 event being added
        if (any(df$hash %in% hashtbl$hash)) {
          f7Dialog(
            id = ns("error"),
            title = "Duplicate Event Rejected!",
            text = "You (or your device) submitted an event which currently exists.",
            type = "alert"
          )
          log_warn("[{id}] append attempt blocked with hash {df$hash}")
        } else {
          get_golem_options("dbi")$append(get_golem_options("schema"), get_golem_options("table"), df)
          log_debug("[{id}] appended {nrow(df)} rows to {get_golem_options('schema')}.{get_golem_options('table')}")
          f7Toast(text = "Event added successfully!", position = "bottom", closeButtonColor = "green")
        }
      } else {
        # idx will have length 0 when all vals are == 0; inform user
        f7Dialog(
          id = ns("warn"),
          title = "No Events Added!",
          text = "You didn't submit any events before confirming, so nothing was recorded.",
          type = "alert"
        )
      }

      hide("sheet")
      log_trace("[{id}] sheet hidden by confirm")

      # reset after hide so user doesn't see
      pstg_reset()
      log_trace("[{id}] pstg zeroed out")
    })

    ## oE cancel btn ----
    observeEvent(input$cancel, {
      hide("sheet")
      log_trace("[{id}] sheet hidden by cancel")

      # reset after hide so user doesn't see
      pstg_reset()
      log_trace("[{id}] pstg zeroed out")
    }
    )

  })
}
