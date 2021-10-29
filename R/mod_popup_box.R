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
        lapply(
          seq_along(get_golem_options(id)),
          function(x) {
            golem_opts <- get_golem_options(id)
            tagList(
              h4(names(golem_opts)[x]),
              f7Row(
                f7Col(
                  f7Stepper(
                    inputId = ns(names(golem_opts)[x]),
                    label = NULL,
                    min = golem_opts[[x]]$min,
                    max = golem_opts[[x]]$max,
                    step = golem_opts[[x]]$step,
                    value = golem_opts[[x]]$value,
                    color = golem_opts[[x]]$color
                  )
                ),
                f7Col(
                  f7Button(
                    inputId = ns(paste(names(golem_opts)[x], "submit", sep = "_")),
                    label = "submit",
                    size = "small",
                    color = golem_opts[[x]]$color,
                    fill = FALSE
                  )
                )
              )
            )
          }
        ),
        br(),
        br(),
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
#'
#' @importFrom golem get_golem_options
#' @importFrom logger log_trace log_debug
#' @importFrom shiny moduleServer is.reactive observeEvent
#' @importFrom shinyjs hide
#' @importFrom shinyMobile updateF7Sheet
mod_popup_box_server <- function(id, sheet_trigger, datetime) {
  stopifnot(is.reactive(sheet_trigger))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ## sheet trigger ----
    observeEvent(
      eventExpr = sheet_trigger(),
      handlerExpr = {
        updateF7Sheet("sheet")
        log_trace("[{id}] sheet triggered")
      }
    )

    ## cancel btn ----
    observeEvent(
      eventExpr = input$cancel,
      handlerExpr = {
        hide("sheet")
        log_trace("[{id}] sheet hidden by cancel")
      }
    )

    ## confirm btn ----
    observeEvent(
      eventExpr = input$confirm,
      handlerExpr = {
        dbi <- get_golem_options("dbi")
        nams <- names(get_golem_options(id))
        vals <- unlist(lapply(nams, function(x) input[[x]]))
        df <- data.frame(
          date = datetime$date(),
          time = datetime$hour(),
          minute = datetime$minute(),
          pet = get_golem_options("pet"),
          action = nams,
          value = vals
        )

        # update db
        dbi$append("kpthor", "events3", df)
        log_debug("[{id}] appended ", nrow(df), " rows to kpthor.events")

        hide("sheet")
        log_trace("[{id}] sheet hidden by confirm")
      }
    )

  })
}
