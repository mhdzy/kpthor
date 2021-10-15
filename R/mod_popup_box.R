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
  ## food sheet ----
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
            tagList(
              h4(names(get_golem_options(id))[x]),
              f7Row(
                f7Col(
                  f7Stepper(
                    inputId = ns(names(get_golem_options(id))[x]),
                    label = NULL,
                    min = get_golem_options(id)[[x]]$min,
                    max = get_golem_options(id)[[x]]$max,
                    step = get_golem_options(id)[[x]]$step,
                    value = get_golem_options(id)[[x]]$value,
                    color = get_golem_options(id)[[x]]$color
                  )
                ),
                f7Col(
                  f7Button(
                    inputId = ns(paste(names(get_golem_options(id))[x], "submit", sep = "_")),
                    label = "submit",
                    size = "small",
                    color = get_golem_options(id)[[x]]$color,
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
#' @importFrom shiny moduleServer is.reactive observeEvent
#' @importFrom shinyjs hide
#' @importFrom shinyMobile updateF7Sheet
mod_popup_box_server <- function(id, sheet_trigger) {
  stopifnot(is.reactive(sheet_trigger))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(
      eventExpr = sheet_trigger(),
      handlerExpr = {
        updateF7Sheet("sheet")
      }
    )

    observeEvent(
      eventExpr = input$cancel,
      handlerExpr = {
        shinyjs::hide("sheet")
      }
    )

    observeEvent(
      eventExpr = input$confirm,
      handlerExpr = {
        dbi <- get_golem_options("dbi")
        nams <- names(get_golem_options(id))
        vals <- unlist(lapply(nams, function(x) input[[x]]))
        df <- data.frame(
          timestamp = Sys.time(),
          pet = get_golem_options("pet"),
          action = nams,
          value = vals
        )
        print(df)
        dbi$append("kpthor", "events", df)
        logger::log_debug("appended ", nrow(df), " rows to kpthor.events")
      }
    )

  })
}

## To be copied in the UI
# mod_popup_box_ui("popup_box_ui_1")

## To be copied in the server
# mod_popup_box_server("popup_box_ui_1")
