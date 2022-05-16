#' predlist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_predlist_ui <- function(id) {
  ns <- NS(id)
  tagList(
    f7Row(
      f7Col(
        align = "center",
        uiOutput(ns("predlist"))
      )
    )
  )
}

#' predlist Server Functions
#'
#' @importFrom dplyr arrange desc
#' @importFrom golem get_golem_options
#' @importFrom logger log_trace
#' @importFrom magrittr %>%
#' @importFrom shiny moduleServer reactive isolate req
#' @importFrom shinyMobile f7List f7ListItem
#'
#' @noRd
#'
mod_predlist_server <- function(id, appdata, appdate, predictions) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sortedpreds <- reactive({
      req(predictions$predictions())
      predictions$predictions() %>% arrange(desc(time))
    })

    output$predlist <- renderUI({
      log_trace("[{id}] rendering predlist")
      if (!nrow(sortedpreds())) {
        return(
          f7List(
            inset = TRUE,
            mode = "media",
            f7ListItem(
              header = "",
              footer = "please check again later",
              "there are no upcoming events right now",
              media = NULL,
              right = NULL
            )
          )
        )
      }

      f7List(
        inset = TRUE,
        lapply(
          seq(nrow(sortedpreds())),
          function(x) {
            tmp <- sortedpreds()[x, ]

            f7ListItem(
              header = paste(get_golem_options("pet"), "needs to"),
              footer = astime(isolate(tmp$time)),
              # prefixes make the labels more natural: 'walk' -> 'go on a walk', etc.
              paste(get_golem_options("predPrefix")[[tmp$action]], tmp$action),
              media = f7Icon(get_golem_options("eventIcons")[[tmp$action]]),
              right = paste0(astime(isolate(tmp$min)), " - ", astime(isolate(tmp$max)))
            )
          }
        )
      )
    })

  })
}
