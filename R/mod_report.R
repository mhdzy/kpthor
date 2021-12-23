#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("swipe")),
    column(
      width = 10,
      f7Button(
        inputId = ns("confirm"),
        label = "confirm",
        color = "blue",
        fill = TRUE
      )
    )
  )
}

#' report Server Functions
#'
#' @importFrom dplyr filter select arrange desc
#' @importFrom golem get_golem_options
#' @importFrom logger log_trace log_warn
#' @importFrom shiny moduleServer reactive renderUI observeEvent
#' @importFrom shinyMobile f7Picker f7SwipeoutItem f7Dialog
#'
#' @noRd
mod_report_server <- function(id, appdata, datetime) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      print(input$delete)
      print(input$modify)
    })

    localdata <- reactive({
      appdata$data() %>%
        filter(
          date == datetime$date(),
          pet == get_golem_options("pet")
        ) %>%
        select(
          -c(pet)
        ) %>%
        arrange(
          desc(time),
          desc(minute)
        )
    })

    deletion_choices <- reactive({
      paste0(localdata()$action, " for ", localdata()$value, " at ", localdata()$time, ":", localdata()$minute)
    })

    #browser()
    output$swipe <- renderUI({
      f7Picker(
        inputId = ns("picker"),
        label = "pick event to delete",
        placeholder = "walk for 30 at 7:15",
        choices = deletion_choices()
      )
    })

    observeEvent(input$confirm, {
      f7Dialog(
        id = ns("delete_dialog"),
        title = "confirm",
        text = "please confirm deletion",
        type = "confirm"
      )
    })

    observeEvent(input$delete_dialog, {
      row_idx <- which(input$picker == deletion_choices())
      del_hash <- localdata()[row_idx, ]$hash
      del_conf <- get_golem_options("dbi")$execute(
        paste0(
          "delete from ",
          get_golem_options("schema"), ".", get_golem_options("table"),
          " where hash='", del_hash, "';"
        )
      )
      if (del_conf) {
        log_trace("[{id}] delete confirmed, deleting")
      } else {
        log_warn("[{id}] delete failed, warning")
      }
    })


  })
}
