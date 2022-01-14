#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput column
#' @importFrom shinyMobile f7Button f7Col
mod_settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("user")),
    uiOutput(ns("pwd")),
    uiOutput(ns("pets")),
    uiOutput(ns("delete")),
    f7Row(
      f7Col(
        width = 10,
        f7Button(
          inputId = ns("confirm"),
          label = "confirm",
          color = "blue",
          fill = TRUE
        )
      )
    )
  )
}

#' settings Server Functions
#'
#' @importFrom dplyr filter select arrange desc
#' @importFrom golem get_golem_options
#' @importFrom logger log_trace log_warn
#' @importFrom lubridate date
#' @importFrom shiny moduleServer reactive renderUI observeEvent
#' @importFrom shinyMobile f7Picker f7SwipeoutItem f7Dialog f7Toast
#' @importFrom shinyMobile f7Text
#'
#' @noRd
#'
mod_settings_server <- function(id, appdata, appdate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    localdata <- reactive({
      appdata$data() %>%
        filter(
          lubridate::date(datetime) == appdate$date(),
          pet == get_golem_options("pet")
        ) %>%
        select(
          -c(id, pet)
        ) %>%
        arrange(
          desc(datetime),
        )
    })

    deletion_choices <- reactive({
      paste0(localdata()$action, " for ", localdata()$value, " at ", localdata()$time, ":", localdata()$minute)
    })

    output$user <- renderUI({
      log_trace("[{id}] render username box")
      f7Text(
        inputId = ns("username"),
        label = "your username",
        value = NULL,
        placeholder = "login to view username"
      )
    })

    output$pwd <- renderUI({
      log_trace("[{id}] render password box")
      f7Text(
        inputId = ns("password"),
        label = "change your password",
        value = NULL,
        placeholder = "x!nG3aefo=24y"
      )
    })

    output$pets <- renderUI({
      log_trace("[{id}] render pets box")
      f7Text(
        inputId = ns("petsowned"),
        label = "your pets",
        value = c("Kashi"),
        placeholder = "add some pets!"
      )
    })

    output$delete <- renderUI({
      log_trace("[{id}] render delete box")
      f7Picker(
        inputId = ns("delpicker"),
        label = "Delete Event",
        placeholder = "walk for 30 at 7:15",
        choices = deletion_choices()
      )
    })

    observeEvent(input$confirm, {
      log_trace("[{id}] render delete confirm button")
      f7Dialog(
        id = ns("delete_dialog"),
        title = "confirm",
        text = "please confirm deletion",
        type = "confirm"
      )
    })

    observeEvent(input$delete_dialog, {
      log_trace("[{id}] delete event confirmed, deleting")
      row_idx <- which(input$delpicker == deletion_choices())
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
        f7Toast(text = "Event deleted successfully!", position = "bottom", closeButtonColor = "green")
      } else {
        log_warn("[{id}] delete failed, warning")
        f7Toast(text = "Event deletion failed.", position = "bottom", closeButtonColor = "red")
      }
    })


  })
}
