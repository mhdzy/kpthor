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
    uiOutput(ns("modify")),
    uiOutput(ns("delete"))
  )
}

#' settings Server Functions
#'
#' @importFrom dplyr filter select arrange desc
#' @importFrom golem get_golem_options
#' @importFrom logger log_trace log_warn
#' @importFrom lubridate date
#' @importFrom shiny moduleServer reactive renderUI observeEvent req
#' @importFrom shinyMobile f7Picker f7SwipeoutItem f7Dialog f7Toast
#' @importFrom shinyMobile f7Text f7Row f7Col f7Button updateF7Text
#'
#' @noRd
#'
mod_settings_server <- function(id, appdata, appdate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    localdata <- reactive({
      req(appdata$data())
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

    event_choices <- reactive({
      if (!nrow(localdata())) return("")
      paste0(
        localdata()$action,
        " for ", localdata()$value,
        " at ", uapply(as.numeric(as_hms(localdata()$datetime))/3600, astime)
      )
    })

    mod_idx <- reactive({ which(input$modify_event == event_choices()) })
    del_idx <- reactive({ which(input$delete_event == event_choices()) })

    mod_data <- reactive({ localdata()[mod_idx(), ] })
    del_data <- reactive({ localdata()[del_idx(), ] })

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

    output$modify <- renderUI({
      log_trace("[{id}] render modify box")
      tagList(
        f7Row(
          f7Col(
            width = 8,
            f7Picker(
              inputId = ns("modify_event"),
              label = "Modify Event",
              placeholder = "event for 0 at 00:00",
              choices = event_choices()
            )
          ),
          f7Col(
            width = 4,
            f7Text(
              inputId = ns("modify_value"),
              label = HTML("<b>Modify Value</b>"),
              value = 0L,
              placeholder = "new value"
            )
          )
        ),
        f7Row(
          f7Col(
            width = 10,
            f7Button(
              inputId = ns("modify_confirm"),
              label = "confirm",
              color = "blue",
              fill = TRUE
            )
          )
        )
      )
    })

    observeEvent(input$modify_event, {
      log_trace("[{id}] modified event changed; updating modify value")
      updateF7Text(
        inputId = "modify_value",
        value = mod_data()$value
      )
    })

    observeEvent(input$modify_confirm, {
      log_trace("[{id}] render modify confirm button")
      f7Dialog(
        id = ns("modify_dialog"),
        title = "confirm",
        text = "please confirm modification",
        type = "confirm"
      )
    })

    observeEvent(input$modify_dialog, {
      log_trace("[{id}] modify event confirmed, modifying")
      if (input$modify_dialog) {
        if (!is.na(as.numeric(input$modify_value))) {
          mod_conf <- get_golem_options("dbi")$execute(
            paste0(
              "update ", get_golem_options("schema"), ".", get_golem_options("table"),
              " set value=", as.numeric(input$modify_value),
              " where hash='", mod_data()$hash, "';"
            )
          )
        } else {
          mod_conf <- FALSE
        }

        if (mod_conf) {
          log_trace("[{id}] modify event confirmed, modifying")
          f7Toast(text = "Event modified successfully!", position = "bottom", closeButtonColor = "green")
        } else {
          log_warn("[{id}] modify failed, warning")
          f7Toast(text = "Event modification failed.", position = "bottom", closeButtonColor = "red")
        }
      } else {
        log_trace("[{id}] modify cancelled")
        f7Toast(text = "Event modification cancelled.", position = "bottom", closeButtonColor = "yellow")
      }
    })

    output$delete <- renderUI({
      log_trace("[{id}] render delete box")
      tagList(
        f7Row(
          f7Col(
            width = 10,
            f7Picker(
              inputId = ns("delete_event"),
              label = "Delete Event",
              placeholder = "event for 0 at 00:00",
              choices = event_choices()
            )
          )
        ),
        f7Row(
          f7Col(
            width = 10,
            f7Button(
              inputId = ns("delete_confirm"),
              label = "confirm",
              color = "blue",
              fill = TRUE
            )
          )
        )
      )
    })

    observeEvent(input$delete_confirm, {
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
      if (input$delete_dialog) {
        del_conf <- get_golem_options("dbi")$execute(
          paste0(
            "delete from ", get_golem_options("schema"), ".", get_golem_options("table"),
            " where hash='", del_data()$hash, "';"
          )
        )
        if (del_conf) {
          log_trace("[{id}] delete confirmed, deleting")
          f7Toast(text = "Event deleted successfully!", position = "bottom", closeButtonColor = "green")
        } else {
          log_warn("[{id}] delete failed, warning")
          f7Toast(text = "Event deletion failed.", position = "bottom", closeButtonColor = "red")
        }
      } else {
        log_trace("[{id}] delete cancelled")
        f7Toast(text = "Event deletion cancelled.", position = "bottom", closeButtonColor = "yellow")
      }
    })

  })
}
