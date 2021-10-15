#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @noRd
#'
#' @importFrom golem get_golem_options
#' @importFrom shiny tagList h2
#' @importFrom shinyMobile f7Page f7TabLayout f7Navbar f7Tabs f7Tab f7Icon f7DatePicker
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    f7Page(
      title = "kpthor",

      f7TabLayout(
        navbar = f7Navbar(
          title = h2("Good Morning, Kashi.")
        ),

        f7Tabs(
          id = "f7_tabs",

          f7Tab(
            tabName = "inputs",
            icon = f7Icon("calendar_badge_plus"),
            active = TRUE,

            ## date picker ----
            f7DatePicker(
              inputId = "date",
              label = "today's date",
              value = Sys.Date(),
              multiple = FALSE,
              dateFormat = "mm/dd/yyyy",
              closeByOutsideClick = TRUE
            ),

            ## button row ----
            mod_button_row_ui("buttons"),
            mod_popup_box_ui("food_vars"),
            mod_popup_box_ui("play_vars"),
            mod_popup_box_ui("poop_vars")
          ),

          mod_monitor_ui("monitor"),
          mod_settings_ui("settings")

        )
      ),

      options = list(
        theme = c("ios"),
        dark = TRUE,
        filled = FALSE,
        color = "#007aff",
        touch = list(
          tapHold = TRUE,
          tapHoldDelay = 750,
          iosTouchRipple = FALSE
        ),
        iosTranslucentBars = TRUE,
        navbar = list(
          iosCenterTitle = FALSE,
          hideNavOnPageScroll = TRUE
        ),
        toolbar = list(
          hideNavOnPageScroll = FALSE
        ),
        pullToRefresh = FALSE
      )

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
#'
#' @importFrom shinyjs useShinyjs
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
golem_add_external_resources <- function() {

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'kpthor'
    ),
    useShinyjs()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

