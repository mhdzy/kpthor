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

    golem_add_external_resources(),

    f7Page(
      title = "kpthor",

      f7TabLayout(
        navbar = f7Navbar(
          title = mod_navbar_ui("navbar")
        ),

        f7Tabs(
          id = "f7_tabs",

          ## inputs ----
          f7Tab(
            tabName = "inputs",
            icon = f7Icon("calendar_badge_plus"),
            active = TRUE,

            # static button inputs
            mod_datetime_row_ui("time_vars"),
            mod_button_action_ui("actions"),
            br(), mod_button_input_ui("inputs"),

            # popup inputs
            mod_popup_box_ui("food_vars"),
            mod_popup_box_ui("play_vars"),
            mod_popup_box_ui("poop_vars")
          ),

          ## monitor ----
          f7Tab(
            tabName = "monitor",
            icon = f7Icon("graph_square"),

            mod_monitor_ui("monitor"),
            mod_table_ui("table")
          ),

          ## settings ----
          f7Tab(
            tabName = "settings",
            icon = f7Icon("gear"),

            mod_settings_ui("settings")
          )

        )
      ),

      options = list(
        theme = c("ios"),
        dark = TRUE,
        filled = FALSE,
        color = "#007aff",
        touch = list(tapHold = TRUE, tapHoldDelay = 750, iosTouchRipple = FALSE),
        navbar = list(iosCenterTitle = FALSE, hideNavOnPageScroll = TRUE),
        toolbar = list(hideNavOnPageScroll = FALSE),
        iosTranslucentBars = TRUE,
        pullToRefresh = TRUE
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
    # shinyalert::useShinyalert()
  )
}

