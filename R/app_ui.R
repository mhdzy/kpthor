#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @noRd
#'
#' @importFrom golem get_golem_options
#' @importFrom shiny tagList h2
#' @importFrom shinyMobile f7Page f7Navbar f7TabLayout f7Tabs f7Tab
#' @importFrom shinyMobile f7Icon f7DatePicker
app_ui <- function(request) {
  tagList(

    golem_add_external_resources(),

    f7Page(
      title = "kpthor",
      preloader = TRUE,
      allowPWA = FALSE,
      loading_duration = 2L,

      f7TabLayout(
        navbar = f7Navbar(
          title = mod_navbar_ui("navbar")
        ),

        f7Tabs(
          id = "f7_tabs",
          animated = FALSE,
          swipeable = TRUE,
          style = "toolbar",

          ## inputs ----
          f7Tab(
            tabName = "inputs",
            icon = f7Icon("calendar_badge_plus"),

            # static button inputs
            mod_appdate_row_ui("time_vars"),
            mod_button_action_ui("actions"),
            mod_button_input_ui("inputs"),
            mod_predlist_ui("input_preds"),

            # popup inputs
            mod_popup_box_ui("food_vars"),
            mod_popup_box_ui("play_vars"),
            mod_popup_box_ui("poop_vars")
          ),

          ## monitor ----
          f7Tab(
            tabName = "monitor",
            icon = f7Icon("graph_square"),

            mod_monitor_ui("monitor")
          ),


          ## home ----
          f7Tab(
            tabName = "home",
            active = TRUE,
            icon = f7Icon("house"),

            mod_home_ui("home")
          ),

          ## table ----
          f7Tab(
            tabName = "table",
            icon = f7Icon("table_badge_more"),

            mod_table_ui("table")
          ),

          f7Tab(
            tabName = "report",
            icon = f7Icon("square_list"),

            mod_report_ui("report")
          ),

          ## settings ----
          f7Tab(
            tabName = "settings",
            icon = f7Icon("gear"),

            mod_settings_ui("settings")
          )

        )
      ),

      ## options ----
      options = list(
        theme = c("ios"),
        dark = FALSE,
        filled = FALSE,
        color = "#a07aff",
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

    tags$link(rel = "icon", href = "favicon.ico"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "64x64", href = "ios/64x64.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "ios/32x32.png"),

    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'kpthor'
    ),
    useShinyjs()
    # shinyalert::useShinyalert()
  )
}

