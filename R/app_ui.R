#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import shinyWidgets
#' @noRd
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

          mod_inputs_ui("inputs_ui_1"),
          mod_monitor_ui("monitor_ui_1"),
          mod_settings_ui("settings_ui_1")

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
          iosCenterTitle = TRUE,
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
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'kpthor'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

