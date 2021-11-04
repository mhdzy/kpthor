#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom keyring key_get
#' @importFrom lubridate hour minute
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom RPostgres Postgres
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      pet = "kashi",
      dbi = dbInterface$new(
        drv = RPostgres::Postgres(),
        host = key_get("kpthor-app", "db-host"),
        port = as.numeric(key_get("kpthor-app", "db-port")),
        user = key_get("kpthor-app", "db-username"),
        pass = key_get("kpthor-app", "db-password"),
        db = key_get("kpthor-app", "db-database"),
      ),
      time_vars = list(
        hour = uvars(0L, 24L, 1L, hour, "gray"),
        minute = uvars(0L, 60L, 1L, minute, "gray")
      ),
      food_vars = list(
        food = uvars(0, 5, 0.5, 2.5, "teal"),
        water = uvars(0, 5, 0.5, 1.0, "lightblue")
      ),
      play_vars = list(
        play = uvars(0L, 60L, 5L, 30L, "purple"),
        walk = uvars(0L, 90L, 5L, 15L, "deeppurple")
      ),
      poop_vars = list(
        poop = uvars(0L, 3L, 1L, 1L, "deeporange"),
        pee = uvars(0L, 3L, 1L, 1L, "yellow")
      )
    )
  )
}
