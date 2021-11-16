#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom golem with_golem_options
#' @importFrom liteq ensure_queue
#' @importFrom lubridate hour minute
#' @importFrom odbc odbc
#' @importFrom shiny shinyApp
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
        # drv defined in /etc/odbcinst.ini
        drv = odbc::odbc(),
        # dsn defined in /etc/odbc.ini
        dsn = "KPthorSQL"
      ),
      timerq = ensure_queue("timerq", db = "db/timerq"),
      time_vars = list(
        hour = uvars(0L, 24L, 1L, lubridate::hour, "gray"),
        minute = uvars(0L, 60L, 1L, lubridate::minute, "gray")
      ),
      actions = list(
        walk = avars("walk", "start walk", "end walk"),
        out = avars("out", "go outside", "come inside"),
        sleep = avars("sleep", "go to sleep", "wake up")
      ),
      inputs = list(
        food = c("food", "food", "green"),
        play = c("play", "play", "teal"),
        poop = c("poop", "poop", "deeporange")
      ),
      food_vars = list(
        food = uvars(0, 3, 0.5, 1.5, "teal"),
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
