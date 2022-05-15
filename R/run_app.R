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
      pet = "Kashi",
      schema = "kpthor",
      table = "allevents",
      dbi = dbInterface$new(
        # drv defined in /etc/odbcinst.ini
        drv = odbc::odbc(),
        # dsn defined in /etc/odbc.ini
        dsn = get_golem_config("app_dsn")
      ),
      timerq = ensure_queue("timerq", db = get_golem_config("app_timer_db")),
      time_vars = list(
        hour   = uvars(0L, 24L, 1L, lubridate::hour,   "gray"),
        minute = uvars(0L, 60L, 5L, lubridate::minute, "gray")
      ),
      actions = list(
        walk  = avars("walk",  "start walk",  "end walk"),
        out   = avars("out",   "go outside",  "come inside"),
        sleep = avars("sleep", "go to sleep", "wake up")
      ),
      inputs = list(
        food = c("food", "food", "green"),
        play = c("play", "play", "teal"),
        poop = c("poop", "poop", "deeporange")
      ),
      eventPrefix = list(
        "food" = "ate",
        "out" = "went",
        "pee" = "went",
        "play" = "went out to",
        "poop" = "went",
        "sleep" = "went to",
        "walk" = "went on a",
        "water" = "drank"
      ),
      predPrefix = list(
        "food" = "eat",
        "out" = "go",
        "pee" = "go",
        "play" = "go",
        "poop" = "go",
        "sleep" = "go to",
        "walk" = "go on a",
        "water" = "drink"
      ),
      eventIcons = list(
        "food" = "poultry_leg",
        "out" = "cloud_sun",
        "pee" = "umbrella",
        "play" = "paw",
        "poop" = "recordingtape",
        "sleep" = "zzz",
        "walk" = "dog",
        "water" = "drop"
      ),
      food_vars = list(
        food  = uvars(0L, 3L, 0.5, 1.5, "teal"),
        water = uvars(0L, 5L, 0.5, 1.0, "lightblue")
      ),
      play_vars = list(
        play = uvars(0L, 60L, 5L, 30L, "purple"),
        walk = uvars(0L, 90L, 5L, 15L, "deeppurple")
      ),
      poop_vars = list(
        poop = uvars(0L, 3L, 1L, 1L, "deeporange"),
        pee  = uvars(0L, 3L, 1L, 1L, "yellow")
      )
    )
  )
}
