#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
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
        host = "192.168.0.111",
        port = 5432,
        user = "pi",
        pass = "blueberry",
        db = "apps"
      ),
      food_vars = list(
        food = uvars(0, 5, 0.5, 2.5, "teal"),
        water = uvars(0, 5, 0.5, 1.0, "lightblue")
      ),
      play_vars = list(
        play = uvars(0L, 60L, 5L, 30L, "deeppurple"),
        walk = uvars(0L, 90L, 5L, 15L, "orange")
      ),
      poop_vars = list(
        poop = uvars(0L, 3L, 1L, 1L, "deeporange"),
        pee = uvars(0L, 3L, 1L, 1L, "yellow")
      )
    )
  )
}
