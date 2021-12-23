#' declare_global_variables
#'
#' @description resolves devtools::check() warnings for the following message:
#' \code{
#'  > checking R code for possible problems ... NOTE
#'  function : <anonymous>: no visible binding for global variable
#'  ‘.’
#'  function : <anonymous>: no visible binding for global variable
#'  ‘.data’
#'  function : <anonymous>: no visible binding for global variable
#'  ‘time’
#'  Undefined global functions or variables:
#'    . .data time
#'  Consider adding
#'  importFrom("stats", "time")
#'  to your NAMESPACE file.
#' }
#'
#' The solution can be generated via the `checkhelper` package from
#'   https://github.com/ThinkR-open/checkhelper
#'
#' \code{
#'  # remotes::install_github("thinkr-open/checkhelper")
#'  # library(checkhelper)
#'
#'  # full report
#'  report <- checkhelper::find_missing_tags()
#'
#'  # missing global bindings
#'  globals <- checkhelper::get_no_visible(".", quiet = TRUE)
#'  checkhelper::print_globals(globals)
#' }
#'
#' @noRd
#'
#' @importFrom utils globalVariables
declare_global_variables <- function() {
  globalVariables(
    unique(
      c(
        # mod_table_server : <anonymous>:
        "pet",
        "time",
        "action",
        "hash",
        "total",
        "value"
      )
    )
  )
}

declare_global_variables()
