#' vars
#'
#' @description Wraps the inputs in a named list, intended to be used to
#' specify a shinyMobile::f7Stepper input.
#'
#' @param min A min value.
#' @param max A max value.
#' @param step A step value (step <= max - min).
#' @param value A default value (min <= value <= max).
#' @param color A color.
#'
#' @return A named list with shinyMobile::f7Stepper inputs.
#'
#' @noRd
uvars <- function(min = 0L, max = 0L, step = 0L, value = 0L, color = "white") {
  return(
    list(
      min = min,
      max = max,
      step = step,
      value = value,
      color = color
    )
  )
}
