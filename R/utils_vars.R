#' uvars
#'
#' @description Wraps the inputs in a named list, intended to be used to
#' specify a shinyMobile::f7Stepper input.
#'
#' @param min A min value.
#' @param max A max value.
#' @param step A step value (step <= max - min).
#' @param value A default value (min <= value <= max).
#' @param color A color, default is 'white'.
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

#' avars
#'
#' @description Wraps the inputs in a named list, intended to be used to
#' specify a shinyMobile::f7Button input.
#'
#' @param name A name to use.
#' @param start_label A label to display when inactive (ready to start).
#' @param end_label A label to display when active (ready to end).
#' @param start_color A label to display when active (ready to end).
#' @param end_color A label to display when inactive (ready to start).
#'
#' @noRd
avars <- function(name, start_label, end_label, start_color = "red", end_color = "blue") {
  return(
    list(
      name = name,
      start_label = start_label,
      start_color = start_color,
      end_label = end_label,
      end_color = end_color
    )
  )
}

#' uapply
#'
#' @description Returns the result of unlist(lapply(...)) for convenience.
#'
#' @param ... Inputs to lapply()
#'
#' @noRd
uapply <- function(...) {
  return(unlist(lapply(...)))
}
