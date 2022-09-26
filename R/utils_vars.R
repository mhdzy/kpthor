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
#' @return A named list with 'active'/'inactive' shinyMobile::f7Button inputs.
#'
#' @noRd
avars <- function(name, start_label, end_label, start_color = "blue", end_color = "red") {
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

#' astime
#'
#' @description Returns a printable time representation of the input `t`
#'
#' @param t A double-valued representation of the 'hour'. Multiplied by 3600 to
#' convert to 'seconds', and is processed into "HH:MM tt" form ('5:30 pm').
#'
#' @return A string representing the formatted time `t` in 'hh:mm am/pm' form.
#'
#' @importFrom hms as_hms hms
#' @importFrom lubridate hour minute
#'
#' @noRd
astime <- function(t) {
  # converts a numeric hour 't' to a time representation
  if (t < 0 || t > 24) stop("'t' must be 0 <= t <= 24")

  # convert numeric hour to seconds
  t <- as_hms(hms(seconds = t * 3600))

  # assume hr is afternoon
  hr <- hour(t) %% 12
  sign <- "pm"

  # make correction
  if (hour(t) < 12) {
    hr <- hour(t)
    sign <- "am"
  } else if (hour(t) == 12) {
    hr <- hour(t)
    sign <- "pm"
  }

  # additional correction:
  # we want to display single digit minutes 't' as '12:0t'
  if (minute(t) < 10) {
    min <- paste0("0", minute(t))
  } else {
    min <- minute(t)
  }

  paste0(hr, ":", min, " ", sign)
}
