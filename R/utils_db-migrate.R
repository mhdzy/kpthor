#' dbFetch
#'
#' replace this function with a proper get_golem_opts()
#'
#' @noRd
dbFetch <- function() {
  dbi <- dbInterface$new(
    drv = odbc::odbc(),
    dsn = "KPthorSQL",
    schema = "kpthor",
    table = "testme"
  )

  return(
    tibble::as_tibble(
      dbi$query_self()
    )
  )
}

#' dbMigrate
#'
#' @description A utils function
#'
#' @param migrate logical value
#' @param push_to_db logical value
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
dbMigrate <- function(df = NULL, migrate = TRUE, push_to_db = FALSE) {
  dbi <- dbInterface$new(
    drv = odbc::odbc(),
    dsn = "KPthorSQL",
    schema = "kpthor",
    table = "testme"
  )

  dbi_insert <- dbInterface$new(
    drv = odbc::odbc(),
    dsn = "KPthorSQL",
    schema = "kpthor",
    table = "allevents"
  )

  # unit conversion columns (not always needed)
  min_to_sec <- c(
    "play",
    "walk" # for values smaller than 120
  )

  sec_to_min <- c(
    "out",
    "play",
    "walk", # for values above 120
    "sleep"
  )

  if (is.null(df) && migrate) {
    df <-
      tibble::as_tibble(
        dbi$query_self()
      ) |>
      dplyr::mutate(
        datetime = lubridate::force_tz(
          lubridate::ymd_hms(
            paste0(date, " ", time, ":", minute, ":", "00")
          ),
          "EST5EDT"),
        pet = "Kashi",
        value = as.numeric(value)
      ) |>
      dplyr::mutate(
        hash = purrr::map_chr(
          paste0(datetime, pet, action, value), digest::digest, algo = "sha256"
        ),
        value =
          dplyr::if_else(
            action %in% min_to_sec & value <= 90,
            value*60,
            value
          ),
        datetime = dplyr::if_else(
          hms::as_hms(datetime) < hms::as_hms("06:30:00"),
          datetime + lubridate::hours(12),
          datetime
        )
      ) |>
      dplyr::mutate(
        datetime = lubridate::with_tz(datetime, "UTC")
      ) |>
      dplyr::select(
        hash,
        datetime,
        pet,
        action,
        value
      )
  }

  if (push_to_db) {
    rows_deleted <- dbi_insert$execute("delete from kpthor.allevents;")
    dbi_insert$append_self(df)
  }

  return(df)
}
