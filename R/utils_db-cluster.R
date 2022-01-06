#' dbCluster
#'
#' @description A utils function
#'
#' @param date A datetime object (preferrably from lubridate).
#' @param eventdf A timestamped data frame of actions & events.
#'
#' @return The computed clusters for each unique action. Number of centroids per
#' cluster group (event action name) is auto-detected by the algorithm.
#'
#' @importFrom Ckmeans.1d.dp Ckmeans.1d.dp
#' @importFrom dplyr bind_rows mutate filter pull
#' @importFrom hms as_hms
#' @importFrom lubridate with_tz date hour
#'
#' @noRd
dbCluster <- function(date, eventdf) {
  ev <- eventdf |>
    dplyr::mutate(
      datetime = lubridate::with_tz(datetime, "EST5EDT")
    ) |>
    dplyr::mutate(
      day = lubridate::date(datetime),
      time = hms::as_hms(datetime),
      hour = lubridate::hour(datetime)
    )

  all_events <- unique(ev$action)
  all_clusters <- lapply(
    all_events,
    function(x) {
      vec <- ev |>
        dplyr::filter(action == x) |>
        dplyr::mutate(time = as.numeric(time)) |>
        dplyr::pull(time)

      return(
        list(
          data = vec,
          clust = Ckmeans.1d.dp::Ckmeans.1d.dp(vec)
        )
      )
    })
  names(all_clusters) <- all_events

  cl <- dplyr::bind_rows(createClusterEvents(all_clusters))

  return(
    list(
      eventdf = ev,
      clusterdf = cl
    )
  )
}

#' createClusterEvents
#'
#' @description
#' Given a list of (named by category) clusters, this function will decompose
#' each cluster to a type, mean, and sd. These are derived from the raw cluster
#' objects, itemized within the `clusters` parameter.
#'
#' @param `clusters` A list of cluster objects from Ckmeans.1d.1p
#'
#' @importFrom purrr flatten
#'
#' @noRd
createClusterEvents <- function(clusters) {
  create_event_struct <- function(type, mean, sd) {
    list(
      'type' = type,
      'mean' = mean,
      'sd' = sd
    )
  }

  create_event <- function(clustername) {
    cldata <- clusters[[clustername]]$data
    cluster <- clusters[[clustername]]$clust
    n_clusters <- length(cluster$centers)
    event_structs <- vector("list", n_clusters)

    for (i in seq(n_clusters)) {
      event_structs[[i]] <- create_event_struct(
        type = clustername,
        mean = cluster$centers[i]/3600,
        sd   = ifelse(
          test = !(length(cldata[cluster$cluster == i]) - 1),
          # if only 1 value in cluster, sd = 0 forces tight windows for
          # event prediction, which is desired
          yes = 0,
          no = sd(cldata[cluster$cluster == i])/3600
        )
      )
    }

    return(event_structs)
  }

  events <- lapply(names(clusters), create_event)

  return(purrr::flatten(events))
}

#' filterEvents
#'
#' @description
#' Filters a full event df to today's events, summing values by a
#' date/action group.
#'
#' @importFrom dplyr filter mutate group_by summarise select
#' @importFrom hms as_hms
#' @importFrom lubridate date
#'
#' @noRd
#'
filterEvents <- function(date, eventdf) {
  # need a few representations of the day/time for compatibility
  today <- lubridate::date(date)
  now_min <- hms::as_hms(date)
  now_hr <- as.numeric(hms::as_hms(date))/3600 # in hours

  return(
    eventdf |>
      dplyr::filter(
        day == today
      ) |>
      dplyr::mutate(
        time = as.numeric(time)/3600 # in hours
      ) |>
      dplyr::group_by(
        time, action
      ) |>
      dplyr::summarise(
        value = sum(value),
        .groups = "drop"
      ) |>
      dplyr::select(
        time,
        action,
        value
      )
  )
}

#' candidateClusters
#'
#' @description
#'
#' @param clusterdf A data frame of cluster info, containing the `type`,
#' `mean` and `sd` columns.
#'
#' @importFrom dplyr mutate rename
#'
#' @noRd
#'
candidateClusters <- function(clusterdf) {
  return(
    clusterdf |>
      dplyr::mutate(
        max = mean+1*sd,
        min = mean-1*sd,
      ) |>
      dplyr::rename(
        time = mean,
        action = type,
      )
  )
}

#' filterCandidateClusters
#'
#' @description
#' Returns clusters which are voided by an existing event.
#'
#' @param events A data.frame with 'action' and 'time' columns.
#' @param clusters A data.frame with
#'
#' @importFrom dplyr between bind_rows
#' @importFrom purrr flatten
#'
#' @noRd
#'
filterCandidateClusters <- function(events, clusters) {
  if (!nrow(events)) return(events)
  if (!nrow(clusters)) return(clusters)

  return(
    dplyr::bind_rows(
      purrr::flatten(
        lapply(
          seq(nrow(events)),
          function(x) {
            e <- events[x, ]
            lapply(
              seq(nrow(clusters)),
              function(y) {
                c <- clusters[y, ]
                if (!nrow(e)) {
                  # no events, return all clusters
                  return(c)
                } else if (!nrow(c)) {
                  # if no clusters, nothing to return
                  return ()
                } else if (e$action == c$action && dplyr::between(e$time, c$min, c$max)) {
                  return(c)
                } else {
                  return()
                }
              }
            )
          }
        )
      )
    )
  )
}

#' upcomingEvents
#'
#' @description
#'
#' @param date A datetime.
#' @param eventdf A data frame of events with dates, times, names, and values.
#' @param clusterdf A data frame of clusters, names, means, and std. deviations.
#'
#' @importFrom dplyr filter
#'
#' @noRd
upcomingEvents <- function(date, eventdf, clusterdf) {
  now_hr <- as.numeric(hms::as_hms(date))/3600 # in hours

  ev <- filterEvents(date, eventdf) |>
    dplyr::filter(
      time < now_hr
    )

  ca <- candidateClusters(clusterdf) |>
    dplyr::filter(
      now_hr > min,
      now_hr < max
    )

  re <- filterCandidateClusters(ev, ca)

  # return all CAndidate events when there are no rows to REmove
  if (!nrow(re))
    return(ca)

  return(setdiff(ca, re))
}

#' nextEvents
#'
#' @description
#'
#' @param date A datetime.
#' @param eventdf A data frame of events with dates, times, names, and values.
#' @param clusterdf A data frame of clusters, names, means, and std. deviations.
#'
#' @noRd
nextEvents <- function(date, eventdf, clusterdf) {
  ca <- candidateClusters(clusterdf)
  # min(time) per cluster for all clusters who do
  # not have events within their range
  #
  # TODO
}

#' prevEvents
#'
#' @description
#'
#' @param date A datetime.
#' @param eventdf A data frame of events with dates, times, names, and values.
#'
#' @importFrom hms as_hms
#' @importFrom dplyr filter mutate group_by summarise
#'
#' @noRd
prevEvents <- function(date, eventdf) {
  now_hr <- as.numeric(hms::as_hms(date))/3600 # in hours

  return(
    eventdf |>
    dplyr::filter(
      day == today,
      time < now_hr
    ) |>
    dplyr::mutate(
      time = as.numeric(time)/3600 # in hours
    ) |>
    dplyr::group_by(action) |>
    dplyr::summarise(time = max(time))
  )
}

#' missedEvents
#'
#' @description
#'
#' @param date A datetime.
#' @param eventdf A data frame of events with dates, times, names, and values.
#' @param clusterdf A data frame of clusters, names, means, and std. deviations.
#'
#' @noRd
missedEvents <- function(date, eventdf, clusterdf) {
  # time per cluster for all clusters < time
  # who do not have events within 2/3 sigma
  #
  # TODO
}
