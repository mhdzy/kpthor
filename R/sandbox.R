# data fetching
library(DBI)
library(logger)
library(odbc)
library(stringi)

# data wrangling
library(dplyr)
library(hms)
library(lubridate)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)

# plotting
library(ggplot2)
library(highcharter)
library(plotly)

source("R/class_dbInterface.R")

dbi = dbInterface$new(
  drv = odbc::odbc(),
  dsn = "KPthorSQL",
  schema = "kpthor",
  table = "testme"
)

dbi_insert = dbInterface$new(
  drv = odbc::odbc(),
  dsn = "KPthorSQL",
  schema = "kpthor",
  table = "allevents"
)

LOAD = TRUE
MIGRATE = TRUE
PUSH_TO_DB = FALSE
PREPDF = TRUE
HEATMAP = FALSE
CLUSTERING = FALSE
KDE = TRUE
PREDICT = TRUE

if (LOAD) {

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

  if (!MIGRATE) {
    df <-
      tibble::as_tibble(
        dbi_insert$query_self()
      ) |>
      dplyr::mutate(datetime = lubridate::with_tz(datetime, "EST5EDT"))
  } else {
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

    if (PUSH_TO_DB) {
      rows_deleted <- dbi_insert$execute("delete from kpthor.allevents;")
      dbi_insert$append_self(pushdf)
    }
  }
}

if (PREPDF) {
  dt <- df |>
    dplyr::mutate(
      datetime = lubridate::with_tz(datetime, "EST5EDT")
    ) |>
    dplyr::mutate(
      day = lubridate::date(datetime),
      time = hms::as_hms(datetime),
      hour = lubridate::hour(datetime)
    ) |>
    dplyr::select(
      datetime,
      day,
      time,
      hour,
      action,
      value
    ) |>
    dplyr::arrange(
      dplyr::desc(day),
      dplyr::desc(hour)
    )

  # see the time distribution of all events
    pDistro <- dt |>
      ggplot2::ggplot(aes(x = hms::as_hms(datetime), y = value, color = action)) +
      ggplot2::geom_point()
    plotly::ggplotly(pDistro)
}

if (HISTOGRAM) {
  # all histograms
  dt |>
    ggplot2::ggplot() +
    ggplot2::geom_histogram(ggplot2::aes(x = time), bins = 60) +
    ggplot2::facet_wrap(~action)
}

if (HEATMAP) {
  dat <- dt |>
    dplyr::group_by(day, action, hour) |>
    dplyr::summarise(freq = dplyr::n()) |>
    dplyr::arrange(
      dplyr::desc(day), dplyr::desc(hour)
    )

  RES_LAPPLY_DATA <- dat
  RES_LAPPLY_DATA <- dat |> dplyr::filter(action == "poop")

  dayrow <- function(day, data) {
    # data needs to have 'hour' and 'freq' columns
    hrs <- seq(24) - 1
    missing_hrs <- hrs[!(hrs %in% data$hour)]
    filled_day <- dplyr::bind_rows(
      data,
      tibble::tibble(
        day = day,
        hour = missing_hrs,
        freq = 0
      )
    ) |>
      dplyr::ungroup() |>
      dplyr::select(
        hour,
        freq
      ) |>
      dplyr::arrange(hour)

    if (nrow(filled_day) == 24) {
      complete_day <- matrix(filled_day$freq, nrow = 1, ncol = 24)
    } else {
      complete_day <- matrix(0, nrow = 1, ncol = 24)
    }

    return(complete_day)
  }

  res <- lapply(
    sort(unique(RES_LAPPLY_DATA$day)),
    function(d) {
      newdat <- RES_LAPPLY_DATA |>
        dplyr::filter(day == d) |>
        dplyr::group_by(hour) |>
        dplyr::summarise(freq = n())
      return(dayrow(d, newdat))
    })

  gdat <- t(matrix(unlist(res), nrow = 24))

  p <-
    plotly::plot_ly() |>
    plotly::add_surface(z = gdat)

  p
}

if (CLUSTERING) {
  require(Ckmeans.1d.dp)

  filt_action = 'walk'
  dt_filtered <- dt |>
    dplyr::filter(action == filt_action) |>
    dplyr::mutate(numtime = as.numeric(time)) |>
    dplyr::pull(time)

  x <- as.numeric(dt_filtered$time)
  # x <- dt_filtered$numtime[dt_filtered$numtime > 20000]

  # auto guess 'k' number of clusters
  result <- Ckmeans.1d.dp::Ckmeans.1d.dp(x)
  plot(result)

  # plot calculated clusters
  plot(x, col=result$cluster, pch=result$cluster, cex=1.5,
       main="Optimal univariate clustering given k",
       sub=paste("Number of clusters given:", k))
  abline(h=result$centers, col=1:k, lty="dashed", lwd=2)
  legend("bottomright", paste("Cluster", 1:k), col=1:k, pch=1:k, cex=1.5, bty="n")

}

if (KDE) {

  ACTION <- "walk"

  kde_dat <- dt |>
    filter(
      action == ACTION,
      value > 300
    ) |>
    mutate(
      time = as.numeric(time)/3600,
      value = value/60
    ) |>
    select(time, value)

  x <- kde_dat
  kde <- ks::kde(x = x)

  str(kde$eval.points)

  image(
    kde$eval.points[[1]],
    kde$eval.points[[2]],
    kde$estimate,
    col = viridis::viridis(20),
    xlab = "time (24-hour)",
    ylab = "duration (minutes)"
  )
  title("walk time vs. walk duration")
  points(kde$x)

  kdde_0 <- ks::kdde(x = x, deriv.order = 0)
  plot(kdde_0, display = "filled.contour2", xlab = "x", ylab = "y")

  kdde_1 <- ks::kdde(x = x, deriv.order = 1)
  plot(kdde_1, display = "quiver", xlab = "x", ylab = "y")

  for(i in 1:3) {
    plot(
      kdde_0,
      display = "filled.contour2",
      which.deriv.ind = i,
      xlab = "x",
      ylab = "y"
    )
  }

  actions <- c(
    "walk",
    "poop"
  )

  res <- lapply(
    actions,
    function(act) {
      dt |>
        filter(action == act) |>
        mutate(
          time = as.numeric(time)/3600,
          value = value/60
        ) |>
        pull(time)
    })

  tibble(
    res[[1]],
    res[[2]]
  )

}

if (PREDICT) {
  all_events <- unique(dt$action)

  event_clusters <- lapply(
    all_events,
    function(x) {
      browser()
      vec <- dt |>
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
  names(event_clusters) <- all_events

  flatten_clusters <- function(clusters) {
    create_event_struct <- function(type, mean, sd, exp) {
      list(
        'type' = type,
        'mean' = mean,
        'sd' = sd,
        'exp' = exp
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
          sd   = sd(cldata[cluster$cluster == i])/3600,
          exp  = NA_integer_
        )
      }

      return(event_structs)
    }

    events <- lapply(names(clusters), create_event)

    return(purrr::flatten(events))
  }

  res <- dplyr::bind_rows(flatten_clusters(event_clusters))



  plot(
    1:nrow(res),
    res$mean,
    pch=10,
    xlab="",
    ylab="",
    xaxt="n",
    xlim=c(0,nrow(res)),
    ylim=c(min(res$mean-res$sd, na.rm = TRUE),max(res$mean+res$sd, na.rm = TRUE))
  )
  lines(rbind(1:nrow(res),1:nrow(res),NA),rbind(res$mean-res$sd,res$mean+res$sd,NA))
  axis(side=1,at=1:nrow(res),labels=res$type)

  upcomingEvents <- 9
  nextEvents <-0
  prevEvents <-0
  missedEvents <-0

  # expected value can be derived from
}
