if (FALSE) {
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
  PLOTS_GENERAL = FALSE
  HISTOGRAM = FALSE
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
    if (PLOTS_GENERAL) {
      (dt |>
         ggplot2::ggplot(aes(x = hms::as_hms(datetime), y = value, color = action)) +
         ggplot2::geom_point()
      ) |>
        plotly::ggplotly(pDistro)
    }
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

    # kdde_0 <- ks::kdde(x = x, deriv.order = 0)
    # plot(kdde_0, display = "filled.contour2", xlab = "x", ylab = "y")
    #
    kdde_1 <- ks::kdde(x = x, deriv.order = 1)
    plot(kdde_1, display = "quiver", xlab = "x", ylab = "y")

    for(i in 1:2) {
      plot(
        kdde_1,
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

    # tibble(
    #   res[[1]],
    #   res[[2]]
    # )

  }

  if (PREDICT) {
    # REQUIRED: dt
    if (!exists("dt")) {
      stop("variable `dt` missing")
    }

    all_events <- unique(dt$action)
    all_clusters <- lapply(
      all_events,
      function(x) {
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
    names(all_clusters) <- all_events

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

    filterEvents <- function(date, eventdf) {
      # need a few representations of the day/time for compatibility
      today <- lubridate::date(date)
      now_min <- hms::as_hms(date)
      now_hr <- as.numeric(hms::as_hms(date))/3600 # in hours

      return(
        eventdf |>
          filter(
            day == today
          ) |>
          mutate(
            time = as.numeric(time)/3600 # in hours
          ) |>
          group_by(
            time, action
          ) |>
          summarise(
            value = sum(value),
            .groups = "drop"
          ) |>
          select(
            time,
            action,
            value
          )
      )
    }

    candidateClusters <- function(clusterdf) {
      return(
        clusterdf |>
          mutate(
            max = mean+1.5*sd,
            min = mean-1.5*sd,
          ) |>
          rename(
            time = mean,
            action = type,
          )
      )
    }

    #' filterCandidateClusters
    #'
    #' @param events A data.frame with 'action' and 'time' columns.
    #' @param clusters A data.frame with
    #'
    #'@noRd
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

    nextEvents <- function(date, eventdf, clusterdf) {
      ca <- candidateClusters(clusterdf)
      # min(time) per cluster for all clusters who do
      # not have events within their range
    }

    prevEvents <- function(date, eventdf) {
      now_hr <- as.numeric(hms::as_hms(date))/3600 # in hours

      mostrecent <- eventdf |>
        filter(
          day == today,
          time < now_hr
        ) |>
        mutate(
          time = as.numeric(time)/3600 # in hours
        ) |>
        group_by(action) |>
        summarise(time = max(time))

      return(mostrecent)
    }

    missedEvents <- function(date, eventdf, clusters) {
      # time per cluster for all clusters < time
      # who do not have events within 2/3 sigma
    }

    res <- dplyr::bind_rows(createClusterEvents(all_clusters))

    date <- lubridate::force_tz(lubridate::as_datetime("2021-12-13 6:00:00"), "EST5EDT")
    eventdf <- dt
    clusterdf <- res
    upcoming <- upcomingEvents(date, dt, res)

    hms::as_hms("07:00:00") + hms::hms(minutes = 15)

    # forward project 15 minute intervals for N_HOURS hours
    N_HOURS <- 16
    simulation_hours <- lapply(
      seq(0, N_HOURS * 60, by = 15),
      function(x) {
        date + lubridate::minutes(x)
      }
    )

    comingup <- lapply(
      simulation_hours,
      function(x) {
        upcomingEvents(x, dt, res)
      }
    )

    cu <- lapply(seq(length(comingup)), function(x) comingup[[x]] |> mutate(id = x))
    cu <- bind_rows(cu)

    todaysevents <- filterEvents(date, dt)

    colormap <- list(
      'food' = 'red',
      'out' =  'orange',
      'pee' = 'olivedrab',
      'play' = 'darkgreen',
      'poop' = 'turquoise',
      'sleep' = 'blue',
      'walk' = 'violet',
      'water' = 'pink4'
    )

    {
      p <- cu |>
        ggplot() +
        geom_point(aes(x = id, y = time, color = action))

      fe <- filterEvents(date, dt)
      # add a little bit of randomness to the times... to get line splitting
      fe$time = fe$time + runif(nrow(fe)) / 5

      if (nrow(fe) > 0) {
        for (i in seq(nrow(fe))) {
          dat <- fe[i, ]

          p <- p +
            #geom_hline(yintercept = dat$time, linetype = "dashed", color = colormap[[dat$action]], size = 0.5) +
            geom_vline(xintercept = ((dat$time - 6) * 60)/15, linetype = "dashed", color = colormap[[dat$action]], size = 0.5)
        }
        p <- p + ggtitle("upcoming events vs. actual events")
        p |> ggplotly()
      } else {
        stop("u idiot there are no events")
      }
    }

    # expected value can be derived from
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
  }

  date <- lubridate::force_tz(
    lubridate::as_datetime("2021-01-05 16:30:00"),
    "EST5EDT"
  )

  {
    dat <- dbMigrate()
    vals <- dbCluster(date, dat)
    predictions <- upcomingEvents(date, vals$eventdf, vals$clusterdf)
    predictions
  }
}
