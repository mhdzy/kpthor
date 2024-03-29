
<!-- README.md is generated from README.Rmd. Please edit that file -->

# { kpthor } <img src="inst/app/www/favicon.ico" align="right" width="120"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-os](https://github.com/mhdzy/kpthor/workflows/R-os/badge.svg)](https://github.com/mhdzy/kpthor/actions)
[![R-install](https://github.com/mhdzy/kpthor/workflows/R-install/badge.svg)](https://github.com/mhdzy/kpthor/actions)
[![codecov](https://codecov.io/gh/mhdzy/kpthor/branch/main/graph/badge.svg?token=6HK4PC05NR)](https://codecov.io/gh/mhdzy/kpthor)
<!-- badges: end -->

# Overview

**kpthor** is a PWA that lets you track your dog’s activity over time.

# Preqreqs

## R

This app requires `R (>= 4.0)`, available for download at
<https://cloud.r-project.org/>.

### packages

To run the app, a few R package dependencies are needed. These are
documented in the `DESCRIPTION` file, but the code below will install
all packages (and their dependencies) imported by the app.

``` r
deps_to_scan <- c("Imports", "Suggests")
uapply <- function(...) unlist(lapply(...))
libs_sub <- function(x) sub(
  " .*", 
  "", 
  trimws(
    stringi::stri_split(
      yaml::read_yaml("DESCRIPTION")[[x]], 
      fixed = ","
    )[[1]]
  )
)
install.packages(uapply(deps_to_scan, libs_sub), dependencies = TRUE)
```

## IPC / streaming

The app uses the R package `liteq` for inter-process communication, and
thus requires a specific file to use as a thread-safe database. We will
also need to set the queue to be writable by the user who will run the
app. For this example, the `shiny` user will run the app. This user is
also a member of the `shiny` group, who will receive write access to the
directory and file. To set this up, we will need to run the following
chunk in a terminal, then the second chunk in R:

``` sh
mkdir db/
chgrp -R shiny db/
chmod -R g+ws db/
```

``` r
install.packages("liteq")
liteq::ensure_queue("timerq", db = "db/timerq")
```

## DBMS

The app uses a database to store pets, actions, and events. The db
interface functions are defined in `R/class_dbInterface.R`, and
connection parameters are configured by the `golem_opts$dbi` field (see
`R/run_app.R`). The DSN name is taken from the `inst/golem-config.yml`
file, depending on which mode is set in `.Renviron`.

Once the driver and DSN are configured on your machine, the database
will need to then be loaded with tables that are required by the app.
Functions to create these tables can be found in
`R/class_dbInterface.R`.

### DSN configuration

The DSN name used for production is `KPthorSQL`, but supports a
`development` mode which uses the `KPthorSQL-dev` DSN name. Add the
contents of `config/etc/odbc.ini` to your host server’s `/etc/odbc.ini`
file (`~/.odbc.ini` on macOS), and fill in your specific connection
details.

### Driver configuration

The ODBC driver needs to downloaded from the internet. Add the contents
of `config/etc/odbcinst.ini` to your server’s `/etc/odbcinst.ini` file.

# Installation

You can install the released version of **kpthor** from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mhdzy/kpthor")
```

You can also clone the **kpthor** source code by running:

``` zsh
git clone git@github.com:mhdzy/kpthor.git
```

# Run App

This is how you run the app from the package (prod):

``` r
# library(kpthor)
kpthor::run_app()
```

This is how you run the app from the repository (dev):

``` r
# setwd("kpthor/")
runApp("app.R")
```

# Host App

The app was primarily developed on macOS 11.16 running R 4.1.0 with an
i7 processor and 32GB RAM.

I hosted this app on a Raspberry Pi 4 Model B (4 GB) running Raspbian
(arm64) ([download
here](https://downloads.raspberrypi.org/raspios_arm64/images/raspios_arm64-2021-05-28/)).
This required building Shiny Server from source, and I loosely followed
[this
guide](https://community.rstudio.com/t/setting-up-your-own-shiny-server-rstudio-server-on-a-raspberry-pi-3b/18982)
from `andresrcs`. There is also an update guide using Ansible, available
[here](https://andresrcs.rbind.io/2021/01/13/raspberry_pi_server/).

## Local

Install R, package dependencies (see `DESCRIPTION`), setup PostgreSQL,
configure the OS files found in `config/`, then run the app.

TODO

-   [ ] Failover to RSQLite temporary in-memory db when Postgres is
    unavailable.

## Shiny Server

Deploying the `kpthor` app to Shiny Server requires a live Shiny Server
installation, available for download here:
<https://www.rstudio.com/products/shiny/download-server/>.

The config file is found in `config/etc/shiny-server/shiny-server.conf`.

### X11 / Xvfb

Viewing plots and graphics requires a window service, and the file
`config/etc/init/shiny-server.conf` contains the startup script to
enable Xvfb (X virtual framebuffer).
