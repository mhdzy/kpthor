
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

## DBMS

The app uses a database to store pets, actions, and events. The db
interface functions are defined in `R/class_dbInterface.R`, and
connection parameters are configured by the `golem_opts$dbi` field (see
`R/run_app.R`).

The `dbInterface` class accepts two required initialization parameters,
`drv` and `dsn`. These correspond to a database driver and a data source
name (DSN). Currently, the app connects to a DSN named `KPthorSQL` using
an `ODBC` driver.

Once the driver and DSN are configured, the database will need to then
be loaded with tables that are required by the app. Functions to create
these tables can be found in `R/class_dbInterface.R` with the
`create_if_not_exist*()` naming pattern.

todo: if no required tables are found, a local SQLite db is created to
store info during each session

### KPthorSQL

Add the following entry to `/etc/odbc.ini`:

``` ini
[KPthorSQL]
Driver=PostgreSQL Default
Description=KPthorSQL DSN
User=my_database_username
Password=my_database_password
Database=apps
Server=192.168.0.1
Port=5432
```

### ODBC

Add the following entry to `/etc/odbcinst.ini`:

``` ini
[PostgreSQL Default]
Description=PostgreSQL ODBC driver
Driver=psqlodbcw.so
Setup=libodbcpsqlS.so
Debug=0
CommLog=1
FileUsage=1
Threading=2
UsageCount=1
```

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

This is how you run the app locally:

``` r
# library(kpthor)
kpthor::run_app()
```

# Host App

## Local

## Shiny Server

## RStudio Connect
