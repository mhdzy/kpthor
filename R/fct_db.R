#' getConnection
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#'
#' @noRd

dbi <- dbInterface$new(
  drv = RPostgres::Postgres(),
  host = "192.168.0.111",
  port = 5432,
  user = "pi",
  pass = "blueberry",
  db = "apps"
)

dbInterface <- R6::R6Class(
  classname = "dbInterface",
  private = list(
    driver = NA,
    host = NA_character_,
    port = NA_integer_,
    user = NA_character_,
    pass = NA_character_,
    db   = NA_character_,

    connection = NA
  ),
  public = list(
    initialize = function(drv, host, port, user, pass, db) {
      self$set("driver", drv)
      self$set("host", host)
      self$set("port", port)
      self$set("user", user)
      self$set("pass", pass)
      self$set("db", db)

      tryCatch(
        expr = {
          self$connect()
          self$disconnect()
        },
        error = function(e) stop("could not initialize a db connection")
      )
    },

    #' get
    #'
    #' Obtains the value of a private variable.
    #'
    get = function(var) {
      return(private[[var]])
    },

    #' set
    #'
    #' Allows the setting of an existing private variable.
    #'
    set = function(var, val) {
      private[[var]] <- val
    },

    #' connect
    #'
    #' Connects to the database parameterized by the private variables.
    #'
    #' @importFrom DBI dbConnect
    #'
    connect = function() {
      self$set(
        "connection",
        DBI::dbConnect(
          drv  = self$get("driver"),
          host = self$get("host"),
          port = self$get("port"),
          user = self$get("user"),
          password = self$get("pass"),
          dbname = self$get("db")
        )
      )
    },

    #' disconnect
    #'
    #' Disconnects the connection stored in the private variable 'connection'.
    #'
    #' @importFrom DBI dbDisconnect
    #'
    disconnect = function() {
      DBI::dbDisconnect(self$get("connection"))
    },

    #' query
    #'
    #' Runs a SQL query against the database connection.
    #'
    #' @param sql A SQL query.
    #'
    #' @return The results of the query.
    #'
    #' @importFrom DBI dbGetQuery
    #'
    query = function(sql) {
      self$connect()
      res <- DBI::dbGetQuery(
        conn = self$get("connection"),
        statement = sql
      )
      self$disconnect()
      return(res)
    },

    #' generic
    #'
    #' Performs a connect/(query) function/disconnect procedure and returns
    #' the result. This is a wrapper for all db functions since it provides a
    #' way to prevent orphaned but open db connections.
    #'
    #' @param fn A function to call.
    #' @param params A list of named parameters to pass to `fn`.
    #'
    #' @return A result of the called function.
    #'
    generic = function(fn, params) {
      self$connect()
      res <- do.call(fn, params)
      self$disconnect()
      return(res)
    },

    #' create
    #'
    #' Creates a table.
    #'
    #' @param schema A schema name to create the table in. Defaults to "public".
    #' @param table A table name to create.
    #' @param fields A named vector: names are column names, values are types.
    #'
    #' @importFrom DBI Id dbCreateTable
    #'
    create = function(schema = "public", table, fields) {
      self$connect()
      res <- DBI::dbCreateTable(
        conn = self$get("connection"),
        name = DBI::Id(schema = schema, table = table),
        fields = fields
      )
      self$disconnect()
      return(res)
    },

    #' append
    #'
    #' Appends the data in `df` to the table `name`.
    #'
    #' @param schema A schema name to reference. Default is "public".
    #' @param table A table name to append to.
    #' @param data A data.frame to append to the table.
    #'
    #' @importFrom DBI Id dbWriteTable
    #'
    append = function(schema = "public", table, data) {
      self$connect()
      res <- DBI::dbWriteTable(
        conn = self$get("connection"),
        name = DBI::Id(schema = schema, table = table),
        value = data,
        append = TRUE
      )
      self$disconnect()
      return(res)
    },

    #' write
    #'
    #' Writes data in `df` to a table `name`.
    #'
    #' @param schema A schema name to reference. Default is "public".
    #' @param table A table name to write to.
    #' @param data A data frame to write to table.
    #'
    #' @importFrom DBI Id dbWriteTable
    #'
    write = function(schema = "public", table, data) {
      self$connect()
      res <- DBI::dbWriteTable(
        conn = self$get("connection"),
        name = DBI::Id(schema = schema, table = table),
        value = data,
        overwrite = TRUE
      )
      self$disconnect()
      return(res)
    },

    #' lineclean
    #'
    #' Cleans a string of whitespace, particularly of:
    #'  * tabs
    #'  * leading and trailing whitespace
    #'  * collapses multiple spaces
    #'  * destroys single line SQL comments
    #'
    #' @param x A string to remove various whitespace from.
    #'
    #' @returns A cleaned string.
    #'
    #' @importFrom stringi stri_replace_all
    #'
    lineclean = function(x) {
      return(
        stringi::stri_replace_all(
          str = x,
          regex = c("\t+", "^\\s+", "\\s+$", "[ ]+", "^[-]+.*$"),
          replacement = " ",
          vectorize_all = FALSE
        )
      )
    },

    #' onelineq
    #'
    #' Parses a SQL file into a valid query string.
    #'
    #' @param file A SQL file on disk to parse into a single line string.
    #'
    #' @return
    #'
    onelineq = function(file) {
      return(
        paste(
          unlist(
            Filter(function(x) x != "", lapply(readLines(file), lineclean))
          ),
          collapse = " "
        )
      )
    }

  )
)
