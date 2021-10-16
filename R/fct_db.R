#' dbInterface
#'
#' @description An R6 class used to interface with a database. Provides
#' wrapper functions for common database operations, such as querying, creating
#' tables, appending and writing to tables,
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom DBI Id dbConnect dbDisconnect
#' @importFrom DBI dbGetQuery dbExecute dbCreateTable dbWriteTable
#' @importFrom R6 R6Class
#' @importFrom stringi stri_replace_all
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

    #'
    #' @param drv A database connection driver.
    #' @param host A hostname to connect to.
    #' @param port A port to connect to.
    #' @param user A database username.
    #' @param pass A password to use to authenticate the user.
    #' @param db A database name to connect to.
    #'
    #' @return self
    #'
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

      invisible(self)
    },

    #'
    #' @param var A variable name to get.
    #'
    #' @return The private variable value.
    #'
    get = function(var) {
      return(private[[var]])
    },

    #'
    #' @param var A variable name to set.
    #' @param val A variable value to set.
    #'
    #' @return self
    #'
    set = function(var, val) {
      private[[var]] <- val
      invisible(self)
    },

    #'
    #' @return self
    #'
    connect = function() {
      self$set(
        "connection",
        dbConnect(
          drv  = self$get("driver"),
          host = self$get("host"),
          port = self$get("port"),
          user = self$get("user"),
          password = self$get("pass"),
          dbname = self$get("db")
        )
      )
      invisible(self)
    },

    #'
    #' @return self
    #'
    disconnect = function() {
      dbDisconnect(self$get("connection"))
      invisible(self)
    },

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

    #'
    #' @param sql A SQL query.
    #'
    #' @return The results of the query.
    #'
    query = function(sql) {
      self$generic(
        fn = dbGetQuery,
        list(
          conn = self$get("connection"),
          statement = self$onelineq(sql)
        )
      )
    },

    #'
    #' @param sql
    #'
    #' @return The results of the execute.
    #'
    execute = function(sql) {
      self$generic(
        fn = dbExecute,
        params = list(
          conn = self$get("connection"),
          statement = self$onelineq(sql),
        )
      )
    },

    #'
    #' @param schema A schema name to create the table in. Defaults to "public".
    #' @param table A table name to create.
    #' @param fields A named vector: names are column names, values are types.
    #'
    create = function(schema = "public", table, fields) {
      self$generic(
        fn = dbCreateTable,
        params = list(
          conn = self$get("connection"),
          name = Id(schema = schema, table = table),
          fields = fields
        )
      )
    },

    #'
    #' @param schema A schema name to reference. Default is "public".
    #' @param table A table name to append to.
    #' @param data A data.frame to append to the table.
    #'
    append = function(schema = "public", table, data) {
      self$generic(
        fn = dbWriteTable,
        params = list(
          conn = self$get("connection"),
          name = Id(schema = schema, table = table),
          value = data,
          append = TRUE
        )
      )
    },

    #'
    #' @param schema A schema name to reference. Default is "public".
    #' @param table A table name to write to.
    #' @param data A data frame to write to table.
    #'
    write = function(schema = "public", table, data) {
      self$generic(
        fn = dbWriteTable,
        params = list(
          conn = self$get("connection"),
          name = Id(schema = schema, table = table),
          value = data,
          overwrite = TRUE
        )
      )
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
    lineclean = function(x) {
      return(
        stri_replace_all(
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
    #' @return A cleaned SQL query from file.
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
